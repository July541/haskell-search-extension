{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
  Most from hoogle
-}

module Processer where

import qualified Data.Map as Map
import Types (PackageName, CabalPackage (..), SearchData, PackageURL)
import Data.Conduit.List (sourceList, consume)
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar (Entries(..), FormatError, EntryContent (..), entryPath, GenEntries(..), GenEntryContent(..))
import Codec.Archive.Tar.Entry(entryContent)
import Control.Monad.IO.Class (liftIO)
import Conduit
import System.FilePath (takeBaseName)
import Data.Bifunctor (first)
import Control.Monad.Extra (whenJust, whileM)
import Control.Monad (when)
import GHC.IO (evaluate)
import Control.DeepSeq (force)
import qualified Data.Text as T
import Data.Tuple.Extra ((***))
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import Data.Char
import Data.List.Extra (replace, headDef)
import Control.Concurrent (newQSem, newChan, Chan, myThreadId, forkFinally, throwTo)
import Control.Concurrent.Extra (newBarrier, Barrier, signalBarrier, waitBarrier)
import Control.Concurrent hiding (yield)
import Data.Maybe (isJust)
import System.Directory.Extra (getCurrentDirectory)

readTarballFiles :: FilePath -> IO [(FilePath, LBS.ByteString)]
readTarballFiles file = f . Tar.read . GZip.decompress <$> LBS.readFile file
  where
    f :: Entries FormatError -> [(FilePath, LBS.ByteString)]
    f Done = []
    f (Next entry rest)
      | NormalFile body _ <- entryContent entry = (entryPath entry, body) : f rest
    f (Next _ rest) = f rest
    f (Fail e) = error $ "readTarballFiles on " <> file <> ", " <> show e

groupOnLastC :: (Monad m, Eq b) => (a -> b) -> ConduitM a a m ()
groupOnLastC op = do
  x <- await
  whenJust x $ \x -> f (op x) x
  where
    f k v = await >>= \case
      Nothing -> yield v
      Just v2 -> do
        let k2 = op v2
        when (k /= k2) (yield v)
        f k2 v2

pipelineC :: Int -> ConduitM o Void IO r -> ConduitM o Void IO r
pipelineC bufferSize sink = do
  sem <- liftIO $ newQSem bufferSize
  (chan :: Chan (Maybe o)) <- liftIO newChan
  (bar :: Barrier r) <- liftIO newBarrier
  me <- liftIO myThreadId
  liftIO $ flip forkFinally (either (throwTo me) (signalBarrier bar)) $ do
    runConduit $
      whileM (do
        x <- liftIO $ readChan chan
        liftIO $ signalQSem sem
        whenJust x yield
        pure $ isJust x) .| sink
  awaitForever $ \x -> liftIO $ do
    waitQSem sem
    writeChan chan $ Just x
  liftIO $ writeChan chan Nothing
  liftIO $ waitBarrier bar

parseCabalTarball :: FilePath -> IO (Map.Map PackageName CabalPackage)
parseCabalTarball tarFile = do
  fmap Map.fromList $ runConduit $
    (liftIO (readTarballFiles tarFile) >>= sourceList)
      .| mapC (first takeBaseName)
      .| groupOnLastC fst
      .| mapMC (evaluate . force)
      .| pipelineC 10
          (mapC (T.pack *** readCabal . LT.toStrict . LT.decodeUtf8With (\_ _ -> Just '\xFFFD'))
            -- .| mapMC (evaluate . force)
            .| consume)

readCabal :: T.Text -> CabalPackage
readCabal src = CabalPackage{..}
  where
    mp = Map.fromList $ lexCabal src
    ask x = Map.findWithDefault [] x mp

    packageSynopsis = T.unwords $ T.words $ T.unwords $ ask "synopsis"
    packageVersion = headDef "0.0.0.0" $ dropWhile T.null $ ask "version"
    isPackageLibrary = "library" `elem` map (T.toLower . trimText) (T.lines src)

lexCabal :: T.Text -> [(T.Text, [T.Text])]
lexCabal = f . T.lines
  where
    f (x:xs) | (white, x) <- T.span isSpace x
             , (name, x) <- T.span (\c -> isAlpha c || c == '-') x
             , not (T.null name)
             , Just (':', x) <- T.uncons $ trimText x
             , (xs1, xs2) <- span (\s -> T.length (T.takeWhile isSpace s) > T.length white) xs
             = (T.toLower name, trimText x : replace ["."] [""] (map (trimText . fst . T.breakOn "--") xs1)) : f xs2
    f (x:xs) = f xs
    f [] = []

trimText :: T.Text -> T.Text
trimText t = T.dropWhileEnd isSpace $ T.dropWhile isSpace t

processSearchData
  :: (PackageName -> CabalPackage -> SearchData)
  -> (SearchData -> T.Text)
  -> Map.Map PackageName CabalPackage
  -> [T.Text]
processSearchData convert format = map (format . uncurry convert) . Map.toAscList

saveSearchData
  :: ([T.Text] -> T.Text)
  -> FilePath
  -> [T.Text]
  -> IO ()
saveSearchData wrap path =  writeFile path . T.unpack . wrap

wrapHackageURL :: PackageName -> PackageURL
wrapHackageURL = ("https://hackage.haskell.org/package/" <>)
