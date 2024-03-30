{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hackage.Generator where

import Conduit qualified as C
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 (toString)
import Data.Char
import Data.Conduit.List qualified as C
import Data.Conduit.Tar qualified as CT
import Data.Conduit.Zlib
import Data.List.Extra (replace)
import Data.Map qualified as Map
import Data.Text qualified as T
import Network.Connection (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Conduit qualified as HC
import System.Directory
import System.Time.Extra (duration, showDuration)

newtype CabalPackage = CabalPackage
  { packageSynopsis :: T.Text
  }
  deriving (Eq, Show)

hackageTarUrl :: String
hackageTarUrl = "https://hackage.haskell.org/packages/index.tar.gz"

readTarballFiles :: FilePath -> IO [(FilePath, BS.ByteString)]
readTarballFiles tarFile =
  C.runResourceT $
    C.runConduit $
      C.sourceFile tarFile
        C..| ungzip
        C..| CT.untarRaw bundleFileName
        C..| C.consume
  where
    bundleFileName :: CT.FileInfo -> C.ConduitM BS.ByteString (FilePath, BS.ByteString) (C.ResourceT IO) ()
    bundleFileName = C.mapC . (,) . toString . CT.filePath

test :: IO ()
test = do
  downloadHackageIndexIfNecessaryWithTiming "./index.tar.gz"
  files <- readTarballFiles "./index.tar.gz"
  print $ map fst files
  print $ length files

parseCabalTarball :: FilePath -> IO CabalPackage
parseCabalTarball = undefined
  where
    f = undefined

readCabal :: T.Text -> CabalPackage
readCabal src = CabalPackage{..}
  where
    mp = Map.fromList $ lexCabal src
    ask x = Map.findWithDefault [] x mp

    packageSynopsis = T.unwords $ T.words $ T.unwords $ ask "synopsis"

-- From hoogle
lexCabal :: T.Text -> [(T.Text, [T.Text])]
lexCabal = f . T.lines
  where
    f (x : xs)
      | (white, x) <- T.span isSpace x
      , (name, x) <- T.span (\c -> isAlpha c || c == '-') x
      , not (T.null name)
      , Just (':', x) <- T.uncons $ T.strip x
      , (xs1, xs2) <- span (\s -> T.length (T.takeWhile isSpace s) > T.length white) xs =
          (T.toLower name, T.strip x : replace ["."] [""] (map (T.strip . fst . T.breakOn "--") xs1)) : f xs2
    f (_ : xs) = f xs
    f [] = []

-- | Download the Hackage index tarball if it doesn't exist with timing information
downloadHackageIndexIfNecessaryWithTiming
  :: FilePath
  -- ^ Path to save the index tarball
  -> IO ()
downloadHackageIndexIfNecessaryWithTiming = downloadHackageWithTiming False

-- | Download the Hackage index tarball if it doesn't exist
downloadHackageIndexIfNecessary
  :: FilePath
  -- ^ Path to save the index tarball
  -> IO ()
downloadHackageIndexIfNecessary = downloadHackageIndex False

-- | Download the Hackage index tarball with timing information
downloadHackageWithTiming
  :: Bool
  -- ^ Always download the index tarball
  -> FilePath
  -- ^ Path to save the index tarball
  -> IO ()
downloadHackageWithTiming alwaysDownload savePath = do
  putStrLn "Downloading Hackage index..."
  (sec, _) <- duration $ downloadHackageIndex alwaysDownload savePath
  putStrLn $ "Hackage index downloaded, time usage: " <> showDuration sec

-- | Download the Hackage index tarball
downloadHackageIndex
  :: Bool
  -- ^ Always download the index tarball
  -> FilePath
  -- ^ Path to save the index tarball
  -> IO ()
downloadHackageIndex alwaysDownload savePath = do
  exist <- doesFileExist savePath
  when (alwaysDownload || not exist) $ downloadHackageIndexImpl savePath

downloadHackageIndexImpl
  :: FilePath
  -- ^ Path to save the index tarball
  -> IO ()
downloadHackageIndexImpl savePath = do
  request <- HC.parseRequest hackageTarUrl
  manager <- HC.newManager $ HC.mkManagerSettings (TLSSettingsSimple True False False) Nothing
  C.runResourceT $ do
    response <- HC.http request manager
    C.runConduit $ HC.responseBody response C..| C.sinkFile savePath
