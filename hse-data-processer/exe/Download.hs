{-# LANGUAGE ScopedTypeVariables #-}
module Download where

import qualified Network.HTTP.Conduit as C
import qualified Data.Conduit as C
import Network.Connection
import Data.Conduit.Binary (sinkFile)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO)
import System.IO.Extra (withTempDir, writeFileUTF8)
import System.FilePath ((</>))
import System.Time.Extra (duration, showDuration)
import Crypto.Hash.Conduit (hashFile)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Hash (Digest)
import System.Directory.Extra (getCurrentDirectory)
import Processer
import qualified Data.Map as Map
import qualified Data.Text as T

type URL = String

-- | Download from a specific url to a target file path,
-- and return its sha256.
downloadFileWithSHA256 :: FilePath -> URL -> IO (Digest SHA256)
downloadFileWithSHA256 file url = do
  downloadFile file url
  hashFile file

processHackage :: IO ()
processHackage = withTempDir $ \dir -> do
  let cabals = dir </> "haskell-cabal.tar.gz"
  -- timing cabals "https://hackage.haskell.org/01-index.tar.gz"
  timing cabals "http://localhost:8000/01-index.tar.gz"
  (res :: Digest SHA256) <- hashFile cabals
  persistSHA256 res
  r <- parseCabalTarball cabals
  print $ Map.lookup (T.pack "haskell-language-server") r
  print $ length $ Map.keys r
  return ()
  where
    timing :: FilePath -> URL -> IO ()
    timing file url = do
      putStrLn $ "Start downloading from " <> url <> "..."
      (sec, _) <- duration (downloadFile file url)
      putStrLn $ "Done. Saved to: " <> file <> ", time usage: " <> showDuration sec

    persistSHA256 :: Digest SHA256 -> IO ()
    persistSHA256 sha256 = do
      dir <- getCurrentDirectory
      let path = dir </> "sha256"
      writeFileUTF8 path (show sha256)

-- From hoogle
downloadFile :: FilePath -> URL -> IO ()
downloadFile file url = do
    let request = C.parseRequest_ url
    manager <- C.newManager $ C.mkManagerSettings (TLSSettingsSimple True False False) Nothing
    runResourceT $ do
        response <- C.http request manager
        C.runConduit $ C.responseBody response C..| sinkFile file
