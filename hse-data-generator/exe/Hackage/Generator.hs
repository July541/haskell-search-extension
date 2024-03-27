module Hackage.Generator where

import Conduit qualified as C
import Control.Monad (when)
import Data.Text qualified as T
import Network.Connection (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Conduit qualified as HC
import System.Directory
import System.Time.Extra (duration, showDuration)

data CabalPackage = CabalPackage
  { packageName :: !T.Text
  , packageSynopsis :: !T.Text
  }
  deriving (Eq, Show)

hackageTarUrl :: String
hackageTarUrl = "https://hackage.haskell.org/index.tar.gz"

parseCabalTarball :: FilePath -> IO CabalPackage
parseCabalTarball = undefined

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
