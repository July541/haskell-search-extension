module Download where

import qualified Network.HTTP.Conduit as C
import qualified Data.Conduit as C
import Network.Connection
import Data.Conduit.Binary (sinkFile)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO)
import System.IO.Extra (withTempDir)
import System.FilePath ((</>))
import System.Time.Extra (duration, showDuration)

processHackage :: IO ()
processHackage = withTempDir $ \dir -> do
  let cabals = dir </> "haskell-cabal.tar.gz"
  timing cabals "https://hackage.haskell.org/packages/index.tar.gz"
  return ()
  where
    timing file url = do
      putStrLn $ "Start downloading from " <> url <> "..."
      (sec, _) <- duration (downloadFile file url)
      putStrLn $ "Done. Saved to: " <> file <> ", time usage: " <> showDuration sec

-- From hoogle
downloadFile :: FilePath -> String -> IO ()
downloadFile file url = do
    let request = C.parseRequest_ url
    manager <- C.newManager $ C.mkManagerSettings (TLSSettingsSimple True False False) Nothing
    runResourceT $ do
        response <- C.http request manager
        C.runConduit $ C.responseBody response C..| sinkFile file
