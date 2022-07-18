{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Download
import Processer
import System.Directory.Extra (getCurrentDirectory)
import System.FilePath
import qualified Data.Map as Map
import Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Show.Unicode

defaultOutPath = do
  path <- getCurrentDirectory
  pure $ path </> ".." </> "extension" </> "data" </> "hackage.ts"

defaultProcess :: Map.Map PackageName CabalPackage -> [T.Text]
defaultProcess = processSearchData convert format
  where
    convert :: PackageName -> CabalPackage -> SearchData
    convert name package = SearchData name (packageSynopsis package)

    format :: SearchData -> T.Text
    format SearchData{..} = T.pack $ wrap (ushow (T.unpack content)) <> ":" <> ushow (urecover (T.unpack description))

    wrap s = s -- "\"" ++ s ++ "\""

defaultSave :: [T.Text] -> IO ()
defaultSave datum = do
  path <- defaultOutPath
  putStrLn $ "Saving to: " <> path
  saveSearchData wrap path datum
  where
    wrap datum = "export const data = {" <> T.intercalate "," datum <> "}"

main :: IO ()
main = processHackage defaultProcess defaultSave
