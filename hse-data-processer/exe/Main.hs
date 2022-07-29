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
import Text.Show.Unicode
import Hoogle (processHoogle)

defaultHackageOutPath = do
  path <- getCurrentDirectory
  pure $ path </> ".." </> "extension" </> "data" </> "hackage.ts"

defaultHoogleOutPat = do
  path <- getCurrentDirectory
  pure $ path </> ".." </> "extension" </> "data" </> "hoogle.ts"

defaultProcess :: Map.Map PackageName CabalPackage -> [T.Text]
defaultProcess = processSearchData convert format
  where
    convert :: PackageName -> CabalPackage -> SearchData
    convert name package = SearchData name (packageSynopsis package)

    format :: SearchData -> T.Text
    format SearchData{..} = T.pack $ ushow (T.unpack content) <> ":" <> ushow (T.unpack description)

defaultSave :: IO FilePath -> [T.Text] -> IO ()
defaultSave getPath datum = do
  path <- getPath
  putStrLn $ "Saving to: " <> path
  saveSearchData wrap path datum
  where
    wrap datum = "export const data = {" <> T.intercalate "," datum <> "}"

main :: IO ()
main = do
  -- processHackage defaultProcess (defaultSave defaultHackageOutPath)
  processHoogle (defaultSave defaultHoogleOutPat)
