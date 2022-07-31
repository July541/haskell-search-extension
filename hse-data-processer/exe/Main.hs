{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Download
import Processer
import System.Directory.Extra (getCurrentDirectory, createDirectoryIfMissing)
import System.FilePath
import qualified Data.Map as Map
import Types
import qualified Data.Text as T
import Text.Show.Unicode
import Hoogle (processHoogle)
import System.Environment (getArgs)
import System.IO.Extra (readFile')

defaultHackageOutPath = do
  path <- getCurrentDirectory
  let basePath = path </> ".." </> "extension" </> "data"
  createDirectoryIfMissing False basePath
  pure $ basePath </> "hackage.ts"

defaultHoogleOutPat = do
  path <- getCurrentDirectory
  let basePath = path </> ".." </> "extension" </> "data"
  createDirectoryIfMissing False basePath
  pure $ basePath </> "hoogle.ts"

defaultProcess :: Map.Map PackageName CabalPackage -> [T.Text]
defaultProcess = processSearchData convert format
  where
    convert :: PackageName -> CabalPackage -> SearchData
    convert name package = SearchData name (packageSynopsis package)

    format :: SearchData -> T.Text
    format SearchData{..} = T.pack $ "[" <> ushow (T.unpack content) <> "," <> ushow (T.unpack description) <> "]"

defaultSave :: IO FilePath -> [T.Text] -> IO ()
defaultSave getPath datum = do
  path <- getPath
  putStrLn $ "Saving to: " <> path
  saveSearchData wrap path datum
  where
    wrap datum = "export const data = [" <> T.intercalate "," datum <> "]"

loadPackageNames :: FilePath -> IO [String]
loadPackageNames file = do
  content <- readFile' file
  print $ lines content
  pure $ lines content

main :: IO ()
main = do
  args <- getArgs
  packages <- loadPackageNames (head args)
  processHackage defaultProcess (defaultSave defaultHackageOutPath)
  processHoogle packages (defaultSave defaultHoogleOutPat)
