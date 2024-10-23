{-# LANGUAGE OverloadedRecordDot #-}

module Generator.Extension where

import Data.List (intercalate)

rawDataPath :: FilePath
rawDataPath = "./data/extension.txt"

data Extension = Extension
  { name :: String
  , since :: String
  , status :: String
  , include :: String
  , url :: String
  }
  deriving (Eq, Show)

loadExtensionData :: FilePath -> IO [Extension]
loadExtensionData = (map parseExtension . drop 1 . lines <$>) . readFile
  where
    parseExtension :: String -> Extension
    parseExtension line = case words line of
      [name, since, status, include, url] -> Extension name since status include url
      _fail -> error $ "Invalid extension data: " ++ line

generateTSFileForExtension :: FilePath -> IO ()
generateTSFileForExtension file = do
  exts <- loadExtensionData rawDataPath
  writeFile file $
    "export const extensionRawData = "
      <> toCommaListString
        ( map
            ( \e ->
                toCommaListString
                  [ show e.name
                  , show e.since
                  , show e.status
                  , show e.include
                  , show e.url
                  ]
            )
            exts
        )
  where
    toCommaListString :: [String] -> String
    toCommaListString ls = "[" <> intercalate "," ls <> "]"
