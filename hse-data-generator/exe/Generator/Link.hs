{-# LANGUAGE OverloadedRecordDot #-}

module Generator.Link where

import Data.List (intercalate)
import Data.List.Extra (splitOn)

rawDataPath :: FilePath
rawDataPath = "./data/link.txt"

data Link = Link
  { name :: String
  , url :: String
  , description :: String
  }
  deriving (Eq, Show)

loadLinkData :: FilePath -> IO [Link]
loadLinkData = (map parseLink . drop 1 . lines <$>) . readFile
  where
    parseLink :: String -> Link
    parseLink line = case splitOn "," line of
      [name, url, description] -> Link name url description
      _fail -> error $ "Invalid link data: " ++ line

generateTSFileForLink :: FilePath -> IO ()
generateTSFileForLink file = do
  links <- loadLinkData rawDataPath
  writeFile file $
    "export const linkRawData = "
      <> toCommaListString
        ( map
            ( \l ->
                toCommaListString
                  [ show l.name
                  , show l.url
                  , show l.description
                  ]
            )
            links
        )
  where
    toCommaListString :: [String] -> String
    toCommaListString ls = "[" <> intercalate "," ls <> "]"
