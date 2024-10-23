module Main where

import Generator.Error (generateTSFileForErrors)
import Generator.Extension (generateTSFileForExtension)
import Generator.Hackage (generateTSFileForHackage)

main :: IO ()
main = do
  generateTSFileForHackage "../extension/Omnibox/data/hackage/hackageRawData.ts"
  generateTSFileForErrors "../extension/Omnibox/data/error/errorRawData.ts"
  generateTSFileForExtension "../extension/Omnibox/data/extension/extensionRawData.ts"
