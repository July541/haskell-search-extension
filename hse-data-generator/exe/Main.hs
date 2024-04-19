module Main where
import Hackage.Generator (generateTSFileForHackage)

main :: IO ()
main = generateTSFileForHackage "../extension/Omnibox/hackageRawData.ts"
