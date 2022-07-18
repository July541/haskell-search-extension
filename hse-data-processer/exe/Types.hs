module Types where

import qualified Data.Text as T
import Control.DeepSeq

type PackageName = T.Text

data CabalPackage = CabalPackage
  { packageSynopsis :: T.Text
  , packageVersion :: T.Text
  , isPackageLibrary :: Bool
  } deriving (Show)

instance NFData CabalPackage where
  rnf (CabalPackage a b c) = rnf (a, b, c)

data SearchData = SearchData
  { content :: !T.Text
  , description :: !T.Text
  } deriving (Show)
