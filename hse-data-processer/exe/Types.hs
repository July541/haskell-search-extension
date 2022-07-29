{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import qualified Data.Text as T
import Control.DeepSeq
import Data.Data (Data, Typeable)
import Language.Haskell.Exts hiding (ModuleName)

type PackageName = T.Text
type PackageURL = T.Text
type ModuleName = T.Text

-- | A URL, complete with a @https:@ prefix.
type URL = String

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

data Target = Target
  { targetURL ::URL
  , targetPackage :: Maybe (String, URL) -- ^ Name and URL of the package it is in (Nothing if it is a package)
  , targetModule :: Maybe (String, URL) -- ^ Name and URL of the module it is in (Nothing if it is a package or module)
  , targetType :: String -- ^ One of package, module or empty string
  , targetItem :: String -- ^ HTML span of the item, using @\<s0\>@ for the name and @\<s1\>@ onwards for arguments
  , targetDocs :: String -- ^ HTML documentation to show, a sequence of block level elements
  } deriving (Show, Eq, Ord)

instance NFData Target where
  rnf (Target a b c d e f) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f

data Sig n = Sig {sigCtx :: [Ctx n], sigTy :: [Ty n]} deriving (Show,Eq,Ord,Typeable,Data,Functor) -- list of -> types
data Ctx n = Ctx n n deriving (Show,Eq,Ord,Typeable,Data,Functor) -- context, second will usually be a free variable
data Ty n = TCon n [Ty n] | TVar n [Ty n] deriving (Show,Eq,Ord,Typeable,Data,Functor) -- type application, vectorised, all symbols may occur at multiple kinds
instance NFData n => NFData (Sig n) where rnf (Sig x y) = rnf x `seq` rnf y
instance NFData n => NFData (Ctx n) where rnf (Ctx x y) = rnf x `seq` rnf y
instance NFData n => NFData (Ty  n) where
    rnf (TCon x y) = rnf x `seq` rnf y
    rnf (TVar x y) = rnf x `seq` rnf y

data Item =
    IPackage PackageName
    | IModule ModuleName
    | IName T.Text
    | ISignature (Sig T.Text)
    | IAlias T.Text [T.Text] (Sig T.Text)
    | IInstance (Sig T.Text)
      deriving (Show,Eq,Ord,Typeable,Data)

instance NFData Item where
    rnf (IPackage x) = rnf x
    rnf (IModule x) = rnf x
    rnf (IName x) = x `seq` ()
    rnf (ISignature x) = rnf x
    rnf (IAlias a b c) = rnf (a,b,c)
    rnf (IInstance a) = rnf a

data Entry =
  EPackage PackageName
  | EModule ModuleName
  | EDecl (Decl ())
  deriving (Show)
