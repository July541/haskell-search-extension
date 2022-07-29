{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Hoogle where
import System.IO.Extra (withTempDir, writeFileUTF8)
import Download (timing, persistSHA256)
import System.FilePath
import Processer (readTarballFiles, wrapHackageURL, pipelineC)
import Control.Monad (forM_, void)
import Conduit
    ( mapC,
      await,
      sinkList,
      (.|),
      runConduit,
      awaitForever,
      yield,
      ConduitM,
      MonadIO (liftIO),
      ConduitT,
      takeExactlyC )
import qualified Data.Text as T
import Types
import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString as BS
import qualified Data.Conduit.Binary as C
import Data.Conduit.List ( sourceList )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as C
import Control.Monad.Extra (whenJust)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace, isAlpha, ord, isAscii, isAlphaNum)
import Data.List.Extra (stripPrefix, isPrefixOf, breakOn, word1, replace, isSuffixOf, isInfixOf, dropPrefix, findIndex, sortOn)
import Language.Haskell.Exts hiding (ModuleName)
import Data.Generics.Uniplate.Data (transform)
import Data.String (IsString)
import qualified Data.ByteString.UTF8 as US
import Control.DeepSeq (rnf)
import Data.Bifunctor (second)
import Text.Show.Unicode (ushow)

processHoogle :: [String] -> ([T.Text] -> IO ()) -> IO ()
processHoogle packages save = withTempDir $ \dir -> do
  let hoogles = dir </> "haskell-hoogle.tar.gz"
      wrap txt = "/" <> txt <> ".txt"
      sortPackages = sortOn (\(x, _) -> findIndex (\p -> wrap p `isSuffixOf` x ) packages)
  -- timing hoogles "https://hackage.haskell.org/packages/hoogle.tar.gz"
  timing hoogles "http://localhost:8000/hoogle.tar.gz"
  tar <- readTarballFiles hoogles
  tar <- pure $ filter ((\x -> any (\p -> wrap p `isSuffixOf` x) packages) . fst) tar
  tar <- pure $ sortPackages tar
  putStrLn "Loading the following packages:"
  mapM_ print (zip [1..] (map fst tar))
  let
    source :: ConduitT () (T.Text, PackageURL, LBS.ByteString) IO ()
    source = forM_ tar $ \(T.pack . takeBaseName -> name, src) ->
                  yield (name, wrapHackageURL name, src)
    consume = awaitForever $ \(i, (pkg, T.unpack -> url, body)) -> do
                parseHoogle url body

  xs <- runConduit $ source .| void (zipFromC 1 .| consume) .| sinkList -- pipelineC 10 (takeExactlyC 100 sinkList)
  print $ length xs
  ys <- runConduit $ sourceList xs .| collectSearchData .| mapC (\(a, b) -> T.pack $ "[" <> ushow (T.unpack a) <> "," <> ushow b <> "]") .| sinkList
  print $ length ys
  save ys
  writeFileUTF8 "./hoogle" (T.unpack $ T.unlines ys)

collectSearchData :: ConduitM (Maybe Target, [Item]) (T.Text, URL) IO ()
collectSearchData = do
  x <- await
  whenJust x $ \(target, items) -> do
    whenJust (findName items) $ \name -> whenJust target $ \target -> do
      let url = prune $ splineURL target
      yield (name, url)
    collectSearchData
  where
    prune :: URL -> URL
    prune = dropPrefix "https://hackage.haskell.org/package/"

splineURL :: Target -> URL
splineURL (Target url Nothing Nothing _ _ _) = url
splineURL (Target package (Just (_, base)) Nothing _ _ _) = base <> package
splineURL (Target func (Just (_, base)) (Just (_, package)) _ _ _)= base <> package <> func
splineURL _ = error "splineURL: impossible"

findName :: [Item] -> Maybe T.Text
findName [] = Nothing
findName (x:xs) = case x of
  IPackage txt -> Just txt
  IModule txt -> Just txt
  IName txt -> Just txt
  ISignature sig -> findName xs
  IAlias txt txts sig -> findName xs
  IInstance sig -> findName xs

-- --------------------------------------------------------
-- The following are transferred from hoogle
-- --------------------------------------------------------

parseHoogle :: MonadIO m => URL -> LBS.ByteString -> ConduitM a (Maybe Target, [Item]) m ()
parseHoogle url body =
  sourceList (LBS.toChunks body)
    .| linesCR
    .| parseC
    .| hierarchyC url
    .| C.map (\x -> rnf x `seq` x)

hierarchyC :: Monad m => URL -> ConduitM (Target, Entry) (Maybe Target, [Item]) m ()
hierarchyC packageUrl = void $ mapAccumC f (Nothing, Nothing)
    where
        f (pkg, mod) (t, EPackage x) = ((Just (T.unpack x, url), Nothing), (Just t{targetURL=url}, [IPackage x]))
            where url = targetURL t `orIfNull` packageUrl
        f (pkg, mod) (t, EModule x) = ((pkg, Just (T.unpack x, url)), (Just t{targetPackage=pkg, targetURL=url}, [IModule x]))
            where url = targetURL t `orIfNull` (if isGhc then ghcModuleURL x else hackageModuleURL x)
        f (pkg, mod) (t, EDecl i@InstDecl{}) = ((pkg, mod), (Nothing, hseToItem_ i))
        f (pkg, mod) (t, EDecl x) = ((pkg, mod), (Just t{targetPackage=pkg, targetModule=mod, targetURL=url}, hseToItem_ x))
            where url = targetURL t `orIfNull` case x of
                            _ | [n] <- declNames x -> hackageDeclURL (isTypeSig x) n
                              | otherwise -> ""

        isGhc = "~ghc" `isInfixOf` packageUrl || "/" `isSuffixOf` packageUrl

        hseToItem_ x = hseToItem x `orIfNull` error ("hseToItem failed, " ++ pretty x)
        infix 1 `orIfNull`
        orIfNull x y = if null x then y else x

pretty :: Pretty a => a -> String
pretty = prettyPrintWithMode defaultMode{layout=PPNoLayout}

hseToItem :: Decl a -> [Item]
hseToItem (TypeSig _ names ty) = ISignature (T.pack <$> hseToSig ty) : map (IName . T.pack . fromName) names
hseToItem (TypeDecl _ (fromDeclHead -> (name, bind)) rhs) = [IAlias (T.pack $ fromName name) (map (T.pack . fromName . fromTyVarBind) bind) (T.pack <$> hseToSig rhs)]
hseToItem (InstDecl an _ (fromIParen -> IRule _ _ ctx (fromInstHead -> (name, args))) _) = [IInstance $ fmap T.pack $ hseToSig $ TyForall an Nothing ctx $ applyType (TyCon an name) args]
hseToItem x = map (IName . T.pack) $ declNames x

fromIParen :: InstRule a -> InstRule a
fromIParen (IParen _ x) = fromIParen x
fromIParen x = x

fromTyVarBind :: TyVarBind a -> Name a
fromTyVarBind (KindedVar _ x _) = x
fromTyVarBind (UnkindedVar _ x) = x

fromInstHead :: InstHead a -> (QName a, [Type a])
fromInstHead (IHCon _ n) = (n, [])
fromInstHead (IHInfix _ x n) = (n, [x])
fromInstHead (IHParen _ x) = fromInstHead x
fromInstHead (IHApp _ ih x) = second (++[x]) $ fromInstHead ih

fromContext :: Context a -> [Asst a]
fromContext (CxSingle _ x) = [x]
fromContext (CxTuple _ xs) = xs
fromContext _ = []

hseToSig :: Type a -> Sig String
hseToSig = tyForall
    where
        -- forall at the top is different
        tyForall (TyParen _ x) = tyForall x
        tyForall (TyForall _ _ c t) | Sig cs ts <- tyForall t =
            Sig (maybe [] (concatMap ctx . fromContext) c ++ cs) ts
        tyForall x = Sig [] $ tyFun x

        tyFun (TyParen _ x) = tyFun x
        tyFun (TyFun _ a b) = ty a : tyFun b
        tyFun x = [ty x]

        ty (TyForall _ _ _ x) = TCon "\\/" [ty x]
        ty x@TyFun{} = TCon "->" $ tyFun x
        ty (TyTuple an box ts) = TCon (fromQName $ Special an $ TupleCon an box $ length ts - 1) (map ty ts)
        ty (TyList _ x) = TCon "[]" [ty x]
        ty (TyParArray _ x) = TCon "[::]" [ty x]
        ty (TyApp _ x y) = case ty x of
            TCon a b -> TCon a (b ++ [ty y])
            TVar a b -> TVar a (b ++ [ty y])
        ty (TyVar _ x) = TVar (fromName x) []
        ty (TyCon _ x) = TCon (fromQName x) []
        ty (TyInfix an a (UnpromotedName _ b) c) = ty $ let ap = TyApp an in TyCon an b `ap` a `ap` c
        ty (TyKind _ x _) = ty x
        ty (TyBang _ _ _ x) = ty x
        ty (TyParen _ x) = ty x
        ty _ = TVar "_" []

        ctx (ParenA _ x) = ctx x
        ctx (TypeA _ x)  = ctxTy x
        ctx _ = []

        ctxTy (TyInfix an a (UnpromotedName _ con) b) = ctxTy $ TyApp an (TyApp an (TyCon an con) a) b
        ctxTy (fromTyApps -> TyCon _ con:TyVar _ var:_) = [Ctx (fromQName con) (fromName var)]
        ctxTy _ = []

        fromTyApps (TyApp _ x y) = fromTyApps x ++ [y]
        fromTyApps x = [x]

fromQName :: QName a -> String
fromQName (Qual _ _ x) = fromName x
fromQName (UnQual _ x) = fromName x
fromQName (Special _ UnitCon{}) = "()"
fromQName (Special _ ListCon{}) = "[]"
fromQName (Special _ FunCon{}) = "->"
fromQName (Special _ (TupleCon _ box n)) = "(" ++ h ++ replicate n ',' ++ h ++ ")"
    where h = ['#' | box == Unboxed]
fromQName (Special _ UnboxedSingleCon{}) = "(##)"
fromQName (Special _ Cons{}) = ":"
fromQName (Special _ ExprHole{}) = ""

fromName :: Name a -> String
fromName (Ident _ x) = x
fromName (Symbol _ x) = x

isTypeSig :: Decl a -> Bool
isTypeSig TypeSig{} = True
isTypeSig PatSynSig{} = True
isTypeSig _ = False

declNames :: Decl a -> [String]
declNames x = map fromName $ case x of
    TypeDecl _ hd _ -> f hd
    DataDecl _ _ _ hd _ _ -> f hd
    GDataDecl _ _ _ hd _ _ _ -> f hd
    TypeFamDecl _ hd _ _ -> f hd
    DataFamDecl _ _ hd _ -> f hd
    ClassDecl _ _ hd _ _ -> f hd
    TypeSig _ names _ -> names
    PatSynSig _ names _ _ _ _ _ -> names
    _ -> []
    where f x = [fst $ fromDeclHead x]

fromDeclHead :: DeclHead a -> (Name a, [TyVarBind a])
fromDeclHead (DHead _ n) = (n, [])
fromDeclHead (DHInfix _ x n) = (n, [x])
fromDeclHead (DHParen _ x) = fromDeclHead x
fromDeclHead (DHApp _ dh x) = second (++[x]) $ fromDeclHead dh

hackageModuleURL :: ModuleName -> URL
hackageModuleURL x = "/docs/" ++ ghcModuleURL x

ghcModuleURL :: ModuleName -> URL
ghcModuleURL x = replace "." "-" (T.unpack x) ++ ".html"

hackageDeclURL :: Bool -> String -> URL
hackageDeclURL typesig x = "#" ++ (if typesig then "v" else "t") ++ ":" ++ concatMap f x
    where
        f x | isLegal x = [x]
            | otherwise = "-" ++ show (ord x) ++ "-"
        -- isLegal is from haddock-api:Haddock.Utils; we need to use
        -- the same escaping strategy here in order for fragment links
        -- to work
        isLegal ':' = True
        isLegal '_' = True
        isLegal '.' = True
        isLegal c = isAscii c && isAlphaNum c

parseC :: MonadIO m => ConduitM BS.ByteString (Target, Entry) m ()
parseC = go [] ""
  where
    go com url = do
      x <- await
      whenJust x $ \s -> case () of
        _ | Just s <- bstrStripPrefix "-- | " s -> go [ignoreMath s] url
          | Just s <- bstrStripPrefix "--" s -> go (if null com then [] else BS8.dropWhile isSpace s : com) url
          | Just s <- bstrStripPrefix "@url " s -> go com (show s)
          | BS.null $ BS8.dropWhile isSpace s -> go [] ""
          | otherwise -> do
            case parseLine $ fixLine $ US.toString s of
              Left x -> pure () -- error $ show i ++ ": " ++ x
              Right [EDecl InfixDecl{}] -> pure ()
              Right xs -> forM_ xs $ \x -> do
                yield (Target url Nothing Nothing (typeItem x) (renderItem x) (reformat $ reverse com), x)
            go [] ""

renderItem :: Entry -> String
renderItem = \case
  EPackage txt -> T.unpack txt
  EModule txt -> T.unpack txt
  EDecl de -> prettyPrintWithMode defaultMode{layout=PPNoLayout} de

bstrStripPrefix :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
bstrStripPrefix needle x
    | BS.isPrefixOf needle x = Just $ BS.drop (BS.length needle) x
    | otherwise = Nothing

typeItem (EPackage x) = "package"
typeItem (EModule x) = "module"
typeItem _ = ""

mapAccumC f = C.mapAccum (\x a -> a `seq` f a x)

reformat :: [BS.ByteString] -> String
reformat = unlines . Prelude.map US.toString

fixLine :: String -> String
fixLine (stripPrefix "instance [incoherent] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlap ok] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlapping] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [safe] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "(#) " -> Just x) = "( # ) " ++ x
fixLine ('[':x:xs) | isAlpha x || x `elem` ("_(" :: String), (a,']':b) <- break (== ']') xs = x : a ++ b
fixLine ('[':':':xs) | (a,']':b) <- break (== ']') xs = "(:" ++ a ++ ")" ++ b
fixLine x | "class " `isPrefixOf` x = fst $ breakOn " where " x
fixLine x = x

parseLine :: String -> Either String [Entry]
parseLine x@('@':str) = case a of
        "package" | [b] <- words b, b /= "" -> Right [EPackage $ T.pack b]
        "version" -> Right []
        _ -> Left $ "unknown attribute: " ++ x
    where (a,b) = word1 str
parseLine (stripPrefix "module " -> Just x) = Right [EModule $ T.pack x]
parseLine x | Just x <- readItem x = case x of
    TypeSig a bs c -> Right [EDecl (TypeSig a [b] c) | b <- bs]
    x -> Right [EDecl x]
parseLine x = Left $ "failed to parse: " ++ x

readItem :: String -> Maybe (Decl ())
readItem x | ParseOk y <- myParseDecl x = Just $ unGADT y
readItem x -- newtype
    | Just x <- stripPrefix "newtype " x
    , ParseOk (DataDecl an _ b c d e) <- fmap unGADT $ myParseDecl $ "data " ++ x
    = Just $ DataDecl an (NewType ()) b c d e
readItem x -- constructors
    | ParseOk (GDataDecl _ _ _ _ _ [GadtDecl s name _ _ _ ty] _) <- myParseDecl $ "data Data where " ++ x
    , let f (TyBang _ _ _ (TyParen _ x@TyApp{})) = x
          f (TyBang _ _ _ x) = x
          f x = x
    = Just $ TypeSig s [name] $ applyFun1 $ Prelude.map f $ unapplyFun ty
readItem ('(':xs) -- tuple constructors
    | (com,')':rest) <- span (== ',') xs
    , ParseOk (TypeSig s [Ident{}] ty) <- myParseDecl $ Prelude.replicate (length com + 2) 'a' ++ rest
    = Just $ TypeSig s [Ident s $ '(':com++")"] ty
readItem (stripPrefix "data (" -> Just xs)  -- tuple data type
    | (com,')':rest) <- span (== ',') xs
    , ParseOk (DataDecl a b c d e f) <- fmap unGADT $ myParseDecl $
        "data " ++ Prelude.replicate (length com + 2) 'A' ++ rest
    = Just $ DataDecl a b c (transform (op $ '(':com++")") d) e f
    where op s DHead{} = DHead () $ Ident () s
          op s x = x
readItem _ = Nothing

myParseDecl = fmap void . parseDeclWithMode parseMode -- partial application, to share the initialisation cost

unGADT (GDataDecl a b c d _  [] e) = DataDecl a b c d [] e
unGADT x = x

parseMode :: ParseMode
parseMode = defaultParseMode{extensions=Prelude.map EnableExtension es}
    where es = [ConstraintKinds,EmptyDataDecls,TypeOperators,ExplicitForAll,GADTs,KindSignatures,MultiParamTypeClasses
               ,TypeFamilies,FlexibleContexts,FunctionalDependencies,ImplicitParams,MagicHash,UnboxedTuples
               ,ParallelArrays,UnicodeSyntax,DataKinds,PolyKinds,PatternSynonyms]

applyType :: Type a -> [Type a] -> Type a
applyType = foldl (\ x t -> TyApp (ann t) x t)

applyFun1 :: [Type a] -> Type a
applyFun1 [] = error "applyFun1: pattern matching failed"
applyFun1 [x] = x
applyFun1 (x:xs) = TyFun (ann x) x $ applyFun1 xs

unapplyFun :: Type a -> [Type a]
unapplyFun (TyFun _ x y) = x : unapplyFun y
unapplyFun x = [x]

linesCR :: Monad m => ConduitM BS.ByteString BS.ByteString m ()
linesCR = C.lines .| mapC f
  where
    f x | Just (x, '\r') <- BS8.unsnoc x = x
        | otherwise = x

zipFromC :: (Monad m, Enum i) => i -> ConduitM a (i, a) m ()
zipFromC = void . mapAccumC (\i x -> (succ i, (i,x)))
  where
    mapAccumC f = C.mapAccum (\x a -> a `seq` f a x)

ignoreMath :: BS.ByteString -> BS.ByteString
ignoreMath x
  | Just x <- BS.stripPrefix "&lt;math&gt;" x = fromMaybe x (BS.stripPrefix ". " x)
  | otherwise = x
