{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  PrettyPrec printing class and instances for CoreHW
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Core.Pretty
  ( PrettyPrec (..)
  , PrettyOptions (..)
  , ClashDoc
  , ClashAnnotation (..)
  , SyntaxElement (..)
  , ppr, ppr'
  , showPpr, showPpr'
  , tracePprId
  , tracePpr
  , tracePprM
  , fromPpr
  )
where

import Data.Char                        (isSymbol, isUpper, ord)
import Data.Default                     (Default(..))
import Data.Text                        (Text)
import Control.Monad.Identity
import Data.List.Extra                  ((<:>))
import qualified Data.Text              as T
import Data.Maybe                       (fromMaybe)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal
import GHC.Show                         (showMultiLineString)
import Numeric                          (fromRat)
import qualified Outputable             as GHC
import System.Environment               (lookupEnv)
import System.IO.Unsafe                 (unsafePerformIO)
import Text.Read                        (readMaybe)

import Clash.Core.DataCon               (DataCon (..))
import Clash.Core.Literal               (Literal (..))
import Clash.Core.Name                  (Name (..))
import Clash.Core.Term
  (Pat (..), Term (..), TickInfo (..), NameMod (..), CoreContext (..), primArg, PrimInfo(primName))
import Clash.Core.TyCon                 (TyCon (..), TyConName, isTupleTyConLike)
import Clash.Core.Type                  (ConstTy (..), Kind, LitTy (..),
                                         Type (..), TypeView (..), tyView)
import Clash.Core.Var                   (Id, TyVar, Var (..), IdScope(..))
import Clash.Debug                      (trace, traceM)
import Clash.Util
import qualified Clash.Util.Interpolate as I
import Clash.Pretty

unsafeLookupEnvBool :: HasCallStack =>  String -> Bool -> Bool
unsafeLookupEnvBool key dflt =
  case unsafePerformIO (lookupEnv key) of
    Nothing -> dflt
    Just a -> flip fromMaybe (readMaybe a) $ error [I.i|
      'unsafeLookupEnvBool' tried to lookup #{key} in the environment. It found
      it, but couldn't interpret it to as a Bool. Expected one of: True, False.
      But found:

        #{a}
    |]

-- | Options for the pretty-printer, controlling which elements to hide.
data PrettyOptions = PrettyOptions
  { displayUniques    :: Bool
  -- ^ whether to display unique identifiers
  , displayTypes      :: Bool
  -- ^ whether to display type information
  , displayQualifiers :: Bool
  -- ^ whether to display module qualifiers
  }
instance Default PrettyOptions where
  def = PrettyOptions
    { displayUniques    = unsafeLookupEnvBool "CLASH_PPR_UNIQUES" True
    , displayTypes      = unsafeLookupEnvBool "CLASH_PPR_TYPES" True
    , displayQualifiers = unsafeLookupEnvBool "CLASH_PPR_QUALIFIERS" True
    }

-- | Annotations carried on pretty-printed code.
data ClashAnnotation
  = AnnContext CoreContext
  -- ^ marking navigation to a different context
  | AnnSyntax  SyntaxElement
  -- ^ marking a specific sort of syntax
  deriving Eq

-- | Specific places in the program syntax.
data SyntaxElement = Keyword | LitS | Type | Unique | Qualifier
  deriving (Eq, Show)

-- | Clash's specialized @Doc@ type holds metadata of type @ClashAnnotation@.
type ClashDoc = Doc ClashAnnotation

-- | PrettyPrec printing Show-like typeclass
class PrettyPrec p where

  -- default pretty-printing without hiding
  pprPrec :: Monad m => Rational -> p -> m ClashDoc

  -- pretty-printing with hiding options
  -- NB: we utilise the syntax annotations to hide the requested parts of syntax
  pprPrec' :: Monad m => PrettyOptions -> Rational -> p -> m ClashDoc
  pprPrec' opts p = fmap hide . pprPrec p
    where
      hide = \case
        FlatAlt d d'         -> FlatAlt (hide d) (hide d')
        Cat d d'             -> Cat (hide d) (hide d')
        Nest i d             -> Nest i (hide d)
        Union d d'           -> Union (hide d) (hide d')
        Column f             -> Column (hide . f)
        WithPageWidth f      -> WithPageWidth (hide . f)
        Nesting f            -> Nesting (hide . f)
        Annotated ann d'     ->
          if not (displayTypes opts)      && ann == AnnSyntax Type
          || not (displayUniques opts)    && ann == AnnSyntax Unique
          || not (displayQualifiers opts) && ann == AnnSyntax Qualifier
            then Empty
            else Annotated ann (hide d')
        d -> d

pprM :: (Monad m, PrettyPrec p) => p -> m ClashDoc
pprM = pprPrec 0

pprM' :: (Monad m, PrettyPrec p) => PrettyOptions -> p -> m ClashDoc
pprM' opts = pprPrec' opts 0

ppr :: PrettyPrec p => p -> ClashDoc
ppr = runIdentity . pprM

ppr' :: PrettyPrec p => PrettyOptions -> p -> ClashDoc
ppr' opts = runIdentity . pprM' opts

fromPpr :: PrettyPrec a => a -> Doc ()
fromPpr = removeAnnotations . ppr

noPrec, opPrec, appPrec :: Num a => a
noPrec = 0
opPrec = 1
appPrec = 2

-- | Print a PrettyPrec thing to a String
showPpr :: PrettyPrec p => p -> String
showPpr = showPpr' def

showPpr' :: PrettyPrec p => PrettyOptions -> p -> String
showPpr' opts = showDoc . ppr' opts

tracePprId :: PrettyPrec p => p -> p
tracePprId p = trace (showPpr p) p

tracePpr :: PrettyPrec p => p -> a -> a
tracePpr p a = trace (showPpr p) a

tracePprM :: (Monad m, PrettyPrec p) => p -> m ()
tracePprM p = traceM (showPpr p)

parensIf :: Bool -> ClashDoc -> ClashDoc
parensIf False = id
parensIf True  = parens

tyParens :: ClashDoc -> ClashDoc
tyParens = enclose (annotate (AnnSyntax Type) lparen)
                   (annotate (AnnSyntax Type) rparen)

tyParensIf :: Bool -> ClashDoc -> ClashDoc
tyParensIf False = id
tyParensIf True  = tyParens

vsepHard :: [ClashDoc] -> ClashDoc
vsepHard = concatWith (\x y -> x <> hardline <> y)

viewName :: Name a -> (Text, Text, Text)
viewName n = (qual, occ, T.pack $ show $ nameUniq n)
  where (qual, occ) = T.breakOnEnd "." $ nameOcc n

instance PrettyPrec (Name a) where
  pprPrec p (viewName -> (qual, occ, uniq)) = do
    qual' <- annotate (AnnSyntax Qualifier) <$> pprPrec p qual
    occ'  <- pprPrec p occ
    uniq' <- annotate (AnnSyntax Unique) . brackets <$> (pprPrec p uniq)
    return $ qual' <> occ' <> uniq'

instance ClashPretty (Name a) where
  clashPretty = fromPpr

instance PrettyPrec a => PrettyPrec [a] where
  pprPrec prec = fmap vcat . mapM (pprPrec prec)

instance PrettyPrec (Id, Term) where
  pprPrec _ = pprTopLevelBndr

pprTopLevelBndr :: Monad m => (Id,Term) -> m ClashDoc
pprTopLevelBndr (bndr,expr) = do
  bndr'    <- pprM bndr
  bndrName <- pprM (varName bndr)
  expr'    <- pprM expr
  return $ bndr' <> line <> hang 2 (sep [(bndrName <+> equals), expr']) <> line

dcolon, rarrow, lam, tylam, at, cast, coerce, letrec, in_, case_, of_, forall_
  :: ClashDoc
[dcolon, rarrow, lam, tylam, at, cast, coerce, letrec, in_, case_, of_, forall_]
  = annotate (AnnSyntax Keyword) <$>
    ["::", "->", "λ", "Λ", "@", "▷", "~", "letrec", "in", "case", "of", "forall"]

instance PrettyPrec Text where
  pprPrec _ = pure . pretty

instance PrettyPrec Type where
  pprPrec _ t = annotate (AnnSyntax Type) <$> pprType t

instance ClashPretty Type where
  clashPretty = fromPpr

instance PrettyPrec TyCon where
  pprPrec _ t = pprM (tyConName t)

instance Pretty LitTy where
  pretty (NumTy i) = pretty i
  pretty (SymTy s) = dquotes $ pretty s

instance PrettyPrec LitTy where
  pprPrec _ = return . annotate (AnnSyntax LitS) . pretty

instance PrettyPrec Term where
  pprPrec prec e = case e of
    Var x           -> do
      v <- pprPrec prec (varName x)
      s <- pprPrecIdScope x
      pure (v <> brackets s)
    Data dc         -> pprPrec prec dc
    Literal l       -> pprPrec prec l
    Prim p          -> pprPrecPrim prec (primName p)
    Lam  v e1       -> annotate (AnnContext $ LamBody v) <$>
                         pprPrecLam prec [v] e1
    TyLam tv e1     -> annotate (AnnContext $ TyLamBody tv) <$>
                         pprPrecTyLam prec [tv] e1
    App fun arg     -> pprPrecApp prec fun arg
    TyApp e' ty     -> annotate (AnnContext TyAppC) <$>
                         pprPrecTyApp prec e' ty
    Letrec xes e1   -> pprPrecLetrec prec xes e1
    Case e' _ alts  -> pprPrecCase prec e' alts
    Cast e' ty1 ty2 -> pprPrecCast prec e' ty1 ty2
    Tick t e'       -> do
      tDoc <- pprPrec prec t
      eDoc <- pprPrec prec e'
      return (tDoc <> line' <> eDoc)

instance PrettyPrec TickInfo where
  pprPrec prec (SrcSpan sp)   = pprPrec prec sp
  pprPrec prec (NameMod PrefixName t) = ("<prefixName>" <>) <$> pprPrec prec t
  pprPrec prec (NameMod SuffixName t) = ("<suffixName>" <>) <$> pprPrec prec t
  pprPrec prec (NameMod SuffixNameP t) = ("<suffixNameP>" <>) <$> pprPrec prec t
  pprPrec prec (NameMod SetName t)    = ("<setName>" <>) <$> pprPrec prec t
  pprPrec _    DeDup                  = pure "<deDup>"
  pprPrec _    NoDeDup                = pure "<noDeDup>"

instance PrettyPrec SrcSpan where
  pprPrec _ sp = return ("<src>"<>pretty (GHC.showSDocUnsafe (GHC.ppr sp)))

instance ClashPretty Term where
  clashPretty = fromPpr

data BindingSite = LambdaBind | CaseBind | LetBind

instance PrettyPrec (Var a) where
  pprPrec _ v@(TyVar {}) = pprM $ varName v
  pprPrec _ v@(Id {})    = do
    v'  <- pprM (varName v)
    ty' <- pprM (varType v)
    return $ v' <> (annotate (AnnSyntax Type) $ align (space <> dcolon <+> ty'))

instance ClashPretty (Var a) where
  clashPretty = fromPpr

instance PrettyPrec DataCon where
  pprPrec _ = pprM . dcName

instance PrettyPrec Literal where
  pprPrec _ l = return $ annotate (AnnSyntax LitS) $ case l of
    IntegerLiteral i
      | i < 0          -> parens (pretty i)
      | otherwise      -> pretty i
    IntLiteral i
      | i < 0          -> parens (pretty i)
      | otherwise      -> pretty i
    Int64Literal i
      | i < 0          -> parens (pretty i)
      | otherwise      -> pretty i
    WordLiteral w      -> pretty w
    Word64Literal w    -> pretty w
    FloatLiteral r     -> pretty (fromRat r :: Float)
    DoubleLiteral r    -> pretty (fromRat r :: Double)
    CharLiteral c      -> pretty c
    StringLiteral s    -> vcat $ map pretty $ showMultiLineString s
    NaturalLiteral n   -> pretty n
    ByteArrayLiteral s -> pretty $ show s

instance PrettyPrec Pat where
  pprPrec prec pat = case pat of
    DataPat dc txs xs -> do
      dc'  <- pprM dc
      txs' <- mapM (pprBndr LetBind) txs
      xs'  <- mapM (pprBndr CaseBind) xs
      return $ parensIf (prec >= appPrec) $
        sep [ hsep (dc':txs')
            , nest 2 (sep xs') ]
    LitPat l   -> pprM l
    DefaultPat -> return "_"

pprPrecIdScope :: Monad m => Var a -> m ClashDoc
pprPrecIdScope (TyVar {}) = pure "TyVar"
pprPrecIdScope (Id _ _ _ GlobalId) = pure "GlobalId"
pprPrecIdScope (Id _ _ _ LocalId) = pure "LocalId"

pprPrecPrim :: Monad m => Rational -> Text -> m ClashDoc
pprPrecPrim prec nm =
  (<>) <$> (annotate (AnnSyntax Qualifier) <$> pprPrec prec qual)
       <*> pprPrec prec occ
  where (qual, occ) = T.breakOnEnd "." nm

pprPrecLam :: Monad m => Rational -> [Id] -> Term -> m ClashDoc
pprPrecLam prec xs e = do
  xs' <- mapM (pprBndr LambdaBind) xs
  e'  <- pprPrec noPrec e
  return $ parensIf (prec > noPrec) $
    lam <> hsep xs' <+> rarrow <> line <> e'

pprPrecTyLam :: Monad m => Rational -> [TyVar] -> Term -> m ClashDoc
pprPrecTyLam prec tvs e = do
  tvs' <- mapM pprM tvs
  e'   <- pprPrec noPrec e
  return $ tyParensIf (prec > noPrec) $
    annotate (AnnSyntax Type) (tylam <> hsep tvs' <+> rarrow <> line) <> e'

pprPrecApp :: Monad m => Rational -> Term -> Term -> m ClashDoc
pprPrecApp prec e1 e2 = do
  e1' <- annotate (AnnContext AppFun) <$> pprPrec opPrec e1
  e2' <- annotate (AnnContext $ AppArg $ primArg e2) <$> pprPrec appPrec e2
  return $ parensIf (prec >= appPrec) $
    hang 2 (sep [e1',e2'])

pprPrecTyApp :: Monad m => Rational -> Term -> Type -> m ClashDoc
pprPrecTyApp prec e ty = do
  e'  <- pprPrec opPrec e
  ty' <- pprParendType ty
  return $ tyParensIf (prec >= appPrec) $
    hang 2 $ group $
      e' <> annotate (AnnSyntax Type) (line <> at <> ty')

pprPrecCast :: Monad m => Rational -> Term -> Type -> Type -> m ClashDoc
pprPrecCast prec e ty1 ty2 = do
  e'   <- annotate (AnnContext CastBody) <$> pprPrec appPrec e
  ty1' <- pprType ty1
  ty2' <- pprType ty2
  return $ tyParensIf (prec >= appPrec) $
    e' <> annotate (AnnSyntax Type)
                   (softline <> nest 2 (vsep [cast, ty1', coerce, ty2']))

pprPrecLetrec :: Monad m => Rational -> [(Id, Term)] -> Term -> m ClashDoc
pprPrecLetrec prec xes body = do
  let bndrs = fst <$> xes
  body' <- annotate (AnnContext $ LetBody bndrs) <$> pprPrec noPrec body
  xes'  <- mapM (\(x,e) -> do
                  x' <- pprBndr LetBind x
                  e' <- pprPrec noPrec e
                  return $ annotate (AnnContext $ LetBinding x bndrs) $
                    vsepHard [x', equals <+> e']
                ) xes
  let xes'' = case xes' of { [] -> ["EmptyLetrec"]; _  -> xes' }
  return $ parensIf (prec > noPrec) $
    vsepHard [hang 2 (vsepHard $ letrec : xes''), in_ <+> body']

pprPrecCase :: Monad m => Rational -> Term -> [(Pat,Term)] -> m ClashDoc
pprPrecCase prec e alts = do
  e'    <- annotate (AnnContext CaseScrut) <$> pprPrec prec e
  alts' <- mapM (pprPrecAlt noPrec) alts
  return $ parensIf (prec > noPrec) $
    hang 2 $ vsepHard $ (case_ <+> e' <+> of_) : alts'

pprPrecAlt :: Monad m => Rational -> (Pat,Term) -> m ClashDoc
pprPrecAlt _ (altPat, altE) = do
  altPat' <- pprPrec noPrec altPat
  altE'   <- pprPrec noPrec altE
  return $ annotate (AnnContext $ CaseAlt altPat) $
    hang 2 $ vsepHard [(altPat' <+> rarrow), altE']

pprBndr :: (Monad m, PrettyPrec a) => BindingSite -> a -> m ClashDoc
pprBndr LetBind = pprM
pprBndr _       = fmap tyParens . pprM

data TypePrec = TopPrec | FunPrec | TyConPrec deriving (Eq,Ord)

maybeParen :: TypePrec -> TypePrec -> ClashDoc -> ClashDoc
maybeParen ctxt_prec inner_prec = parensIf (ctxt_prec >= inner_prec)

pprType :: Monad m => Type -> m ClashDoc
pprType = ppr_type TopPrec

pprParendType :: Monad m => Type -> m ClashDoc
pprParendType = ppr_type TyConPrec

ppr_type :: Monad m => TypePrec -> Type -> m ClashDoc
ppr_type _ (VarTy tv)                   = pprM tv
ppr_type _ (LitTy tyLit)                = pprM tyLit
ppr_type p ty@(ForAllTy {})             = pprForAllType p ty
ppr_type p (ConstTy (TyCon tc))         = pprTcApp p ppr_type tc []
ppr_type p (AnnType _ann typ)           = ppr_type p typ
ppr_type p (tyView -> TyConApp tc args) = pprTcApp p ppr_type tc args
ppr_type p (tyView -> FunTy ty1 ty2)
  = pprArrowChain <$> ppr_type FunPrec ty1 <:> pprFunTail ty2
  where
    pprFunTail (tyView -> FunTy ty1' ty2')
      = ppr_type FunPrec ty1' <:> pprFunTail ty2'
    pprFunTail otherTy
      = ppr_type TopPrec otherTy <:> pure []

    pprArrowChain []
      = emptyDoc
    pprArrowChain (arg:args)
      = maybeParen p FunPrec $ sep [arg, sep (map (rarrow <+>) args)]

ppr_type p (AppTy ty1 ty2) = maybeParen p TyConPrec <$> ((<+>) <$> pprType ty1
                                                               <*> ppr_type TyConPrec ty2)
ppr_type _ (ConstTy Arrow) = return (parens rarrow)

pprForAllType :: Monad m => TypePrec -> Type -> m ClashDoc
pprForAllType p ty = maybeParen p FunPrec <$> pprSigmaType True ty

pprSigmaType :: Monad m => Bool -> Type -> m ClashDoc
pprSigmaType showForalls ty = do
    (tvs, rho)     <- split1 [] ty
    sep <$> sequenceA [ if showForalls then pprForAll tvs else pure emptyDoc
                      , pprType rho
                      ]
  where
    split1 tvs (ForAllTy tv resTy) = split1 (tv:tvs) resTy
    split1 tvs resTy               = return (reverse tvs,resTy)

pprForAll :: Monad m => [TyVar] -> m ClashDoc
pprForAll []  = return emptyDoc
pprForAll tvs = do
  tvs' <- mapM pprTvBndr tvs
  return $ forall_ <+> sep tvs' <> dot

pprTvBndr :: Monad m => TyVar -> m ClashDoc
pprTvBndr tv = do
  tv'   <- pprM tv
  kind' <- pprKind (varType tv)
  return $ tyParens $ tv' <> (annotate (AnnSyntax Type) $ space <> dcolon <+> kind')

pprKind :: Monad m => Kind -> m ClashDoc
pprKind = pprType

pprTcApp :: Monad m => TypePrec -> (TypePrec -> Type -> m ClashDoc)
  -> TyConName -> [Type] -> m ClashDoc
pprTcApp p pp tc tys
  | null tys
  = pprM tc

  | isTupleTyConLike tc
  = do tys' <- mapM (pp TopPrec) tys
       return $ parens $ sep $ punctuate comma tys'

  | isSym
  , [ty1, ty2] <- tys
  = do ty1' <- pp FunPrec ty1
       ty2' <- pp FunPrec ty2
       tc' <- pprM tc
       return $ maybeParen p FunPrec $
         sep [ty1', enclose "`" "`" tc' <+> ty2']

  | otherwise
  = do tys' <- mapM (pp TyConPrec) tys
       tc' <- parensIf isSym <$> pprM tc
       return $ maybeParen p TyConPrec $
         hang 2 $ sep (tc':tys')

  where isSym = isSymName tc

isSymName :: Name a -> Bool
isSymName n = go (nameOcc n)
  where
    go s | T.null s           = False
         | isUpper $ T.head s = isLexConSym s
         | otherwise          = isLexSym s

isLexSym :: Text -> Bool
isLexSym cs = isLexConSym cs || isLexVarSym cs

isLexConSym :: Text -> Bool
isLexConSym "->" = True
isLexConSym cs   = startsConSym (T.head cs)

isLexVarSym :: Text -> Bool
isLexVarSym cs = startsVarSym (T.head cs)

startsConSym :: Char -> Bool
startsConSym c = c == ':'

startsVarSym :: Char -> Bool
startsVarSym c = isSymbolASCII c || (ord c > 0x7f && isSymbol c)

isSymbolASCII :: Char -> Bool
isSymbolASCII c = c `elem` ("!#$%&*+./<=>?@\\^|~-" :: String)
