{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  PrettyPrec printing class and instances for CoreHW
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Core.Pretty
  ( PrettyPrec (..)
  , PrettyOptions (..), defPrettyOptions
  , ppr, ppr'
  , showDoc
  , showPpr, showPpr'
  , tracePprId
  , tracePpr
  )
where

import Data.Char                        (isSymbol, isUpper, ord)
import Data.Text                        (Text)
import Control.Monad.Identity
import qualified Data.Text              as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Debug.Trace                      (trace)
import GHC.Show                         (showMultiLineString)
import Numeric                          (fromRat)

import Clash.Core.DataCon               (DataCon (..))
import Clash.Core.Literal               (Literal (..))
import Clash.Core.Name                  (Name (..))
import Clash.Core.Term                  (Pat (..), Term (..))
import Clash.Core.TyCon                 (TyCon (..), TyConName, isTupleTyConLike)
import Clash.Core.Type                  (ConstTy (..), Kind, LitTy (..),
                                         Type (..), TypeView (..), tyView)
import Clash.Core.Var                   (Id, TyVar, Var (..))
import Clash.Util

-- | PrettyPrec printing Show-like typeclass
class PrettyPrec p where
  pprPrec :: Monad m => PrettyOptions -> Rational -> p -> m (Doc ann)

data PrettyOptions = PrettyOptions
  { displayUniques    :: Bool
  , displayTypes      :: Bool
  , displayQualifiers :: Bool
  }

defPrettyOptions :: PrettyOptions
defPrettyOptions = PrettyOptions True True True

pprM :: (Monad m, PrettyPrec p) => PrettyOptions -> p -> m (Doc ann)
pprM opts = pprPrec opts 0

ppr :: PrettyPrec p => p -> Doc ann
ppr = runIdentity . pprM defPrettyOptions

ppr' :: PrettyPrec p => PrettyOptions -> p -> Doc ann
ppr' opts = runIdentity . pprM opts

noPrec, opPrec, appPrec :: Num a => a
noPrec = 0
opPrec = 1
appPrec = 2

-- | Print a PrettyPrec thing to a String
showDoc :: Doc ann -> String
showDoc = renderString . layoutPretty (LayoutOptions (AvailablePerLine 80 0.6))

showPpr :: PrettyPrec p => p -> String
showPpr = showDoc . ppr

showPpr' :: PrettyPrec p => PrettyOptions -> p -> String
showPpr' opts = showDoc . ppr' opts

tracePprId :: PrettyPrec p => p -> p
tracePprId p = trace (showPpr p) p

tracePpr :: PrettyPrec p => p -> a -> a
tracePpr p a = trace (showPpr p) a

prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen False = id
prettyParen True  = parens

removeQualifiers :: PrettyOptions -> Text -> Text
removeQualifiers opts | displayQualifiers opts = id
                      | otherwise              = snd . T.breakOnEnd "."

nameOcc' :: PrettyOptions -> Name a -> Text
nameOcc' opts = removeQualifiers opts . nameOcc

instance PrettyPrec (Name a) where
  pprPrec opts p n
    | displayUniques opts
    = pprPrec opts p (nameOcc' opts n `T.append` T.pack (show (nameUniq n)))
    | otherwise
    = pprPrec opts p (nameOcc' opts n)

instance PrettyPrec a => PrettyPrec [a] where
  pprPrec opts prec xs = do
    xs' <- mapM (pprPrec opts prec) xs
    return $ vcat xs'

instance PrettyPrec (Id, Term) where
  pprPrec opts _ = pprTopLevelBndr opts

pprTopLevelBndr :: Monad m => PrettyOptions -> (Id,Term) -> m (Doc ann)
pprTopLevelBndr opts (bndr,expr) = do
  bndr' <- pprM opts bndr
  bndrName <- pprM opts (varName bndr)
  expr' <- pprM opts expr
  return $ bndr' <> line <> hang 2 (sep [(bndrName <+> equals), expr']) <> line

dcolon :: (Doc ann)
dcolon = "::"

rarrow :: (Doc ann)
rarrow = "->"

instance PrettyPrec Text where
  pprPrec opts _ = pure . pretty . removeQualifiers opts

instance PrettyPrec Type where
  pprPrec opts _ t | displayTypes opts = pprType opts t
                   | otherwise         = pure emptyDoc

instance Pretty Type where
  pretty = ppr

instance PrettyPrec TyCon where
  pprPrec opts _ tc = pprM opts $ tyConName tc

instance PrettyPrec LitTy where
  pprPrec _ _ (NumTy i) = return $ pretty i
  pprPrec _ _ (SymTy s) = return $ dquotes $ pretty s

instance PrettyPrec Term where
  pprPrec opts prec e = case e of
    Var x           -> pprPrec opts prec (varName x)
    Data dc         -> pprPrec opts prec dc
    Literal l       -> pprPrec opts prec l
    Prim nm _       -> pprPrec opts prec nm
    Lam v e1        -> pprPrecLam opts prec [v] e1
    TyLam tv e1     -> pprPrecTyLam opts prec [tv] e1
    App fun arg     -> pprPrecApp opts prec fun arg
    TyApp e' ty     -> pprPrecTyApp opts prec e' ty
    Letrec xes e1   -> pprPrecLetrec opts prec xes e1
    Case e' _ alts  -> pprPrecCase opts prec e' alts
    Cast e' ty1 ty2 -> pprPrecCast opts prec e' ty1 ty2

instance Pretty Term where
  pretty = ppr

data BindingSite
  = LambdaBind
  | CaseBind
  | LetBind

instance PrettyPrec (Var a) where
  pprPrec opts _ v@(TyVar {}) = pprM opts $ varName v
  pprPrec opts _ v@(Id {})
    | displayTypes opts
    = do v'  <- pprM opts (varName v)
         ty' <- pprM opts (varType v)
         return $ v' <+> align (dcolon <+> ty')
    | otherwise
    = pprM opts (varName v)

instance Pretty (Var a) where
  pretty = ppr

instance PrettyPrec DataCon where
  pprPrec opts _ dc = pprM opts $ dcName dc

instance PrettyPrec Literal where
  pprPrec _ _ l = case l of
    IntegerLiteral i
      | i < 0         -> return $ parens (pretty i)
      | otherwise     -> return $ pretty i
    IntLiteral i
      | i < 0         -> return $ parens (pretty i)
      | otherwise     -> return $ pretty i
    Int64Literal i
      | i < 0         -> return $ parens (pretty i)
      | otherwise     -> return $ pretty i
    WordLiteral w     -> return $ pretty w
    Word64Literal w   -> return $ pretty w
    FloatLiteral r    -> return $ pretty (fromRat r :: Float)
    DoubleLiteral r   -> return $ pretty (fromRat r :: Double)
    CharLiteral c     -> return $ pretty c
    StringLiteral s   -> return $ vcat $ map pretty $ showMultiLineString s
    NaturalLiteral n  -> return $ pretty n
    ByteArrayLiteral s -> return $ pretty $ show s

instance PrettyPrec Pat where
  pprPrec opts prec pat = case pat of
    DataPat dc txs xs -> do
      dc'  <- pprM opts dc
      txs' <- mapM (pprBndr opts LetBind) txs
      xs'  <- mapM (pprBndr opts CaseBind) xs
      return $ prettyParen (prec >= appPrec) $ dc' <+> hsep txs' <> softline <> (nest 2 (vcat xs'))
    LitPat l   -> pprM opts l
    DefaultPat -> return $ pretty '_'

pprPrecLam :: Monad m => PrettyOptions -> Rational -> [Id] -> Term -> m (Doc ann)
pprPrecLam opts prec xs e = do
  xs' <- mapM (pprBndr opts LambdaBind) xs
  e'  <- pprPrec opts noPrec e
  return $ prettyParen (prec > noPrec) $
    pretty 'λ' <> hsep xs' <+> rarrow <> line <> e'

pprPrecTyLam :: Monad m => PrettyOptions -> Rational -> [TyVar] -> Term -> m (Doc ann)
pprPrecTyLam opts prec tvs e
  | displayTypes opts
  = do tvs' <- mapM (pprM opts) tvs
       e'   <- pprPrec opts noPrec e
       return $ prettyParen (prec > noPrec) $
         pretty 'Λ' <> hsep tvs' <+> rarrow <> line <> e'
  | otherwise
  = pprPrec opts prec e

pprPrecApp :: Monad m => PrettyOptions -> Rational -> Term -> Term -> m (Doc ann)
pprPrecApp opts prec e1 e2 = do
  e1' <- pprPrec opts opPrec e1
  e2' <- pprPrec opts appPrec e2
  return $ prettyParen (prec >= appPrec) $
    hang 2 (vsep [e1',e2'])

pprPrecTyApp :: Monad m => PrettyOptions -> Rational -> Term -> Type -> m (Doc ann)
pprPrecTyApp opts prec e ty
  | displayTypes opts
  = do e' <- pprPrec opts opPrec e
       ty' <- pprParendType opts ty
       return $ prettyParen (prec >= appPrec) $
         hang 2 (sep [e', (pretty '@' <> ty')])
  | otherwise
  = pprPrec opts prec e

-- TODO use more conventional cast operator (|> or ▷) ?
pprPrecCast :: Monad m => PrettyOptions -> Rational -> Term -> Type -> Type -> m (Doc ann)
pprPrecCast opts prec e ty1 ty2
  | displayTypes opts
  = do e' <- pprPrec opts appPrec e
       ty1' <- pprType opts ty1
       ty2' <- pprType opts ty2
       return $ prettyParen (prec >= appPrec) $
         parens ("cast" <> softline <> nest 5 (vcat [dcolon <+> ty1', rarrow <+> ty2']))
           <> softline <> nest 2 e'
  | otherwise
  = pprPrec opts prec e

pprPrecLetrec :: Monad m => PrettyOptions -> Rational -> [(Id, Term)] -> Term -> m (Doc ann)
pprPrecLetrec opts prec xes body = do
  body' <- pprPrec opts noPrec body
  xes'  <- mapM (\(x,e) -> do
                  x' <- pprBndr opts LetBind x
                  e' <- pprPrec opts noPrec e
                  return $ x' <> line <> equals <+> e'
                ) xes
  let xes'' = case xes' of
                [] -> ["EmptyLetrec"]
                _  -> xes'
  return $ prettyParen (prec > noPrec) $
    hang 2 (vcat ("letrec":xes'')) <> line <> "in" <+> body'

pprPrecCase :: Monad m => PrettyOptions -> Rational -> Term -> [(Pat,Term)] -> m (Doc ann)
pprPrecCase opts prec e alts = do
  e' <- pprPrec opts prec e
  alts' <- mapM (pprPrecAlt opts noPrec) alts
  return $ prettyParen (prec > noPrec) $
    hang 2 (vcat (("case" <+> e' <+> "of"):alts'))

pprPrecAlt :: Monad m => PrettyOptions -> Rational -> (Pat,Term) -> m (Doc ann)
pprPrecAlt opts _ (altPat, altE) = do
  altPat' <- pprPrec opts noPrec altPat
  altE'   <- pprPrec opts noPrec altE
  return $ hang 2 (vcat [(altPat' <+> rarrow), altE'])

pprBndr :: (Monad m, PrettyPrec a) => PrettyOptions -> BindingSite -> a -> m (Doc ann)
pprBndr opts bs x = prettyParen needsParen <$> pprM opts x
  where
    needsParen = case bs of
      LambdaBind -> displayTypes opts
      CaseBind   -> True
      LetBind    -> False

data TypePrec
  = TopPrec
  | FunPrec
  | TyConPrec
  deriving (Eq,Ord)

maybeParen :: TypePrec -> TypePrec -> (Doc ann) -> (Doc ann)
maybeParen ctxt_prec inner_prec = prettyParen (ctxt_prec >= inner_prec)

pprType :: Monad m => PrettyOptions -> Type -> m (Doc ann)
pprType opts = ppr_type opts TopPrec

pprParendType :: Monad m => PrettyOptions -> Type -> m (Doc ann)
pprParendType opts = ppr_type opts TyConPrec

ppr_type :: Monad m => PrettyOptions -> TypePrec -> Type -> m (Doc ann)
ppr_type opts _ (VarTy tv)                   = pprM opts tv
ppr_type opts _ (LitTy tyLit)                = pprM opts tyLit
ppr_type opts p ty@(ForAllTy {})             = pprForAllType opts p ty
ppr_type opts p (ConstTy (TyCon tc))         = pprTcApp opts p (ppr_type opts) tc []
ppr_type opts p (AnnType _ann typ)           = ppr_type opts p typ
ppr_type opts p (tyView -> TyConApp tc args) = pprTcApp opts p (ppr_type opts) tc args
ppr_type opts p (tyView -> FunTy ty1 ty2)    = pprArrowChain p <$> ppr_type opts FunPrec ty1 <:> pprFunTail ty2
  where
    pprFunTail (tyView -> FunTy ty1' ty2') = ppr_type opts FunPrec ty1' <:> pprFunTail ty2'
    pprFunTail otherTy                     = ppr_type opts TopPrec otherTy <:> pure []

ppr_type opts p (AppTy ty1 ty2) = maybeParen p TyConPrec <$> ((<+>) <$> pprType opts ty1
                                                                    <*> ppr_type opts TyConPrec ty2)
ppr_type _    _ (ConstTy Arrow) = return (parens rarrow)

pprForAllType :: Monad m => PrettyOptions -> TypePrec -> Type -> m (Doc ann)
pprForAllType opts p ty = maybeParen p FunPrec <$> pprSigmaType opts True ty

pprSigmaType :: Monad m => PrettyOptions -> Bool -> Type -> m (Doc ann)
pprSigmaType opts showForalls ty = do
    (tvs, rho)     <- split1 [] ty
    sep <$> sequenceA [ if showForalls then pprForAll opts tvs else pure emptyDoc
                      , pprType opts rho
                      ]
  where
    split1 tvs (ForAllTy tv resTy) = split1 (tv:tvs) resTy
    split1 tvs resTy               = return (reverse tvs,resTy)

pprForAll :: Monad m => PrettyOptions -> [TyVar] -> m (Doc ann)
pprForAll _    []  = return emptyDoc
pprForAll opts tvs = do
  tvs' <- mapM (pprTvBndr opts) tvs
  return $ pretty '∀' <+> sep tvs' <> dot

pprTvBndr :: Monad m => PrettyOptions -> TyVar -> m (Doc ann)
pprTvBndr opts tv
  | displayTypes opts
  = do tv'   <- pprM opts tv
       kind' <- pprKind opts (varType tv)
       return $ parens (tv' <+> dcolon <+> kind')
  | otherwise
  = pprM opts tv

pprKind :: Monad m => PrettyOptions -> Kind -> m (Doc ann)
pprKind opts = pprType opts

pprTcApp :: Monad m => PrettyOptions -> TypePrec -> (TypePrec -> Type -> m (Doc ann))
  -> TyConName -> [Type] -> m (Doc ann)
pprTcApp opts _ _  tc []
  = return . pretty $ nameOcc' opts tc

pprTcApp opts p pp tc tys
  | isTupleTyConLike tc
  = do
    tys' <- mapM (pp TopPrec) tys
    return $ parens $ sep $ punctuate comma tys'

  | otherwise
  = pprTypeNameApp opts p pp tc tys

pprTypeNameApp :: Monad m => PrettyOptions -> TypePrec -> (TypePrec -> Type -> m (Doc ann))
  -> Name a -> [Type] -> m (Doc ann)
pprTypeNameApp opts p pp name tys
  | isSym
  , [ty1,ty2] <- tys
  = pprInfixApp opts p pp name ty1 ty2
  | otherwise
  = do
    tys' <- mapM (pp TyConPrec) tys
    let name' = pretty $ nameOcc' opts name
    return $ pprPrefixApp p (pprPrefixVar isSym name') tys'
  where
    isSym = isSymName name

pprInfixApp :: Monad m => PrettyOptions -> TypePrec -> (TypePrec -> Type -> m (Doc ann))
  -> Name a -> Type -> Type -> m (Doc ann)
pprInfixApp opts p pp name ty1 ty2 = do
  ty1'  <- pp FunPrec ty1
  ty2'  <- pp FunPrec ty2
  let name' = pretty $ nameOcc' opts name
  return $ maybeParen p FunPrec $ sep [ty1', pprInfixVar True name' <+> ty2']

pprPrefixApp :: TypePrec -> (Doc ann) -> [(Doc ann)] -> (Doc ann)
pprPrefixApp p pp_fun pp_tys = maybeParen p TyConPrec $
                                 hang 2 (sep (pp_fun:pp_tys))

pprPrefixVar :: Bool -> (Doc ann) -> (Doc ann)
pprPrefixVar is_operator pp_v
  | is_operator = parens pp_v
  | otherwise   = pp_v

pprInfixVar :: Bool -> (Doc ann) -> (Doc ann)
pprInfixVar is_operator pp_v
  | is_operator = pp_v
  | otherwise   = pretty '`' <> pp_v <> pretty '`'

pprArrowChain :: TypePrec -> [(Doc ann)] -> (Doc ann)
pprArrowChain _ []         = emptyDoc
pprArrowChain p (arg:args) = maybeParen p FunPrec $
                               sep [arg, sep (map (rarrow <+>) args)]

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
