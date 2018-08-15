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
  , ppr
  , showDoc
  , showPpr
  )
where

import Data.Char                        (isSymbol, isUpper, ord)
import Data.Text                        (Text)
import Control.Monad.Identity
import qualified Data.Text              as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
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
  pprPrec :: Monad m => Rational -> p -> m (Doc ann)

pprM :: (Monad m, PrettyPrec p) => p -> m (Doc ann)
pprM = pprPrec 0

ppr :: PrettyPrec p => p -> Doc ann
ppr = runIdentity . pprM

lunbind
  :: (a,b)
  -> ((a,b) -> r)
  -> r
lunbind a f = f a

noPrec, opPrec, appPrec :: Num a => a
noPrec = 0
opPrec = 1
appPrec = 2

-- | Print a PrettyPrec thing to a String
showDoc :: Doc ann -> String
showDoc = renderString . layoutPretty (LayoutOptions (AvailablePerLine 80 0.6))

showPpr :: PrettyPrec p => p -> String
showPpr = showDoc . ppr

prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen False = id
prettyParen True  = parens

instance PrettyPrec (Name a) where
  pprPrec p n = pprPrec p (nameOcc n `T.append` T.pack (show (nameUniq n)))

instance PrettyPrec a => PrettyPrec [a] where
  pprPrec prec xs = do
    xs' <- mapM (pprPrec prec) xs
    return $ vcat xs'

instance PrettyPrec (Id, Term) where
  pprPrec _ = pprTopLevelBndr

pprTopLevelBndr :: Monad m => (Id,Term) -> m (Doc ann)
pprTopLevelBndr (bndr,expr) = do
  bndr' <- pprM bndr
  bndrName <- pprM (varName bndr)
  expr' <- pprM expr
  return $ bndr' <> line <> hang 2 (sep [(bndrName <+> equals), expr']) <> line

dcolon :: (Doc ann)
dcolon = "::"

rarrow :: (Doc ann)
rarrow = "->"

instance PrettyPrec Text where
  pprPrec _ = pure . pretty

instance PrettyPrec Type where
  pprPrec _ = pprType

instance Pretty Type where
  pretty = ppr

instance PrettyPrec TyCon where
  pprPrec _ tc = pprM $ tyConName tc

instance PrettyPrec LitTy where
  pprPrec _ (NumTy i) = return $ pretty i
  pprPrec _ (SymTy s) = return $ pretty s

instance PrettyPrec Term where
  pprPrec prec e = case e of
    Var x        -> pprPrec prec (varName x)
    Data dc      -> pprPrec prec dc
    Literal l    -> pprPrec prec l
    Prim nm _    -> return $ pretty nm
    Lam  v e1    -> lunbind (v,e1) $ \(v1,e2)  -> pprPrecLam prec [v1] e2
    TyLam v e1   -> lunbind (v,e1) $ \(tv,e2) -> pprPrecTyLam prec [tv] e2
    App fun arg  -> pprPrecApp prec fun arg
    TyApp e' ty  -> pprPrecTyApp prec e' ty
    Letrec xes e1  -> lunbind (xes,e1) $ \(xes1,e2) -> pprPrecLetrec prec xes1 e2
    Case e' _ alts -> pprPrecCase prec e' =<< mapM (`lunbind` return) alts
    Cast e' ty1 ty2-> pprPrecCast prec e' ty1 ty2

instance Pretty Term where
  pretty = ppr

data BindingSite
  = LambdaBind
  | CaseBind
  | LetBind

instance PrettyPrec (Var a) where
  pprPrec _ v@(TyVar {}) = pprM $ varName v
  pprPrec _ v@(Id {}) = do
    v'  <- pprM (varName v)
    ty' <- pprM (varType v)
    return $ v' <+> dcolon <+> ty'

instance Pretty (Var a) where
  pretty = ppr

instance PrettyPrec DataCon where
  pprPrec _ dc = pprM $ dcName dc

instance PrettyPrec Literal where
  pprPrec _ l = case l of
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
  pprPrec prec pat = case pat of
    DataPat dc txs xs -> do
      dc'  <- pprM  dc
      txs' <- mapM (pprBndr LetBind) txs
      xs'  <- mapM (pprBndr CaseBind) xs
      return $ prettyParen (prec >= appPrec) $ dc' <+> hsep txs' <> softline <> (nest 2 (vcat xs'))
    LitPat l   -> pprM l
    DefaultPat -> return $ pretty '_'

pprPrecLam :: Monad m => Rational -> [Id] -> Term -> m (Doc ann)
pprPrecLam prec xs e = do
  xs' <- mapM (pprBndr LambdaBind) xs
  e'  <- pprPrec noPrec e
  return $ prettyParen (prec > noPrec) $
    pretty 'λ' <> hsep xs' <+> rarrow <> line <> e'

pprPrecTyLam :: Monad m => Rational -> [TyVar] -> Term -> m (Doc ann)
pprPrecTyLam prec tvs e = do
  tvs' <- mapM pprM tvs
  e'   <- pprPrec noPrec e
  return $ prettyParen (prec > noPrec) $
    pretty 'Λ' <> hsep tvs' <+> rarrow <> line <> e'

pprPrecApp :: Monad m => Rational -> Term -> Term -> m (Doc ann)
pprPrecApp prec e1 e2 = do
  e1' <- pprPrec opPrec e1
  e2' <- pprPrec appPrec e2
  return $ prettyParen (prec >= appPrec) $
    hang 2 (vsep [e1',e2'])

pprPrecTyApp :: Monad m => Rational -> Term -> Type -> m (Doc ann)
pprPrecTyApp prec e ty = do
  e' <- pprPrec opPrec e
  ty' <- pprParendType ty
  return $ prettyParen (prec >= appPrec) $
    hang 2 (sep [e', (pretty '@' <> ty')])

-- TODO use more conventional cast operator (|> or ▷) ?
pprPrecCast :: Monad m => Rational -> Term -> Type -> Type -> m (Doc ann)
pprPrecCast prec e ty1 ty2 = do
  e' <- pprPrec appPrec e
  ty1' <- pprType ty1
  ty2' <- pprType ty2
  return $ prettyParen (prec >= appPrec) $
    parens ("cast" <> softline <> nest 5 (vcat [dcolon <+> ty1', rarrow <+> ty2']))
      <> softline <> nest 2 e'

pprPrecLetrec :: Monad m => Rational -> [(Id, Term)] -> Term -> m (Doc ann)
pprPrecLetrec prec xes body = do
  body' <- pprPrec noPrec body
  xes'  <- mapM (\(x,e) -> do
                  x' <- pprBndr LetBind x
                  e' <- pprPrec noPrec e
                  return $ x' <> line <> equals <+> e'
                ) xes
  let xes'' = case xes' of
                [] -> ["EmptyLetrec"]
                _  -> xes'
  return $ prettyParen (prec > noPrec) $
    hang 2 (vcat ("letrec":xes'')) <> line <> "in" <+> body'

pprPrecCase :: Monad m => Rational -> Term -> [(Pat,Term)] -> m (Doc ann)
pprPrecCase prec e alts = do
  e' <- pprPrec prec e
  alts' <- mapM (pprPrecAlt noPrec) alts
  return $ prettyParen (prec > noPrec) $
    hang 2 (vcat (("case" <+> e' <+> "of"):alts'))

pprPrecAlt :: Monad m => Rational -> (Pat,Term) -> m (Doc ann)
pprPrecAlt _ (altPat, altE) = do
  altPat' <- pprPrec noPrec altPat
  altE'   <- pprPrec noPrec altE
  return $ hang 2 (vcat [(altPat' <+> rarrow), altE'])

pprBndr :: (Monad m, PrettyPrec a) => BindingSite -> a -> m (Doc ann)
pprBndr bs x = prettyParen needsParen <$> pprM x
  where
    needsParen = case bs of
      LambdaBind -> True
      CaseBind   -> True
      LetBind    -> False

data TypePrec
  = TopPrec
  | FunPrec
  | TyConPrec
  deriving (Eq,Ord)

maybeParen :: TypePrec -> TypePrec -> (Doc ann) -> (Doc ann)
maybeParen ctxt_prec inner_prec = prettyParen (ctxt_prec >= inner_prec)

pprType :: Monad m => Type -> m (Doc ann)
pprType = ppr_type TopPrec

pprParendType :: Monad m => Type -> m (Doc ann)
pprParendType = ppr_type TyConPrec

ppr_type :: Monad m => TypePrec -> Type -> m (Doc ann)
ppr_type _ (VarTy tv)                   = pprM tv
ppr_type _ (LitTy tyLit)                = pprM tyLit
ppr_type p ty@(ForAllTy {})             = pprForAllType p ty
ppr_type p (ConstTy (TyCon tc))         = pprTcApp p ppr_type tc []
ppr_type p (AnnType _ann typ)           = ppr_type p typ
ppr_type p (tyView -> TyConApp tc args) = pprTcApp p ppr_type tc args
ppr_type p (tyView -> FunTy ty1 ty2)    = pprArrowChain p <$> ppr_type FunPrec ty1 <:> pprFunTail ty2
  where
    pprFunTail (tyView -> FunTy ty1' ty2') = ppr_type FunPrec ty1' <:> pprFunTail ty2'
    pprFunTail otherTy                     = ppr_type TopPrec otherTy <:> pure []

ppr_type p (AppTy ty1 ty2) = maybeParen p TyConPrec <$> ((<+>) <$> pprType ty1 <*> ppr_type TyConPrec ty2)
ppr_type _ (ConstTy Arrow) = return (parens rarrow)

pprForAllType :: Monad m => TypePrec -> Type -> m (Doc ann)
pprForAllType p ty = maybeParen p FunPrec <$> pprSigmaType True ty

pprSigmaType :: Monad m => Bool -> Type -> m (Doc ann)
pprSigmaType showForalls ty = do
    (tvs, rho)     <- split1 [] ty
    sep <$> sequenceA [ if showForalls then pprForAll tvs else pure emptyDoc
                      , pprType rho
                      ]
  where
    split1 tvs (ForAllTy tv resTy) =
      lunbind (tv,resTy) $ \(tv',resTy') -> split1 (tv':tvs) resTy'
    split1 tvs resTy = return (reverse tvs,resTy)

pprForAll :: Monad m => [TyVar] -> m (Doc ann)
pprForAll [] = return emptyDoc
pprForAll tvs = do
  tvs' <- mapM pprTvBndr tvs
  return $ pretty '∀' <+> sep tvs' <> dot

pprTvBndr :: Monad m => TyVar -> m (Doc ann)
pprTvBndr tv
  = do
      tv'   <- pprM tv
      kind' <- pprKind (varType tv)
      return $ parens (tv' <+> dcolon <+> kind')

pprKind :: Monad m => Kind -> m (Doc ann)
pprKind = pprType

pprTcApp :: Monad m => TypePrec -> (TypePrec -> Type -> m (Doc ann))
  -> TyConName -> [Type] -> m (Doc ann)
pprTcApp _ _  tc []
  = return . pretty $ nameOcc tc

pprTcApp p pp tc tys
  | isTupleTyConLike tc
  = do
    tys' <- mapM (pp TopPrec) tys
    return $ parens $ sep $ punctuate comma tys'

  | otherwise
  = pprTypeNameApp p pp tc tys

pprTypeNameApp :: Monad m => TypePrec -> (TypePrec -> Type -> m (Doc ann))
  -> Name a -> [Type] -> m (Doc ann)
pprTypeNameApp p pp name tys
  | isSym
  , [ty1,ty2] <- tys
  = pprInfixApp p pp name ty1 ty2
  | otherwise
  = do
    tys' <- mapM (pp TyConPrec) tys
    let name' = pretty $ nameOcc name
    return $ pprPrefixApp p (pprPrefixVar isSym name') tys'
  where
    isSym = isSymName name

pprInfixApp :: Monad m => TypePrec -> (TypePrec -> Type -> m (Doc ann))
  -> Name a -> Type -> Type -> m (Doc ann)
pprInfixApp p pp name ty1 ty2 = do
  ty1'  <- pp FunPrec ty1
  ty2'  <- pp FunPrec ty2
  let name' = pretty $ nameOcc name
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
