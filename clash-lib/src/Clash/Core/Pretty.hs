{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Pretty printing class and instances for CoreHW
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Core.Pretty
  ( Pretty (..)
  , showDoc
  )
where

import Data.Char                        (isSymbol, isUpper, ord)
import Data.Text                        (Text)
import Data.Text.Prettyprint.Doc        hiding (Doc, Pretty)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.String
import GHC.Show                         (showMultiLineString)
import Numeric                          (fromRat)
import Unbound.Generics.LocallyNameless
  (Embed (..), LFresh, lunbind, runLFreshM, unembed, unrebind, unrec)

import Clash.Core.DataCon               (DataCon (..))
import Clash.Core.Literal               (Literal (..))
import Clash.Core.Name                  (Name (..), OccName, name2String)
import Clash.Core.Term                  (Pat (..), Term (..))
import Clash.Core.TyCon                 (TyCon (..), TyConName, isTupleTyConLike)
import Clash.Core.Type                  (ConstTy (..), Kind, LitTy (..),
                                         Type (..), TypeView (..), tyView)
import Clash.Core.Var                   (Id, TyVar, Var, varKind, varName,
                                         varType)
import Clash.Util

type Doc = PP.Doc ()

-- | Pretty printing Show-like typeclass
class Pretty p where
  ppr :: LFresh m => p -> m Doc
  ppr = pprPrec 0

  pprPrec :: LFresh m => Rational -> p -> m Doc

noPrec, opPrec, appPrec :: Num a => a
noPrec = 0
opPrec = 1
appPrec = 2

-- | Print a Pretty thing to a String
showDoc :: Pretty p => p -> String
showDoc = renderString . layoutPretty (LayoutOptions (AvailablePerLine 80 0.6)) . runLFreshM . ppr

prettyParen :: Bool -> Doc -> Doc
prettyParen False = id
prettyParen True  = parens

instance Pretty (OccName a) where
  pprPrec _ = return . PP.pretty . show

instance Pretty (Name a) where
  pprPrec p = pprPrec p . nameOcc

instance Pretty a => Pretty [a] where
  pprPrec prec xs = do
    xs' <- mapM (pprPrec prec) xs
    return $ vcat xs'

instance Pretty (Id, Term) where
  pprPrec _ = pprTopLevelBndr

pprTopLevelBndr :: LFresh m => (Id,Term) -> m Doc
pprTopLevelBndr (bndr,expr) = do
  bndr' <- ppr bndr
  bndrName <- ppr (varName bndr)
  expr' <- ppr expr
  return $ bndr' <> line <> hang 2 (sep [(bndrName <+> equals), expr']) <> line

dcolon :: Doc
dcolon = PP.pretty "::"

rarrow :: Doc
rarrow = PP.pretty "->"

instance Pretty Text where
  pprPrec _ = pure . PP.pretty

instance Pretty Type where
  pprPrec _ = pprType

instance Pretty (Var Type) where
  pprPrec _ v = ppr $ varName v

instance Pretty TyCon where
  pprPrec _ tc = return . PP.pretty . name2String $ tyConName tc

instance Pretty LitTy where
  pprPrec _ (NumTy i) = return $ PP.pretty i
  pprPrec _ (SymTy s) = return $ PP.pretty s

instance Pretty Term where
  pprPrec prec e = case e of
    Var _ x      -> pprPrec prec x
    Data dc      -> pprPrec prec dc
    Literal l    -> pprPrec prec l
    Prim nm _    -> return $ PP.pretty nm
    Lam b        -> lunbind b $ \(v,e')  -> pprPrecLam prec [v] e'
    TyLam b      -> lunbind b $ \(tv,e') -> pprPrecTyLam prec [tv] e'
    App fun arg  -> pprPrecApp prec fun arg
    TyApp e' ty  -> pprPrecTyApp prec e' ty
    Letrec b     -> lunbind b $ \(xes,e') -> pprPrecLetrec prec (unrec xes) e'
    Case e' _ alts -> pprPrecCase prec e' =<< mapM (`lunbind` return) alts
    Cast e' ty1 ty2-> pprPrecCast prec e' ty1 ty2

data BindingSite
  = LambdaBind
  | CaseBind
  | LetBind

instance Pretty (Var Term) where
  pprPrec _ v = do
    v'  <- ppr (varName v)
    ty' <- ppr (unembed $ varType v)
    return $ v' <+> dcolon <+> ty'

instance Pretty DataCon where
  pprPrec _ dc = return . PP.pretty . name2String $ dcName dc

instance Pretty Literal where
  pprPrec _ l = case l of
    IntegerLiteral i
      | i < 0         -> return $ parens (PP.pretty i)
      | otherwise     -> return $ PP.pretty i
    IntLiteral i
      | i < 0         -> return $ parens (PP.pretty i)
      | otherwise     -> return $ PP.pretty i
    Int64Literal i
      | i < 0         -> return $ parens (PP.pretty i)
      | otherwise     -> return $ PP.pretty i
    WordLiteral w     -> return $ PP.pretty w
    Word64Literal w   -> return $ PP.pretty w
    FloatLiteral r    -> return $ PP.pretty (fromRat r :: Float)
    DoubleLiteral r   -> return $ PP.pretty (fromRat r :: Double)
    CharLiteral c     -> return $ PP.pretty c
    StringLiteral s   -> return $ vcat $ map PP.pretty $ showMultiLineString s
    NaturalLiteral n  -> return $ PP.pretty n
    ByteArrayLiteral s -> return $ PP.pretty $ show s

instance Pretty Pat where
  pprPrec prec pat = case pat of
    DataPat dc pxs -> do
      let (txs,xs) = unrebind pxs
      dc'  <- ppr (unembed dc)
      txs' <- mapM (pprBndr LetBind) txs
      xs'  <- mapM (pprBndr CaseBind) xs
      return $ prettyParen (prec >= appPrec) $ dc' <+> hsep txs' <> softline <> (nest 2 (vcat xs'))
    LitPat l   -> ppr (unembed l)
    DefaultPat -> return $ PP.pretty '_'

pprPrecLam :: LFresh m => Rational -> [Id] -> Term -> m Doc
pprPrecLam prec xs e = do
  xs' <- mapM (pprBndr LambdaBind) xs
  e'  <- pprPrec noPrec e
  return $ prettyParen (prec > noPrec) $
    PP.pretty 'λ' <> hsep xs' <+> rarrow <> line <> e'

pprPrecTyLam :: LFresh m => Rational -> [TyVar] -> Term -> m Doc
pprPrecTyLam prec tvs e = do
  tvs' <- mapM ppr tvs
  e'   <- pprPrec noPrec e
  return $ prettyParen (prec > noPrec) $
    PP.pretty 'Λ' <> hsep tvs' <+> rarrow <> line <> e'

pprPrecApp :: LFresh m => Rational -> Term -> Term -> m Doc
pprPrecApp prec e1 e2 = do
  e1' <- pprPrec opPrec e1
  e2' <- pprPrec appPrec e2
  return $ prettyParen (prec >= appPrec) $
    hang 2 (vsep [e1',e2'])

pprPrecTyApp :: LFresh m => Rational -> Term -> Type -> m Doc
pprPrecTyApp prec e ty = do
  e' <- pprPrec opPrec e
  ty' <- pprParendType ty
  return $ prettyParen (prec >= appPrec) $
    hang 2 (sep [e', (PP.pretty '@' <> ty')])

-- TODO use more conventional cast operator (|> or ▷) ?
pprPrecCast :: LFresh m => Rational -> Term -> Type -> Type -> m Doc
pprPrecCast prec e ty1 ty2 = do
  e' <- pprPrec appPrec e
  ty1' <- pprType ty1
  ty2' <- pprType ty2
  return $ prettyParen (prec >= appPrec) $
    parens (PP.pretty "cast" <> softline <> nest 5 (vcat [dcolon <+> ty1', rarrow <+> ty2']))
      <> softline <> nest 2 e'

pprPrecLetrec :: LFresh m => Rational -> [(Id, Embed Term)] -> Term -> m Doc
pprPrecLetrec prec xes body = do
  body' <- pprPrec noPrec body
  xes'  <- mapM (\(x,e) -> do
                  x' <- pprBndr LetBind x
                  e' <- pprPrec noPrec (unembed e)
                  return $ x' <> line <> equals <+> e'
                ) xes
  let xes'' = case xes' of
                [] -> [PP.pretty "EmptyLetrec"]
                _  -> xes'
  return $ prettyParen (prec > noPrec) $
    hang 2 (vcat ((PP.pretty "letrec"):xes'')) <> line <> PP.pretty "in" <+> body'

pprPrecCase :: LFresh m => Rational -> Term -> [(Pat,Term)] -> m Doc
pprPrecCase prec e alts = do
  e' <- pprPrec prec e
  alts' <- mapM (pprPrecAlt noPrec) alts
  return $ prettyParen (prec > noPrec) $
    hang 2 (vcat ((PP.pretty "case" <+> e' <+> PP.pretty "of"):alts'))

pprPrecAlt :: LFresh m => Rational -> (Pat,Term) -> m Doc
pprPrecAlt _ (altPat, altE) = do
  altPat' <- pprPrec noPrec altPat
  altE'   <- pprPrec noPrec altE
  return $ hang 2 (vcat [(altPat' <+> rarrow), altE'])

pprBndr :: (LFresh m, Pretty a) => BindingSite -> a -> m Doc
pprBndr bs x = prettyParen needsParen <$> ppr x
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

maybeParen :: TypePrec -> TypePrec -> Doc -> Doc
maybeParen ctxt_prec inner_prec = prettyParen (ctxt_prec >= inner_prec)

pprType :: LFresh m => Type -> m Doc
pprType = ppr_type TopPrec

pprParendType :: LFresh m => Type -> m Doc
pprParendType = ppr_type TyConPrec

ppr_type :: LFresh m => TypePrec -> Type -> m Doc
ppr_type _ (VarTy _ tv)                 = ppr tv
ppr_type _ (LitTy tyLit)                = ppr tyLit
ppr_type p ty@(ForAllTy _)              = pprForAllType p ty
ppr_type p (ConstTy (TyCon tc))         = pprTcApp p ppr_type tc []
ppr_type p (AnnType _ann typ)           = ppr_type p typ
ppr_type p (tyView -> TyConApp tc args) = pprTcApp p ppr_type tc args
ppr_type p (tyView -> FunTy ty1 ty2)    = pprArrowChain p <$> ppr_type FunPrec ty1 <:> pprFunTail ty2
  where
    pprFunTail (tyView -> FunTy ty1' ty2') = ppr_type FunPrec ty1' <:> pprFunTail ty2'
    pprFunTail otherTy                     = ppr_type TopPrec otherTy <:> pure []

ppr_type p (AppTy ty1 ty2) = maybeParen p TyConPrec <$> ((<+>) <$> pprType ty1 <*> ppr_type TyConPrec ty2)
ppr_type _ (ConstTy Arrow) = return (parens rarrow)

pprForAllType :: LFresh m => TypePrec -> Type -> m Doc
pprForAllType p ty = maybeParen p FunPrec <$> pprSigmaType True ty

pprSigmaType :: LFresh m => Bool -> Type -> m Doc
pprSigmaType showForalls ty = do
    (tvs, rho)     <- split1 [] ty
    sep <$> sequenceA [ if showForalls then pprForAll tvs else pure emptyDoc
                      , pprType rho
                      ]
  where
    split1 tvs (ForAllTy b) =
      lunbind b $ \(tv,resTy) -> split1 (tv:tvs) resTy
    split1 tvs resTy = return (reverse tvs,resTy)

pprForAll :: LFresh m => [TyVar] -> m Doc
pprForAll [] = return emptyDoc
pprForAll tvs = do
  tvs' <- mapM pprTvBndr tvs
  return $ PP.pretty '∀' <+> sep tvs' <> PP.dot

pprTvBndr :: LFresh m => TyVar -> m Doc
pprTvBndr tv
  = do
      tv'   <- ppr tv
      kind' <- pprKind kind
      return $ parens (tv' <+> dcolon <+> kind')
  where
    kind = unembed $ varKind tv

pprKind :: LFresh m => Kind -> m Doc
pprKind = pprType

pprTcApp :: LFresh m => TypePrec -> (TypePrec -> Type -> m Doc)
  -> TyConName -> [Type] -> m Doc
pprTcApp _ _  tc []
  = return . PP.pretty $ name2String tc

pprTcApp p pp tc tys
  | isTupleTyConLike tc
  = do
    tys' <- mapM (pp TopPrec) tys
    return $ parens $ sep $ punctuate comma tys'

  | otherwise
  = pprTypeNameApp p pp tc tys

pprTypeNameApp :: LFresh m => TypePrec -> (TypePrec -> Type -> m Doc)
  -> Name a -> [Type] -> m Doc
pprTypeNameApp p pp name tys
  | isSym
  , [ty1,ty2] <- tys
  = pprInfixApp p pp name ty1 ty2
  | otherwise
  = do
    tys' <- mapM (pp TyConPrec) tys
    let name' = PP.pretty $ name2String name
    return $ pprPrefixApp p (pprPrefixVar isSym name') tys'
  where
    isSym = isSymName name

pprInfixApp :: LFresh m => TypePrec -> (TypePrec -> Type -> m Doc)
  -> Name a -> Type -> Type -> m Doc
pprInfixApp p pp name ty1 ty2 = do
  ty1'  <- pp FunPrec ty1
  ty2'  <- pp FunPrec ty2
  let name' = PP.pretty $ name2String name
  return $ maybeParen p FunPrec $ sep [ty1', pprInfixVar True name' <+> ty2']

pprPrefixApp :: TypePrec -> Doc -> [Doc] -> Doc
pprPrefixApp p pp_fun pp_tys = maybeParen p TyConPrec $
                                 hang 2 (sep (pp_fun:pp_tys))

pprPrefixVar :: Bool -> Doc -> Doc
pprPrefixVar is_operator pp_v
  | is_operator = parens pp_v
  | otherwise   = pp_v

pprInfixVar :: Bool -> Doc -> Doc
pprInfixVar is_operator pp_v
  | is_operator = pp_v
  | otherwise   = PP.pretty '`' <> pp_v <> PP.pretty '`'

pprArrowChain :: TypePrec -> [Doc] -> Doc
pprArrowChain _ []         = emptyDoc
pprArrowChain p (arg:args) = maybeParen p FunPrec $
                               sep [arg, sep (map (rarrow <+>) args)]

isSymName :: Name a -> Bool
isSymName n = go (name2String n)
  where
    go s | null s           = False
         | isUpper $ head s = isLexConSym s
         | otherwise        = isLexSym s

isLexSym :: String -> Bool
isLexSym cs = isLexConSym cs || isLexVarSym cs

isLexConSym :: String -> Bool
isLexConSym "->" = True
isLexConSym cs   = startsConSym (head cs)

isLexVarSym :: String -> Bool
isLexVarSym cs = startsVarSym (head cs)

startsConSym :: Char -> Bool
startsConSym c = c == ':'

startsVarSym :: Char -> Bool
startsVarSym c = isSymbolASCII c || (ord c > 0x7f && isSymbol c)

isSymbolASCII :: Char -> Bool
isSymbolASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"
