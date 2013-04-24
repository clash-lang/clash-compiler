{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE ViewPatterns      #-}
module CLaSH.Core.Pretty
  ( showDoc
  )
where

import Data.Char (isUpper,ord,isSymbol)
import Data.Traversable (sequenceA)
import GHC.Show (showMultiLineString)
import Text.PrettyPrint (Doc,(<+>),(<>),($+$),($$),render,parens,text,sep,
  punctuate,comma,hang,char,brackets,empty,fsep,hsep,equals,vcat,integer,int)
import Unbound.LocallyNameless (Embed(..),Name,LFresh,runLFreshM,unembed,
  name2String,name2Integer,lunbind,unrec,unrebind)

import CLaSH.Core.DataCon (DataCon(..))
import CLaSH.Core.Literal (Literal(..))
import CLaSH.Core.Prim    (Prim(..))
import CLaSH.Core.Term    (Term(..),Pat(..))
import CLaSH.Core.Type    (Type(..),ConstTy(..),LitTy(..),Kind,TypeView(..),ThetaType,tyView,noParenPred,isPredTy,isLiftedTypeKind)
import CLaSH.Core.TysPrim (eqTyConKey,listTyConKey)
import CLaSH.Core.TyCon   (TyCon(..),isTupleTyConLike)
import CLaSH.Core.Var     (Var,TyVar,Id,varName,varType,varKind)
import CLaSH.Util

class Pretty p where
  ppr :: (Applicative m, LFresh m) => p -> m Doc
  ppr = pprPrec 0

  pprPrec :: (Applicative m, LFresh m) => Rational -> p -> m Doc

noPrec, opPrec, appPrec :: Num a => a
noPrec = 0
opPrec = 1
appPrec = 2

showDoc :: Pretty p => p -> String
showDoc = render . runLFreshM . ppr

prettyParen :: Bool -> Doc -> Doc
prettyParen False = id
prettyParen True  = parens

instance Pretty (Name a) where
  pprPrec _ = return . text . show

instance Pretty a => Pretty [a] where
  pprPrec prec xs = do
    xs' <- mapM (pprPrec prec) xs
    return $ vcat xs'

instance Pretty (Id, Term) where
  pprPrec _ = pprTopLevelBndr

pprTopLevelBndr :: (Applicative m, LFresh m) => (Id,Term) -> m Doc
pprTopLevelBndr (bndr,expr) = do
  bndr' <- ppr bndr
  bndrName <- ppr (varName bndr)
  expr' <- ppr expr
  return $ bndr' $$ hang (bndrName <+> equals) 2 expr' <> text "\n"

dcolon :: Doc
dcolon = text "::"

period :: Doc
period = char '.'

darrow :: Doc
darrow = text "=>"

rarrow :: Doc
rarrow = text "->"

instance Pretty Type where
  pprPrec _ ty = pprType ty

instance Pretty (Var Type) where
  pprPrec _ v = ppr $ varName v

instance Pretty TyCon where
  pprPrec _ tc = return . text . name2String $ tyConName tc

instance Pretty LitTy where
  pprPrec _ (NumTy i) = return $ int i
  pprPrec _ (SymTy s) = return $ text s

instance Pretty Term where
  pprPrec prec e = case e of
    Var _ x        -> pprPrec prec x
    Data _ dc      -> pprPrec prec dc
    Literal l      -> pprPrec prec l
    Prim p         -> pprPrec prec p
    Lam b          -> lunbind b $ \(v,e')  -> pprPrecLam prec [v] e'
    TyLam b        -> lunbind b $ \(tv,e') -> pprPrecTyLam prec [tv] e'
    App fun arg    -> pprPrecApp prec fun arg
    TyApp e' ty    -> pprPrecTyApp prec e' ty
    Letrec b       -> lunbind b $ \(xes,e') -> pprPrecLetrec prec (unrec xes) e'
    Case e' _ alts -> pprPrecCase prec e' =<< mapM (flip lunbind return) alts

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
  pprPrec _ dc = return . text . name2String $ dcName dc

instance Pretty Literal where
  pprPrec _ l = case l of
    IntegerLiteral i
      | i < 0       -> return $ parens (integer i)
      | otherwise   -> return $ integer i
    StringLiteral s -> return $ vcat $ map text $ showMultiLineString s

instance Pretty Prim where
  pprPrec prec p = case p of
    PrimFun f _   -> return . text $ name2String f
    PrimCon dc    -> pprPrec prec dc
    PrimDict di _ -> return . text $ name2String di
    PrimDFun df _ -> return . text $ name2String df
    PrimCo _      -> return $ text "co"

instance Pretty Pat where
  pprPrec prec pat = case pat of
    DataPat dc pxs -> do
      let (txs,xs) = unrebind pxs
      dc'  <- ppr (unembed dc)
      txs' <- mapM (pprBndr LetBind) txs
      xs'  <- mapM (pprBndr CaseBind) xs
      return $ prettyParen (prec >= appPrec) $ dc' <+> hsep txs' <+> hsep xs'
    LitPat l   -> ppr (unembed l)
    DefaultPat -> return $ char '_'

pprPrecLam :: (Applicative m, LFresh m) => Rational -> [Id] -> Term -> m Doc
pprPrecLam prec xs e = do
  xs' <- mapM (pprBndr LambdaBind) xs
  e'  <- pprPrec noPrec e
  return $ prettyParen (prec > noPrec) $
    char 'λ' <> hsep xs' <+> rarrow $+$ e'

pprPrecTyLam :: (Applicative m, LFresh m) => Rational -> [TyVar] -> Term -> m Doc
pprPrecTyLam prec tvs e = do
  tvs' <- mapM ppr tvs
  e'   <- pprPrec noPrec e
  return $ prettyParen (prec > noPrec) $
    char 'Λ' <> hsep tvs' <+> rarrow $+$ e'

pprPrecApp :: (Applicative m, LFresh m) => Rational -> Term -> Term -> m Doc
pprPrecApp prec e1 e2 = do
  e1' <- pprPrec opPrec e1
  e2' <- pprPrec appPrec e2
  return $ prettyParen (prec >= appPrec) $ e1' <+> e2'

pprPrecTyApp :: (Applicative m, LFresh m) => Rational -> Term -> Type -> m Doc
pprPrecTyApp prec e ty = do
  e' <- pprPrec opPrec e
  ty' <- pprParendType ty
  return $ prettyParen (prec >= appPrec) $ e' <+> char '@' <> ty'

pprPrecLetrec :: (Applicative m, LFresh m) => Rational -> [(Id, Embed Term)] -> Term
  -> m Doc
pprPrecLetrec prec xes body
  | [] <- xes = pprPrec prec body
  | otherwise = do
    body' <- pprPrec noPrec body
    xes'  <- mapM (\(x,e) -> do
                    x' <- pprBndr LetBind x
                    e' <- pprPrec noPrec (unembed e)
                    return $ x' <+> equals <+> e'
                  ) xes
    return $ prettyParen (prec > noPrec) $
      hang (text "letrec") 2 (vcat xes') $$ text "in" <+> body'

pprPrecCase :: (Applicative m, LFresh m) => Rational -> Term -> [(Pat,Term)] -> m Doc
pprPrecCase prec e alts = do
  e' <- pprPrec prec e
  alts' <- mapM (pprPrecAlt noPrec) alts
  return $ prettyParen (prec > noPrec) $
    hang (text "case" <+> e' <+> text "of") 2 $ vcat alts'

pprPrecAlt :: (Applicative m, LFresh m) => Rational -> (Pat,Term) -> m Doc
pprPrecAlt _ (altPat, altE) = do
  altPat' <- pprPrec noPrec altPat
  altE'   <- pprPrec noPrec altE
  return $ hang (altPat' <+> rarrow) 2 altE'

pprBndr :: (Applicative m, LFresh m, Pretty a) => BindingSite -> a -> m Doc
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

pprType :: (Applicative m, LFresh m) => Type -> m Doc
pprType = ppr_type TopPrec

pprParendType :: (Applicative m, LFresh m) => Type -> m Doc
pprParendType = ppr_type TyConPrec

ppr_type :: (Applicative m, LFresh m) => TypePrec -> Type -> m Doc
ppr_type _ (VarTy _ tv)                 = ppr tv
ppr_type _ (LitTy tyLit)                = ppr tyLit
ppr_type p ty@(ForAllTy _)              = pprForAllType p ty
ppr_type p (ConstTy (TyCon tc))         = pprTcApp p ppr_type tc []
ppr_type p (tyView -> TyConApp tc args) = pprTcApp p ppr_type tc args
ppr_type p fun_ty@(tyView -> FunTy ty1 ty2)
  | isPredTy ty1                = pprForAllType p fun_ty
  | otherwise                   = pprArrowChain p <$> ppr_type FunPrec ty1 <:> pprFunTail ty2
  where
    pprFunTail (tyView -> FunTy ty1' ty2')
      | not (isPredTy ty1) = ppr_type FunPrec ty1' <:> pprFunTail ty2'
    pprFunTail otherTy     = ppr_type TopPrec otherTy <:> pure []

ppr_type p (AppTy ty1 ty2) = maybeParen p TyConPrec <$> ((<+>) <$> pprType ty1 <*> ppr_type TyConPrec ty2)
ppr_type p ty = error $ $(curLoc) ++ "Can't pretty print type: " ++ show ty

pprForAllType :: (Applicative m, LFresh m) => TypePrec -> Type -> m Doc
pprForAllType p ty = maybeParen p FunPrec <$> pprSigmaType True ty

pprSigmaType :: (Applicative m, LFresh m) => Bool -> Type -> m Doc
pprSigmaType showForalls ty = do
    (tvs, rho)     <- split1 [] ty
    let (ctxt,tau) =  split2 [] rho
    sep <$> sequenceA [ if showForalls then pprForAll tvs else pure empty
                      , pprThetaArrowTy ctxt
                      , pprType tau
                      ]
  where
    split1 tvs (ForAllTy b) = do
      lunbind b $ \(tv,resTy) -> split1 (tv:tvs) resTy
    split1 tvs resTy = return (reverse tvs,resTy)

    split2 ps (tyView -> FunTy ty1 ty2)
      | isPredTy ty1 = split2 (ty1:ps) ty2
    split2 ps resTy  = (reverse ps, resTy)

pprForAll :: (Applicative m, LFresh m) => [TyVar] -> m Doc
pprForAll [] = return empty
pprForAll tvs = do
  tvs' <- mapM pprTvBndr tvs
  return $ char '∀' <+> sep tvs' <> period

pprTvBndr :: (Applicative m, LFresh m) => TyVar -> m Doc
pprTvBndr tv
  | isLiftedTypeKind kind = ppr tv
  | otherwise             = do
      tv'   <- ppr tv
      kind' <- pprKind kind
      return $ parens (tv' <+> dcolon <+> kind')
  where
    kind = unembed $ varKind tv

pprKind :: (Applicative m, LFresh m) => Kind -> m Doc
pprKind = pprType

pprThetaArrowTy :: (Applicative m, LFresh m) => ThetaType -> m Doc
pprThetaArrowTy [] = return empty
pprThetaArrowTy [predTy]
  | noParenPred predTy
  = do predType' <- pprType predTy
       return $ predType' <+> darrow
pprThetaArrowTy preds = do
  preds' <- mapM pprType preds
  return $ parens (fsep (punctuate comma preds')) <+> darrow

pprTcApp :: (Applicative m, LFresh m) => TypePrec -> (TypePrec -> Type -> m Doc)
  -> TyCon -> [Type] -> m Doc
pprTcApp _ _  tc []
  = ppr tc
pprTcApp _ pp tc [ty]
  | (name2Integer $ tyConName tc) == listTyConKey
  = pp TopPrec ty >>= (return . brackets)

pprTcApp p pp tc tys
  | isTupleTyConLike tc && tyConArity tc == length tys
  = do
    tys' <- mapM (pp TopPrec) tys
    return $ parens $ sep $ punctuate comma tys'
  | (name2Integer $ tyConName tc) == eqTyConKey
  , [_,ty1,ty2] <- tys
  = pprInfixApp p pp (tyConName tc) ty1 ty2
  | otherwise
  = pprTypeNameApp p pp (tyConName tc) tys

pprTypeNameApp :: LFresh m => TypePrec -> (TypePrec -> Type -> m Doc)
  -> Name a -> [Type] -> m Doc
pprTypeNameApp p pp name tys
  | isSym
  , [ty1,ty2] <- tys
  = pprInfixApp p pp name ty1 ty2
  | otherwise
  = do
    tys' <- mapM (pp TyConPrec) tys
    let name' = text $ name2String name
    return $ pprPrefixApp p (pprPrefixVar isSym name') tys'
  where
    isSym = isSymName name

pprInfixApp :: LFresh m => TypePrec -> (TypePrec -> Type -> m Doc)
  -> Name a -> Type -> Type -> m Doc
pprInfixApp p pp name ty1 ty2 = do
  ty1'  <- pp FunPrec ty1
  ty2'  <- pp FunPrec ty2
  let name' = text $ name2String name
  return $ maybeParen p FunPrec $ sep [ty1', pprInfixVar True name' <+> ty2']

pprPrefixApp :: TypePrec -> Doc -> [Doc] -> Doc
pprPrefixApp p pp_fun pp_tys = maybeParen p TyConPrec $
                                 hang pp_fun 2 (sep pp_tys)

pprPrefixVar :: Bool -> Doc -> Doc
pprPrefixVar is_operator pp_v
  | is_operator = parens pp_v
  | otherwise   = pp_v

pprInfixVar :: Bool -> Doc -> Doc
pprInfixVar is_operator pp_v
  | is_operator = pp_v
  | otherwise   = char '`' <> pp_v <> char '`'

pprArrowChain :: TypePrec -> [Doc] -> Doc
pprArrowChain _ []         = empty
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
