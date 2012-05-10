{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards     #-}
module CLaSH.Core.Pretty
  ( showDoc
  )
where

import Data.Char (isUpper,ord,isSymbol)
import qualified Data.HashMap.Lazy as HashMap
import GHC.Show (showMultiLineString)
import Text.PrettyPrint (Doc,(<+>),(<>),($+$),($$),render,parens,text,sep,
  punctuate,comma,hang,char,brackets,empty,fsep,hsep,equals,vcat,integer)
import Unbound.LocallyNameless (Embed(..),LFresh,Name,runLFreshM,unembed,
  name2String,name2Integer,lunbind,unrec)

import CLaSH.Core.DataCon (DataCon(..))
import CLaSH.Core.Literal (Literal(..))
import CLaSH.Core.Prim    (Prim(..))
import CLaSH.Core.Term    (Term(..),Pat(..))
import CLaSH.Core.Type    (Kind,ThetaType,Delta,noParenPred,isPredTy,
  isLiftedTypeKind)
import CLaSH.Core.TypeRep (Type(..))
import CLaSH.Core.TysPrim (eqTyConKey,listTyConKey)
import CLaSH.Core.TyCon   (TyCon(..),isTupleTyConLike)
import CLaSH.Core.Var     (Var,TyVar,Id,varName,varType,varKind)

class Pretty p where
  ppr :: (LFresh m) => Delta -> p -> m Doc
  ppr = pprPrec 0

  pprPrec :: (LFresh m) => Rational -> Delta -> p -> m Doc

noPrec, opPrec, appPrec :: Num a => a
noPrec = 0
opPrec = 1
appPrec = 2

showDoc :: Pretty p => Delta -> p -> String
showDoc delta = render . runLFreshM . ppr delta

prettyParen :: Bool -> Doc -> Doc
prettyParen False = id
prettyParen True  = parens

instance Pretty (Name a) where
  pprPrec _ _ = return . text . name2String

instance Pretty a => Pretty [a] where
  pprPrec prec d xs = do
    xs' <- mapM (pprPrec prec d) xs
    return $ vcat xs'

instance Pretty (Id, Term) where
  pprPrec _ = pprTopLevelBndr

pprTopLevelBndr :: LFresh m => Delta -> (Id,Term) -> m Doc
pprTopLevelBndr d (bndr,expr) = do
  bndr' <- ppr d bndr
  bndrName <- ppr d (varName bndr)
  expr' <- ppr d expr
  return $ bndr' $$ bndrName <+> equals <+> expr' <> text "\n"

dcolon :: Doc
dcolon = text "::"

period :: Doc
period = char '.'

darrow :: Doc
darrow = text "=>"

rarrow :: Doc
rarrow = text "->"

instance Pretty Type where
  pprPrec _ d ty = pprType d ty

instance Pretty (Var Type) where
  pprPrec _ d v = ppr d $ varName v

instance Pretty TyCon where
  pprPrec _ d tc = ppr d (tyConName tc)

instance Pretty Term where
  pprPrec prec d e = case e of
    Var x        -> pprPrec prec d x
    Data dc      -> pprPrec prec d dc
    Literal l    -> pprPrec prec d l
    Prim p       -> pprPrec prec d p
    Lam b        -> lunbind b $ \(v,e') -> pprPrecLam prec d [v] e'
    TyLam b      -> lunbind b $ \(tv,e') ->
                      let d' = HashMap.insert
                                (varName tv)
                                (unembed $ varKind tv) d
                      in pprPrecTyLam prec d' [tv] e'
    App fun arg  -> pprPrecApp prec d fun arg
    TyApp e' ty  -> pprPrecTyApp prec d e' ty
    Letrec b     -> do
                      lunbind b $ \(xes,e') ->
                        pprPrecLetrec prec d (unrec xes) e'
    Case e' alts -> mapM (flip lunbind return) alts >>= pprPrecCase prec d e'

data BindingSite
  = LambdaBind
  | CaseBind
  | LetBind

instance Pretty (Var Term) where
  pprPrec _ d v = do
    v'  <- ppr d (varName v)
    ty' <- ppr d (unembed $ varType v)
    return $ v' <+> dcolon <+> ty'

instance Pretty DataCon where
  pprPrec _ d dc = ppr d (dcName dc)

instance Pretty Literal where
  pprPrec _ _ l = case l of
    IntegerLiteral i
      | i < 0       -> return $ parens (integer i)
      | otherwise   -> return $ integer i
    StringLiteral s -> return $ vcat $ map text $ showMultiLineString s

instance Pretty Prim where
  pprPrec prec d p = case p of
    PrimFun f _   -> pprPrec prec d f
    PrimCon dc    -> pprPrec prec d dc
    PrimDict di _ -> pprPrec prec d di
    PrimDFun df _ -> pprPrec prec d df
    PrimCo _      -> return $ text "co"

instance Pretty Pat where
  pprPrec prec d pat = case pat of
    DataPat dc xs -> do
      dc' <- ppr d dc
      xs' <- mapM (pprBndr d CaseBind) xs
      return $ prettyParen (prec >= appPrec) $ dc' <+> hsep xs'
    LitPat l   -> ppr d l
    DefaultPat -> return $ char '_'

pprPrecLam :: LFresh m => Rational -> Delta -> [Id] -> Term -> m Doc
pprPrecLam prec d xs e = do
  xs' <- mapM (pprBndr d LambdaBind) xs
  e'  <- pprPrec noPrec d e
  return $ prettyParen (prec > noPrec) $
    char 'λ' <> hsep xs' <+> rarrow $+$ e'

pprPrecTyLam :: LFresh m => Rational -> Delta -> [TyVar] -> Term -> m Doc
pprPrecTyLam prec d tvs e = do
  tvs' <- mapM (ppr d) tvs
  e'   <- pprPrec noPrec d e
  return $ prettyParen (prec > noPrec) $
    char 'Λ' <> hsep tvs' <+> rarrow $+$ e'

pprPrecApp :: LFresh m => Rational -> Delta -> Term -> Term -> m Doc
pprPrecApp prec d e1 e2 = do
  e1' <- pprPrec opPrec d e1
  e2' <- pprPrec appPrec d e2
  return $ prettyParen (prec >= appPrec) $ e1' <+> e2'

pprPrecTyApp :: LFresh m => Rational -> Delta -> Term -> Type -> m Doc
pprPrecTyApp prec d e ty = do
  e' <- pprPrec opPrec d e
  ty' <- pprPrec appPrec d ty
  return $ prettyParen (prec >= appPrec) $ e' <+> char '@' <> ty'

pprPrecLetrec :: LFresh m => Rational -> Delta -> [(Id, Embed Term)] -> Term
  -> m Doc
pprPrecLetrec prec d xes body
  | [] <- xes = pprPrec prec d body
  | otherwise = do
    body' <- pprPrec noPrec d body
    xes'  <- mapM (\(x,e) -> do
                    x' <- pprBndr d LetBind x
                    e' <- pprPrec noPrec d (unembed e)
                    return $ x' <+> equals <+> e'
                    --return x'
                  ) xes
    --let xes' = [empty]
    return $ prettyParen (prec > noPrec) $
      hang (text "letrec") 2 (vcat xes') $$ text "in" <+> body'

pprPrecCase :: LFresh m => Rational -> Delta -> Term -> [(Pat,Term)] -> m Doc
pprPrecCase prec d e alts = do
  e' <- pprPrec prec d e
  alts' <- mapM (pprPrecAlt noPrec d) alts
  return $ prettyParen (prec > noPrec) $
    hang (text "case" <+> e' <+> text "of") 2 $ vcat alts'

pprPrecAlt :: LFresh m => Rational -> Delta -> (Pat,Term) -> m Doc
pprPrecAlt _ d (altPat, altE) = do
  altPat' <- pprPrec noPrec d altPat
  altE'   <- pprPrec noPrec d altE
  return $ hang (altPat' <+> rarrow) 2 altE'

pprBndr :: LFresh m => Delta -> BindingSite -> Id -> m Doc
pprBndr d bs x = do
    x' <- ppr d x
    return $ prettyParen needsParen x'
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

pprType :: LFresh m => Delta -> Type -> m Doc
pprType = go TopPrec
  where
    go _ d (TyVarTy tv)      = ppr d tv
    go p d (TyConApp tc tys) = pprTcApp p d ((flip go) d) tc tys
    go p d ty@(ForAllTy _)   = pprForAllType p d ty
    go p d funTy@(FunTy ty1 ty2)
      | isPredTy d ty1
      = pprForAllType p d funTy
      | otherwise
      = do
        ty1' <- go FunPrec d ty1
        ty2' <- pprFunTail d ty2
        return $ pprArrowChain p (ty1' : ty2')

    pprFunTail d (FunTy ty1 ty2)
      | not (isPredTy d ty1) = do
          ty1' <- go FunPrec d ty1
          ty2' <- pprFunTail d ty2
          return (ty1':ty2')
    pprFunTail d otherTy = go TopPrec d otherTy >>= (return . (:[]))

pprForAllType :: LFresh m => TypePrec -> Delta -> Type -> m Doc
pprForAllType p delta ty
  = do
    (tvs, rho) <- split1 [] ty
    let delta' = foldl (\d tv -> HashMap.insert (varName tv)
                                                (unembed $ varKind tv) d)
                       delta tvs
    let (ctxt,tau) = split2 delta' [] rho
    tau' <- pprType delta' tau
    tvs' <- pprForAll delta' tvs
    ctxt' <- pprThetaArrowTy delta' ctxt
    return $ maybeParen p FunPrec $ sep [tvs', ctxt', tau']
  where
    split1 tvs (ForAllTy b) = do
      lunbind b $ \(tv,resTy) -> split1 (tv:tvs) resTy
    split1 tvs resTy = return (reverse tvs,resTy)

    split2 d' ps (FunTy ty1 ty2) | isPredTy d' ty1 = split2 d' (ty1:ps) ty2
    split2 _ ps resTy                              = (reverse ps, resTy)

pprForAll :: LFresh m => Delta -> [TyVar] -> m Doc
pprForAll _ [] = return empty
pprForAll delta tvs = do
  tvs' <- mapM (pprTvBndr delta) tvs
  return $ char '∀' <+> sep tvs' <> period

pprTvBndr :: LFresh m => Delta -> TyVar -> m Doc
pprTvBndr delta tv
  | isLiftedTypeKind kind = ppr delta tv
  | otherwise             = do
      tv' <- ppr delta tv
      kind' <- pprKind delta kind
      return $ parens (tv' <+> dcolon <+> kind')
  where
    kind = unembed $ varKind tv

pprKind :: LFresh m => Delta -> Kind -> m Doc
pprKind = pprType

pprThetaArrowTy :: LFresh m => Delta -> ThetaType -> m Doc
pprThetaArrowTy _ [] = return empty
pprThetaArrowTy d [predTy]
  | noParenPred predTy
  = do
    predType' <- pprType d predTy
    return $ predType' <+> darrow
pprThetaArrowTy d preds = do
  preds' <- mapM (pprType d) preds
  return $ parens (fsep (punctuate comma preds')) <+> darrow

pprTcApp :: LFresh m => TypePrec -> Delta -> (TypePrec -> Type -> m Doc)
  -> TyCon -> [Type] -> m Doc
pprTcApp _ d _  tc []    = ppr d tc
pprTcApp _ _ pp tc [ty]
  | (name2Integer $ tyConName tc) == listTyConKey
  = pp TopPrec ty >>= (return . brackets)

pprTcApp p d pp tc tys
  | isTupleTyConLike tc && tyConArity tc == length tys
  = do
    tys' <- mapM (pp TopPrec) tys
    return $ parens $ sep $ punctuate comma tys'
  | (name2Integer $ tyConName tc) == eqTyConKey
  , [_,ty1,ty2] <- tys
  = pprInfixApp p d pp (tyConName tc) ty1 ty2
  | otherwise
  = pprTypeNameApp p d pp (tyConName tc) tys

pprTypeNameApp :: LFresh m => TypePrec -> Delta
  -> (TypePrec -> Type -> m Doc) -> Name a -> [Type] -> m Doc
pprTypeNameApp p d pp name tys
  | isSym
  , [ty1,ty2] <- tys
  = pprInfixApp p d pp name ty1 ty2
  | otherwise
  = do
    tys' <- mapM (pp TyConPrec) tys
    name' <- ppr d name
    return $ pprPrefixApp p (pprPrefixVar isSym name') tys'
  where
    isSym = isSymName name

pprInfixApp :: LFresh m => TypePrec -> Delta -> (TypePrec -> Type -> m Doc)
  -> Name a -> Type -> Type -> m Doc
pprInfixApp p d pp name ty1 ty2 = do
  ty1'  <- pp FunPrec ty1
  ty2'  <- pp FunPrec ty2
  name' <- ppr d name
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
