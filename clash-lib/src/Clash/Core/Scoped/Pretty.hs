{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Pretty printing of scoped Clash Core: the 'PrettyPrec' instances of its
  types, terms, binders, global variables and type constructors. A port of the printer of Clash
  Core ("Clash.Core.Pretty"), which prints the same text as printing the
  conversion to Clash Core ('Clash.Core.Scoped.Convert.toClashTerm') does:

  * The unique of a local variable is its raw name.

  * An occurrence of a variable that is bound in the printed term shows the
    occurrence name of its binder, not its own.

  * Subterms are annotated with their position in their parent term
    ('Clash.Core.Pretty.AnnSubterm', see 'Clash.Core.Pretty.SubtermPos'), for
    navigation, where the printer of Clash Core annotates them with their
    context ('Clash.Core.Pretty.AnnContext'). The bodies of ticks are
    annotated too.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Core.Scoped.Pretty () where

import Data.Char (isSymbol, isUpper, ord)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as T
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
#else
import Data.Text.Prettyprint.Doc
#endif

import Clash.Core.Basic (ConstTy (..), NameMod (..))
import Clash.Core.DataCon (DataCon (..))
import qualified Clash.Core.Name as C
import Clash.Core.Pretty
  (ClashAnnotation (..), ClashDoc, PrettyPrec (..), SubtermPos (..),
   SyntaxElement (..), fromPpr, ppr)
import Clash.Core.Scoped.Sig (TyFamEqn (..))
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (primArg)
import qualified Clash.Core.Term as C (Term)
import Clash.Core.TyCon
  (AlgTyConRhs (..), TyCon (..), TyConName, isTupleTyConLike)
import qualified Clash.Core.Type as C (Type)
import Clash.Pretty (ClashPretty (..))

-- * Instances

instance PrettyPrec (Type n) where
  pprPrec _ = pure . pprType emptyEnv

instance ClashPretty (Type n) where
  clashPretty = fromPpr

instance PrettyPrec (Term n) where
  pprPrec prec = pure . pprTerm emptyEnv prec

instance ClashPretty (Term n) where
  clashPretty = fromPpr

-- | Its name and its type, like a local variable
instance PrettyPrec GlobalId where
  pprPrec _ g = pure (pprTyped (ppr (globalIdName g)) (pprType emptyEnv (globalIdType g)))

-- | Its name and its type
instance PrettyPrec (IdBinder n l) where
  pprPrec _ = pure . fst . pprIdBinder emptyEnv

-- | Its name
instance PrettyPrec (TyBinder n l) where
  pprPrec _ = pure . fst . pprTyBinder emptyEnv

instance PrettyPrec (TickInfo n) where
  pprPrec prec = pure . pprTick emptyEnv prec

-- | Its declaration: its kind, and its data constructors or equations. Their
-- types are closed, and the uniques of the variables they bind are their raw
-- names.
instance PrettyPrec TyCon where
  pprPrec _ = pure . pprTyCon

-- * Names

-- | The names of the variables bound in the printed term, by raw name, see
-- the module header
data Env = Env !(IntMap (C.Name C.Type)) !(IntMap (C.Name C.Term))

emptyEnv :: Env
emptyEnv = Env IntMap.empty IntMap.empty

-- | The name of a local variable: its occurrence name, with its raw name as
-- unique
localName :: C.Name a -> Int -> C.Name a
localName nm raw
  | C.nameUniq nm == u = nm
  | otherwise = nm {C.nameUniq = u}
 where
  u = fromIntegral raw
{-# INLINE localName #-}

tyVarName :: Env -> C.Name C.Type -> TyName n -> C.Name C.Type
tyVarName (Env tys _) occ n =
  let raw = tyNameRaw n
  in case IntMap.lookup raw tys of
       Just nm -> nm
       Nothing -> localName occ raw

tmVarName :: Env -> C.Name C.Term -> TmName n -> C.Name C.Term
tmVarName (Env _ tms) occ n =
  let raw = tmNameRaw n
  in case IntMap.lookup raw tms of
       Just nm -> nm
       Nothing -> localName occ raw

-- | The name of a type variable binder, and the environment of its scope
bindTy :: Env -> TyBinder n l -> (C.Name C.Type, Env)
bindTy (Env tys tms) b =
  let raw = tyNameRaw (tyNameOf b)
      nm = localName (tyBinderOcc b) raw
  in (nm, Env (IntMap.insert raw nm tys) tms)

-- | The name of a term variable binder, and the environment of its scope
bindId :: Env -> IdBinder n l -> (C.Name C.Term, Env)
bindId (Env tys tms) b =
  let raw = tmNameRaw (tmNameOf b)
      nm = localName (idBinderOcc b) raw
  in (nm, Env tys (IntMap.insert raw nm tms))

-- * Binders

-- | A name with a type
pprTyped :: ClashDoc -> ClashDoc -> ClashDoc
pprTyped nm ty = nm <> annotate (AnnSyntax Type) (align (space <> dcolon <+> ty))

-- | A type variable binder is printed by its name
pprTyBinder :: Env -> TyBinder n l -> (ClashDoc, Env)
pprTyBinder env b =
  let (nm, env') = bindTy env b
  in (ppr nm, env')

-- | A term variable binder is printed by its name and its type
pprIdBinder :: Env -> IdBinder n l -> (ClashDoc, Env)
pprIdBinder env b =
  let (nm, env') = bindId env b
  in (pprTyped (ppr nm) (pprType env (idBinderType b)), env')

-- | The binders of a telescope, and the environment of its scope
pprTele
  :: (forall x y. Env -> b x y -> (ClashDoc, Env))
  -> Env -> Tele b n l -> ([ClashDoc], Env)
pprTele _ env TNil = ([], env)
pprTele pprB env (TCons b bs) =
  let (d, env1) = pprB env b
      (ds, env2) = pprTele pprB env1 bs
  in (d : ds, env2)

-- * Terms

noPrec, opPrec, appPrec :: Rational
noPrec = 0
opPrec = 1
appPrec = 2

pprTerm :: Env -> Rational -> Term n -> ClashDoc
pprTerm env prec = \case
  Var occ _ n ->
    ppr (tmVarName env occ n) <> annotate (AnnSyntax Unique) (brackets "LocalId")
  GlobalVar g ->
    ppr (globalIdName g) <> annotate (AnnSyntax Unique) (brackets "GlobalId")
  Data dc -> ppr (dcName dc)
  Literal l -> ppr l
  Prim p -> pprPrim (primName p)
  Lam b e ->
    let (b', env') = pprIdBinder env b
    in annotate (AnnSubterm PosLamBody) $ parensIf (prec > noPrec) $
         lam <> tyParens b' <+> rarrow <> line <> pprTerm env' noPrec e
  TyLam b e ->
    let (b', env') = pprTyBinder env b
    in annotate (AnnSubterm PosTyLamBody) $ tyParensIf (prec > noPrec) $
         annotate (AnnSyntax Type) (tylam <> b' <+> rarrow <> line)
           <> pprTerm env' noPrec e
  App e1 e2 ->
    let e1' = annotate (AnnSubterm PosAppFun) (pprTerm env opPrec e1)
        e2' = annotate (AnnSubterm (PosAppArg (primArg e2))) (pprTerm env appPrec e2)
    in parensIf (prec >= appPrec) (hang 2 (sep [e1', e2']))
  TyApp e ty ->
    annotate (AnnSubterm PosTyAppFun) $ tyParensIf (prec >= appPrec) $
      hang 2 $ group $
        pprTerm env opPrec e
          <> annotate (AnnSyntax Type) (line <> at <> pprType' env TyConPrec ty)
  Let (NonRec b x) e ->
    let (b', env') = pprIdBinder env b
    in pprLet prec False [(b', pprTerm env noPrec x)] (pprTerm env' noPrec e)
  Let (Rec bs xs) e ->
    let (bs', env') = pprTele pprIdBinder env bs
    in pprLet prec True (zip bs' (map (pprTerm env' noPrec) xs)) (pprTerm env' noPrec e)
  Case e _ alts ->
    let e' = annotate (AnnSubterm PosCaseScrut) (pprTerm env prec e)
        alts' = zipWith (pprAlt env) [0 ..] alts
    in parensIf (prec > noPrec) $ hang 2 $ vsepHard $ (case_ <+> e' <+> of_) : alts'
  Cast e ty1 ty2 ->
    let e' = annotate (AnnSubterm PosCastBody) (pprTerm env appPrec e)
    in tyParensIf (prec >= appPrec) $
         e' <> annotate (AnnSyntax Type)
                 (softline <> nest 2 (vsep [cast, pprType env ty1, coerce, pprType env ty2]))
  Tick t e ->
    annotate (AnnSyntax Ticky) (pprTick env prec t <> line')
      <> annotate (AnnSubterm PosTickBody) (pprTerm env prec e)

pprPrim :: Text -> ClashDoc
pprPrim nm = annotate (AnnSyntax Qualifier) (pretty qual) <> pretty occ
 where
  (qual, occ) = T.breakOnEnd "." nm

-- | A let-expression, given whether it is recursive, its binders and
-- right-hand sides, and its body
pprLet :: Rational -> Bool -> [(ClashDoc, ClashDoc)] -> ClashDoc -> ClashDoc
pprLet prec isRec xes body =
  parensIf (prec > noPrec) $
    vsepHard [hang 2 (vsepHard (kw : xes'')), in_ <+> annotate (AnnSubterm PosLetBody) body]
 where
  xes' = zipWith (\i (x, e) -> annotate (AnnSubterm (PosLetRhs i)) (vsepHard [x, equals <+> e]))
           [0 ..] xes
  xes'' = case xes' of { [] -> ["EmptyLetrec"]; _ -> xes' }
  kw = if isRec then letrec else let_

pprAlt :: Env -> Int -> Alt n -> ClashDoc
pprAlt env i (Alt pat e) =
  let (pat', env') = pprPat env pat
  in annotate (AnnSubterm (PosCaseAlt i)) $
       hang 2 $ vsepHard [pat' <+> rarrow, pprTerm env' noPrec e]

-- | A pattern, and the environment of the right-hand side of its alternative
pprPat :: Env -> Pat n l -> (ClashDoc, Env)
pprPat env = \case
  DataPat dc tvs ids ->
    let (tvs', env1) = pprTele pprTyBinder env tvs
        (ids', env2) = pprTele pprIdBinder env1 ids
    in (sep [hsep (ppr (dcName dc) : tvs'), nest 2 (sep (map tyParens ids'))], env2)
  LitPat l -> (ppr l, env)
  DefaultPat -> ("_", env)

pprTick :: Env -> Rational -> TickInfo n -> ClashDoc
pprTick env prec = \case
  SrcSpan sp -> ppr sp
  NameMod m ty -> nameMod m <> pprType env ty
  DeDup -> "<deDup>"
  NoDeDup -> "<noDeDup>"
  Attributes _ attrs -> "<attributes>" <> pprTerm env prec attrs
 where
  nameMod = \case
    PrefixName -> "<prefixName>"
    SuffixName -> "<suffixName>"
    SuffixNameP -> "<suffixNameP>"
    SetName -> "<setName>"

-- * Types

data TypePrec = TopPrec | FunPrec | TyConPrec deriving (Eq, Ord)

maybeParen :: TypePrec -> TypePrec -> ClashDoc -> ClashDoc
maybeParen ctxt_prec inner_prec = parensIf (ctxt_prec >= inner_prec)

pprType :: Env -> Type n -> ClashDoc
pprType env = pprType' env TopPrec

pprType' :: Env -> TypePrec -> Type n -> ClashDoc
pprType' env p ty = case ty of
  VarTy occ _ n -> ppr (tyVarName env occ n)
  LitTy l -> ppr l
  ForAllTy {} -> maybeParen p FunPrec (pprForAllType env ty)
  ConstTy (TyCon tc) -> pprTcApp env p tc []
  AnnType _ t -> pprType' env p t
  _ | TyConAppView tc args <- tyView ty -> pprTcApp env p tc args
    | FunTyView arg res <- tyView ty -> pprFunTy env p arg res
  AppTy t1 t2 ->
    maybeParen p TyConPrec (pprType env t1 <+> pprType' env TyConPrec t2)
  ConstTy Arrow -> parens rarrow

-- | A chain of function types
pprFunTy :: Env -> TypePrec -> Type n -> Type n -> ClashDoc
pprFunTy env p arg0 res0 =
  maybeParen p FunPrec (sep [pprType' env FunPrec arg0, sep (map (rarrow <+>) (tails res0))])
 where
  tails res = case tyView res of
    FunTyView arg res' -> pprType' env FunPrec arg : tails res'
    _ -> [pprType env res]

-- | The type variables of consecutive foralls, and the type in their scope
pprForAllType :: Env -> Type n -> ClashDoc
pprForAllType env0 ty0 = go [] env0 ty0
 where
  go :: [ClashDoc] -> Env -> Type x -> ClashDoc
  go tvs env = \case
    ForAllTy b t ->
      let (nm, env') = bindTy env b
          tv = tyParens (ppr nm <> annotate (AnnSyntax Type)
                                     (space <> dcolon <+> pprType env (tyBinderKind b)))
      in go (tv : tvs) env' t
    rho -> sep [forall_ <+> sep (reverse tvs) <> dot, pprType env rho]

pprTcApp :: Env -> TypePrec -> TyConName -> [Type n] -> ClashDoc
pprTcApp env p tc tys
  | null tys
  = ppr tc

  | isTupleTyConLike tc
  = parens $ sep $ punctuate comma (map (pprType' env TopPrec) tys)

  | isSym
  , [ty1, ty2] <- tys
  = maybeParen p FunPrec $
      sep [pprType' env FunPrec ty1, enclose "`" "`" (ppr tc) <+> pprType' env FunPrec ty2]

  | otherwise
  = maybeParen p TyConPrec $
      hang 2 $ sep (parensIf isSym (ppr tc) : map (pprType' env TyConPrec) tys)
 where
  isSym = isSymName tc

-- | The view on types of 'Clash.Core.Scoped.Type.tyView', which imports this
-- module
data TypeView n
  = FunTyView (Type n) (Type n)
  | TyConAppView TyConName [Type n]
  | OtherTypeView

tyView :: Type n -> TypeView n
tyView = go []
 where
  go args = \case
    ConstTy (TyCon tc) -> TyConAppView tc args
    ConstTy Arrow -> case args of
      [arg, res] -> FunTyView arg res
      -- With the kinds of the argument and the result, see Note [Arrow
      -- arguments] in "Clash.Core.Type"
      [_, _, arg, res] -> FunTyView arg res
      _ -> OtherTypeView
    AppTy l r -> go (r : args) l
    _ -> OtherTypeView

-- * Type constructors

pprTyCon :: TyCon -> ClashDoc
pprTyCon tc = case tc of
  AlgTyCon {algTcRhs = DataTyCon dcs} ->
    vsep (data_ <+> decl : map pprDataCon dcs)
  AlgTyCon {algTcRhs = NewTyCon dc _ _} ->
    vsep [newtype_ <+> decl, pprDataCon dc]
  PromotedDataCon {tyConData = dc} ->
    "promoted" <+> ppr (dcName dc)
  FunTyCon {tyConEqns = eqns} ->
    vsep (type_ <+> family_ <+> decl : map pprEqn eqns)
  PrimTyCon {} ->
    decl
 where
  decl = ppr (tyConName tc)
    <> annotate (AnnSyntax Type) (space <> dcolon <+> pprType emptyEnv (tyConKind tc))

  pprDataCon dc = ppr (dcName dc)
    <> annotate (AnnSyntax Type) (space <> dcolon <+> pprType emptyEnv (dcType dc))

  pprEqn (TyFamEqn tvs lhs rhs) =
    let (_, env) = pprTele pprTyBinder emptyEnv tvs
    in type_ <+> instance_ <+> pprTcApp env TopPrec (tyConName tc) lhs
         <+> "=" <+> pprType env rhs

-- * Syntax

dcolon, rarrow, lam, tylam, at, cast, coerce, let_, letrec, in_, case_, of_, forall_,
  data_, newtype_, type_, family_, instance_
  :: ClashDoc
dcolon = annotate (AnnSyntax Keyword) "::"
rarrow = annotate (AnnSyntax Keyword) "->"
lam = annotate (AnnSyntax Keyword) "λ"
tylam = annotate (AnnSyntax Keyword) "Λ"
at = annotate (AnnSyntax Keyword) "@"
cast = annotate (AnnSyntax Keyword) "▷"
coerce = annotate (AnnSyntax Keyword) "~"
let_ = annotate (AnnSyntax Keyword) "let"
letrec = annotate (AnnSyntax Keyword) "letrec"
in_ = annotate (AnnSyntax Keyword) "in"
case_ = annotate (AnnSyntax Keyword) "case"
of_ = annotate (AnnSyntax Keyword) "of"
forall_ = annotate (AnnSyntax Keyword) "forall"
data_ = annotate (AnnSyntax Keyword) "data"
newtype_ = annotate (AnnSyntax Keyword) "newtype"
type_ = annotate (AnnSyntax Keyword) "type"
family_ = annotate (AnnSyntax Keyword) "family"
instance_ = annotate (AnnSyntax Keyword) "instance"

parensIf :: Bool -> ClashDoc -> ClashDoc
parensIf False = id
parensIf True = parens

tyParens :: ClashDoc -> ClashDoc
tyParens = enclose (annotate (AnnSyntax Type) lparen) (annotate (AnnSyntax Type) rparen)

tyParensIf :: Bool -> ClashDoc -> ClashDoc
tyParensIf False = id
tyParensIf True = tyParens

vsepHard :: [ClashDoc] -> ClashDoc
vsepHard = concatWith (\x y -> x <> hardline <> y)

isSymName :: C.Name a -> Bool
isSymName n = go (C.nameOcc n)
 where
  go s | T.null s = False
       | isUpper (T.head s) = isLexConSym s
       | otherwise = isLexSym s

isLexSym :: Text -> Bool
isLexSym cs = isLexConSym cs || isLexVarSym cs

isLexConSym :: Text -> Bool
isLexConSym "->" = True
isLexConSym cs = T.head cs == ':'

isLexVarSym :: Text -> Bool
isLexVarSym cs = startsVarSym (T.head cs)

startsVarSym :: Char -> Bool
startsVarSym c = isSymbolASCII c || (ord c > 0x7f && isSymbol c)

isSymbolASCII :: Char -> Bool
isSymbolASCII c = c `elem` ("!#$%&*+./<=>?@\\^|~-" :: String)
