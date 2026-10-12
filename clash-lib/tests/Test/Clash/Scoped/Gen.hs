{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  QuickCheck generators of well-scoped Clash Core ("Clash.Core.Term"), to test
  scoped Clash Core ("Clash.Core.Scoped") against the current implementation.

  A generated term is /well-scoped/ in the scope of its free variables
  ('FreeVars'): every local variable it mentions is bound by an enclosing
  binder or is one of the free variables, every occurrence carries the type of
  its binder, and the types of occurrences and binders only mention type
  variables in scope at them. The term need not be well-typed, but kinds are
  plausible: a type is generated at a kind, and a type variable is only used
  at its kind.

  The generators make names clash on purpose:

  * binders reuse the uniques of variables in scope of the same sort, i.e.
    shadow them;

  * binders reuse the uniques of variables in scope of the other sort: type
    and term variables can share a unique
    (<https://github.com/clash-lang/clash-compiler/issues/1046>);

  * binders, and free variables, reuse the uniques of 'globals' (D5).

  A type variable binder that shadows a type variable @a@ hides the variables
  whose types (or kinds) mention @a@, including itself if its kind does, as
  an occurrence of them would mention the binder rather than @a@. That is what
  makes shadowing type variables legal Clash Core.

  The signatures of data constructors, globals and primitives are fixed, see
  'dataCons', 'globals' and 'prims'.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Clash.Scoped.Gen
  ( -- * Signatures
    dataCons
  , tyConMap
  , fDataCon
  , kDataCon
  , globals
  , prims
    -- * Free variables
  , FreeVars (..)
  , genFreeVars
  , genMoreFreeVars
  , freeVarsUniqs
  , inScopeOf
    -- * Generating
  , G
  , runG
  , Ctx
  , ctxTyVars
  , ctxIds
  , ctxOf
  , bindTyVar
  , bindId
  , genKind
  , genType
  , kindOf
  , genTerm
  , genIdBinder
  , genTyBinder
  , frequencyG
  , elementsG
    -- * Inputs of properties
  , InScope (..)
  , genInScope
  , shrinkInScope
  , Sunk (..)
  , genSunk
  , shrinkSunk
  , Redex (..)
  , genRedex
  , shrinkRedex
  , genSameRhsCase
  , DataConInst (..)
  , KnownCon (..)
  , genKnownCon
  , shrinkKnownCon
  , genDataConInst
  , genPerturbed
  , renameBinders
  , termSize
  , termFeatures
    -- * Clash Core
  , patBindersFreeIn
    -- * Shrinking
  , shrinkTerm
  , shrinkRaw
  , wellScopedIn
    -- * Showing
  , showTerm
  , showType
  , showFreeVars
  ) where

import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import qualified Control.Lens as Lens
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty.QuickCheck

import Clash.Annotations.SynthesisAttributes (Attr (..))
import qualified Clash.Core.DataCon as C
import qualified Clash.Core.FreeVars as C
import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import Clash.Core.Pretty (PrettyOptions (..), PrettyPrec, showPpr')
import qualified Clash.Core.Subst as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Var as C
import Clash.Core.VarEnv (InScopeSet, emptyInScopeSet, extendInScopeSetList)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Unique (Unique)

import Test.Clash.Rewrite
  ( existentialDataCon, existentialTy, globalId, intTy, intTyCon, kindedTyVar
  , localId, mkDataCon, pairDataCon, pairTy, parseTyConTy, tyVar)

-- * Signatures

-- | @Type -> Type@
arrKind :: C.Kind
arrKind = C.mkFunTy C.liftedTypeKind C.liftedTypeKind

-- | Type constructors, besides those of 'Test.Clash.Rewrite': @F, Maybe ::
-- Type -> Type@, @Either :: Type -> Type -> Type@, @Signed :: Nat -> Type@,
-- @Lbl :: Symbol -> Type@, @P :: forall k. k -> Type@ and @K :: Type -> Type@.
fTy, maybeTy, eitherTy, signedTy, lblTy, pTy, kTy, nTy :: C.Type
fTy = parseTyConTy "F"
maybeTy = tyConTy "Maybe" 2001
eitherTy = tyConTy "Either" 2002
signedTy = tyConTy "Signed" 2003
lblTy = parseTyConTy "Lbl"
pTy = parseTyConTy "P"
kTy = parseTyConTy "K"
nTy = parseTyConTy "N"

-- | A type constructor with the given name and unique
tyConTy :: Text -> Unique -> C.Type
tyConTy nm u = C.ConstTy (C.TyCon (C.mkUnsafeName C.User nm u))

-- | @MkF :: forall a. forall b. a -> b -> F a@, with an existential type
-- variable @b@
fDataCon :: C.DataCon
fDataCon = mkDataCon "MkF" 1003 [a] [b] [C.VarTy a, C.VarTy b] (C.AppTy fTy (C.VarTy a))
 where
  a = tyVar C.User "a" 60
  b = tyVar C.User "b" 61

-- | @MkK :: forall (k :: Type). forall (x :: k). P k x -> Signed 8 -> K k@:
-- the kind of the existential type variable mentions the universal one
kDataCon :: C.DataCon
kDataCon =
  mkDataCon "MkK" 1004 [k] [x]
    [C.AppTy (C.AppTy pTy (C.VarTy k)) (C.VarTy x), C.AppTy signedTy (C.LitTy (C.NumTy 8))]
    (C.AppTy kTy (C.VarTy k))
 where
  k = tyVar C.User "k" 62
  x = kindedTyVar C.User "x" 63 (C.VarTy k)

-- | @Just :: forall a. a -> Maybe a@
justDataCon :: C.DataCon
justDataCon = mkDataCon "Just" 1005 [a] [] [C.VarTy a] (C.AppTy maybeTy (C.VarTy a))
 where
  a = tyVar C.User "a" 64

-- | @Nothing :: forall a. Maybe a@
nothingDataCon :: C.DataCon
nothingDataCon = (mkDataCon "Nothing" 1006 [a] [] [] (C.AppTy maybeTy (C.VarTy a))) {C.dcTag = 2}
 where
  a = tyVar C.User "a" 65

-- | @MkN :: forall (n :: Nat). Signed n -> N@
nDataCon :: C.DataCon
nDataCon = mkDataCon "MkN" 1007 [] [n] [C.AppTy signedTy (C.VarTy n)] nTy
 where
  n = kindedTyVar C.User "n" 66 C.typeNatKind

-- | The data constructors that generated terms use: 'pairDataCon',
-- 'existentialDataCon', 'fDataCon', 'kDataCon', and ones for @Maybe@ and a
-- type with a type-level natural.
dataCons :: [C.DataCon]
dataCons =
  [pairDataCon, existentialDataCon, fDataCon, kDataCon, justDataCon, nothingDataCon, nDataCon]

-- | The type constructors of the types that generated terms use, and the
-- primitive ones, for functions that need them
tyConMap :: C.TyConMap
tyConMap = foldr UniqMap.insertUnique C.tysPrimMap tcs
 where
  tcs =
    [ intTyCon
    , alg pairTy C.liftedTypeKind 0 [pairDataCon]
    , alg existentialTy C.liftedTypeKind 0 [existentialDataCon]
    , alg fTy arrKind 1 [fDataCon]
    , alg kTy arrKind 1 [kDataCon]
    , alg maybeTy arrKind 1 [justDataCon, nothingDataCon]
    , alg nTy C.liftedTypeKind 0 [nDataCon]
    , prim eitherTy (C.mkFunTy C.liftedTypeKind arrKind) 2
    , prim signedTy (C.mkFunTy C.typeNatKind C.liftedTypeKind) 1
    , prim lblTy (C.mkFunTy C.typeSymbolKind C.liftedTypeKind) 1
    , prim pTy (C.ForAllTy k (C.mkFunTy (C.VarTy k) C.liftedTypeKind)) 2
    ]
  k = tyVar C.User "k" 69

  tyConName = \case
    C.ConstTy (C.TyCon nm) -> nm
    ty -> error ("tyConMap: not a type constructor: " <> show ty)

  alg ty kind arity dcs =
    let nm = tyConName ty
    in  C.AlgTyCon (C.nameUniq nm) nm kind arity (C.DataTyCon dcs) False

  prim ty kind arity =
    let nm = tyConName ty
    in  C.PrimTyCon (C.nameUniq nm) nm kind arity

-- | The global variables that generated terms use. Their uniques are small, so
-- that they clash with those of local variables (D5).
globals :: [C.Id]
globals =
  [ globalId C.User "f" 2 (C.mkFunTy intTy intTy)
  , globalId C.User "g" 4 (C.ForAllTy a (C.mkFunTy (C.VarTy a) (C.VarTy a)))
  , globalId C.User "h" 6 pairTy
  ]
 where
  a = tyVar C.User "a" 67

-- | The primitives that generated terms use
prims :: [C.PrimInfo]
prims =
  [ C.PrimInfo "Test.plus" (C.mkFunTy intTy (C.mkFunTy intTy intTy)) C.WorkNever C.SingleResult C.NoUnfolding
  , C.PrimInfo "Test.undefined" (C.ForAllTy a (C.VarTy a)) C.WorkNever C.SingleResult C.NoUnfolding
  ]
 where
  a = tyVar C.User "a" 68

-- * Free variables

-- | The free variables of a generated term, i.e. the variables of the scope it
-- is generated in. Type variables come first; their kinds only mention the type
-- variables before them. All have different uniques.
data FreeVars = FreeVars
  { fvTyVars :: [C.TyVar]
  , fvIds :: [C.Id]
  }

instance Semigroup FreeVars where
  FreeVars tvs1 ids1 <> FreeVars tvs2 ids2 = FreeVars (tvs1 <> tvs2) (ids1 <> ids2)

-- | The uniques of free variables
freeVarsUniqs :: FreeVars -> [Unique]
freeVarsUniqs (FreeVars tvs ids) = map C.varUniq tvs <> map C.varUniq ids

-- | An in-scope set with the free variables
inScopeOf :: FreeVars -> InScopeSet
inScopeOf (FreeVars tvs ids) =
  extendInScopeSetList (extendInScopeSetList emptyInScopeSet tvs) ids

-- | Free variables with the uniques @1..n@, in random order, so that some clash
-- with those of 'globals'
genFreeVars :: Gen FreeVars
genFreeVars = do
  nTvs <- choose (0, 3)
  nIds <- choose (0, 4)
  uniqs <- shuffle [1 .. fromIntegral (nTvs + nIds)]
  runG 100 (genFreeVarsWith (FreeVars [] []) nTvs uniqs)

-- | More free variables, whose uniques are just above those of the given ones:
-- the names that conversion gives to the binders of a term converted in the
-- scope of the given free variables. Sinking such a term into the scope of
-- both makes its binders clash with the new variables.
genMoreFreeVars :: FreeVars -> Gen FreeVars
genMoreFreeVars fvs = do
  nTvs <- choose (0, 2)
  nIds <- choose (1, 4)
  let above = maximum (0 : freeVarsUniqs fvs)
  uniqs <- take (nTvs + nIds) <$> shuffle [above + 1 .. above + 12]
  runG 200 (genFreeVarsWith fvs nTvs uniqs)

-- | Free variables with the given uniques, the first ones type variables, in
-- the scope of the given free variables
genFreeVarsWith :: FreeVars -> Int -> [Unique] -> G FreeVars
genFreeVarsWith fvs0 nTvs uniqs = do
  let (tvUniqs, idUniqs) = splitAt nTvs uniqs
  tvs <- goTvs (ctxOf fvs0) tvUniqs
  let ctx1 = foldl (flip bindTyVar) (ctxOf fvs0) tvs
  ids <- mapM (\u -> do ty <- genType ctx1 C.liftedTypeKind 2
                        nm <- elementsG ["x", "y", "z"]
                        pure (localId C.User nm u ty)) idUniqs
  pure (FreeVars tvs ids)
 where
  goTvs _ [] = pure []
  goTvs ctx (u:us) = do
    k <- genKind ctx
    nm <- elementsG ["a", "b", "c"]
    let tv = kindedTyVar C.User nm u k
    (tv :) <$> goTvs (bindTyVar tv ctx) us

-- * Generating

-- | Generators with a supply of fresh uniques
type G = StateT Unique Gen

-- | Run a generator, with fresh uniques from the given one on
runG :: Unique -> G a -> Gen a
runG = flip evalStateT

liftG :: Gen a -> G a
liftG = lift

freshUniq :: G Unique
freshUniq = state (\u -> (u, u + 1))

-- | 'frequency' for 'G'
frequencyG :: [(Int, G a)] -> G a
frequencyG alts = do
  i <- liftG (frequency [(w, pure i) | (i, (w, _)) <- zip [0 :: Int ..] alts, w > 0])
  snd (alts !! i)

-- | 'elements' for 'G'
elementsG :: [a] -> G a
elementsG = liftG . elements

-- | The variables in scope where a term is generated
data Ctx = Ctx
  { ctxTyVars :: [C.TyVar]
  -- ^ Type variables in scope, that are not shadowed, innermost first
  , ctxIds :: [C.Id]
  -- ^ Local term variables in scope, that are not shadowed, innermost first
  }

-- | The context of the free variables
ctxOf :: FreeVars -> Ctx
ctxOf (FreeVars tvs ids) = Ctx (reverse tvs) (reverse ids)

-- | Bring a type variable into scope. Hides the type variable with the same
-- unique, and the variables whose types or kinds mention it. If the kind of
-- the new type variable mentions the one it hides, it is hidden too: the kind
-- of an occurrence would mention the new type variable.
bindTyVar :: C.TyVar -> Ctx -> Ctx
bindTyVar tv (Ctx tvs ids) = Ctx ([tv | not (mentions [u] (C.varType tv))] <> tvs') ids'
 where
  u = C.varUniq tv
  -- Outermost first, so that a kind only mentions type variables before it
  (tvs', hidden) = List.foldl' visit ([], [u]) (reverse tvs)
  visit (acc, hid) v
    | C.varUniq v `elem` hid || mentions hid (C.varType v) = (acc, C.varUniq v : hid)
    | otherwise = (v : acc, hid)
  ids' = filter (not . mentions hidden . C.varType) ids
  mentions hid ty = any ((`elem` hid) . C.varUniq) (Lens.toListOf C.typeFreeVars ty)

-- | Bring a term variable into scope, hiding the one with the same unique
bindId :: C.Id -> Ctx -> Ctx
bindId v (Ctx tvs ids) = Ctx tvs (v : filter ((/= C.varUniq v) . C.varUniq) ids)

-- | The sort of a binder
data Sort = TySort | TmSort

-- | The unique of a binder: mostly fresh, but it may also reuse the unique of a
-- variable in scope, of either sort, or of a global. Never one of the given
-- uniques.
genBinderUniq :: [Unique] -> Ctx -> Sort -> G Unique
genBinderUniq avoid (Ctx tvs ids) sort = do
  u <- frequencyG $
    [(10, freshUniq)] <>
    [(3, elementsG same) | not (null same)] <>
    [(1, elementsG other) | not (null other)] <>
    [(1, elementsG (map C.varUniq globals))]
  if u `elem` avoid then freshUniq else pure u
 where
  (same, other) = case sort of
    TySort -> (map C.varUniq tvs, map C.varUniq ids)
    TmSort -> (map C.varUniq ids, map C.varUniq tvs)

-- | A kind: mostly @Type@, sometimes @Type -> Type@, @Nat@, @Symbol@, or a
-- type variable of kind @Type@
genKind :: Ctx -> G C.Kind
genKind ctx = frequencyG $
  [ (8, pure C.liftedTypeKind)
  , (2, pure arrKind)
  , (2, pure C.typeNatKind)
  , (1, pure C.typeSymbolKind)
  ] <>
  [(2, C.VarTy <$> elementsG kvs) | not (null kvs)]
 where
  kvs = [tv | tv <- ctxTyVars ctx, C.varType tv `C.aeqType` C.liftedTypeKind]

-- | A type of the given kind, of at most the given depth
genType :: Ctx -> C.Kind -> Int -> G C.Type
genType ctx k n
  | n <= 0 || null nodes = frequencyG leaves
  | otherwise = frequencyG (leaves <> nodes)
 where
  vars = [C.VarTy tv | tv <- ctxTyVars ctx, C.varType tv `C.aeqType` k]
  varLeaf = [(4, elementsG vars) | not (null vars)]
  sub = genType ctx C.liftedTypeKind (n - 1)

  (leaves, nodes)
    | k `C.aeqType` C.liftedTypeKind =
        ( varLeaf <> [(3, elementsG [intTy, pairTy, existentialTy])]
        , [ (3, C.mkFunTy <$> sub <*> sub)
          , (2, genForAll)
          , (2, C.AppTy <$> genType ctx arrKind (n - 1) <*> sub)
          , (1, C.AnnType <$> genAttrs <*> sub)
          , (1, C.AppTy signedTy <$> genType ctx C.typeNatKind (n - 1))
          , (1, C.AppTy lblTy <$> genType ctx C.typeSymbolKind (n - 1))
          , (1, genP)
          ] )
    | k `C.aeqType` arrKind =
        ( varLeaf <> [(2, elementsG [fTy, maybeTy])]
        , [(2, C.AppTy eitherTy <$> sub)] )
    | k `C.aeqType` C.typeNatKind =
        (varLeaf <> [(3, C.LitTy . C.NumTy <$> liftG (choose (0, 16)))], [])
    | k `C.aeqType` C.typeSymbolKind =
        (varLeaf <> [(3, C.LitTy . C.SymTy <$> elementsG ["a", "b"])], [])
    | null vars =
        -- No type of this kind. Plausible is good enough.
        ([(1, pure intTy)], [])
    | otherwise = (varLeaf, [])

  genForAll = do
    (tv, ctx') <- genTyBinder [] ctx
    C.ForAllTy tv <$> genType ctx' C.liftedTypeKind (n - 1)

  -- @P k x@ for some kind @k@ and type @x :: k@: a kind that has types
  genP = do
    kv <- elementsG $
      [C.liftedTypeKind, arrKind, C.typeNatKind, C.typeSymbolKind] <>
      map C.varType (ctxTyVars ctx)
    x <- genType ctx kv (n - 1)
    pure (C.AppTy (C.AppTy pTy kv) x)

-- | The kind of a type, if it is well-kinded, given the kinds of the type
-- constructors in 'tyConMap'.
kindOf :: C.Type -> Maybe C.Kind
kindOf = \case
  C.VarTy tv -> Just (C.varType tv)
  C.ConstTy (C.TyCon nm) -> C.tyConKind <$> UniqMap.lookup nm tyConMap
  C.ConstTy C.Arrow -> Just (C.mkFunTy C.liftedTypeKind arrKind)
  C.ForAllTy _ t -> kindOf t
  C.LitTy (C.NumTy _) -> Just C.typeNatKind
  C.LitTy (C.SymTy _) -> Just C.typeSymbolKind
  C.LitTy (C.CharTy _) -> Just C.charPrimTy
  C.AnnType _ t -> kindOf t
  C.AppTy f a -> do
    kf <- kindOf f
    ka <- kindOf a
    case kf of
      -- A dependent kind, like that of @P@
      C.ForAllTy kv res -> Just (C.substTyWith [kv] [a] res)
      _ | C.FunTy dom res <- C.tyView kf, dom `C.aeqType` ka -> Just res
        | otherwise -> Nothing

genAttrs :: G [Attr Text]
genAttrs = do
  n <- liftG (choose (1, 2))
  replicateM n (elementsG [BoolAttr "keep" True, IntegerAttr "depth" 3, StringAttr "chip" "x", Attr "dont_touch"])

-- | A type variable binder whose unique is not one of the given ones, and the
-- context in its scope
genTyBinder :: [Unique] -> Ctx -> G (C.TyVar, Ctx)
genTyBinder avoid ctx = do
  u <- genBinderUniq avoid ctx TySort
  k <- genKind ctx
  nm <- elementsG ["a", "b", "k"]
  let tv = kindedTyVar C.User nm u k
  pure (tv, bindTyVar tv ctx)

-- | A term variable binder whose unique is not one of the given ones, and the
-- context in its scope
genIdBinder :: [Unique] -> Ctx -> G (C.Id, Ctx)
genIdBinder avoid ctx = do
  u <- genBinderUniq avoid ctx TmSort
  ty <- genType ctx C.liftedTypeKind 2
  nm <- elementsG ["x", "y", "z"]
  let v = localId C.User nm u ty
  pure (v, bindId v ctx)

-- | Term variable binders with different uniques, whose types are in the
-- given context, and the context in their scope
genIdBinders :: [Unique] -> Ctx -> [C.Type] -> G ([C.Id], Ctx)
genIdBinders avoid ctx0 tys = go avoid ctx0 tys
 where
  go _ ctx [] = pure ([], ctx)
  go av ctx (ty:rest) = do
    u <- genBinderUniq av ctx0 TmSort
    nm <- elementsG ["x", "y", "z"]
    let v = localId C.User nm u ty
    (vs, ctx') <- go (u : av) (bindId v ctx) rest
    pure (v : vs, ctx')

-- | Split a size over a subterm and the given number of other subterms
splitSize :: Int -> Int -> G (Int, [Int])
splitSize n k = do
  cuts <- List.sort <$> replicateM k (liftG (choose (0, n)))
  pure (case cuts of
    [] -> (n, [])
    c:_ -> (c, zipWith (-) (drop 1 cuts <> [n]) cuts))

-- | Split a size over two subterms
splitSize2 :: Int -> G (Int, Int)
splitSize2 n = do
  n1 <- liftG (choose (0, n))
  pure (n1, n - n1)

-- | A term with about the given number of nodes
genTerm :: Ctx -> Int -> G C.Term
genTerm ctx n
  | n <= 0 = leaf
  | otherwise = frequencyG
      [ (1, leaf)
      , (3, genLam)
      , (2, genTyLam)
      , (3, genApp)
      , (2, C.TyApp <$> genTerm ctx (n - 1) <*> (genKind ctx >>= \k -> genType ctx k 2))
      , (2, genNonRec)
      , (1, genRec)
      , (2, genCase)
      , (1, genCast)
      , (1, genTick)
      ]
 where
  leaf = frequencyG $
    [(6, C.Var <$> elementsG (ctxIds ctx)) | not (null (ctxIds ctx))] <>
    [ (2, C.Var <$> elementsG globals)
    , (1, C.Literal . C.IntLiteral <$> liftG (choose (0, 3)))
    , (1, C.Data <$> elementsG dataCons)
    , (1, C.Prim <$> elementsG prims)
    ]

  genLam = do
    (v, ctx') <- genIdBinder [] ctx
    C.Lam v <$> genTerm ctx' (n - 1)

  genTyLam = do
    (tv, ctx') <- genTyBinder [] ctx
    C.TyLam tv <$> genTerm ctx' (n - 1)

  genApp = do
    (n1, n2) <- splitSize2 (n - 1)
    C.App <$> genTerm ctx n1 <*> genTerm ctx n2

  genNonRec = do
    (n1, n2) <- splitSize2 (n - 1)
    x <- genTerm ctx n1
    (v, ctx') <- genIdBinder [] ctx
    C.Let (C.NonRec v x) <$> genTerm ctx' n2

  genRec = do
    k <- liftG (choose (1, 3))
    tys <- replicateM k (genType ctx C.liftedTypeKind 2)
    (vs, ctx') <- genIdBinders [] ctx tys
    (nBody, ns) <- splitSize (n - 1) k
    xs <- mapM (genTerm ctx') ns
    C.Let (C.Rec (zip vs xs)) <$> genTerm ctx' nBody

  genCase = do
    k <- liftG (choose (1, 3))
    (nSubj, ns) <- splitSize (n - 1) k
    subj <- genTerm ctx nSubj
    ty <- genType ctx C.liftedTypeKind 2
    C.Case subj ty <$> mapM (genAlt ctx) ns

  genCast = do
    e <- genTerm ctx (n - 1)
    C.Cast e <$> genType ctx C.liftedTypeKind 2 <*> genType ctx C.liftedTypeKind 2

  genTick = do
    t <- frequencyG
      [ (1, pure (C.SrcSpan C.noSrcSpan))
      , (2, C.NameMod <$> elementsG [C.PrefixName, C.SuffixName, C.SetName]
                      <*> genType ctx C.typeSymbolKind 1)
      , (1, pure C.DeDup)
      , (1, pure C.NoDeDup)
      , (2, C.Attributes <$> genType ctx C.liftedTypeKind 1 <*> genTerm ctx (n `div` 3))
      ]
    C.Tick t <$> genTerm ctx (n - 1)

-- | A case alternative
genAlt :: Ctx -> Int -> G C.Alt
genAlt ctx n = frequencyG
  [ (5, genDataAlt ctx n)
  , (1, (,) . C.LitPat . C.IntLiteral <$> liftG (choose (0, 3)) <*> genTerm ctx n)
  , (1, (,) C.DefaultPat <$> genTerm ctx n)
  ]

-- | A case alternative with a pattern that matches a data constructor
genDataAlt :: Ctx -> Int -> G C.Alt
genDataAlt ctx n = do
  dc <- elementsG dataCons
  let univ = C.dcUnivTyVars dc
  univTys <- mapM (\tv -> genType ctx (C.varType tv) 1) univ
  -- The existentials must not capture the free type variables of the
  -- instantiation of the universals: the field types mention them.
  let avoid = map C.varUniq (concatMap (Lens.toListOf C.typeFreeVars) univTys)
  (exts, ctx1) <- goExts avoid ctx (zip univ univTys) (C.dcExtTyVars dc)
  let inst = C.substTyWith (univ <> C.dcExtTyVars dc) (univTys <> map C.VarTy exts)
  (ids, ctx2) <- genIdBinders [] ctx1 (map inst (C.dcArgTys dc))
  (,) (C.DataPat dc exts ids) <$> genTerm ctx2 n
 where
  -- Existentials, whose kinds may mention the universals and the existentials
  -- before them
  goExts _ ctx' _ [] = pure ([], ctx')
  goExts avoid ctx' sub (tv:tvs) = do
    u <- genBinderUniq avoid ctx' TySort
    let (vs, tys) = unzip sub
        k = C.substTyWith vs tys (C.varType tv)
        tv' = kindedTyVar C.User "e" u k
    (tvs', ctx'') <- goExts (u : avoid) (bindTyVar tv' ctx') (sub <> [(tv, C.VarTy tv')]) tvs
    pure (tv' : tvs', ctx'')

-- * Inputs of properties

-- | The number of nodes of the terms that the inputs below have, given the
-- QuickCheck size: up to about 33
termSize :: Int -> Int
termSize s = s `div` 3

-- | The unique fresh binders start at: above those of the free variables
firstFresh :: Unique
firstFresh = 30

-- | A term in the scope of its free variables
data InScope = InScope FreeVars C.Term

instance Show InScope where
  show (InScope fvs tm) = showFreeVars fvs <> "\nterm:\n" <> showTerm tm

genInScope :: Gen InScope
genInScope = do
  fvs <- genFreeVars
  sized $ \s -> InScope fvs <$> runG firstFresh (genTerm (ctxOf fvs) (termSize s))

shrinkInScope :: InScope -> [InScope]
shrinkInScope (InScope fvs tm) = InScope fvs <$> shrinkTerm fvs tm

-- | A term in the scope of the first free variables, to be converted there and
-- 'Clash.Core.Scoped.Syntax.sink'ed into the scope of both, where the second
-- free variables clash with its binders (see 'genMoreFreeVars')
data Sunk = Sunk FreeVars FreeVars C.Term

instance Show Sunk where
  show (Sunk fvs0 fvs1 tm) =
    showFreeVars fvs0 <> "\nsunk into the scope of:\n" <> showFreeVars fvs1 <>
      "\nterm:\n" <> showTerm tm

genSunk :: Gen Sunk
genSunk = do
  fvs0 <- genFreeVars
  fvs1 <- genMoreFreeVars fvs0
  sized $ \s -> Sunk fvs0 fvs1 <$> runG firstFresh (genTerm (ctxOf fvs0) (termSize s))

shrinkSunk :: Sunk -> [Sunk]
shrinkSunk (Sunk fvs0 fvs1 tm) = Sunk fvs0 fvs1 <$> shrinkTerm fvs0 tm

-- | A beta-redex: a lambda or type lambda in the scope of the first free
-- variables, as in 'Sunk', and an argument in the scope of both
data Redex
  = TmRedex FreeVars FreeVars C.Id C.Term C.Term
  -- ^ @(\x -> body) arg@
  | TyRedex FreeVars FreeVars C.TyVar C.Term C.Type
  -- ^ @(/\a -> body) \@ty@

instance Show Redex where
  show = \case
    TmRedex fvs0 fvs1 x body arg ->
      common fvs0 fvs1 (C.Lam x body) <> "\nargument:\n" <> showTerm arg
    TyRedex fvs0 fvs1 a body ty ->
      common fvs0 fvs1 (C.TyLam a body) <> "\nargument:\n" <> showType ty
   where
    common fvs0 fvs1 fun =
      showFreeVars fvs0 <> "\nsunk into the scope of:\n" <> showFreeVars fvs1 <>
        "\nfunction:\n" <> showTerm fun

genRedex :: Gen Redex
genRedex = do
  fvs0 <- genFreeVars
  fvs1 <- genMoreFreeVars fvs0
  let ctx0 = ctxOf fvs0
      ctx01 = ctxOf (fvs0 <> fvs1)
  sized $ \s -> runG firstFresh $ frequencyG
    [ (2, do (x, ctx) <- genIdBinder [] ctx0
             body <- genTerm ctx (termSize s)
             TmRedex fvs0 fvs1 x body <$> genTerm ctx01 (termSize s `div` 3))
      -- A binder with the unique of a global (D5), which the body likely
      -- mentions
    , (1, do g <- elementsG globals
             ty <- genType ctx0 C.liftedTypeKind 2
             let x = localId C.User "x" (C.varUniq g) ty
             body <- genTerm (bindId x ctx0) (termSize s)
             TmRedex fvs0 fvs1 x (C.App body (C.Var g)) <$> genTerm ctx01 (termSize s `div` 3))
    , (1, do (a, ctx) <- genTyBinder [] ctx0
             body <- genTerm ctx (termSize s)
             TyRedex fvs0 fvs1 a body <$> genType ctx01 (C.varType a) 2)
    ]

shrinkRedex :: Redex -> [Redex]
shrinkRedex = \case
  TmRedex fvs0 fvs1 x body arg ->
    [TmRedex fvs0 fvs1 x body' arg | C.Lam _ body' <- shrinkTerm fvs0 (C.Lam x body)] <>
    [TmRedex fvs0 fvs1 x body arg' | arg' <- shrinkTerm (fvs0 <> fvs1) arg]
  TyRedex fvs0 fvs1 a body ty ->
    [TyRedex fvs0 fvs1 a body' ty | C.TyLam _ body' <- shrinkTerm fvs0 (C.TyLam a body)]

-- | A case alternative that matches a data constructor, in the scope of the
-- first free variables, as in 'Sunk', and types for its existentials and
-- terms for its fields in the scope of both: the alternative of
-- case-of-known-constructor
data KnownCon = KnownCon FreeVars FreeVars C.Alt [C.Type] [C.Term]

instance Show KnownCon where
  show (KnownCon fvs0 fvs1 (pat, rhs) tys tms) =
    showFreeVars fvs0 <> "\nsunk into the scope of:\n" <> showFreeVars fvs1 <>
      "\nalternative:\n" <> showTerm (C.Case (C.Literal (C.IntLiteral 0)) intTy [(pat, rhs)]) <>
      "\ntypes:\n" <> unlines (map showType tys) <> "terms:\n" <> unlines (map showTerm tms)

genKnownCon :: Gen KnownCon
genKnownCon = do
  fvs0 <- genFreeVars
  fvs1 <- genMoreFreeVars fvs0
  let ctx01 = ctxOf (fvs0 <> fvs1)
  sized $ \s -> runG firstFresh $ do
    alt@(pat, _) <- genDataAlt (ctxOf fvs0) (termSize s)
    case pat of
      C.DataPat _ tvs ids -> do
        tys <- mapM (\tv -> genType ctx01 (C.varType tv) 2) tvs
        tms <- mapM (\_ -> genTerm ctx01 (termSize s `div` 4)) ids
        pure (KnownCon fvs0 fvs1 alt tys tms)
      _ -> pure (KnownCon fvs0 fvs1 alt [] [])

shrinkKnownCon :: KnownCon -> [KnownCon]
shrinkKnownCon (KnownCon fvs0 fvs1 (pat, rhs) tys tms) =
  [ KnownCon fvs0 fvs1 (pat, rhs') tys tms
  | C.Case _ _ [(_, rhs')] <- shrinkTerm fvs0 (C.Case (C.Literal (C.IntLiteral 0)) intTy [(pat, rhs)]) ] <>
  [ KnownCon fvs0 fvs1 (pat, rhs) tys (pre <> [tm'] <> post)
  | (pre, tm : post) <- splits tms, tm' <- shrinkTerm (fvs0 <> fvs1) tm ]

-- | Types to instantiate a data constructor with, in the scope of the free
-- variables: types for its universally quantified type variables, and for its
-- existentially quantified ones, of their kinds. Sometimes there is one type too
-- few or too many.
data DataConInst = DataConInst FreeVars C.DataCon [C.Type] [C.Type]

instance Show DataConInst where
  show (DataConInst fvs dc univTys extTys) =
    showFreeVars fvs <> "\ndata constructor: " <> showType (C.dcLegacyType dc) <>
      "\nuniversal types: " <> List.intercalate ", " (map showType univTys) <>
      "\nexistential types: " <> List.intercalate ", " (map showType extTys)

genDataConInst :: Gen DataConInst
genDataConInst = do
  fvs <- genFreeVars
  dc <- elements dataCons
  runG firstFresh $ do
    let ctx = ctxOf fvs
        univ = C.dcUnivTyVars dc
    univTys <- mapM (\tv -> genType ctx (C.varType tv) 2) univ
    let ext = C.dcExtTyVars dc
    extTys <- mapM (\tv -> genType ctx (C.substTyWith univ univTys (C.varType tv)) 2) ext
    frequencyG
      [ (8, pure (DataConInst fvs dc univTys extTys))
      , (1, pure (DataConInst fvs dc univTys (drop 1 extTys)))
      , (1, DataConInst fvs dc univTys . (extTys <>) . pure <$> genType ctx C.liftedTypeKind 1)
      ]

-- | A case-expression whose alternatives have the same right-hand side where
-- that is well-scoped, i.e. where its patterns don't bind variables that it
-- mentions with another type
genSameRhsCase :: Gen InScope
genSameRhsCase = do
  fvs <- genFreeVars
  sized $ \s -> runG firstFresh $ do
    let ctx = ctxOf fvs
    subj <- genTerm ctx 1
    rhs <- genTerm ctx (termSize s `div` 2)
    k <- liftG (choose (1, 3))
    alts <- replicateM k (genAlt ctx 1)
    let alts' = [ if wellScopedIn fvs (C.Case subj intTy [(p, rhs)]) then (p, rhs) else (p, e)
                | (p, e) <- alts ]
    pure (InScope fvs (C.Case subj intTy alts'))

-- | A term like the given one, but changed in one place: a variable occurrence
-- replaced by another one in scope, a literal or type argument changed, a case
-- alternative dropped, or a subterm replaced by a leaf. The result may still
-- be alpha-equivalent to the input.
genPerturbed :: FreeVars -> C.Term -> Gen C.Term
genPerturbed fvs tm0 = runG firstFresh $ case edits (ctxOf fvs) tm0 of
  [] -> pure tm0
  es -> do
    i <- liftG (choose (0, length es - 1))
    es !! i
 where
  -- One generator per position in the term, of the term changed there
  edits :: Ctx -> C.Term -> [G C.Term]
  edits ctx tm = here <> inside
   where
    here = genTerm ctx 0 : case tm of
      C.Var v | not (C.isGlobalId v), not (null (ctxIds ctx)) -> [C.Var <$> elementsG (ctxIds ctx)]
      C.Literal (C.IntLiteral n) -> [pure (C.Literal (C.IntLiteral (n + 1)))]
      C.TyApp e _ -> [C.TyApp e <$> genType ctx C.liftedTypeKind 1]
      C.Case e ty alts@(_:_:_) -> [pure (C.Case e ty (drop 1 alts))]
      _ -> []

    inside = case tm of
      C.Lam v e -> fmap (C.Lam v) <$> edits (bindId v ctx) e
      C.TyLam tv e -> fmap (C.TyLam tv) <$> edits (bindTyVar tv ctx) e
      C.App e1 e2 -> (fmap (`C.App` e2) <$> edits ctx e1) <> (fmap (C.App e1) <$> edits ctx e2)
      C.TyApp e ty -> fmap (`C.TyApp` ty) <$> edits ctx e
      C.Let (C.NonRec v x) e ->
        (fmap (\x' -> C.Let (C.NonRec v x') e) <$> edits ctx x) <>
          (fmap (C.Let (C.NonRec v x)) <$> edits (bindId v ctx) e)
      C.Let (C.Rec bs) e ->
        let ctx' = List.foldl' (flip bindId) ctx (map fst bs)
        in  (fmap (C.Let (C.Rec bs)) <$> edits ctx' e) <>
              [ fmap (\x' -> C.Let (C.Rec (pre <> [(v, x')] <> post)) e) g
              | (pre, (v, x) : post) <- splits bs
              , g <- edits ctx' x ]
      C.Case e ty alts ->
        (fmap (\e' -> C.Case e' ty alts) <$> edits ctx e) <>
          [ fmap (\a' -> C.Case e ty (pre <> [(p, a')] <> post)) g
          | (pre, (p, a) : post) <- splits alts
          , g <- edits (patCtx p ctx) a ]
      C.Cast e t1 t2 -> fmap (\e' -> C.Cast e' t1 t2) <$> edits ctx e
      C.Tick t e -> fmap (C.Tick t) <$> edits ctx e
      _ -> []

  patCtx p ctx = case p of
    C.DataPat _ tvs ids -> List.foldl' (flip bindId) (List.foldl' (flip bindTyVar) ctx tvs) ids
    _ -> ctx

-- | Rename every binder of a term, by adding a million to its unique. Binders
-- that shadow each other still do, but no longer shadow free variables.
renameBinders :: C.Term -> C.Term
renameBinders = goTm (Ren Map.empty Map.empty)
 where
  rename :: C.Var a -> C.Var a
  rename v = let u = C.varUniq v + 1000000 in v {C.varName = (C.varName v) {C.nameUniq = u}, C.varUniq = u}

  bindTv :: Ren -> C.TyVar -> (Ren, C.TyVar)
  bindTv ren@(Ren tvs ids) tv =
    let tv' = (rename tv) {C.varType = goTy ren (C.varType tv)}
    in  (Ren (Map.insert (C.varUniq tv) tv' tvs) ids, tv')

  bindId' :: Ren -> Ren -> C.Id -> (Ren, C.Id)
  bindId' tyRen (Ren tvs ids) v =
    let v' = (rename v) {C.varType = goTy tyRen (C.varType v)}
    in  (Ren tvs (Map.insert (C.varUniq v) v' ids), v')

  bindIds :: Ren -> [C.Id] -> (Ren, [C.Id])
  bindIds ren = List.mapAccumL (bindId' ren) ren

  goTy :: Ren -> C.Type -> C.Type
  goTy ren@(Ren tvs _) = \case
    C.VarTy tv -> C.VarTy (Map.findWithDefault tv (C.varUniq tv) tvs)
    C.ForAllTy tv t -> let (ren', tv') = bindTv ren tv in C.ForAllTy tv' (goTy ren' t)
    C.AppTy t1 t2 -> C.AppTy (goTy ren t1) (goTy ren t2)
    C.AnnType a t -> C.AnnType a (goTy ren t)
    t -> t

  goTm :: Ren -> C.Term -> C.Term
  goTm ren@(Ren _ ids) = \case
    C.Var v
      | C.isGlobalId v -> C.Var v
      | otherwise -> C.Var (Map.findWithDefault v (C.varUniq v) ids)
    C.Lam v e -> let (ren', v') = bindId' ren ren v in C.Lam v' (goTm ren' e)
    C.TyLam tv e -> let (ren', tv') = bindTv ren tv in C.TyLam tv' (goTm ren' e)
    C.App e1 e2 -> C.App (goTm ren e1) (goTm ren e2)
    C.TyApp e ty -> C.TyApp (goTm ren e) (goTy ren ty)
    C.Let (C.NonRec v x) e ->
      let (ren', v') = bindId' ren ren v in C.Let (C.NonRec v' (goTm ren x)) (goTm ren' e)
    C.Let (C.Rec bs) e ->
      let (ren', vs') = bindIds ren (map fst bs)
      in  C.Let (C.Rec (zip vs' (map (goTm ren' . snd) bs))) (goTm ren' e)
    C.Case e ty alts -> C.Case (goTm ren e) (goTy ren ty) (map (goAlt ren) alts)
    C.Cast e t1 t2 -> C.Cast (goTm ren e) (goTy ren t1) (goTy ren t2)
    C.Tick t e -> C.Tick (goTick ren t) (goTm ren e)
    e -> e

  goAlt ren (p, e) = case p of
    C.DataPat dc tvs ids ->
      let (ren1, tvs') = List.mapAccumL bindTv ren tvs
          (ren2, ids') = bindIds ren1 ids
      in  (C.DataPat dc tvs' ids', goTm ren2 e)
    _ -> (p, goTm ren e)

  goTick ren = \case
    C.NameMod m ty -> C.NameMod m (goTy ren ty)
    C.Attributes ty e -> C.Attributes (goTy ren ty) (goTm ren e)
    t -> t

-- | The renamed type and term variables, by their old uniques
data Ren = Ren (Map Unique C.TyVar) (Map Unique C.Id)

-- | The features of a generated term that the generators aim for, to check
-- that they occur often enough: binders that shadow a variable of the same
-- sort, binders and free variables that share a unique with a variable of the
-- other sort or with a global, and the constructs that bind variables.
termFeatures :: FreeVars -> C.Term -> [String]
termFeatures fvs@(FreeVars fTvs fIds) tm0 =
  List.nub (goTm (map C.varUniq fTvs, map C.varUniq fIds) tm0 <> freeFeatures)
 where
  globalUniqs = map C.varUniq globals
  freeFeatures =
    ["free variable shares a unique with a global" | any (`elem` globalUniqs) (freeVarsUniqs fvs)]

  binder sort (tys, tms) v =
    let u = C.varUniq v
        (same, other) = case sort of
          TySort -> (tys, tms)
          TmSort -> (tms, tys)
    in  ["shadowing binder" | u `elem` same] <>
        ["binder shares a unique with a variable of the other sort" | u `elem` other] <>
        ["binder shares a unique with a global" | u `elem` globalUniqs]

  bindTy v (tys, tms) = (C.varUniq v : tys, tms)
  bindTm v (tys, tms) = (tys, C.varUniq v : tms)

  goTy sc = \case
    C.ForAllTy tv t -> "forall" : binder TySort sc tv <> goTy (bindTy tv sc) t
    C.AppTy t1 t2 -> goTy sc t1 <> goTy sc t2
    C.AnnType _ t -> "annotated type" : goTy sc t
    C.LitTy {} -> ["type literal"]
    _ -> []

  goTm sc = \case
    C.Var v | C.isGlobalId v -> ["global"]
    C.Lam v e -> "lambda" : binder TmSort sc v <> goTy sc (C.varType v) <> goTm (bindTm v sc) e
    C.TyLam tv e -> "type lambda" : binder TySort sc tv <> goTm (bindTy tv sc) e
    C.App e1 e2 -> goTm sc e1 <> goTm sc e2
    C.TyApp e ty -> "type application" : goTm sc e <> goTy sc ty
    C.Let (C.NonRec v x) e ->
      "let" : binder TmSort sc v <> goTm sc x <> goTm (bindTm v sc) e
    C.Let (C.Rec bs) e ->
      let sc' = List.foldl' (flip bindTm) sc (map fst bs)
      in  "letrec" : concatMap (binder TmSort sc . fst) bs <> concatMap (goTm sc' . snd) bs <> goTm sc' e
    C.Case e _ alts -> "case" : goTm sc e <> concatMap (goAlt sc) alts
    C.Cast e _ _ -> "cast" : goTm sc e
    C.Tick t e -> "tick" : goTick sc t <> goTm sc e
    _ -> []

  goAlt sc (p, e) = case p of
    C.DataPat _ tvs ids ->
      let sc1 = List.foldl' (flip bindTy) sc tvs
          sc2 = List.foldl' (flip bindTm) sc1 ids
      in  ["existential pattern" | not (null tvs)] <>
          ["field type mentions an existential" | any (\i -> any (`elem` map C.varUniq tvs) (map C.varUniq (Lens.toListOf C.typeFreeVars (C.varType i)))) ids] <>
          concatMap (binder TySort sc) tvs <> concatMap (binder TmSort sc1) ids <> goTm sc2 e
    C.LitPat _ -> "literal pattern" : goTm sc e
    C.DefaultPat -> "default pattern" : goTm sc e

  goTick sc = \case
    C.Attributes _ e -> goTm sc e
    _ -> []

-- * Clash Core

-- | Whether a variable that a Clash pattern binds occurs free in a term
patBindersFreeIn :: C.Pat -> C.Term -> Bool
patBindersFreeIn pat e = case pat of
  C.DataPat _ tvs ids ->
    any ((`elem` map C.varUniq (Lens.toListOf C.termFreeTyVars e)) . C.varUniq) tvs ||
      any ((`elem` map C.varUniq (Lens.toListOf C.freeLocalIds e)) . C.varUniq) ids
  _ -> False

-- * Shrinking

-- | Whether a term is well-scoped in the scope of the free variables, as
-- generated terms are (see the module header): every local variable occurrence
-- refers to a variable in scope that isn't hidden (see 'bindTyVar'), and has
-- the type or kind of that variable; recursive let-bindings and patterns bind
-- different variables; and patterns bind as many variables as their data
-- constructor has.
wellScopedIn :: FreeVars -> C.Term -> Bool
wellScopedIn fvs = goTm (ctxOf fvs)
 where
  inScope :: C.Var a -> [C.Var a] -> Bool
  inScope v = any (\v' -> C.varUniq v' == C.varUniq v && C.varType v' `C.eqType` C.varType v)

  distinct :: [C.Var a] -> Bool
  distinct vs = let us = map C.varUniq vs in length (List.nub us) == length us

  goTy ctx = \case
    C.VarTy tv -> inScope tv (ctxTyVars ctx)
    C.ForAllTy tv t -> goTy ctx (C.varType tv) && goTy (bindTyVar tv ctx) t
    C.AppTy t1 t2 -> goTy ctx t1 && goTy ctx t2
    C.AnnType _ t -> goTy ctx t
    C.ConstTy {} -> True
    C.LitTy {} -> True

  goTm ctx = \case
    C.Var v
      | C.isGlobalId v -> True
      | otherwise -> inScope v (ctxIds ctx)
    C.Lam v e -> goTy ctx (C.varType v) && goTm (bindId v ctx) e
    C.TyLam tv e -> goTy ctx (C.varType tv) && goTm (bindTyVar tv ctx) e
    C.App e1 e2 -> goTm ctx e1 && goTm ctx e2
    C.TyApp e ty -> goTm ctx e && goTy ctx ty
    C.Let (C.NonRec v x) e ->
      goTy ctx (C.varType v) && goTm ctx x && goTm (bindId v ctx) e
    C.Let (C.Rec bs) e ->
      let ctx' = List.foldl' (flip bindId) ctx (map fst bs)
      in  distinct (map fst bs) && all (goTy ctx . C.varType . fst) bs &&
            all (goTm ctx' . snd) bs && goTm ctx' e
    C.Case e ty alts -> goTm ctx e && goTy ctx ty && all (goAlt ctx) alts
    C.Cast e t1 t2 -> goTm ctx e && goTy ctx t1 && goTy ctx t2
    C.Tick t e -> goTick ctx t && goTm ctx e
    C.Data {} -> True
    C.Literal {} -> True
    C.Prim {} -> True

  goAlt ctx (pat, e) = case pat of
    C.DataPat dc tvs ids ->
      length tvs == length (C.dcExtTyVars dc) && length ids == length (C.dcArgTys dc) &&
        distinct tvs && distinct ids &&
        goTvs ctx tvs (\ctx1 ->
          all (goTy ctx1 . C.varType) ids &&
            goTm (List.foldl' (flip bindId) ctx1 ids) e)
    _ -> goTm ctx e

  goTvs ctx [] k = k ctx
  goTvs ctx (tv:tvs) k = goTy ctx (C.varType tv) && goTvs (bindTyVar tv ctx) tvs k

  goTick ctx = \case
    C.NameMod _ ty -> goTy ctx ty
    C.Attributes ty e -> goTy ctx ty && goTm ctx e
    _ -> True

-- | Shrink a term, keeping it well-scoped in the scope of the free variables
-- (see 'wellScopedIn'): replace it by a literal or one of its subterms, drop
-- case alternatives, simplify types, or shrink a subterm.
shrinkTerm :: FreeVars -> C.Term -> [C.Term]
shrinkTerm fvs = filter (wellScopedIn fvs) . shrinkRaw

-- | Shrink a term without regard for scoping. Subterms are shrunk in place,
-- under the same binders, but may then mention variables that are no longer
-- bound: 'shrinkTerm' filters the results.
shrinkRaw :: C.Term -> [C.Term]
shrinkRaw tm = [lit | not (isLit tm)] <> children <> inner
 where
  lit = C.Literal (C.IntLiteral 0)
  isLit = \case
    C.Literal {} -> True
    _ -> False

  children = case tm of
    C.Lam _ e -> [e]
    C.TyLam _ e -> [e]
    C.App e1 e2 -> [e1, e2]
    C.TyApp e _ -> [e]
    C.Let (C.NonRec _ x) e -> [x, e]
    C.Let (C.Rec bs) e -> e : map snd bs
    C.Case e _ alts -> e : map snd alts
    C.Cast e _ _ -> [e]
    C.Tick _ e -> [e]
    _ -> []

  inner = case tm of
    C.Lam v e -> C.Lam v <$> shrinkRaw e
    C.TyLam v e -> C.TyLam v <$> shrinkRaw e
    C.App e1 e2 -> [C.App e1' e2 | e1' <- shrinkRaw e1] <> [C.App e1 e2' | e2' <- shrinkRaw e2]
    C.TyApp e ty -> [C.TyApp e intTy | not (ty `C.eqType` intTy)] <> [C.TyApp e' ty | e' <- shrinkRaw e]
    C.Let (C.NonRec v x) e ->
      [C.Let (C.NonRec v x') e | x' <- shrinkRaw x] <> [C.Let (C.NonRec v x) e' | e' <- shrinkRaw e]
    C.Let (C.Rec bs) e ->
      [C.Let (C.Rec bs) e' | e' <- shrinkRaw e] <>
      [ C.Let (C.Rec (pre <> [(v, x')] <> post)) e
      | (pre, (v, x) : post) <- splits bs, x' <- shrinkRaw x ]
    C.Case e ty alts ->
      [C.Case e intTy alts | not (ty `C.eqType` intTy)] <>
      [C.Case e ty (pre <> post) | (pre, _ : post) <- splits alts, not (null (pre <> post))] <>
      [C.Case e' ty alts | e' <- shrinkRaw e] <>
      [ C.Case e ty (pre <> [(p, a')] <> post)
      | (pre, (p, a) : post) <- splits alts, a' <- shrinkRaw a ]
    C.Cast e t1 t2 -> [C.Cast e' t1 t2 | e' <- shrinkRaw e]
    C.Tick t e -> C.Tick t <$> shrinkRaw e
    _ -> []

-- | All ways to split a list
splits :: [a] -> [([a], [a])]
splits xs = [splitAt i xs | i <- [0 .. length xs - 1]]

-- * Showing

prettyOpts :: PrettyOptions
prettyOpts = PrettyOptions
  { displayUniques = True
  , displayTypes = True
  , displayQualifiers = False
  , displayTicks = True
  }

-- | Pretty print a term, with uniques and types
showTerm :: C.Term -> String
showTerm = showP

-- | Pretty print a type, with uniques
showType :: C.Type -> String
showType = showP

showP :: PrettyPrec p => p -> String
showP = showPpr' prettyOpts

-- | Pretty print free variables, with their types
showFreeVars :: FreeVars -> String
showFreeVars (FreeVars tvs ids) =
  "free type variables: " <> List.intercalate ", " (map showVar tvs) <>
  "\nfree term variables: " <> List.intercalate ", " (map showVar ids)
 where
  showVar :: C.Var a -> String
  showVar v = Text.unpack (C.nameOcc (C.varName v)) <> "[" <> show (C.varUniq v) <> "] :: "
    <> showP (C.varType v)
