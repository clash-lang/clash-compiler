{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reducing primitives of the scoped machine ("Clash.Core.Scoped.Evaluator")
  with the evaluator of Clash Core. And the conversion of open scoped terms to
  Clash Core and back that it needs.

  Every primitive reduction of the evaluator of Clash Core is ported to
  scoped Core ("Clash.Core.Scoped.Evaluator.Primitives"), so the scoped
  evaluator doesn't use the adapter. The tests of the ported reductions
  compare them with the reductions of Clash Core through it
  ("Test.Clash.Scoped.Evaluator", @clash-differential prims@ and
  @clash-differential whnf --check-prims@).

  = The adapter

  'adaptPrim' converts the arguments of a primitive to values of Clash Core,
  runs the reduction of Clash Core ('Clash.Core.Evaluator.Types.primStep') in
  a machine of Clash Core, and converts its result back. A reduction of Clash
  Core can:

  * Inspect the arguments, and build a result from them. Their free
    variables are variables of the machine's scope, which keep their raw names
    as uniques, and the result is converted back with the same variables.
  * Use the primitive heap, which is shared.
  * Evaluate an argument with a nested machine, which looks up variables in
    the local heap. So the adapter converts the bindings of the local heap
    that the arguments can reach, transitively, for every reduction: it can't
    tell which reductions look at the heap. Most reductions get arguments
    without free variables, for which that is no work. Evaluated globals are
    not converted, but re-evaluated from their global bindings if needed; the
    set of globals under evaluation is.
  * Allocate heap bindings: those that the result refers to become bindings
    the scoped machine allocated, with fresh binders. They are converted back
    together with the result, as one @letrec@.

  Updates of existing heap bindings are not converted back: that only loses
  sharing.

  The in-scope set of the machine of Clash Core has all variables of the
  converted terms, and its unique supply hands out uniques that are made
  unique in it, so its new binders don't capture them.

  = Converting open terms

  'toClashTermWith' converts a scoped term in scope @n@ to Clash Core, like
  'Clash.Core.Scoped.Convert.toClashTerm': the uniques of the Clash variables
  are the raw names. It also records the free variables of the term in a
  'ClashVars', with their scoped variables, moved into scope @n@ out of the
  scope of the binders around their occurrence ('unsinkTerm'). With these,
  'Clash.Core.Scoped.Convert.fromClashTermWith' converts a Clash Core term
  that only mentions those free variables back into scope @n@.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Clash.Scoped.Evaluator.Adapter
  ( -- * The adapter
    adaptPrim
    -- * Converting open terms
  , ClashVars
  , emptyClashVars
  , clashVarsTyVars
  , clashVarsIds
  , toClashTermWith
  , toClashTypeWith
  , toClashValueWith
  ) where

import Control.Monad.State.Strict (State, get, modify', runState)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import GHC.Stack (HasCallStack)
import System.IO.Unsafe (unsafePerformIO)

import qualified Control.Lens as Lens
import qualified Clash.Core.Evaluator.Types as C
import qualified Clash.Core.FreeVars as C (freeLocalIds)
import Clash.Core.HasType (inferCoreTypeOf)
import Clash.Core.Literal (Literal (CharLiteral))
import Clash.Core.TyCon (TyConMap)
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import Clash.Core.VarEnv
  (emptyInScopeSet, emptyVarEnv, extendInScopeSetList, mkVarSet)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Unique (Unique)
import Clash.Util (curLoc)
import qualified Clash.Util.Supply as Supply

import Clash.Core.Scoped.Convert (fromClashTermWith)
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TypeEnv (typeEnvTyConMap)

-- * The adapter

-- | Reduce a primitive with the reduction of Clash Core of the given
-- evaluator, see the module header
adaptPrim :: C.Evaluator -> PrimReduction
adaptPrim eval = PrimReduction $ \pc ->
  let m = pcMachine pc
      sc = mScope m
      tcm = typeEnvTyConMap (envTypeEnv (pcEnv pc))
      ((tys, args, heap), vars) = flip runState emptyClashVars $ do
        tys0 <- mapM toClashTypeWith (pcTys pc)
        args0 <- mapM toClashValueWith (pcArgs pc)
        heap0 <- reachableHeap (mHeapLocal m)
        pure (tys0, args0, heap0)
      tvs = clashVarsTyVars vars
      ids = clashVarsIds vars
      cm = C.Machine
        { C.mHeapPrim = mHeapPrim m
        , C.mHeapGlobalBase = mHeapGlobalBase m
        , C.mHeapGlobalOverlay = emptyVarEnv
        , C.mHeapGlobalDeleted =
            mkVarSet (map globalIdVar (globalSetToList (mHeapGlobalDeleted m)))
        , C.mHeapLocal = UniqMap.fromList heap
        , C.mStack = []
        , C.mSupply = adapterSupply
        , C.mScopeNames =
            extendInScopeSetList (extendInScopeSetList emptyInScopeSet (map fst tvs)) (map fst ids)
        , C.mTerm = C.Literal (CharLiteral '_')
        }
  in case C.primStep eval tcm (pcIsSubj pc) (pcInfo pc) tys args cm of
       Nothing -> Nothing
       Just cm'
         | not (null (C.mStack cm')) ->
             error ($(curLoc) ++ "the reduction left frames on the stack")
         | otherwise ->
             let reached = IntSet.fromList (map (fromIntegral . fst) heap)
                 new = [ (u, rhs) | (u, rhs) <- UniqMap.toList (C.mHeapLocal cm')
                                  , not (IntSet.member (fromIntegral u) reached) ]
                 m1 = m {mHeapPrim = C.mHeapPrim cm'}
             in Just (bindNew tcm sc tvs ids new (C.mTerm cm') m1)

-- | A unique supply for the machine of Clash Core. Every reduction starts
-- with the same one: the uniques it hands out are made unique in the in-scope
-- set of the machine, and converted back to fresh scoped binders.
adapterSupply :: Supply.Supply
adapterSupply = unsafePerformIO Supply.newSupply
{-# NOINLINE adapterSupply #-}

-- | The bindings of the local heap that the variables converted so far can
-- reach, converted, by raw name
reachableHeap :: Distinct n => PureHeap n -> State (ClashVars n) [(Unique, C.Term)]
reachableHeap heap = go IntSet.empty []
 where
  go done acc = do
    vars <- get
    case [ (r, t) | (r, (_, t)) <- IntMap.toList (cvIds vars)
                  , not (IntSet.member r done) ] of
      [] -> pure acc
      todo -> do
        new <- mapM convert [ (r, rhs) | (r, Var _ _ x) <- todo
                                       , Just rhs <- [lookupNameMap x heap] ]
        go (IntSet.union done (IntSet.fromList (map fst todo))) (new ++ acc)

  convert (r, rhs) = (,) (fromIntegral r) <$> toClashTermWith rhs

-- | Continue the scoped machine with the result of the reduction of Clash
-- Core, binding the heap bindings it allocated that the result refers to.
-- They are converted together with the result, as one @letrec@, whose
-- binders the scoped machine allocates.
bindNew
  :: Distinct n
  => TyConMap
  -> Scope n
  -> [(C.TyVar, Type n)]
  -> [(C.Id, Term n)]
  -> [(Unique, C.Term)]
  -- ^ The new heap bindings
  -> C.Term
  -- ^ The result
  -> Machine o n
  -> Next o n
bindNew tcm sc tvs ids new res m = case usedNew of
  [] -> Next m {mTerm = fromClashTermWith sc tvs ids res}
  _ -> case fromClashTermWith sc tvs ids (C.Letrec bindings res) of
    Let bind body -> openBind sc bind body $ \sc1 bind1 body1 -> case bind1 of
      Rec bs rhss ->
        let m1 = extendMachineTele sc1 bs m
            heap = List.foldl' (\h (v, rhs) -> insertNameMap (varName v) rhs h)
                     (mHeapLocal m1) (zip (teleIdVars bs) rhss)
        in Next m1 {mHeapLocal = heap, mTerm = body1}
      NonRec {} -> error ($(curLoc) ++ "expected a recursive let-binding")
    _ -> error ($(curLoc) ++ "expected a let-binding")
 where
  newEnv = IntMap.fromList [ (fromIntegral u, rhs) | (u, rhs) <- new ]
  -- The new bindings the result refers to, transitively
  usedNew
    | null new = []
    | otherwise = go IntSet.empty (freeIds res)
   where
    go seen [] = [ (u, rhs) | u <- IntSet.toList seen, Just rhs <- [IntMap.lookup u newEnv] ]
    go seen (u : us)
      | IntSet.member u seen = go seen us
      | Just rhs <- IntMap.lookup u newEnv = go (IntSet.insert u seen) (freeIds rhs ++ us)
      | otherwise = go seen us
  freeIds t = [ fromIntegral (C.varUniq v) | v <- Lens.toListOf C.freeLocalIds t ]
  bindings =
    [ (C.mkLocalId (inferCoreTypeOf tcm rhs) (C.mkUnsafeSystemName "x" (fromIntegral u)), rhs)
    | (u, rhs) <- usedNew ]
  varName :: Term x -> TmName x
  varName = \case
    Var _ _ x -> x
    _ -> error ($(curLoc) ++ "not a variable")

-- * Converting open terms

-- | The free variables of scoped values in scope @n@ that were converted to
-- Clash Core, by raw name, with their Clash variables and scoped variables
data ClashVars (n :: S) = ClashVars
  { cvTyVars :: !(IntMap (C.TyVar, Type n))
  , cvIds :: !(IntMap (C.Id, Term n))
  }

-- | No variables
emptyClashVars :: ClashVars n
emptyClashVars = ClashVars IntMap.empty IntMap.empty

-- | The type variables
clashVarsTyVars :: ClashVars n -> [(C.TyVar, Type n)]
clashVarsTyVars = IntMap.elems . cvTyVars

-- | The term variables
clashVarsIds :: ClashVars n -> [(C.Id, Term n)]
clashVarsIds = IntMap.elems . cvIds

-- | The binders between scope @n@ and @x@, with their Clash variables by raw
-- name
data UEnv (n :: S) (x :: S) = UEnv !(NameBinders n x) !(IntMap C.TyVar) !(IntMap C.Id)

emptyUEnv :: UEnv n n
emptyUEnv = UEnv emptyNameBinders IntMap.empty IntMap.empty

-- | Convert a term to Clash Core, recording its free variables, see the
-- module header
toClashTermWith :: HasCallStack => Term n -> State (ClashVars n) C.Term
toClashTermWith = goTm emptyUEnv

-- | Convert a type to Clash Core, recording its free type variables
toClashTypeWith :: HasCallStack => Type n -> State (ClashVars n) C.Type
toClashTypeWith = goTy emptyUEnv

-- | Convert a value to Clash Core, recording its free variables
toClashValueWith :: HasCallStack => Value n -> State (ClashVars n) C.Value
toClashValueWith = \case
  Lambda b e -> toClashTermWith (Lam b e) >>= \case
    C.Lam x e' -> pure (C.Lambda x e')
    _ -> error ($(curLoc) ++ "not a lambda")
  TyLambda b e -> toClashTermWith (TyLam b e) >>= \case
    C.TyLam x e' -> pure (C.TyLambda x e')
    _ -> error ($(curLoc) ++ "not a type lambda")
  DC dc args ->
    C.DC dc <$> mapM (either (fmap Left . toClashTermWith) (fmap Right . toClashTypeWith)) args
  Lit l -> pure (C.Lit l)
  PrimVal p tys vs -> C.PrimVal p <$> mapM toClashTypeWith tys <*> mapM toClashValueWith vs
  Suspend e -> C.Suspend <$> toClashTermWith e
  TickValue t v -> C.TickValue <$> goTick emptyUEnv t <*> toClashValueWith v
  CastValue v t1 t2 ->
    C.CastValue <$> toClashValueWith v <*> toClashTypeWith t1 <*> toClashTypeWith t2

setUniq :: C.Name a -> Int -> C.Name a
setUniq nm r = nm {C.nameUniq = fromIntegral r}

goTy :: HasCallStack => UEnv n x -> Type x -> State (ClashVars n) C.Type
goTy env@(UEnv bs tvs _) = \case
  VarTy occ k a -> case IntMap.lookup (tyNameRaw a) tvs of
    Just tv -> pure (C.VarTy tv)
    Nothing -> C.VarTy <$> freeTyVar bs occ k a
  ConstTy c -> pure (C.ConstTy c)
  ForAllTy b t -> tyBinder env b $ \env' tv -> C.ForAllTy tv <$> goTy env' t
  AppTy t1 t2 -> C.AppTy <$> goTy env t1 <*> goTy env t2
  LitTy l -> pure (C.LitTy l)
  AnnType a t -> C.AnnType a <$> goTy env t

-- | The Clash variable of a free type variable, which is recorded if it
-- wasn't yet
freeTyVar
  :: HasCallStack
  => NameBinders n x -> C.Name C.Type -> Kind x -> TyName x -> State (ClashVars n) C.TyVar
freeTyVar bs occ k a = do
  vars <- get
  case IntMap.lookup r (cvTyVars vars) of
    Just (tv, _) -> pure tv
    Nothing -> case unsinkType bs (VarTy occ k a) of
      Just ty@(VarTy _ kN _) -> do
        k' <- goTy emptyUEnv kN
        let tv = C.TyVar (setUniq occ r) (fromIntegral r) k'
        modify' (\vs -> vs {cvTyVars = IntMap.insert r (tv, ty) (cvTyVars vs)})
        pure tv
      _ -> error ($(curLoc) ++ "the kind of free type variable " ++ show occ
                  ++ " mentions a variable bound around it")
 where
  r = tyNameRaw a

-- | The Clash variable of a free term variable, which is recorded if it
-- wasn't yet
freeId
  :: HasCallStack
  => NameBinders n x -> C.Name C.Term -> Type x -> TmName x -> State (ClashVars n) C.Id
freeId bs occ ty x = do
  vars <- get
  case IntMap.lookup r (cvIds vars) of
    Just (i, _) -> pure i
    Nothing -> case unsinkTerm bs (Var occ ty x) of
      Just tm@(Var _ tyN _) -> do
        ty' <- goTy emptyUEnv tyN
        let i = C.Id (setUniq occ r) (fromIntegral r) ty' C.LocalId
        modify' (\vs -> vs {cvIds = IntMap.insert r (i, tm) (cvIds vs)})
        pure i
      _ -> error ($(curLoc) ++ "the type of free variable " ++ show occ
                  ++ " mentions a variable bound around it")
 where
  r = tmNameRaw x

tyBinder
  :: HasCallStack
  => UEnv n x -> TyBinder x y -> (UEnv n y -> C.TyVar -> State (ClashVars n) r)
  -> State (ClashVars n) r
tyBinder env@(UEnv bs tvs ids) b k = do
  kind <- goTy env (tyBinderKind b)
  let r = tyNameRaw (tyNameOf b)
      tv = C.TyVar (setUniq (tyBinderOcc b) r) (fromIntegral r) kind
  k (UEnv (appendNameBinders bs (tyBinderNameBinders b)) (IntMap.insert r tv tvs) ids) tv

idBinder
  :: HasCallStack
  => UEnv n x -> IdBinder x y -> (UEnv n y -> C.Id -> State (ClashVars n) r)
  -> State (ClashVars n) r
idBinder env@(UEnv bs tvs ids) b k = do
  ty <- goTy env (idBinderType b)
  let r = tmNameRaw (tmNameOf b)
      i = C.Id (setUniq (idBinderOcc b) r) (fromIntegral r) ty C.LocalId
  k (UEnv (appendNameBinders bs (idBinderNameBinders b)) tvs (IntMap.insert r i ids)) i

tele
  :: (forall x' y' r'. UEnv n x' -> b x' y' -> (UEnv n y' -> v -> State (ClashVars n) r') -> State (ClashVars n) r')
  -> UEnv n x -> Tele b x y -> (UEnv n y -> [v] -> State (ClashVars n) r)
  -> State (ClashVars n) r
tele _ env TNil k = k env []
tele f env (TCons b bs) k = f env b $ \env1 v -> tele f env1 bs $ \env2 vs -> k env2 (v : vs)

goTm :: HasCallStack => UEnv n x -> Term x -> State (ClashVars n) C.Term
goTm env@(UEnv bs _ ids) = \case
  Var occ ty x -> case IntMap.lookup (tmNameRaw x) ids of
    Just i -> pure (C.Var i)
    Nothing -> C.Var <$> freeId bs occ ty x
  GlobalVar g -> pure (C.Var (globalIdVar g))
  Data dc -> pure (C.Data dc)
  Literal l -> pure (C.Literal l)
  Prim p -> pure (C.Prim p)
  Lam b e -> idBinder env b $ \env' i -> C.Lam i <$> goTm env' e
  TyLam b e -> tyBinder env b $ \env' tv -> C.TyLam tv <$> goTm env' e
  App e1 e2 -> C.App <$> goTm env e1 <*> goTm env e2
  TyApp e ty -> C.TyApp <$> goTm env e <*> goTy env ty
  Let (NonRec b x) e -> do
    x' <- goTm env x
    idBinder env b $ \env' i -> C.Let (C.NonRec i x') <$> goTm env' e
  Let (Rec bndrs xs) e ->
    tele idBinder env bndrs $ \env' is -> do
      xs' <- mapM (goTm env') xs
      e' <- goTm env' e
      pure (C.Let (C.Rec (zip is xs')) e')
  Case e ty alts -> C.Case <$> goTm env e <*> goTy env ty <*> mapM (goAlt env) alts
  Cast e t1 t2 -> C.Cast <$> goTm env e <*> goTy env t1 <*> goTy env t2
  Tick t e -> C.Tick <$> goTick env t <*> goTm env e

goAlt :: HasCallStack => UEnv n x -> Alt x -> State (ClashVars n) C.Alt
goAlt env (Alt p e) = case p of
  DataPat dc tvs ids ->
    tele tyBinder env tvs $ \env1 tvs' ->
      tele idBinder env1 ids $ \env2 ids' ->
        (,) (C.DataPat dc tvs' ids') <$> goTm env2 e
  LitPat l -> (,) (C.LitPat l) <$> goTm env e
  DefaultPat -> (,) C.DefaultPat <$> goTm env e

goTick :: HasCallStack => UEnv n x -> TickInfo x -> State (ClashVars n) C.TickInfo
goTick env = \case
  SrcSpan sp -> pure (C.SrcSpan sp)
  NameMod m ty -> C.NameMod m <$> goTy env ty
  DeDup -> pure C.DeDup
  NoDeDup -> pure C.NoDeDup
  Attributes ty e -> C.Attributes <$> goTy env ty <*> goTm env e
