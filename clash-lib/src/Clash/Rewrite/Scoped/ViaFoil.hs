{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Adapters between transformations of Clash Core and of scoped Clash Core,
  for the duration of the port:

  * 'viaFoil' runs a scoped transformation as a transformation of Clash Core,
    so that transformations can be ported one at a time. Phase 4 of the port
    deletes it, when the normalizer runs on scoped Core.

  * 'whnfRW' evaluates a scoped term with the evaluator of Clash Core. Phase 6
    replaces it with the ported evaluator.

  = 'viaFoil'

  'viaFoil' converts the subterm it is offered into scoped Core, in a scope
  with its free variables, runs the scoped transformation in a context whose
  root is a 'LegacyRoot', and converts the result back if the transformation
  signaled a change. Otherwise, it returns the original subterm, so that
  unchanged terms keep their sharing.

  The root scope also has the free variables of the let-bindings of the
  context that the evaluator may use for the subterm (see 'rootVars'), so
  that the result of 'whnfRW' is in scope.

  __Uniques.__ Converting from scoped Core uses raw names as uniques, and raw
  names of binders are just above those in scope: small numbers, like the ones
  the unique supply hands out. A converted binder could then share its unique
  with a variable that the supply hands out later, e.g. a new global binder,
  whose freshness is only checked against the global bindings. So the result
  of 'viaFoil' gives every binder a unique from the unique supply, made unique
  in the 'Clash.Core.VarEnv.InScopeSet' of the context, like
  'Clash.Rewrite.Util.mkTmBinderFor' does. Its free variables are the
  variables of the original subterm and its context.

  = 'whnfRW'

  'whnfRW' converts the subterm and the frames of its context to Clash Core,
  on top of the Clash Core context of the 'LegacyRoot', and evaluates it with
  'Clash.Rewrite.Util.whnfHeap'. The evaluator uses the let-bindings of the
  context that are not shadowed on the path to the subterm, so this includes
  the scoped 'LetBody' frames as well as the Clash Core ones. Then it converts
  the result back into the scope of the subterm, binds the bindings the
  evaluator allocated around it, and runs the continuation on it, like
  'Clash.Rewrite.Util.bindPureHeap'.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Rewrite.Scoped.ViaFoil
  ( viaFoil
  , whnfRW
    -- * Internals, exported for testing
  , rootVars
  , fromClashTermIn
  ) where

import           Control.Lens                ((.=))
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.State.Strict  (State, evalState, state)
import qualified Control.Monad.Writer        as Writer
import           Data.Functor.Compose        (Compose (..))
import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntMap.Strict          as IntMap
import qualified Data.List                   as List
import qualified Data.Monoid                 as Monoid
import           GHC.Stack                   (HasCallStack)

import qualified Clash.Core.FreeVars         as C
import qualified Clash.Core.Name             as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Var              as C
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, eltsVarEnv, elemVarEnv, emptyInScopeSet,
   extendInScopeSet, extendInScopeSetList, lookupVarEnv, mkVarEnv, uniqAway)
import           Clash.Driver.Types          (DebugOpts (..))
import           Clash.Rewrite.Types
  (Rewrite, RewriteMonad, debugOpts, tcCache, uniqSupply)
import qualified Clash.Rewrite.Types         as C
import           Clash.Rewrite.Util
  (cleanupHeapBindings, heapLetBindings, whnfHeap)
import           Clash.Unique                (Unique)
import           Clash.Util                  (curLoc)
import           Clash.Util.Supply           (Supply, freshId, splitSupply)

import           Clash.Core.Scoped.Alpha     (aeqTm)
import           Clash.Core.Scoped.Convert
import           Clash.Core.Scoped.FreeVars  (bindNameBinders)
import           Clash.Core.Scoped.Subst
import           Clash.Core.Scoped.Syntax
import           Clash.Rewrite.Scoped.Types

-- * viaFoil

-- | Run a scoped transformation as a transformation of Clash Core, see the
-- module header. Uninstrumented: wrap it in 'Clash.Rewrite.Util.apply' (the
-- strategy compiler does so) to count it and check its result.
viaFoil
  :: HasCallStack
  => String
  -- ^ Name of the transformation, for error messages
  -> ScopedRewrite extra
  -> Rewrite extra
viaFoil name (ScopedTransform rw) ctx e =
  withConvEnv tvs ids $ \env ->
    let root = LegacyRoot
          { legacyContext = ctx
          , legacyTyVars = [(tv, fromClashType env (C.VarTy tv)) | tv <- tvs]
          , legacyIds = [(i, fromClashTerm env (C.Var i)) | i <- ids]
          }
        e0 = fromClashTerm env e
    in do
      (e1, Monoid.getAny -> changed) <-
        Writer.listen (rw (TransformContext (convScope env) (PLegacy root)) e0)
      if changed
        then withSupply (toClashFresh (rootInScope root) (rootBackEnv root) e1)
        else do
          opts <- Lens.view debugOpts
          Monad.when (dbg_invariants opts && not (aeqTm e0 e1)) $
            error ($(curLoc) ++ "Expression changed without notice ("
                   ++ name ++ "), under viaFoil")
          pure e
 where
  (tvs, ids) = rootVars (C.tfContext ctx) e

-- | The variables of the root scope of 'viaFoil': the free variables of the
-- subterm, and those of the right-hand sides of the let-bindings of its
-- context ('Clash.Core.Term.LetBody' frames) that the evaluator may use for
-- it, transitively. These are the bindings of variables in the set so far.
-- Type variables are ordered such that their kinds only mention the ones
-- before them.
--
-- This overapproximates the bindings the evaluator uses: e.g. a binding of a
-- variable shadowed between it and the subterm is unusable (see
-- 'Clash.Rewrite.Util.whnfHeap'). The variables of its right-hand side are
-- then added needlessly. That is harmless, as long as a variable of the
-- subterm wins from one of the context with the same unique, which is what
-- the 'Clash.Core.VarEnv.VarEnv's do: the subterm comes first.
rootVars :: C.Context -> C.Term -> ([C.TyVar], [C.Id])
rootVars frames e =
  (C.dependencySortTyVars (eltsVarEnv tvsN), eltsVarEnv idsN)
 where
  (tvs0, ids0) = freeVarsOf e
  (tvsN, idsN) = List.foldl' frame (tvs0, ids0) frames

  frame acc@(_, ids) = \case
    C.LetBody bs | any ((`elemVarEnv` ids) . fst) bs -> close bs acc
    _ -> acc

  -- Add the free variables of the right-hand sides of the needed bindings,
  -- which may make more bindings of the same frame needed
  close bs acc0@(_, ids0') = go acc0 (filter (`elemVarEnv` bsEnv) (eltsVarEnv ids0'))
   where
    bsEnv = mkVarEnv [(x, rhs) | (x, rhs) <- bs]
    -- Every variable enters the work list at most once: when it is added to
    -- the set.
    go acc [] = acc
    go acc@(tvs, ids) (x:xs) = case lookupVarEnv x bsEnv of
      Nothing -> go acc xs
      Just rhs ->
        let (tvsR, idsR) = freeVarsOf rhs
            new = [i | i <- eltsVarEnv idsR, not (i `elemVarEnv` ids)]
        in go (tvs <> tvsR, ids <> idsR) (filter (`elemVarEnv` bsEnv) new ++ xs)

  freeVarsOf :: C.Term -> (VarEnv C.TyVar, VarEnv C.Id)
  freeVarsOf t =
    ( mkVarEnv [(tv, tv) | tv <- Lens.toListOf C.termFreeTyVars t]
    , mkVarEnv [(i, i) | i <- Lens.toListOf C.freeLocalIds t] )

-- * Converting back

-- | Clash variables of the names of a scope, by raw name
data BackEnv = BackEnv !(IntMap C.TyVar) !(IntMap C.Id)

-- | The Clash variables of the root scope of 'viaFoil'
rootBackEnv :: LegacyRoot n -> BackEnv
rootBackEnv root = BackEnv
  (IntMap.fromList [(tyRaw ty, tv) | (tv, ty) <- legacyTyVars root])
  (IntMap.fromList [(tmRaw tm, i) | (i, tm) <- legacyIds root])
 where
  tyRaw = \case
    VarTy _ _ n -> tyNameRaw n
    _ -> error "rootBackEnv: not a type variable"
  tmRaw = \case
    Var _ _ n -> tmNameRaw n
    _ -> error "rootBackEnv: not a variable"

-- | The in-scope set of the context of the root, extended with the variables
-- of the root scope, in case the context doesn't have all of them
rootInScope :: LegacyRoot n -> InScopeSet
rootInScope root =
  extendInScopeSetList
    (extendInScopeSetList (C.tfInScope (legacyContext root)) (map fst (legacyTyVars root)))
    (map fst (legacyIds root))

-- | Run a computation with a split of the unique supply
withSupply :: State Supply a -> RewriteMonad extra a
withSupply m = do
  sup <- Lens.use uniqSupply
  let (sup1, sup2) = splitSupply sup
  uniqSupply .= sup2
  pure (evalState m sup1)

-- | A Clash variable for a binder: one with a fresh unique from the supply,
-- made unique in the in-scope set, which is extended with it
freshVar :: InScopeSet -> C.Var a -> State Supply (InScopeSet, C.Var a)
freshVar is v = do
  u <- state freshId
  let v' = uniqAway is (setUniq v u)
  pure (extendInScopeSet is v', v')
 where
  setUniq :: C.Var a -> Unique -> C.Var a
  setUniq var u = var {C.varUniq = u, C.varName = (C.varName var) {C.nameUniq = u}}

freshIdBinder
  :: InScopeSet -> BackEnv -> IdBinder n l
  -> State Supply (InScopeSet, BackEnv, C.Id)
freshIdBinder is env@(BackEnv tvs ids) b = do
  ty <- toClashTypeFresh is env (idBinderType b)
  (is', v) <- freshVar is (C.mkLocalId ty (idBinderOcc b))
  pure (is', BackEnv tvs (IntMap.insert (tmNameRaw (tmNameOf b)) v ids), v)

freshTyBinder
  :: InScopeSet -> BackEnv -> TyBinder n l
  -> State Supply (InScopeSet, BackEnv, C.TyVar)
freshTyBinder is env@(BackEnv tvs ids) b = do
  k <- toClashTypeFresh is env (tyBinderKind b)
  (is', v) <- freshVar is (C.mkTyVar k (tyBinderOcc b))
  pure (is', BackEnv (IntMap.insert (tyNameRaw (tyNameOf b)) v tvs) ids, v)

freshTele
  :: (forall x y. InScopeSet -> BackEnv -> b x y -> State Supply (InScopeSet, BackEnv, v))
  -> InScopeSet -> BackEnv -> Tele b n l
  -> State Supply (InScopeSet, BackEnv, [v])
freshTele _ is env TNil = pure (is, env, [])
freshTele f is env (TCons b bs) = do
  (is1, env1, v) <- f is env b
  (is2, env2, vs) <- freshTele f is1 env1 bs
  pure (is2, env2, v : vs)

freshPat
  :: InScopeSet -> BackEnv -> Pat n l
  -> State Supply (InScopeSet, BackEnv, C.Pat)
freshPat is env = \case
  DataPat dc tvs ids -> do
    (is1, env1, tvs') <- freshTele freshTyBinder is env tvs
    (is2, env2, ids') <- freshTele freshIdBinder is1 env1 ids
    pure (is2, env2, C.DataPat dc tvs' ids')
  LitPat l -> pure (is, env, C.LitPat l)
  DefaultPat -> pure (is, env, C.DefaultPat)

-- | Convert a type to Clash Core, see 'toClashFresh'
toClashTypeFresh :: InScopeSet -> BackEnv -> Type n -> State Supply C.Type
toClashTypeFresh is env@(BackEnv tvs _) = \case
  VarTy occ _ n -> case IntMap.lookup (tyNameRaw n) tvs of
    Just tv -> pure (C.VarTy tv)
    Nothing -> error ($(curLoc) ++ "type variable not in scope: " ++ show occ)
  ConstTy c -> pure (C.ConstTy c)
  ForAllTy b t -> do
    (is', env', tv) <- freshTyBinder is env b
    C.ForAllTy tv <$> toClashTypeFresh is' env' t
  AppTy t1 t2 -> C.AppTy <$> toClashTypeFresh is env t1 <*> toClashTypeFresh is env t2
  LitTy l -> pure (C.LitTy l)
  AnnType a t -> C.AnnType a <$> toClashTypeFresh is env t

-- | Convert a term to Clash Core, giving every binder a fresh unique, see the
-- module header. The free variables of the term must be in the environment.
toClashFresh :: HasCallStack => InScopeSet -> BackEnv -> Term n -> State Supply C.Term
toClashFresh is env@(BackEnv _ ids) = \case
  Var occ _ n -> case IntMap.lookup (tmNameRaw n) ids of
    Just v -> pure (C.Var v)
    Nothing -> error ($(curLoc) ++ "variable not in scope: " ++ show occ)
  GlobalVar g -> pure (C.Var (globalIdVar g))
  Data dc -> pure (C.Data dc)
  Literal l -> pure (C.Literal l)
  Prim p -> pure (C.Prim p)
  Lam b e -> do
    (is', env', v) <- freshIdBinder is env b
    C.Lam v <$> toClashFresh is' env' e
  TyLam b e -> do
    (is', env', tv) <- freshTyBinder is env b
    C.TyLam tv <$> toClashFresh is' env' e
  App e1 e2 -> C.App <$> go e1 <*> go e2
  TyApp e ty -> C.TyApp <$> go e <*> goTy ty
  Let (NonRec b x) e -> do
    x' <- go x
    (is', env', v) <- freshIdBinder is env b
    C.Let (C.NonRec v x') <$> toClashFresh is' env' e
  Let (Rec bs xs) e -> do
    (is', env', vs) <- freshTele freshIdBinder is env bs
    xs' <- mapM (toClashFresh is' env') xs
    e' <- toClashFresh is' env' e
    pure (C.Let (C.Rec (zip vs xs')) e')
  Case e ty alts -> C.Case <$> go e <*> goTy ty <*> mapM goAlt alts
  Cast e ty1 ty2 -> C.Cast <$> go e <*> goTy ty1 <*> goTy ty2
  Tick t e -> C.Tick <$> toClashTickFresh is env t <*> go e
 where
  go :: Term x -> State Supply C.Term
  go = toClashFresh is env

  goTy :: Type x -> State Supply C.Type
  goTy = toClashTypeFresh is env

  goAlt :: Alt x -> State Supply C.Alt
  goAlt (Alt p e) = do
    (is', env', p') <- freshPat is env p
    (p',) <$> toClashFresh is' env' e

toClashTickFresh :: InScopeSet -> BackEnv -> TickInfo n -> State Supply C.TickInfo
toClashTickFresh is env = \case
  SrcSpan s -> pure (C.SrcSpan s)
  NameMod m ty -> C.NameMod m <$> toClashTypeFresh is env ty
  DeDup -> pure C.DeDup
  NoDeDup -> pure C.NoDeDup
  Attributes ty e -> C.Attributes <$> toClashTypeFresh is env ty <*> toClashFresh is env e

-- * Converting into an existing scope

-- | Convert a Clash term into scope @n@, given the scoped counterparts of its
-- free variables. Errors if it has other free variables.
--
-- The term is abstracted over the free variables, converted as a closed term,
-- and then applied to the scoped variables by substitution.
fromClashTermIn
  :: forall n
   . (HasCallStack, Distinct n)
  => Scope n
  -> [(C.TyVar, Type n)]
  -- ^ Type variables in scope, ordered such that the kind of a type variable
  -- only mentions the ones before it
  -> [(C.Id, Term n)]
  -- ^ Term variables in scope
  -> C.Term
  -> Term n
fromClashTermIn sc tvs ids tm =
  peelTy (closedSubst sc) (fromClashTermClosed closed) tvs
 where
  closed = foldr (C.TyLam . fst) (foldr (C.Lam . fst) tm ids) tvs

  peelTy :: Subst i n -> Term i -> [(C.TyVar, Type n)] -> Term n
  peelTy s (TyLam b e) ((_, ty) : rest) = peelTy (extendTySubst s b ty) e rest
  peelTy s e [] = peelId s e ids
  peelTy _ _ _ = error ($(curLoc) ++ "expected a type lambda")

  peelId :: Subst i n -> Term i -> [(C.Id, Term n)] -> Term n
  peelId s (Lam b e) ((_, x) : rest) = peelId (extendTmSubst s b x) e rest
  peelId s e [] = substTm s e
  peelId _ _ _ = error ($(curLoc) ++ "expected a lambda")

-- * whnfRW

-- | A Clash Core view of the context of a subterm in scope @n@
data LegacyView n = LegacyView
  { lvEnv :: !BackEnv
  -- ^ Clash variables of the names of scope @n@
  , lvInScope :: !InScopeSet
  , lvFrames :: C.Context
  -- ^ Innermost first
  , lvTyVarsRev :: [(C.TyVar, Type n)]
  -- ^ The type variables of scope @n@, innermost first
  , lvIdsRev :: [(C.Id, Term n)]
  -- ^ The term variables of scope @n@, innermost first
  }

-- | Move a 'LegacyView' into an extended scope
sinkView :: DExt i n => LegacyView i -> LegacyView n
sinkView (LegacyView env is frames tvs ids) =
  LegacyView env is frames (getCompose (sink1 (Compose tvs))) (getCompose (sink1 (Compose ids)))

-- | The Clash Core view of the context of a subterm. Gives the frames of the
-- path fresh Clash variables, like 'toClashFresh'.
legacyView :: HasCallStack => TransformContext n -> State Supply (LegacyView n)
legacyView = go . tfPath
 where
  go :: Path x -> State Supply (LegacyView x)
  go = \case
    PRoot -> pure (LegacyView (BackEnv IntMap.empty IntMap.empty) emptyInScopeSet [] [] [])
    PLegacy root -> pure LegacyView
      { lvEnv = rootBackEnv root
      , lvInScope = rootInScope root
      , lvFrames = C.tfContext (legacyContext root)
      , lvTyVarsRev = reverse (legacyTyVars root)
      , lvIdsRev = reverse (legacyIds root)
      }
    PFrame f p -> do
      lv <- go p
      f' <- frame lv f
      pure lv {lvFrames = f' : lvFrames lv}
    PUnder u p -> do
      lv <- go p
      under lv u

  frame :: LegacyView x -> Frame x -> State Supply C.CoreContext
  frame lv@(LegacyView env is _ _ _) = \case
    AppFun -> pure C.AppFun
    AppArg a -> pure (C.AppArg a)
    TyAppC -> pure C.TyAppC
    LetBinding b -> do
      -- The binder is not in scope of the right-hand side
      (_, _, v) <- freshIdBinder (lvInScope lv) env b
      pure (C.LetBinding v [v])
    CaseScrut -> pure C.CaseScrut
    CastBody -> pure C.CastBody
    TickC t -> C.TickC <$> toClashTickFresh is env t

  under :: (Distinct i, DExt i y) => LegacyView i -> Under i y -> State Supply (LegacyView y)
  under lv@(LegacyView env is frames _ _) = \case
    LamBody b -> do
      (is', env', v) <- freshIdBinder is env b
      pure (extendIds env' is' (C.LamBody v : frames) [(v, idBinderVar b)] (sinkView lv))
    TyLamBody b -> do
      (is', env', tv) <- freshTyBinder is env b
      pure (extendTvs env' is' (C.TyLamBody tv : frames) [(tv, tyBinderVar b)] (sinkView lv))
    CaseAlt p -> do
      (is', env', p') <- freshPat is env p
      let (tys, tms) = patVars p
          (tvs', ids') = C.patIds p'
          lv1 = extendTvs env' is' (C.CaseAlt p' : frames) (zip tvs' tys) (sinkView lv)
      pure (extendIds env' is' (C.CaseAlt p' : frames) (zip ids' tms) lv1)
    LetBody (NonRec b x) -> do
      x' <- toClashFresh is env x
      (is', env', v) <- freshIdBinder is env b
      pure (extendIds env' is' (C.LetBody [(v, x')] : frames) [(v, idBinderVar b)] (sinkView lv))
    LetBody (Rec bs xs) -> do
      (is', env', vs) <- freshTele freshIdBinder is env bs
      xs' <- mapM (toClashFresh is' env') xs
      pure (extendIds env' is' (C.LetBody (zip vs xs') : frames) (zip vs (teleIdVars bs)) (sinkView lv))
    LetBindingRec bs i -> do
      (is', env', vs) <- freshTele freshIdBinder is env bs
      pure (extendIds env' is' (C.LetBinding (vs !! i) vs : frames) (zip vs (teleIdVars bs)) (sinkView lv))

  extendIds env is frames new lv =
    lv { lvEnv = env, lvInScope = is, lvFrames = frames
       , lvIdsRev = reverse new ++ lvIdsRev lv }

  extendTvs env is frames new lv =
    lv { lvEnv = env, lvInScope = is, lvFrames = frames
       , lvTyVarsRev = reverse new ++ lvTyVarsRev lv }

-- | Evaluate a term to weak-head normal form, and apply a transformation to
-- the result, in the context extended with the bindings the evaluator
-- allocated. See 'Clash.Rewrite.Util.whnfRW', and the module header.
--
-- If the transformation signals a change, the result is bound by those
-- bindings, of which the work-free ones are inlined, and the unused ones
-- removed, like 'Clash.Rewrite.Util.bindPureHeap' does. Otherwise the result
-- is the one of the transformation, which must not mention them: return
-- (the 'sink' of) the original term when nothing changes.
whnfRW
  :: (HasCallStack, Distinct n)
  => Bool
  -- ^ Whether the term is the subject of a case-expression
  -> TransformContext n
  -> Term n
  -> (forall l. DExt n l => TransformContext l -> Term l -> RewriteMonad extra (Term l))
  -> RewriteMonad extra (Term n)
whnfRW isSubj ctx e k = do
  (lv, e') <- withSupply $ do
    lv <- legacyView ctx
    e' <- toClashFresh (lvInScope lv) (lvEnv lv) e
    pure (lv, e')
  let ctxC = C.TransformContext (lvInScope lv) (lvFrames lv)
      sc = tfScope ctx
      tvs = reverse (lvTyVarsRev lv)
      ids = reverse (lvIdsRev lv)
      back = fromClashTermIn sc tvs ids
  (heap, v) <- whnfHeap isSubj ctxC e'
  tcm <- Lens.view tcCache
  case heapLetBindings tcm heap of
    [] -> k ctx (back v)
    bndrs -> case back (C.Letrec bndrs v) of
      Let bind body -> underLet ctx bind body $ \ctxL bind' body' -> do
        (r, Monoid.getAny -> changed) <- Writer.listen (k ctxL body')
        if changed
          then do
            -- Inline the work-free bindings and remove the unused ones, on
            -- Clash Core
            res <- withSupply (toClashFresh (lvInScope lv) (lvEnv lv) (Let bind' r))
            case res of
              C.Let (C.Rec bs) r' -> back <$> cleanupHeapBindings ctxC bs r'
              _ -> error ($(curLoc) ++ "expected a recursive let-binding")
          else case unsinkTerm (bindNameBinders bind') r of
            Just r' -> pure r'
            Nothing -> pure (Let bind' r)
      _ -> error ($(curLoc) ++ "expected a let-binding")
