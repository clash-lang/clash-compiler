{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on letrec expressions.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Transformations.Letrec
  ( deadCode
  , deadCodeWorker
  , flattenLet
  , flattenLetWorker
  , recToLetRec
  , removeUnusedExpr
  , removeUnusedExprCase
  , removeUnusedExprSpine
  , simpleCSE
  , simpleCSEWorker
  , topLet
  , topLetWorker
  ) where

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Writer (listen)
import Data.Bifunctor (second)
import qualified Data.Either as Either
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map.Strict as Map
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.Maybe (fromMaybe)
import qualified Data.Monoid as Monoid (Any(..))
import qualified Data.Text as Text
import qualified Data.Text.Extra as Text
import GHC.Stack (HasCallStack)

import Clash.Annotations.BitRepresentation.Deriving (dontApplyInHDL)
import Clash.Sized.Vector as Vec (Vec(Cons), splitAt)

import Clash.Annotations.Primitive (extractPrim)
import Clash.Core.DataCon (DataCon(..))
import Clash.Core.FreeVars (freeLocalIds)
import Clash.Core.HasFreeVars
import Clash.Core.HasType
import Clash.Core.Name (mkUnsafeSystemName, nameOcc)
import Clash.Core.Subst
import Clash.Core.Term
  ( Alt, CoreContext(..), LetBinding, Pat(..), PrimInfo(..), Term(..), TickInfo(..)
  , bindToList, collectArgs, collectArgsTicks, collectTicks, isLambdaBodyCtx
  , isTickCtx, mkApps, mkLams, mkTicks, Bind(..), partitionTicks
  , stripAllTicks)
import Clash.Core.TermInfo (isCon, isLet, isLocalVar, isTick)
import Clash.Core.TyCon (TyConMap, tyConDataCons)
import Clash.Core.Type
  (Type(..), TypeView(..), isClassTy, normalizeType
  , splitFunForallTy, tyView)
import Clash.Core.Util (inverseTopSortLetBindings, mkVec, tyNatSize)
import Clash.Core.Var (isGlobalId, varUniq)
import Clash.Core.VarEnv
  ( InScopeSet, elemInScopeSet, emptyVarEnv, extendInScopeSetList, lookupVarEnv
  , unionVarEnvWith, unitVarEnv, mkVarSet)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Netlist.BlackBox.Types ()
import Clash.Netlist.BlackBox.Util (getUsedArguments)
import Clash.Netlist.Util (splitNormalized)
import Clash.Normalize.Primitives (removedArg)
import qualified Clash.Core.Scoped.Syntax as S
import qualified Clash.Normalize.Scoped.Transformations.Letrec as Scoped
import Clash.Normalize.Transformations.Reduce (reduceBinders)
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Primitives.Types (Primitive(..), UsedArguments(..))
import Clash.Rewrite.Scoped (ScopedTransform (..))
import Clash.Rewrite.Scoped.ViaFoil (viaFoilWhen)
import Clash.Rewrite.StrategyDSL
  ( Transformation, anyConstructor, onAppNode, onCase, onLetNode, onPrimNode
  , onTickNode, onTyAppNode, toTransformation )
import Clash.Rewrite.Types
  (TransformContext(..), bindings, curFun, tcCache, workFreeBinders, primitives)
import Clash.Rewrite.Util
  (changed, isFromInt, isUntranslatable, mkTmBinderFor, removeUnusedBinders, setChanged)
import Clash.Rewrite.WorkFree
import Clash.Unique (Unique)

{- [Note: Name re-creation]
The names of heap bound variables are safely generate with mkUniqSystemId in
Clash.Core.Evaluator.newLetBinding. But only their uniqs end up in the heap,
not the complete names. So we use mkUnsafeSystemName to recreate the same Name.
-}

-- | Remove unused let-bindings
deadCode :: Transformation
deadCode = Scoped.deadCode

-- | The 'Let' handler of 'deadCode'.
deadCodeWorker
  :: HasCallStack
  => TransformContext -> Term -> Bind Term -> Term -> NormalizeSession Term
deadCodeWorker _ e binds body =
  case removeUnusedBinders binds body of
    Just t -> changed t
    Nothing -> return e
{-# SCC deadCodeWorker #-}

-- | Replace unused arguments of primitives by 'removedArg'. This is the port
-- 'Scoped.removeUnusedExpr', which only converts the nodes at which it can
-- fire, see Note [Converting only where a port can fire].
removeUnusedExpr :: Transformation
removeUnusedExpr = toTransformation "removeUnusedExpr"
  (  onPrimNode 'removeUnusedExprViaFoil
  <> onAppNode 'removeUnusedExprViaFoil
  <> onTyAppNode 'removeUnusedExprViaFoil
  <> onTickNode 'removeUnusedExprViaFoil
  <> onCase 'removeUnusedExprCaseViaFoil)

{- Note [Converting only where a port can fire]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The strategy compiler runs a scoped transformation through 'viaFoil' at every
node it is offered at, which converts the whole subterm to scoped Core. For a
transformation that is offered at most nodes, such as 'removeUnusedExpr' at
every application and 'topLet' at every node, that makes normalization
quadratic in the size of a function. So the handlers below check on Clash Core
whether the port can fire at a node, and only then run it through 'viaFoil',
with 'viaFoilWhen'.
They go away together with 'viaFoil'.
-}

-- | The application-spine handler of 'removeUnusedExpr': the port at the root
-- of an application of a primitive or of 'Vec.Cons'
removeUnusedExprViaFoil :: NormRewrite
removeUnusedExprViaFoil =
  viaFoilWhen "removeUnusedExpr" removeUnusedExprCanFire
    (ScopedTransform Scoped.removeUnusedExprSpine)

-- | Whether 'Scoped.removeUnusedExprSpine' can fire at a node: the root of an
-- application of a primitive to an unused argument that is not 'removedArg'
-- yet, or of a 'Vec.Cons' of a non-constructor vector of length zero
removeUnusedExprCanFire :: TransformContext -> Term -> NormalizeSession Bool
removeUnusedExprCanFire (TransformContext _ cs) e
  | c:_ <- cs, isSpineCtx c = return False
  | otherwise = case collectArgsTicks e of
      (Prim pInfo, args, _) -> primUsedArgs pInfo (length args) >>= \case
        Just used
          | let arity = length . Either.rights . fst $ splitFunForallTy (coreTypeOf pInfo)
          , or [ n < arity && n `notElem` used && not (isRemovedArg tm)
               | (n, tm) <- zip [0..] (Either.lefts args) ]
          -> return True
        _ -> return False
      (Data dc, [_,_,Right nTy,_,Left _,Left nil], _)
        | nameOcc (dcName dc) == Text.showt 'Vec.Cons
        , (con, _) <- collectArgs nil
        , not (isCon con) -> do
            tcm <- Lens.view tcCache
            case runExcept (tyNatSize tcm nTy) of
              Right 0 -> return True
              _ -> return False
      _ -> return False
 where
  isRemovedArg = \case
    TyApp (Prim p0) _ -> primName p0 == Text.showt 'removedArg
    _ -> False
  isSpineCtx = \case
    AppFun -> True
    TyAppC -> True
    TickC _ -> True
    _ -> False

-- | The 'Case' handler of 'removeUnusedExpr': the port at a case-expression
-- with a single alternative without existentials
removeUnusedExprCaseViaFoil
  :: TransformContext -> Term -> Term -> Type -> [Alt] -> NormalizeSession Term
removeUnusedExprCaseViaFoil ctx e _ _ alts0 =
  viaFoilWhen "removeUnusedExpr" (\_ _ -> pure canFire) (ScopedTransform (\sctx -> \case
    t@(S.Case subj ty alts) -> Scoped.removeUnusedExprCase sctx t subj ty alts
    t -> return t)) ctx e
 where
  canFire = case alts0 of
    [(DataPat _ [] _, _)] -> True
    _ -> False

-- | The 'Case' handler of 'removeUnusedExpr': a single-alternative case whose
-- pattern binds no used variables is replaced by the alternative.
removeUnusedExprCase
  :: TransformContext -> Term -> Term -> Type -> [Alt] -> NormalizeSession Term
removeUnusedExprCase _ e _subj _ty [(DataPat _ [] xs,altExpr)] =
  if mkVarSet xs `disjointFreeVars` altExpr
     then changed altExpr
     else return e
removeUnusedExprCase _ e _subj _ty _alts = return e

-- | The application-spine handlers of 'removeUnusedExpr': replace unused
-- arguments of primitives (and 'Vec's 'Vec.Cons') with 'removedArg'.
removeUnusedExprSpine :: HasCallStack => NormRewrite
-- The equations below collect the whole application spine with
-- 'collectArgsTicks'. Say we have an application:
--
--     f a b c
--
-- Then we're only interested in the root (@f a b c@), not in the inner nodes
-- (@f a@, @f a b@). At the root, 'collectArgsTicks' collects the full argument
-- list @[a, b, c]@; an inner node would only ever see a prefix of that (@[a]@,
-- @[a, b]@) at the same argument indices. So any argument an inner node could
-- remove, the root removes too, which makes the inner attempts wasted work.
-- This holds through ticks and type applications as well, since
-- 'collectArgsTicks' looks through both. We therefore skip any node whose parent
-- continues the spine (an @AppFun@, @TyAppC@, or @TickC@ context);
-- 'removeUnusedExpr' only routes spine nodes (@App@, @TyApp@, @Prim@, @Tick@)
-- here, so no explicit node-constructor check is needed. The
-- single-alternative-Case handling lives in 'removeUnusedExprCase' and is
-- unaffected: the spine only threads through applications, type applications,
-- and ticks, so a Case is never an inner node of a spine.
removeUnusedExprSpine (TransformContext _ (cc:_)) e
  | isSpineCtx cc
  = return e
 where
  isSpineCtx AppFun = True
  isSpineCtx TyAppC = True
  isSpineCtx (TickC _) = True
  isSpineCtx _ = False

removeUnusedExprSpine _ e@(collectArgsTicks -> (p@(Prim pInfo),args,ticks)) = do
  usedArgs0 <- primUsedArgs pInfo (length args)
  case usedArgs0 of
    Nothing ->
      return e
    Just usedArgs1 -> do
      tcm <- Lens.view tcCache
      (args1, Monoid.getAny -> hasChanged) <- listen (go tcm 0 usedArgs1 args)
      if hasChanged then
        return (mkApps (mkTicks p ticks) args1)
      else
        return e

  where
    arity = length . Either.rights . fst $ splitFunForallTy (coreTypeOf pInfo)

    go _ _ _ [] = return []
    go tcm !n used (Right ty:args') = do
      args'' <- go tcm n used args'
      return (Right ty : args'')
    go tcm !n used (Left tm : args') = do
      args'' <- go tcm (n+1) used args'
      case tm of
        TyApp (Prim p0) _
          | primName p0 == Text.showt 'removedArg
          -> return (Left tm : args'')
        _ -> do
          let ty = inferCoreTypeOf tcm tm
              p' = TyApp (Prim removedArg) ty
          if n < arity && n `notElem` used
             then changed (Left p' : args'')
             else return  (Left tm : args'')

-- Replace any expression that creates a Vector of size 0 within the application
-- of the Cons constructor, by the Nil constructor.
removeUnusedExprSpine _ e@(collectArgsTicks -> (Data dc, [_,Right aTy,Right nTy,_,Left a,Left nil],ticks))
  | nameOcc (dcName dc) == Text.showt 'Vec.Cons
  = do
    tcm <- Lens.view tcCache
    case runExcept (tyNatSize tcm nTy) of
      Right 0
        | (con, _) <- collectArgs nil
        , not (isCon con)
        -> let eTy = inferCoreTypeOf tcm e
               v = fromMaybe (error "removeUnusedExpr: failed to create Vec DCs") $ do
                  (TyConApp vecTcNm _) <- pure (tyView eTy)
                  vecTc <- UniqMap.lookup vecTcNm tcm
                  [nilCon,consCon] <- pure (tyConDataCons vecTc)
                  return (mkTicks (mkVec nilCon consCon aTy 1 [a]) ticks)
           in  changed v
      _ -> return e

removeUnusedExprSpine _ e = return e
{-# SCC removeUnusedExprSpine #-}

-- | The arguments of a primitive that its blackbox uses, given the number of
-- arguments it is applied to. 'Nothing' if it has no blackbox.
primUsedArgs :: PrimInfo -> Int -> NormalizeSession (Maybe [Int])
primUsedArgs pInfo nArgs = do
  bbM <- HashMap.lookup (primName pInfo) <$> Lens.view primitives
  pure $ case Monad.join (extractPrim <$> bbM) of
    Just (BlackBoxHaskell{usedArguments}) ->
      case usedArguments of
        UsedArguments used -> Just used
        IgnoredArguments ignored -> Just ([0..nArgs - 1] \\ ignored)
    Just (BlackBox pNm _ _ _ _ _ _ _ _ _ inc r ri templ) -> Just $
      if | isFromInt pNm -> [0,1,2]
         | primName pInfo `elem` [ Text.showt 'dontApplyInHDL
                                 , Text.showt 'Vec.splitAt
                                 ] -> [0,1]
         | otherwise -> concat [ concatMap getUsedArguments r
                               , concatMap getUsedArguments ri
                               , getUsedArguments templ
                               , concatMap (getUsedArguments . snd) inc ]
    _ ->
      Nothing

-- | Prepare a let-expression for merging its bindings into a recursive group.
-- The binder of a 'NonRec' binding is not in scope of its right-hand side,
-- which can therefore refer to a variable that the binder shadows:
--
-- > \x -> let x = g x in ...
--
-- Merging that binding into a 'Rec' group as is would turn it into the loop
-- @letrec x = g x@, so this renames its binder first.
deshadowNonRec
  :: HasCallStack => InScopeSet -> Bind Term -> Term -> (Bind Term, Term)
deshadowNonRec is0 bind@(NonRec x e) body
  | x `elemFreeVars` e = deshadowLetExpr is0 bind body
deshadowNonRec _ bind body = (bind, body)

-- | Flatten's letrecs after `inlineCleanup`
--
-- `inlineCleanup` sometimes exposes additional possibilities for `caseCon`,
-- which then introduces let-bindings in what should be ANF. This transformation
-- flattens those nested let-bindings again.
--
-- NB: must only be called in the cleaning up phase.
flattenLet :: Transformation
flattenLet = toTransformation "flattenLet" (onLetNode 'flattenLetViaFoil)

-- | The 'Let' handler of 'flattenLet': the port 'Scoped.flattenLet' at a
-- let-expression with a let-expression in its body or in a right-hand side,
-- or with a single binding that it inlines, see Note [Converting only where a
-- port can fire]
flattenLetViaFoil :: NormRewrite
flattenLetViaFoil =
  viaFoilWhen "flattenLet" flattenLetCanFire (ScopedTransform Scoped.flattenLetWorker)

-- | Whether 'Scoped.flattenLetWorker' can fire at a node, see
-- 'flattenLetViaFoil'
flattenLetCanFire :: TransformContext -> Term -> NormalizeSession Bool
flattenLetCanFire _ e = case e of
  Let bind body
    | Let {} <- body -> return True
    | any (isLet . fst . collectTicks . snd) (bindToList bind) -> return True
    | [(x, rhs)] <- bindToList bind -> do
        let occ = Lens.lengthOf (freeLocalIds . Lens.filtered (== x)) body
        inline <-
          if | occ == 0 -> pure False
             | occ < 2 -> pure True
             | otherwise -> do
                 bndrs <- Lens.use bindings
                 isWorkFree workFreeBinders bndrs rhs
        -- The right-hand side of a 'NonRec' binding doesn't refer to its binder
        let recursive = case bind of
              Rec {} -> x `elemFreeVars` rhs
              NonRec {} -> False
        return (inline && not recursive)
  _ -> return False

-- | The 'Let' handler of 'flattenLet'; recurses on the rebuilt let-expression
-- after merging nested bindings.
flattenLetWorker :: HasCallStack => NormRewrite
-- Flattening merges the binding into a recursive group, see 'deshadowNonRec'
flattenLetWorker ctx@(TransformContext is0 _) (Let bind@(NonRec x e) body)
  | x `elemFreeVars` e
  = flattenLetWorker ctx (uncurry Let (deshadowNonRec is0 bind body))

flattenLetWorker ctx@(TransformContext is0 _) (Letrec binds0 body0@Letrec{}) = do
  -- deshadow binds1, so binds0 and binds1 don't conflict when merged
  let is1 = extendInScopeSetList is0 (fmap fst binds0)
  case deShadowTerm is1 body0 of
    Letrec binds1 body1 -> do
      setChanged
      flattenLetWorker ctx{tfInScope=is1} (Letrec (binds0 <> binds1) body1)
    _ -> error "internal error"

flattenLetWorker (TransformContext is0 _) letE@(Letrec binds body) = do
  let is1 = extendInScopeSetList is0 (map fst binds)
      bodyOccs = Lens.foldMapByOf
                   freeLocalIds (unionVarEnvWith (+))
                   emptyVarEnv (`unitVarEnv` (1 :: Int))
                   body
  (is2,binds1) <- second concat <$> List.mapAccumLM go is1 binds
  bndrs <- Lens.use bindings
  e1WorkFree <-
    case binds1 of
      [(_,e1)] -> isWorkFree workFreeBinders bndrs e1
      _ -> pure (error "flattenLet: unreachable")
  case binds1 of
    -- inline binders into the body when there's only a single binder, and only
    -- if that binder doesn't perform any work or is only used once in the body
    [(id1,e1)] | Just occ <- lookupVarEnv id1 bodyOccs, e1WorkFree || occ < 2 ->
      if id1 `elemFreeVars` e1
         -- Except when the binder is recursive!
         then return (rebuild binds1)
         else let subst = extendIdSubst (mkSubst is2) id1 e1
              in changed (substTm "flattenLet" subst body)
    _ -> return (rebuild binds1)
  where
    -- A single binding keeps its form. Rebuilding a 'NonRec' as a 'Rec' would
    -- bring its binder into scope of its own right-hand side.
    rebuild [(id1,e1)] | Let NonRec{} _ <- letE = Let (NonRec id1 e1) body
    rebuild binds1 = Letrec binds1 body

    go :: InScopeSet -> LetBinding -> NormalizeSession (InScopeSet,[LetBinding])
    go isN (id1,collectTicks -> (Let bind1 body1,ticks)) = do
      let bs1 = map fst (bindToList bind1)
      let (bind2,body2,isN1) =
            -- We need to deshadow because we're merging nested let-expressions
            -- into a single let-expression: and within a let-expression, the
            -- bindings are not allowed to shadow each-other. Of course, we
            -- only need to deshadow if any shadowing is happening in the
            -- first place.
            --
            -- This is much better than blindly calling freshenTm, and saves
            -- almost 30% run-time of the normalization phase on some examples.
            --
            -- We deshadow a 'NonRec' binding as such: its right-hand side can
            -- refer to a variable its binder shadows, which must not be
            -- renamed along with the binder.
            if any (`elemInScopeSet` isN) bs1 then
              let (bindN,bodyN) = deshadowLetExpr isN bind1 body1
              in  (bindN,bodyN,extendInScopeSetList isN (map fst (bindToList bindN)))
            else
              (bind1,body1,extendInScopeSetList isN bs1)
          binds2 = bindToList bind2
          isRecursive id2 e2 = case bind2 of
            NonRec{} -> False
            Rec{} -> id2 `elemFreeVars` e2
      let bodyOccs = Lens.foldMapByOf
                       freeLocalIds (unionVarEnvWith (+))
                       emptyVarEnv (`unitVarEnv` (1 :: Int))
                       body2
          (srcTicks,nmTicks) = partitionTicks ticks
      bndrs <- Lens.use bindings
      e2WorkFree <-
        case binds2 of
          [(_,e2)] -> isWorkFree workFreeBinders bndrs e2
          _ -> pure (error "flattenLet: unreachable")
      -- Distribute the name ticks of the let-expression over all the bindings
      (isN1,) . map (second (`mkTicks` nmTicks)) <$> case binds2 of
        -- inline binders into the body when there's only a single binder, and
        -- only if that binder doesn't perform any work or is only used once in
        -- the body
        [(id2,e2)] | Just occ <- lookupVarEnv id2 bodyOccs, e2WorkFree || occ < 2 ->
          if isRecursive id2 e2
             -- Except when the binder is recursive!
             then changed ([(id2,e2),(id1, body2)])
             else let subst = extendIdSubst (mkSubst isN1) id2 e2
                  in  changed [(id1
                               -- Only apply srcTicks to the body
                               ,mkTicks (substTm "flattenLetGo" subst body2)
                                        srcTicks)]
        bs -> changed (bs ++ [(id1
                               -- Only apply srcTicks to the body
                              ,mkTicks body2 srcTicks)])
    go isN b = return (isN,[b])

flattenLetWorker _ e = return e
{-# SCC flattenLetWorker #-}

-- | Turn a  normalized recursive function, where the recursive calls only pass
-- along the unchanged original arguments, into let-recursive function. This
-- means that all recursive calls are replaced by the same variable reference as
-- found in the body of the top-level let-expression.
recToLetRec :: HasCallStack => NormRewrite
recToLetRec (TransformContext is0 []) e = do
  (fn,_) <- Lens.use curFun
  tcm    <- Lens.view tcCache
  case splitNormalized tcm e of
    Right (args,bndrs,res) -> do
      let args'             = map Var args
          (toInline,others) = List.partition (eqApp tcm fn args' . snd) bndrs
          resV              = Var res
      case (toInline,others) of
        (_:_,_:_) -> do
          let is1          = extendInScopeSetList is0 (args ++ map fst bndrs)
          let substsInline = extendIdSubstList (mkSubst is1)
                           $ map (second (const resV)) toInline
              others'      = map (second (substTm "recToLetRec" substsInline))
                                 others
          changed $ mkLams (Letrec others' resV) args
        _ -> return e
    _ -> return e
  where
    -- This checks whether things are semantically equal. For example, say we
    -- have:
    --
    --   x :: (a, (b, c))
    --
    -- and
    --
    --   y :: (a, (b, c))
    --
    -- If we can determine that 'y' is constructed solely using the
    -- corresponding fields in 'x', then we can say they are semantically
    -- equal. The algorithm below keeps track of what (sub)field it is
    -- constructing, and checks if the field-expression projects the
    -- corresponding (sub)field from the target variable.
    --
    -- TODO: See [Note: Breaks on constants and predetermined equality]
    --
    -- Since 'aeqTerm' now looks at ticks when determining equality, it is
    -- required that all ticks are removed with 'stripAllTicks' to keep the
    -- previous behaviour of this function. If we remove this, most terms will
    -- not be identified as equal.
    eqApp tcm v args (collectArgs . stripAllTicks -> (Var v',args'))
      | isGlobalId v'
      , v == v'
      , let args2 = Either.lefts args'
      , length args == length args2
      = and (zipWith (eqArg tcm) args args2)
    eqApp _ _ _ _ = False

    eqArg _ v1 v2@Var{}
      = v1 == v2
    eqArg tcm v1 v2@(collectArgs -> (Data _, args'))
      | let t1 = normalizeType tcm (inferCoreTypeOf tcm v1)
      , let t2 = normalizeType tcm (inferCoreTypeOf tcm v2)
      , t1 == t2
      = if isClassConstraint t1 then
          -- Class constraints are equal if their types are equal, so we can
          -- take a shortcut here.
          True
        else
          -- Check whether all arguments to the data constructor are projections
          --
          and (zipWith (eqDat tcm v1) (map pure [0..]) (Either.lefts args'))
    eqArg _ _ _
      = False

    -- Recursively check whether a term /e/ is semantically equal to some variable /v/.
    -- Currently it can only assert equality when /e/:
    --
    --   * is syntactically equal to /v/; or
    --   * is constructed out of projections of /v/; or
    --   * is constructed out of type-equal class dictionaries
    --
    -- or a mix of these.
    --
    -- [Note: Breaks on constants and predetermined equality]
    -- This function currently breaks if:
    --
    --   * One or more subfields are constants. Constants might have been
    --     inlined for the construction, instead of being a projection of the
    --     target variable.
    --
    --   * One or more subfields are determined to be equal and one is simply
    --     swapped / replaced by the other. For example, say we have
    --     `x :: (a, a)`. If GHC determines that both elements of the tuple will
    --     always be the same, it might replace the (semantically equal to 'x')
    --     construction of `y` with `(fst x, fst x)`.
    --
    eqDat :: TyConMap -> Term -> [Int] -> Term -> Bool
    eqDat tcm v fTrace (collectArgs -> (Data _, args)) =
      and (zipWith (eqDat tcm v) (map (:fTrace) [0..]) (Either.lefts args))
    eqDat tcm v1 fTrace v2 =
      case stripProjection (reverse fTrace) v1 v2 of
        Just [] -> True
        -- A class dictionary subfield is uniquely determined by its type, so we
        -- don't require it to be projected from the exact corresponding field.
        -- GHC routinely shares such dictionaries (e.g. the @KnownDomain@ inside
        -- a @HiddenClockResetEnable@) via CSE, projecting them from a different
        -- but type-equal field of the target than the one being reconstructed.
        _ -> isClassTy tcm (inferCoreTypeOf tcm v2)

    stripProjection :: [Int] -> Term -> Term -> Maybe [Int]
    stripProjection fTrace0 vTarget0 (Case v _ [(DataPat _ _ xs, r)]) = do
      -- Get projection made in subject of case:
      fTrace1 <- stripProjection fTrace0 vTarget0 v

      -- Extract projection of this case statement. Subsequent calls to
      -- 'stripProjection' will check if new target is actually used.
      (n, fTrace2) <- List.uncons fTrace1
      vTarget1 <- List.indexMaybe xs n

      stripProjection fTrace2 (Var vTarget1) r

    stripProjection fTrace (Var sTarget) (Var s) =
      if sTarget == s then Just fTrace else Nothing

    stripProjection _fTrace _vTarget _v =
      Nothing

recToLetRec _ e = return e
{-# SCC recToLetRec #-}

isClassConstraint :: Type -> Bool
isClassConstraint (tyView -> TyConApp nm0 _) =
  if -- Constraint tuple. The host module is 'GHC.Classes' on GHC <= 9.12 and
     -- 'GHC.Internal.Classes' on GHC >= 9.14; both end in '.Classes.(%'.
     | ".Classes.(%" `Text.isInfixOf` nm1 -> True
     -- Constraint class:
     | "C:" `Text.isInfixOf` nm2 -> True
     | otherwise -> False
 where
  nm1 = nameOcc nm0
  nm2 = snd (Text.breakOnEnd "." nm1)

isClassConstraint _ = False

-- | Simplified CSE, only works on let-bindings, does an inverse topological
-- sort of the let-bindings and then works from top to bottom
--
-- XXX: Check whether inverse top-sort followed by single traversal removes as
-- many binders as the previous "apply-until-fixpoint" approach in the presence
-- of recursive groups in the let-bindings. If not but just for checking whether
-- changes to transformation affect the eventual size of the circuit, it would
-- be really helpful if we tracked circuit size in the regression/test suite.
-- On the two examples that were tested, Reducer and PipelinesViaFolds, this new
-- version of CSE removed the same amount of let-binders.
simpleCSE :: Transformation
simpleCSE = toTransformation "CSE" (onLetNode 'simpleCSEViaFoil)

-- | The 'Let' handler of 'simpleCSE': the port 'Scoped.simpleCSE' at a
-- recursive let-expression with more than one binding, see Note [Converting
-- only where a port can fire]
simpleCSEViaFoil :: NormRewrite
simpleCSEViaFoil =
  viaFoilWhen "CSE" (\_ e -> pure (canFire e)) (ScopedTransform (\sctx -> \case
    t@(S.Let bind body) -> Scoped.simpleCSEWorker sctx t bind body
    t -> return t))
 where
  canFire = \case
    Let (Rec bs@(_:_:_)) _ -> hasEqualRhss (map snd bs)
    _ -> False

-- | The 'Let' handler of 'simpleCSE'.
simpleCSEWorker
  :: HasCallStack
  => TransformContext -> Term -> Bind Term -> Term -> NormalizeSession Term
simpleCSEWorker (TransformContext is0 _) term (bindToList -> bndrsX) body = do
  let bndrs = inverseTopSortLetBindings bndrsX
  let is1 = extendInScopeSetList is0 (map fst bndrs)
  ((subst,bndrs1), change) <- listen $ reduceBinders (mkSubst is1) [] bndrs
  -- TODO: check whether a substitution over the body is enough, the reason I'm
  -- doing a substitution over the the binders as well is that I don't know in
  -- what order a recursive group shows up in a inverse topological sort.
  -- Depending on the order and forgetting to apply the substitution over the
  -- let-bindings might lead to the introduction of free variables.
  --
  -- NB: don't apply the substitution to the entire let-expression, and that
  -- would rename the let-bindings because they've been added to the InScopeSet
  -- of the substitution.
  if Monoid.getAny change
     then
       let bndrs2 = map (second (substTm "simpleCSE.bndrs" subst)) bndrs1
           body1 = substTm "simpleCSE.body" subst body
        in changed (Letrec bndrs2 body1)
     else
       return term
{-# SCC simpleCSEWorker #-}

-- | Whether two of the given right-hand sides of let-bindings are
-- alpha-equivalent and not marked 'NoDeDup': the condition for 'simpleCSE' to
-- replace one of them. Only compares right-hand sides that apply the same head
-- to the same number of arguments.
hasEqualRhss :: [Term] -> Bool
hasEqualRhss rhss = any pairwise (Map.elems groups)
 where
  groups = Map.fromListWith (<>)
    [ (key hd (length args), [rhs])
    | rhs <- rhss
    , let (hd, args, ticks) = collectArgsTicks rhs
    , NoDeDup `notElem` ticks ]

  pairwise (t:ts) = any (== t) ts || pairwise ts
  pairwise [] = False

  key :: Term -> Int -> (Int, Unique, Text.Text, Int)
  key hd n = case hd of
    Var v -> (0, varUniq v, mempty, n)
    Data dc -> (1, dcUniq dc, mempty, n)
    Prim p -> (2, 0, primName p, n)
    Literal {} -> (3, 0, mempty, n)
    _ -> (4, 0, mempty, n)

-- | Ensure that top-level lambda's eventually bind a let-expression of which
-- the body is a variable-reference.
topLet :: Transformation
topLet = anyConstructor "topLet" 'topLetViaFoil

-- | The worker of 'topLet': the port 'Scoped.topLet' in the body of a
-- function, at a term that is not a tick, and whose result (the body, if it is
-- a let-expression) is translatable and not a local variable, see Note
-- [Converting only where a port can fire]
topLetViaFoil :: NormRewrite
topLetViaFoil =
  viaFoilWhen "topLet" topLetCanFire (ScopedTransform Scoped.topLetWorker)

-- | Whether 'Scoped.topLetWorker' can fire at a node, see 'topLetViaFoil'
topLetCanFire :: TransformContext -> Term -> NormalizeSession Bool
topLetCanFire (TransformContext _ cs) e
  | all (\c -> isLambdaBodyCtx c || isTickCtx c) cs
  , not (isTick e)
  = case e of
      Let _ body
        | isLocalVar body -> return False
        | otherwise -> translatable body
      _ -> translatable e
  | otherwise = return False
 where
  translatable t = not <$> isUntranslatable False t

-- | The worker of 'topLet': a plain rewrite, offered at every node.
topLetWorker :: HasCallStack => NormRewrite
topLetWorker (TransformContext is0 ctx) e
  | all (\c -> isLambdaBodyCtx c || isTickCtx c) ctx && not (isLet e) && not (isTick e)
  = do
  untranslatable <- isUntranslatable False e
  if untranslatable
    then return e
    else do tcm <- Lens.view tcCache
            argId <- mkTmBinderFor is0 tcm (mkUnsafeSystemName "result" 0) e
            changed (Let (NonRec argId e) (Var argId))

topLetWorker (TransformContext is0 ctx) e@(Let bind0 body0)
  | all (\c -> isLambdaBodyCtx c || isTickCtx c) ctx
  = do
    let localVar = isLocalVar body0
    untranslatable <- isUntranslatable False body0
    if localVar || untranslatable
      then return e
      else do
        tcm <- Lens.view tcCache
        -- The new binder joins the bindings in a recursive group, see
        -- 'deshadowNonRec'
        let (bindToList -> binds, body) = deshadowNonRec is0 bind0 body0
        let is2 = extendInScopeSetList is0 (fmap fst binds)
        argId <- mkTmBinderFor is2 tcm (mkUnsafeSystemName "result" 0) body

        -- NB: We cannot nest the new binding instead:
        --
        -- Let bind0 (Let (NonRec argId body0) (Var argId))
        --
        -- 'flattenLet' would later inline the single use of its binder, e.g.
        -- after this let-expression was inlined into an argument of
        -- 'bindSimIO#', leaving a let-expression whose body is not a variable.
        -- That makes tests/shouldwork/SimIO/Test00.hs fail.
        changed (Letrec (binds ++ [(argId, body)]) (Var argId))

topLetWorker _ e = return e
{-# SCC topLetWorker #-}
