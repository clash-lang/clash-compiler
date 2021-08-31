{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021     , QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on letrec expressions.
-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Transformations.Letrec
  ( deadCode
  , flattenLet
  , recToLetRec
  , removeUnusedExpr
  , simpleCSE
  , topLet
  ) where

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Writer (listen)
import Data.Bifunctor (second)
import qualified Data.Either as Either
import qualified Data.HashMap.Lazy as HashMap
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Monoid as Monoid (Any(..))
import qualified Data.Text as Text
import qualified Data.Text.Extra as Text
import GHC.Stack (HasCallStack)

import Clash.Annotations.BitRepresentation.Deriving (dontApplyInHDL)
import Clash.Sized.Vector as Vec (Vec(Cons), splitAt)

import Clash.Annotations.Primitive (extractPrim)
import Clash.Core.DataCon (DataCon(..))
import Clash.Core.FreeVars (freeLocalIds, localIdOccursIn, localIdsDoNotOccurIn)
import Clash.Core.HasType
import Clash.Core.Name (mkUnsafeSystemName, nameOcc)
import Clash.Core.Subst
import Clash.Core.Term
  ( LetBinding, Pat(..), PrimInfo(..), Term(..), collectArgs, collectArgsTicks
  , collectTicks, isLambdaBodyCtx, isTickCtx, mkApps, mkLams, mkTicks
  , partitionTicks, stripTicks)
import Clash.Core.TermInfo (isCon, isLet, isLocalVar, isTick)
import Clash.Core.TyCon (tyConDataCons)
import Clash.Core.Type
  (Type(..), TypeView(..), normalizeType
  , splitFunForallTy, tyView)
import Clash.Core.Util (inverseTopSortLetBindings, mkVec, tyNatSize)
import Clash.Core.Var (isGlobalId)
import Clash.Core.VarEnv
  ( InScopeSet, elemInScopeSet, emptyVarEnv, extendInScopeSetList, lookupVarEnv
  , unionVarEnvWith, unitVarEnv)
import Clash.Netlist.BlackBox.Types ()
import Clash.Netlist.BlackBox.Util (getUsedArguments)
import Clash.Netlist.Util (splitNormalized)
import Clash.Normalize.Primitives (removedArg)
import Clash.Normalize.Transformations.Reduce (reduceBinders)
import Clash.Normalize.Types (NormRewrite, NormalizeSession, primitives)
import Clash.Primitives.Types (Primitive(..), UsedArguments(..))
import Clash.Rewrite.Types
  (TransformContext(..), bindings, curFun, extra, tcCache, workFreeBinders)
import Clash.Rewrite.Util
  (changed, isFromInt, isUntranslatable, mkTmBinderFor, removeUnusedBinders, setChanged)
import Clash.Rewrite.WorkFree
import Clash.Unique (lookupUniqMap)

{- [Note: Name re-creation]
The names of heap bound variables are safely generate with mkUniqSystemId in
Clash.Core.Evaluator.newLetBinding. But only their uniqs end up in the heap,
not the complete names. So we use mkUnsafeSystemName to recreate the same Name.
-}

-- | Remove unused let-bindings
deadCode :: HasCallStack => NormRewrite
deadCode _ e@(Letrec binds body) =
  case removeUnusedBinders binds body of
    Just t -> changed t
    Nothing -> return e
deadCode _ e = return e
{-# SCC deadCode #-}

removeUnusedExpr :: HasCallStack => NormRewrite
removeUnusedExpr _ e@(collectArgsTicks -> (p@(Prim pInfo),args,ticks)) = do
  bbM <- HashMap.lookup (primName pInfo) <$> Lens.use (extra.primitives)
  let
    usedArgs0 =
      case Monad.join (extractPrim <$> bbM) of
        Just (BlackBoxHaskell{usedArguments}) ->
          case usedArguments of
            UsedArguments used -> Just used
            IgnoredArguments ignored -> Just ([0..length args - 1] \\ ignored)
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

removeUnusedExpr _ e@(Case _ _ [(DataPat _ [] xs,altExpr)]) =
  if xs `localIdsDoNotOccurIn` altExpr
     then changed altExpr
     else return e

-- Replace any expression that creates a Vector of size 0 within the application
-- of the Cons constructor, by the Nil constructor.
removeUnusedExpr _ e@(collectArgsTicks -> (Data dc, [_,Right aTy,Right nTy,_,Left a,Left nil],ticks))
  | nameOcc (dcName dc) == Text.showt 'Vec.Cons
  = do
    tcm <- Lens.view tcCache
    case runExcept (tyNatSize tcm nTy) of
      Right 0
        | (con, _) <- collectArgs nil
        , not (isCon con)
        -> let eTy = inferCoreTypeOf tcm e
               (TyConApp vecTcNm _) = tyView eTy
               (Just vecTc) = lookupUniqMap vecTcNm tcm
               [nilCon,consCon] = tyConDataCons vecTc
               v = mkTicks (mkVec nilCon consCon aTy 1 [a]) ticks
           in  changed v
      _ -> return e

removeUnusedExpr _ e = return e
{-# SCC removeUnusedExpr #-}

-- | Flatten's letrecs after `inlineCleanup`
--
-- `inlineCleanup` sometimes exposes additional possibilities for `caseCon`,
-- which then introduces let-bindings in what should be ANF. This transformation
-- flattens those nested let-bindings again.
--
-- NB: must only be called in the cleaning up phase.
flattenLet :: HasCallStack => NormRewrite
flattenLet ctx@(TransformContext is0 _) (Letrec binds0 body0@Letrec{}) = do
  -- deshadow binds1, so binds0 and binds1 don't conflict when merged
  let is1 = extendInScopeSetList is0 (fmap fst binds0)
      Letrec binds1 body1 = deShadowTerm is1 body0

  setChanged
  flattenLet ctx{tfInScope=is1} (Letrec (binds0 <> binds1) body1)

flattenLet (TransformContext is0 _) (Letrec binds body) = do
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
      if id1 `localIdOccursIn` e1
         -- Except when the binder is recursive!
         then return (Letrec binds1 body)
         else let subst = extendIdSubst (mkSubst is2) id1 e1
              in changed (substTm "flattenLet" subst body)
    _ -> return (Letrec binds1 body)
  where
    go :: InScopeSet -> LetBinding -> NormalizeSession (InScopeSet,[LetBinding])
    go isN (id1,collectTicks -> (Letrec binds1 body1,ticks)) = do
      let bs1 = map fst binds1
      let (binds2,body2,isN1) =
            -- We need to deshadow because we're merging nested let-expressions
            -- into a single let-expression: and within a let-expression, the
            -- bindings are not allowed to shadow each-other. Of course, we
            -- only need to deshadow if any shadowing is happening in the
            -- first place.
            --
            -- This is much better than blindly calling freshenTm, and saves
            -- almost 30% run-time of the normalization phase on some examples.
            if any (`elemInScopeSet` isN) bs1 then
              let Letrec bindsN bodyN = deShadowTerm isN (Letrec binds1 body1)
              in  (bindsN,bodyN,extendInScopeSetList isN (map fst bindsN))
            else
              (binds1,body1,extendInScopeSetList isN bs1)
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
          if id2 `localIdOccursIn` e2
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

flattenLet _ e = return e
{-# SCC flattenLet #-}

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
    eqApp tcm v args (collectArgs . stripTicks -> (Var v',args'))
      | isGlobalId v'
      , v == v'
      , let args2 = Either.lefts args'
      , length args == length args2
      = and (zipWith (eqArg tcm) args args2)
    eqApp _ _ _ _ = False

    eqArg _ v1 v2@(stripTicks -> Var {})
      = v1 == v2
    eqArg tcm v1 v2@(collectArgs . stripTicks -> (Data _, args'))
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
          and (zipWith (eqDat v1) (map pure [0..]) (Either.lefts args'))
    eqArg _ _ _
      = False

    -- Recursively check whether a term /e/ is semantically equal to some variable /v/.
    -- Currently it can only assert equality when /e/ is  syntactically equal
    -- to /v/, or is constructed out of projections of /v/, importantly:
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
    eqDat :: Term -> [Int] -> Term -> Bool
    eqDat v fTrace (collectArgs . stripTicks -> (Data _, args)) =
      and (zipWith (eqDat v) (map (:fTrace) [0..]) (Either.lefts args))
    eqDat v1 fTrace v2 =
      case stripProjection (reverse fTrace) v1 v2 of
        Just [] -> True
        _ -> False

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
  if -- Constraint tuple:
     | "GHC.Classes.(%" `Text.isInfixOf` nm1 -> True
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
simpleCSE :: HasCallStack => NormRewrite
simpleCSE (TransformContext is0 _) term@Letrec{} = do
  let Letrec bndrs body = inverseTopSortLetBindings term
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

simpleCSE _ e = return e
{-# SCC simpleCSE #-}

-- | Ensure that top-level lambda's eventually bind a let-expression of which
-- the body is a variable-reference.
topLet :: HasCallStack => NormRewrite
topLet (TransformContext is0 ctx) e
  | all (\c -> isLambdaBodyCtx c || isTickCtx c) ctx && not (isLet e) && not (isTick e)
  = do
  untranslatable <- isUntranslatable False e
  if untranslatable
    then return e
    else do tcm <- Lens.view tcCache
            argId <- mkTmBinderFor is0 tcm (mkUnsafeSystemName "result" 0) e
            changed (Letrec [(argId, e)] (Var argId))

topLet (TransformContext is0 ctx) e@(Letrec binds body)
  | all (\c -> isLambdaBodyCtx c || isTickCtx c) ctx
  = do
    let localVar = isLocalVar body
    untranslatable <- isUntranslatable False body
    if localVar || untranslatable
      then return e
      else do
        tcm <- Lens.view tcCache
        let is2 = extendInScopeSetList is0 (map fst binds)
        argId <- mkTmBinderFor is2 tcm (mkUnsafeSystemName "result" 0) body
        changed (Letrec (binds ++ [(argId,body)]) (Var argId))

topLet _ e = return e
{-# SCC topLet #-}
