{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021,      QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for converting to A-Normal Form.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations.ANF
  ( makeANF
  , nonRepANF
  ) where

import Control.Arrow ((***))
import Control.Lens (_2)
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Control.Monad.State (StateT, lift, modify, runStateT)
import Control.Monad.Writer (listen)
import Data.Bifunctor (second)
import qualified Data.Monoid as Monoid (Any(..))
import qualified Data.Text.Extra as Text (showt)
import GHC.Stack (HasCallStack)

import Clash.Signal.Internal (Signal(..))

import Clash.Core.DataCon (DataCon(..))
import Clash.Core.FreeVars (localIdsDoNotOccurIn)
import Clash.Core.Name (mkUnsafeSystemName, nameOcc)
import Clash.Core.Subst (deshadowLetExpr, freshenTm)
import Clash.Core.Term
  ( Alt, CoreContext(..), LetBinding, Pat(..), PrimInfo(..), Term(..)
  , collectArgs, collectTicks, mkTicks, partitionTicks, stripTicks)
import Clash.Core.TermInfo (isCon, isLocalVar, isPrim, isVar, termType)
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type (Type, TypeView(..), coreView, tyView)
import Clash.Core.Util (mkSelectorCase)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (InScopeSet, extendInScopeSet, extendInScopeSetList)
import Clash.Netlist.Util (bindsExistentials)
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Normalize.Util (specializeNorm)
import Clash.Rewrite.Combinators (bottomupR)
import Clash.Rewrite.Types
  (Transform, TransformContext(..), tcCache)
import Clash.Rewrite.Util
  (changed, isUntranslatable, mkDerivedName, mkTmBinderFor)
import Clash.Rewrite.WorkFree (isConstant, isConstantNotClockReset)
import Clash.Util (curLoc)

{- [Note: Name re-creation]
The names of heap bound variables are safely generate with mkUniqSystemId in
Clash.Core.Evaluator.newLetBinding. But only their uniqs end up in the heap,
not the complete names. So we use mkUnsafeSystemName to recreate the same Name.
-}

-- | Turn an expression into a modified ANF-form. As opposed to standard ANF,
-- constants do not become let-bound.
makeANF :: HasCallStack => NormRewrite
makeANF (TransformContext is0 ctx) (Lam bndr e) = do
  let ctx' = TransformContext (extendInScopeSet is0 bndr) (LamBody bndr : ctx)
  e' <- makeANF ctx' e
  return (Lam bndr e')

makeANF _ e@(TyLam {}) = return e

makeANF ctx@(TransformContext is0 _) e0 = do
    -- We need to freshen all binders in `e` because we're shuffling them around
    -- into a single let-binder, because even when binders don't shadow, they
    -- don't have to be unique within an expression. And so lifting them all
    -- to a single let-binder will cause issues when they're not unique.
    --
    -- We cannot make freshening part of collectANF, because when we generate
    -- new binders, we need to make sure those names do not conflict with _any_
    -- of the existing binders in the expression.
    --
    -- See also Note [ANF InScopeSet]
    let (is2,e1) = freshenTm is0 e0
    ((e2,(bndrs,_)),Monoid.getAny -> hasChanged) <-
      listen (runStateT (bottomupR collectANF ctx e1) ([],is2))
    case bndrs of
      [] -> if hasChanged then return e2 else return e0
      _  -> do
        let (e3,ticks) = collectTicks e2
            (srcTicks,nmTicks) = partitionTicks ticks
        -- Ensure that `AppendName` ticks still scope over the entire expression
        changed (mkTicks (Letrec bndrs (mkTicks e3 srcTicks)) nmTicks)
{-# SCC makeANF #-}

type NormRewriteW = Transform (StateT ([LetBinding],InScopeSet) NormalizeSession)

-- | See Note [ANF InScopeSet]
tellBinders :: [LetBinding] -> StateT ([LetBinding],InScopeSet) NormalizeSession ()
tellBinders bs = modify ((bs ++) *** (`extendInScopeSetList` (map fst bs)))

-- | See Note [ANF InScopeSet]; only extends the inscopeset
notifyBinders :: Monad m => [LetBinding] -> StateT ([LetBinding],InScopeSet) m ()
notifyBinders bs = modify (second (`extendInScopeSetList` (map fst bs)))

-- | Is the given type IO-like
isSimIOTy
  :: TyConMap
  -> Type
  -- ^ Type to check for IO-likeness
  -> Bool
isSimIOTy tcm ty = case tyView (coreView tcm ty) of
  TyConApp tcNm args
    | nameOcc tcNm == "Clash.Explicit.SimIO.SimIO"
    -> True
    | nameOcc tcNm == "GHC.Prim.(#,#)"
    , [_,_,st,_] <- args
    -> isStateTokenTy tcm st
  FunTy _ res -> isSimIOTy tcm res
  _ -> False

-- | Is the given type the state token
isStateTokenTy
  :: TyConMap
  -> Type
  -- ^ Type to check for state tokenness
  -> Bool
isStateTokenTy tcm ty = case tyView (coreView tcm ty) of
  TyConApp tcNm _ -> nameOcc tcNm == "GHC.Prim.State#"
  _ -> False

-- | Note [ANF InScopeSet]
--
-- The InScopeSet contains:
--
--    1. All the free variables of the expression we are traversing
--
--    2. All the bound variables of the expression we are traversing
--
--    3. The newly created let-bindings as we recurse back up the traversal
--
-- All of these are needed to created let-bindings that
--
--    * Do not shadow
--    * Are not shadowed
--    * Nor conflict with each other (i.e. have the same unique)
--
-- Initially we start with the local InScopeSet and add the global variables:
--
-- @
-- is1 <- unionInScope is0 <$> Lens.use globalInScope
-- @
--
-- Which will gives us the (superset of) free variables of the expression. Then
-- we call  'freshenTm'
--
-- @
-- let (is2,e1) = freshenTm is1 e0
-- @
--
-- Which extends the InScopeSet with all the bound variables in 'e1', the
-- version of 'e0' where all binders are unique (not just deshadowed).
--
-- So we start out with an InScopeSet that satisfies points 1 and 2, now every
-- time we create a new binder we must add it to the InScopeSet to satisfy
-- point 3.
--
-- Note [ANF no let-bind]
--
-- | Do not let-bind:
--
-- 1. Arguments with an untranslatable type: untranslatable expressions
--    should be propagated down as far as possible
--
-- 2. Local variables or constants: they don't add any work, so no reason
--    to let-bind to enable sharing
--
-- 3. IO actions, the translation of IO actions to sequential HDL constructs
--    depends on IO actions to be propagated down as far as possible.
collectANF :: HasCallStack => NormRewriteW
collectANF ctx e@(App appf arg)
  | (conVarPrim, _) <- collectArgs e
  , isCon conVarPrim || isPrim conVarPrim || isVar conVarPrim
  = do
    tcm <- Lens.view tcCache
    untranslatable <- lift (isUntranslatable False arg)
    let localVar   = isLocalVar arg
        constantNoCR = isConstantNotClockReset tcm arg
    -- See Note [ANF no let-bind]
    case (untranslatable,localVar || constantNoCR, isSimBind conVarPrim,arg) of
      (False,False,False,_) -> do
        -- See Note [ANF InScopeSet]
        is1   <- Lens.use _2
        argId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "app_arg") arg)
        -- See Note [ANF InScopeSet]
        tellBinders [(argId,arg)]
        return (App appf (Var argId))
      (True,False,_,Letrec binds body) -> do
        tellBinders binds
        return (App appf body)
      _ -> return e
 where
  isSimBind (Prim p) = primName p == "Clash.Explicit.SimIO.bindSimIO#"
  isSimBind _ = False

collectANF _ (Letrec binds body) = do
  tcm <- Lens.view tcCache
  let isSimIO = isSimIOTy tcm (termType tcm body)
  untranslatable <- lift (isUntranslatable False body)
  let localVar = isLocalVar body
  -- See Note [ANF no let-bind]
  if localVar || untranslatable || isSimIO
    then do
      tellBinders binds
      return body
    else do
      -- See Note [ANF InScopeSet]
      is1 <- Lens.use _2
      argId <- lift (mkTmBinderFor is1 tcm (mkUnsafeSystemName "result" 0) body)
      -- See Note [ANF InScopeSet]
      tellBinders [(argId,body)]
      tellBinders binds
      return (Var argId)

-- TODO: The code below special-cases ANF for the ':-' constructor for the
-- 'Signal' type. The 'Signal' type is essentially treated as a "transparent"
-- type by the Clash compiler, so observing its constructor leads to all kinds
-- of problems. In this case that "Clash.Rewrite.Util.mkSelectorCase" will
-- try to project the LHS and RHS of the ':-' constructor, however,
-- 'mkSelectorCase' uses 'coreView1' to find the "real" data-constructor.
-- 'coreView1' however looks through the 'Signal' type, and hence 'mkSelector'
-- finds the data constructors for the element type of Signal. This resulted in
-- error #24 (https://github.com/christiaanb/clash2/issues/24), where we
-- try to get the first field out of the 'Vec's 'Nil' constructor.
--
-- Ultimately we should stop treating Signal as a "transparent" type and deal
-- handling of the Signal type, and the involved co-recursive functions,
-- properly. At the moment, Clash cannot deal with this recursive type and the
-- recursive functions involved, hence the need for special-casing code. After
-- everything is done properly, we should remove the two lines below.
collectANF _ e@(Case _ _ [(DataPat dc _ _,_)])
  | nameOcc (dcName dc) == Text.showt '(:-) = return e

collectANF ctx (Case subj ty alts) = do
    let localVar = isLocalVar subj
    let isConstantSubj = isConstant subj

    (subj',subjBinders) <- if localVar || isConstantSubj
      then return (subj,[])
      else do
        tcm <- Lens.view tcCache
        -- See Note [ANF InScopeSet]
        is1 <- Lens.use _2
        argId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "case_scrut") subj)
        -- See Note [ANF InScopeSet]
        notifyBinders [(argId,subj)]
        return (Var argId,[(argId,subj)])

    tcm <- Lens.view tcCache
    let isSimIOAlt = isSimIOTy tcm ty

    alts' <- mapM (doAlt isSimIOAlt subj') alts
    tellBinders subjBinders

    case alts' of
      [(DataPat _ [] xs,altExpr)]
        | xs `localIdsDoNotOccurIn` altExpr || isSimIOAlt
        -> return altExpr
      _ -> return (Case subj' ty alts')
  where
    doAlt :: Bool -> Term -> Alt -> StateT ([LetBinding],InScopeSet) NormalizeSession Alt
    doAlt isSimIOAlt subj' alt@(DataPat dc exts xs,altExpr) | not (bindsExistentials exts xs) = do
      let lv = isLocalVar altExpr
      patSels <- Monad.zipWithM (doPatBndr subj' dc) xs [0..]
      let altExprIsConstant = isConstant altExpr
      let usesXs (Var n) = any (== n) xs
          usesXs _       = False
      -- See [ANF no let-bind]
      if or [isSimIOAlt, lv && (not (usesXs altExpr) || length alts == 1), altExprIsConstant]
        then do
          -- See Note [ANF InScopeSet]
          tellBinders patSels
          return alt
        else do
          tcm <- Lens.view tcCache
          -- See Note [ANF InScopeSet]
          is1 <- Lens.use _2
          altId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "case_alt") altExpr)
          -- See Note [ANF InScopeSet]
          tellBinders (patSels ++ [(altId,altExpr)])
          return (DataPat dc exts xs,Var altId)
    doAlt _ _ alt@(DataPat {}, _) = return alt
    doAlt isSimIOAlt _ alt@(pat,altExpr) = do
      let lv = isLocalVar altExpr
      let altExprIsConstant = isConstant altExpr
      -- See [ANF no let-bind]
      if isSimIOAlt || lv || altExprIsConstant
        then return alt
        else do
          tcm <- Lens.view tcCache
          -- See Note [ANF InScopeSet]
          is1 <- Lens.use _2
          altId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "case_alt") altExpr)
          tellBinders [(altId,altExpr)]
          return (pat,Var altId)

    doPatBndr :: Term -> DataCon -> Id -> Int -> StateT ([LetBinding],InScopeSet) NormalizeSession LetBinding
    doPatBndr subj' dc pId i = do
      tcm <- Lens.view tcCache
      -- See Note [ANF InScopeSet]
      is1 <- Lens.use _2
      patExpr <- lift (mkSelectorCase ($(curLoc) ++ "doPatBndr") is1 tcm subj' (dcTag dc) i)
      -- No need to 'tellBinders' here because 'pId' is already in the ANF
      -- InScopeSet.
      --
      -- See also Note [ANF InScopeSet]
      return (pId,patExpr)

collectANF _ e = return e
{-# SCC collectANF #-}

-- | Bring an application of a DataCon or Primitive in ANF, when the argument is
-- is considered non-representable
nonRepANF :: HasCallStack => NormRewrite
nonRepANF ctx@(TransformContext is0 _) e@(App appConPrim arg)
  | (conPrim, _) <- collectArgs e
  , isCon conPrim || isPrim conPrim
  = do
    untranslatable <- isUntranslatable False arg
    case (untranslatable,stripTicks arg) of
      (True,Letrec binds body) ->
        -- This is a situation similar to Note [CaseLet deshadow]
        let (binds1,body1) = deshadowLetExpr is0 binds body
        in  changed (Letrec binds1 (App appConPrim body1))
      (True,Case {})  -> specializeNorm ctx e
      (True,Lam {})   -> specializeNorm ctx e
      (True,TyLam {}) -> specializeNorm ctx e
      _               -> return e

nonRepANF _ e = return e
{-# SCC nonRepANF #-}
