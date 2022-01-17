{-|
Copyright   : (C) 2020-2022, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qaylogic.com>

Check whether a term is work free or not. This is used by transformations /
evaluation to check whether it is possible to perform changes without
duplicating work in the result, e.g. inlining.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Rewrite.WorkFree
  ( isWorkFree
  , isWorkFreePure
  , isWorkFreeClockOrResetOrEnable
  , isWorkFreeIsh
  , isConstant
  , isConstantNotClockReset
  ) where

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar.Lifted as MVar
import Control.Lens as Lens (Lens', use)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Text.Extra as Text
import GHC.Stack (HasCallStack)

import Clash.Core.HasFreeVars
import Clash.Core.FreeVars
import Clash.Core.HasType
import Clash.Core.Pretty (showPpr)
import Clash.Core.Term
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type (isPolyFunTy)
import Clash.Core.Util
import Clash.Core.Var (Id, isLocalId)
import Clash.Core.VarEnv (VarEnv, extendVarEnv, lookupVarEnv, unionVarEnv)
import Clash.Driver.Types (BindingMap, Binding(..))
import Clash.Normalize.Primitives (removedArg)

-- TODO I think isWorkFree only needs to exist within the rewriting monad, and
-- this extra polymorphism is probably unnecessary. Needs checking. -- Alex

{-# INLINABLE isWorkFree #-}
isWorkFree
  :: (HasCallStack, MonadState s m, MonadBaseControl IO m)
  => Lens' s (MVar (VarEnv Bool))
  -> BindingMap
  -> Term
  -> m Bool
isWorkFree cacheL bndrs bndr = do
  lock <- Lens.use cacheL
  MVar.modifyMVar lock (\cache -> pure (isWorkFreePure cache bndrs bndr))

-- | Determines whether a global binder is work free. Errors if binder does
-- not exist.
isWorkFreeBinder
  :: HasCallStack
  => VarEnv Bool
  -> BindingMap
  -> Id
  -> (VarEnv Bool, Bool)
isWorkFreeBinder cache bndrs bndr =
  case lookupVarEnv bndr cache of
    Just value ->
      (cache, value)

    Nothing ->
      case lookupVarEnv bndr bndrs of
        Nothing ->
          error ("isWorkFreeBinder: couldn't find binder: " ++ showPpr bndr)

        Just (bindingTerm -> t) ->
          if bndr `globalIdOccursIn` t
            then (extendVarEnv bndr False cache, False)
            else isWorkFreePure cache bndrs t

{-# INLINABLE isWorkFreePure #-}
-- | Determine whether a term does any work, i.e. adds to the size of the
-- circuit. This function requires a cache (specified as a lens) to store the
-- result for querying work info of global binders.
--
isWorkFreePure
  :: HasCallStack
  => VarEnv Bool
  -> BindingMap
  -> Term
  -> (VarEnv Bool, Bool)
isWorkFreePure cache bndrs = go True
 where
  -- If we are in the outermost level of a term (i.e. not checking a subterm)
  -- then a term is work free if it simply refers to a local variable. This
  -- does not apply to subterms, as we do not want to count expressions like
  --
  --   f[LocalId] x[LocalId]
  --
  -- as being work free, as the term bound to f may introduce work.
  --
  go :: HasCallStack => Bool -> Term -> (VarEnv Bool, Bool)
  go isOutermost (collectArgs -> (fun, args)) =
    case fun of
      Var i
        -- We only allow polymorphic / function typed variables to be inlined
        -- if they are locally scoped, and the term is only a variable.
        --
        -- TODO This could be improved later by passing an InScopeSet to
        -- isWorkFree with all the local FVs of the term being checked. PE
        -- would need to be changed to know the FVs of global binders first.
        --
        | isPolyFunTy (coreTypeOf i) ->
            (cache, isLocalId i && isOutermost && null args)
        | isLocalId i ->
          (cache, True)
        | otherwise ->
            let (cache', wf) = isWorkFreeBinder cache bndrs i
                (caches, wfs) = unzip (fmap goArg args)
             in (foldr unionVarEnv cache' caches, and (wf : wfs))

      Data _ ->
        let (caches, wfs) = unzip (fmap goArg args)
         in (foldr unionVarEnv mempty caches, and wfs)

      Literal _ ->
        (cache, True)

      Prim pr ->
        case primWorkInfo pr of
          -- We can ignore arguments because the primitive outputs a constant
          -- regardless of their values.
          WorkConstant -> (cache, True)
          WorkNever ->
            let (caches, wfs) = unzip (fmap goArg args)
             in (foldr unionVarEnv mempty caches, and wfs)
          WorkIdentity _ _ ->
            let (caches, wfs) = unzip (fmap goArg args)
             in (foldr unionVarEnv mempty caches, and wfs)
          WorkVariable -> (cache, all isConstantArg args)
          WorkAlways -> (cache, False)

      Lam _ e ->
        let (cache', wf) = go False e
            (caches, wfs) = unzip (fmap goArg args)
         in (foldr unionVarEnv cache' caches, and (wf : wfs))

      TyLam _ e ->
        let (cache', wf) = go False e
            (caches, wfs) = unzip (fmap goArg args)
         in (foldr unionVarEnv cache' caches, and (wf : wfs))

      Let (NonRec _ x) e ->
        let (cacheE, wfE) = go False e
            (cacheX, wfX) = go False x
            (caches, wfs) = unzip (fmap goArg args)
         in (foldr unionVarEnv cacheE (cacheX : caches), and (wfE : wfX : wfs))

      Let (Rec bs) e ->
        let (cacheE, wfE) = go False e
            (cacheBs, wfBs) = unzip (fmap (go False . snd) bs)
            (caches, wfs) = unzip (fmap goArg args)
         in (foldr unionVarEnv cacheE (cacheBs <> caches), and (wfE : (wfBs <> wfs)))

      Case s _ [(_, a)] ->
        let (cacheS, wfS) = go False s
            (cacheA, wfA) = go False a
            (caches, wfs) = unzip (fmap goArg args)
         in (foldr unionVarEnv cacheS (cacheA : caches), and (wfS : wfA : wfs))

      Case e _ _ ->
        let (cache', wf) = go False e
            (caches, wfs) = unzip (fmap goArg args)
         in (foldr unionVarEnv cache' caches, and (wf : wfs))

      Cast e _ _ ->
        let (cache', wf) = go False e
            (caches, wfs) = unzip (fmap goArg args)
         in (foldr unionVarEnv cache' caches, and (wf : wfs))

      -- (Ty)App's and  Ticks are removed by collectArgs
      Tick _ _ -> error "isWorkFree: unexpected Tick"
      App {}   -> error "isWorkFree: unexpected App"
      TyApp {} -> error "isWorkFree: unexpected TyApp"

  goArg e = either (go False) (const (cache, True)) e
  isConstantArg = either isConstant (const True)

-- | Determine if a term represents a constant
isConstant :: Term -> Bool
isConstant e = case collectArgs e of
  (Data _, args)   -> all (either isConstant (const True)) args
  (Prim _, args) -> all (either isConstant (const True)) args
  (Lam _ _, _)     -> isClosed e
  (Literal _,_)    -> True
  _                -> False

isConstantNotClockReset :: TyConMap -> Term -> Bool
isConstantNotClockReset tcm e
  | isClockOrReset tcm eTy =
      case fst (collectArgs e) of
        Prim pr -> primName pr == Text.showt 'removedArg
        _ -> False

  | otherwise = isConstant e
 where
  eTy = inferCoreTypeOf tcm e

-- TODO: Remove function after using WorkInfo in 'isWorkFreeIsh'
isWorkFreeClockOrResetOrEnable
  :: TyConMap
  -> Term
  -> Maybe Bool
isWorkFreeClockOrResetOrEnable tcm e =
  let eTy = inferCoreTypeOf tcm e in
  if isClockOrReset tcm eTy || isEnable tcm eTy then
    case collectArgs e of
      (Prim p,_) -> Just (primName p == Text.showt 'removedArg)
      -- Only local variables with a clock type are work-free. When it is a global
      -- variable, it is probably backed by a clock generator, which is definitely
      -- not work-free.
      --
      -- Inlining let-bindings referencing a global variable with a clock type
      -- can sometimes lead to the post-normalization flattening stage to generate
      -- code that violates the invariants of the netlist generation stage.
      -- Especially when this global binder is defined recursively such as when
      -- using `tbClockGen`.
      -- This then ultimately leads to bad verilog names being generated as
      -- reported in: https://github.com/clash-lang/clash-compiler/issues/2845
      (Var v, []) -> Just (isLocalId v)
      (Data _, [_dom, Left (stripTicks -> Data _)]) -> Just True -- For Enable True/False
      (Literal _,_) -> Just True
      _ -> Just False
  else
    Nothing

-- | A conservative version of 'isWorkFree'. Is used to determine in 'bindConstantVar'
-- to determine whether an expression can be "bound" (locally inlined). While
-- binding workfree expressions won't result in extra work for the circuit, it
-- might very well cause extra work for Clash. In fact, using 'isWorkFree' in
-- 'bindConstantVar' makes Clash two orders of magnitude slower for some of our
-- test cases.
--
-- In effect, this function is a version of 'isConstant' that also considers
-- references to clocks and resets constant. This allows us to bind
-- HiddenClock(ResetEnable) constructs, allowing Clash to constant spec
-- subconstants - most notably KnownDomain. Doing that enables Clash to
-- eliminate any case-constructs on it.
isWorkFreeIsh
  :: TyConMap
  -> Term
  -> Bool
isWorkFreeIsh tcm e =
  case isWorkFreeClockOrResetOrEnable tcm e of
    Just b -> b
    Nothing ->
      case collectArgs e of
        (Data _, args)     -> all isWorkFreeIshArg args
        (Prim pInfo, args) -> case primWorkInfo pInfo of
          WorkAlways   -> False -- Things like clock or reset generator always
                                       -- perform work
          WorkVariable -> all isConstantArg args
          _            -> all isWorkFreeIshArg args

        (Lam _ _, _)       -> isClosed e
        (Literal _,_)      -> True
        _                  -> False
 where
  isWorkFreeIshArg = either (isWorkFreeIsh tcm) (const True)
  isConstantArg    = either isConstant (const True)
