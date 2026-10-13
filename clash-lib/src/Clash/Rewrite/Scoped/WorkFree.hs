{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Whether a scoped Clash Core term is work free, i.e. doesn't add to the size
  of the circuit: a port of "Clash.Rewrite.WorkFree".

  The work-freeness of global binders is cached in a t'GlobalEnv', keyed by
  'GlobalId', so a local variable can't hit an entry of a global with the same
  unique. Global bindings are closed terms (@'Term' ''VoidS'@).
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Rewrite.Scoped.WorkFree
  ( isWorkFree
  , isWorkFreeClockOrResetOrEnable
  , isWorkFreeIsh
  , isConstant
  , isConstantNotClockReset
  ) where

import Control.Lens (Lens', use, (%=))
import Control.Monad.Extra (allM, andM)
import Control.Monad.State.Class (MonadState)
import GHC.Stack (HasCallStack)

import Clash.Core.Basic (WorkInfo (..))
import Clash.Driver.Types (Binding (..))
import Clash.Normalize.Scoped.Primitives (removedArg)

import Clash.Core.Scoped.FreeVars (termGlobalIds)
import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.NameMap
  (GlobalEnv, insertGlobalEnv, lookupGlobalEnv, memberGlobalSet)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgs, stripTicks)
import Clash.Core.Scoped.TermInfo (isClosed)
import Clash.Core.Scoped.Type (isClockOrReset, isEnable, isPolyFunTy)
import Clash.Core.Scoped.TypeEnv (TypeEnv)

-- | Whether a global binder is work free, see 'isWorkFree'. Errors if there is
-- no binding for it.
isWorkFreeBinder
  :: (HasCallStack, MonadState s m)
  => Lens' s (GlobalEnv Bool)
  -> GlobalEnv (Binding (Term 'VoidS))
  -> GlobalId
  -> m Bool
isWorkFreeBinder cache bndrs g = do
  cached <- use cache
  case lookupGlobalEnv g cached of
    Just b -> pure b
    Nothing -> do
      b <- case lookupGlobalEnv g bndrs of
        Nothing -> error ("isWorkFreeBinder: couldn't find binder: " ++ show (globalIdName g))
        Just (bindingTerm -> t)
          | g `memberGlobalSet` termGlobalIds t -> pure False
          | otherwise -> isWorkFree cache bndrs t
      cache %= insertGlobalEnv g b
      pure b

{-# INLINABLE isWorkFree #-}
-- | Whether a term does any work, i.e. adds to the size of the circuit, see
-- 'Clash.Rewrite.WorkFree.isWorkFree'. Caches the work-freeness of global
-- binders.
isWorkFree
  :: forall s m n
   . (HasCallStack, MonadState s m)
  => Lens' s (GlobalEnv Bool)
  -- ^ Cache of the work-freeness of global binders
  -> GlobalEnv (Binding (Term 'VoidS))
  -- ^ Global bindings
  -> Term n
  -> m Bool
isWorkFree cache bndrs = go True
 where
  -- At the outermost level of a term, a reference to a local variable is work
  -- free, even if it has a function type. In a subterm it isn't, as e.g. in
  -- @f x@ the term bound to @f@ may do work.
  go :: HasCallStack => Bool -> Term x -> m Bool
  go isOutermost (collectArgs -> (fun, args)) = case fun of
    Var _ ty _
      | isPolyFunTy ty -> pure (isOutermost && null args)
      | otherwise -> pure True
    GlobalVar g
      | isPolyFunTy (globalIdType g) -> pure False
      | otherwise -> andM [isWorkFreeBinder cache bndrs g, allM goArg args]
    Data _ -> allM goArg args
    Literal _ -> pure True
    Prim pr -> case primWorkInfo pr of
      -- The primitive outputs a constant, whatever its arguments
      WorkConstant -> pure True
      WorkNever -> allM goArg args
      WorkIdentity _ _ -> allM goArg args
      WorkVariable -> pure (all isConstantArg args)
      WorkAlways -> pure False
    Lam _ e -> andM [go False e, allM goArg args]
    TyLam _ e -> andM [go False e, allM goArg args]
    Let (NonRec _ x) e -> andM [go False e, go False x, allM goArg args]
    Let (Rec _ xs) e -> andM [go False e, allM (go False) xs, allM goArg args]
    Case s _ [Alt _ a] -> andM [go False s, go False a, allM goArg args]
    Case e _ _ -> andM [go False e, allM goArg args]
    Cast e _ _ -> andM [go False e, allM goArg args]
    -- 'collectArgs' removes applications and ticks
    Tick _ _ -> error "isWorkFree: unexpected Tick"
    App {} -> error "isWorkFree: unexpected App"
    TyApp {} -> error "isWorkFree: unexpected TyApp"

  goArg :: HasCallStack => Either (Term x) (Type x) -> m Bool
  goArg = either (go False) (const (pure True))

  isConstantArg :: Either (Term x) (Type x) -> Bool
  isConstantArg = either isConstant (const True)

-- | Whether a term is a constant, see 'Clash.Rewrite.WorkFree.isConstant'
isConstant :: Term n -> Bool
isConstant e = case collectArgs e of
  (Data _, args) -> all (either isConstant (const True)) args
  (Prim _, args) -> all (either isConstant (const True)) args
  (Lam _ _, _) -> isClosed e
  (Literal _, _) -> True
  _ -> False

-- | Like 'isConstant', but a clock or reset is only constant if it is a
-- removed argument, see 'Clash.Rewrite.WorkFree.isConstantNotClockReset'
isConstantNotClockReset
  :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Term n -> Bool
isConstantNotClockReset tenv sc e
  | isClockOrReset tenv sc (inferCoreTypeOf tenv sc e) =
      case fst (collectArgs e) of
        Prim pr -> primName pr == primName removedArg
        _ -> False
  | otherwise = isConstant e

-- | Whether a clock, reset or enable is work free, see
-- 'Clash.Rewrite.WorkFree.isWorkFreeClockOrResetOrEnable'. 'Nothing' if the
-- term is not a clock, reset or enable.
isWorkFreeClockOrResetOrEnable
  :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Term n -> Maybe Bool
isWorkFreeClockOrResetOrEnable tenv sc e =
  let eTy = inferCoreTypeOf tenv sc e in
  if isClockOrReset tenv sc eTy || isEnable tenv sc eTy then
    case collectArgs e of
      (Prim p, _) -> Just (primName p == primName removedArg)
      -- Only local variables with a clock type are work-free. A global
      -- variable is probably backed by a clock generator, see #2845.
      (Var {}, []) -> Just True
      (GlobalVar {}, []) -> Just False
      -- Enable True/False
      (Data _, [_dom, Left (stripTicks -> Data _)]) -> Just True
      (Literal _, _) -> Just True
      _ -> Just False
  else
    Nothing

-- | A conservative version of 'isWorkFree', which also considers references
-- to clocks and resets constant, see 'Clash.Rewrite.WorkFree.isWorkFreeIsh'
isWorkFreeIsh :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Term n -> Bool
isWorkFreeIsh tenv sc e =
  case isWorkFreeClockOrResetOrEnable tenv sc e of
    Just b -> b
    Nothing -> case collectArgs e of
      (Data _, args) -> all isWorkFreeIshArg args
      (Prim pInfo, args) -> case primWorkInfo pInfo of
        -- Things like clock or reset generators always perform work
        WorkAlways -> False
        WorkVariable -> all isConstantArg args
        _ -> all isWorkFreeIshArg args
      (Lam _ _, _) -> isClosed e
      (Literal _, _) -> True
      _ -> False
 where
  isWorkFreeIshArg = either (isWorkFreeIsh tenv sc) (const True)
  isConstantArg = either isConstant (const True)
