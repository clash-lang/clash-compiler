{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The evaluator keeps global and local bindings in separate heaps. A global
-- and a local variable can share a unique, for example the local copies of a
-- recursive group of functions (see NOTE [bindings in recursive groups]).
-- The evaluator of Clash Core looked up a global variable in the local heap,
-- and found the binding of the local one.
--
-- In scoped Clash Core, locals and globals are separate namespaces: a local
-- has a raw name of its scope, a global a unique. The scoped evaluator
-- ("Clash.Core.Scoped.Evaluator") must keep them apart, also when the raw
-- name of a local is the unique of a global.
--
-- This test evaluates a global variable @f@, and an application of a free
-- local variable @h@ to @f@, with a local variable on the local heap whose
-- raw name is @f@'s unique. Neither should be affected by that local.
module GlobalLocalHeapLookup where

import Prelude

import Control.Monad (unless)
import qualified Data.IntMap.Strict as IntMap

import Clash.Core.Literal (Literal (IntLiteral))
import Clash.Core.Name (mkUnsafeSystemName, noSrcSpan)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (Binding (..), IsPrim (IsFun))
import GHC.Types.Basic (InlineSpec (NoUserInlinePrag))

import Clash.Core.Pretty (showPpr)
import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Syntax (mkGlobalId)
import Clash.Core.Scoped.Evaluator (whnf)
import Clash.Core.Scoped.Evaluator.Types (Machine (..), initMachine)
import Clash.Core.Scoped.NameMap (emptyNameMap, insertNameMap, singletonGlobalEnv)
import Clash.Core.Scoped.Subst (withFreshIdBinder)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (stripAllTicks)
import Clash.Core.Scoped.Type (mkFunTy)
import Clash.Core.Scoped.TypeEnv (mkTypeEnv)
import Clash.Core.Scoped.TysPrim (intPrimTy)
import Clash.Rewrite.Scoped.Whnf (evaluatorEnv)

mainVerilog :: IO ()
mainVerilog =
  -- The local f gets the raw name 1000, just above the watermark 999
  withFreshIdBinder (raiseWatermark 999 emptyScope) (mkUnsafeSystemName "f" 0) intPrimTy $ \sc1 fB ->
  withFreshIdBinder sc1 (mkUnsafeSystemName "h" 0) (mkFunTy intPrimTy intPrimTy) $ \sc2 hB -> do
    let fLocal = sink (tmNameOf fB)
        h = idBinderVar hB
        locals = insertNameMap fLocal (lit 2) emptyNameMap
        -- The local f is 2 on the local heap, the global f is 1
        check e expected =
          whnf env False (initMachine sc2 locals globals (IntMap.empty, 0) e) $ \m ->
            unless (aeqTm (stripAllTicks (mTerm m)) (sink expected)) $
              error ("Evaluating " <> showPpr e <> " gave " <> showPpr (mTerm m)
                       <> ", but expected " <> showPpr expected)
    unless (tmNameRaw (tmNameOf fB) == fromIntegral (globalIdUniq f)) $
      error "The raw name of the local f is not the unique of the global f"
    check (GlobalVar f) (lit 1)
    check (App h (GlobalVar f)) (App h (GlobalVar f))
 where
  f = mkGlobalId (mkUnsafeSystemName "f" 1000) intPrimTy
  lit = Literal . IntLiteral

  globals = singletonGlobalEnv f (Binding f noSrcSpan NoUserInlinePrag IsFun (lit 1) False)
  env = evaluatorEnv (mkTypeEnv UniqMap.empty)
