{-# LANGUAGE OverloadedStrings #-}

-- | The evaluator keeps global and local bindings in separate heaps. A global
-- and a local variable can share a unique, for example the local copies of a
-- recursive group of functions (see NOTE [bindings in recursive groups]).
-- Looking up a global variable in the local heap then finds the binding of
-- the local one.
--
-- This test evaluates a global variable @f@, and an application of a free
-- local variable to @f@, with a local variable sharing @f@'s unique on the
-- heap. Neither should be affected by that local.
module GlobalLocalHeapLookup where

import Prelude

import Control.Monad (unless)

import Clash.Core.Evaluator.Types (whnf')
import Clash.Core.Literal (Literal (IntLiteral))
import Clash.Core.Name (mkUnsafeSystemName, noSrcSpan)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Scoped.Convert (mkGlobalIdFromClash)
import Clash.Core.Term (Term (..), stripTicks)
import Clash.Core.Type (mkFunTy)
import Clash.Core.TysPrim (intPrimTy)
import Clash.Core.Var (mkGlobalId, mkLocalId)
import Clash.Core.VarEnv (mkInScopeSet, mkVarEnv, mkVarSet)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (Binding (..), IsPrim (IsFun))
import Clash.Util.Supply (newSupply)
import GHC.Types.Basic (InlineSpec (NoUserInlinePrag))

import Clash.GHC.Evaluator (evaluator)

mainVerilog :: IO ()
mainVerilog = do
  check (Var f) (lit 1)
  check (App (Var h) (Var f)) (App (Var h) (Var f))
 where
  f = mkGlobalId intPrimTy (mkUnsafeSystemName "f" 1000)
  fLocal = mkLocalId intPrimTy (mkUnsafeSystemName "f" 1000)
  h = mkLocalId (mkFunTy intPrimTy intPrimTy) (mkUnsafeSystemName "h" 1001)
  lit = Literal . IntLiteral

  globals = mkVarEnv
    [ (f, Binding (mkGlobalIdFromClash f) noSrcSpan NoUserInlinePrag IsFun (lit 1) False) ]
  locals = mkVarEnv [(fLocal, lit 2)]

  check e expected = do
    ids <- newSupply
    let inScope = mkInScopeSet (mkVarSet [fLocal, h])
        (_, _, result) =
          whnf' evaluator globals locals UniqMap.empty (mempty, 0) ids inScope
                False e
    unless (stripTicks result == expected) $
      error ("Evaluating " <> showPpr e <> " gave " <> showPpr result
               <> ", but expected " <> showPpr expected)
