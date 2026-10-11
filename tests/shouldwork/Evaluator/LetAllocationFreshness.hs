{-# LANGUAGE OverloadedStrings #-}

-- | When the evaluator puts a let-binding on its heap, it renames the binder
-- to a unique from its unique supply. That unique must not belong to a
-- variable in scope. Not all uniques in a term come from the supply, e.g.
-- those 'Clash.Core.VarEnv.uniqAway' derives, so checking only the heap isn't
-- enough.
--
-- This test evaluates
--
-- > let x = 1# in h x z
--
-- where @h@ and @z@ are free, and @z@ has the unique the supply hands out
-- next. The let-binding must not capture @z@.
module LetAllocationFreshness where

import Prelude

import Control.Monad (unless)

import Clash.Core.Evaluator.Types (whnf')
import Clash.Core.Literal (Literal (IntLiteral))
import Clash.Core.Name (mkUnsafeSystemName)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Term (Bind (NonRec), Term (..), mkApps, stripTicks)
import Clash.Core.Type (mkFunTy)
import Clash.Core.TysPrim (intPrimTy)
import Clash.Core.Var (mkLocalId)
import Clash.Core.VarEnv (emptyVarEnv, lookupVarEnv, mkInScopeSet, mkVarSet)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util.Supply (freshId, newSupply)

import Clash.GHC.Evaluator (evaluator)

mainVerilog :: IO ()
mainVerilog = do
  ids <- newSupply
  let (u, _) = freshId ids
      x = mkLocalId intPrimTy (mkUnsafeSystemName "x" (u + 1))
      z = mkLocalId intPrimTy (mkUnsafeSystemName "z" u)
      h = mkLocalId (mkFunTy intPrimTy (mkFunTy intPrimTy intPrimTy))
                    (mkUnsafeSystemName "h" (u + 2))
      lit = Literal (IntLiteral 1)
      e = Let (NonRec x lit) (mkApps (Var h) [Left (Var x), Left (Var z)])
      inScope = mkInScopeSet (mkVarSet [h, z])
      (_, heap, result) =
        whnf' evaluator emptyVarEnv emptyVarEnv UniqMap.empty (mempty, 0) ids
              inScope False e
  -- 'z' is free, so it must not be bound on the heap, and the result must
  -- still refer to it
  case (lookupVarEnv z heap, stripTicks result) of
    (Nothing, App (App (Var h') _) (Var z'))
      | h' == h, z' == z -> pure ()
    _ -> error ("Evaluating " <> showPpr e <> " gave " <> showPpr result
                  <> ", which binds or replaces 'z'")
