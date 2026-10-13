{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | When the evaluator puts a let-binding on its heap, the binder must not
-- capture a variable in scope.
--
-- The evaluator of Clash Core renamed the binder to a unique from its unique
-- supply, which needn't be fresh for the variables in scope: not all uniques
-- in a term come from the supply. The evaluator
-- ("Clash.Core.Evaluator") gives the binders it allocates the name
-- just above the watermark of its scope, which is fresh by construction:
-- capture can't be expressed in Clash Core. The closest check is that the
-- machine allocates above the watermark, not at it.
--
-- This test evaluates
--
-- > let x = 1# in h x z
--
-- where @h@ and @z@ are free, and @z@ has the highest name of the scope, the
-- watermark: the name a machine that doesn't go above the watermark would
-- give a binder. The let-binding must not capture @z@: @z@ must not be bound
-- on the heap, no binder the machine allocates may have its name, and the
-- result must still refer to it. More generally, the binders the machine
-- allocates must have distinct names above the watermark.
module LetAllocationFreshness where

import Prelude

import Control.Monad (unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List

import Clash.Core.Literal (Literal (IntLiteral))
import Clash.Core.Name (mkUnsafeSystemName)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Evaluator (bindAllocated, whnf)
import Clash.Core.Evaluator.Types (Machine (..), allocTele, initMachine)
import Clash.Core.Internal.Scope (scopeWatermark)
import Clash.Core.NameMap (emptyGlobalEnv, emptyNameMap, memberNameMap)
import Clash.Core.Subst (teleIdVars, withFreshIdBinder)
import Clash.Core.Syntax
import Clash.Core.Term (mkTmApps, stripAllTicks)
import Clash.Core.Type (mkFunTy)
import Clash.Core.TypeEnv (mkTypeEnv)
import Clash.Core.TysPrim (intPrimTy)
import Clash.Core.Pretty (showPpr)
import Clash.Rewrite.Whnf (evaluatorEnv)

mainVerilog :: IO ()
mainVerilog =
  withFreshIdBinder emptyScope (mkUnsafeSystemName "h" 0) (mkFunTy intPrimTy (mkFunTy intPrimTy intPrimTy)) $ \sc1 hB ->
  withFreshIdBinder sc1 (mkUnsafeSystemName "z" 0) intPrimTy $ \(sc2 :: Scope l2) zB ->
  withFreshIdBinder sc2 (mkUnsafeSystemName "x" 0) intPrimTy $ \_ xB ->
    let h :: Term l2
        h = sink (idBinderVar hB)
        z = idBinderVar zB
        zName = tmNameOf zB
        e = Let (NonRec xB (Literal (IntLiteral 1))) (mkTmApps (sink h) [idBinderVar xB, sink z])
        env = evaluatorEnv (mkTypeEnv UniqMap.empty)
        m0 = initMachine sc2 emptyNameMap emptyGlobalEnv (IntMap.empty, 0) e
    in whnf env False m0 $ \m -> do
         let result = bindAllocated m (mTerm m)
             zRaw = tmNameRaw zName
             allocatedRaws = [tmNameRaw x | Var _ _ x <- teleIdVars (allocTele (mAlloc m))]
             failure msg = error ("Evaluating " <> showPpr e <> " gave " <> showPpr result <> ": " <> msg)
         -- The precondition: z has the highest name of the scope
         unless (zRaw == scopeWatermark sc2) (failure "z is not the watermark")
         -- z is free, so it must not be bound on the heap, nor by a binder
         -- the machine allocated
         when (memberNameMap (sink zName) (mHeapLocal m)) (failure "z is bound on the heap")
         when (zRaw `elem` allocatedRaws) (failure "a binder captures z")
         -- More generally, the binders the machine allocated have distinct
         -- names above the watermark of the scope
         unless (List.nub allocatedRaws == allocatedRaws && all (> scopeWatermark sc2) allocatedRaws)
           (failure "the allocated binders are not fresh")
         -- and the result must still refer to it
         case stripAllTicks (mTerm m) of
           App (App (Var _ _ h') _) (Var _ _ z')
             | Var _ _ hName <- sink h, h' == hName, z' == sink zName -> pure ()
           _ -> failure "it doesn't apply h to z"
