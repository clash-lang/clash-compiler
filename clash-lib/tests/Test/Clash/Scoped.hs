{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities to write unit tests on scoped Clash Core ("Clash.Core.Scoped").

  Most tests write their inputs as Clash Core, e.g. with
  'Test.Clash.Rewrite.parseToTermQQ', and convert them. Note that conversion
  deshadows, so a converted input never has binders that clash with the
  scope. To test how a function handles such binders, convert the part of the
  input that has them in a smaller scope, and 'sink' it into a scope with more
  variables: e.g. into the scope of 'extendSunk', which contains 'sunkVar',
  whose unique is above the names conversion gave the binders. Sinking allows
  binders to clash, so this is the situation that functions going under
  binders must handle: they have to rename them.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.Clash.Scoped
  ( -- * Converting
    withFreeVarsOf
  , roundTrip
  , extendSunk
  , sunkVar
    -- * Assertions
  , assertScopedAlphaEq
  , assertScopedTyAlphaEq
  ) where

import qualified Control.Lens as Lens
import qualified Data.List as List
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (Assertion)

import qualified Clash.Core.FreeVars as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite (assertAlphaEq, intId)

-- | A conversion environment with the free variables of the given Clash terms
-- (both type and term variables), which keep their uniques
withFreeVarsOf :: [C.Term] -> (forall n. Distinct n => ConvEnv n -> r) -> r
withFreeVarsOf tms = withConvEnv tvs ids
 where
  tvs = List.nubBy sameUniq (concatMap (Lens.toListOf C.termFreeTyVars) tms)
  ids = List.nubBy sameUniq (concatMap (Lens.toListOf C.freeLocalIds) tms)

  sameUniq :: C.Var a -> C.Var a -> Bool
  sameUniq v1 v2 = C.varUniq v1 == C.varUniq v2

-- | Convert a Clash term to scoped Core and back, in the scope of its free
-- variables
roundTrip :: HasCallStack => C.Term -> C.Term
roundTrip tm = withFreeVarsOf [tm] $ \env -> toClashTerm (fromClashTerm env tm)

-- | A variable whose unique is above the names conversion gives the binders of
-- the inputs of the tests, see 'extendSunk'
sunkVar :: C.Id
sunkVar = intId C.System "sunk" 1000000

-- | Extend a conversion environment with 'sunkVar' and the given variables.
-- A value converted in the original environment can be 'sink'ed into the
-- extended one, where all of its binders may clash with the scope: those of the
-- extended scope are at most the unique of 'sunkVar'.
extendSunk
  :: Distinct n
  => ConvEnv n
  -> [C.TyVar]
  -> [C.Id]
  -> (forall l. DExt n l => ConvEnv l -> r)
  -> r
extendSunk env tvs ids = extendConvEnv env tvs (sunkVar : ids)

-- | Assert that a scoped term, converted to Clash, is alpha-equivalent to a
-- Clash term
assertScopedAlphaEq :: HasCallStack => C.Term -> Term n -> Assertion
assertScopedAlphaEq expected actual = assertAlphaEq expected (toClashTerm actual)

-- | Assert that a scoped type, converted to Clash, is alpha-equivalent to a
-- Clash type
assertScopedTyAlphaEq :: HasCallStack => C.Type -> Type n -> Assertion
assertScopedTyAlphaEq expected actual = assertAlphaEq expected (toClashType actual)
