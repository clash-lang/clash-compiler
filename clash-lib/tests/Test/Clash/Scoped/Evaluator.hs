{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Helpers to test the scoped evaluator ("Clash.Core.Scoped.Evaluator") on
  terms of scoped Clash Core. The helpers that compare it with the evaluator
  of Clash Core are in "Test.Clash.Scoped.Evaluator.Differential".
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.Clash.Scoped.Evaluator
  ( -- * Evaluating
    whnfIn
  , nfDataIn
  ) where

import qualified Data.IntMap.Strict as IntMap

import Clash.Driver.Types (Binding)

import Clash.Core.Scoped.Evaluator (bindAllocated, nfData, whnf)
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Syntax (DExt, Distinct, S (VoidS), Scope, Term)
import Clash.Core.Scoped.NameMap (GlobalEnv, emptyNameMap)

-- * Evaluating

-- | Evaluate a term in a scope to WHNF, with the given environment and global
-- bindings, and whether it is the subject of a case-expression. Gives the
-- result, bound by the bindings the evaluator allocated. E.g. in a test scope
-- ('Test.Clash.Scoped.TestScope'):
--
-- > withTerm "(case 1 of { 1 -> (y :: Int); _ -> 2 }) :: Int" $ \ts e ->
-- >   assertTermIs ts "y" (whnfIn env emptyGlobalEnv False (testScope ts) e)
whnfIn
  :: Distinct n
  => Env -> GlobalEnv (Binding (Term 'VoidS)) -> Bool -> Scope n -> Term n -> Term n
whnfIn env bs isSubj = evalIn (\m0 k -> whnf env isSubj m0 k) bs

-- | Evaluate a term in a scope as far as it is data ('nfData'), see 'whnfIn'
nfDataIn
  :: Distinct n
  => Env -> GlobalEnv (Binding (Term 'VoidS)) -> Scope n -> Term n -> Term n
nfDataIn env = evalIn (nfData env)

-- | Run the evaluator of scoped Clash Core on a term in a scope, see 'whnfIn'
evalIn
  :: Distinct n
  => (forall r. Machine n n -> (forall l. DExt n l => Machine n l -> r) -> r)
  -> GlobalEnv (Binding (Term 'VoidS))
  -> Scope n
  -> Term n
  -> Term n
evalIn eval bs sc t =
  eval (initMachine sc emptyNameMap bs (IntMap.empty, 0) t) $ \m -> bindAllocated m (mTerm m)

