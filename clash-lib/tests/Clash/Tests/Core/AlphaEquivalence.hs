{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for alpha equivalence and alpha comparison of 'Term' and 'Type'
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Core.AlphaEquivalence (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Data.Text (Text)

import Clash.Core.HasFreeVars (freeVarsOf)
import Clash.Core.Name (NameSort (..), mkUnsafeName)
import Clash.Core.Subst (freshenTm)
import Clash.Core.Term (Term (..), NameMod (..), TickInfo (..))
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Type (Type (..))
import Clash.Core.Var (TyVar, Var (..))
import Clash.Core.VarEnv (eltsVarSet, mkInScopeSet, mkVarSet)
import Clash.Unique (Unique)

import Test.Clash.Rewrite (intTy, localId, parseToTermQQ)


-- | A 'TyVar' with the given human readable name and unique. 'Test.Clash.Rewrite'
-- has no equivalent of 'localId' for type variables, and 'parseToTermQQ' cannot
-- express the terms the tick tests below need.
mkTyVar :: Text -> Unique -> TyVar
mkTyVar occ uniq = TyVar (mkUnsafeName User occ uniq) uniq liftedTypeKind


-- | Assert that two terms are alpha-equivalent, and that 'Ord' agrees, in both
-- directions
assertAlphaEqual :: Term -> Term -> Assertion
assertAlphaEqual t1 t2 = do
  t1 @=? t2
  t2 @=? t1
  EQ @=? compare t1 t2
  EQ @=? compare t2 t1

-- | Assert that two terms are not alpha-equivalent, and that 'Ord' agrees. Also
-- checks that comparison in the opposite direction yields the opposite result.
assertAlphaNotEqual :: Term -> Term -> Assertion
assertAlphaNotEqual t1 t2 = do
  assertBool "t1 /= t2" (t1 /= t2)
  assertBool "t2 /= t1" (t2 /= t1)
  case compare t1 t2 of
    EQ -> assertFailure "compare == EQ"
    LT -> GT @=? compare t2 t1
    GT -> LT @=? compare t2 t1

case_arxiv_2105_02856_eq1 :: Assertion
case_arxiv_2105_02856_eq1 = assertAlphaEqual a b
 where
  a = [parseToTermQQ|let (x :: Int) = exp z in x + 7|]
  b = [parseToTermQQ|let (y :: Int) = exp z in y + 7|]

case_arxiv_2105_02856_eq2 :: Assertion
case_arxiv_2105_02856_eq2 = assertAlphaEqual a b
 where
  a = [parseToTermQQ|\(x :: Int) -> x + 7|]
  b = [parseToTermQQ|\(y :: Int) -> y + 7|]

case_arxiv_2105_02856_eq3 :: Assertion
case_arxiv_2105_02856_eq3 = assertAlphaEqual a b
 where
  a = [parseToTermQQ|\(x :: Int) -> x + y|]
  b = [parseToTermQQ|\(p :: Int) -> p + y|]

case_arxiv_2105_02856_eq4 :: Assertion
case_arxiv_2105_02856_eq4 = assertAlphaEqual a b
 where
  a = [parseToTermQQ|let (bar :: Int) = x + 1 in bar * y|]
  b = [parseToTermQQ|let (pub :: Int) = x + 1 in pub * y|]

case_arxiv_2105_02856_eq5 :: Assertion
case_arxiv_2105_02856_eq5 = assertAlphaEqual a b
 where
  a = [parseToTermQQ|\(x :: Int) -> x + t|]
  b = [parseToTermQQ|\(x :: Int) -> x + t|]

case_arxiv_2105_02856_neq1 :: Assertion
case_arxiv_2105_02856_neq1 = assertAlphaNotEqual a b
 where
  a = [parseToTermQQ|\(x :: Int) -> x + y|]
  b = [parseToTermQQ|\(q :: INt) -> q + z|]

case_arxiv_2105_02856_neq2 :: Assertion
case_arxiv_2105_02856_neq2 = assertAlphaNotEqual a b
 where
  a = [parseToTermQQ|(x :: Int) + 2|]
  b = [parseToTermQQ|(y :: INt) + 2|]

case_arxiv_2105_02856_neq3 :: Assertion
case_arxiv_2105_02856_neq3 = assertAlphaNotEqual a b
 where
  a = [parseToTermQQ|let (x :: Int) = e1 in let (y :: Int) = e2 in x + y|]
  b = [parseToTermQQ|let (y :: Int) = e2 in let (x :: Int) = e1 in x + y|]

case_shadowing_1 :: Assertion
case_shadowing_1 = do
  assertAlphaEqual a a
  assertAlphaEqual a b
  assertAlphaEqual a c

  assertAlphaEqual b a
  assertAlphaEqual b b
  assertAlphaEqual b c

  assertAlphaEqual c a
  assertAlphaEqual c b
  assertAlphaEqual c c

  assertAlphaNotEqual d a
  assertAlphaNotEqual d b
  assertAlphaNotEqual d c

  assertAlphaNotEqual a d
  assertAlphaNotEqual b d
  assertAlphaNotEqual c d
 where
  -- EQ:
  a = [parseToTermQQ|\(x :: Int) -> \(x :: Int) -> x|]
  b = [parseToTermQQ|\(x :: Int) -> \(y :: Int) -> y|]
  c = [parseToTermQQ|\(a :: Int) -> \(b :: Int) -> b|]

  -- NEQ:
  d = [parseToTermQQ|\(x :: Int) -> \(y :: Int) -> x|]

case_shadowing_2 :: Assertion
case_shadowing_2 = do
  assertAlphaEqual a a
  assertAlphaEqual a b
  assertAlphaEqual a c

  assertAlphaEqual b a
  assertAlphaEqual b b
  assertAlphaEqual b c

  assertAlphaEqual c a
  assertAlphaEqual c b
  assertAlphaEqual c c

  assertAlphaNotEqual d a
  assertAlphaNotEqual d b
  assertAlphaNotEqual d c

  assertAlphaNotEqual a d
  assertAlphaNotEqual b d
  assertAlphaNotEqual c d
 where
  -- EQ:
  a = [parseToTermQQ|\(x :: Int) -> x + (\(x :: Int) -> x * 2) 5|]
  b = [parseToTermQQ|\(x :: Int) -> x + (\(y :: Int) -> y * 2) 5|]
  c = [parseToTermQQ|\(a :: Int) -> a + (\(b :: Int) -> b * 2) 5|]

  -- NEQ:
  d = [parseToTermQQ|\(x :: Int) -> x + (\(y :: Int) -> x * 2) 5|]

case_shadowing_3 :: Assertion
case_shadowing_3 = assertAlphaNotEqual a b
 where
  a = [parseToTermQQ|let (x :: Int) = 10 in let (x :: Int) = x + 5 in x * 2|]
  b = [parseToTermQQ|let (y :: Int) = 10 in let (z :: Int) = y + 5 in z * 2|]

case_captureFreeVar :: Assertion
case_captureFreeVar = assertAlphaNotEqual a b
 where
  a = [parseToTermQQ|\(x :: Int) -> x + y|]
  b = [parseToTermQQ|\(y :: Int) -> y + y|]

-- | Free variables are compared by unique alone: their human readable names
-- are irrelevant.
case_freeVarsCompareByUnique :: Assertion
case_freeVarsCompareByUnique = do
  assertAlphaEqual a b
  assertAlphaNotEqual a c
 where
  a = [parseToTermQQ|x_1 + y|]
  b = [parseToTermQQ|q_1 + y|]
  c = [parseToTermQQ|x_2 + y|]

-- | A bound variable is never equal to a free variable, even if the free
-- variable has the same unique as the binder on the other side.
case_boundVersusFree :: Assertion
case_boundVersusFree = assertAlphaNotEqual a b
 where
  a = [parseToTermQQ|\(x_1 :: Int) -> x_1|]
  b = [parseToTermQQ|\(y_2 :: Int) -> x_1|]

-- | Renaming a binder such that it captures a free variable of the other term
-- does not make the terms equal, even when uniques line up exactly.
case_captureWithExplicitUniques :: Assertion
case_captureWithExplicitUniques = assertAlphaNotEqual a b
 where
  a = [parseToTermQQ|\(z_5 :: Int) -> y_2|]
  b = [parseToTermQQ|\(y_2 :: Int) -> y_2|]

-- | Swapping binder names consistently is fine, but swapping the references
-- without swapping the binders is not.
case_binderSwap :: Assertion
case_binderSwap = do
  assertAlphaEqual a b
  assertAlphaNotEqual a c
 where
  a = [parseToTermQQ|\(x :: Int) -> \(y :: Int) -> x + y|]
  b = [parseToTermQQ|\(y :: Int) -> \(x :: Int) -> y + x|]
  c = [parseToTermQQ|\(x :: Int) -> \(y :: Int) -> y + x|]

-- | Lambda binders must have alpha-equivalent types.
case_lambdaBinderTypeSignificant :: Assertion
case_lambdaBinderTypeSignificant = assertAlphaNotEqual a b
 where
  a = [parseToTermQQ|\(x :: Int) -> x|]
  b = [parseToTermQQ|\(x :: Bool) -> x|]

-- | An inner binder may shadow an outer binder by unique; references resolve
-- to the innermost one.
case_sameUniqueShadowing :: Assertion
case_sameUniqueShadowing = assertAlphaEqual a b
 where
  a = [parseToTermQQ|\(x_1 :: Int) -> \(y_1 :: Int) -> y_1|]
  b = [parseToTermQQ|\(p_2 :: Int) -> \(q_3 :: Int) -> q_3|]

-- | Mutually recursive bindings can be renamed, but the binding a body refers
-- to is significant.
case_letrecMutualRecursion :: Assertion
case_letrecMutualRecursion = do
  assertAlphaEqual a b
  assertAlphaNotEqual a c
 where
  a = [parseToTermQQ|let { (x :: Int) = y; (y :: Int) = x } in x|]
  b = [parseToTermQQ|let { (p :: Int) = q; (q :: Int) = p } in p|]
  c = [parseToTermQQ|let { (p :: Int) = q; (q :: Int) = p } in q|]

-- | Letrecs with a different number of bindings are never equal; in
-- particular the positional comparison must not silently drop the extra
-- binding.
case_letrecBindingCountSignificant :: Assertion
case_letrecBindingCountSignificant = assertAlphaNotEqual a b
 where
  a = [parseToTermQQ|let { (x :: Int) = 1 } in x|]
  b = [parseToTermQQ|let { (x :: Int) = 1; (y :: Int) = 1 } in x|]

-- | Self-referencing letrec bindings can be renamed like any other binding.
case_letrecSelfReference :: Assertion
case_letrecSelfReference = assertAlphaEqual a b
 where
  a = [parseToTermQQ|let (x_1 :: Int) = x_1 in x_1|]
  b = [parseToTermQQ|let (y_2 :: Int) = y_2 in y_2|]

-- | Terms built from different constructors are never equal, even when they
-- would evaluate to the same value.
case_differentConstructors :: Assertion
case_differentConstructors = assertAlphaNotEqual a b
 where
  a = [parseToTermQQ|\(x :: Int) -> x|]
  b = [parseToTermQQ|let (x :: Int) = x in x|]

-- | The Core in 'Attributes' lives in the scope enclosing the tick, so it is
-- compared under the enclosing renaming environment. Judging it in an empty
-- environment would compare an occurrence of a bound variable by its raw
-- unique, making these two unequal.
case_tickAttributesSeesEnclosingBinders :: Assertion
case_tickAttributesSeesEnclosingBinders = assertAlphaEqual a b
 where
  a = attributed (localId User "x" 100 intTy)
  b = attributed (localId User "y" 200 intTy)
  attributed v = Lam v (Tick (Attributes intTy (Var v)) (Var v))

-- | Free variables in 'Attributes' are still significant: only binders are
-- renamed away.
case_tickAttributesDistinguishesFreeVars :: Assertion
case_tickAttributesDistinguishesFreeVars = assertAlphaNotEqual a b
 where
  x = localId User "x" 100 intTy
  a = attributed x
  b = attributed (localId User "y" 200 intTy)
  attributed v = Lam x (Tick (Attributes intTy (Var v)) (Var x))

-- | The 'Type' in 'NameMod' lives in the scope enclosing the tick, so it is
-- compared under the enclosing renaming environment, just like the 'Term' in
-- 'Attributes'. This is the shape @Clash.GHC.GHC2Core.nameModTerm@ builds for
-- @prefixName@ and friends:
-- @/\\nm. \\x -> Tick (NameMod PrefixName (VarTy nm)) x@.
case_tickNameModSeesEnclosingBinders :: Assertion
case_tickNameModSeesEnclosingBinders = assertAlphaEqual a b
 where
  a = nameModTerm (mkTyVar "nm" 300)
  b = nameModTerm (mkTyVar "nm" 400)
  x = localId User "x" 100 intTy
  nameModTerm tv =
    TyLam tv (Lam x (Tick (NameMod PrefixName (VarTy tv)) (Var x)))

-- | 'freshenTm' gives every binder a fresh unique. The Core in 'Attributes'
-- lives in the scope enclosing the tick, so an occurrence of the binder inside
-- it has to be renamed along with the body; leaving it alone turns it into a
-- free variable pointing at the old unique.
case_freshenTmRenamesInsideAttributes :: Assertion
case_freshenTmRenamesInsideAttributes =
  case freshened of
    Lam x' (Tick (Attributes _ (Var attributed)) _) -> do
      assertBool "binder was freshened" (varUniq x' /= varUniq x)
      varUniq x' @=? varUniq attributed
      [] @=? eltsVarSet (freeVarsOf freshened)
    other -> assertFailure ("unexpected shape: " <> show other)
 where
  x = localId User "x" 100 intTy
  term = Lam x (Tick (Attributes intTy (Var x)) (Var x))
  -- 'x' is already in scope, so 'freshenTm' has to rename the binder
  (_, freshened) = freshenTm (mkInScopeSet (mkVarSet [x])) term

tests :: TestTree
tests = $(testGroupGenerator)
