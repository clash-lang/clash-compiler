{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Netlist.Util"
-}

module Clash.Tests.Netlist.Util (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Bind (..), Term (..), mkApps, mkLams)
import Clash.Netlist.Util (splitNormalized)
import Clash.Unique (Unique)

import Test.Clash.Rewrite (assertAlphaEq, globalId, intFunTy, intId)

-- | Apply the global function with the given name and unique to the given
-- arguments
app :: String -> Unique -> [Term] -> Term
app f u args = mkApps (Var (globalId User f u (intFunTy (length args)))) (map Left args)

-- | Run 'splitNormalized', and rebuild its result as a term in which all
-- let-bindings form a single recursive group, as the netlist treats them
splitNormalizedRebuild :: Term -> IO Term
splitNormalizedRebuild e = case splitNormalized mempty e of
  Right (args, binds, res) -> pure (mkLams (Letrec binds (Var res)) args)
  Left err -> assertFailure err

-- | The binder of a non-recursive let-binding is not in scope of its
-- right-hand side, which here refers to the argument @x@:
--
-- @
-- \\x -> let x = g x in x
-- @
--
-- Merging that binding into the bindings of the netlist as is, turns it into
-- the loop @x = g x@.
splitNormalizedNonRec :: Assertion
splitNormalizedNonRec = do
  actual <- splitNormalizedRebuild
    (Lam x (Let (NonRec x (app "g" 10 [Var x])) (Var x)))
  assertAlphaEq
    (Lam x (Letrec [(x', app "g" 10 [Var x])] (Var x')))
    actual
 where
  x = intId User "x" 0
  x' = intId User "x'" 1

-- | Like 'splitNormalizedNonRec', but for a nested let-expression, whose
-- binding refers to a binding of the let-expression it is nested in:
--
-- @
-- \\a -> letrec y = h a in let y = g y in y
-- @
--
-- Merging the nested let-expression as is gives @letrec y = h a; y = g y in
-- y@, binding @y@ twice.
splitNormalizedNestedNonRec :: Assertion
splitNormalizedNestedNonRec = do
  actual <- splitNormalizedRebuild
    (Lam a (Letrec [(y, app "h" 10 [Var a])]
             (Let (NonRec y (app "g" 11 [Var y])) (Var y))))
  assertAlphaEq
    (Lam a (Letrec [(y, app "h" 10 [Var a]), (y', app "g" 11 [Var y])] (Var y')))
    actual
 where
  a = intId User "a" 0
  y = intId User "y" 1
  y' = intId User "y'" 2

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Netlist.Util"
    [ testCase "splitNormalized renames a NonRec binder its right-hand side refers to"
        splitNormalizedNonRec
    , testCase "splitNormalized renames a nested NonRec binder its right-hand side refers to"
        splitNormalizedNestedNonRec
    ]
