{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize"

  Note that there is no test for #448 ("Use proper InScopeSet in
  flattenCallTree"): since commit
  https://github.com/clash-lang/clash-compiler/commit/d997cb6314372803c036da1bba7f1807706d0728
  ("Disambiguate local and global ids"), 'flattenCallTree' substitutes globals
  only, whose replacements are closed terms, so its in-scope set no longer
  matters for correctness.
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Normalize (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import Clash.Core.Subst (unsafeSubstTm)
import Clash.Core.Term (Term (..))
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (emptyVarEnv, lookupVarEnv, mkVarEnv)
import Clash.Driver.Types (Binding (..))
import Clash.Normalize (cleanupGraph)
import Clash.Rewrite.Types (RewriteState (..))

import Test.Clash.Rewrite
  ( assertWellScoped, globalId, intFunTy, mkBindingMap, nameToUnique
  , parseToTermQQ, runRewriteTest, showPprU )

-- | A global binder, as parsed from @<nm>_G@, of type @Int -> .. -> Int@ with
-- the given number of arguments
globalFun :: String -> Int -> Id
globalFun nm n = globalId User nm (nameToUnique nm) (intFunTy n)

-- | Give global variables in a term the types of the given binders.
-- 'Test.Clash.Rewrite.parseToTerm' cannot parse function types.
withTypes :: [Id] -> Term -> Term
withTypes globals = unsafeSubstTm (mkVarEnv [(i, Var i) | i <- globals]) emptyVarEnv

-- | Run 'cleanupGraph' with the given top entity and (normalized) binders,
-- return the cleaned up term of the top entity. All global variables in the
-- binders' terms get the types of the given globals.
cleanup :: [Id] -> Id -> [(Id, Term)] -> IO Term
cleanup globals top bndrs = do
  let bm = mkBindingMap [(i, withTypes globals tm) | (i, tm) <- bndrs]
  (bm1, _, _) <- runRewriteTest def def{_bindings = bm} (cleanupGraph top bm)
  case lookupVarEnv top bm1 of
    Just b -> pure (bindingTerm b)
    Nothing -> assertFailure ("cleanupGraph dropped " <> showPprU top)

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize"
  [ -- #445: when inlining a wrapper @f@ into its caller, 'flattenNode' strips
    -- the arguments @f@ passes on to the function it wraps. It did not check
    -- that the result binder of @f@ was not used in that application, so
    -- @f = \x y -> let r0 = g r0 x y in r0@ became @g r0@, with @r0@ free.
    -- https://github.com/clash-lang/clash-compiler/pull/445
    testCase "flattenNode keeps LHS of recursive binders (#445)" $ do
      let f = globalFun "f" 2
          g = globalFun "g" 3
          h = globalFun "h" 2
      actual <- cleanup [f, g, h] h
        [ ( h
          , [parseToTermQQ|
              \(a_10 :: Int) (b_11 :: Int) ->
                let r1_12 :: Int
                    r1_12 = f_G a_10 b_11
                in r1_12
            |] )
        , ( f
          , [parseToTermQQ|
              \(x_20 :: Int) (y_21 :: Int) ->
                let r0_22 :: Int
                    r0_22 = g_G r0_22 x_20 y_21
                in r0_22
            |] )
        ]
      assertWellScoped actual

    -- 'stripArgs' used to only check whether the remaining (unstripped)
    -- arguments were /equal/ to one of the lambda-bound variables, not whether
    -- they /mentioned/ one. So @f = \x -> let r = g (p x) x in r@ became
    -- @g (p x)@, with @x@ free. Such non-ANF arguments occur for higher-order
    -- primitives.
    -- https://github.com/clash-lang/clash-compiler/commit/bb1ea69be69d5497bb9fb01a287adb607844a70d
  , testCase "flattenNode doesn't strip args when they are mentioned" $ do
      let f = globalFun "f" 1
          g = globalFun "g" 2
          h = globalFun "h" 1
          p = globalFun "p" 1
      actual <- cleanup [f, g, h, p] h
        [ ( h
          , [parseToTermQQ|
              \(a_10 :: Int) ->
                let r1_12 :: Int
                    r1_12 = f_G a_10
                in r1_12
            |] )
        , ( f
          , [parseToTermQQ|
              \(x_20 :: Int) ->
                let r0_22 :: Int
                    r0_22 = g_G (p_G x_20) x_20
                in r0_22
            |] )
        ]
      assertWellScoped actual
  ]
