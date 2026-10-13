{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.DEC": regression tests,
  and tests of the cases of DEC.

  The regression tests are about binders in the alternatives that shadow, or
  are captured by, variables outside of them. In scoped Clash Core, binders
  don't shadow, and DEC can only move the parts of an application out of the
  alternatives that don't refer to their binders, so these bugs can't be
  written there. 'assertRewritesTo' runs every test in the scope of the free
  variables of its input, and in a scope that the binders of the input clash
  with.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.DEC (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List as List
import Data.String (fromString)

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Basic as C (IsMultiPrim (SingleResult), WorkInfo (WorkVariable))
import Clash.Core.Scoped.Syntax (mkPrimInfo)
import qualified Clash.Core.TyCon as C
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Core.Scoped.Syntax
  (Alt (..), GlobalId, Pat (DefaultPat), PrimInfo, PrimUnfolding (NoUnfolding), Term (Case))
import qualified Clash.Normalize.Scoped.Transformations.DEC as Scoped
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Scoped (Rewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteEnv, RewriteState (..))

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, ghcName, kindedTyVarD, parseTestTermWith, strQQ, testAlgTyCon
  , testDataCon, testGlobalId, testTyConMap, tupleTyCon, varD, withTupleTyCons, withTyConMap )
import Test.Clash.Scoped
  ( ScopeDecl, assertCaptureRewritesTo, assertNoRewrite, assertRewritesTo, closedType
  , mkGlobalBindings, termIn )

-- * Definitions

-- | @MkEx :: forall n. n -> Ex@ and @MkT :: forall n. n -> T@, constructors
-- with an existential type variable
mkEx, mkT :: C.DataCon
mkEx = testDataCon "MkEx" 1001 1 [] [kindedTyVarD "n" "Type"] ["n"] "Ex"
mkT = testDataCon "MkT" 101 1 [] [kindedTyVarD "n" "Type"] ["n"] "T"

-- | The global functions of the tests
globals :: [GlobalId]
globals =
  [ fReg, fAgr
  , testGlobalId "k_G33" "Int -> Int -> Int"
  , testGlobalId "h_G46" "forall (a :: Type). a -> Int"
  , testGlobalId "g_G211" "Int -> Int"
  , testGlobalId "k_G212" "Int -> Int -> Int"
  , testGlobalId "h_G213" "forall (a :: Type). a -> Int"
  , testGlobalId "hof_G215" "(Int -> Int) -> Int -> Int"
  , testGlobalId "fI_G245" "GHC.Num.Integer.Integer -> GHC.Num.Integer.Integer" ]

-- | Global functions @f :: Int -> Int@, with body @\\a -> a@: of the
-- regression tests, and of the others
fReg, fAgr :: GlobalId
fReg = testGlobalId "f_G10" "Int -> Int"
fAgr = testGlobalId "f_G210" "Int -> Int"

-- | The primitives of the tests:
--
-- * @integerMul@ and @integerQuot@, interesting unless an argument is a power
--   of two
-- * a higher-order primitive @hoPrim :: (Integer -> Integer) -> Integer ->
--   Integer@, which is interesting if its function argument is
-- * a primitive @plainPrim :: Integer -> Integer@, which isn't interesting
-- * @Clash.Sized.Internal.Unsigned.fromInteger# :: forall n. Natural ->
--   Integer -> Integer@ (with a simplified result type)
prims :: [PrimInfo]
prims =
  [ prim "GHC.Num.Integer.integerMul" (integerFunTy 2)
  , prim "GHC.Num.Integer.integerQuot" (integerFunTy 2)
  , prim "hoPrim" ("(" <> integerFunTy 1 <> ") -> " <> integerFunTy 1)
  , prim "plainPrim" (integerFunTy 1)
  , prim "Clash.Sized.Internal.Unsigned.fromInteger#"
      ("forall (n :: Nat). GHC.Num.Natural.Natural -> " <> integerFunTy 1) ]
 where
  prim nm ty = mkPrimInfo (ghcName nm) (closedType (fromString ty)) C.WorkVariable C.SingleResult NoUnfolding
  integerFunTy :: Int -> String
  integerFunTy n = List.intercalate " -> " (replicate (n + 1) "GHC.Num.Integer.Integer")

tm :: String -> TestTerm
tm = parseTestTermWith
  (TestDefs ([mkEx, mkT] <> concatMap (C.tyConDataCons . tupleTyCon) [2, 3]) prims globals)

-- | The type constructors of 'testTyConMap', @T@, @Ex@ and tuples
env :: RewriteEnv
env = withTupleTyCons 3 (withTyConMap tcm def)
 where
  tcm = foldr (\tc -> UniqMap.insert (C.tyConName tc) tc) testTyConMap
          [testAlgTyCon "T" [] [mkT], testAlgTyCon "Ex" [] [mkEx]]

-- | A state with the bindings of the global functions @f@
st :: RewriteState NormalizeState
st = def
  { _bindings = mkGlobalBindings [(f, "\\(a :: Int) -> a") | f <- [fReg, fAgr]]
  , _globalHeap = (mempty, 0) }

-- | Assert that DEC rewrites a term to the expected one, see
-- 'assertRewritesTo'
decTo :: [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
decTo = assertRewritesTo env st decS

decS :: Rewrite NormalizeState
decS = $(asRewriteQ Scoped.disjointExpressionConsolidation)

-- | Assert that DEC leaves a term as it is
decKeeps :: [ScopeDecl] -> TestTerm -> Assertion
decKeeps = assertNoRewrite env st $(asRewriteQ Scoped.disjointExpressionConsolidation)

tests :: TestTree
tests =
  testGroup "Clash.Tests.Normalize.Scoped.Transformations.DEC"
    [ testGroup "regression tests"
      [ -- Clash drops casts, so after matching on a GADT, an argument can have
        -- a type that mentions an existential type variable where the
        -- function expects another type: here @v :: n@ for @f :: Int -> Int@.
        -- DEC used to give the let-binder of the lifted argument of @f@ the
        -- type of the argument of the first application, @n@, which leaves
        -- @n@ free outside of the pattern that binds it.
        testCase "DEC gives lifted arguments a type in scope" $
          decTo [varD "s_30" "Int", varD "x_31" "Int", varD "e_35" "Ex"]
            (tm [strQQ|
              (case s_30 of
                { _ -> (case e_35 of { MkEx (n :: Type) (v :: n) -> f_G10 v }) :: Int
                ; 1 -> f_G10 x_31
                }) :: Int
            |])
            (tm [strQQ|
              let
                { (cfout_1000036 :: Int) = let
                    { (cfarg0_37 :: Int) = (case s_30 of
                        { _ -> (case e_35 of { MkEx (n :: Type) (v :: n) -> v }) :: Int
                        ; 1 -> x_31
                        }) :: Int
                    } in f_G10 cfarg0_37
                } in (case s_30 of
                { _ -> (case e_35 of { MkEx (n :: Type) (v :: n) -> cfout_1000036 }) :: Int
                ; 1 -> cfout_1000036
                }) :: Int
            |])
      , -- DEC substitutes a reference to the lifted @f_out@ for every
        -- application of @f@ in the alternatives, which puts that reference
        -- under the binders of the alternatives. It used to pick the unique of
        -- @f_out@ avoiding only the variables in scope of the case-expression,
        -- not the binders inside the alternatives, so a let-binder in an
        -- alternative could capture the reference.
        -- https://github.com/clash-lang/clash-compiler/pull/1317
        -- https://github.com/clash-lang/clash-compiler/commit/959ee9072fdd0c5deaf91f5e82adc2c23b0f8e16
        testCase "DEC does not capture lifted expressions (#1317)" $
          decTo [varD "s_30" "Int", varD "x_31" "Int", varD "y_32" "Int"]
            (tm "(case s_30 of { _ -> f_G10 y_32; 1 -> let { (b :: Int) = 3 } in k_G33 b (f_G10 x_31) }) :: Int")
            (tm [strQQ|
              let
                { (fout :: Int) = let { (farg :: Int) = (case s_30 of { _ -> y_32; 1 -> x_31 }) :: Int } in f_G10 farg
                } in (case s_30 of { _ -> fout; 1 -> let { (b :: Int) = 3 } in k_G33 b fout }) :: Int
            |])
        -- Both alternatives apply the /local/ function @g@, which they
        -- let-bind themselves. DEC used to consider every local variable in
        -- scope of an application interesting to lift, including those bound
        -- inside the alternatives, and lifted @g@ out of the alternatives that
        -- bind it.
      , testCase "DEC does not lift local functions bound in the alternatives" $
          decKeeps [varD "s_30" "Int", varD "x_31" "Int", varD "y_32" "Int"]
            (tm [strQQ|
              (case s_30 of
                { _ -> let { (g :: Int -> Int) = f_G10 } in g y_32
                ; 1 -> let { (g :: Int -> Int) = f_G10 } in g x_31
                }) :: Int
            |])
        -- DEC used to consider type arguments shareable as long as they were
        -- equal, without checking whether they mention type variables bound
        -- by the patterns of the alternatives. It lifted @h \@n@ out of the
        -- case-expression that binds @n@.
      , testCase "DEC does not share type arguments bound in the alternatives" $
          decKeeps [varD "s_30" "Int", varD "t_42" "T"]
            (tm [strQQ|
              (case s_30 of
                { _ -> (case t_42 of { MkT (n :: Type) (v :: n) -> h_G46 @n v }) :: Int
                ; 1 -> (case t_42 of { MkT (n :: Type) (w :: n) -> h_G46 @n w }) :: Int
                }) :: Int
            |])
        -- DEC used to consider arguments shareable if all their free variables
        -- are in scope of the case-expression. The pattern-bound @x@ had the
        -- same unique as the @x@ in scope, so DEC shared @f x@,
        -- making the second alternative refer to the wrong @x@.
      , testCase "DEC does not share arguments bound in the alternatives" $ do
          decTo [varD "s_30" "Int", varD "x_31" "Int", varD "p_47" "Pair"]
            (tm [strQQ|
              (case s_30 of
                { _ -> f_G10 x_31
                ; 1 -> (case p_47 of { MkPair (x :: Int) (z :: Int) -> f_G10 x }) :: Int
                }) :: Int
            |])
            (tm [strQQ|
              let
                { (cfout_48 :: Int) = let
                    { (cfarg0_49 :: Int) = (case s_30 of
                        { _ -> x_31
                        ; 1 -> (case p_47 of { MkPair (x :: Int) (z :: Int) -> x }) :: Int
                        }) :: Int
                    } in f_G10 cfarg0_49
                } in (case s_30 of
                { _ -> cfout_48
                ; 1 -> (case p_47 of { MkPair (x :: Int) (z :: Int) -> cfout_48 }) :: Int
                }) :: Int
            |])
          -- The pattern binder @x@ has the name of the free @x@
          assertCaptureRewritesTo env st decS [varD "s_30" "Int", varD "p_47" "Pair"]
            (tm [strQQ|
              (case s_30 of
                { _ -> 0
                ; 1 -> (case p_47 of { MkPair (x :: Int) (z :: Int) -> f_G10 x }) :: Int
                }) :: Int
            |])
            [varD "x_31" "Int"]
            (\ts -> \case
              Case subj ty (_ : alts) -> Case subj ty (Alt DefaultPat (termIn ts (tm "f_G10 x_31")) : alts)
              e -> e)
            (tm [strQQ|
              let
                { (cfout_48 :: Int) = let
                    { (cfarg0_49 :: Int) = (case s_30 of
                        { _ -> x_31
                        ; 1 -> (case p_47 of { MkPair (x :: Int) (z :: Int) -> x }) :: Int
                        }) :: Int
                    } in f_G10 cfarg0_49
                } in (case s_30 of
                { _ -> cfout_48
                ; 1 -> (case p_47 of { MkPair (x :: Int) (z :: Int) -> cfout_48 }) :: Int
                }) :: Int
            |])
      ]
  , testGroup "cases"
      [ testGroup "heads"
          [ testCase "a global function in two alternatives" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm "(case s_200 of { _ -> f_G210 x_202; 1 -> f_G210 y_203; 2 -> g_G211 x_202 }) :: Int")
                (tm [strQQ|
                  let
                    { (cfout_204 :: Int) = let
                        { (cfarg0_205 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203 }) :: Int
                        } in f_G210 cfarg0_205
                    } in (case s_200 of { _ -> cfout_204; 1 -> cfout_204; 2 -> g_G211 x_202 }) :: Int
                |])
          , testCase "a global function in one alternative" $
              decKeeps [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm "(case s_200 of { _ -> f_G210 x_202; 1 -> g_G211 y_203 }) :: Int")
          , testCase "a local function of the context" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int", varD "q_216" "Int -> Int"]
                (tm "(case s_200 of { _ -> q_216 x_202; 1 -> q_216 y_203 }) :: Int")
                (tm [strQQ|
                  let
                    { (cqout_217 :: Int) = let
                        { (cqarg0_218 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203 }) :: Int
                        } in q_216 cqarg0_218
                    } in (case s_200 of { _ -> cqout_217; 1 -> cqout_217 }) :: Int
                |])
          , testCase "several functions, applied to each other" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm [strQQ|
                  (case s_200 of { _ -> f_G210 (g_G211 x_202); 1 -> f_G210 (g_G211 y_203); 2 -> g_G211 y_203 }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (cfout_204 :: Int) = let
                        { (cfarg0_206 :: Int) = (case s_200 of { _ -> cgout_205; 1 -> cgout_205 }) :: Int
                        } in f_G210 cfarg0_206
                    ; (cgout_205 :: Int) = let
                        { (cgarg0_206 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203; 2 -> y_203 }) :: Int
                        } in g_G211 cgarg0_206
                    } in (case s_200 of { _ -> cfout_204; 1 -> cfout_204; 2 -> cgout_205 }) :: Int
                |])
          , testCase "a function applied twice in one alternative" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm "(case s_200 of { _ -> f_G210 (f_G210 x_202); 1 -> f_G210 y_203 }) :: Int")
                (tm [strQQ|
                  let
                    { (cfout_204 :: Int) = let
                        { (cfarg0_205 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203 }) :: Int
                        } in f_G210 cfarg0_205
                    } in (case s_200 of { _ -> f_G210 cfout_204; 1 -> cfout_204 }) :: Int
                |])
          , testCase "NoDeDup" $
              decKeeps [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm [strQQ|
                  (case s_200 of { _ -> Core.noDeDup (f_G210 x_202); 1 -> Core.noDeDup (f_G210 y_203) }) :: Int
                |])
          , testCase "a tick around one application" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm "(case s_200 of { _ -> Core.deDup (f_G210 x_202); 1 -> f_G210 y_203 }) :: Int")
                (tm [strQQ|
                  let
                    { (cfout_204 :: Int) = let
                        { (cfarg0_205 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203 }) :: Int
                        } in f_G210 cfarg0_205
                    } in (case s_200 of { _ -> cfout_204; 1 -> cfout_204 }) :: Int
                |])
          , testCase "a function in the subject of a nested case-expression" $
              decTo [varD "s_200" "Int", varD "t_201" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm [strQQ|
                  (case s_200 of { _ -> f_G210 x_202; 1 -> (case f_G210 y_203 of { _ -> f_G210 t_201 }) :: Int }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (cfout_204 :: Int) = let
                        { (cfarg0_205 :: Int) = (case s_200 of { _ -> x_202; 1 -> t_201 }) :: Int
                        } in f_G210 cfarg0_205
                    } in (case s_200 of
                    { _ -> cfout_204
                    ; 1 -> (case f_G210 y_203 of { _ -> cfout_204 }) :: Int
                    }) :: Int
                |])
          ]
      , testGroup "arguments"
          [ testCase "two arguments that differ, through a tuple" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm "(case s_200 of { _ -> k_G212 x_202 y_203; 1 -> k_G212 y_203 x_202 }) :: Int")
                (tm [strQQ|
                  let
                    { (ckout_204 :: Int) = let
                        { (cktupIn_205 :: Tup2_3502 Int Int) = (case s_200 of
                            { _ -> MkTup2 @Int @Int x_202 y_203
                            ; 1 -> MkTup2 @Int @Int y_203 x_202
                            }) :: Tup2_3502 Int Int
                        ; (ckarg0_206 :: Int) = (case cktupIn_205 of { MkTup2 (csel :: Int) (cwild_209 :: Int) -> csel }) :: Int
                        ; (ckarg1_207 :: Int) = (case cktupIn_205 of { MkTup2 (cwild_208 :: Int) (csel :: Int) -> csel }) :: Int
                        } in k_G212 ckarg0_206 ckarg1_207
                    } in (case s_200 of { _ -> ckout_204; 1 -> ckout_204 }) :: Int
                |])
          , testCase "three arguments that differ, through a tuple" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int", varD "z_204" "Int"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> k_G212 (k_G212 x_202 y_203) (k_G212 z_204 x_202)
                    ; 1 -> k_G212 (g_G211 y_203) (f_G210 z_204)
                    }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (ckout_205 :: Int) = let
                        { (cktupIn_206 :: Tup2_3502 Int Int) = (case s_200 of
                            { _ -> MkTup2 @Int @Int x_202 y_203
                            ; 1 -> MkTup2 @Int @Int (g_G211 y_203) (f_G210 z_204)
                            }) :: Tup2_3502 Int Int
                        ; (ckarg0_207 :: Int) = (case cktupIn_206 of { MkTup2 (csel :: Int) (cwild_210 :: Int) -> csel }) :: Int
                        ; (ckarg1_208 :: Int) = (case cktupIn_206 of { MkTup2 (cwild_209 :: Int) (csel :: Int) -> csel }) :: Int
                        } in k_G212 ckarg0_207 ckarg1_208
                    } in (case s_200 of
                    { _ -> k_G212 ckout_205 (k_G212 z_204 x_202)
                    ; 1 -> ckout_205
                    }) :: Int
                |])
          , testCase "a shared argument" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int", varD "z_204" "Int"]
                (tm "(case s_200 of { _ -> k_G212 x_202 z_204; 1 -> k_G212 y_203 z_204 }) :: Int")
                (tm [strQQ|
                  let
                    { (ckout_205 :: Int) = let
                        { (ckarg0_206 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203 }) :: Int
                        } in k_G212 ckarg0_206 z_204
                    } in (case s_200 of { _ -> ckout_205; 1 -> ckout_205 }) :: Int
                |])
          , testCase "all arguments shared" $
              decTo [varD "s_200" "Int", varD "x_202" "Int"]
                (tm "(case s_200 of { _ -> f_G210 x_202; 1 -> f_G210 x_202 }) :: Int")
                (tm [strQQ|
                  let { (cfout_203 :: Int) = f_G210 x_202 } in (case s_200 of { _ -> cfout_203; 1 -> cfout_203 }) :: Int
                |])
          , testCase "a shared argument up to ticks" $
              decTo [varD "s_200" "Int", varD "x_202" "Int"]
                (tm "(case s_200 of { _ -> f_G210 (Core.deDup x_202); 1 -> f_G210 x_202 }) :: Int")
                (tm [strQQ|
                  let { (cfout_203 :: Int) = f_G210 (Core.deDup x_202) } in (case s_200 of
                    { _ -> cfout_203
                    ; 1 -> cfout_203
                    }) :: Int
                |])
          , testCase "a non-representable argument" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm "(case s_200 of { _ -> hof_G215 f_G210 x_202; 1 -> hof_G215 g_G211 y_203 }) :: Int")
                (tm [strQQ|
                  let
                    { (chofout_204 :: Int) = let
                        { (chofarg1_205 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203 }) :: Int
                        } in hof_G215 ((case s_200 of { _ -> f_G210; 1 -> g_G211 }) :: Int -> Int) chofarg1_205
                    } in (case s_200 of { _ -> chofout_204; 1 -> chofout_204 }) :: Int
                |])
          , testCase "an argument mentioning a pattern binder" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "p_217" "Pair"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> f_G210 x_202
                    ; 1 -> (case p_217 of { MkPair (a :: Int) (b :: Int) -> f_G210 b }) :: Int
                    }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (cfout_218 :: Int) = let
                        { (cfarg0_219 :: Int) = (case s_200 of
                            { _ -> x_202
                            ; 1 -> (case p_217 of { MkPair (a :: Int) (b :: Int) -> b }) :: Int
                            }) :: Int
                        } in f_G210 cfarg0_219
                    } in (case s_200 of
                    { _ -> cfout_218
                    ; 1 -> (case p_217 of { MkPair (a :: Int) (b :: Int) -> cfout_218 }) :: Int
                    }) :: Int
                |])
          , testCase "an argument mentioning a let-binder in an alternative" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm [strQQ|
                  (case s_200 of { _ -> f_G210 x_202; 1 -> let { !(a :: Int) = g_G211 y_203 } in f_G210 a }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (cfout_204 :: Int) = let
                        { (cfarg0_205 :: Int) = (case s_200 of
                            { _ -> x_202
                            ; 1 -> let { !(a :: Int) = g_G211 y_203 } in a
                            }) :: Int
                        } in f_G210 cfarg0_205
                    } in (case s_200 of { _ -> cfout_204; 1 -> cfout_204 }) :: Int
                |])
          , testCase "an application in the right-hand side of a let-binding" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm [strQQ|
                  (case s_200 of { _ -> f_G210 x_202; 1 -> let { !(a :: Int) = f_G210 y_203 } in k_G212 a a }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (cfout_204 :: Int) = let
                        { (cfarg0_205 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203 }) :: Int
                        } in f_G210 cfarg0_205
                    } in (case s_200 of
                    { _ -> cfout_204
                    ; 1 -> let { !(a :: Int) = cfout_204 } in k_G212 a a
                    }) :: Int
                |])
          , testCase "an argument mentioning a recursive let-binder" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> f_G210 x_202
                    ; 1 -> let { (a :: Int) = g_G211 y_203; (b :: Int) = k_G212 a x_202 } in f_G210 b
                    }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (cfout_204 :: Int) = let
                        { (cfarg0_205 :: Int) = (case s_200 of
                            { _ -> x_202
                            ; 1 -> let { (a :: Int) = g_G211 y_203; (b :: Int) = k_G212 a x_202 } in b
                            }) :: Int
                        } in f_G210 cfarg0_205
                    } in (case s_200 of { _ -> cfout_204; 1 -> cfout_204 }) :: Int
                |])
          , testCase "applications in a recursive group are left alone" $
              decKeeps [varD "s_200" "Int", varD "x_202" "Int"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> f_G210 x_202
                    ; 1 -> let { (a :: Int) = f_G210 b; (b :: Int) = g_G211 a } in a
                    }) :: Int
                |])
          ]
      , testGroup "type arguments"
          [ testCase "shared type arguments" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm "(case s_200 of { _ -> h_G213 @Int x_202; 1 -> h_G213 @Int y_203 }) :: Int")
                (tm [strQQ|
                  let
                    { (chout_204 :: Int) = let
                        { (charg1_205 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203 }) :: Int
                        } in h_G213 @Int charg1_205
                    } in (case s_200 of { _ -> chout_204; 1 -> chout_204 }) :: Int
                |])
          , testCase "a type variable of the context" $
              decTo [kindedTyVarD "n_220" "Type", varD "s_200" "Int", varD "xn_243" "n_220", varD "yn_244" "n_220"]
                (tm "(case s_200 of { _ -> h_G213 @n_220 xn_243; 1 -> h_G213 @n_220 yn_244 }) :: Int")
                (tm [strQQ|
                  let
                    { (chout_245 :: Int) = let
                        { (charg1_246 :: n_220) = (case s_200 of { _ -> xn_243; 1 -> yn_244 }) :: n_220
                        } in h_G213 @n_220 charg1_246
                    } in (case s_200 of { _ -> chout_245; 1 -> chout_245 }) :: Int
                |])
          , testCase "different type arguments" $
              decKeeps [varD "s_200" "Int", varD "x_202" "Int", varD "p_217" "Pair"]
                (tm "(case s_200 of { _ -> h_G213 @Int x_202; 1 -> h_G213 @Pair p_217 }) :: Int")
          , testCase "an existential type variable" $
              decKeeps [varD "s_200" "Int", varD "e_219" "T"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> (case e_219 of { MkT (n :: Type) (v :: n) -> h_G213 @n v }) :: Int
                    ; 1 -> (case e_219 of { MkT (n :: Type) (v :: n) -> h_G213 @n v }) :: Int
                    }) :: Int
                |])
          , -- Clash drops casts, so after matching on a GADT, an argument can
            -- have a type that mentions an existential type variable where the
            -- function expects another type: here @v :: n@ for @f :: Int ->
            -- Int@. DEC gives the let-binder of the argument the type of the
            -- parameter of @f@, rather than the type of the first argument,
            -- which mentions @n@ outside of the pattern that binds it.
            -- 'assertRewritesTo' checks that the result is lint-clean, so that
            -- it has no free type variables.
            testCase "an argument whose type mentions an existential type variable" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "e_219" "T"]
                (tm "(case s_200 of { _ -> (case e_219 of { MkT (n :: Type) (v :: n) -> f_G210 v }) :: Int; 1 -> f_G210 x_202 }) :: Int")
                (tm [strQQ|
                  let
                    { (fout :: Int) = let
                        { (farg :: Int) = (case s_200 of
                            { _ -> (case e_219 of { MkT (n :: Type) (v :: n) -> v }) :: Int
                            ; 1 -> x_202
                            }) :: Int
                        } in f_G210 farg
                    } in (case s_200 of
                      { _ -> (case e_219 of { MkT (n :: Type) (v :: n) -> fout }) :: Int
                      ; 1 -> fout
                      }) :: Int
                |])
          ]
      , testGroup "nested case-expressions"
          [ testCase "literal patterns" $
              decTo [varD "s_200" "Int", varD "t_201" "Int", varD "x_202" "Int", varD "y_203" "Int"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> (case t_201 of { 1 -> f_G210 x_202; _ -> g_G211 y_203 }) :: Int
                    ; 1 -> f_G210 y_203
                    }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (cfout_204 :: Int) = let
                        { (cfarg0_205 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203 }) :: Int
                        } in f_G210 cfarg0_205
                    } in (case s_200 of
                    { _ -> (case t_201 of { 1 -> cfout_204; _ -> g_G211 y_203 }) :: Int
                    ; 1 -> cfout_204
                    }) :: Int
                |])
          , testCase "a non-exhaustive data case-expression" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "m_218" "Maybe Int"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> f_G210 x_202
                    ; 1 -> (case m_218 of { Nothing -> g_G211 x_202; Just (a :: Int) -> f_G210 a }) :: Int
                    }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (cfout_219 :: Int) = let
                        { (cfarg0_220 :: Int) = (case s_200 of
                            { _ -> x_202
                            ; 1 -> (case m_218 of { _ -> Clash.Normalize.Primitives.undefined @Int; Just (a :: Int) -> a }) :: Int
                            }) :: Int
                        } in f_G210 cfarg0_220
                    } in (case s_200 of
                    { _ -> cfout_219
                    ; 1 -> (case m_218 of { Nothing -> g_G211 x_202; Just (a :: Int) -> cfout_219 }) :: Int
                    }) :: Int
                |])
          , testCase "a case-expression in which the pattern binders are not used" $
              decTo [varD "s_200" "Int", varD "x_202" "Int", varD "y_203" "Int", varD "p_217" "Pair"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> f_G210 x_202
                    ; 1 -> (case p_217 of { MkPair (a :: Int) (b :: Int) -> f_G210 y_203 }) :: Int
                    }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (cfout_218 :: Int) = let
                        { (cfarg0_219 :: Int) = (case s_200 of { _ -> x_202; 1 -> y_203 }) :: Int
                        } in f_G210 cfarg0_219
                    } in (case s_200 of
                    { _ -> cfout_218
                    ; 1 -> (case p_217 of { MkPair (a :: Int) (b :: Int) -> cfout_218 }) :: Int
                    }) :: Int
                |])
          , testCase "two levels" $
              decTo [varD "s_200" "Int", varD "t_201" "Int", varD "x_202" "Int", varD "y_203" "Int", varD "z_204" "Int", varD "w_205" "Int"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> (case t_201 of { _ -> k_G212 x_202 y_203; 1 -> k_G212 y_203 z_204 }) :: Int
                    ; 1 -> k_G212 z_204 w_205
                    ; 2 -> (case t_201 of { _ -> g_G211 x_202; 2 -> k_G212 w_205 x_202 }) :: Int
                    }) :: Int
                |])
                (tm [strQQ|
                  let
                    { (ckout_206 :: Int) = let
                        { (cktupIn_207 :: Tup2_3502 Int Int) = (case s_200 of
                            { _ -> (case t_201 of { _ -> MkTup2 @Int @Int x_202 y_203; 1 -> MkTup2 @Int @Int y_203 z_204 }) :: Tup2_3502 Int Int
                            ; 1 -> MkTup2 @Int @Int z_204 w_205
                            ; 2 -> MkTup2 @Int @Int w_205 x_202
                            }) :: Tup2_3502 Int Int
                        ; (ckarg0_208 :: Int) = (case cktupIn_207 of { MkTup2 (csel :: Int) (cwild_211 :: Int) -> csel }) :: Int
                        ; (ckarg1_209 :: Int) = (case cktupIn_207 of { MkTup2 (cwild_210 :: Int) (csel :: Int) -> csel }) :: Int
                        } in k_G212 ckarg0_208 ckarg1_209
                    } in (case s_200 of
                    { _ -> (case t_201 of { _ -> ckout_206; 1 -> ckout_206 }) :: Int
                    ; 1 -> ckout_206
                    ; 2 -> (case t_201 of { _ -> g_G211 x_202; 2 -> ckout_206 }) :: Int
                    }) :: Int
                |])
          ]
      , testGroup "primitives"
          [ testCase "an interesting primitive" $
              decTo [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> GHC.Num.Integer.integerMul ix_230 (3 :: Integer)
                    ; 1 -> GHC.Num.Integer.integerMul iy_231 (5 :: Integer)
                    }) :: GHC.Num.Integer.Integer
                |])
                (tm [strQQ|
                  let
                    { (cintegerMulout_232 :: GHC.Num.Integer.Integer) = let
                        { (cintegerMultupIn_233 :: Tup2_3502 GHC.Num.Integer.Integer GHC.Num.Integer.Integer) = (case s_200 of
                            { _ -> MkTup2 @GHC.Num.Integer.Integer @GHC.Num.Integer.Integer ix_230 (3 :: Integer)
                            ; 1 -> MkTup2 @GHC.Num.Integer.Integer @GHC.Num.Integer.Integer iy_231 (5 :: Integer)
                            }) :: Tup2_3502 GHC.Num.Integer.Integer GHC.Num.Integer.Integer
                        ; (cintegerMularg0_234 :: GHC.Num.Integer.Integer) = (case cintegerMultupIn_233 of
                            { MkTup2 (csel :: GHC.Num.Integer.Integer) (cwild_237 :: GHC.Num.Integer.Integer) -> csel
                            }) :: GHC.Num.Integer.Integer
                        ; (cintegerMularg1_235 :: GHC.Num.Integer.Integer) = (case cintegerMultupIn_233 of
                            { MkTup2 (cwild_236 :: GHC.Num.Integer.Integer) (csel :: GHC.Num.Integer.Integer) -> csel
                            }) :: GHC.Num.Integer.Integer
                        } in GHC.Num.Integer.integerMul cintegerMularg0_234 cintegerMularg1_235
                    } in (case s_200 of { _ -> cintegerMulout_232; 1 -> cintegerMulout_232 }) :: GHC.Num.Integer.Integer
                |])
          , testCase "a primitive that isn't interesting" $
              decKeeps [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of { _ -> Prim.plainPrim ix_230; 1 -> Prim.plainPrim iy_231 }) :: GHC.Num.Integer.Integer
                |])
          , testCase "a primitive with DeDup" $
              decTo [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> Core.deDup (Prim.plainPrim ix_230)
                    ; 1 -> Core.deDup (Prim.plainPrim iy_231)
                    }) :: GHC.Num.Integer.Integer
                |])
                (tm [strQQ|
                  let
                    { (cplainPrimout_232 :: GHC.Num.Integer.Integer) = let
                        { (cplainPrimarg0_233 :: GHC.Num.Integer.Integer) = (case s_200 of { _ -> ix_230; 1 -> iy_231 }) :: GHC.Num.Integer.Integer
                        } in Prim.plainPrim cplainPrimarg0_233
                    } in (case s_200 of { _ -> cplainPrimout_232; 1 -> cplainPrimout_232 }) :: GHC.Num.Integer.Integer
                |])
          , testCase "a higher-order primitive, multiplying by a power of two" $
              decKeeps [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> Prim.hoPrim (GHC.Num.Integer.integerMul (4 :: Integer)) ((case ix_230 of { _ -> ix_230 }) :: GHC.Num.Integer.Integer)
                    ; 1 -> Prim.hoPrim (GHC.Num.Integer.integerMul (4 :: Integer)) ((case iy_231 of { _ -> iy_231 }) :: GHC.Num.Integer.Integer)
                    }) :: GHC.Num.Integer.Integer
                |])
          , testCase "a higher-order primitive, multiplying by zero" $
              decKeeps [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> Prim.hoPrim (GHC.Num.Integer.integerMul (0 :: Integer)) ((case ix_230 of { _ -> ix_230 }) :: GHC.Num.Integer.Integer)
                    ; 1 -> Prim.hoPrim (GHC.Num.Integer.integerMul (0 :: Integer)) ((case iy_231 of { _ -> iy_231 }) :: GHC.Num.Integer.Integer)
                    }) :: GHC.Num.Integer.Integer
                |])
          , testCase "a higher-order primitive, multiplying by another number" $
              decTo [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> Prim.hoPrim (GHC.Num.Integer.integerMul (3 :: Integer)) ((case ix_230 of { _ -> ix_230 }) :: GHC.Num.Integer.Integer)
                    ; 1 -> Prim.hoPrim (GHC.Num.Integer.integerMul (3 :: Integer)) ((case iy_231 of { _ -> iy_231 }) :: GHC.Num.Integer.Integer)
                    }) :: GHC.Num.Integer.Integer
                |])
                (tm [strQQ|
                  let
                    { (choPrimout_232 :: GHC.Num.Integer.Integer) = let
                        { (choPrimarg1_233 :: GHC.Num.Integer.Integer) = (case s_200 of
                            { _ -> (case ix_230 of { _ -> ix_230 }) :: GHC.Num.Integer.Integer
                            ; 1 -> (case iy_231 of { _ -> iy_231 }) :: GHC.Num.Integer.Integer
                            }) :: GHC.Num.Integer.Integer
                        } in Prim.hoPrim (GHC.Num.Integer.integerMul (3 :: Integer)) choPrimarg1_233
                    } in (case s_200 of { _ -> choPrimout_232; 1 -> choPrimout_232 }) :: GHC.Num.Integer.Integer
                |])
          , testCase "a higher-order primitive, multiplying by fromInteger# of a power of two" $
              decKeeps [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> Prim.hoPrim (GHC.Num.Integer.integerMul (Clash.Sized.Internal.Unsigned.fromInteger# @8 (8 :: Natural) (8 :: Integer))) ((case ix_230 of
                        { _ -> ix_230
                        }) :: GHC.Num.Integer.Integer)
                    ; 1 -> Prim.hoPrim (GHC.Num.Integer.integerMul (Clash.Sized.Internal.Unsigned.fromInteger# @8 (8 :: Natural) (8 :: Integer))) ((case iy_231 of
                        { _ -> iy_231
                        }) :: GHC.Num.Integer.Integer)
                    }) :: GHC.Num.Integer.Integer
                |])
          , testCase "a higher-order primitive, multiplying by fromInteger# of another number" $
              decTo [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> Prim.hoPrim (GHC.Num.Integer.integerMul (Clash.Sized.Internal.Unsigned.fromInteger# @8 (8 :: Natural) (6 :: Integer))) ((case ix_230 of
                        { _ -> ix_230
                        }) :: GHC.Num.Integer.Integer)
                    ; 1 -> Prim.hoPrim (GHC.Num.Integer.integerMul (Clash.Sized.Internal.Unsigned.fromInteger# @8 (8 :: Natural) (6 :: Integer))) ((case iy_231 of
                        { _ -> iy_231
                        }) :: GHC.Num.Integer.Integer)
                    }) :: GHC.Num.Integer.Integer
                |])
                (tm [strQQ|
                  let
                    { (choPrimout_232 :: GHC.Num.Integer.Integer) = let
                        { (choPrimarg1_233 :: GHC.Num.Integer.Integer) = (case s_200 of
                            { _ -> (case ix_230 of { _ -> ix_230 }) :: GHC.Num.Integer.Integer
                            ; 1 -> (case iy_231 of { _ -> iy_231 }) :: GHC.Num.Integer.Integer
                            }) :: GHC.Num.Integer.Integer
                        } in Prim.hoPrim (GHC.Num.Integer.integerMul (Clash.Sized.Internal.Unsigned.fromInteger# @8 (8 :: Natural) (6 :: Integer))) choPrimarg1_233
                    } in (case s_200 of { _ -> choPrimout_232; 1 -> choPrimout_232 }) :: GHC.Num.Integer.Integer
                |])
          , testCase "a higher-order primitive, dividing by a power of two" $
              decKeeps [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> Prim.hoPrim (GHC.Num.Integer.integerQuot (7 :: Integer) (2 :: Integer)) ((case ix_230 of
                        { _ -> ix_230
                        }) :: GHC.Num.Integer.Integer)
                    ; 1 -> Prim.hoPrim (GHC.Num.Integer.integerQuot (7 :: Integer) (2 :: Integer)) ((case iy_231 of
                        { _ -> iy_231
                        }) :: GHC.Num.Integer.Integer)
                    }) :: GHC.Num.Integer.Integer
                |])
          , testCase "a higher-order primitive, dividing by another number" $
              decTo [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of
                    { _ -> Prim.hoPrim (GHC.Num.Integer.integerQuot (8 :: Integer) (3 :: Integer)) ((case ix_230 of
                        { _ -> ix_230
                        }) :: GHC.Num.Integer.Integer)
                    ; 1 -> Prim.hoPrim (GHC.Num.Integer.integerQuot (8 :: Integer) (3 :: Integer)) ((case iy_231 of
                        { _ -> iy_231
                        }) :: GHC.Num.Integer.Integer)
                    }) :: GHC.Num.Integer.Integer
                |])
                (tm [strQQ|
                  let
                    { (choPrimout_232 :: GHC.Num.Integer.Integer) = let
                        { (choPrimarg1_233 :: GHC.Num.Integer.Integer) = (case s_200 of
                            { _ -> (case ix_230 of { _ -> ix_230 }) :: GHC.Num.Integer.Integer
                            ; 1 -> (case iy_231 of { _ -> iy_231 }) :: GHC.Num.Integer.Integer
                            }) :: GHC.Num.Integer.Integer
                        } in Prim.hoPrim (GHC.Num.Integer.integerQuot (8 :: Integer) (3 :: Integer)) choPrimarg1_233
                    } in (case s_200 of { _ -> choPrimout_232; 1 -> choPrimout_232 }) :: GHC.Num.Integer.Integer
                |])
          , testCase "a higher-order primitive, applied to a function" $
              decTo [varD "s_200" "Int", varD "ix_230" "GHC.Num.Integer.Integer", varD "iy_231" "GHC.Num.Integer.Integer"]
                (tm [strQQ|
                  (case s_200 of { _ -> Prim.hoPrim fI_G245 ix_230; 1 -> Prim.hoPrim fI_G245 iy_231 }) :: GHC.Num.Integer.Integer
                |])
                (tm [strQQ|
                  let
                    { (choPrimout_232 :: GHC.Num.Integer.Integer) = let
                        { (choPrimarg1_233 :: GHC.Num.Integer.Integer) = (case s_200 of { _ -> ix_230; 1 -> iy_231 }) :: GHC.Num.Integer.Integer
                        } in Prim.hoPrim fI_G245 choPrimarg1_233
                    } in (case s_200 of { _ -> choPrimout_232; 1 -> choPrimout_232 }) :: GHC.Num.Integer.Integer
                |])
          ]
      ]

    ]
