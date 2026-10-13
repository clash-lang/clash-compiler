{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Util". Most expected results are those of the
  functions of "Clash.Core.Util" that these replace, rendered as text.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.Scoped.Util (tests) where

import qualified Data.List as List
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Basic as C (ConstTy (TyCon), LitTy (NumTy))
import Clash.Core.Pretty (PrettyOptions (..), PrettyPrec, showPpr')

import Clash.Core.Scoped.Builder (appTyB, constTyB, litTyB)
import Clash.Core.Scoped.FreeVars (teleNameBinders)
import Clash.Core.Scoped.Subst (unsinkTerm)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Util

import Test.Clash.Rewrite
  (addTyConName, brDc, consDc, lrDc, nilDc)
import Test.Clash.Scoped

-- | Render a term or a type as text, with types
pp :: PrettyPrec p => p -> String
pp = showPpr' (PrettyOptions False True False True)

-- | Assert that a rendered value is the expected one, see 'expected'
assertRenders :: String -> String -> Assertion
assertRenders key actual = case lookup key expected of
  Just ls -> assertEqual key (List.intercalate "\n" ls) actual
  Nothing -> assertFailure ("no expected result for " <> key)

-- * Let-bindings

chainOrdered, chainReversed, withCycle :: TestTerm
-- | Bindings that are in dependency order
chainOrdered = "let { (x :: Int) = 1; (y :: Int) = (f :: Int -> Int) x; (z :: Int) = f y } in x"
-- | Bindings that are in reverse dependency order
chainReversed = "let { (z :: Int) = (f :: Int -> Int) y; (y :: Int) = f x; (x :: Int) = 1 } in x"
-- | Bindings with a cycle
withCycle = "let { (c :: Int) = (f :: Int -> Int) a; (a :: Int) = f b; (b :: Int) = f a } in c"

-- | Like 'inverseTopSortLetBindings', on a let-expression
inverseTopSort :: Distinct n => Scope n -> Term n -> Term n
inverseTopSort sc = \case
  Let (Rec bs xs) e ->
    inverseTopSortLetBindings sc bs xs e $ \_ bs' xs' e' -> Let (Rec bs' xs') e'
  _ -> error "inverseTopSort: not a recursive let-expression"

-- | Like 'listToLets', on a let-expression
listToLetsTm :: Distinct n => Scope n -> Term n -> Term n
listToLetsTm sc = \case
  Let (Rec bs xs) e -> listToLets sc bs xs e
  _ -> error "listToLetsTm: not a recursive let-expression"

-- | The raw names of the binders of nested let-expressions
letRaws :: Term n -> [Int]
letRaws = \case
  Let (NonRec bndr _) e -> tmNameRaw (tmNameOf bndr) : letRaws e
  Let (Rec bs _) e -> teleRaws bs ++ letRaws e
  _ -> []
 where
  teleRaws :: Tele IdBinder n l -> [Int]
  teleRaws TNil = []
  teleRaws (TCons bndr rest) = tmNameRaw (tmNameOf bndr) : teleRaws rest

letTests :: TestTree
letTests = testGroup "Let-bindings"
  [ testCase "listToLets" $
      sequence_
        [ withTerm tm $ \ts e -> assertRenders key (pp (listToLetsTm (testScope ts) e))
        | (key, tm) <- [("listToLets#1", chainOrdered), ("listToLets#2", chainReversed)] ]

  , testCase "listToLets on a cycle" $
      withTerm withCycle $ \ts e ->
        assertTermIs ts "let { (a :: Int) = f b; (b :: Int) = f a } in let { !(c :: Int) = f a } in c"
          (listToLetsTm (testScope ts) e)

  , testCase "inverseTopSortLetBindings" $
      sequence_
        [ withTerm tm $ \ts e -> assertRenders key (pp (inverseTopSort (testScope ts) e))
        | (key, tm) <- [("inverseTopSort#1", chainOrdered), ("inverseTopSort#2", chainReversed)] ]

  , testCase "bindings in order keep their binders" $
      withTerm "let { (x :: Int) = 1; (y :: Int) = (f :: Int -> Int) x; (z :: Int) = f y } in z" $ \ts tm ->
        letRaws (listToLetsTm (testScope ts) tm) @?= letRaws tm

  , testCase "reordered bindings get fresh binders" $
      withTerm "let { (z :: Int) = (f :: Int -> Int) y; (y :: Int) = f x; (x :: Int) = 1 } in z" $ \ts0 tm0 ->
        withClashing ts0 [sunkD] tm0 $ \ts tm -> do
          let res = listToLetsTm (testScope ts) tm
          assertBool "fresh binders" (all (> 1000000) (letRaws res))
          assertRenders "reordered#1" (pp res)
  ]

-- * Vectors and trees

-- | A type variable @a@, elements of type @a@, a vector @v :: Vec 3 a@, a
-- tree @t :: RTree 2 a@ and a pair @p@
vecDecls :: [ScopeDecl]
vecDecls =
  [ tyVarD "a", varD "e11_11" "a", varD "e12_12" "a", varD "e13_13" "a", varD "e14_14" "a"
  , varD "v_20" "Vec 3 a", varD "t_21" "RTree 2 a", varD "p_30" "Pair" ]

-- | Run an assertion in the scope of 'vecDecls'
withVecs :: (forall n. Distinct n => TestScope n -> Assertion) -> Assertion
withVecs = withTestScope vecDecls

vecTests :: TestTree
vecTests = testGroup "Vectors and trees"
  [ testCase "mkVec" $ withVecs $ \ts -> do
      let sc = testScope ts
      assertRenders "mkVec2#1"
        (pp (mkVec testTypeEnv sc nilDc consDc (typeIn ts "a") 2 [termIn ts "e11_11", termIn ts "e12_12"]))
      assertRenders "mkVec0#1" (pp (mkVec testTypeEnv sc nilDc consDc (typeIn ts "a") 0 []))

  , testCase "appendToVec" $ withVecs $ \ts ->
      assertRenders "appendToVec#1"
        (pp (appendToVec testTypeEnv (testScope ts) consDc (typeIn ts "a") (termIn ts "v_20") 5
              [termIn ts "e11_11", termIn ts "e12_12"]))

  , testCase "mkRTree" $ withVecs $ \ts ->
      assertRenders "mkRTree#1"
        (pp (mkRTree testTypeEnv (testScope ts) lrDc brDc (typeIn ts "a") 2
              (map (termIn ts) ["e11_11", "e12_12", "e13_13", "e14_14"])))

  , testCase "extractElems" $ withVecs $ \ts -> do
      let ty = typeIn ts "a"
      assertRenders "extractElems#1"
        (pp (extractElems testTypeEnv (testScope ts) consDc ty 'V' 3 3 (termIn ts "v_20") $ \sc1 bs rhss vs _ ->
          Let (Rec bs rhss) (mkVec testTypeEnv sc1 nilDc consDc (sink ty) 3 vs)))

  , testCase "extractElems of fewer elements" $ withVecs $ \ts -> do
      let ty = typeIn ts "a"
          sc = testScope ts
      -- The head, as 'Clash.Normalize.Scoped.PrimitiveReductions.reduceHead'
      -- makes it
      assertRenders "extractElemsHead#1"
        (pp (extractElems testTypeEnv sc consDc ty 'V' 3 1 (termIn ts "v_20") $ \_ bs rhss vs _ ->
          case vs of
            v1:_ -> Let (Rec bs rhss) v1
            [] -> error "no elements"))
      -- The tail, as the right-hand side of the binder of the tail
      assertRenders "extractElemsTail#1"
        (pp (extractElems testTypeEnv sc consDc ty 'V' 3 1 (termIn ts "v_20") $ \_ bs _ _ tl ->
          case unsinkTerm (teleNameBinders bs) tl of
            Just tl' -> tl'
            Nothing -> error "the tail refers to a binder"))

  , testCase "extractElems gives fresh binders" $ withVecs $ \ts0 ->
      withClashing ts0 [sunkD] (termIn ts0 "v_20") $ \ts v ->
        extractElems testTypeEnv (testScope ts) consDc (typeIn ts "a") 'V' 3 3 v $ \_ bs rhss _ _ -> do
          assertBool "fresh binders" (all (> 1000000) (teleRawsOf bs))
          assertBool "fresh pattern binders" (all (> 1000000) (concatMap caseBinderRaws rhss))

  , testCase "extractTElems" $ withVecs $ \ts -> do
      let ty = typeIn ts "a"
      assertRenders "extractTElems#1"
        (pp (extractTElems testTypeEnv (testScope ts) lrDc brDc ty 'T' 2 (termIn ts "t_21") $ \sc1 bs rhss vs ->
          Let (Rec bs rhss) (mkRTree testTypeEnv sc1 lrDc brDc (sink ty) 2 vs)))
  ]
 where
  teleRawsOf :: Tele IdBinder n l -> [Int]
  teleRawsOf TNil = []
  teleRawsOf (TCons bndr rest) = tmNameRaw (tmNameOf bndr) : teleRawsOf rest

  caseBinderRaws :: Term n -> [Int]
  caseBinderRaws = \case
    Case _ _ [Alt (DataPat _ tvs ids) _] -> tyRaws tvs ++ teleRawsOf ids
    _ -> []

  tyRaws :: Tele TyBinder n l -> [Int]
  tyRaws TNil = []
  tyRaws (TCons bndr rest) = tyNameRaw (tyNameOf bndr) : tyRaws rest

-- * Data constructors

dataConTests :: TestTree
dataConTests = testGroup "Data constructors"
  [ testCase "mkSelectorCase on a product" $ withVecs $ \ts -> do
      assertRenders "sel#1" (pp (mkSelectorCase "test" (testScope ts) testTypeEnv (termIn ts "p_30") 1 0))
      assertRenders "sel#2" (pp (mkSelectorCase "test" (testScope ts) testTypeEnv (termIn ts "p_30") 1 1))

    -- Selecting the tail of a vector solves the equation @3 ~ m + 1@ for the
    -- existentially quantified @m@
  , testCase "mkSelectorCase on Cons" $ withVecs $ \ts -> do
      assertRenders "sel#3" (pp (mkSelectorCase "test" (testScope ts) testTypeEnv (termIn ts "v_20") 2 1))
      assertRenders "sel#4" (pp (mkSelectorCase "test" (testScope ts) testTypeEnv (termIn ts "v_20") 2 2))

  , testCase "mkSelectorCase gives fresh binders" $ withVecs $ \ts0 ->
      withClashing ts0 [sunkD] (termIn ts0 "v_20") $ \ts v ->
        case mkSelectorCase "test" (testScope ts) testTypeEnv v 2 2 of
          Case _ _ [Alt (DataPat _ (TCons m TNil) ids) _] ->
            assertBool "fresh binders" (all (> 1000000) (tyNameRaw (tyNameOf m) : idRaws ids))
          _ -> assertFailure "unexpected shape"

  , testCase "substArgTys" $ do
      let three = LitTy (C.NumTy 3)
      case substArgTys testTypeEnv emptyScope consDc [three, closedType "Int"] of
        Nothing -> pure ()
        Just _ -> assertFailure "the type of a field of Cons mentions an existential"
      case substArgTys testTypeEnv emptyScope lrDc [LitTy (C.NumTy 0), closedType "Int"] of
        Just [_, ty] -> assertTypeIs emptyTestScope "Int" ty
        _ -> assertFailure "unexpected result"
  ]
 where
  idRaws :: Tele IdBinder n l -> [Int]
  idRaws TNil = []
  idRaws (TCons bndr rest) = tmNameRaw (tmNameOf bndr) : idRaws rest

-- * Types

-- | Types to compare splitting on
exampleTypes :: [TestType]
exampleTypes =
  [ "Int", "Pair", "Clock \"System\"", "Reset \"System\"", "Enable \"System\""
  , "Signal \"System\" Int", "ClkPair", "Vec 2 (Clock \"System\")", "Vec 2 Int", "a", "3"
  , builtType (appTyB (appTyB (constTyB (C.TyCon addTyConName)) (litTyB (C.NumTy 2))) (litTyB (C.NumTy 1)))
  , "\"x\"" ]

typeTests :: TestTree
typeTests = testGroup "Types"
  [ testCase "splitShouldSplit" $ withTestScope [tyVarD "a"] $ \ts ->
      assertRenders "splitShouldSplit#1"
        (show (map pp (splitShouldSplit testTypeEnv (testScope ts) (map (typeIn ts) exampleTypes))))
  , testCase "shouldSplit on a product" $ assertShouldSplit "shouldSplit#1" "ClkPair"
  , testCase "shouldSplit on a vector" $ assertShouldSplit "shouldSplit#2" "Vec 2 (Clock \"System\")"
  ]

-- | Assert that 'shouldSplit' splits a type: the types of the fields,
-- constructing a value out of variables, and the projections of a variable
assertShouldSplit :: String -> TestType -> Assertion
assertShouldSplit key ty =
  withTestScope [varD "s_50" ty, varD "f40_40" "Clock \"System\"", varD "f41_41" fieldTy] $ \ts -> do
    let sc = testScope ts
    case shouldSplit testTypeEnv sc (typeIn ts ty) of
      Nothing -> assertFailure "not split"
      Just info -> do
        assertRenders (key <> ".tys") (show (map pp (splitArgTys info)))
        assertRenders (key <> ".con") (pp (splitConstruct info sc [termIn ts "f40_40", termIn ts "f41_41"]))
        assertRenders (key <> ".proj") (show (map pp (splitProject info sc (termIn ts "s_50"))))
 where
  -- The type of the second field
  fieldTy = if key == "shouldSplit#1" then "Int" else "Clock \"System\""

-- * Primitives

primTests :: TestTree
primTests = testGroup "Primitives"
  [ testCase "primAtType gives a primitive the type if it is closed" $
      case primAtType errPrim (closedType "Int") of
        Prim p -> assertTypeIs emptyTestScope "Int" (primType p)
        _ -> assertFailure "not a primitive"
  , testCase "primAtType applies a primitive to its type if it is open" $
      withTestScope [tyVarD "a"] $ \ts -> case primAtType errPrim (typeIn ts "a") of
        TyApp (Prim p) ty -> do
          assertTypeIs emptyTestScope "forall a. a" (primType p)
          assertTypeIs ts "a" ty
        _ -> assertFailure "not a type application of a primitive"
  ]
 where
  errPrim = testPrim "err" "Int"

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Util"
  [ letTests
  , vecTests
  , dataConTests
  , typeTests
  , primTests
  ]

-- | The results of the functions of "Clash.Core.Util" that the functions
-- of "Clash.Core.Scoped.Util" replace, rendered as text ('pp'), by test, as
-- lines
expected :: [(String, [String])]
expected =
  [ ("listToLets#1",
      [ "let"
      , "  x :: Int"
      , "  = 1#"
      , "in let"
      , "     y :: Int"
      , "     = f x"
      , "in let"
      , "     z :: Int"
      , "     = f y"
      , "in x" ])
  , ("listToLets#2",
      [ "let"
      , "  x :: Int"
      , "  = 1#"
      , "in let"
      , "     y :: Int"
      , "     = f x"
      , "in let"
      , "     z :: Int"
      , "     = f y"
      , "in x" ])
  , ("inverseTopSort#1",
      [ "letrec"
      , "  x :: Int"
      , "  = 1#"
      , "  y :: Int"
      , "  = f x"
      , "  z :: Int"
      , "  = f y"
      , "in x" ])
  , ("inverseTopSort#2",
      [ "letrec"
      , "  x :: Int"
      , "  = 1#"
      , "  y :: Int"
      , "  = f x"
      , "  z :: Int"
      , "  = f y"
      , "in x" ])
  , ("reordered#1",
      [ "let"
      , "  x :: Int"
      , "  = 1#"
      , "in let"
      , "     y :: Int"
      , "     = f x"
      , "in let"
      , "     z :: Int"
      , "     = f y"
      , "in z" ])
  , ("mkVec2#1",
      [ "Cons @2 @a @1 (_CO_ @(~# Natural Natural 2 (+ 1 1))) e11"
      , "  (Cons @1 @a @0 (_CO_ @(~# Natural Natural 1 (+ 0 1))) e12 (Nil @0 @a (_CO_ @(~# Natural Natural 0 0))))" ])
  , ("mkVec0#1",
      [ "Nil @0 @a (_CO_ @(~# Natural Natural 0 0))" ])
  , ("appendToVec#1",
      [ "Cons @5 @a @4 (_CO_ @(~# Natural Natural 5 (+ 4 1))) e11 (Cons @4 @a @3 (_CO_ @(~# Natural Natural 4 (+ 3 1))) e12 v)" ])
  , ("mkRTree#1",
      [ "BR @2 @a @1 (_CO_ @(~# Natural Natural 2 (+ 1 1)))"
      , "  (BR @1 @a @0 (_CO_ @(~# Natural Natural 1 (+ 0 1))) (LR @0 @a (_CO_ @(~# Natural Natural 0 0)) e11)"
      , "     (LR @0 @a (_CO_ @(~# Natural Natural 0 0)) e12))"
      , "  (BR @1 @a @0 (_CO_ @(~# Natural Natural 1 (+ 0 1))) (LR @0 @a (_CO_ @(~# Natural Natural 0 0)) e13)"
      , "     (LR @0 @a (_CO_ @(~# Natural Natural 0 0)) e14))" ])
  , ("extractElems#1",
      [ "letrec"
      , "  elV0 :: a"
      , "  = case v of"
      , "      Cons m (_co_ :: ~# Natural Natural 3 (+ 2 1)) (el :: a) (rest :: Vec 2 a) ->"
      , "        el"
      , "  restV0 :: Vec 2 a"
      , "  = case v of"
      , "      Cons m (_co_ :: ~# Natural Natural 3 (+ 2 1)) (el :: a) (rest :: Vec 2 a) ->"
      , "        rest"
      , "  elV1 :: a"
      , "  = case restV0 of"
      , "      Cons m (_co_ :: ~# Natural Natural 2 (+ 1 1)) (el :: a) (rest :: Vec 1 a) ->"
      , "        el"
      , "  restV1 :: Vec 1 a"
      , "  = case restV0 of"
      , "      Cons m (_co_ :: ~# Natural Natural 2 (+ 1 1)) (el :: a) (rest :: Vec 1 a) ->"
      , "        rest"
      , "  elV2 :: a"
      , "  = case restV1 of"
      , "      Cons m (_co_ :: ~# Natural Natural 1 (+ 0 1)) (el :: a) (rest :: Vec 0 a) ->"
      , "        el"
      , "in Cons @3 @a @2 (_CO_ @(~# Natural Natural 3 (+ 2 1))) elV0"
      , "     (Cons @2 @a @1 (_CO_ @(~# Natural Natural 2 (+ 1 1))) elV1"
      , "        (Cons @1 @a @0 (_CO_ @(~# Natural Natural 1 (+ 0 1))) elV2 (Nil @0 @a (_CO_ @(~# Natural Natural 0 0)))))" ])
  , ("extractElemsHead#1",
      [ "letrec"
      , "  elV0 :: a"
      , "  = case v of"
      , "      Cons m (_co_ :: ~# Natural Natural 3 (+ 2 1)) (el :: a) (rest :: Vec 2 a) ->"
      , "        el"
      , "in elV0" ])
  , ("extractElemsTail#1",
      [ "case v of"
      , "  Cons m (_co_ :: ~# Natural Natural 3 (+ 2 1)) (el :: a) (rest :: Vec 2 a) ->"
      , "    rest" ])
  , ("extractTElems#1",
      [ "letrec"
      , "  ltT0 :: RTree 1 a"
      , "  = case t of"
      , "      BR m (_co_ :: ~# Natural Natural 2 (+ 1 1)) (lt :: RTree 1 a) (rt :: RTree 1 a) ->"
      , "        lt"
      , "  rtT3 :: RTree 1 a"
      , "  = case t of"
      , "      BR m (_co_ :: ~# Natural Natural 2 (+ 1 1)) (lt :: RTree 1 a) (rt :: RTree 1 a) ->"
      , "        rt"
      , "  ltT1 :: RTree 0 a"
      , "  = case ltT0 of"
      , "      BR m (_co_ :: ~# Natural Natural 1 (+ 0 1)) (lt :: RTree 0 a) (rt :: RTree 0 a) ->"
      , "        lt"
      , "  rtT2 :: RTree 0 a"
      , "  = case ltT0 of"
      , "      BR m (_co_ :: ~# Natural Natural 1 (+ 0 1)) (lt :: RTree 0 a) (rt :: RTree 0 a) ->"
      , "        rt"
      , "  elT0 :: a"
      , "  = case ltT1 of"
      , "      LR (_co_ :: ~# Natural Natural 0 0) (el :: a) ->"
      , "        el"
      , "  elT1 :: a"
      , "  = case rtT2 of"
      , "      LR (_co_ :: ~# Natural Natural 0 0) (el :: a) ->"
      , "        el"
      , "  ltT4 :: RTree 0 a"
      , "  = case rtT3 of"
      , "      BR m (_co_ :: ~# Natural Natural 1 (+ 0 1)) (lt :: RTree 0 a) (rt :: RTree 0 a) ->"
      , "        lt"
      , "  rtT5 :: RTree 0 a"
      , "  = case rtT3 of"
      , "      BR m (_co_ :: ~# Natural Natural 1 (+ 0 1)) (lt :: RTree 0 a) (rt :: RTree 0 a) ->"
      , "        rt"
      , "  elT2 :: a"
      , "  = case ltT4 of"
      , "      LR (_co_ :: ~# Natural Natural 0 0) (el :: a) ->"
      , "        el"
      , "  elT3 :: a"
      , "  = case rtT5 of"
      , "      LR (_co_ :: ~# Natural Natural 0 0) (el :: a) ->"
      , "        el"
      , "in BR @2 @a @1 (_CO_ @(~# Natural Natural 2 (+ 1 1)))"
      , "     (BR @1 @a @0 (_CO_ @(~# Natural Natural 1 (+ 0 1))) (LR @0 @a (_CO_ @(~# Natural Natural 0 0)) elT0)"
      , "        (LR @0 @a (_CO_ @(~# Natural Natural 0 0)) elT1))"
      , "     (BR @1 @a @0 (_CO_ @(~# Natural Natural 1 (+ 0 1))) (LR @0 @a (_CO_ @(~# Natural Natural 0 0)) elT2)"
      , "        (LR @0 @a (_CO_ @(~# Natural Natural 0 0)) elT3))" ])
  , ("sel#1",
      [ "case p of"
      , "  MkPair (c$sel :: Int) (c$wild :: Int) ->"
      , "    c$sel" ])
  , ("sel#2",
      [ "case p of"
      , "  MkPair (c$wild :: Int) (c$sel :: Int) ->"
      , "    c$sel" ])
  , ("sel#3",
      [ "case v of"
      , "  Cons m (c$wild :: ~# Natural Natural 3 (+ 2 1)) (c$sel :: a) (c$wild :: Vec 2 a) ->"
      , "    c$sel" ])
  , ("sel#4",
      [ "case v of"
      , "  Cons m (c$wild :: ~# Natural Natural 3 (+ 2 1)) (c$wild :: a) (c$sel :: Vec 2 a) ->"
      , "    c$sel" ])
  , ("splitShouldSplit#1",
      [ "[\"Int\",\"Pair\",\"Clock \\\"System\\\"\",\"Reset \\\"System\\\"\",\"Enable \\\"System\\\"\",\"Signal \\\"System\\\" Int\",\"Clock \\\"System\\\"\",\"Int\",\"Clock \\\"System\\\"\",\"Clock \\\"System\\\"\",\"Vec 2 Int\",\"a\",\"3\",\"+ 2 1\",\"\\\"x\\\"\"]" ])
  , ("shouldSplit#1.tys",
      [ "[\"Clock \\\"System\\\"\",\"Int\"]" ])
  , ("shouldSplit#1.con",
      [ "MkClkPair f40 f41" ])
  , ("shouldSplit#1.proj",
      [ "[\"case s of\\n  MkClkPair (c$sel :: Clock \\\"System\\\") (c$wild :: Int) ->\\n    c$sel\",\"case s of\\n  MkClkPair (c$wild :: Clock \\\"System\\\") (c$sel :: Int) ->\\n    c$sel\"]" ])
  , ("shouldSplit#2.tys",
      [ "[\"Clock \\\"System\\\"\",\"Clock \\\"System\\\"\"]" ])
  , ("shouldSplit#2.con",
      [ "Cons @2 @(Clock \"System\") @1 (_CO_ @(~# Natural Natural 2 (+ 1 1))) f40"
      , "  (Cons @1 @(Clock \"System\") @0 (_CO_ @(~# Natural Natural 1 (+ 0 1))) f41"
      , "     (Nil @0 @(Clock \"System\") (_CO_ @(~# Natural Natural 0 0))))" ])
  , ("shouldSplit#2.proj",
      [ "[\"case s of\\n  Cons m (c$wild :: ~# Natural Natural 2 (+ 1 1)) (c$sel :: Clock \\\"System\\\") (c$wild :: Vec 1 (Clock \\\"System\\\")) ->\\n    c$sel\",\"case case s of\\n       Cons m (c$wild :: ~# Natural Natural 2 (+ 1 1)) (c$wild :: Clock \\\"System\\\") (c$sel :: Vec 1 (Clock \\\"System\\\")) ->\\n         c$sel of\\n  Cons m (c$wild :: ~# Natural Natural 1 (+ 0 1)) (c$sel :: Clock \\\"System\\\") (c$wild :: Vec 0 (Clock \\\"System\\\")) ->\\n    c$sel\"]" ])
  ]
