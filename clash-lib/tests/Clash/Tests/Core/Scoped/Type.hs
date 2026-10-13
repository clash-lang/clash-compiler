{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Type". The functions give the results that
  their counterparts in "Clash.Core.Type" gave on 'corpus' ('expected'), in
  the scope of the free type variables of the corpus, and sunk into a larger
  scope where all binders of the types are below the watermark of the scope.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.Scoped.Type (tests, corpus) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExcept)
import Data.Either (isLeft)
import Data.Maybe (isJust)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.Basic as C (ConstTy (TyCon), LitTy (NumTy))
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Core.Pretty (PrettyOptions (..), PrettyPrec, showPpr')

import Clash.Core.Scoped.Builder (TyB, annTyB, appTyB, constTyB, litTyB, runTyFamEqn)
import Clash.Core.TyCon (mkFunTyCon)
import Clash.Core.Scoped.FreeVars (freeTyVarsOfType)
import Clash.Core.Scoped.NameMap (nullNameSet)
import Clash.Core.Scoped.Subst (unsinkType)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Type
import Clash.Core.Scoped.TypeEnv (TypeEnv, mkTypeEnv)
import qualified Clash.Core.Scoped.TysPrim as P

import Test.Clash.Rewrite (addTyConName, cmpNatTyConName, mkTyConName, testTyConMap)
import Test.Clash.Scoped

tenv :: TypeEnv
tenv = testTypeEnv

-- | The free type variables of the corpus. @c@ gets name 2: that of the type
-- variable that the representation type of @P@, and the right-hand side of
-- @Q@, bind.
decls :: [ScopeDecl]
decls = [tyVarD "x", tyVarD "c"]

-- | Run an assertion in the scope of 'decls', and sunk into a larger scope,
-- where all binders of the types are below the watermark of the scope
inScopes :: (forall n. Distinct n => TestScope n -> Assertion) -> Assertion
inScopes k = withTestScope decls $ \ts0 -> do
  k ts0
  extendTestScope ts0 [sunkD] k

-- | A type constructor applied to types
tcB :: C.TyConName -> [TyB] -> TyB
tcB nm = foldl appTyB (constTyB (C.TyCon nm))

litB :: Integer -> TyB
litB = litTyB . C.NumTy

-- | @GHC.TypeNats.+@
addB :: TyB -> TyB -> TyB
addB a b = tcB addTyConName [a, b]

-- | Types with newtypes, type families, function types, binders and
-- annotations, closed and with free type variables
corpus :: [TestType]
corpus =
  [ "Int", "Bool", "Maybe Int", "Maybe x"
  , "N Int", "N x", "N (N x)", "P Int", "P x", "P c", "W"
  , "N Int Bool"
  , "Signal \"System\" (N Int)", "Signal \"System\" (Int -> x)"
  , "F Int", "F (Maybe Bool)", "F x", "F (Maybe x)", "F (F Int)"
  , "F (N Int)", builtType (appTyB (testTypeB "F") (annTyB [] (testTypeB "Int")))
  , "Max Int Int", "Max Int Bool", "Max x x", "Max (F Int) Bool"
  , "K Int"
  , "Q Int", "Q x", "Q c"
  , builtType (addB (litB 3) (litB 4)), builtType (addB (addB (litB 1) (litB 2)) (testTypeB "x"))
  , builtType (addB (testTypeB "x") (litB 1))
  , "CmpNat 1 2", builtType (tcB cmpNatTyConName [litB 2, addB (litB 1) (litB 1)])
  , "UnconsSymbol \"ab\"", "UnconsSymbol \"\""
  , "Int -> N Int", "W -> x", "(Int -> x) -> W"
  , "forall a. a -> N a"
  , "forall b. x -> b"
  , "forall a b. a -> P b"
  , "Int -> forall a. a -> W"
  , builtType (annTyB [] (testTypeB "N Int")), builtType (appTyB (testTypeB "N") (annTyB [] (testTypeB "Int")))
  , "5", "\"s\"", "Type"
  , "Pair", "Clock \"System\"", "Reset \"System\"", "Enable \"System\"", "ClkPair"
  , "Vec 2 (Clock \"System\")"
  , "Vec 2 Int"
  ]

-- | Render a value as text, as the expected results are
pp :: PrettyPrec p => p -> String
pp = showPpr' (PrettyOptions False False False False)

-- | Render an optional type
ppm :: PrettyPrec p => Maybe p -> String
ppm = maybe "Nothing" pp

-- | A function on types, whose results 'render' as text
newtype Render = Render (forall n. Distinct n => Scope n -> Type n -> String)

-- | The functions of "Clash.Core.Scoped.Type", rendering their results
render :: [(String, Render)]
render =
  [ ("tyView", Render $ \_ ty -> case tyView ty of
      FunTy a b -> "FunTy (" <> pp a <> ") (" <> pp b <> ")"
      TyConApp tc as -> "TyConApp " <> show (C.nameOcc tc) <> " " <> show (map pp as)
      OtherType t -> "OtherType " <> pp t)
  , ("coreView1", Render $ \sc ty -> ppm (coreView1 tenv sc ty))
  , ("coreView", Render $ \sc ty -> pp (coreView tenv sc ty))
  , ("normalizeType", Render $ \sc ty -> pp (normalizeType tenv sc ty))
  , ("reduceTypeFamily", Render $ \sc ty -> ppm (reduceTypeFamily tenv sc ty))
  , ("isTypeFamilyApplication", Render $ \_ ty -> show (isTypeFamilyApplication tenv ty))
  , ("splitTyConAppM", Render $ \_ ty ->
      show (fmap (\(tc, as) -> (C.nameOcc tc, map pp as)) (splitTyConAppM ty)))
  , ("splitFunTy", Render $ \sc ty -> show (fmap (\(a, b) -> (pp a, pp b)) (splitFunTy tenv sc ty)))
  , ("splitFunTys", Render $ \sc ty -> show (case splitFunTys tenv sc ty of (as, r) -> (map pp as, pp r)))
  , ("applyFunTy", Render $ \sc ty ->
      if isFunTy tenv sc ty then pp (applyFunTy tenv sc ty (sinkClosed (closedType "Int"))) else "-")
  , ("isFunTy", Render $ \sc ty -> show (isFunTy tenv sc ty))
  , ("isPolyTy", Render $ \_ ty -> show (isPolyTy ty))
  , ("isPolyFunTy", Render $ \_ ty -> show (isPolyFunTy ty))
  , ("isPolyFunCoreTy", Render $ \sc ty -> show (isPolyFunCoreTy tenv sc ty))
  , ("splitFunForallTy", Render $ \_ ty -> case splitFunForallTy ty of
      SplitFunForall tele res -> show (piTeleShape tele) <> " " <> pp (mkPolyFunTy tele res))
  , ("splitCoreFunForallTy", Render $ \sc ty -> case splitCoreFunForallTy tenv sc ty of
      SplitCoreFunForall _ tele res -> show (piTeleShape tele) <> " " <> pp (mkPolyFunTy tele res))
  , ("tyNatSize", Render $ \sc ty -> show (eitherToMaybe (runExcept (tyNatSize tenv sc ty))))
  , ("tyLitShow", Render $ \sc ty -> show (eitherToMaybe (runExcept (tyLitShow tenv sc ty))))
  , ("isSignalType", Render $ \sc ty -> show (isSignalType tenv sc ty))
  , ("isEnable", Render $ \sc ty -> show (isEnable tenv sc ty))
  , ("isClockOrReset", Render $ \sc ty -> show (isClockOrReset tenv sc ty))
  , ("isClassTy", Render $ \_ ty -> show (isClassTy tenv ty))
  , ("isIntegerTy", Render $ \_ ty -> show (isIntegerTy ty))
  , ("typeAttrs", Render $ \_ ty -> show (typeAttrs ty))
  , ("stripAnnTypes", Render $ \_ ty -> pp (stripAnnTypes ty))
  , ("stripIP", Render $ \_ ty -> pp (stripIP ty))
  ]
 where
  eitherToMaybe :: Either e a -> Maybe a
  eitherToMaybe = either (const Nothing) Just

-- | The shape of a split function type: which are quantified type variables
-- and which are argument types
piTeleShape :: PiTele n l -> [Bool]
piTeleShape = \case
  PiNil -> []
  PiForall _ rest -> False : piTeleShape rest
  PiArg _ rest -> True : piTeleShape rest

-- | The results of the functions of "Clash.Core.Type" on 'corpus', as
-- 'render' renders them: per function, the results on the types of the corpus
-- in order
expected :: [(String, [String])]
expected =
  [ ("tyView",
      [ "TyConApp \"Int\" []", "TyConApp \"Bool\" []", "TyConApp \"Maybe\" [\"Int\"]",
        "TyConApp \"Maybe\" [\"x\"]", "TyConApp \"N\" [\"Int\"]", "TyConApp \"N\" [\"x\"]",
        "TyConApp \"N\" [\"N x\"]", "TyConApp \"P\" [\"Int\"]", "TyConApp \"P\" [\"x\"]",
        "TyConApp \"P\" [\"c\"]", "TyConApp \"W\" []", "TyConApp \"N\" [\"Int\",\"Bool\"]",
        "TyConApp \"Clash.Signal.Internal.Signal\" [\"\\\"System\\\"\",\"N Int\"]",
        "TyConApp \"Clash.Signal.Internal.Signal\" [\"\\\"System\\\"\",\"Int -> x\"]",
        "TyConApp \"F\" [\"Int\"]", "TyConApp \"F\" [\"Maybe Bool\"]", "TyConApp \"F\" [\"x\"]",
        "TyConApp \"F\" [\"Maybe x\"]", "TyConApp \"F\" [\"F Int\"]", "TyConApp \"F\" [\"N Int\"]",
        "TyConApp \"F\" [\"Int\"]", "TyConApp \"Max\" [\"Int\",\"Int\"]",
        "TyConApp \"Max\" [\"Int\",\"Bool\"]", "TyConApp \"Max\" [\"x\",\"x\"]",
        "TyConApp \"Max\" [\"F Int\",\"Bool\"]", "TyConApp \"K\" [\"Int\"]",
        "TyConApp \"Q\" [\"Int\"]", "TyConApp \"Q\" [\"x\"]", "TyConApp \"Q\" [\"c\"]",
        "TyConApp \"GHC.TypeNats.+\" [\"3\",\"4\"]", "TyConApp \"GHC.TypeNats.+\" [\"+ 1 2\",\"x\"]",
        "TyConApp \"GHC.TypeNats.+\" [\"x\",\"1\"]",
        "TyConApp \"GHC.TypeNats.CmpNat\" [\"1\",\"2\"]",
        "TyConApp \"GHC.TypeNats.CmpNat\" [\"2\",\"+ 1 1\"]",
        "TyConApp \"GHC.TypeLits.UnconsSymbol\" [\"\\\"ab\\\"\"]",
        "TyConApp \"GHC.TypeLits.UnconsSymbol\" [\"\\\"\\\"\"]", "FunTy (Int) (N Int)",
        "FunTy (W) (x)", "FunTy (Int -> x) (W)", "OtherType forall a. a -> N a",
        "OtherType forall b. x -> b", "OtherType forall a b. a -> P b",
        "FunTy (Int) (forall a. a -> W)", "OtherType N Int", "TyConApp \"N\" [\"Int\"]",
        "OtherType 5", "OtherType \"s\"", "TyConApp \"Type\" []", "TyConApp \"Pair\" []",
        "TyConApp \"Clash.Signal.Internal.Clock\" [\"\\\"System\\\"\"]",
        "TyConApp \"Clash.Signal.Internal.Reset\" [\"\\\"System\\\"\"]",
        "TyConApp \"Clash.Signal.Internal.Enable\" [\"\\\"System\\\"\"]", "TyConApp \"ClkPair\" []",
        "TyConApp \"Clash.Sized.Vector.Vec\" [\"2\",\"Clock \\\"System\\\"\"]",
        "TyConApp \"Clash.Sized.Vector.Vec\" [\"2\",\"Int\"]" ])
  , ("coreView1",
      [ "Nothing", "Nothing", "Nothing", "Nothing", "Maybe Int", "Maybe x", "Maybe (N x)",
        "forall b. Int -> b", "forall b. x -> b", "forall b. c -> b", "Int -> Int", "Maybe Int Bool",
        "N Int", "Int -> x", "Bool", "Bool", "N x", "x", "N Bool", "N (N Int)", "Bool", "Int",
        "Maybe Bool", "x", "Bool", "Maybe Int", "forall b. Int -> b", "forall b. x -> b",
        "forall b. c -> b", "7", "Nothing", "Nothing", "LT", "EQ",
        "Just (T2 Char# Symbol) (MkT2 Char# Symbol 'a' \"b\")", "Nothing (T2 Char# Symbol)",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Maybe Int",
        "Maybe Int", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing" ])
  , ("coreView",
      [ "Int", "Bool", "Maybe Int", "Maybe x", "Maybe Int", "Maybe x", "Maybe (N x)",
        "forall b. Int -> b", "forall b. x -> b", "forall b. c -> b", "Int -> Int", "Maybe Int Bool",
        "Maybe Int", "Int -> x", "Bool", "Bool", "Maybe x", "x", "Maybe Bool", "Maybe (N Int)",
        "Bool", "Int", "Maybe Bool", "x", "Bool", "Maybe Int", "forall b. Int -> b",
        "forall b. x -> b", "forall b. c -> b", "7", "+ (+ 1 2) x", "+ x 1", "LT", "EQ",
        "Just (T2 Char# Symbol) (MkT2 Char# Symbol 'a' \"b\")", "Nothing (T2 Char# Symbol)",
        "Int -> N Int", "W -> x", "(Int -> x) -> W", "forall a. a -> N a", "forall b. x -> b",
        "forall a b. a -> P b", "Int -> forall a. a -> W", "Maybe Int", "Maybe Int", "5", "\"s\"",
        "Type", "Pair", "Clock \"System\"", "Reset \"System\"", "Enable \"System\"", "ClkPair",
        "Vec 2 (Clock \"System\")", "Vec 2 Int" ])
  , ("normalizeType",
      [ "Int", "Bool", "Maybe Int", "Maybe x", "Maybe Int", "Maybe x", "Maybe (Maybe x)",
        "forall b. Int -> b", "forall b. x -> b", "forall b. c -> b", "Int -> Int", "Maybe Int Bool",
        "Signal \"System\" (Maybe Int)", "Signal \"System\" (Int -> x)", "Bool", "Bool", "Maybe x",
        "x", "Maybe Bool", "Int", "Bool", "Int", "Maybe Bool", "x", "Bool", "Maybe Int",
        "forall b. Int -> b", "forall b. x -> b", "forall b. c -> b", "7", "+ 3 x", "+ x 1", "LT",
        "EQ", "Just (T2 Char# Symbol) (MkT2 Char# Symbol 'a' \"b\")", "Nothing (T2 Char# Symbol)",
        "Int -> Maybe Int", "(Int -> Int) -> x", "(Int -> x) -> Int -> Int",
        "forall a. a -> Maybe a", "forall b. x -> b", "forall a b. a -> forall b. b -> b",
        "Int -> forall a. a -> Int -> Int", "N Int", "Maybe Int", "5", "\"s\"", "Type", "Pair",
        "Clock \"System\"", "Reset \"System\"", "Enable \"System\"", "ClkPair",
        "Vec 2 (Clock \"System\")", "Vec 2 Int" ])
  , ("reduceTypeFamily",
      [ "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Bool", "Bool", "N x", "x",
        "N Bool", "N (N Int)", "Bool", "Int", "Maybe Bool", "x", "Bool", "Maybe Int",
        "forall b. Int -> b", "forall b. x -> b", "forall b. c -> b", "7", "Nothing", "Nothing",
        "LT", "EQ", "Just (T2 Char# Symbol) (MkT2 Char# Symbol 'a' \"b\")",
        "Nothing (T2 Char# Symbol)", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing" ])
  , ("isTypeFamilyApplication",
      [ "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "True", "True", "True", "True", "True", "True", "True",
        "True", "True", "True", "True", "True", "True", "True", "True", "True", "True", "True",
        "True", "True", "True", "True", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False" ])
  , ("splitTyConAppM",
      [ "Just (\"Int\",[])", "Just (\"Bool\",[])", "Just (\"Maybe\",[\"Int\"])",
        "Just (\"Maybe\",[\"x\"])", "Just (\"N\",[\"Int\"])", "Just (\"N\",[\"x\"])",
        "Just (\"N\",[\"N x\"])", "Just (\"P\",[\"Int\"])", "Just (\"P\",[\"x\"])",
        "Just (\"P\",[\"c\"])", "Just (\"W\",[])", "Just (\"N\",[\"Int\",\"Bool\"])",
        "Just (\"Clash.Signal.Internal.Signal\",[\"\\\"System\\\"\",\"N Int\"])",
        "Just (\"Clash.Signal.Internal.Signal\",[\"\\\"System\\\"\",\"Int -> x\"])",
        "Just (\"F\",[\"Int\"])", "Just (\"F\",[\"Maybe Bool\"])", "Just (\"F\",[\"x\"])",
        "Just (\"F\",[\"Maybe x\"])", "Just (\"F\",[\"F Int\"])", "Just (\"F\",[\"N Int\"])",
        "Just (\"F\",[\"Int\"])", "Just (\"Max\",[\"Int\",\"Int\"])",
        "Just (\"Max\",[\"Int\",\"Bool\"])", "Just (\"Max\",[\"x\",\"x\"])",
        "Just (\"Max\",[\"F Int\",\"Bool\"])", "Just (\"K\",[\"Int\"])", "Just (\"Q\",[\"Int\"])",
        "Just (\"Q\",[\"x\"])", "Just (\"Q\",[\"c\"])", "Just (\"GHC.TypeNats.+\",[\"3\",\"4\"])",
        "Just (\"GHC.TypeNats.+\",[\"+ 1 2\",\"x\"])", "Just (\"GHC.TypeNats.+\",[\"x\",\"1\"])",
        "Just (\"GHC.TypeNats.CmpNat\",[\"1\",\"2\"])",
        "Just (\"GHC.TypeNats.CmpNat\",[\"2\",\"+ 1 1\"])",
        "Just (\"GHC.TypeLits.UnconsSymbol\",[\"\\\"ab\\\"\"])",
        "Just (\"GHC.TypeLits.UnconsSymbol\",[\"\\\"\\\"\"])", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Just (\"N\",[\"Int\"])", "Nothing",
        "Nothing", "Just (\"Type\",[])", "Just (\"Pair\",[])",
        "Just (\"Clash.Signal.Internal.Clock\",[\"\\\"System\\\"\"])",
        "Just (\"Clash.Signal.Internal.Reset\",[\"\\\"System\\\"\"])",
        "Just (\"Clash.Signal.Internal.Enable\",[\"\\\"System\\\"\"])", "Just (\"ClkPair\",[])",
        "Just (\"Clash.Sized.Vector.Vec\",[\"2\",\"Clock \\\"System\\\"\"])",
        "Just (\"Clash.Sized.Vector.Vec\",[\"2\",\"Int\"])" ])
  , ("splitFunTy",
      [ "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Just (\"Int\",\"Int\")", "Nothing", "Nothing", "Just (\"Int\",\"x\")",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Just (\"Int\",\"N Int\")",
        "Just (\"W\",\"x\")", "Just (\"Int -> x\",\"W\")", "Nothing", "Nothing", "Nothing",
        "Just (\"Int\",\"forall a. a -> W\")", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing" ])
  , ("splitFunTys",
      [ "([],\"Int\")", "([],\"Bool\")", "([],\"Maybe Int\")", "([],\"Maybe x\")", "([],\"N Int\")",
        "([],\"N x\")", "([],\"N (N x)\")", "([],\"P Int\")", "([],\"P x\")", "([],\"P c\")",
        "([\"Int\"],\"Int\")", "([],\"N Int Bool\")", "([],\"Signal \\\"System\\\" (N Int)\")",
        "([\"Int\"],\"x\")", "([],\"F Int\")", "([],\"F (Maybe Bool)\")", "([],\"F x\")",
        "([],\"F (Maybe x)\")", "([],\"F (F Int)\")", "([],\"F (N Int)\")", "([],\"F Int\")",
        "([],\"Max Int Int\")", "([],\"Max Int Bool\")", "([],\"Max x x\")",
        "([],\"Max (F Int) Bool\")", "([],\"K Int\")", "([],\"Q Int\")", "([],\"Q x\")",
        "([],\"Q c\")", "([],\"+ 3 4\")", "([],\"+ (+ 1 2) x\")", "([],\"+ x 1\")",
        "([],\"CmpNat 1 2\")", "([],\"CmpNat 2 (+ 1 1)\")", "([],\"UnconsSymbol \\\"ab\\\"\")",
        "([],\"UnconsSymbol \\\"\\\"\")", "([\"Int\"],\"N Int\")", "([\"W\"],\"x\")",
        "([\"Int -> x\",\"Int\"],\"Int\")", "([],\"forall a. a -> N a\")",
        "([],\"forall b. x -> b\")", "([],\"forall a b. a -> P b\")",
        "([\"Int\"],\"forall a. a -> W\")", "([],\"N Int\")", "([],\"N Int\")", "([],\"5\")",
        "([],\"\\\"s\\\"\")", "([],\"Type\")", "([],\"Pair\")", "([],\"Clock \\\"System\\\"\")",
        "([],\"Reset \\\"System\\\"\")", "([],\"Enable \\\"System\\\"\")", "([],\"ClkPair\")",
        "([],\"Vec 2 (Clock \\\"System\\\")\")", "([],\"Vec 2 Int\")" ])
  , ("applyFunTy",
      [ "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "Int", "-", "-", "x", "-", "-", "-", "-",
        "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",
        "N Int", "x", "W", "-", "-", "-", "forall a. a -> W", "-", "-", "-", "-", "-", "-", "-", "-",
        "-", "-", "-", "-" ])
  , ("isFunTy",
      [ "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "True", "False", "False", "True", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "True", "True", "True", "False",
        "False", "False", "True", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False" ])
  , ("isPolyTy",
      [ "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "True",
        "True", "True", "True", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False" ])
  , ("isPolyFunTy",
      [ "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "True", "True", "True", "True", "True",
        "True", "True", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False" ])
  , ("isPolyFunCoreTy",
      [ "False", "False", "False", "False", "False", "False", "False", "True", "True", "True",
        "True", "False", "False", "True", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "True", "True", "True", "False",
        "False", "False", "False", "False", "False", "False", "True", "True", "True", "True", "True",
        "True", "True", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False" ])
  , ("splitFunForallTy",
      [ "[] Int", "[] Bool", "[] Maybe Int", "[] Maybe x", "[] N Int", "[] N x", "[] N (N x)",
        "[] P Int", "[] P x", "[] P c", "[] W", "[] N Int Bool", "[] Signal \"System\" (N Int)",
        "[] Signal \"System\" (Int -> x)", "[] F Int", "[] F (Maybe Bool)", "[] F x",
        "[] F (Maybe x)", "[] F (F Int)", "[] F (N Int)", "[] F Int", "[] Max Int Int",
        "[] Max Int Bool", "[] Max x x", "[] Max (F Int) Bool", "[] K Int", "[] Q Int", "[] Q x",
        "[] Q c", "[] + 3 4", "[] + (+ 1 2) x", "[] + x 1", "[] CmpNat 1 2", "[] CmpNat 2 (+ 1 1)",
        "[] UnconsSymbol \"ab\"", "[] UnconsSymbol \"\"", "[True] Int -> N Int", "[True] W -> x",
        "[True] (Int -> x) -> W", "[False,True] forall a. a -> N a", "[False,True] forall b. x -> b",
        "[False,False,True] forall a b. a -> P b", "[True,False,True] Int -> forall a. a -> W",
        "[] N Int", "[] N Int", "[] 5", "[] \"s\"", "[] Type", "[] Pair", "[] Clock \"System\"",
        "[] Reset \"System\"", "[] Enable \"System\"", "[] ClkPair", "[] Vec 2 (Clock \"System\")",
        "[] Vec 2 Int" ])
  , ("splitCoreFunForallTy",
      [ "[] Int", "[] Bool", "[] Maybe Int", "[] Maybe x", "[] N Int", "[] N x", "[] N (N x)",
        "[False,True] forall b. Int -> b", "[False,True] forall b. x -> b",
        "[False,True] forall b. c -> b", "[True] Int -> Int", "[] N Int Bool",
        "[] Signal \"System\" (N Int)", "[True] Int -> x", "[] F Int", "[] F (Maybe Bool)", "[] F x",
        "[] F (Maybe x)", "[] F (F Int)", "[] F (N Int)", "[] F Int", "[] Max Int Int",
        "[] Max Int Bool", "[] Max x x", "[] Max (F Int) Bool", "[] K Int",
        "[False,True] forall b. Int -> b", "[False,True] forall b. x -> b",
        "[False,True] forall b. c -> b", "[] + 3 4", "[] + (+ 1 2) x", "[] + x 1", "[] CmpNat 1 2",
        "[] CmpNat 2 (+ 1 1)", "[] UnconsSymbol \"ab\"", "[] UnconsSymbol \"\"",
        "[True] Int -> N Int", "[True] W -> x", "[True,True] (Int -> x) -> Int -> Int",
        "[False,True] forall a. a -> N a", "[False,True] forall b. x -> b",
        "[False,False,True,False,True] forall a b. a -> forall b. b -> b",
        "[True,False,True,True] Int -> forall a. a -> Int -> Int", "[] N Int", "[] N Int", "[] 5",
        "[] \"s\"", "[] Type", "[] Pair", "[] Clock \"System\"", "[] Reset \"System\"",
        "[] Enable \"System\"", "[] ClkPair", "[] Vec 2 (Clock \"System\")", "[] Vec 2 Int" ])
  , ("tyNatSize",
      [ "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Just 7", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Just 5", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing" ])
  , ("tyLitShow",
      [ "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Just \"7\"", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Just \"5\"", "Just \"s\"", "Nothing",
        "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing", "Nothing" ])
  , ("isSignalType",
      [ "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "True", "True", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False" ])
  , ("isEnable",
      [ "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "True", "False", "False", "False" ])
  , ("isClockOrReset",
      [ "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "True",
        "True", "False", "False", "False", "False" ])
  , ("isClassTy",
      [ "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False" ])
  , ("isIntegerTy",
      [ "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False", "False", "False", "False", "False", "False",
        "False", "False", "False", "False", "False" ])
  , ("typeAttrs",
      [ "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]",
        "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]",
        "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]",
        "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]", "[]" ])
  , ("stripAnnTypes",
      [ "Int", "Bool", "Maybe Int", "Maybe x", "N Int", "N x", "N (N x)", "P Int", "P x", "P c", "W",
        "N Int Bool", "Signal \"System\" (N Int)", "Signal \"System\" (Int -> x)", "F Int",
        "F (Maybe Bool)", "F x", "F (Maybe x)", "F (F Int)", "F (N Int)", "F Int", "Max Int Int",
        "Max Int Bool", "Max x x", "Max (F Int) Bool", "K Int", "Q Int", "Q x", "Q c", "+ 3 4",
        "+ (+ 1 2) x", "+ x 1", "CmpNat 1 2", "CmpNat 2 (+ 1 1)", "UnconsSymbol \"ab\"",
        "UnconsSymbol \"\"", "Int -> N Int", "W -> x", "(Int -> x) -> W", "forall a. a -> N a",
        "forall b. x -> b", "forall a b. a -> P b", "Int -> forall a. a -> W", "N Int", "N Int", "5",
        "\"s\"", "Type", "Pair", "Clock \"System\"", "Reset \"System\"", "Enable \"System\"",
        "ClkPair", "Vec 2 (Clock \"System\")", "Vec 2 Int" ])
  , ("stripIP",
      [ "Int", "Bool", "Maybe Int", "Maybe x", "N Int", "N x", "N (N x)", "P Int", "P x", "P c", "W",
        "N Int Bool", "Signal \"System\" (N Int)", "Signal \"System\" (Int -> x)", "F Int",
        "F (Maybe Bool)", "F x", "F (Maybe x)", "F (F Int)", "F (N Int)", "F Int", "Max Int Int",
        "Max Int Bool", "Max x x", "Max (F Int) Bool", "K Int", "Q Int", "Q x", "Q c", "+ 3 4",
        "+ (+ 1 2) x", "+ x 1", "CmpNat 1 2", "CmpNat 2 (+ 1 1)", "UnconsSymbol \"ab\"",
        "UnconsSymbol \"\"", "Int -> N Int", "W -> x", "(Int -> x) -> W", "forall a. a -> N a",
        "forall b. x -> b", "forall a b. a -> P b", "Int -> forall a. a -> W", "N Int", "N Int", "5",
        "\"s\"", "Type", "Pair", "Clock \"System\"", "Reset \"System\"", "Enable \"System\"",
        "ClkPair", "Vec 2 (Clock \"System\")", "Vec 2 Int" ])
  ]

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Type"
  [ testGroup "results on the corpus"
    [ testCase nm $ inScopes $ \ts -> do
        let sc = testScope ts
        sequence_
          [ assertEqual (nm <> " of type " <> show i <> ": " <> pp ty) e (f sc ty)
          | (i, t, e) <- zip3 [0 :: Int ..] corpus (lookupExpected nm)
          , let ty = typeIn ts t ]
    | (nm, Render f) <- render ]

  , testCase "tyNatSize of a type family application" $
      withTypeAndSunk (builtType (addB (litB 3) (litB 4))) $ \ts ty ->
        runExcept (tyNatSize tenv (testScope ts) ty) @?= Right 7

  , testCase "tyNatSize fails on a type that is not a number" $
      withTypeAndSunk "Maybe Int" $ \ts ty ->
        assertBool "fails" (isLeft (runExcept (tyNatSize tenv (testScope ts) ty)))

    -- A newtype whose representation type binds a type variable, applied to a
    -- type variable with the same raw name as that binder
  , testCase "coreView1 renames binders of a newtype that would capture" $
      inScopes $ \ts ->
        maybe (assertFailure "no view") (assertTypeIs ts "forall b. c -> b")
          (coreView1 tenv (testScope ts) (typeIn ts "P c"))

    -- The same for a type family whose right-hand side binds a type variable
  , testCase "reduceTypeFamily renames binders that would capture" $
      inScopes $ \ts ->
        maybe (assertFailure "no reduction") (assertTypeIs ts "forall b. c -> b")
          (reduceTypeFamily tenv (testScope ts) (typeIn ts "Q c"))

  , testCase "reduceTypeFamily of builtin type families" $
      forM_ [ builtType (addB (litB 3) (litB 4))
            , "CmpNat 1 2"
            , "UnconsSymbol \"ab\"" ] $ \ty0 ->
        inScopes $ \ts ->
          assertBool ("reduces: " <> pp (typeIn ts ty0))
            (isJust (reduceTypeFamily tenv (testScope ts) (typeIn ts ty0)))

  , testCase "reduceTypeFamily matches repeated pattern variables" $ inScopes $ \ts -> do
      maybe (assertFailure "no reduction") (assertTypeIs ts "x")
        (reduceTypeFamily tenv (testScope ts) (typeIn ts "Max x x"))
      maybe (assertFailure "no reduction") (assertTypeIs ts "Maybe Int")
        (reduceTypeFamily tenv (testScope ts) (typeIn ts "Max x Int"))

  , testCase "reduceTypeFamily applies the remaining arguments" $
      withTypeAndSunk "K Int" $ \ts ty ->
        maybe (assertFailure "no reduction") (assertTypeIs ts "Maybe Int")
          (reduceTypeFamily tenv (testScope ts) ty)

  , testCase "reduceTypeFamily reduces the arguments" $
      withTypeAndSunk "F (F Int)" $ \ts ty ->
        -- F (F Int) = F Bool = N Bool
        maybe (assertFailure "no reduction") (assertTypeIs ts "N Bool")
          (reduceTypeFamily tenv (testScope ts) ty)

    -- Unlike Clash.Core.Type, an equation whose right-hand side mentions a
    -- pattern variable that its left-hand side doesn't bind doesn't reduce
  , testCase "reduceTypeFamily of an equation with an unbound pattern variable" $ do
      let uNm = mkTyConName "U" 3105
          uTc = mkFunTyCon uNm (mkFunTy P.liftedTypeKind P.liftedTypeKind) 1
                  [runTyFamEqn [(1, C.mkUnsafeSystemName "x" 0, testTypeB "Type")] [testTypeB "Int"] (testTypeB "x_1")]
          tenv' = mkTypeEnv (UniqMap.insert (C.tyConName uTc) uTc testTyConMap)
      withTypeAndSunk (builtType (tcB uNm [testTypeB "Int"])) $ \ts ty ->
        assertBool "doesn't reduce" (null (reduceTypeFamily tenv' (testScope ts) ty))

  , testCase "splitCoreFunForallTy gives the scope of the result type" $
      withTypeAndSunk "forall a. a -> Signal \"System\" (N a)" $ \ts ty ->
        case splitCoreFunForallTy tenv (testScope ts) ty of
          SplitCoreFunForall sc' (PiForall _ (PiArg _ PiNil)) res -> do
            -- The result type keeps the Signal, and looks through it in its
            -- scope
            pp res @?= "Signal \"System\" (N a)"
            pp (coreView tenv sc' res) @?= "Maybe a"
          _ -> assertFailure "unexpected split"

  , testCase "piTele arities" $
      case splitFunForallTy (closedType "forall a. a -> forall b. Int -> b") of
        SplitFunForall tele _ -> do
          piTeleTyArity tele @?= 2
          piTeleTmArity tele @?= 2

  , testCase "piTeleNameBinders moves a result type out of the telescope" $
      case splitFunForallTy (closedType "forall a. a -> Int") of
        SplitFunForall tele res -> do
          maybe (assertFailure "mentions a") (assertTypeIs emptyTestScope "Int")
            (unsinkType (piTeleNameBinders tele) res)
          case splitFunForallTy (closedType "forall a. Int -> a") of
            SplitFunForall tele' res' -> do
              assertBool "mentions a" (not (nullNameSet (freeTyVarsOfType res')))
              assertBool "can't move out" (null (unsinkType (piTeleNameBinders tele') res'))
  ]
 where
  lookupExpected nm = maybe (error ("no expected results for " <> nm)) id (lookup nm expected)
