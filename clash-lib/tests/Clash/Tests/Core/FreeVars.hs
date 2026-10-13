{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the free variables of scoped Clash Core, and tests of occurrence
  checks.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.FreeVars (tests) where

import Data.Maybe (isJust)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Clash.Core.FreeVars
import Clash.Core.NameMap
import Clash.Core.Subst
  (strengthenTyPat, unsinkAlt, unsinkIdBinder, unsinkTerm, unsinkTyBinder, unsinkType)
import Clash.Core.Syntax
import qualified Clash.Core.TysPrim as P

import Test.Clash.Core
  ( TestScope (..), atRaw, closedTerm, emptyTestScope, forAllScrambled, kindedTyVarD, nameSetRaws
  , termIn, testGlobalId, tyVarD, typeIn, varD, withClashing, withTerm, withTestScope
  , withTests)
import Test.Clash.Core.Gen
  ( InScope (..), Redex (..), SomeScope (..), Sunk (..), ctxOf, genClosedSunk, genInScope
  , genRedex, genSameRhsCase, genSunk, genTestScope, genType, refFreeTmOccurrences
  , refFreeTmVars, refFreeTyVars, refFreeTyVarsOfType, refGlobals, shrinkInScope
  , shrinkRedex, shrinkSunk, showT, showTy)

-- | The free variables of a term are those that the oracle gives, see
-- 'Test.Clash.Core.Gen.refFreeTmVars'
freeVarsAgree :: Term n -> Property
freeVarsAgree tm =
  counterexample "free term variables" (nameSetRaws (freeTmVars tm) === refFreeTmVars tm) .&&.
    counterexample "free type variables" (nameSetRaws (freeTyVars tm) === refFreeTyVars tm)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.FreeVars"
  [ -- @f g@, where the local @f@ has the raw name of the unique of the global
    -- @g@
    testCase "local and global variables are kept apart" $ do
      withTestScope [atRaw 20 (varD "f" "Int -> Int")] $ \ts -> do
        let tm = termIn ts "f (g_G20 :: Int)"
        globalSetToList (termGlobalIds tm) @?= [testGlobalId "g_G20" "Int"]
        nameSetRaws (freeTmVars tm) @?= [20]
      -- The binder of a lambda gets name 1, the unique of the global
      let lam = closedTerm "\\(f :: Int) -> (f_G1 :: Int -> Int) f"
      globalSetToList (termGlobalIds lam) @?= [testGlobalId "f_G1" "Int -> Int"]
      assertBool "no free local variables" (nullNameSet (freeTmVars lam))

    -- In @forall k. b@ with @b :: k@, the @k@ in the kind of the free @b@ is
    -- not the @k@ bound by the @forall@, so it's free.
    -- https://github.com/clash-lang/clash-compiler/issues/398,
    -- https://github.com/clash-lang/clash-compiler/pull/446
  , testCase "free type variables close over kinds (#446)" $
      withTestScope [tyVarD "k", kindedTyVarD "b" "k"] $ \ts ->
        nameSetRaws (freeTyVarsOfType (typeIn ts "forall (k :: Type). b")) @?= [1, 2]

    -- The term level equivalent: in @/\\k -> a@ with @a :: k@, the @k@ in the
    -- type of the free @a@ is free.
  , testCase "free type variables of a term close over types (#446)" $
      withTestScope [tyVarD "k", varD "a" "k"] $ \ts -> do
        let tm = termIn ts "\\(k :: Type) -> a"
        nameSetRaws (freeTyVars tm) @?= [1]
        nameSetRaws (freeTmVars tm) @?= [2]

  , testCase "freeVars splits type and term variables" $
      -- a_12 gets name 1, f_10 name 2 and x_11 name 3
      withTerm "\\(y :: Int) -> f_10 y (x_11 :: a_12)" $ \_ tm -> do
        let (tvs, ids) = freeVars tm
        nameSetRaws tvs @?= [1]
        nameSetRaws ids @?= [2, 3]

  , testCase "occurrence checks look at the types of occurrences" $
      -- The type variable only occurs in the type of the free variable x
      withTestScope [tyVarD "a", varD "x" "a"] $ \ts -> case termIn ts "x" of
        tm@(Var _ (VarTy _ _ an) xn) -> do
          assertBool "a occurs" (tyNameOccursIn an tm)
          assertBool "x occurs" (tmNameOccursIn xn tm)
        _ -> assertFailure "unexpected shape"

  , testCase "occurrence checks account for shadowing binders" $
      -- Build @\\y -> y@ in the empty scope, and sink it into the scope of
      -- @x@, where its binder has the same name as @x@
      withTerm "\\(y :: Int) -> y" $ \ts0 lam ->
        withClashing ts0 [varD "x" "Int"] lam $ \ts tm -> case termIn ts "x" of
          Var _ _ xn -> do
            assertBool "x does not occur" (not (tmNameOccursIn xn tm))
            assertBool "no free variables" (nullNameSet (freeTmVars tm))
          _ -> assertFailure "unexpected shape"

  , testCase "patBindersOccurIn" $
      case closedTerm "\\(s :: Pair) -> (case s of { MkPair (a :: Int) (b :: Int) -> a }) :: Int" of
        Lam _ (Case _ _ [Alt p e]) -> do
          assertBool "a occurs" (patBindersOccurIn p e)
          case p of
            DataPat _ TNil (TCons ba (TCons _ TNil)) ->
              assertBool "only a occurs"
                (nameSetRaws (freeTmVars e) == [tmNameRaw (tmNameOf ba)])
            _ -> assertFailure "unexpected pattern"
        _ -> assertFailure "unexpected shape"

  , withTests 500 $ testGroup "properties"
      [ testProperty "free variables agree with the oracle, per namespace" $
          forAllShrink genInScope shrinkInScope $ \(InScope _ tm) -> freeVarsAgree tm

      , testProperty "free variables of sunk terms agree with the oracle" $
          forAllShrink genSunk shrinkSunk $ \(Sunk _ ts1 tm) ->
            forAllScrambled (testScope ts1) (sink tm) freeVarsAgree

      , testProperty "occurrence counts agree with the oracle" $
          forAllShrink genInScope shrinkInScope $ \(InScope _ tm) ->
            [ (tmNameRaw x, k) | (x, k) <- nameMapToList (freeTmVarOccurrences tm) ]
              === refFreeTmOccurrences tm

      , testProperty "globals agree with the oracle" $
          forAllShrink genInScope shrinkInScope $ \(InScope _ tm) ->
            map globalIdUniq (globalSetToList (termGlobalIds tm)) === refGlobals tm

      , testProperty "occurrence checks agree with free variables" $
          forAllShrink genInScope shrinkInScope $ \(InScope ts tm) ->
            conjoin $
              [ counterexample (show (tmNameRaw n)) $
                  tmNameOccursIn n tm === memberNameSet n (freeTmVars tm)
              | Var _ _ n <- testVarsOf ts ] <>
              [ counterexample (show (tyNameRaw n)) $
                  tyNameOccursIn n tm === memberNameSet n (freeTyVars tm)
              | VarTy _ _ n <- testTyVarsOf ts ]

        -- Moving a term out of the scope of a binder succeeds if and only if
        -- the binder doesn't occur in it
      , testProperty "unsinking a binder agrees with the oracle" $
          forAllShrink genRedex shrinkRedex $ \case
            TmRedex _ _ (Lam b e) _ -> isJust (unsinkIdBinder b e)
              === notElem (tmNameRaw (tmNameOf b)) (refFreeTmVars e)
            TyRedex _ _ (TyLam b e) _ -> isJust (unsinkTyBinder b e)
              === notElem (tyNameRaw (tyNameOf b)) (refFreeTyVars e)
            _ -> property False

        -- With scrambled binders, binders have the names of variables in
        -- scope, and shadow each other
      , testProperty "unsinking agrees with free variables at every binder" $
          forAllShrink genSunk shrinkSunk $ \(Sunk _ ts1 tm) ->
            forAllScrambled (testScope ts1) (sink tm) $ \tm1 ->
              conjoin (freeVarsAgree tm1 : unsinkChecks tm1)

        -- A closed term sunk into a scope has no free variables
      , testProperty "hasFreeVars agrees with free variables" $
          forAll arbitrary $ \closed ->
            forAllShrink (if closed then genClosedSunk else genSunk) shrinkSunk $ \(Sunk _ ts1 tm) ->
              forAllScrambled (testScope ts1) (sink tm) $ \tm1 -> do
                let free = not (nullNameSet (freeTmVars tm1) && nullNameSet (freeTyVars tm1))
                cover 20 (not free) "no free variables" $
                  cover 20 free "free variables" $
                    hasFreeVars tm1 === free

      , testProperty "hasFreeVarsType agrees with free variables" $
          forAllBlind genTestScope $ \(SomeScope ts) ->
            forAll arbitrary $ \closed ->
              forAllShow
                (if closed
                   then sinkClosed <$> genType (ctxOf emptyTestScope) P.liftedTypeKind 3
                   else genType (ctxOf ts) P.liftedTypeKind 3)
                showTy $ \ty -> do
                  let free = not (nullNameSet (freeTyVarsOfType ty))
                  cover 20 (not free) "no free variables" $
                    cover 10 free "free variables" $
                      hasFreeVarsType ty === free .&&.
                        nameSetRaws (freeTyVarsOfType ty) === refFreeTyVarsOfType ty

      , testProperty "unsinking an alternative agrees with the oracle" $
          forAllShrink genSameRhsCase shrinkInScope $ \(InScope _ tm) -> case tm of
            Case _ _ alts ->
              conjoin
                [ counterexample (showT e) $
                    isJust (unsinkAlt p e) === not (any (`elem` refFree e) (patRaws p))
                | Alt p e <- alts ]
            _ -> property False
      ]
  ]
 where
  testVarsOf :: TestScope n -> [Term n]
  testVarsOf = foldr (:) [] . testVars
  testTyVarsOf :: TestScope n -> [Type n]
  testTyVarsOf = foldr (:) [] . testTyVars

  refFree :: Term n -> [Int]
  refFree e = refFreeTyVars e <> refFreeTmVars e

  patRaws :: Pat n l -> [Int]
  patRaws = \case
    DataPat _ tvs ids -> teleRaws tyNameRaw tyNameOf tvs <> teleRaws tmNameRaw tmNameOf ids
    _ -> []

-- | At every binder in a term, and in its types, check that moving a value out
-- of its scope succeeds if and only if the variables it binds are not among the
-- free variables of the value
unsinkChecks :: Term n -> [Property]
unsinkChecks = goTm
 where
  agree :: String -> Bool -> [Int] -> [Int] -> Property
  agree what ok bound free =
    counterexample (what <> ": binds " <> show bound <> ", free " <> show free) $
      ok === not (any (`elem` free) bound)

  goTy :: Type x -> [Property]
  goTy = \case
    ForAllTy b t ->
      agree "forall" (isJust (unsinkType (tyBinderNameBinders b) t))
        [tyNameRaw (tyNameOf b)] (nameSetRaws (freeTyVarsOfType t))
      : goTy (tyBinderKind b) <> goTy t
    VarTy _ k _ -> goTy k
    AppTy t1 t2 -> goTy t1 <> goTy t2
    AnnType _ t -> goTy t
    _ -> []

  goTm :: Term x -> [Property]
  goTm = \case
    Var _ ty _ -> goTy ty
    Lam b e ->
      agree "lambda" (isJust (unsinkIdBinder b e)) [tmNameRaw (tmNameOf b)] (nameSetRaws (freeTmVars e))
      : goTy (idBinderType b) <> goTm e
    TyLam b e ->
      agree "type lambda" (isJust (unsinkTyBinder b e)) [tyNameRaw (tyNameOf b)] (nameSetRaws (freeTyVars e))
      : goTy (tyBinderKind b) <> goTm e
    App e1 e2 -> goTm e1 <> goTm e2
    TyApp e ty -> goTm e <> goTy ty
    Let bs e ->
      agree "let" (isJust (unsinkTerm (bindNameBinders bs) e)) (bindRaws bs) (nameSetRaws (freeTmVars e))
      : (case bs of
           NonRec b x -> goTy (idBinderType b) <> goTm x
           Rec _ xs -> concatMap goTm xs)
      <> goTm e
    Case e ty alts -> goTm e <> goTy ty <> concatMap goAlt alts
    Cast e t1 t2 -> goTm e <> goTy t1 <> goTy t2
    Tick (Attributes ty x) e -> goTy ty <> goTm x <> goTm e
    Tick (NameMod _ ty) e -> goTy ty <> goTm e
    Tick _ e -> goTm e
    _ -> []

  goAlt :: Alt x -> [Property]
  goAlt (Alt p e) = case p of
    DataPat _ tvs ids ->
      let tvRaws = teleRaws tyNameRaw tyNameOf tvs
          idRaws = teleRaws tmNameRaw tmNameOf ids
          freeTvs = nameSetRaws (freeTyVars e)
      in  agree "pattern" (isJust (unsinkAlt p e)) (tvRaws <> idRaws)
            (freeTvs <> nameSetRaws (freeTmVars e))
          -- The types of the variable occurrences in the alternative
          : [ agree "pattern, type" (isJust (strengthenTyPat p ty)) tvRaws
                (nameSetRaws (freeTyVarsOfType ty))
            | ty <- occTypes e ]
          <> goTm e
    _ -> goTm e

  bindRaws :: Bind x y -> [Int]
  bindRaws = \case
    NonRec b _ -> [tmNameRaw (tmNameOf b)]
    Rec bs _ -> teleRaws tmNameRaw tmNameOf bs


  -- The types of the variable occurrences that are not under binders
  occTypes :: Term x -> [Type x]
  occTypes = \case
    Var _ ty _ -> [ty]
    App e1 e2 -> occTypes e1 <> occTypes e2
    TyApp e _ -> occTypes e
    Cast e _ _ -> occTypes e
    Tick _ e -> occTypes e
    Case e _ _ -> occTypes e
    _ -> []

-- | The raw names a telescope binds
teleRaws :: (forall z. k z -> Int) -> (forall x' y'. b x' y' -> k y') -> Tele b x y -> [Int]
teleRaws _ _ TNil = []
teleRaws raw nm (TCons b bs) = raw (nm b) : teleRaws raw nm bs
