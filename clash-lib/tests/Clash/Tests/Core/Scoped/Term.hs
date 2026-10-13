{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Term". The expected results are those of the
  functions of "Clash.Core.Term" that these replace.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Term (tests) where

import GHC.Types.SrcLoc (noSrcSpan)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Basic as C (NameMod (PrefixName))

import Clash.Core.Scoped.Builder
  ( TmB, appB, deDupTickB, nameModTickB, noDeDupTickB, primB, srcSpanTickB, tickB, tyAppB)
import Clash.Core.Scoped.NameMap (emptyGlobalEnv, insertGlobalEnv)
import Clash.Core.Scoped.Subst (substGlobals)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term

import Test.Clash.Scoped

-- | A primitive @p :: forall a. a -> a -> a@
primP :: TmB
primP = primB (testPrim "p" "forall a. a -> a -> a")

-- | @p \@Int@, with a tick around it
pInt :: TmB
pInt = tickB deDupTickB (tyAppB primP (testTypeB "Int"))

-- | Build a term with the builders in the scope of @x, y :: Int@
withXY :: (forall n. Distinct n => TestScope n -> Assertion) -> Assertion
withXY = withTestScope [varD "x" "Int", varD "y" "Int"]

-- | @p \@Int (tick x) y@, with ticks in between the applications, see 'withXY'
appTerm :: TmB
appTerm =
  tickB (srcSpanTickB noSrcSpan)
    (appB (appB pInt (tickB noDeDupTickB (testTermB "x"))) (testTermB "y"))

-- | A term with lambdas and type lambdas at its head
absTerm :: TestTerm
absTerm = "\\(a :: Type) (x :: a) (b :: Type) (y :: b) -> x"

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Term"
  [ testCase "substGlobals" $ do
      -- @\(x :: Int) -> f x (let (y :: Int) = g in \(x :: Int) -> y)@, with
      -- @f := \(x :: Int) (y :: Int) -> x@ and @g := 7@: their binders have
      -- the names of the binders they end up under
      let f = testGlobalId "f_G10" "Int -> Int -> Int"
          g = testGlobalId "g_G11" "Int"
          tm = closedTerm
            "\\(x :: Int) -> f_G10 x (let { !(y :: Int) = g_G11 } in \\(x :: Int) -> y)"
          genv = insertGlobalEnv f (closedTerm "\\(x :: Int) (y :: Int) -> x")
                   (insertGlobalEnv g (closedTerm "7") emptyGlobalEnv)
          res = substGlobals genv tm
      assertTermIs emptyTestScope
        "\\(x :: Int) -> (\\(x :: Int) (y :: Int) -> x) x (let { !(y :: Int) = 7 } in \\(x :: Int) -> y)"
        res
      assertLintClean emptyTestScope res

  , testCase "collectArgs and mkApps" $ withXY $ \ts -> do
      let tm = termIn ts (builtTerm appTerm)
      -- The ticks around the head of the application, and around it, are
      -- dropped
      assertTermAlphaEq
        (termIn ts (builtTerm (appB (appB (tyAppB primP (testTypeB "Int")) (tickB noDeDupTickB (testTermB "x"))) (testTermB "y"))))
        (uncurry mkApps (collectArgs tm))

  , testCase "collectArgs" $ withXY $ \ts -> do
      let (fun, args) = collectArgs (termIn ts (builtTerm appTerm))
      assertTermAlphaEq (termIn ts (builtTerm primP)) fun
      length args @?= 3
      case args of
        [Right t, Left e1, Left e2] -> do
          assertTypeIs ts "Int" t
          assertTermAlphaEq (termIn ts (builtTerm (tickB noDeDupTickB (testTermB "x")))) e1
          assertTermIs ts "y" e2
        _ -> assertFailure "unexpected arguments"

  , testCase "collectArgsTicks" $ withXY $ \ts -> do
      let (_, _, ticks) = collectArgsTicks (termIn ts (builtTerm appTerm))
      -- The tick around the head, and the one around the application
      length ticks @?= 2

  , testCase "primArg" $ withXY $ \ts ->
      primArg (termIn ts (builtTerm appTerm)) @?= Just ("p", 1, 2)

  , testCase "mkTmApps and mkTyApps" $ do
      let tm = closedTerm "\\(x :: Int) -> x"
      assertTermIs emptyTestScope "(\\(x :: Int) -> x) @Int (\\(x :: Int) -> x)"
        (mkTmApps (mkTyApps tm [closedType "Int"]) [tm])

  , testCase "stripTicks" $ withXY $ \ts ->
      assertTermAlphaEq
        (termIn ts (builtTerm (appB (appB pInt (tickB noDeDupTickB (testTermB "x"))) (testTermB "y"))))
        (stripTicks (termIn ts (builtTerm appTerm)))

  , testCase "stripAllTicks" $ withXY $ \ts ->
      assertTermIs ts "p_G0 @Int x y" (globalP (stripAllTicks (termIn ts (builtTerm appTerm))))

  , testCase "collectTicks and mkTicks" $ withXY $ \ts -> do
      let tm = termIn ts (builtTerm (tickB deDupTickB (tickB deDupTickB appTerm)))
      -- The duplicate tick is removed
      assertTermAlphaEq
        (termIn ts (builtTerm (tickB deDupTickB appTerm)))
        (uncurry mkTicks (collectTicks tm))

  , testCase "mkTicks removes duplicates up to alpha-equivalence" $ withXY $ \ts -> do
      -- Two @NameMod@ ticks with alpha-equivalent types
      let nameMod t = tickB (nameModTickB C.PrefixName (testTypeB t))
          tm = termIn ts (builtTerm (nameMod "forall a. a" (nameMod "forall b. b" (testTermB "x"))))
          res = uncurry mkTicks (collectTicks tm)
      assertTermAlphaEq (termIn ts (builtTerm (nameMod "forall a. a" (testTermB "x")))) res
      length (snd (collectTicks res)) @?= 1

  , testCase "partitionTicks" $ withXY $ \ts -> do
      let (_, _, ticks) = collectArgsTicks (termIn ts (builtTerm appTerm))
          (src, other) = partitionTicks ticks
      (length src, length other) @?= (1, 1)

  , testCase "collectBndrs and mkAbstraction" $
      withTerm absTerm $ \_ tm -> assertTermAlphaEq tm (collectBndrs tm mkAbsFlipped)

  , testCase "openBndrs and mkAbstraction" $
      withTerm absTerm $ \ts tm ->
        assertTermAlphaEq tm (openBndrs (testScope ts) tm (const mkAbsFlipped))

  , testCase "collectBndrs collects all binders at the head" $
      withTerm absTerm $ \_ tm -> collectBndrs tm (\bs _ -> teleLength bs) @?= 4

    -- The binders of the term clash with the sunk variable
  , testCase "openBndrs renames binders that clash with the scope" $
      withTerm absTerm $ \ts0 tm0 -> withClashing ts0 [sunkD] tm0 $ \ts tm ->
        openBndrs (testScope ts) tm $ \_ bs body -> do
          assertBool "binders are above the sunk variable" (all (> 1000000) (teleRaws bs))
          assertTermAlphaEq tm (mkAbstraction body bs)

  , testCase "mkLams and mkTyLams" $
      withTerm "\\(a :: Type) (x :: a) -> x" $ \_ tm -> case tm of
        TyLam ab (Lam xb e) ->
          assertTermAlphaEq tm (mkTyLams (mkLams e (TCons xb TNil)) (TCons ab TNil))
        _ -> assertFailure "unexpected shape"

  , testCase "appendTele and splitTele" $
      withTerm absTerm $ \_ tm ->
        assertTermAlphaEq tm
          (collectBndrs tm $ \bs body ->
             splitTele 2 bs $ \pre post -> mkAbstraction body (appendTele pre post))

  , testCase "walkTerm" $ withXY $ \ts -> do
      let count :: Term x -> Maybe ()
          count _ = Just ()
      length (walkTerm count (termIn ts (builtTerm appTerm))) @?= 9
      withTerm absTerm $ \_ tm -> length (walkTerm count tm) @?= 5
  ]
 where
  mkAbsFlipped :: Tele AbsBinder n l -> Term l -> Term n
  mkAbsFlipped bs body = mkAbstraction body bs

  teleRaws :: Tele AbsBinder n l -> [Int]
  teleRaws TNil = []
  teleRaws (TCons (AbsId b) bs) = tmNameRaw (tmNameOf b) : teleRaws bs
  teleRaws (TCons (AbsTy b) bs) = tyNameRaw (tyNameOf b) : teleRaws bs

  -- Replace the primitive @p@ by a global with the same name, which the
  -- parser can express
  globalP :: Term n -> Term n
  globalP = \case
    App e1 e2 -> App (globalP e1) e2
    TyApp e ty -> TyApp (globalP e) ty
    Prim p | primName p == "p" -> GlobalVar (testGlobalId "p_G0" "forall a. a -> a -> a")
    e -> e
