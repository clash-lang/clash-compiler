{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.ANF": the regression tests
  of "Clash.Normalize.Transformations.ANF", run on the scoped 'Scoped.makeANF'
  and 'Scoped.nonRepANF' through 'Clash.Rewrite.Scoped.ViaFoil.viaFoil', and
  agreement with the current ones.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.ANF (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term
  ( Bind (..), NameMod (PrefixName), Pat (..), Term (..), TickInfo (NameMod)
  , mkApps )
import Clash.Core.Type (LitTy (SymTy), Type (ForAllTy, LitTy, VarTy), mkFunTy)
import Clash.Core.TysPrim (typeNatKind)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (emptyInScopeSet)
import qualified Clash.Normalize.Scoped.Transformations.ANF as Scoped
import Clash.Normalize.Transformations.ANF (makeANF, nonRepANF)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.Scoped (ScopedTransform (..))
import Clash.Rewrite.Scoped.ViaFoil (viaFoil)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteEnv, RewriteState (..))
import Clash.Util.Supply (newSupply)

import Clash.Tests.Normalize.Transformations.ANF (anfTests)
import Test.Clash.Rewrite
  ( addTy, assertAlphaEq, assertWellScoped, consDc, eqTy, globalId, intFunTy
  , intId, intLit, intTy, justDataCon, localId, maybeTy, mkDataCon
  , nothingDataCon, numTy, pairDataCon, pairTy, parseTyConTy
  , rewriteEnvWithTyCons, runSingleTransformation, tyVar, vecM, vecTy )

-- | The scoped 'Scoped.makeANF', as a rewrite of Clash Core
makeANFViaFoil :: NormRewrite
makeANFViaFoil = viaFoil "ANF" (ScopedTransform Scoped.makeANF)

-- | The scoped 'Scoped.nonRepANF', as a rewrite of Clash Core
nonRepANFViaFoil :: NormRewrite
nonRepANFViaFoil = $(asRewriteQ Scoped.nonRepANF)

-- | The current 'nonRepANF', as a rewrite
nonRepANFR :: NormRewrite
nonRepANFR = $(asRewriteQ nonRepANF)

-- | Global functions @k :: Int -> Int -> Int@ and @h :: Int -> Int@
kId, hId :: Id
kId = globalId User "k" 33 (intFunTy 2)
hId = globalId User "h" 50 (intFunTy 1)

-- | @k a b@
k :: Term -> Term -> Term
k e1 e2 = mkApps (Var kId) [Left e1, Left e2]

-- | @h a@
h :: Term -> Term
h = App (Var hId)

-- | Run a rewrite on a closed term, at its root, with the type constructors of
-- 'Test.Clash.Rewrite.testTyConMap'
runOn :: RewriteEnv -> NormRewrite -> Term -> IO Term
runOn env rw tm = do
  supply <- newSupply
  runSingleTransformation env def{_uniqSupply = supply} emptyInScopeSet rw tm

-- | The scoped 'makeANF' gives the same result as the current one, up to
-- alpha-equivalence, and its result doesn't shadow binders
agrees :: Term -> Assertion
agrees tm = do
  current <- runOn rewriteEnvWithTyCons makeANF tm
  scoped <- runOn rewriteEnvWithTyCons makeANFViaFoil tm
  assertAlphaEq current scoped
  assertWellScoped scoped

a, b, m, n, p, v, x, z :: Id
a = intId User "a" 101
b = intId User "b" 102
m = localId User "m" 103 (maybeTy intTy)
n = intId User "n" 104
p = localId User "p" 105 pairTy
v = localId User "v" 106 (vecTy (numTy 2) intTy)
x = intId User "x" 107
z = intId User "z" 108

-- | Alternatives of @Maybe Int@: @Nothing -> 0@, and @Just a -> e@
maybeAlts :: Term -> [(Pat, Term)]
maybeAlts e =
  [ (DataPat nothingDataCon [] [], intLit 0)
  , (DataPat justDataCon [] [a], e) ]

-- | @Cons \@m co x xs@, whose existential type variable @m@ doesn't occur in
-- the types of the fields, as when the length of the vector is known
vecConsPat :: Pat
vecConsPat = DataPat consDc [vecM] [co, a, xs]
 where
  co = localId User "co" 110 (eqTy typeNatKind (numTy 2) (addTy (numTy 1) (numTy 1)))
  xs = localId User "xs" 111 (vecTy (numTy 1) intTy)

agreementTests :: [TestTree]
agreementTests =
  [ testCase "pattern variable used by a let-bound alternative" $
      agrees (Lam m (Case (Var m) intTy (maybeAlts (k (h (Var a)) (Var a)))))
  , testCase "pattern variable as the right-hand side of one of two alternatives" $
      agrees (Lam m (Case (Var m) intTy (maybeAlts (Var a))))
  , testCase "pattern variable as the right-hand side of a single alternative" $
      agrees (Lam p (Case (Var p) intTy [(DataPat pairDataCon [] [a, b], Var b)]))
  , testCase "single alternative that doesn't use its pattern variables" $
      agrees (Lam x (Lam p (Case (Var p) intTy [(DataPat pairDataCon [] [a, b], k (Var x) (Var x))])))
  , testCase "existential type variable that doesn't occur in the fields" $
      agrees (Lam v (Lam x (Case (Var v) intTy [(vecConsPat, k (h (Var a)) (Var x))])))
  , testCase "nested let-expressions" $
      agrees (Lam x (Let (NonRec a (h (Var x)))
                       (Let (NonRec b (k (Var a) (h (Var a))))
                         (k (Var b) (h (Var b))))))
  , testCase "recursive let-expression" $
      agrees (Lam x (Letrec [(a, k (Var b) (Var x)), (b, h (Var a))] (k (Var a) (Var b))))
  , testCase "let-bound subject" $
      agrees (Lam x (Case (h (Var x)) intTy [(DefaultPat, k (Var x) (h (Var x)))]))
  , testCase "let-expression in an argument" $
      agrees (Lam x (k (Let (NonRec n (h (Var x))) (k (Var n) (Var n))) (Var x)))
  , testCase "name modifier around the term" $
      agrees (Lam x (Tick (NameMod PrefixName (LitTy (SymTy "pre")))
                       (Let (NonRec a (h (Var x))) (k (Var a) (Var a)))))
  , testCase "no let-bindings" $
      agrees (Lam x (k (Var x) (intLit 1)))
  ]

-- | Bindings created under a lambda in the term stay under it, unlike in
-- Clash Core, which lets @h z@ out of @\\z -> k (h z) z@ without
-- 'Clash.Normalize.Transformations.ANF.nonRepANF' (see "ANF does not lift
-- expressions out of lambdas")
anfLambdaRegion :: Assertion
anfLambdaRegion =
  runOn def makeANFViaFoil (Lam x (App (Var fHo) (Lam z (k (h (Var z)) (Var z)))))
    >>= assertWellScoped
 where
  fHo = globalId User "fHo" 51 (mkFunTy (intFunTy 1) intTy)

-- | An alternative whose existential type variable doesn't occur in the
-- types of its fields, but in its right-hand side, keeps its let-bindings,
-- like an alternative that binds existential type variables. Clash Core lets
-- @g \@e y@ out of it:
--
-- > \s -> case s of MkE @e (y :: Int) -> k (g @e y) y
anfExistentialInBody :: Assertion
anfExistentialInBody =
  runOn def makeANFViaFoil
    (Lam s (Case (Var s) intTy
      [ (DataPat mkE [e] [y]
        , k (App (TyApp (Var gId) (VarTy e)) (Var y)) (Var y)) ]))
    >>= assertWellScoped
 where
  e = tyVar User "e" 1301
  -- MkE :: forall e. Int -> E
  mkE :: DataCon
  mkE = mkDataCon "MkE" 1302 [] [e] [intTy] (parseTyConTy "E")
  s = localId User "s" 1303 (parseTyConTy "E")
  y = intId User "y" 1304
  c = tyVar User "c" 1305
  gId = globalId User "g" 1306 (ForAllTy c (intFunTy 1))

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Scoped.Transformations.ANF"
    [ testGroup "regression tests" (anfTests makeANFViaFoil nonRepANFViaFoil)
    , testGroup "agrees with the current makeANF" agreementTests
    , testCase "nonRepANF agrees with the current nonRepANF" $ do
        -- MkP :: Int -> (Int -> Int) -> P
        let mkP = mkDataCon "MkP" 1001 [] [] [intTy, intFunTy 1] (parseTyConTy "P")
            tm = Lam x (mkApps (Data mkP)
                   [ Left (Var x)
                   , Left (Let (NonRec a (h (Var x))) (Lam z (k (Var a) (Var z)))) ])
        current <- runOn rewriteEnvWithTyCons nonRepANFR tm
        scoped <- runOn rewriteEnvWithTyCons nonRepANFViaFoil tm
        assertAlphaEq current scoped
    , testCase "makeANF keeps let-bindings under a lambda" anfLambdaRegion
    , testCase "makeANF keeps let-bindings in an alternative that mentions its existential type variable"
        anfExistentialInBody
    ]
