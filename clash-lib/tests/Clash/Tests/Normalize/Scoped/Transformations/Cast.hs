{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Cast": the tests of the
  transformations of Clash Core, run on the ports through
  'Test.Clash.Scoped.ViaFoil.viaFoil', and agreement of the ports with the
  transformations of Clash Core.

  The transformations on casts never fired while capturing the corpus of the
  differential harness, so these tests construct their inputs. They include
  the inputs of the regression tests of 'Clash.Normalize.Transformations.Cast.caseCast'
  (in "Clash.Tests.Normalize.Transformations.Case") and of
  'Clash.Normalize.Transformations.Cast.splitCastWork' (in
  "Clash.Tests.Normalize.Transformations"), whose bugs can't be written in
  scoped Core: the binders of the alternatives and the let-binders can't
  capture the variables they are moved past.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Cast (tests) where

import Control.Exception (try)
import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Bind (..), Pat (..), Term (..), TickInfo (..))
import qualified Clash.Core.Term as C
import Clash.Core.Type (Type (VarTy), mkFunTy)
import Clash.Core.Var (Id, TyVar)
import Clash.Core.VarEnv (extendInScopeSet)
import qualified Clash.Normalize.Scoped.Transformations.Cast as Scoped
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.StrategyDSL.TH (asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteState (..), TransformContext (..))
import Clash.Util (ClashException, noSrcSpan)
import Clash.Util.Supply (freshId, newSupply)

import Clash.Tests.Normalize.Transformations.Cast
  (CastRewrites (..), castTests, currentRewrites)
import Test.Clash.Rewrite
  ( assertFires, globalId, inScopeOf, intFunTy, intId, intLit, intTy, localId, mkTestDataCon
  , pairDataCon, pairTy, parseTyConTy, runRewriteTest, tyVar )
import Test.Clash.Scoped (assertRewritesAgree)
import Test.Clash.Scoped.ViaFoil (viaFoil)
import Clash.Core.Scoped.Convert (mkGlobalIdFromClash)

-- | The ports, on terms of Clash Core: through
-- 'Test.Clash.Scoped.ViaFoil.viaFoil'
scopedRewrites :: CastRewrites
scopedRewrites = CastRewrites
  { crCaseCast = (viaFoil "caseCast" $(asScopedRewriteQ Scoped.caseCast))
  , crElimCastCast = (viaFoil "elimCastCast" $(asScopedRewriteQ Scoped.elimCastCast))
  , crLetCast = (viaFoil "letCast" $(asScopedRewriteQ Scoped.letCast))
  , crSplitCastWork = (viaFoil "splitCastWork" $(asScopedRewriteQ Scoped.splitCastWork))
  }

-- | Assert that a transformation and its port agree on a term, with the given
-- variables in scope, and whether they fire
agree :: Bool -> (CastRewrites -> NormRewrite) -> [Id] -> [TyVar] -> Term -> Assertion
agree fires rw ids tvs tm = do
  supply <- newSupply
  let st = def{_uniqSupply = supply} :: RewriteState NormalizeState
      ctx = TransformContext (foldl extendInScopeSet (inScopeOf ids) tvs) []
  assertFires fires def st ctx (rw currentRewrites) tm
  assertRewritesAgree def st ctx (rw currentRewrites) (rw scopedRewrites) tm

-- * Variables

s, x, y, z :: Id
s = localId User "s" 30 pairTy
x = intId User "x" 31
y = intId User "y" 32
z = intId User "z" 33

-- | Global functions @f :: Int -> Int@ and @g :: Int -> Int@
f, g :: Term
f = Var (globalId User "f" 40 (intFunTy 1))
g = Var (globalId User "g" 41 (intFunTy 1))

-- | @a@, a free type variable
a :: TyVar
a = tyVar User "a" 50

aTy :: Type
aTy = VarTy a

-- | @E@, a data type with a single constructor 'mkE'
eTy :: Type
eTy = parseTyConTy "E"

-- | @MkE :: forall b. b -> E@, a data constructor with an existential type
-- variable
mkE :: DataCon
mkE = mkTestDataCon "MkE" 1001 [] [b] [VarTy b] eTy
 where
  b = tyVar User "b" 1002

-- | A cast from and to @Int@
castInt :: Term -> Term
castInt e = Cast e intTy intTy

srcTick :: TickInfo
srcTick = NameMod C.PrefixName (parseTyConTy "Pfx")

-- * caseCast

caseCastTests :: TestTree
caseCastTests = testGroup "caseCast"
  [ testCase "pushes a cast into the alternatives" $
      agree True crCaseCast [s] [] $
        Cast
          (Case (Var s) intTy
            [ (DataPat pairDataCon [] [x, y], App f (Var x))
            , (DefaultPat, intLit 3) ])
          intTy (mkFunTy intTy intTy)
  , testCase "looks through ticks" $
      agree True crCaseCast [s] [] $
        castInt (Tick srcTick (Case (Var s) intTy [(DataPat pairDataCon [] [x, y], Var y)]))

    -- The input of the regression test "caseCast deshadows existentials and
    -- sets the type": the existential @a'@ shadows the free @a@ of the cast
  , testCase "existential type variables that shadow the cast's" $ do
      let a' = tyVar User "a'" 50
          sE = localId User "s" 30 eTy
          yA = localId User "y" 32 aTy
          xA' = localId User "x" 31 (VarTy a')
      agree True crCaseCast [sE, yA] [a] $
        Cast (Case (Var sE) aTy [(DataPat mkE [a'] [xA'], Var yA)]) aTy intTy
  , testCase "doesn't fire on another term" $
      agree False crCaseCast [x] [] (castInt (App f (Var x)))
  ]

-- * letCast

letCastTests :: TestTree
letCastTests = testGroup "letCast"
  [ testCase "pushes a cast into the body of a let" $
      agree True crLetCast [x] [] $
        castInt (Let (NonRec y (App f (Var x))) (App g (Var y)))
  , testCase "pushes a cast into the body of a letrec" $
      agree True crLetCast [x] [] $
        castInt (Letrec [(y, App f (Var z)), (z, App g (Var y))] (Var y))
  , testCase "the cast may mention variables the let shadows" $
      -- The cast's type variable is @a@; the let's binder @x@ shadows the
      -- free @x@
      agree True crLetCast [x] [a] $
        Cast (Let (NonRec x (App f (Var x))) (Var x)) intTy aTy
  , testCase "doesn't fire on another term" $
      agree False crLetCast [x] [] (castInt (Var x))
  ]

-- * elimCastCast

elimCastCastTests :: TestTree
elimCastCastTests = testGroup "elimCastCast"
  [ testCase "eliminates back to back casts" $
      agree True crElimCastCast [x] [a] $
        Cast (Cast (Var x) intTy aTy) aTy intTy
  , testCase "looks through ticks" $
      agree True crElimCastCast [x] [a] $
        Cast (Tick srcTick (Cast (Var x) intTy aTy)) aTy intTy
  , testCase "doesn't fire on another term" $
      agree False crElimCastCast [x] [] (castInt (Var x))
  , testCase "both fail on casts whose types don't line up" $ do
      let tm = Cast (Cast (Var x) intTy aTy) intTy intTy
          run rw = do
            supply <- newSupply
            let st = def{_uniqSupply = supply, _curFun = (mkGlobalIdFromClash (globalId User "top" 60 intTy), noSrcSpan)}
                       :: RewriteState NormalizeState
                ctx = TransformContext (extendInScopeSet (inScopeOf [x]) a) []
            r <- try (runRewriteTest def st (crElimCastCast rw ctx tm))
            case r of
              Left (_ :: ClashException) -> pure ()
              Right _ -> assertFailure "no exception"
      run currentRewrites
      run scopedRewrites
  ]

-- * splitCastWork

splitCastWorkTests :: TestTree
splitCastWorkTests = testGroup "splitCastWork"
  [ testCase "splits a NonRec binding" $
      agree True crSplitCastWork [x] [] $
        Let (NonRec y (castInt (App f (Var x)))) (Var y)
  , testCase "a NonRec binding that shadows a free variable of its cast" $
      agree True crSplitCastWork [x] [] $
        Let (NonRec x (castInt (App f (Var x)))) (Var x)
  , testCase "looks through ticks" $
      agree True crSplitCastWork [x] [] $
        Let (NonRec y (Tick srcTick (castInt (App f (Var x))))) (Var y)
  , testCase "splits the bindings of a letrec in place" $
      agree True crSplitCastWork [x] [] $
        Letrec
          [ (y, castInt (App f (Var z)))
          , (z, App g (Var x))
          , (s, Cast (App g (Var y)) intTy pairTy) ]
          (Var s)

    -- The input of the regression test "splitCastWork binders are fresh
    -- w.r.t. the let's binders": @y@ has the unique the supply hands out
    -- first
  , testCase "new binders are fresh with respect to the let's binders" $ do
      supply <- newSupply
      let (uy, supply1) = freshId supply
          (ux, _) = freshId supply1
          yF = intId User "y" uy
          xF = intId User "x" ux
          tm = Letrec [(yF, intLit 3), (xF, castInt (App g (Var yF)))] (Var xF)
      let st = def{_uniqSupply = supply} :: RewriteState NormalizeState
          ctx = TransformContext (inScopeOf []) []
      assertFires True def st ctx (crSplitCastWork currentRewrites) tm
      assertRewritesAgree def st ctx (crSplitCastWork currentRewrites) (crSplitCastWork scopedRewrites) tm
  , testCase "doesn't split a cast of a variable" $
      agree False crSplitCastWork [x] [] $
        Letrec [(y, castInt (Var x)), (z, castInt f)] (Var y)
  , testCase "doesn't split a cast of a cast" $
      agree False crSplitCastWork [x] [] $
        Let (NonRec y (castInt (castInt (App f (Var x))))) (Var y)
  ]

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.Cast"
  [ testGroup "Regression tests" (castTests scopedRewrites)
  , caseCastTests
  , letCastTests
  , elimCastCastTests
  , splitCastWorkTests
  ]
