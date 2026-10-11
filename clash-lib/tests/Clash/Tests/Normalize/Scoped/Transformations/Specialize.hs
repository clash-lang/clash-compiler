{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Specialize": the
  regression tests of 'Clash.Normalize.Transformations.Specialize.specialize',
  run on the scoped 'specialize' through
  'Clash.Rewrite.Scoped.ViaFoil.viaFoil', and agreement with it.
-}

module Clash.Tests.Normalize.Scoped.Transformations.Specialize (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (IntLiteral))
import Clash.Core.Name (NameSort (User), noSrcSpan)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (extendInScopeSetList)
import qualified Clash.Normalize.Scoped.Transformations.Specialize as Scoped
import qualified Clash.Normalize.Transformations.Specialize as C
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.Scoped (ScopedTransform (..))
import Clash.Rewrite.Scoped.ViaFoil (viaFoil)
import Clash.Rewrite.Types (RewriteState (..))
import Clash.Rewrite.Util (runRewrite)
import Clash.Util.Supply (Supply, newSupply)

import Clash.Tests.Normalize.Transformations.Specialize (specializeTests)
import Test.Clash.Rewrite
  ( assertAlphaEq, globalId, globalsByName, inScopeOf, intFunTy, intId, intTy
  , kindedTyVar, localId, mkBindingMap, runRewriteTest, tyVar )
import Test.Clash.Scoped (assertNewBindingsAgree)

-- | The scoped 'Scoped.specialize', as a rewrite of Clash Core
specializeViaFoil :: NormRewrite
specializeViaFoil = viaFoil "specialize" (ScopedTransform Scoped.specialize)

-- | Global functions with bindings: @f :: Int -> Int@, @fPoly :: forall b. b
-- -> Int -> Int@, @fT :: forall b. b -> b@, and @fHo :: (Int -> Int) -> Int@;
-- and global functions @g, k :: Int -> Int -> Int@, of which only @k@ has a
-- binding
f, fPoly, fT, fHo, g, k :: Id
f = globalId User "f" 10 (intFunTy 1)
fPoly = globalId User "fPoly" 14 (C.ForAllTy b (C.mkFunTy (C.VarTy b) (intFunTy 1)))
fT = globalId User "fT" 16 (C.ForAllTy b (C.mkFunTy (C.VarTy b) (C.VarTy b)))
fHo = globalId User "fHo" 17 (C.mkFunTy (intFunTy 1) intTy)
g = globalId User "g" 11 (intFunTy 2)
k = globalId User "k" 18 (intFunTy 2)

b :: C.TyVar
b = tyVar User "b" 51

-- | The bindings of the global functions
bindings :: [(Id, C.Term)]
bindings =
  [ (f, C.Lam p (C.Var p))
  , (fPoly, C.TyLam b (C.Lam (localId User "y" 21 (C.VarTy b)) (C.Lam p (C.Var p))))
  , (fT, C.TyLam b (C.Lam (localId User "y" 21 (C.VarTy b)) (C.Var (localId User "y" 21 (C.VarTy b)))))
  , (fHo, C.Lam h (C.App (C.Var h) (C.Literal (IntLiteral 1))))
  , (k, C.Lam p (C.Lam q (C.Var q)))
  ]
 where
  p = intId User "p" 20
  q = intId User "q" 22
  h = localId User "h" 23 (intFunTy 1)

-- | Run a rewrite on a term, with the given type and term variables in scope
runSpecialize
  :: Supply -> NormRewrite -> [C.TyVar] -> [Id] -> C.Term
  -> IO (C.Term, RewriteState NormalizeState, RewriteState NormalizeState)
runSpecialize supply rw tvs ids tm = do
  let st0 :: RewriteState NormalizeState
      st0 = def
        { _bindings = mkBindingMap bindings
        , _uniqSupply = supply
        , _curFun = (globalId User "top" 42 (intFunTy 1), noSrcSpan) }
      is = extendInScopeSetList (inScopeOf ids) tvs
  (res, st1, _) <- runRewriteTest def st0 (runRewrite "specialize" is rw tm)
  pure (res, st0, st1)

-- | The scoped 'Scoped.specialize' agrees with
-- 'Clash.Normalize.Transformations.Specialize.specialize': the results are
-- alpha-equivalent, up to the uniques of new functions, which are created
-- with alpha-equivalent bodies
specializeAgrees :: [C.TyVar] -> [Id] -> C.Term -> Assertion
specializeAgrees tvs ids tm = do
  supply <- newSupply
  (expected, st0, st1) <- runSpecialize supply C.specialize tvs ids tm
  (actual, _, st2) <- runSpecialize supply specializeViaFoil tvs ids tm
  assertAlphaEq (globalsByName expected) (globalsByName actual)
  assertNewBindingsAgree st0 st1 st2

tests :: TestTree
tests =
  testGroup "Clash.Tests.Normalize.Scoped.Transformations.Specialize"
    [ testGroup "regression tests of Clash Core, through viaFoil"
        (specializeTests specializeViaFoil)
    , testGroup "agrees with Clash Core"
        [ testCase "f (g x y)" $
            specializeAgrees [] [x, y] (C.App (C.Var f) (app g [C.Var x, C.Var y]))
        , testCase "fPoly @a (x :: a) (g u v)" $
            specializeAgrees [a] [xa, u, v]
              (C.App (C.mkApps (C.Var fPoly) [Right (C.VarTy a), Left (C.Var xa)])
                     (app g [C.Var u, C.Var v]))
        , testCase "f (case (x :: a) of {}), a :: kk" $
            specializeAgrees [kk, ak] [xak] (C.App (C.Var f) (C.Case (C.Var xak) intTy []))
        , testCase "fT @Int" $
            specializeAgrees [] [] (C.TyApp (C.Var fT) intTy)
        , testCase "fHo (k x)" $
            specializeAgrees [] [x] (C.App (C.Var fHo) (app k [C.Var x]))
        , testCase "fHo (g x)" $
            specializeAgrees [] [x] (C.App (C.Var fHo) (app g [C.Var x]))
        , testCase "(\\z -> z) (g x y)" $
            specializeAgrees [] [x, y]
              (C.App (C.Lam z (C.Var z)) (app g [C.Var x, C.Var y]))
        ]
    ]
 where
  x = intId User "x" 12
  y = intId User "y" 13
  z = intId User "z" 19
  u = intId User "u" 15
  v = intId User "v" 16
  a = tyVar User "a" 50
  xa = localId User "x" 12 (C.VarTy a)
  kk = tyVar User "kk" 52
  ak = kindedTyVar User "a" 50 (C.VarTy kk)
  xak = localId User "x" 12 (C.VarTy ak)

  app hd = C.mkApps (C.Var hd) . map Left
