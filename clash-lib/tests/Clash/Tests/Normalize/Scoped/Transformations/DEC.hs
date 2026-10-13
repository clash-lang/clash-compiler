{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.DEC": the regression tests
  of DEC of Clash Core, run on the port through
  'Test.Clash.Scoped.ViaFoil.viaFoil', and agreement of the port with DEC of
  Clash Core on inputs that cover its cases.

  The regression tests are about binders in the alternatives that shadow, or
  are captured by, variables outside of them. In scoped Clash Core, binders
  don't shadow, and DEC can only move the parts of an application out of the
  alternatives that don't refer to their binders, so these bugs can't be
  written there. The tests still run on Clash Core terms with such binders,
  and test the conversion to scoped Core and the port together.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.DEC (tests) where

import qualified Control.Lens as Lens
import Data.Default (def)
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.FreeVars (termFreeTyVars)
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term
  (Bind (..), Pat (..), PrimInfo (..), TickInfo (..), WorkInfo (..), mkApps)
import qualified Clash.Core.Term as C
import Clash.Core.TyCon (TyCon (..))
import Clash.Core.Type (LitTy (..), Type (..), mkFunTy)
import Clash.Core.TysPrim (integerPrimTy, naturalPrimTy)
import Clash.Core.Var (Id, TyVar)
import Clash.Core.VarEnv (extendInScopeSetList)
import qualified Clash.Data.UniqMap as UniqMap
import qualified Clash.Normalize.Scoped.Transformations.DEC as Scoped
import Clash.Normalize.Transformations.DEC (disjointExpressionConsolidationWorker)
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.StrategyDSL (onCase, toTransformation)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ, asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..))
import Clash.Rewrite.Util (runRewrite)
import Clash.Util.Supply (newSupply)

import Clash.Tests.Normalize.Transformations.DEC (decTests, mkTDc, tN, tTy)
import Test.Clash.Differential (KnownDifference, Outcome (..), letrecOrder, recToNonRec)
import Test.Clash.Differential.Sites (compareAtSite)
import Test.Clash.Rewrite
  ( globalId, heapLookupEvaluator, inScopeOf, intFunTy, intId, intTy
  , justDataCon, localId, maybeTy, mkTestAlgTyCon, mkBindingMap, nothingDataCon
  , pairDataCon, pairTy, parseTyConName, runRewriteTest, showPprU, testTyConMap, tyVar, withTupleTyCons
  , withTyConMap )
import Test.Clash.Scoped.ViaFoil (viaFoil)

-- | DEC of Clash Core
decCurrent :: NormRewrite
decCurrent =
  $(asRewriteQ (toTransformation "DEC" (onCase 'disjointExpressionConsolidationWorker)))

-- | The port of DEC, on terms of Clash Core: through
-- 'Test.Clash.Scoped.ViaFoil.viaFoil'
decScoped :: NormRewrite
decScoped = (viaFoil "disjointExpressionConsolidation" $(asScopedRewriteQ Scoped.disjointExpressionConsolidation))

-- | The known differences of the port, see
-- "Clash.Normalize.Scoped.Transformations.DEC"
knownDifferences :: [KnownDifference]
knownDifferences = [recToNonRec, letrecOrder]

-- | An environment with the type constructors of 'testTyConMap', @T@, and
-- tuples, and an evaluator that doesn't reduce primitives
env :: RewriteEnv
env = (withTupleTyCons 3 (withTyConMap tcm def)) {_evaluator = heapLookupEvaluator}
 where
  tcm = UniqMap.insert (tyConName tTc) tTc testTyConMap
  tTc = mkTestAlgTyCon (parseTyConName "T") [] [mkTDc]

-- | Run DEC at the root of a term, with the given variables in scope. Gives
-- the result, and whether it signaled a change.
runDEC :: NormRewrite -> [TyVar] -> [Id] -> C.Term -> IO (C.Term, Bool)
runDEC dec tvs ids tm = do
  supply <- newSupply
  let st :: RewriteState NormalizeState
      st = def
        { _bindings = mkBindingMap [(f, C.Lam a (C.Var a))]
        , _uniqSupply = supply
        , _globalHeap = (mempty, 0) }
      is = extendInScopeSetList (inScopeOf ids) tvs
  (res, _, ch) <- runRewriteTest env st (runRewrite "DEC" is dec tm)
  pure (res, ch)
 where
  a = intId User "a" 299

-- | Assert that the port agrees with DEC of Clash Core on a term, up to the
-- known differences, and whether both fire
decAgrees :: Bool -> [TyVar] -> [Id] -> C.Term -> Assertion
decAgrees fires tvs ids tm = do
  (expected, ch1) <- runDEC decCurrent tvs ids tm
  (actual, ch2) <- runDEC decScoped tvs ids tm
  let shown = "Clash Core:\n" <> showPprU expected <> "\nscoped:\n" <> showPprU actual
  assertBool ("DEC of Clash Core fired: " <> show ch1 <> "\n" <> shown) (ch1 == fires)
  assertBool ("the port fired: " <> show ch2 <> "\n" <> shown) (ch2 == fires)
  case compareAtSite knownDifferences expected actual of
    Different _ -> assertFailure ("different results\n" <> shown)
    _ -> pure ()

-- * Variables

s, t, x, y, z, w :: Id
s = intId User "s" 200
t = intId User "t" 201
x = intId User "x" 202
y = intId User "y" 203
z = intId User "z" 204
w = intId User "w" 205

-- | Global functions @f, g :: Int -> Int@ and @k :: Int -> Int -> Int@
f, g, k :: Id
f = globalId User "f" 210 (intFunTy 1)
g = globalId User "g" 211 (intFunTy 1)
k = globalId User "k" 212 (intFunTy 2)

-- | Global function @h :: forall a. a -> Int@
h :: Id
h = globalId User "h" 213 (ForAllTy hA (mkFunTy (VarTy hA) intTy))
 where
  hA = tyVar User "a" 214

-- | Global function @hof :: (Int -> Int) -> Int -> Int@
hof :: Id
hof = globalId User "hof" 215 (mkFunTy (intFunTy 1) (intFunTy 1))

-- | Local function @q :: Int -> Int@
q :: Id
q = localId User "q" 216 (intFunTy 1)

-- | @p :: Pair@, @m :: Maybe Int@ and @e :: T@
p, m, e :: Id
p = localId User "p" 217 pairTy
m = localId User "m" 218 (maybeTy intTy)
e = localId User "e" 219 tTy

-- | @n@, a type variable in scope of the case-expression
n :: TyVar
n = tyVar User "n" 220

app :: Id -> [C.Term] -> C.Term
app fun = mkApps (C.Var fun) . map Left

-- | A case-expression on @s@ with the given alternatives, the first of which
-- is the default one
caseS :: [C.Term] -> C.Term
caseS alts = C.Case (C.Var s) intTy
  (zip (DefaultPat : [LitPat (IntLiteral i) | i <- [1 ..]]) alts)

-- * Primitives

-- | A primitive of the given name and type
prim :: Text.Text -> Type -> PrimInfo
prim nm ty = PrimInfo
  { primName = nm
  , primType = ty
  , primWorkInfo = WorkVariable
  , primMultiResult = C.SingleResult
  , primUnfolding = C.NoUnfolding
  }

integerFunTy :: Int -> Type
integerFunTy i = foldr mkFunTy integerPrimTy (replicate i integerPrimTy)

-- | @integerMul@, an interesting primitive unless an argument is a power of
-- two
integerMul :: PrimInfo
integerMul = prim "GHC.Num.Integer.integerMul" (integerFunTy 2)

-- | @integerQuot@, an interesting primitive unless its divisor is a power of
-- two
integerQuot :: PrimInfo
integerQuot = prim "GHC.Num.Integer.integerQuot" (integerFunTy 2)

-- | A higher-order primitive @hoPrim :: (Integer -> Integer) -> Integer ->
-- Integer@, which is interesting if its function argument is
hoPrim :: PrimInfo
hoPrim = prim "hoPrim" (mkFunTy (integerFunTy 1) (integerFunTy 1))

-- | A primitive @plainPrim :: Integer -> Integer@, which isn't interesting
plainPrim :: PrimInfo
plainPrim = prim "plainPrim" (integerFunTy 1)

-- | @Clash.Sized.Internal.Unsigned.fromInteger# :: forall n. Natural ->
-- Integer -> Integer@ (with a simplified result type)
fromIntegerU :: PrimInfo
fromIntegerU = prim "Clash.Sized.Internal.Unsigned.fromInteger#"
  (ForAllTy nU (mkFunTy naturalPrimTy (integerFunTy 1)))
 where
  nU = tyVar User "n" 221

primApp :: PrimInfo -> [C.Term] -> C.Term
primApp pr = mkApps (C.Prim pr) . map Left

integerLit :: Integer -> C.Term
integerLit = C.Literal . IntegerLiteral

ix, iy :: Id
ix = localId User "ix" 230 integerPrimTy
iy = localId User "iy" 231 integerPrimTy

-- | A case-expression on @s@ with alternatives of type @Integer@
caseSI :: [C.Term] -> C.Term
caseSI alts = C.Case (C.Var s) integerPrimTy
  (zip (DefaultPat : [LitPat (IntLiteral i) | i <- [1 ..]]) alts)

-- * Tests

tests :: TestTree
tests =
  testGroup "Clash.Tests.Normalize.Scoped.Transformations.DEC"
    [ testGroup "regression tests of Clash Core, through viaFoil" (decTests decScoped)
    , testGroup "agrees with Clash Core"
        [ testGroup "heads"
            [ testCase "a global function in two alternatives" $
                decAgrees True [] [s, x, y]
                  (caseS [app f [C.Var x], app f [C.Var y], app g [C.Var x]])
            , testCase "a global function in one alternative" $
                decAgrees False [] [s, x, y]
                  (caseS [app f [C.Var x], app g [C.Var y]])
            , testCase "a local function of the context" $
                decAgrees True [] [s, q, x, y]
                  (caseS [app q [C.Var x], app q [C.Var y]])
            , testCase "several functions, applied to each other" $
                decAgrees True [] [s, x, y]
                  (caseS [app f [app g [C.Var x]], app f [app g [C.Var y]], app g [C.Var y]])
            , testCase "a function applied twice in one alternative" $
                decAgrees True [] [s, x, y]
                  (caseS [app f [app f [C.Var x]], app f [C.Var y]])
            , testCase "NoDeDup" $
                decAgrees False [] [s, x, y]
                  (caseS [C.Tick NoDeDup (app f [C.Var x]), C.Tick NoDeDup (app f [C.Var y])])
            , testCase "a tick around one application" $
                decAgrees True [] [s, x, y]
                  (caseS [C.Tick DeDup (app f [C.Var x]), app f [C.Var y]])
            , testCase "a function in the subject of a nested case-expression" $
                decAgrees True [] [s, t, x, y]
                  (caseS [ app f [C.Var x]
                         , C.Case (app f [C.Var y]) intTy [(DefaultPat, app f [C.Var t])] ])
            ]
        , testGroup "arguments"
            [ testCase "two arguments that differ, through a tuple" $
                decAgrees True [] [s, x, y]
                  (caseS [app k [C.Var x, C.Var y], app k [C.Var y, C.Var x]])
            , testCase "three arguments that differ, through a tuple" $
                decAgrees True [] [s, x, y, z]
                  (caseS [ app k [app k [C.Var x, C.Var y], app k [C.Var z, C.Var x]]
                         , app k [app g [C.Var y], app f [C.Var z]] ])
            , testCase "a shared argument" $
                decAgrees True [] [s, x, y, z]
                  (caseS [app k [C.Var x, C.Var z], app k [C.Var y, C.Var z]])
            , testCase "all arguments shared" $
                decAgrees True [] [s, x]
                  (caseS [app f [C.Var x], app f [C.Var x]])
            , testCase "a shared argument up to ticks" $
                decAgrees True [] [s, x]
                  (caseS [app f [C.Tick DeDup (C.Var x)], app f [C.Var x]])
            , testCase "a non-representable argument" $
                decAgrees True [] [s, x, y]
                  (caseS [ mkApps (C.Var hof) [Left (C.Var f), Left (C.Var x)]
                         , mkApps (C.Var hof) [Left (C.Var g), Left (C.Var y)] ])
            , testCase "an argument mentioning a pattern binder" $
                decAgrees True [] [s, p, x]
                  (caseS [ app f [C.Var x]
                         , C.Case (C.Var p) intTy
                             [(DataPat pairDataCon [] [a, b], app f [C.Var b])] ])
            , testCase "an argument mentioning a let-binder in an alternative" $
                decAgrees True [] [s, x, y]
                  (caseS [ app f [C.Var x]
                         , C.Let (NonRec a (app g [C.Var y])) (app f [C.Var a]) ])
            , testCase "an application in the right-hand side of a let-binding" $
                decAgrees True [] [s, x, y]
                  (caseS [ app f [C.Var x]
                         , C.Let (NonRec a (app f [C.Var y])) (app k [C.Var a, C.Var a]) ])
            , testCase "an argument mentioning a recursive let-binder" $
                decAgrees True [] [s, x, y]
                  (caseS [ app f [C.Var x]
                         , C.Letrec [(a, app g [C.Var y]), (b, app k [C.Var a, C.Var x])]
                             (app f [C.Var b]) ])
            , testCase "applications in a recursive group are left alone" $
                decAgrees False [] [s, x, y]
                  (caseS [ app f [C.Var x]
                         , C.Letrec [(a, app f [C.Var b]), (b, app g [C.Var a])]
                             (C.Var a) ])
            ]
        , testGroup "type arguments"
            [ testCase "shared type arguments" $
                decAgrees True [] [s, x, y]
                  (caseS [ mkApps (C.Var h) [Right intTy, Left (C.Var x)]
                         , mkApps (C.Var h) [Right intTy, Left (C.Var y)] ])
            , testCase "a type variable of the context" $
                decAgrees True [n] [s, xn, yn]
                  (caseS [ mkApps (C.Var h) [Right (VarTy n), Left (C.Var xn)]
                         , mkApps (C.Var h) [Right (VarTy n), Left (C.Var yn)] ])
            , testCase "different type arguments" $
                decAgrees False [] [s, x, p]
                  (caseS [ mkApps (C.Var h) [Right intTy, Left (C.Var x)]
                         , mkApps (C.Var h) [Right pairTy, Left (C.Var p)] ])
            , testCase "an existential type variable" $
                decAgrees False [] [s, e]
                  (caseS [ C.Case (C.Var e) intTy
                             [(DataPat mkTDc [tN] [v], mkApps (C.Var h) [Right (VarTy tN), Left (C.Var v)])]
                         , C.Case (C.Var e) intTy
                             [(DataPat mkTDc [tN] [v], mkApps (C.Var h) [Right (VarTy tN), Left (C.Var v)])] ])
            , testCase "an argument whose type mentions an existential type variable" $ do
                -- Clash drops casts, so after matching on a GADT, an argument
                -- can have a type that mentions an existential type variable
                -- where the function expects another type: here @v :: n@ for
                -- @f :: Int -> Int@. Both give the let-binder of the argument
                -- the type of the parameter of @f@, rather than the type of
                -- the first argument, which mentions @n@ outside of the
                -- pattern that binds it.
                let term = caseS [ C.Case (C.Var e) intTy [(DataPat mkTDc [tN] [v], app f [C.Var v])]
                                 , app f [C.Var x] ]
                decAgrees True [] [s, e, x] term
                (scoped, ch) <- runDEC decScoped [] [s, e, x] term
                ch @?= True
                assertBool ("expected no free type variables:\n" <> showPprU scoped)
                  (null (Lens.toListOf termFreeTyVars scoped))
            ]
        , testGroup "nested case-expressions"
            [ testCase "literal patterns" $
                decAgrees True [] [s, t, x, y]
                  (caseS [ C.Case (C.Var t) intTy
                             [(LitPat (IntLiteral 1), app f [C.Var x]), (DefaultPat, app g [C.Var y])]
                         , app f [C.Var y] ])
            , testCase "a non-exhaustive data case-expression" $
                decAgrees True [] [s, m, x]
                  (caseS [ app f [C.Var x]
                         , C.Case (C.Var m) intTy
                             [ (DataPat nothingDataCon [] [], app g [C.Var x])
                             , (DataPat justDataCon [] [a], app f [C.Var a]) ] ])
            , testCase "a case-expression in which the pattern binders are not used" $
                decAgrees True [] [s, p, x, y]
                  (caseS [ app f [C.Var x]
                         , C.Case (C.Var p) intTy [(DataPat pairDataCon [] [a, b], app f [C.Var y])] ])
            , testCase "two levels" $
                decAgrees True [] [s, t, x, y, z, w]
                  (caseS [ C.Case (C.Var t) intTy
                             [ (DefaultPat, app k [C.Var x, C.Var y])
                             , (LitPat (IntLiteral 1), app k [C.Var y, C.Var z]) ]
                         , app k [C.Var z, C.Var w]
                         , C.Case (C.Var t) intTy
                             [ (DefaultPat, app g [C.Var x])
                             , (LitPat (IntLiteral 2), app k [C.Var w, C.Var x]) ] ])
            ]
        , testGroup "primitives"
            [ testCase "an interesting primitive" $
                decAgrees True [] [s, ix, iy]
                  (caseSI [ primApp integerMul [C.Var ix, integerLit 3]
                          , primApp integerMul [C.Var iy, integerLit 5] ])
            , testCase "a primitive that isn't interesting" $
                decAgrees False [] [s, ix, iy]
                  (caseSI [primApp plainPrim [C.Var ix], primApp plainPrim [C.Var iy]])
            , testCase "a primitive with DeDup" $
                decAgrees True [] [s, ix, iy]
                  (caseSI [ C.Tick DeDup (primApp plainPrim [C.Var ix])
                          , C.Tick DeDup (primApp plainPrim [C.Var iy]) ])
            , testCase "a higher-order primitive, multiplying by a power of two" $
                decAgrees False [] [s, ix, iy]
                  (caseSI [ primApp hoPrim [primApp integerMul [integerLit 4], opaque ix]
                          , primApp hoPrim [primApp integerMul [integerLit 4], opaque iy] ])
            , testCase "a higher-order primitive, multiplying by zero" $
                decAgrees False [] [s, ix, iy]
                  (caseSI [ primApp hoPrim [primApp integerMul [integerLit 0], opaque ix]
                          , primApp hoPrim [primApp integerMul [integerLit 0], opaque iy] ])
            , testCase "a higher-order primitive, multiplying by another number" $
                decAgrees True [] [s, ix, iy]
                  (caseSI [ primApp hoPrim [primApp integerMul [integerLit 3], opaque ix]
                          , primApp hoPrim [primApp integerMul [integerLit 3], opaque iy] ])
            , testCase "a higher-order primitive, multiplying by fromInteger# of a power of two" $
                decAgrees False [] [s, ix, iy]
                  (caseSI [ primApp hoPrim [primApp integerMul [fromIntegerLit 8], opaque ix]
                          , primApp hoPrim [primApp integerMul [fromIntegerLit 8], opaque iy] ])
            , testCase "a higher-order primitive, multiplying by fromInteger# of another number" $
                decAgrees True [] [s, ix, iy]
                  (caseSI [ primApp hoPrim [primApp integerMul [fromIntegerLit 6], opaque ix]
                          , primApp hoPrim [primApp integerMul [fromIntegerLit 6], opaque iy] ])
            , testCase "a higher-order primitive, dividing by a power of two" $
                decAgrees False [] [s, ix, iy]
                  (caseSI [ primApp hoPrim [primApp integerQuot [integerLit 7, integerLit 2], opaque ix]
                          , primApp hoPrim [primApp integerQuot [integerLit 7, integerLit 2], opaque iy] ])
            , testCase "a higher-order primitive, dividing by another number" $
                decAgrees True [] [s, ix, iy]
                  (caseSI [ primApp hoPrim [primApp integerQuot [integerLit 8, integerLit 3], opaque ix]
                          , primApp hoPrim [primApp integerQuot [integerLit 8, integerLit 3], opaque iy] ])
            , testCase "a higher-order primitive, applied to a function" $
                decAgrees True [] [s, ix, iy]
                  (caseSI [ primApp hoPrim [C.Var fI, C.Var ix]
                          , primApp hoPrim [C.Var fI, C.Var iy] ])
            ]
        ]
    ]
 where
  a = intId User "a" 240
  b = intId User "b" 241
  v = localId User "v" 242 (VarTy tN)
  xn = localId User "xn" 243 (VarTy n)
  yn = localId User "yn" 244 (VarTy n)
  fI = globalId User "fI" 245 (integerFunTy 1)

  -- @fromInteger# \@8 8 i@
  fromIntegerLit i =
    mkApps (C.Prim fromIntegerU)
      [Right (LitTy (NumTy 8)), Left (C.Literal (NaturalLiteral 8)), Left (integerLit i)]

  -- A term that isn't constant, and whose head isn't interesting
  opaque i = C.Case (C.Var i) integerPrimTy [(DefaultPat, C.Var i)]
