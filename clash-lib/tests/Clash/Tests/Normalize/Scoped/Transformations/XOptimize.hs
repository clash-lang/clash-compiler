{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.XOptimize".
  'assertRewritesTo' runs every test in the scope of the free variables of its
  input, and in a scope that the binders of the input clash with.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.XOptimize (tests) where

import Data.Default (def)
import qualified Data.HashMap.Strict as HashMap

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Annotations.Primitive (PrimitiveGuard (HasBlackBox))
import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Basic as C (IsMultiPrim (SingleResult), WorkInfo (WorkNever))
import qualified Clash.Core.TyCon as C
import Clash.Core.Scoped.Syntax (mkPrimInfo)
import Clash.Core.Scoped.Syntax (GlobalId, PrimInfo, PrimUnfolding (NoUnfolding), Term (Case))
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (ClashEnv (..), ClashOpts (..))
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Types (BlackBox (BBTemplate), Usage (Cont))
import qualified Clash.Normalize.Scoped.Transformations.XOptimize as Scoped
import Clash.Normalize.Types (NormalizeState)
import Clash.Primitives.Types (Primitive (..))
import Clash.Rewrite.Scoped (Rewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState)

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, kindedTyVarD, parseTestTermWith, strQQ, testAlgTyCon
  , testDataCon, testGlobalId, testTyConMap, withTyConMap )
import Test.Clash.Scoped
  ( ScopeDecl, assertCaptureRewritesTo, assertNoRewrite, assertRewritesTo, closedType, termIn
  , varD )

-- * Definitions

-- | @MkT :: forall n. n -> T@
mkTDc :: C.DataCon
mkTDc = testDataCon "MkT" 101 1 [] [kindedTyVarD "n" "Type"] ["n"] "T"

-- | A primitive that 'xOptimize' considers to be undefined: its template is an
-- error
errPrim :: PrimInfo
errPrim = mkPrimInfo "err" (closedType "Int") C.WorkNever C.SingleResult NoUnfolding

-- | Global functions @f, g :: Int -> Int@ and @h :: forall a. a -> Int@
globals :: [GlobalId]
globals =
  [ testGlobalId "f_G10" "Int -> Int"
  , testGlobalId "g_G40" "Int -> Int"
  , testGlobalId "h_G46" "forall (a :: Type). a -> Int" ]

tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [mkTDc] [errPrim] globals)

-- | The default rewrite environment, with aggressive X-optimization enabled,
-- the primitive of 'errPrim', and the type constructors of 'testTyConMap' and
-- @T@
env :: RewriteEnv
env = withTyConMap tcm def{_clashEnv = cenv}
 where
  cenv0 = _clashEnv def
  cenv = cenv0
    { envOpts = (envOpts cenv0){opt_aggressiveXOpt = True}
    , envPrimitives = HashMap.singleton "err" (HasBlackBox [] errBB)
    }
  tcm = UniqMap.insert (C.tyConName tTc) tTc testTyConMap
  tTc = testAlgTyCon "T" [] [mkTDc]
  errBB = BlackBox
    { name = "err"
    , workInfo = C.WorkNever
    , renderVoid = NoRenderVoid
    , multiResult = False
    , kind = TExpr
    , warning = ()
    , outputUsage = Cont
    , libraries = []
    , imports = []
    , functionPlurality = []
    , includes = []
    , resultNames = []
    , resultInits = []
    , template = BBTemplate [Err Nothing]
    }

st :: RewriteState NormalizeState
st = def

xOptS :: Rewrite NormalizeState
xOptS = $(asRewriteQ Scoped.xOptimize)

-- | Assert that 'Scoped.xOptimize' rewrites a term to the expected one
xOptTo :: [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
xOptTo = assertRewritesTo env st xOptS

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.XOptimize"
  [ testGroup "regression tests"
      [ -- @case p of { DEFAULT -> err; MkPair p z -> f p }@, where the pattern
        -- binder @p@ shadows the subject @p@. 'xOptimize'
        -- replaces a case-expression with a single defined alternative by
        -- let-bindings for its pattern binders, alongside a let-binding for the
        -- subject. It used to do so without renaming the pattern binders, so
        -- they could capture the free variables of the subject:
        --
        -- > letrec subj = p; p = case subj of MkPair a _ -> a; ... in f p
        testCase "xOptimize: pattern binders do not capture the subject" $ do
          xOptTo [varD "p_47" "Pair"]
            (tm "(case p_47 of { _ -> Prim.err; MkPair (p :: Int) (z :: Int) -> f_G10 p }) :: Int")
            expected
          -- The pattern binder @p@ has the name of the subject @p@
          assertCaptureRewritesTo env st xOptS [varD "d" "Pair"]
            (tm "(case d of { _ -> Prim.err; MkPair (p :: Int) (z :: Int) -> f_G10 p }) :: Int")
            [varD "p_47" "Pair"]
            (\ts -> \case
              Case _ ty alts -> Case (termIn ts (tm "p_47")) ty alts
              e -> e)
            expected
        -- @case s of { DEFAULT -> err; MkPair a z -> f a }@, where @a@ had the
        -- unique 'xOptimize' picks for the let-binder of the subject. It used
        -- to pick that unique avoiding only the variables in scope of the
        -- case-expression, not the pattern binders, which it let-binds
        -- alongside it. In scoped Core, the new binder is fresh by
        -- construction.
      , testCase "xOptimize: subject binder is not a pattern binder" $
          xOptTo [varD "s_30" "Pair"]
            (tm "(case s_30 of { _ -> Prim.err; MkPair (a :: Int) (z :: Int) -> f_G10 a }) :: Int")
            (tm [strQQ|
              let
                { (subj :: Pair) = s_30
                ; (a :: Int) = (case subj of { MkPair (f0 :: Int) (f1 :: Int) -> f0 }) :: Int
                ; (z :: Int) = (case subj of { MkPair (f0 :: Int) (f1 :: Int) -> f1 }) :: Int
                } in f_G10 a
            |])
        -- @case t of { DEFAULT -> err; MkT \@n (v :: n) -> h \@n v }@, where
        -- @MkT@ has an existential type variable @n@. 'xOptimize' used to
        -- let-bind @v@ outside of the case-expression, leaving the existential
        -- type variable @n@ free.
      , testCase "xOptimize: existential type variables stay bound" $
          xOptTo [varD "t_42" "T"]
            (tm "(case t_42 of { _ -> Prim.err; MkT (n :: Type) (v :: n) -> h_G46 @n v }) :: Int")
            (tm "(case t_42 of { MkT (n :: Type) (v :: n) -> h_G46 @n v }) :: Int")
      ]
  , testGroup "cases"
      [ testCase "no defined alternatives" $
          xOptTo [varD "x_33" "Int"]
            (tm "(case x_33 of { 1 -> Prim.err; _ -> Prim.err }) :: Int")
            (tm "Clash.XException.errorX")
        -- The type of a primitive is closed, so @errorX@ is applied to the
        -- open type @a@, see 'Clash.Core.Scoped.Util.primAtType'
      , testCase "no defined alternatives, at an open type" $
          xOptTo [kindedTyVarD "a_50" "Type", varD "x_33" "Int"]
            (tm "(case x_33 of { _ -> Prim.err }) :: a_50")
            (tm "Clash.XException.errorX @a_50")
      , testCase "one defined alternative with a pattern" $
          xOptTo [varD "s_30" "Pair"]
            (tm "(case s_30 of { MkPair (x :: Int) (y :: Int) -> f_G10 y; _ -> Prim.err }) :: Int")
            (tm [strQQ|
              let
                { (subj :: Pair) = s_30
                ; (x :: Int) = (case subj of { MkPair (f0 :: Int) (f1 :: Int) -> f0 }) :: Int
                ; (y :: Int) = (case subj of { MkPair (f0 :: Int) (f1 :: Int) -> f1 }) :: Int
                } in f_G10 y
            |])
      , testCase "one defined alternative with a literal" $
          xOptTo [varD "x_33" "Int"]
            (tm "(case x_33 of { 1 -> f_G10 x_33; _ -> Prim.err }) :: Int")
            (tm "f_G10 x_33")
      , testCase "one defined default alternative" $
          xOptTo [varD "x_33" "Int"]
            (tm "(case x_33 of { 1 -> Prim.err; _ -> g_G40 x_33 }) :: Int")
            (tm "g_G40 x_33")
      , testCase "one defined alternative whose pattern binds a type variable" $
          xOptTo [varD "t_32" "T"]
            (tm "(case t_32 of { _ -> Prim.err; MkT (n :: Type) (v :: n) -> 3 }) :: Int")
            (tm "(case t_32 of { MkT (n :: Type) (v :: n) -> 3 }) :: Int")
      , testCase "many defined alternatives, without a default" $
          xOptTo [varD "m_31" "Maybe Int"]
            (tm "(case m_31 of { Nothing -> 3; Just (z :: Int) -> f_G10 z; _ -> Prim.err }) :: Int")
            (tm "(case m_31 of { Just (z :: Int) -> f_G10 z; _ -> let { (subj :: Maybe Int) = m_31 } in 3 }) :: Int")
      , testCase "many defined alternatives, with a default" $
          xOptTo [varD "x_33" "Int"]
            (tm "(case x_33 of { 1 -> Prim.err; 2 -> f_G10 x_33; _ -> g_G40 x_33 }) :: Int")
            (tm "(case x_33 of { 2 -> f_G10 x_33; _ -> g_G40 x_33 }) :: Int")
      , testCase "all alternatives defined" $
          assertNoRewrite env st xOptS [varD "x_33" "Int"]
            (tm "(case x_33 of { 1 -> f_G10 x_33; _ -> g_G40 x_33 }) :: Int")
      , testCase "doesn't fire without aggressive X-optimization" $ do
          let cenv = _clashEnv env
              envNoXOpt = env{_clashEnv = cenv{envOpts = (envOpts cenv){opt_aggressiveXOpt = False}}}
          assertNoRewrite envNoXOpt st xOptS [varD "x_33" "Int"]
            (tm "(case x_33 of { _ -> Prim.err }) :: Int")
      ]
  ]
 where
  expected = tm [strQQ|
    let
      { (subj :: Pair) = p_47
      ; (p :: Int) = (case subj of { MkPair (f0 :: Int) (f1 :: Int) -> f0 }) :: Int
      ; (z :: Int) = (case subj of { MkPair (f0 :: Int) (f1 :: Int) -> f1 }) :: Int
      } in f_G10 p
  |]
