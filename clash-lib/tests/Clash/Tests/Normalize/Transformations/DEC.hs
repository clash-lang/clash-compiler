{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.DEC"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.DEC (tests) where

import qualified Control.Lens as Lens
import Data.Default (def)
import Data.List (find)
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.FreeVars (freeLocalVars, termFreeTyVars)
import Clash.Core.Literal (Literal (IntLiteral))
import Clash.Core.Name (Name (..), NameSort (User), mkUnsafeName)
import Clash.Core.Term (Pat (..), Term (..), mkApps)
import Clash.Core.TyCon (AlgTyConRhs (..), TyCon (..))
import Clash.Core.Type (Type (ForAllTy, VarTy), mkFunTy)
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Var (Id, TyVar, Var (..))
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (ClashEnv (..))
import Clash.Normalize.Transformations.DEC (disjointExpressionConsolidation)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..))
import Clash.Unique (Unique)
import Clash.Util.Supply (Supply, newSupply)

import Test.Clash.Rewrite
  ( assertAlphaEq, assertNoShadowing, globalId, inScopeOf, intFunTy, intId
  , intLit, intTy, letBinders, localId, mkBindingMap, mkDataCon, nameToUnique
  , pairDataCon, pairTy, parseTyConName, parseTyConTy, runSingleTransformation
  , showPprU, tyVar, withTyConMap )

decR :: NormRewrite
decR = $(asRewriteQ disjointExpressionConsolidation)

-- | @MkEx :: forall n. n -> Ex@, a constructor with an existential type variable
mkEx :: DataCon
mkEx = mkDataCon "MkEx" 1001 [] [exN] [VarTy exN] exTy

exN :: TyVar
exN = tyVar User "n" 34

exTy :: Type
exTy = parseTyConTy "Ex"

-- | The default 'RewriteEnv', extended with the type constructor of 'mkEx'
exEnv :: RewriteEnv
exEnv = withTyConMap (UniqMap.insert exNm exTc (envTyConMap env)) def
 where
  env = _clashEnv def
  exNm = parseTyConName "Ex"
  exTc = AlgTyCon
    { tyConUniq = nameUniq exNm
    , tyConName = exNm
    , tyConKind = liftedTypeKind
    , tyConArity = 0
    , algTcRhs = DataTyCon [mkEx]
    , isClassTc = False
    }

-- | @case s of { DEFAULT -> case e of { MkEx \@n v -> f v }; 1 -> f x }@
--
-- Clash drops casts, so after matching on a GADT, an argument can have a type
-- that mentions an existential type variable where the function expects
-- another type: here @v :: n@ for @f :: Int -> Int@. DEC used to give the
-- let-binder of the lifted argument of @f@ the type of the argument of the
-- first application, @n@, which leaves @n@ free outside of the pattern that
-- binds it.
decExistentialArgType :: Assertion
decExistentialArgType = do
  supply <- newSupply
  let st = def{_bindings = mkBindingMap [(fId, Lam a (Var a))], _uniqSupply = supply}
  res <- runSingleTransformation exEnv st (inScopeOf [sId, eId, xId]) decR term
  case Lens.toListOf termFreeTyVars res of
    [] -> pure ()
    tvs -> assertFailure
      ("Unexpected free type variables " <> showPprU tvs <> " in:\n" <> showPprU res)
 where
  a = intId User "a" 20
  eId = localId User "e" 35 exTy
  v = localId User "v" 36 (VarTy exN)
  term =
    Case (Var sId) intTy
      [ (DefaultPat, Case (Var eId) intTy [(DataPat mkEx [exN] [v], App (Var fId) (Var v))])
      , (LitPat (IntLiteral 1), App (Var fId) (Var xId))
      ]

sId, xId, yId :: Id
sId = intId User "s" 30
xId = intId User "x" 31
yId = intId User "y" 32

-- | Global function @f :: Int -> Int@, with body @\\a -> a@
fId :: Id
fId = globalId User "f" 10 (intFunTy 1)

-- | Global function @k :: Int -> Int -> Int@
kId :: Id
kId = globalId User "k" 33 (intFunTy 2)

-- | @case s of { DEFAULT -> f y; 1 -> let b = 3 in k b (f x) }@, where @b@ has
-- the given unique. DEC lifts out @f@, and substitutes @f_out@ for its
-- applications in both alternatives.
decTerm :: Unique -> Term
decTerm bUniq =
  Case (Var sId) intTy
    [ (DefaultPat, App (Var fId) (Var yId))
    , ( LitPat (IntLiteral 1)
      , Letrec [(b, intLit 3)]
          (mkApps (Var kId) [Left (Var b), Left (App (Var fId) (Var xId))]))
    ]
 where
  b = intId User "b" bUniq

-- | Run DEC on a term, with the given unique supply
runDEC :: Supply -> Term -> IO Term
runDEC supply = runDECIn supply [sId, xId, yId]

-- | Run DEC on a term, with the given unique supply and variables in scope,
-- in an environment that knows @T@ and @Pair@
runDECIn :: Supply -> [Id] -> Term -> IO Term
runDECIn supply inScope = runSingleTransformation envT st (inScopeOf inScope) decR
 where
  st = def{_bindings = mkBindingMap [(fId, Lam a (Var a))], _uniqSupply = supply}
  a = intId User "a" 20

-- | DEC substitutes a reference to the lifted @f_out@ for every application of
-- @f@ in the alternatives, which puts that reference under the binders of the
-- alternatives. It used to pick the unique of @f_out@ avoiding only the
-- variables in scope of the case-expression, not the binders inside the
-- alternatives, so a let-binder in an alternative could capture the reference.
--
-- To provoke this, we first run DEC to find out which unique @f_out@ gets, and
-- then run it again, with the same unique supply, on a term in which the
-- let-binder in the alternative has exactly that unique.
--
-- https://github.com/clash-lang/clash-compiler/pull/1317
-- https://github.com/clash-lang/clash-compiler/commit/959ee9072fdd0c5deaf91f5e82adc2c23b0f8e16
decDoesNotCaptureLiftedExpression :: Assertion
decDoesNotCaptureLiftedExpression = do
  supply <- newSupply
  res1 <- runDEC supply (decTerm 1000)
  fOut <- case find (Text.isSuffixOf "f_out" . nameOcc . varName) (letBinders res1) of
    Just i -> pure i
    Nothing -> assertFailure ("DEC did not lift out f:\n" <> showPprU res1)
  runDEC supply (decTerm (varUniq fOut)) >>= assertNoShadowing

-- | @T@, a data type with a single constructor 'mkTDc'
tTy :: Type
tTy = parseTyConTy "T"

-- | Existential type variable of 'mkTDc'
tN :: TyVar
tN = tyVar User "n" 41

-- | @MkT :: forall n. n -> T@
mkTDc :: DataCon
mkTDc = mkDataCon "MkT" 101 [] [tN] [VarTy tN] tTy

-- | The default rewrite environment, extended with @T@ and @Pair@
envT :: RewriteEnv
envT = withTyConMap tcm def
 where
  env = _clashEnv def
  tcm = foldr (\tc -> UniqMap.insert (tyConName tc) tc) (envTyConMap env)
          [mkTc "T" mkTDc, mkTc "Pair" pairDataCon]
  mkTc nm dc = AlgTyCon
    { tyConUniq = nameToUnique nm
    , tyConName = mkUnsafeName User (Text.pack nm) (nameToUnique nm)
    , tyConKind = liftedTypeKind
    , tyConArity = 0
    , algTcRhs = DataTyCon [dc]
    , isClassTc = False
    }

-- | Assert that the free local (type and term) variables of a term are amongst
-- the given ones
assertFreeLocalsIn :: [Id] -> Term -> Assertion
assertFreeLocalsIn inScope tm =
  case filter ((`notElem` map varUniq inScope) . varUniq) fvs of
    [] -> pure ()
    vs -> assertFailure
      ("Unexpected free variables " <> showPprU vs <> " in:\n" <> showPprU tm)
 where
  fvs :: [Var ()]
  fvs = Lens.toListOf freeLocalVars tm

-- | @case s of { DEFAULT -> let g = f in g y; 1 -> let g = f in g x }@
--
-- Both alternatives apply the /local/ function @g@, which they let-bind
-- themselves. DEC used to consider every local variable in scope of an
-- application interesting to lift, including those bound inside the
-- alternatives, and lifted @g@ out of the alternatives that bind it.
decLocalFunctionBoundInAlts :: Assertion
decLocalFunctionBoundInAlts = do
  supply <- newSupply
  res <- runDEC supply term
  assertFreeLocalsIn [sId, xId, yId] res
 where
  g = localId User "g" 40 (intFunTy 1)
  term =
    Case (Var sId) intTy
      [ (DefaultPat, Letrec [(g, Var fId)] (App (Var g) (Var yId)))
      , (LitPat (IntLiteral 1), Letrec [(g, Var fId)] (App (Var g) (Var xId)))
      ]

-- | @case s of { DEFAULT -> case t of MkT \@n v -> h \@n v; 1 -> case t of MkT \@n w -> h \@n w }@,
-- where @MkT :: forall n. n -> T@ has an existential type variable @n@.
--
-- DEC used to consider type arguments shareable as long as they were equal,
-- without checking whether they mention type variables bound by the patterns
-- of the alternatives. It lifted @h \@n@ out of the case-expression that binds
-- @n@.
decTypeArgBoundInAlts :: Assertion
decTypeArgBoundInAlts = do
  supply <- newSupply
  res <- runDECIn supply [sId, tId] term
  assertFreeLocalsIn [sId, tId] res
 where
  tId = localId User "t" 42 tTy
  v = localId User "v" 43 (VarTy tN)
  w = localId User "w" 44 (VarTy tN)
  a = tyVar User "a" 45
  -- Global function @h :: forall a. a -> Int@
  h = globalId User "h" 46 (ForAllTy a (mkFunTy (VarTy a) intTy))
  alt i =
    Case (Var tId) intTy
      [(DataPat mkTDc [tN] [i], App (TyApp (Var h) (VarTy tN)) (Var i))]
  term =
    Case (Var sId) intTy
      [ (DefaultPat, alt v)
      , (LitPat (IntLiteral 1), alt w)
      ]

-- | @case s of { DEFAULT -> f x; 1 -> case p of MkPair x z -> f x }@, where
-- the pattern binder @x@ shadows the @x@ in scope of the case-expression.
--
-- DEC used to consider arguments shareable if all their free variables are in
-- scope of the case-expression. The pattern-bound @x@ has the same unique as
-- the @x@ in scope, so DEC shared @f x@, making the second alternative refer
-- to the wrong @x@.
decSharedArgShadowsContext :: Assertion
decSharedArgShadowsContext = do
  supply <- newSupply
  res <- runDECIn supply [sId, xId, pId] term
  -- DEC itself does not deshadow, so the pattern binder still shadows @x@. But
  -- the argument of @f@ has to be selected by the case-expression.
  let fArg = intId User "f_arg0" 50
      fOut = intId User "f_out" 51
      expected =
        Letrec
          [ ( fOut
            , Letrec
                [ ( fArg
                  , Case (Var sId) intTy
                      [ (DefaultPat, Var xId)
                      , ( LitPat (IntLiteral 1)
                        , Case (Var pId) intTy [(xzPat, Var xId)])
                      ])
                ]
                (App (Var fId) (Var fArg)))
          ]
          (Case (Var sId) intTy
            [ (DefaultPat, Var fOut)
            , (LitPat (IntLiteral 1), Case (Var pId) intTy [(xzPat, Var fOut)])
            ])
  assertAlphaEq expected res
 where
  pId = localId User "p" 47 pairTy
  zId = intId User "z" 48
  xzPat = DataPat pairDataCon [] [xId, zId]
  term =
    Case (Var sId) intTy
      [ (DefaultPat, App (Var fId) (Var xId))
      , ( LitPat (IntLiteral 1)
        , Case (Var pId) intTy [(xzPat, App (Var fId) (Var xId))])
      ]

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.DEC"
    [ testCase "DEC gives lifted arguments a type in scope" decExistentialArgType
    , testCase "DEC does not capture lifted expressions (#1317)"
        decDoesNotCaptureLiftedExpression
    , testCase "DEC does not lift local functions bound in the alternatives"
        decLocalFunctionBoundInAlts
    , testCase "DEC does not share type arguments bound in the alternatives"
        decTypeArgBoundInAlts
    , testCase "DEC does not share arguments bound in the alternatives"
        decSharedArgShadowsContext
    ]
