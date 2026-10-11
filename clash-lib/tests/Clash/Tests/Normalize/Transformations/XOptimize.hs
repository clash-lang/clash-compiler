{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.XOptimize"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.XOptimize (tests) where

import qualified Control.Lens as Lens
import Data.Default (def)
import qualified Data.HashMap.Strict as HashMap
import Data.List (find)
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Annotations.Primitive (PrimitiveGuard (HasBlackBox))
import Clash.Core.DataCon (DataCon)
import Clash.Core.FreeVars (freeLocalVars)
import Clash.Core.Name (Name (..), NameSort (User), mkUnsafeName)
import Clash.Core.Term
  ( IsMultiPrim (SingleResult), Pat (..), PrimInfo (..)
  , PrimUnfolding (NoUnfolding), Term (..), WorkInfo (WorkNever) )
import Clash.Core.TyCon (AlgTyConRhs (..), TyCon (..))
import Clash.Core.Type (Type (ForAllTy, VarTy), mkFunTy)
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Var (Id, TyVar, Var (..))
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (ClashEnv (..), ClashOpts (..))
import Clash.Netlist.BlackBox.Types
  (Element (Err), RenderVoid (NoRenderVoid), TemplateKind (TExpr))
import Clash.Netlist.Types (BlackBox (BBTemplate), Usage (Cont))
import Clash.Normalize.Transformations.XOptimize (xOptimize)
import Clash.Normalize.Types (NormRewrite)
import Clash.Primitives.Types (Primitive (..))
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..))
import Clash.Util.Supply (Supply, newSupply)

import Test.Clash.Rewrite
  ( assertAlphaEq, assertNoShadowing, globalId, inScopeOf, intFunTy, intId
  , intTy, letBinders, localId, mkDataCon, nameToUnique, pairDataCon, pairTy
  , parseTyConTy, runSingleTransformation, showPprU, tyVar, withTyConMap )

xOptimizeR :: NormRewrite
xOptimizeR = $(asRewriteQ xOptimize)

-- | @T@, a data type with a single constructor 'mkTDc'
tTy :: Type
tTy = parseTyConTy "T"

-- | Existential type variable of 'mkTDc'
tN :: TyVar
tN = tyVar User "n" 41

-- | @MkT :: forall n. n -> T@
mkTDc :: DataCon
mkTDc = mkDataCon "MkT" 101 [] [tN] [VarTy tN] tTy

-- | A primitive that 'xOptimize' considers to be undefined: its template is an
-- error
errPrim :: Term
errPrim = Prim (PrimInfo "err" intTy WorkNever SingleResult NoUnfolding)

-- | The default rewrite environment, with aggressive X-optimization enabled,
-- extended with the primitive of 'errPrim', and the data types @T@ and @Pair@
envXOpt :: RewriteEnv
envXOpt = withTyConMap tcm def{_clashEnv = env}
 where
  env0 = _clashEnv def
  env = env0
    { envOpts = (envOpts env0){opt_aggressiveXOpt = True}
    , envPrimitives = HashMap.singleton "err" (HasBlackBox [] errBB)
    }
  tcm =
    foldr (\tc -> UniqMap.insert (tyConName tc) tc) (envTyConMap env0)
      [mkTc "T" mkTDc, mkTc "Pair" pairDataCon]
  mkTc nm dc = AlgTyCon
    { tyConUniq = nameToUnique nm
    , tyConName = mkUnsafeName User (Text.pack nm) (nameToUnique nm)
    , tyConKind = liftedTypeKind
    , tyConArity = 0
    , algTcRhs = DataTyCon [dc]
    , isClassTc = False
    }
  errBB = BlackBox
    { name = "err"
    , workInfo = WorkNever
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

-- | Run 'xOptimize' on a term, with the given unique supply and variables in
-- scope
runXOpt :: Supply -> [Id] -> Term -> IO Term
runXOpt supply inScope =
  runSingleTransformation envXOpt def{_uniqSupply = supply} (inScopeOf inScope)
    xOptimizeR

-- | Global function @f :: Int -> Int@
fId :: Id
fId = globalId User "f" 10 (intFunTy 1)

-- | @case p of { DEFAULT -> err; MkPair p z -> f p }@, where the pattern binder
-- @p@ shadows the subject @p@.
--
-- 'xOptimize' replaces a case-expression with a single defined alternative by
-- let-bindings for its pattern binders, alongside a let-binding for the
-- subject. It used to do so without renaming the pattern binders, so they
-- could capture the free variables of the subject:
--
-- > letrec subj = p; p = case subj of MkPair a _ -> a; ... in f p
xOptimizeSubjectCapture :: Assertion
xOptimizeSubjectCapture = do
  supply <- newSupply
  res <- runXOpt supply [pId] term
  let subj = localId User "subj" 50 pairTy
      a = intId User "a" 51
      b = intId User "b" 52
      sel i = Case (Var subj) intTy
        [(DataPat pairDataCon [] [intId User "f0" 53, intId User "f1" 54], Var i)]
      expected =
        Letrec
          [ (subj, Var pId)
          , (a, sel (intId User "f0" 53))
          , (b, sel (intId User "f1" 54))
          ]
          (App (Var fId) (Var a))
  assertAlphaEq expected res
 where
  pId = localId User "p" 47 pairTy
  pPat = intId User "p" 47
  zId = intId User "z" 48
  term =
    Case (Var pId) intTy
      [ (DefaultPat, errPrim)
      , (DataPat pairDataCon [] [pPat, zId], App (Var fId) (Var pPat))
      ]

-- | @case s of { DEFAULT -> err; MkPair a z -> f a }@, where @a@ has the unique
-- 'xOptimize' picks for the let-binder of the subject. It used to pick that
-- unique avoiding only the variables in scope of the case-expression, not the
-- pattern binders, which it let-binds alongside it.
xOptimizeSubjectDuplicate :: Assertion
xOptimizeSubjectDuplicate = do
  supply <- newSupply
  res1 <- runXOpt supply [sId] (term 1000)
  subj <- case find (Text.isSuffixOf "subj" . nameOcc . varName) (letBinders res1) of
    Just i -> pure i
    Nothing -> assertFailure ("xOptimize did not let-bind the subject:\n" <> showPprU res1)
  runXOpt supply [sId] (term (varUniq subj)) >>= assertNoShadowing
 where
  sId = localId User "s" 30 pairTy
  term aUniq =
    let a = intId User "a" aUniq
        z = intId User "z" 48
    in  Case (Var sId) intTy
          [ (DefaultPat, errPrim)
          , (DataPat pairDataCon [] [a, z], App (Var fId) (Var a))
          ]

-- | @case t of { DEFAULT -> err; MkT \@n (v :: n) -> h \@n v }@, where @MkT@
-- has an existential type variable @n@.
--
-- 'xOptimize' used to let-bind @v@ outside of the case-expression, leaving the
-- existential type variable @n@ free.
xOptimizeExistential :: Assertion
xOptimizeExistential = do
  supply <- newSupply
  res <- runXOpt supply [tId] term
  case filter ((/= varUniq tId) . varUniq) (Lens.toListOf freeLocalVars res) of
    [] -> pure ()
    vs -> assertFailure
      ("Unexpected free variables " <> showPprU (vs :: [Var ()]) <> " in:\n"
        <> showPprU res)
 where
  tId = localId User "t" 42 tTy
  v = localId User "v" 43 (VarTy tN)
  a = tyVar User "a" 45
  -- Global function @h :: forall a. a -> Int@
  h = globalId User "h" 46 (ForAllTy a (mkFunTy (VarTy a) intTy))
  term =
    Case (Var tId) intTy
      [ (DefaultPat, errPrim)
      , (DataPat mkTDc [tN] [v], App (TyApp (Var h) (VarTy tN)) (Var v))
      ]

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.XOptimize"
    [ testCase "xOptimize: pattern binders do not capture the subject"
        xOptimizeSubjectCapture
    , testCase "xOptimize: subject binder is not a pattern binder"
        xOptimizeSubjectDuplicate
    , testCase "xOptimize: existential type variables stay bound"
        xOptimizeExistential
    ]
