{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.PrimitiveReductions": the tests of
  "Clash.Normalize.PrimitiveReductions", run on the scoped reductions through
  'Test.Clash.Scoped.ViaFoil.viaFoil', and agreement with the reductions of
  Clash Core.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Normalize.Scoped.PrimitiveReductions (tests) where

import qualified Data.Either as Either
import Data.Default (def)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import GHC.Types.SrcLoc (noSrcSpan)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (User))
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.Var (Id, TyVar)
import Clash.Core.TysPrim (liftedTypeKind, naturalPrimTy, typeNatKind)
import Clash.Normalize.PrimitiveReductions (mkVecNil)
import Clash.Normalize.Scoped.Transformations.Reduce (PrimReduction (..))
import qualified Clash.Normalize.Scoped.Transformations.Reduce as Scoped
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), TransformContext (..))

import Clash.Core.Scoped.Syntax (Term (Prim))
import Clash.Core.Scoped.Convert (mkGlobalIdFromClash)
import Clash.Core.Scoped.Term (collectArgs)
import Clash.Rewrite.Scoped (ScopedTransform (..))
import Test.Clash.Scoped.ViaFoil (viaFoil)

import Clash.Tests.Normalize.PrimitiveReductions
  (PrimReductionImpl, currentPrimReduction, envVec, prim, primitiveReductionTests)
import Test.Clash.Rewrite
  ( addTy, applicativeTy, boolTy, globalId, hIntTy, heapLookupEvaluator, inScopeOf, indexTy
  , intFunTy, intId, intTy, kindedTyVar, localId, maybeTyConName, nilDc, numTy
  , rtreeTy, runRewriteTest, showPprU, snatDc, snatTy, tcTy, ubpTy, vecTy )
import Test.Clash.Scoped (aeqTermTyped, stripCoercionTypes)

-- | The scoped primitive reductions, as a rewrite of Clash Core of the
-- saturated application of the primitive
scopedPrimReduction :: PrimReductionImpl
scopedPrimReduction red = viaFoil "reducePrim" $ ScopedTransform $ \ctx e ->
  case collectArgs e of
    (Prim p, args) ->
      let (tms, tys) = Either.partitionEithers args
      in Scoped.runPrimReduction ctx red p tys tms
    _ -> error "scopedPrimReduction: not a primitive application"

-- | The environment of the reductions: 'envVec', with an evaluator for the
-- 'Clash.Normalize.Strategy.constantPropagation' strategy that some
-- reductions run on their function argument
env :: RewriteEnv
env = envVec {_evaluator = heapLookupEvaluator}

st :: RewriteState NormalizeState
st = def
  { _globalHeap = (IntMap.empty, 0)
  , _curFun = (mkGlobalIdFromClash (globalId User "top" 100 intTy), noSrcSpan) }

-- | Run a reduction of Clash Core and the scoped one on the saturated
-- application of a primitive with free variables, and assert that they agree:
-- alpha-equivalent results (comparing types too, up to the form of
-- coercions), and both signal a change or neither does.
assertAgrees :: [Id] -> PrimReduction -> C.PrimInfo -> [C.Type] -> [C.Term] -> Assertion
assertAgrees ids red p tys tms = do
  let
    e = C.mkApps (C.Prim p) (map Right tys <> map Left tms)
    ctx = TransformContext (inScopeOf ids) []
  (expected, _, changed1) <- runRewriteTest env st (currentPrimReduction red ctx e)
  (actual, _, changed2) <- runRewriteTest env st (scopedPrimReduction red ctx e)
  assertBool
    ("Clash Core:\n" <> showPprU expected <> "\nscoped:\n" <> showPprU actual)
    (aeqTermTyped (stripCoercionTypes expected) (stripCoercionTypes actual))
  assertEqual "change signaled (Clash Core, scoped)" changed1 changed2

-- | Type variables of the types of the primitives and functions
tvN, tvA, tvL :: TyVar
tvN = kindedTyVar User "n" 60 typeNatKind
tvA = kindedTyVar User "a" 61 liftedTypeKind
tvL = kindedTyVar User "l" 62 typeNatKind

-- | A primitive whose type the reductions don't look at
untypedPrim :: Text -> C.PrimInfo
untypedPrim nm = prim nm intTy

-- | A primitive @forall n a. ts -> r@, of which the reductions infer the
-- result type
typedPrim :: Text -> ([C.Type] -> C.Type -> C.Type -> C.Type) -> C.PrimInfo
typedPrim nm mk = prim nm $ C.ForAllTy tvN $ C.ForAllTy tvA $ mk [] (C.VarTy tvN) (C.VarTy tvA)

funTys :: [C.Type] -> C.Type -> C.Type
funTys args res = foldr C.mkFunTy res args

-- | A natural number literal, for @KnownNat@ arguments
natLit :: Integer -> C.Term
natLit = C.Literal . NaturalLiteral

-- | An @SNat@
snat :: Integer -> C.Term
snat n = C.mkApps (C.Data snatDc) [Right (numTy n), Left (natLit n)]

-- | Free variables of the applications
f, g, mkV, dict, h, df, br, imapF, motive, tree, xss :: Id
f = localId User "f" 1 (intFunTy 1)
g = localId User "g" 2 (intFunTy 2)
-- | A function creating vectors, to apply primitives to vectors that aren't
-- variables, which the reductions let-bind
mkV = localId User "mkV" 3 (C.mkFunTy intTy (vecTy (numTy 2) intTy))
dict = localId User "dict" 4 (applicativeTy (tcTy maybeTyConName))
h = localId User "h" 5 (C.mkFunTy intTy (C.mkTyConApp maybeTyConName [intTy]))
-- | The function of @dfold@: @forall l. UBP (l + 1) 2 -> SNat l -> Int -> Int
-- -> Int@
df = localId User "df" 6 $ C.ForAllTy tvL $
  funTys [ubpTy (addTy (C.VarTy tvL) (numTy 1)) (numTy 2), snatTy (C.VarTy tvL), intTy, intTy] intTy
-- | The function combining branches of @dtfold@ and @tdfold@: @forall l. SNat l
-- -> Int -> Int -> Int@
br = localId User "br" 7 $ C.ForAllTy tvL $ funTys [snatTy (C.VarTy tvL), intTy, intTy] intTy
imapF = localId User "im" 8 (C.mkFunTy (indexTy (numTy 2)) (intFunTy 1))
-- | Stands in for the motives of @dfold@, @dtfold@ and @tdfold@
motive = intId User "p" 9
tree = localId User "t" 10 (rtreeTy (numTy 1) intTy)
xss = localId User "xss" 11 (vecTy (numTy 0) (vecTy (numTy 2) intTy))

-- | @v<n> :: Vec n Int@
v :: Integer -> Id
v n = localId User ("v" <> show n) (20 + fromInteger n) (vecTy (numTy n) intTy)

k, z, i :: Id
k = intId User "k" 30
z = intId User "z" 31
i = localId User "i" 32 hIntTy

var :: Id -> C.Term
var = C.Var

-- | The cases of the agreement tests: a name, the reduction, the primitive,
-- its type and term arguments, and the free variables
agreementTests :: [TestTree]
agreementTests =
  [ agrees "zipWith, let-binding a vector" (ReduceZipWith 2 True True)
      (untypedPrim "Clash.Sized.Vector.zipWith") [intTy, intTy, intTy, numTy 2]
      [var g, var (v 2), C.App (var mkV) (var k)]
  , agrees "zipWith of non-representable vectors" (ReduceZipWith 2 False False)
      (untypedPrim "Clash.Sized.Vector.zipWith") [intTy, intTy, intTy, numTy 2]
      [var g, C.App (var mkV) (var k), C.App (var mkV) (var z)]
  , agrees "zipWith on empty vectors" (ReduceZipWith 0 True True)
      (untypedPrim "Clash.Sized.Vector.zipWith") [intTy, intTy, intTy, numTy 0]
      [var g, var (v 0), var (v 0)]
  , agrees "map, let-binding the vector" (ReduceMap 2 True)
      (untypedPrim "Clash.Sized.Vector.map") [intTy, intTy, numTy 2]
      [var f, C.App (var mkV) (var k)]
  , agrees "map on an empty vector" (ReduceMap 0 True)
      (untypedPrim "Clash.Sized.Vector.map") [intTy, intTy, numTy 0] [var f, var (v 0)]
  , agrees "traverse#" (ReduceTraverse 2)
      (untypedPrim "Clash.Sized.Vector.traverse#") [intTy, tcTy maybeTyConName, intTy, numTy 2]
      [var dict, var h, var (v 2)]
  , agrees "traverse# on an empty vector" (ReduceTraverse 0)
      (untypedPrim "Clash.Sized.Vector.traverse#") [intTy, tcTy maybeTyConName, intTy, numTy 0]
      [var dict, var h, var (v 0)]
  , agrees "fold" (ReduceFold 3)
      (untypedPrim "Clash.Sized.Vector.fold") [numTy 2, intTy] [var g, var (v 3)]
  , agrees "foldr, let-binding the vector" (ReduceFoldr 2 True)
      (untypedPrim "Clash.Sized.Vector.foldr") [intTy, intTy, numTy 2]
      [var g, var z, C.App (var mkV) (var k)]
  , agrees "foldr on an empty vector" (ReduceFoldr 0 True)
      (untypedPrim "Clash.Sized.Vector.foldr") [intTy, intTy, numTy 0] [var g, var z, var (v 0)]
  , agrees "dfold" (ReduceDFold 2)
      (untypedPrim "Clash.Sized.Vector.dfold") [intTy, numTy 2, intTy]
      [natLit 2, var motive, var df, var z, var (v 2)]
  , agrees "++" (ReduceAppend 2 1)
      (untypedPrim "Clash.Sized.Vector.++") [numTy 2, intTy, numTy 1] [var (v 2), var (v 1)]
  , agrees "head" (ReduceHead 2)
      (untypedPrim "Clash.Sized.Vector.head") [numTy 1, intTy] [var (v 2)]
  , agrees "tail" (ReduceTail 2)
      (untypedPrim "Clash.Sized.Vector.tail") [numTy 1, intTy] [var (v 2)]
  , agrees "last" (ReduceLast 3)
      (untypedPrim "Clash.Sized.Vector.last") [numTy 2, intTy] [var (v 3)]
  , agrees "init" (ReduceInit 2 True)
      (untypedPrim "Clash.Sized.Vector.init") [numTy 2, intTy] [var (v 3)]
  , agrees "init to an empty vector" (ReduceInit 0 True)
      (untypedPrim "Clash.Sized.Vector.init") [numTy 0, intTy] [var (v 1)]
  , agrees "unconcat" (ReduceUnconcat 2 2)
      (untypedPrim "Clash.Sized.Vector.unconcat") [numTy 2, numTy 2, intTy]
      [natLit 2, snat 2, var (v 4)]
  , agrees "unconcat into empty vectors" (ReduceUnconcat 2 0)
      (untypedPrim "Clash.Sized.Vector.unconcat") [numTy 2, numTy 0, intTy]
      [natLit 2, snat 0, var (v 0)]
  , agrees "unconcat into an empty vector" (ReduceUnconcat 0 2)
      (untypedPrim "Clash.Sized.Vector.unconcat") [numTy 0, numTy 2, intTy]
      [natLit 0, snat 2, var (v 0)]
  , agrees "transpose" (ReduceTranspose 2)
      (untypedPrim "Clash.Sized.Vector.transpose") [numTy 2, numTy 0, intTy] [natLit 2, var xss]
  , agrees "replicate" (ReduceReplicate 3)
      (typedPrim "Clash.Sized.Vector.replicate" $ \_ n a -> funTys [snatTy n, a] (vecTy n a))
      [numTy 3, intTy] [snat 3, var k]
  , agrees "replace_int" (ReduceReplaceInt 2)
      (typedPrim "Clash.Sized.Vector.replace_int" $ \_ n a ->
         funTys [naturalPrimTy, vecTy n a, hIntTy, a] (vecTy n a))
      [numTy 2, intTy] [natLit 2, var (v 2), var i, var k]
  , agrees "index_int" (ReduceIndexInt 2)
      (untypedPrim "Clash.Sized.Vector.index_int") [numTy 2, intTy] [natLit 2, var (v 2), var i]
  , agrees "index_int on an empty vector" (ReduceIndexInt 0)
      (untypedPrim "Clash.Sized.Vector.index_int") [numTy 0, intTy] [natLit 0, var (v 0), var i]
  , agrees "imap" (ReduceImap 2)
      (untypedPrim "Clash.Sized.Vector.imap") [numTy 2, intTy, intTy]
      [natLit 2, var imapF, var (v 2)]
  , agrees "iterateI" (ReduceIterateI 3)
      (typedPrim "Clash.Sized.Vector.iterateI" $ \_ n a ->
         funTys [naturalPrimTy, C.mkFunTy a a, a] (vecTy n a))
      [numTy 3, intTy] [natLit 3, var f, var k]
  , agrees "iterateI of an empty vector" (ReduceIterateI 0)
      (typedPrim "Clash.Sized.Vector.iterateI" $ \_ n a ->
         funTys [naturalPrimTy, C.mkFunTy a a, a] (vecTy n a))
      [numTy 0, intTy] [natLit 0, var f, var k]
  , agrees "dtfold" (ReduceDTFold 2)
      (untypedPrim "Clash.Sized.Vector.dtfold") [intTy, numTy 2, intTy]
      [natLit 2, var motive, var f, var br, var (v 4)]
  , agrees "reverse" (ReduceReverse 3)
      (untypedPrim "Clash.Sized.Vector.reverse") [numTy 3, intTy] [var (v 3)]
  , agrees "tdfold" (ReduceTDFold 1)
      (untypedPrim "Clash.Sized.RTree.tdfold") [intTy, numTy 1, intTy]
      [natLit 1, var motive, var f, var br, var tree]
  , agrees "treplicate" (ReduceTReplicate 2)
      (typedPrim "Clash.Sized.RTree.treplicate" $ \_ d a -> funTys [snatTy d, a] (rtreeTy d a))
      [numTy 2, intTy] [snat 2, var k]
  ]
 where
  agrees nm red pr tys tms =
    testCase nm (assertAgrees ([f, g, mkV, dict, h, df, br, imapF, motive, tree, xss, k, z, i] <> map v [0 .. 4]) red pr tys tms)

-- | The reduction of @map f v@ on an empty vector @v :: Vec 0 Int@, with @f
-- :: Int -> Bool@, gives an empty vector of @Bool@s.
--
-- The reduction of Clash Core gave an empty vector of @Int@s, of the
-- argument element type, so its result was ill-typed.
mapEmptyResultElemTy :: Assertion
mapEmptyResultElemTy = do
  let
    fb = localId User "fb" 40 (C.mkFunTy intTy boolTy)
    e = C.mkApps (C.Prim (untypedPrim "Clash.Sized.Vector.map"))
          [Right intTy, Right boolTy, Right (numTy 0), Left (var fb), Left (var (v 0))]
  (actual, _, _) <-
    runRewriteTest env st (scopedPrimReduction (ReduceMap 0 True) (TransformContext (inScopeOf [fb, v 0]) []) e)
  assertBool ("Expected Nil @Bool, got:\n" <> showPprU actual)
    (aeqTermTyped (stripCoercionTypes (mkVecNil nilDc boolTy)) (stripCoercionTypes actual))

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.PrimitiveReductions"
  [ testGroup "tests of the reductions of Clash Core" (primitiveReductionTests scopedPrimReduction)
  , testGroup "agrees with the reductions of Clash Core" agreementTests
  , testCase "map on an empty vector gives an empty vector of the result element type"
      mapEmptyResultElemTy
  ]
