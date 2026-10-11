{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.PrimitiveReductions". The tests take the
  implementation of the reductions as a parameter ('PrimReductionImpl'), so
  that "Clash.Tests.Normalize.Scoped.PrimitiveReductions" runs them on those of
  "Clash.Normalize.Scoped.PrimitiveReductions" too.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Normalize.PrimitiveReductions
  ( tests
  , PrimReductionImpl
  , currentPrimReduction
  , primitiveReductionTests
  , envVec
  , prim
  ) where

import qualified Control.Lens as Lens
import qualified Data.Either as Either
import Data.Default (def)
import Data.Text (Text)
import GHC.Types.SrcLoc (noSrcSpan)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.HasType (inferCoreTypeOf)
import Clash.Core.Name (Name (..), NameSort (..))
import Clash.Core.Literal (Literal (..))
import Clash.Core.Pretty (showPpr)
import Clash.Core.Subst (extendTvSubst, mkSubst, substTm)
import Clash.Core.Term
  ( IsMultiPrim (..), PrimInfo (..), PrimUnfolding (..), Term (..)
  , WorkInfo (..), collectArgs, collectTermIds, mkApps )
import Clash.Core.Type (LitTy (..), Type (..), mkFunTy)
import Clash.Core.TysPrim (naturalPrimTy, typeNatKind)
import Clash.Core.Var (Var (..))
import Clash.Core.VarEnv (extendInScopeSet)
import Clash.Normalize.PrimitiveReductions
import Clash.Normalize.Scoped.Transformations.Reduce (PrimReduction (..))
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), TransformContext, tcCache)
import Clash.Rewrite.Util (runRewrite)
import Clash.Unique (Unique)
import Clash.Util.Supply (freshId, newSupply)

import Test.Clash.Rewrite
  ( globalId, inScopeOf, intFunTy, intId, intTy, kindedTyVar
  , localId, nilDc, runRewriteTest, runSingleTransformation, showPprU
  , testTyConMap, tyVar, vecTy, withTyConMap )
import Test.Clash.Scoped (aeqTermTyped, stripCoercionTypes)

-- | The default rewrite environment, extended with @Vec@ and the other type
-- constructors of 'testTyConMap'
envVec :: RewriteEnv
envVec = withTyConMap testTyConMap def

-- | An implementation of the primitive reductions: a rewrite of the saturated
-- application of a primitive, which runs the given reduction on it
type PrimReductionImpl = PrimReduction -> NormRewrite

-- | The reductions of "Clash.Normalize.PrimitiveReductions", given the
-- parameters 'Clash.Normalize.Scoped.Transformations.Reduce.reducePrim'
-- decided on, as 'Clash.Normalize.Transformations.Reduce.reducePrim' calls
-- them
currentPrimReduction :: HasCallStack => PrimReductionImpl
currentPrimReduction red ctx e = case collectArgs e of
  (Prim p, args) -> do
    tcm <- Lens.view tcCache
    let (tms, tys) = Either.partitionEithers args
    reduction red p tys tms (inferCoreTypeOf tcm e) ctx
  _ -> error ("currentPrimReduction: not a primitive application: " <> showPpr e)

-- | Run a reduction on the type and term arguments of a primitive, given the
-- type of its saturated application
reduction
  :: HasCallStack
  => PrimReduction -> PrimInfo -> [Type] -> [Term] -> Type -> TransformContext
  -> NormalizeSession Term
reduction red p tys tms resTy ctx = case (red, tys, tms) of
  (ReduceZipWith n lRep rRep, [lhsElTy, rhsElTy, resElTy, _], [f, xs, ys]) ->
    reduceZipWith p n lhsElTy rhsElTy resElTy lRep rRep f xs ys ctx
  (ReduceMap n argRep, [argElTy, resElTy, _], [f, xs]) ->
    reduceMap p n argElTy resElTy argRep f xs ctx
  (ReduceTraverse n, [aTy, fTy, bTy, _], [dict, f, xs]) ->
    reduceTraverse n aTy fTy bTy dict f xs ctx
  (ReduceFold n, [_, aTy], [f, xs]) ->
    reduceFold n aTy f xs ctx
  (ReduceFoldr n aRep, [aTy, _, _], [f, z, xs]) ->
    reduceFoldr p n aTy aRep f z xs ctx
  (ReduceDFold n, [_, _, aTy], [kn, proxy, f, z, xs]) ->
    reduceDFold n aTy kn proxy f z xs ctx
  (ReduceAppend n m, [_, aTy, _], [xs, ys]) ->
    reduceAppend n m aTy xs ys ctx
  (ReduceHead n, [_, aTy], [xs]) ->
    reduceHead n aTy xs ctx
  (ReduceTail n, [_, aTy], [xs]) ->
    reduceTail n aTy xs ctx
  (ReduceLast n, [_, aTy], [xs]) ->
    reduceLast n aTy xs ctx
  (ReduceInit n aRep, [_, aTy], [xs]) ->
    reduceInit p n aTy aRep xs ctx
  (ReduceUnconcat n m, [_, _, aTy], [kn, sm, xs]) ->
    reduceUnconcat p n m aTy kn sm xs ctx
  (ReduceTranspose n, [_, _, aTy], [kn, xss]) ->
    reduceTranspose n 0 aTy kn xss ctx
  (ReduceReplicate n, [_, aTy], [sn, x]) ->
    reduceReplicate n aTy resTy sn x ctx
  (ReduceReplaceInt n, [_, aTy], [kn, xs, i, x]) ->
    reduceReplace_int n aTy resTy kn xs i x ctx
  (ReduceIndexInt n, [_, aTy], [kn, xs, i]) ->
    reduceIndex_int n aTy kn xs i ctx
  (ReduceImap n, [_, argElTy, resElTy], [kn, f, xs]) ->
    reduceImap n argElTy resElTy kn f xs ctx
  (ReduceIterateI n, [_, aTy], [kn, f, x]) ->
    reduceIterateI n aTy resTy kn f x ctx
  (ReduceDTFold n, [_, _, aTy], [kn, proxy, lrFun, brFun, xs]) ->
    reduceDTFold n aTy kn proxy lrFun brFun xs ctx
  (ReduceReverse n, [_, aTy], [xs]) ->
    reduceReverse n aTy xs ctx
  (ReduceTDFold n, [_, _, aTy], [kn, proxy, lrFun, brFun, t]) ->
    reduceTFold n aTy kn proxy lrFun brFun t ctx
  (ReduceTReplicate n, [_, aTy], [sn, x]) ->
    reduceTReplicate n aTy resTy sn x ctx
  _ ->
    error ("currentPrimReduction: unexpected arguments for " <> show red <> ": "
           <> showPpr (mkApps (Prim p) (map Right tys <> map Left tms)))

-- | A primitive with a name and type
prim :: Text -> Type -> PrimInfo
prim nm ty = PrimInfo nm ty WorkNever SingleResult NoUnfolding

-- | Type variables of the types of the primitives
tvN, tvA, tvB :: Var Type
tvN = kindedTyVar User "n" 50 typeNatKind
tvA = tyVar User "a" 51
tvB = tyVar User "b" 52

-- | @map :: forall a b n. (a -> b) -> Vec n a -> Vec n b@
mapPrim :: PrimInfo
mapPrim = prim "Clash.Sized.Vector.map" $
  ForAllTy tvA (ForAllTy tvB (ForAllTy tvN
    (mkFunTy (mkFunTy (VarTy tvA) (VarTy tvB))
      (mkFunTy (vecTy (VarTy tvN) (VarTy tvA)) (vecTy (VarTy tvN) (VarTy tvB))))))

-- | Run a reduction on an empty vector @v :: Vec 0 Int@, given the application
-- of the primitive to @v@. The reductions of primitives returning a vector
-- usually don't see empty vectors, as
-- 'Clash.Normalize.Transformations.Reduce.reducePrim' replaces a fully applied
-- primitive returning @Vec 0 a@ by @Nil@. They do when the primitive is only
-- partially applied.
reduceEmptyVec :: PrimReductionImpl -> PrimReduction -> (Term -> Term) -> IO Term
reduceEmptyVec impl red app =
  runSingleTransformation envVec def (inScopeOf [v, idxI, elA, fnF]) (impl red) (app (Var v))
 where
  v = localId User "v" 1 (vecTy (LitTy (NumTy 0)) intTy)

-- | An index @i@ and an element @a@, to give @replace_int@
idxI, elA :: Var Term
idxI = intId User "i" 2
elA = intId User "a" 3

-- | A function @f :: Int -> Int -> Int@, to give @map@
fnF :: Var Term
fnF = localId User "f" 4 (intFunTy 2)

-- | Run the reduction of @map f v@, with @v :: Vec 1 a@ and @f :: a -> a@
-- free, where the type variable @a@ has unique 0. Then substitute @Int@ for
-- @a@, like specializing the function this term belongs to would.
--
-- The reduction of @map@ extracts the head and tail of @v@ with a
-- case-expression, whose pattern binds the existential type variable of
-- @Cons@, and the element of @v@ at type @a@:
--
-- > case v of Cons @m _co (el :: a) res -> el
reduceMapSubstElemTy :: PrimReductionImpl -> IO Term
reduceMapSubstElemTy impl = do
  let
    aV = tyVar User "a" 0
    aTy = VarTy aV
    v = localId User "v" 1000 (vecTy (LitTy (NumTy 1)) aTy)
    f = localId User "f" 1001 (mkFunTy aTy aTy)
    is0 = extendInScopeSet (inScopeOf [v, f]) aV
    mapApp =
      mkApps (Prim mapPrim)
        [Right aTy, Right aTy, Right (LitTy (NumTy 1)), Left (Var f), Left (Var v)]
  res <- runSingleTransformation envVec def is0 (impl (ReduceMap 1 False)) mapApp
  pure (substTm "reduceMapSubstElemTy" (extendTvSubst (mkSubst is0) aV intTy) res)

-- | Run the reduction of @fold f v@, with @v :: Vec 2 a@ free, @f :: a -> a ->
-- a@ a global function, and @a@ a function type of many arguments. Returns the
-- result, and the next unique the unique supply hands out afterwards.
--
-- The reduction of @fold@ applies 'constantPropagation' to @f@, which
-- eta-expands it using a fresh unique per argument: @\x1 .. xn -> f x1 ..
-- xn@. That's more uniques than the reduction uses itself to bind the elements
-- of @v@.
reduceFoldNextUnique :: PrimReductionImpl -> IO (Term, Unique)
reduceFoldNextUnique impl = do
  supply <- newSupply
  let
    aTy = intFunTy 16
    v = localId User "v" 1 (vecTy (LitTy (NumTy 2)) aTy)
    f = globalId User "f" 2 (mkFunTy aTy (mkFunTy aTy aTy))
    is0 = inScopeOf [v]
    -- fold :: forall n a. (a -> a -> a) -> Vec (n + 1) a -> a
    foldApp =
      mkApps (Prim (prim "Clash.Sized.Vector.fold" intTy))
        [Right (LitTy (NumTy 1)), Right aTy, Left (Var f), Left (Var v)]
    -- Some transformations of 'constantPropagation' look at the function being
    -- normalized
    st0 = def{_uniqSupply = supply, _curFun = (globalId User "top" 3 intTy, noSrcSpan)}
  (res, st, _) <-
    runRewriteTest envVec st0 (runRewrite "reduceFold" is0 (impl (ReduceFold 2)) foldApp)
  pure (res, fst (freshId (_uniqSupply st)))

-- | Assert that two terms are alpha-equivalent, up to the form of coercions:
-- the scoped reductions apply @_CO_@ to its type, see 'stripCoercionTypes'
assertAlphaEqCo :: Term -> Term -> Assertion
assertAlphaEqCo expected actual =
  assertBool
    ("Expected (up to alpha equivalence):\n" <> showPprU expected <> "\nbut got:\n" <> showPprU actual)
    (aeqTermTyped (stripCoercionTypes expected) (stripCoercionTypes actual))

-- | The tests of the primitive reductions, for the given implementation of
-- them
primitiveReductionTests :: PrimReductionImpl -> [TestTree]
primitiveReductionTests impl =
    -- 'extractElems' only works on non-empty vectors
  [ testCase "reduceReverse on an empty vector" $
      reduceEmptyVec impl (ReduceReverse 0)
        (\v -> mkApps (Prim (prim "Clash.Sized.Vector.reverse" intTy))
                 [Right (LitTy (NumTy 0)), Right intTy, Left v])
        >>= assertAlphaEqCo (mkVecNil nilDc intTy)
  , testCase "reduceReplace_int on an empty vector" $
      reduceEmptyVec impl (ReduceReplaceInt 0)
        (\v -> mkApps (Prim replaceIntPrim)
                 [ Right (LitTy (NumTy 0)), Right intTy
                 , Left (Literal (NaturalLiteral 0)), Left v, Left (Var idxI), Left (Var elA) ])
        >>= assertAlphaEqCo (mkVecNil nilDc intTy)

    -- 'extractHeadTail' used fixed uniques for the binders of the pattern. If
    -- the element type mentions a type variable with the unique of the type
    -- variable the pattern binds, that pattern captures it. Substituting for
    -- the type variable then no longer changes the type of the element.
  , testCase "reduceMap binds the elements of a vector at their type" $ do
      actual <- reduceMapSubstElemTy impl
      let els = filter ((== "el") . nameOcc . varName) (collectTermIds actual)
      assertBool ("Expected elements in:\n" <> showPprU actual) (not (null els))
      mapM_
        (\el -> assertBool
           ("Element " <> showPprU el <> " is not of type Int in:\n" <> showPprU actual)
           (varType el == intTy))
        els

    -- 'reduceFold' read the unique supply before applying 'constantPropagation'
    -- to the function, and wrote back a supply derived from that afterwards.
    -- This rewound the supply, so it would hand out the uniques
    -- 'constantPropagation' used once more.
  , testCase "reduceFold does not rewind the unique supply" $ do
      (actual, next) <- reduceFoldNextUnique impl
      assertBool
        ("Next unique " <> show next <> " is already used in:\n" <> showPprU actual)
        (next `notElem` map varUniq (collectTermIds actual))

    -- 'reduceMap' gave an empty vector of the argument element type, rather
    -- than one of the result element type
  , testCase "reduceMap on an empty vector gives an empty vector of the result element type" $
      reduceEmptyVec impl (ReduceMap 0 True)
        (\v -> mkApps (Prim mapPrim)
                 [Right intTy, Right (intFunTy 1), Right (LitTy (NumTy 0)), Left (Var fnF), Left v])
        >>= assertAlphaEqCo (mkVecNil nilDc (intFunTy 1))
  ]
 where
  -- replace_int :: forall n a. KnownNat n => Vec n a -> Int -> a -> Vec n a
  replaceIntPrim = prim "Clash.Sized.Vector.replace_int" $
    ForAllTy tvN (ForAllTy tvA
      (foldr mkFunTy (vecTy (VarTy tvN) (VarTy tvA))
        [naturalPrimTy, vecTy (VarTy tvN) (VarTy tvA), intTy, VarTy tvA]))

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.PrimitiveReductions"
    (primitiveReductionTests currentPrimReduction)
