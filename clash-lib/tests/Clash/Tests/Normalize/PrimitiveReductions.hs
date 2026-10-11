{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.PrimitiveReductions"
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Normalize.PrimitiveReductions (tests) where

import Data.Default (def)
import GHC.Types.SrcLoc (noSrcSpan)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (Name (..), NameSort (..))
import Clash.Core.Literal (Literal (..))
import Clash.Core.Subst (extendTvSubst, mkSubst, substTm)
import Clash.Core.Term
  ( IsMultiPrim (..), PrimInfo (..), PrimUnfolding (..), Term (..)
  , WorkInfo (..), collectTermIds, mkApps )
import Clash.Core.Type (LitTy (..), Type (..), mkFunTy)
import Clash.Core.Var (Var (..))
import Clash.Core.VarEnv (extendInScopeSet)
import Clash.Driver.Types (ClashEnv (..))
import Clash.Normalize.PrimitiveReductions
  (mkVecNil, reduceFold, reduceMap, reduceReplace_int, reduceReverse)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..), TransformContext)
import Clash.Rewrite.Util (runRewrite)
import Clash.Unique (Unique)
import Clash.Util.Supply (freshId, newSupply)

import Test.Clash.Rewrite
  ( assertAlphaEq, globalId, inScopeOf, intFunTy, intId, intTy
  , localId, nilDc, runRewriteTest, runSingleTransformation, showPprU
  , testTyConMap, tyVar, vecA, vecM, vecN, vecTy )

-- | The default rewrite environment, extended with @Vec@
envVec :: RewriteEnv
envVec = def { _clashEnv = (_clashEnv def) { envTyConMap = testTyConMap } }

-- | Run 'reduceMap' on @map f v@, with @v :: Vec 1 a@ and @f :: a -> a@ free,
-- where the type variable @a@ has unique 0. Then substitute @Int@ for @a@,
-- like specializing the function this term belongs to would.
--
-- 'reduceMap' extracts the head and tail of @v@ with a case-expression, whose
-- pattern binds the existential type variable of @Cons@, and the element of
-- @v@ at type @a@:
--
-- > case v of Cons @m _co (el :: a) res -> el
reduceMapSubstElemTy :: IO Term
reduceMapSubstElemTy = do
  let
    a = tyVar User "a" 0
    aTy = VarTy a
    v = localId User "v" 1000 (vecTy (LitTy (NumTy 1)) aTy)
    f = localId User "f" 1001 (mkFunTy aTy aTy)
    is0 = extendInScopeSet (inScopeOf [v, f]) a
    -- map :: forall a b n. (a -> b) -> Vec n a -> Vec n b
    mapTy =
      ForAllTy vecA (ForAllTy vecM (ForAllTy vecN
        (mkFunTy (mkFunTy (VarTy vecA) (VarTy vecM))
          (mkFunTy (vecTy (VarTy vecN) (VarTy vecA))
                   (vecTy (VarTy vecN) (VarTy vecM))))))
    mapInfo =
      PrimInfo "Clash.Sized.Vector.map" mapTy WorkNever SingleResult NoUnfolding
    mapApp =
      mkApps (Prim mapInfo)
        [Right aTy, Right aTy, Right (LitTy (NumTy 1)), Left (Var f), Left (Var v)]
  res <- runSingleTransformation envVec def is0
    (\ctx _ -> reduceMap mapInfo 1 aTy aTy False (Var f) (Var v) ctx) mapApp
  pure (substTm "reduceMapSubstElemTy" (extendTvSubst (mkSubst is0) a intTy) res)

-- | Run a primitive reduction on an empty vector @v :: Vec 0 Int@, given @v@.
-- The reductions of primitives returning a vector usually don't see empty
-- vectors, as 'Clash.Normalize.Transformations.Reduce.reducePrim' replaces a
-- fully applied primitive returning @Vec 0 a@ by @Nil@. They do when the
-- primitive is only partially applied.
reduceEmptyVec :: (Term -> TransformContext -> NormalizeSession Term) -> IO Term
reduceEmptyVec reduction =
  runSingleTransformation envVec def (inScopeOf [v]) (\ctx _ -> reduction (Var v) ctx) (Var v)
 where
  v = localId User "v" 1 (vecTy (LitTy (NumTy 0)) intTy)

-- | Run 'reduceFold' on @fold f v@, with @v :: Vec 2 a@ free, @f :: a -> a ->
-- a@ a global function, and @a@ a function type of many arguments. Returns the
-- result, and the next unique the unique supply hands out afterwards.
--
-- 'reduceFold' applies 'constantPropagation' to @f@, which eta-expands it
-- using a fresh unique per argument: @\x1 .. xn -> f x1 .. xn@. That's more
-- uniques than 'reduceFold' uses itself to bind the elements of @v@.
reduceFoldNextUnique :: IO (Term, Unique)
reduceFoldNextUnique = do
  supply <- newSupply
  let
    aTy = intFunTy 16
    v = localId User "v" 1 (vecTy (LitTy (NumTy 2)) aTy)
    f = globalId User "f" 2 (mkFunTy aTy (mkFunTy aTy aTy))
    is0 = inScopeOf [v]
    -- Some transformations of 'constantPropagation' look at the function being
    -- normalized
    st0 = def{_uniqSupply = supply, _curFun = (globalId User "top" 3 intTy, noSrcSpan)}
  (res, st, _) <-
    runRewriteTest envVec st0
      (runRewrite "reduceFold" is0
        (\ctx _ -> reduceFold 2 aTy (Var f) (Var v) ctx) (Var v))
  pure (res, fst (freshId (_uniqSupply st)))

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.PrimitiveReductions"
    -- 'extractElems' only works on non-empty vectors
    [ testCase "reduceReverse on an empty vector" $
        reduceEmptyVec (reduceReverse 0 intTy)
          >>= assertAlphaEq (mkVecNil nilDc intTy)
    , testCase "reduceReplace_int on an empty vector" $
        reduceEmptyVec
          (\v -> reduceReplace_int 0 intTy (vecTy (LitTy (NumTy 0)) intTy)
                    (Literal (NaturalLiteral 0)) v (Var i) (Var a))
          >>= assertAlphaEq (mkVecNil nilDc intTy)

    -- 'extractHeadTail' used fixed uniques for the binders of the pattern. If
    -- the element type mentions a type variable with the unique of the type
    -- variable the pattern binds, that pattern captures it. Substituting for
    -- the type variable then no longer changes the type of the element.
    , testCase "reduceMap binds the elements of a vector at their type" $ do
        actual <- reduceMapSubstElemTy
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
        (actual, next) <- reduceFoldNextUnique
        assertBool
          ("Next unique " <> show next <> " is already used in:\n" <> showPprU actual)
          (next `notElem` map varUniq (collectTermIds actual))

    -- 'reduceMap' gave an empty vector of the argument element type, rather
    -- than one of the result element type
    , testCase "reduceMap on an empty vector gives an empty vector of the result element type" $
        reduceEmptyVec (reduceMap mapInfo 0 intTy (intFunTy 1) True (Var f))
          >>= assertAlphaEq (mkVecNil nilDc (intFunTy 1))
    ]
 where
  i = intId User "i" 2
  a = intId User "a" 3
  f = localId User "f" 4 (intFunTy 2)
  -- The reduction of an empty vector doesn't look at the type of 'map'
  mapInfo = PrimInfo "Clash.Sized.Vector.map" intTy WorkNever SingleResult NoUnfolding
