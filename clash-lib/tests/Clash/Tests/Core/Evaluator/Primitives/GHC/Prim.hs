{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Evaluator.Primitives.GHC.Prim" on fixed inputs:
  reductions with unboxed tuples as results, the reductions that combinators
  build, and the byte arrays on the primitive heap.

  The unboxed tuples of "GHC.Prim" are 'T2' here: the reductions only look at
  the only constructor of the result type.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Evaluator.Primitives.GHC.Prim (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal (..))
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Builder (TmB, TyB, dataB, literalB, tyAppB)
import Clash.Core.Syntax (PrimInfo (..), S (VoidS), Term (..))
import Clash.Core.Term (collectArgs)
import Clash.Core.Alpha (aeqTy)

import Test.Clash.Rewrite
  (ghcName, ghcTrueDataCon, mkTyConName, showPprU, t2TyConName, testTyConMap)
import Test.Clash.Core (assertTermAlphaEq, closedType)
import Test.Clash.Core.Evaluator (allocatedBody)
import Test.Clash.Core.Evaluator.Primitives

-- | Evaluate with the reductions, as the subject of a case-expression
evalT :: TmB -> Term 'VoidS
evalT = allocatedBody . evalSubj

-- | Assert that a term evaluates ('evalT') to the given one
(@~>) :: HasCallStack => TmB -> TmB -> Assertion
t @~> expected = assertTermAlphaEq (closed expected) (evalT t)
infix 1 @~>

-- * Unboxed tuples

t2Ty :: TyB -> TyB -> TyB
t2Ty a b = tc t2TyConName [a, b]

-- | The constructor of 'T2'
t2Dc :: DataCon
t2Dc = case UniqMap.lookup t2TyConName testTyConMap of
  Just tcon | [dc] <- tyConDataCons tcon -> dc
  _ -> error "T2 is not in testTyConMap"

-- | @case scrut of MkT2 x y -> k x y@, with binders of the given types and
-- uniques
caseT2 :: TmB -> (TyB, Int) -> (TyB, Int) -> (TmB -> TmB -> TmB) -> TmB
caseT2 scrut (aTy, i) (bTy, j) k =
  caseData scrut wordPrimTy t2Dc [("x_" <> show i, aTy), ("y_" <> show j, bTy)] $ \case
    [x, y] -> k x y
    _ -> error "caseT2: impossible"

-- * Byte arrays

stateTy, mbaTy :: TyB -> TyB
stateTy s = tc (mkTyConName "GHC.Prim.State#" 4100) [s]
mbaTy s = tc (mkTyConName "GHC.Prim.MutableByteArray#" 4101) [s]

rwTy :: TyB
rwTy = tc (mkTyConName "GHC.Prim.RealWorld" 4102) []

-- | A primitive of type @forall s. ...@, applied to 'rwTy'
stPrim :: Text -> (TyB -> TyB) -> TmB
stPrim nm f = tyAppB (prim nm (forAll "s_9100" typeKind f)) rwTy

realWorld :: TmB
realWorld = prim "GHC.Prim.realWorld#" (stateTy rwTy)

newByteArray :: Integer -> TmB -> TmB
newByteArray n s =
  apps (stPrim "GHC.Prim.newByteArray#" (\st -> intPrimTy ~> stateTy st ~> t2Ty (stateTy st) (mbaTy st)))
    [Left (intLit n), Left s]

-- | @case newByteArray# n realWorld# of MkT2 s mba -> k s mba@
withNew :: Integer -> Int -> (TmB -> TmB -> TmB) -> TmB
withNew n u = caseT2 (newByteArray n realWorld) (stateTy rwTy, u) (mbaTy rwTy, u + 1)

-- | @case unsafeFreezeByteArray# mba s of MkT2 _ ba -> k ba@
withFrozen :: TmB -> TmB -> Int -> (TmB -> TmB) -> TmB
withFrozen mba s u k =
  caseT2
    (apps (stPrim "GHC.Prim.unsafeFreezeByteArray#" (\st -> mbaTy st ~> stateTy st ~> t2Ty (stateTy st) byteArrayPrimTy))
       [Left mba, Left s])
    (stateTy rwTy, u) (byteArrayPrimTy, u + 1) (const k)

setByteArray :: TmB -> Integer -> Integer -> Integer -> TmB -> TmB
setByteArray mba off len c s =
  apps (stPrim "GHC.Prim.setByteArray#" (\st -> mbaTy st ~> intPrimTy ~> intPrimTy ~> intPrimTy ~> stateTy st ~> stateTy st))
    [Left mba, Left (intLit off), Left (intLit len), Left (intLit c), Left s]

writeWordArray :: TmB -> Integer -> Integer -> TmB -> TmB
writeWordArray mba i w s =
  apps (stPrim "GHC.Prim.writeWordArray#" (\st -> mbaTy st ~> intPrimTy ~> wordPrimTy ~> stateTy st ~> stateTy st))
    [Left mba, Left (intLit i), Left (wordLit w), Left s]

sizeofByteArray :: TmB -> TmB
sizeofByteArray ba = apps (prim "GHC.Prim.sizeofByteArray#" (byteArrayPrimTy ~> intPrimTy)) [Left ba]

indexWordArray :: TmB -> Integer -> TmB
indexWordArray ba i =
  apps (prim "GHC.Prim.indexWordArray#" (byteArrayPrimTy ~> intPrimTy ~> wordPrimTy))
    [Left ba, Left (intLit i)]

byteArrayTests :: [TestTree]
byteArrayTests =
  [ testCase "newByteArray# gives a closed MutableByteArray# applied to s" $
      case collectArgs (evalT (newByteArray 8 realWorld)) of
        (Data dc, [Right _, Right _, Left (Prim rw), Left mba])
          | dc == t2Dc
          , primName rw == ghcName "GHC.Prim.realWorld#"
          , (Prim p, [Right sTy, Left (Literal (IntLiteral 0))]) <- collectArgs mba
          -> do
            -- The type of the primitive is closed by construction: it is a
            -- 'Type' of the empty scope
            primName p @?= ghcName "GHC.Prim.MutableByteArray#"
            assertBool "MutableByteArray# isn't applied to RealWorld"
              (aeqTy (closedType "GHC.Prim.RealWorld_4102") sTy)
        (r, _) -> assertFailure ("unexpected result: " <> showPprU r)

  , testCase "setByteArray#, writeWordArray#, indexWordArray#" $
      let at i =
            withNew 16 100 $ \s1 mba ->
              withFrozen mba (writeWordArray mba 1 42 (setByteArray mba 0 16 255 s1)) 102 $ \ba ->
                indexWordArray ba i
      in do
        at 0 @~> wordLit (2 ^ (64 :: Int) - 1)
        at 1 @~> wordLit 42

  , testCase "sizeofByteArray#" $
      withNew 16 100 (\s1 mba -> withFrozen mba s1 102 sizeofByteArray) @~> intLit 16

  , testCase "readWordArray# reads what writeWordArray# wrote" $
      withNew 16 100 (\s1 mba ->
        caseT2
          (apps (stPrim "GHC.Prim.readWordArray#" (\st -> mbaTy st ~> intPrimTy ~> stateTy st ~> t2Ty (stateTy st) wordPrimTy))
             [Left mba, Left (intLit 1), Left (writeWordArray mba 1 7 s1)])
          (stateTy rwTy, 102) (wordPrimTy, 103) (\_ w -> w))
        @~> wordLit 7

  , testCase "resizeMutableByteArray# gives a byte array of the new size" $
      withNew 8 100 (\s1 mba ->
        caseT2
          (apps (stPrim "GHC.Prim.resizeMutableByteArray#" (\st -> mbaTy st ~> intPrimTy ~> stateTy st ~> t2Ty (stateTy st) (mbaTy st)))
             [Left mba, Left (intLit 32), Left s1])
          (stateTy rwTy, 102) (mbaTy rwTy, 103) $ \s2 mba2 ->
            withFrozen mba2 s2 104 sizeofByteArray)
        @~> intLit 32

  , testCase "shrinkMutableByteArray#" $
      withNew 8 100 (\s1 mba ->
        withFrozen mba
          (apps (stPrim "GHC.Prim.shrinkMutableByteArray#" (\st -> mbaTy st ~> intPrimTy ~> stateTy st ~> stateTy st))
             [Left mba, Left (intLit 4), Left s1])
          102 sizeofByteArray)
        @~> intLit 4

  , testCase "copyByteArray#" $
      let copyTy st = byteArrayPrimTy ~> intPrimTy ~> mbaTy st ~> intPrimTy ~> intPrimTy ~> stateTy st ~> stateTy st
          at i =
            withNew 16 100 $ \s1 src ->
              withFrozen src (setByteArray src 0 16 1 s1) 102 $ \srcBa ->
                withNew 16 104 $ \s2 dst ->
                  withFrozen dst
                    (apps (stPrim "GHC.Prim.copyByteArray#" copyTy)
                       [Left srcBa, Left (intLit 0), Left dst, Left (intLit 8), Left (intLit 8), Left s2])
                    106 (`indexWordArray` i)
      in do
        at 0 @~> wordLit 0
        at 1 @~> wordLit 0x0101010101010101

  , testCase "copyAddrToByteArray#" $
      let copyTy st = stringPrimTy ~> mbaTy st ~> intPrimTy ~> intPrimTy ~> stateTy st ~> stateTy st
      in withNew 8 100 (\s1 mba ->
           withFrozen mba
             (apps (stPrim "GHC.Prim.copyAddrToByteArray#" copyTy)
                [Left (literalB (StringLiteral "abcdefgh")), Left mba, Left (intLit 0), Left (intLit 8), Left s1])
             102 (`indexWordArray` 0))
           @~> wordLit 0x6867666564636261
  ]

-- * Other reductions

otherTests :: [TestTree]
otherTests =
  [ testCase "addIntC# builds the constructor of its result type" $
      apps (prim "GHC.Prim.addIntC#" (intPrimTy ~> intPrimTy ~> t2Ty intPrimTy intPrimTy))
        [Left (intLit (2 ^ (63 :: Int) - 1)), Left (intLit 1)]
        @~> apps (dataB t2Dc)
              [Right intPrimTy, Right intPrimTy, Left (intLit (-2 ^ (63 :: Int))), Left (intLit 1)]

  , testCase "quotRemWord# by zero is undefined in both fields" $
      let undef = undefinedAt (t2Ty wordPrimTy wordPrimTy)
      in apps (prim "GHC.Prim.quotRemWord#" (wordPrimTy ~> wordPrimTy ~> t2Ty wordPrimTy wordPrimTy))
           [Left (wordLit 7), Left (wordLit 0)]
           @~> apps (dataB t2Dc) [Right wordPrimTy, Right wordPrimTy, Left undef, Left undef]

  , testCase "quotRemWord2# divides a two-word number" $
      -- 2^64 `quotRem` 3
      apps (prim "GHC.Prim.quotRemWord2#" (wordPrimTy ~> wordPrimTy ~> wordPrimTy ~> t2Ty wordPrimTy wordPrimTy))
        [Left (wordLit 1), Left (wordLit 0), Left (wordLit 3)]
        @~> apps (dataB t2Dc)
              [Right wordPrimTy, Right wordPrimTy, Left (wordLit 0x5555555555555555), Left (wordLit 1)]

  , testCase "quotWord# by zero is undefined (liftLit2)" $
      apps (prim "GHC.Prim.quotWord#" (wordPrimTy ~> wordPrimTy ~> wordPrimTy))
        [Left (wordLit 7), Left (wordLit 0)]
        @~> undefinedAt wordPrimTy

  , testCase "popCnt# (liftLit1)" $
      apps (prim "GHC.Prim.popCnt#" (wordPrimTy ~> wordPrimTy)) [Left (wordLit 0b1011)]
        @~> wordLit 3

  , testCase "narrow8Int#" $
      apps (prim "GHC.Prim.narrow8Int#" (intPrimTy ~> intPrimTy)) [Left (intLit 300)]
        @~> intLit 44

  , testCase "tagToEnum#" $
      apps (prim "GHC.Prim.tagToEnum#" (forAll "a_9101" typeKind (\a -> intPrimTy ~> a)))
        [Right ghcBoolTy, Left (intLit 1)]
        @~> dataB ghcTrueDataCon

  , testCase "dataToTag#" $
      apps (prim dataToTagName (ghcBoolTy ~> intPrimTy)) [Left (dataB ghcTrueDataCon)]
        @~> intLit 1
  ]
 where
#if MIN_VERSION_ghc(9,10,0)
  dataToTagName = "GHC.Prim.dataToTagLarge#"
#else
  dataToTagName = "GHC.Prim.dataToTag#"
#endif

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Evaluator.Primitives.GHC.Prim"
  [ testGroup "byte arrays" byteArrayTests
  , testGroup "other reductions" otherTests
  ]
