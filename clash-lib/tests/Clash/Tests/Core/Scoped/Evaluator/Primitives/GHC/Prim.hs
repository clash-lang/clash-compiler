{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives.GHC.Prim" on fixed inputs:
  reductions with unboxed tuples as results, the reductions that combinators
  build, and the byte arrays on the primitive heap, which
  @clash-differential prims@ can't generate.

  The unboxed tuples of "GHC.Prim" are 'T2' here: the reductions only look at
  the only constructor of the result type.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives.GHC.Prim (tests) where

import qualified Control.Lens as Lens
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.FreeVars (typeFreeVars)
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (mkUnsafeSystemName)
import qualified Clash.Core.Term as C
import Clash.Core.Subst (aeqType)
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim
  (byteArrayPrimTy, intPrimTy, liftedTypeKind, stringPrimTy, wordPrimTy)
import qualified Clash.Core.Var as C
import qualified Clash.Data.UniqMap as UniqMap
import qualified Clash.Normalize.Primitives as NP
import Clash.Unique (Unique)

import Test.Clash.Rewrite
  ( assertAlphaEq, ghcBoolTy, ghcTrueDataCon, heapLookupEvaluator, mkTyConName
  , t2TyConName, tcApp, testTyConMap )
import Test.Clash.Scoped (testTypeEnv)
import Test.Clash.Scoped.Evaluator (portedEnv, scopedWhnf)

-- | Evaluate with the ported reductions, as the subject of a case-expression
eval :: C.Term -> C.Term
eval = allocatedBody . scopedWhnf (portedEnv testTypeEnv heapLookupEvaluator) mempty True
 where
  allocatedBody = \case
    C.Letrec _ e -> e
    e -> e

-- | A primitive with the given name and type
prim :: Text -> C.Type -> C.Term
prim nm ty = C.Prim (C.PrimInfo nm ty C.WorkNever C.SingleResult C.NoUnfolding)

intLit, wordLit :: Integer -> C.Term
intLit = C.Literal . IntLiteral
wordLit = C.Literal . WordLiteral

(~>) :: C.Type -> C.Type -> C.Type
(~>) = C.mkFunTy
infixr 5 ~>

-- * Unboxed tuples

t2Ty :: C.Type -> C.Type -> C.Type
t2Ty a b = tcApp t2TyConName [a, b]

-- | The constructor of 'T2'
t2Dc :: DataCon
t2Dc = case UniqMap.lookup t2TyConName testTyConMap of
  Just tc | [dc] <- tyConDataCons tc -> dc
  _ -> error "T2 is not in testTyConMap"

-- | @case scrut of MkT2 x y -> k x y@, with binders of the given types and
-- uniques
caseT2 :: C.Term -> (C.Type, Unique) -> (C.Type, Unique) -> (C.Term -> C.Term -> C.Term) -> C.Term
caseT2 scrut (aTy, i) (bTy, j) k =
  C.Case scrut wordPrimTy [(C.DataPat t2Dc [] [x, y], k (C.Var x) (C.Var y))]
 where
  x = C.mkLocalId aTy (mkUnsafeSystemName "x" i)
  y = C.mkLocalId bTy (mkUnsafeSystemName "y" j)

-- * Byte arrays

sTv :: C.TyVar
sTv = C.mkTyVar liftedTypeKind (mkUnsafeSystemName "s" 9100)

stateTy, mbaTy :: C.Type -> C.Type
stateTy s = tcApp (mkTyConName "GHC.Prim.State#" 4100) [s]
mbaTy s = tcApp (mkTyConName "GHC.Prim.MutableByteArray#" 4101) [s]

rwTy :: C.Type
rwTy = tcApp (mkTyConName "GHC.Prim.RealWorld" 4102) []

-- | A primitive of type @forall s. ...@, applied to 'rwTy'
stPrim :: Text -> (C.Type -> C.Type) -> C.Term
stPrim nm f = C.TyApp (prim nm (C.ForAllTy sTv (f (C.VarTy sTv)))) rwTy

realWorld :: C.Term
realWorld = prim "GHC.Prim.realWorld#" (stateTy rwTy)

newByteArray :: Integer -> C.Term -> C.Term
newByteArray n s =
  C.mkApps (stPrim "GHC.Prim.newByteArray#" (\st -> intPrimTy ~> stateTy st ~> t2Ty (stateTy st) (mbaTy st)))
    [Left (intLit n), Left s]

-- | @case newByteArray# n realWorld# of MkT2 s mba -> k s mba@
withNew :: Integer -> Unique -> (C.Term -> C.Term -> C.Term) -> C.Term
withNew n u = caseT2 (newByteArray n realWorld) (stateTy rwTy, u) (mbaTy rwTy, u + 1)

-- | @case unsafeFreezeByteArray# mba s of MkT2 _ ba -> k ba@
withFrozen :: C.Term -> C.Term -> Unique -> (C.Term -> C.Term) -> C.Term
withFrozen mba s u k =
  caseT2
    (C.mkApps (stPrim "GHC.Prim.unsafeFreezeByteArray#" (\st -> mbaTy st ~> stateTy st ~> t2Ty (stateTy st) byteArrayPrimTy))
       [Left mba, Left s])
    (stateTy rwTy, u) (byteArrayPrimTy, u + 1) (const k)

setByteArray :: C.Term -> Integer -> Integer -> Integer -> C.Term -> C.Term
setByteArray mba off len c s =
  C.mkApps (stPrim "GHC.Prim.setByteArray#" (\st -> mbaTy st ~> intPrimTy ~> intPrimTy ~> intPrimTy ~> stateTy st ~> stateTy st))
    [Left mba, Left (intLit off), Left (intLit len), Left (intLit c), Left s]

writeWordArray :: C.Term -> Integer -> Integer -> C.Term -> C.Term
writeWordArray mba i w s =
  C.mkApps (stPrim "GHC.Prim.writeWordArray#" (\st -> mbaTy st ~> intPrimTy ~> wordPrimTy ~> stateTy st ~> stateTy st))
    [Left mba, Left (intLit i), Left (wordLit w), Left s]

sizeofByteArray :: C.Term -> C.Term
sizeofByteArray ba = C.App (prim "GHC.Prim.sizeofByteArray#" (byteArrayPrimTy ~> intPrimTy)) ba

indexWordArray :: C.Term -> Integer -> C.Term
indexWordArray ba i =
  C.mkApps (prim "GHC.Prim.indexWordArray#" (byteArrayPrimTy ~> intPrimTy ~> wordPrimTy))
    [Left ba, Left (intLit i)]

byteArrayTests :: [TestTree]
byteArrayTests =
  [ testCase "newByteArray# gives a closed MutableByteArray# applied to s" $
      case C.collectArgs (eval (newByteArray 8 realWorld)) of
        (C.Data dc, [Right _, Right _, Left (C.Prim rw), Left mba])
          | dc == t2Dc
          , C.primName rw == "GHC.Prim.realWorld#"
          , (C.Prim p, [Right sTy, Left (C.Literal (IntLiteral 0))]) <- C.collectArgs mba
          -> do
            C.primName p @?= "GHC.Prim.MutableByteArray#"
            assertBool "the type of MutableByteArray# has free type variables" $
              null (Lens.toListOf typeFreeVars (C.primType p))
            assertBool "MutableByteArray# isn't applied to RealWorld" (aeqType sTy rwTy)
        r -> assertFailure ("unexpected result: " <> show r)

  , testCase "setByteArray#, writeWordArray#, indexWordArray#" $
      let at i =
            withNew 16 100 $ \s1 mba ->
              withFrozen mba (writeWordArray mba 1 42 (setByteArray mba 0 16 255 s1)) 102 $ \ba ->
                indexWordArray ba i
      in do
        assertAlphaEq (wordLit (2 ^ (64 :: Int) - 1)) (eval (at 0))
        assertAlphaEq (wordLit 42) (eval (at 1))

  , testCase "sizeofByteArray#" $
      assertAlphaEq (intLit 16)
        (eval (withNew 16 100 $ \s1 mba -> withFrozen mba s1 102 sizeofByteArray))

  , testCase "readWordArray# reads what writeWordArray# wrote" $
      assertAlphaEq (wordLit 7)
        (eval (withNew 16 100 $ \s1 mba ->
          caseT2
            (C.mkApps (stPrim "GHC.Prim.readWordArray#" (\st -> mbaTy st ~> intPrimTy ~> stateTy st ~> t2Ty (stateTy st) wordPrimTy))
               [Left mba, Left (intLit 1), Left (writeWordArray mba 1 7 s1)])
            (stateTy rwTy, 102) (wordPrimTy, 103) (\_ w -> w)))

  , testCase "resizeMutableByteArray# gives a byte array of the new size" $
      assertAlphaEq (intLit 32)
        (eval (withNew 8 100 $ \s1 mba ->
          caseT2
            (C.mkApps (stPrim "GHC.Prim.resizeMutableByteArray#" (\st -> mbaTy st ~> intPrimTy ~> stateTy st ~> t2Ty (stateTy st) (mbaTy st)))
               [Left mba, Left (intLit 32), Left s1])
            (stateTy rwTy, 102) (mbaTy rwTy, 103) $ \s2 mba2 ->
              withFrozen mba2 s2 104 sizeofByteArray))

  , testCase "shrinkMutableByteArray#" $
      assertAlphaEq (intLit 4)
        (eval (withNew 8 100 $ \s1 mba ->
          withFrozen mba
            (C.mkApps (stPrim "GHC.Prim.shrinkMutableByteArray#" (\st -> mbaTy st ~> intPrimTy ~> stateTy st ~> stateTy st))
               [Left mba, Left (intLit 4), Left s1])
            102 sizeofByteArray))

  , testCase "copyByteArray#" $
      let copyTy st = byteArrayPrimTy ~> intPrimTy ~> mbaTy st ~> intPrimTy ~> intPrimTy ~> stateTy st ~> stateTy st
          at i =
            withNew 16 100 $ \s1 src ->
              withFrozen src (setByteArray src 0 16 1 s1) 102 $ \srcBa ->
                withNew 16 104 $ \s2 dst ->
                  withFrozen dst
                    (C.mkApps (stPrim "GHC.Prim.copyByteArray#" copyTy)
                       [Left srcBa, Left (intLit 0), Left dst, Left (intLit 8), Left (intLit 8), Left s2])
                    106 (`indexWordArray` i)
      in do
        assertAlphaEq (wordLit 0) (eval (at 0))
        assertAlphaEq (wordLit 0x0101010101010101) (eval (at 1))

  , testCase "copyAddrToByteArray#" $
      let copyTy st = stringPrimTy ~> mbaTy st ~> intPrimTy ~> intPrimTy ~> stateTy st ~> stateTy st
      in assertAlphaEq (wordLit 0x6867666564636261)
           (eval (withNew 8 100 $ \s1 mba ->
             withFrozen mba
               (C.mkApps (stPrim "GHC.Prim.copyAddrToByteArray#" copyTy)
                  [Left (C.Literal (StringLiteral "abcdefgh")), Left mba, Left (intLit 0), Left (intLit 8), Left s1])
               102 (`indexWordArray` 0)))
  ]

-- * Other reductions

otherTests :: [TestTree]
otherTests =
  [ testCase "addIntC# builds the constructor of its result type" $
      assertAlphaEq
        (C.mkApps (C.Data t2Dc)
           [Right intPrimTy, Right intPrimTy, Left (intLit (-2 ^ (63 :: Int))), Left (intLit 1)])
        (eval (C.mkApps (prim "GHC.Prim.addIntC#" (intPrimTy ~> intPrimTy ~> t2Ty intPrimTy intPrimTy))
                 [Left (intLit (2 ^ (63 :: Int) - 1)), Left (intLit 1)]))

  , testCase "quotRemWord# by zero is undefined in both fields" $
      let undef = C.TyApp (C.Prim NP.undefined) (t2Ty wordPrimTy wordPrimTy)
      in assertAlphaEq
           (C.mkApps (C.Data t2Dc) [Right wordPrimTy, Right wordPrimTy, Left undef, Left undef])
           (eval (C.mkApps (prim "GHC.Prim.quotRemWord#" (wordPrimTy ~> wordPrimTy ~> t2Ty wordPrimTy wordPrimTy))
                    [Left (wordLit 7), Left (wordLit 0)]))

  , testCase "quotRemWord2# divides a two-word number" $
      -- 2^64 `quotRem` 3
      assertAlphaEq
        (C.mkApps (C.Data t2Dc)
           [Right wordPrimTy, Right wordPrimTy, Left (wordLit 0x5555555555555555), Left (wordLit 1)])
        (eval (C.mkApps (prim "GHC.Prim.quotRemWord2#" (wordPrimTy ~> wordPrimTy ~> wordPrimTy ~> t2Ty wordPrimTy wordPrimTy))
                 [Left (wordLit 1), Left (wordLit 0), Left (wordLit 3)]))

  , testCase "quotWord# by zero is undefined (liftLit2)" $
      assertAlphaEq (C.TyApp (C.Prim NP.undefined) wordPrimTy)
        (eval (C.mkApps (prim "GHC.Prim.quotWord#" (wordPrimTy ~> wordPrimTy ~> wordPrimTy))
                 [Left (wordLit 7), Left (wordLit 0)]))

  , testCase "popCnt# (liftLit1)" $
      assertAlphaEq (wordLit 3)
        (eval (C.App (prim "GHC.Prim.popCnt#" (wordPrimTy ~> wordPrimTy)) (wordLit 0b1011)))

  , testCase "narrow8Int#" $
      assertAlphaEq (intLit 44)
        (eval (C.App (prim "GHC.Prim.narrow8Int#" (intPrimTy ~> intPrimTy)) (intLit 300)))

  , testCase "tagToEnum#" $
      let aTv = C.mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 9101)
      in assertAlphaEq (C.Data ghcTrueDataCon)
           (eval (C.mkApps (prim "GHC.Prim.tagToEnum#" (C.ForAllTy aTv (intPrimTy ~> C.VarTy aTv)))
                    [Right ghcBoolTy, Left (intLit 1)]))

  , testCase "dataToTag#" $
      assertAlphaEq (intLit 1)
        (eval (C.App (prim dataToTagName (ghcBoolTy ~> intPrimTy)) (C.Data ghcTrueDataCon)))
  ]
 where
#if MIN_VERSION_ghc(9,10,0)
  dataToTagName = "GHC.Prim.dataToTagLarge#"
#else
  dataToTagName = "GHC.Prim.dataToTag#"
#endif

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives.GHC.Prim"
  [ testGroup "byte arrays" byteArrayTests
  , testGroup "other reductions" otherTests
  ]
