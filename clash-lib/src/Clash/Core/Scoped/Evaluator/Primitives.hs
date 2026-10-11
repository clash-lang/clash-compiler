{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of primitives in the whnf evaluator of scoped Clash Core
  ("Clash.Core.Scoped.Evaluator"): ports of those of
  "Clash.GHC.Evaluator.Primitive".

  Only a few are ported so far, one of each kind:

  * Integer arithmetic: literals in, a literal or constructor out. Most
    primitives are like this, and port mechanically.
  * @map@: embeds its arguments in its result.
  * @zipWith@: let-binds its arguments, so its result lives in an extension
    of the scope of the primitive.
  * @splitAt@: builds case-expressions with patterns binding fresh variables,
    and let-binds a recursive call.
  * @head@, @tail@, @(&&)@ and @(||)@: evaluate an argument with a nested
    machine.

  The others run the reduction of Clash Core through an adapter, see
  "Clash.Core.Scoped.Evaluator.Adapter".
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives
  ( scopedPrimReductions
  ) where

import Control.Monad.Trans.Except (runExcept)
import qualified Data.Either as Either
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Text.Extra (showt)
import GHC.Stack (HasCallStack)

import GHC.Builtin.Names (falseDataConKey, trueDataConKey)
import qualified GHC.Classes
import qualified GHC.Num.Integer
import qualified GHC.Prim

import qualified Clash.Sized.Vector

import Clash.Core.DataCon (DataCon (..))
import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Core.Type as C
import qualified Clash.Normalize.Primitives as NP
import Clash.Normalize.PrimitiveReductions (vecHeadPrim, vecTailPrim)
import Clash.Unique (fromGhcUnique)
import Clash.Util (curLoc)

import Clash.Core.Scoped.Evaluator
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.HasType (piResultTys, primInfoType)
import Clash.Core.Scoped.Subst (withFreshIdBinder)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (mkApps)
import Clash.Core.Scoped.Type
  (TypeView (..), mkTyConApp, splitFunTy, tyNatSize, tyView)
import Clash.Core.Scoped.TypeEnv (TypeEnv, lookupTyCon)
import Clash.Core.Scoped.Util (appendToVec, mkVec)

-- | The ported reductions, by the name of their primitive
scopedPrimReductions :: HashMap Text PrimReduction
scopedPrimReductions = HashMap.fromList
  [ (showt 'GHC.Num.Integer.integerAdd, integerOp (+))
  , (showt 'GHC.Num.Integer.integerSub, integerOp (-))
  , (showt 'GHC.Num.Integer.integerMul, integerOp (*))
  , ( showt 'GHC.Num.Integer.integerEq
    , PrimReduction $ \pc -> case pairOf integerLiteral (pcArgs pc) of
        Just (i, j) -> reduce pc (boolToBoolLiteral (envTypeEnv (pcEnv pc)) (pcInfo pc) (i == j))
        Nothing -> Nothing )
  , (showt '(GHC.Prim.+#), intOp (+))
  , (showt '(GHC.Prim.-#), intOp (-))
  , (showt '(GHC.Prim.*#), intOp (*))
  , ( showt '(GHC.Prim.==#)
    , PrimReduction $ \pc -> case pairOf intLiteral (pcArgs pc) of
        Just (i, j) -> reduce pc (boolToIntLiteral (i == j))
        Nothing -> Nothing )
  , (showt 'Clash.Sized.Vector.map, PrimReduction vecMap)
  , (showt 'Clash.Sized.Vector.zipWith, PrimReduction vecZipWith)
  , (showt 'Clash.Sized.Vector.splitAt, PrimReduction vecSplitAt)
  , ( showt 'Clash.Sized.Vector.head
    , PrimReduction $ \pc -> case pcArgs pc of
        -- head :: Vec (n+1) a -> a
        [DC _ vArgs] | pcIsSubj pc, _ : x : _ <- Either.lefts vArgs -> reduceWHNF pc x
        _ -> Nothing )
  , ( showt 'Clash.Sized.Vector.tail
    , PrimReduction $ \pc -> case pcArgs pc of
        -- tail :: Vec (n+1) a -> Vec n a
        [DC _ vArgs] | pcIsSubj pc, _ : _ : xs : _ <- Either.lefts vArgs -> reduceWHNF pc xs
        _ -> Nothing )
  , (showt '(GHC.Classes.&&), PrimReduction (andOr True))
  , (showt '(GHC.Classes.||), PrimReduction (andOr False))
  ]
 where
  integerOp f = PrimReduction $ \pc -> case pairOf integerLiteral (pcArgs pc) of
    Just (i, j) -> reduce pc (Literal (IntegerLiteral (f i j)))
    Nothing -> Nothing

  intOp f = PrimReduction $ \pc -> case pairOf intLiteral (pcArgs pc) of
    Just (i, j) -> reduce pc (integerToIntLiteral (f i j))
    Nothing -> Nothing

-- * Vectors

-- | @map :: (a -> b) -> Vec n a -> Vec n b@. Its arguments are in the
-- machine's scope, so it can embed them in its result.
vecMap :: Distinct n => PrimContext o n -> Maybe (Next o n)
vecMap pc
  | pcIsSubj pc
  , [f, DC dc vArgs] <- pcArgs pc
  , aTy : bTy : nTy : _ <- pcTys pc
  , Just n <- natSize pc nTy
  = case n of
      0 -> reduce pc (mkVec te sc dc dc bTy 0 [])
      _ | _ : x : xs : _ <- Either.lefts vArgs ->
          reduce pc $ appendToVec te sc dc bTy
            (mkApps (Prim (pcInfo pc))
              [ Right aTy, Right bTy, Right (numTy (n - 1))
              , Left (valToTerm f), Left xs ])
            n [App (valToTerm f) x]
        | otherwise -> Nothing
  | otherwise = Nothing
 where
  sc = mScope (pcMachine pc)
  te = envTypeEnv (pcEnv pc)

-- | @zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c@. It shares
-- the function and the second vector by let-binding them on the heap (see
-- #3308), so its result lives in an extension of the primitive's scope, into
-- which everything from before has to be sunk.
vecZipWith :: Distinct n => PrimContext o n -> Maybe (Next o n)
vecZipWith pc
  | pcIsSubj pc
  , aTy : bTy : cTy : nTy : _ <- pcTys pc
  , [f, DC dc vArgs, ys] <- pcArgs pc
  , (_, C.tyView -> C.TyConApp vecTcNm _) <- C.splitFunForallTy (C.primType (pcInfo pc))
  , Just n <- natSize pc nTy
  = case n of
      0 -> reduce pc (mkVec (envTypeEnv env) (mScope m) dc dc cTy 0 [])
      _ | _ : x : xs : _ <- Either.lefts vArgs ->
          newLetBinding env m (valToTerm f) $ \m1 fV ->
            newLetBinding env m1 (sink (valToTerm ys)) $ \m2 ysV ->
              let n1 = numTy (n - 1)
                  fT = sink (heapVarTerm fV)
                  ysT = heapVarTerm ysV
                  bTy' = sink bTy
              in reduceIn pc m2 $ appendToVec (envTypeEnv env) (mScope m2) dc (sink cTy)
                   (mkApps (Prim (pcInfo pc))
                      [ Right (sink aTy), Right bTy', Right (sink cTy), Right n1, Left fT
                      , Left (sink xs)
                      , Left (mkApps (closedPrim (vecTailPrim vecTcNm))
                                [Right n1, Right bTy', Left ysT]) ])
                   n
                   [ mkApps fT
                       [ Left (sink x)
                       , Left (mkApps (closedPrim (vecHeadPrim vecTcNm))
                                 [Right n1, Right bTy', Left ysT]) ] ]
        | otherwise -> Nothing
  | otherwise = Nothing
 where
  m = pcMachine pc
  env = pcEnv pc

-- | @splitAt :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)@. Let-binds
-- the recursive call, and selects its fields with case-expressions whose
-- patterns bind fresh variables.
vecSplitAt :: forall o n. Distinct n => PrimContext o n -> Maybe (Next o n)
vecSplitAt pc
  | pcIsSubj pc
  , DC snatDc (Right mTy : _) : _ <- pcArgs pc
  , Just mN <- natSize pc mTy
  , _ : nTy : aTy : _ <- pcTys pc
  , TyConApp tupTcNm tyArgs@(tyArg : _) <- tyView (resultType pc)
  , Just tupTc <- lookupTyCon te tupTcNm
  , [tupDc] <- tyConDataCons tupTc
  , TyConApp vecTcNm _ <- tyView tyArg
  , Just vecTc <- lookupTyCon te vecTcNm
  , [nilCon, consCon] <- tyConDataCons vecTc
  = let vecTy :: Type x -> Type x -> Type x
        vecTy len el = mkTyConApp vecTcNm [len, el]
        m1Lit :: Type x
        m1Lit = numTy (mN - 1)
        splitAtRec v = mkApps (Prim (pcInfo pc))
          [ Right m1Lit, Right nTy, Right aTy
          , Left (mkApps (Data snatDc)
                    [Right m1Lit, Left (Literal (NaturalLiteral (mN - 1)))])
          , Left v ]
    in case mN of
         -- (Nil, v)
         0 -> reduce pc $ mkApps (Data tupDc) $ map Right tyArgs ++
                [ Left (mkVec te sc nilCon consCon aTy 0 [])
                , Left (valToTerm (last (pcArgs pc))) ]
         -- (x : fst (splitAt (m-1) xs), snd (splitAt (m-1) xs))
         _ | DC _ vArgs <- last (pcArgs pc) -> case Either.lefts vArgs of
               _ : x : xs : _ ->
                 newLetBinding (pcEnv pc) m (splitAtRec xs) $ \m1 recV ->
                   let sc1 = mScope m1
                       aTy1 = sink aTy
                       nTy1 = sink nTy
                       -- A field of the recursive call
                       sel first =
                         withFreshIdBinder sc1 lName (vecTy m1Lit aTy1) $ \sc2 lB ->
                           withFreshIdBinder sc2 rName (vecTy (sink nTy1) (sink aTy1)) $ \_ rB ->
                             Case (heapVarTerm recV)
                               (if first then vecTy m1Lit aTy1 else vecTy nTy1 aTy1)
                               [Alt (DataPat tupDc TNil (TCons lB (TCons rB TNil)))
                                    (if first then sink (idBinderVar lB) else idBinderVar rB)]
                   in reduceIn pc m1 $ mkApps (Data tupDc) $ map (Right . sink) tyArgs ++
                        [ Left (appendToVec te sc1 consCon aTy1 (sel True) mN [sink x])
                        , Left (sel False) ]
               _ ->
                 -- The vector reduces to Nil and not Cons, which only happens
                 -- when 'n' would be negative. See
                 -- https://github.com/clash-lang/clash-compiler/issues/2831
                 reduce pc (TyApp (Prim NP.undefined) (resultType pc))
           -- The vector doesn't reduce to a data constructor
           | otherwise -> Nothing
  | otherwise = Nothing
 where
  m = pcMachine pc
  sc = mScope m
  te = envTypeEnv (pcEnv pc)
  lName = C.mkUnsafeSystemName "l" 0
  rName = C.mkUnsafeSystemName "r" 1

-- * Booleans

-- | @(&&)@ and @(||)@, lazy in both arguments: evaluates both with a nested
-- 'whnf'. The nested machines live in extensions of the primitive's scope, so
-- the result continues in the last one, with the original stack sunk into it.
-- See @Clash.GHC.Evaluator.Primitives.GHC.Classes@.
andOr :: Distinct n => Bool -> PrimContext o n -> Maybe (Next o n)
andOr isAnd pc
  | [lArg, rArg] <- pcArgs pc
  = whnf env True m {mStack = [], mTerm = valToTerm lArg} $ \m1 ->
      whnf env True m1 {mStack = [], mTerm = sink (valToTerm rArg)} $ \m2 ->
        let lW = sink (mTerm m1)
            rW = mTerm m2
            m2' = m2 {mStack = sink1 (mStack m)}
            lit = boolToBoolLiteral (envTypeEnv env) (pcInfo pc)
            -- The constructor that decides the result on its own
            absorbing con = if isAnd then isFalseDC con else isTrueDC con
            combine l r = if isAnd then l && r else l || r
        in case (lW, rW) of
             (Data lCon, Data rCon) ->
               Just (Next m2' {mTerm = lit (combine (isTrueDC lCon) (isTrueDC rCon))})
             (Data lCon, _)
               | absorbing lCon -> reduceIn pc m2' (lit (not isAnd))
               | otherwise -> reduceIn pc m2' rW
             (_, Data rCon)
               | absorbing rCon -> reduceIn pc m2' (lit (not isAnd))
               | otherwise -> reduceIn pc m2' lW
             _ -> Nothing
  | otherwise = Nothing
 where
  m = pcMachine pc
  env = pcEnv pc

-- * Helpers

-- | The result type of the primitive, applied to its type arguments, after
-- its term arguments
resultType :: Distinct n => PrimContext o n -> Type n
resultType pc = go (piResultTys te sc (primInfoType sc (pcInfo pc)) (pcTys pc))
 where
  te = envTypeEnv (pcEnv pc)
  sc = mScope (pcMachine pc)
  go ty = maybe ty (go . snd) (splitFunTy te sc ty)

-- | The size of a type-level natural
natSize :: Distinct n => PrimContext o n -> Type n -> Maybe Integer
natSize pc ty =
  either (const Nothing) Just
    (runExcept (tyNatSize (envTypeEnv (pcEnv pc)) (mScope (pcMachine pc)) ty))

-- | A natural number as a type
numTy :: Integer -> Type n
numTy = LitTy . C.NumTy

-- | A primitive of Clash Core, which is closed
closedPrim :: HasCallStack => C.Term -> Term n
closedPrim = \case
  C.Prim p -> Prim p
  _ -> error ($(curLoc) ++ "not a primitive")

pairOf :: (Value n -> Maybe a) -> [Value n] -> Maybe (a, a)
pairOf f [a, b] = (,) <$> f a <*> f b
pairOf _ _ = Nothing

-- | See @Clash.GHC.Evaluator.Primitive.Util.intLiteral@
intLiteral :: Value n -> Maybe Integer
intLiteral = \case
  Lit (IntLiteral i) -> Just i
  _ -> Nothing

-- | An @Int#@ literal, with the overflow behaviour of 'Int'
integerToIntLiteral :: Integer -> Term n
integerToIntLiteral = Literal . IntLiteral . toInteger . (fromInteger :: Integer -> Int)

boolToIntLiteral :: Bool -> Term n
boolToIntLiteral b = Literal (IntLiteral (if b then 1 else 0))

-- | A constructor of 'Bool', the result type of the primitive
boolToBoolLiteral :: HasCallStack => TypeEnv -> C.PrimInfo -> Bool -> Term n
boolToBoolLiteral te p b =
  case C.splitFunForallTy (C.primType p) of
    (_, C.tyView -> C.TyConApp boolTcNm [])
      | Just boolTc <- lookupTyCon te boolTcNm
      , [falseDc, trueDc] <- tyConDataCons boolTc
      -> Data (if b then trueDc else falseDc)
    _ -> error ($(curLoc) ++ "the result type is not Bool: " ++ show (C.primName p))

isTrueDC, isFalseDC :: DataCon -> Bool
isTrueDC dc = dcUniq dc == fromGhcUnique trueDataConKey
isFalseDC dc = dcUniq dc == fromGhcUnique falseDataConKey
