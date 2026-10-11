{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Clash.Sized.Vector" in the scoped evaluator:
  ports of @Clash.GHC.Evaluator.Primitives.Clash.Sized.Vector@. See
  "Clash.Core.Scoped.Evaluator.Primitives".
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector
  ( primitives
  ) where

import Control.Monad.Trans.Except (runExcept)
import qualified Data.Either as Either
import Data.Text (Text)
import Data.Text.Extra (showt)

import qualified Clash.Sized.Vector

import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Core.Type as C
import qualified Clash.Normalize.Primitives as NP
import Clash.Normalize.PrimitiveReductions (vecHeadPrim, vecTailPrim)

import Clash.Core.Scoped.Evaluator
import Clash.Core.Scoped.Evaluator.Primitives.Util (closedPrim, numTy)
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.HasType (piResultTys, primInfoType)
import Clash.Core.Scoped.Subst (withFreshIdBinder)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (mkApps)
import Clash.Core.Scoped.Type
  (TypeView (..), mkTyConApp, splitFunTy, tyNatSize, tyView)
import Clash.Core.Scoped.TypeEnv (lookupTyCon)
import Clash.Core.Scoped.Util (appendToVec, mkVec)

primitives :: [(Text, PrimReduction)]
primitives =
  [ (showt 'Clash.Sized.Vector.map, PrimReduction vecMap)
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
  ]

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

