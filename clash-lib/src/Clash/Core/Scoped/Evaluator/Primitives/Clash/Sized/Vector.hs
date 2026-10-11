{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Clash.Sized.Vector" in the scoped
  evaluator: ports of @Clash.GHC.Evaluator.Primitives.Clash.Sized.Vector@.
  See "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector
  ( primitives
  ) where

import Control.Monad.Trans.Except (runExcept)
import qualified Data.Either as Either
import Data.Text (Text)

import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as CT
import qualified Clash.Core.Type as C
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Normalize.Primitives as NP
import Clash.Normalize.PrimitiveReductions (vecHeadPrim, vecTailPrim)
import Clash.Util (textNameLit)

import qualified Clash.Sized.Vector

import Clash.Core.Scoped.Evaluator (newLetBinding)
import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Subst (withFreshIdBinder)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (mkApps)
import Clash.Core.Scoped.Type (TypeView (..), mkTyConApp, tyView)
import Clash.Core.Scoped.TypeEnv (lookupTyCon)

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'Clash.Sized.Vector.length) $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> Int
        | isSubj
        , [nTy, _] <- tys
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> reduce (mkIntCLit te IntLiteral n ty)
      _ -> Nothing

  -- A reduction that continues with the WHNF of a field
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.head) $ \case
      PrimStepContext{..} -- :: Vec (n+1) a -> a
        | isSubj
        , [DC _ vArgs] <- args
        -> reduceWHNF (Either.lefts vArgs !! 1)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.tail) $ \case
      PrimStepContext{..} -- :: Vec (n+1) a -> Vec n a
        | isSubj
        , [DC _ vArgs] <- args
        -> reduceWHNF (Either.lefts vArgs !! 2)
      _ -> Nothing

  -- A reduction that embeds an argument in its result. Its arguments are in
  -- the scope of the machine, so they can be embedded as they are.
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.map) $ \case
      PrimStepContext{..} -- :: (a -> b) -> Vec n a -> Vec n b
        | isSubj
        , DC dc vArgs <- args !! 1
        , aTy : bTy : nTy : _ <- tys
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0  -> reduce (mkVecNil te sc dc bTy)
             n' -> reduce $
                   mkVecCons te sc dc bTy n'
                     (mkApps (valToTerm (args !! 0)) [Left (Either.lefts vArgs !! 1)])
                     (mkApps (Prim pInfo)
                                          [Right aTy
                                          ,Right bTy
                                          ,Right (LitTy (C.NumTy (n' - 1)))
                                          ,Left (valToTerm (args !! 0))
                                          ,Left (Either.lefts vArgs !! 2)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.replicate) $ \case
      PrimStepContext{..} -- :: SNat n -> a -> Vec n a
        | isSubj
        , TyConApp vecTcNm [lenTy,argTy] <- tyView (getResultTy te sc ty tys)
        , Right len <- runExcept (tyNatSize te sc lenTy)
        , Just vecTc <- lookupTyCon te vecTcNm
        , [nilCon,consCon] <- tyConDataCons vecTc
        -> reduce $
           mkVec te sc nilCon consCon argTy len
                 (replicate (fromInteger len) (valToTerm (last args)))
      _ -> Nothing

  -- A reduction that allocates: @zipWith@ shares the function and the second
  -- vector by let-binding them on the heap (see #3308), so its result lives
  -- in an extension of the scope of the primitive, into which everything from
  -- before has to be sunk.
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.zipWith) $ \case
      PrimStepContext{..} -- :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
        | isSubj
        , aTy : bTy : cTy : nTy : _ <- tys
        , [f, DC dc vArgs, ys] <- args
        , (_, C.tyView -> C.TyConApp vecTcNm _) <- C.splitFunForallTy (CT.primType pInfo)
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0 -> reduce (mkVecNil te sc dc cTy)
             _ | _ : x : xs : _ <- Either.lefts vArgs ->
                 newLetBinding evalEnv mach (valToTerm f) $ \m1 fV ->
                   newLetBinding evalEnv m1 (sink (valToTerm ys)) $ \m2 ysV ->
                     let n1 = numTy (n - 1)
                         fT = sink (heapVarTerm fV)
                         ysT = heapVarTerm ysV
                         bTy' = sink bTy
                     in reduceWith m2 $ mkVecCons te (mScope m2) dc (sink cTy) n
                          (mkApps fT
                             [ Left (sink x)
                             , Left (mkApps (closedPrim (vecHeadPrim vecTcNm))
                                       [Right n1, Right bTy', Left ysT]) ])
                          (mkApps (Prim pInfo)
                             [ Right (sink aTy), Right bTy', Right (sink cTy), Right n1, Left fT
                             , Left (sink xs)
                             , Left (mkApps (closedPrim (vecTailPrim vecTcNm))
                                       [Right n1, Right bTy', Left ysT]) ])
               | otherwise -> Nothing
      _ -> Nothing

  -- A reduction that let-binds a recursive call, and selects its fields with
  -- case-expressions whose patterns bind fresh variables
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.splitAt) $ \case
      PrimStepContext{..} -- :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
        | isSubj
        , DC snatDc (Right mTy : _) : _ <- args
        , Right mN <- runExcept (tyNatSize te sc mTy)
        , _ : nTy : aTy : _ <- tys
        , TyConApp tupTcNm tyArgs@(tyArg : _) <- tyView (getResultTy te sc ty tys)
        , Just tupTc <- lookupTyCon te tupTcNm
        , [tupDc] <- tyConDataCons tupTc
        , TyConApp vecTcNm _ <- tyView tyArg
        , Just vecTc <- lookupTyCon te vecTcNm
        , [nilCon, consCon] <- tyConDataCons vecTc
        -> let vecTy :: Type x -> Type x -> Type x
               vecTy len el = mkTyConApp vecTcNm [len, el]
               m1Lit :: Type x
               m1Lit = numTy (mN - 1)
               splitAtRec v = mkApps (Prim pInfo)
                 [ Right m1Lit, Right nTy, Right aTy
                 , Left (mkApps (Data snatDc)
                           [Right m1Lit, Left (Literal (NaturalLiteral (mN - 1)))])
                 , Left v ]
           in case mN of
                -- (Nil, v)
                0 -> reduce $ mkApps (Data tupDc) $ map Right tyArgs ++
                       [ Left (mkVecNil te sc nilCon aTy)
                       , Left (valToTerm (last args)) ]
                -- (x : fst (splitAt (m-1) xs), snd (splitAt (m-1) xs))
                _ | DC _ vArgs <- last args -> case Either.lefts vArgs of
                      _ : x : xs : _ ->
                        newLetBinding evalEnv mach (splitAtRec xs) $ \m1 recV ->
                          let sc1 = mScope m1
                              aTy1 = sink aTy
                              nTy1 = sink nTy
                              -- A field of the recursive call
                              sel first =
                                withFreshIdBinder sc1 (C.mkUnsafeSystemName "l" 0) (vecTy m1Lit aTy1) $ \sc2 lB ->
                                  withFreshIdBinder sc2 (C.mkUnsafeSystemName "r" 1) (vecTy (sink nTy1) (sink aTy1)) $ \_ rB ->
                                    Case (heapVarTerm recV)
                                      (if first then vecTy m1Lit aTy1 else vecTy nTy1 aTy1)
                                      [Alt (DataPat tupDc TNil (TCons lB (TCons rB TNil)))
                                           (if first then sink (idBinderVar lB) else idBinderVar rB)]
                          in reduceWith m1 $ mkApps (Data tupDc) $ map (Right . sink) tyArgs ++
                               [ Left (mkVecCons te sc1 consCon aTy1 mN (sink x) (sel True))
                               , Left (sel False) ]
                      _ ->
                        -- The vector reduces to Nil and not Cons, which only
                        -- happens when 'n' would be negative. See
                        -- https://github.com/clash-lang/clash-compiler/issues/2831
                        reduce (TyApp (Prim NP.undefined) (getResultTy te sc ty tys))
                  -- The vector doesn't reduce to a data constructor
                  | otherwise -> Nothing
      _ -> Nothing
  ]
