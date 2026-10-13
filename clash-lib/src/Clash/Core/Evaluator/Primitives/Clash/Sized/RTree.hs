{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Clash.Sized.RTree" in the evaluator.
  See "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Evaluator.Primitives.Clash.Sized.RTree
  ( primitives
  ) where

import Control.Monad.Trans.Except (runExcept)
import qualified Data.Either as Either
import Data.Text (Text)

import Clash.Core.Literal (Literal (..))
import Clash.Core.TyCon (tyConDataCons)
import Clash.Util (textNameLit)

import qualified Clash.Sized.RTree

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax
import Clash.Core.Term (mkApps)
import Clash.Core.Type (TypeView (..), mkTyConApp, tyView)
import Clash.Core.TypeEnv (lookupTyCon)

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'Clash.Sized.RTree.textract) $ \case
      PrimStepContext{..} -- :: RTree 0 a -> a
        | isSubj
        , [DC _ tArgs] <- args
        -> reduceWHNF (Either.lefts tArgs !! 1)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.RTree.tsplit) $ \case
      PrimStepContext{..} -- :: RTree (d+1) a -> (RTree d a, RTree d a)
        | isSubj
        , dTy : aTy : _ <- tys
        , [DC _ tArgs] <- args
        , (treeTy : _, resTy) <- primArgResTys pInfo
        , Just tupTcNm <- tyConNameOf resTy
        , Just treeTcNm <- tyConNameOf treeTy
        , Just tupTc <- lookupTyCon te tupTcNm
        , [tupDc] <- tyConDataCons tupTc
        -> reduce $
           mkApps (Data tupDc)
             [ Right (mkTyConApp treeTcNm [dTy, aTy])
             , Right (mkTyConApp treeTcNm [dTy, aTy])
             , Left (Either.lefts tArgs !! 1)
             , Left (Either.lefts tArgs !! 2) ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.RTree.tdfold) $ \case
      PrimStepContext{..}
        -- :: KnownNat k => Proxy p -> (a -> (p @@ 0))
        --    -> (forall l . SNat l -> (p @@ l) -> (p @@ l) -> (p @@ (l + 1)))
        --    -> RTree k a -> (p @@ k)
        | isSubj
        , pTy : kTy : aTy : _ <- tys
        , _ : p : f : g : ts : _ <- args
        , DC _ tArgs <- ts
        , Right k' <- runExcept (tyNatSize te sc kTy)
        -> case k' of
             0 -> reduceWHNF (mkApps (valToTerm f) [Left (Either.lefts tArgs !! 1)])
             _ | (_ : _ : _ : gTy : _, _) <- primArgResTys pInfo
               , snatTy : _ <- funArgTys gTy
               , Just snatTcNm <- tyConNameOf snatTy
               , Just snatTc <- lookupTyCon te snatTcNm
               , [snatDc] <- tyConDataCons snatTc
               -> let k1 = numTy (k' - 1)
                      -- @tdfold@ of a subtree
                      tdfoldSub t =
                        mkApps (Prim pInfo)
                          [ Right pTy
                          , Right k1
                          , Right aTy
                          , Left (Literal (NaturalLiteral (k' - 1)))
                          , Left (valToTerm p)
                          , Left (valToTerm f)
                          , Left (valToTerm g)
                          , Left t ]
                  in reduceWHNF $
                     mkApps (valToTerm g)
                       [ Right k1
                       , Left (mkApps (Data snatDc)
                                 [Right k1, Left (Literal (NaturalLiteral (k' - 1)))])
                       , Left (tdfoldSub (Either.lefts tArgs !! 1))
                       , Left (tdfoldSub (Either.lefts tArgs !! 2)) ]
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.RTree.treplicate) $ \case
      PrimStepContext{..} -- :: SNat d -> a -> RTree d a
        | isSubj
        , TyConApp treeTcNm [lenTy, argTy] <- tyView (getResultTy te sc ty tys)
        , Right len <- runExcept (tyNatSize te sc lenTy)
        , Just treeTc <- lookupTyCon te treeTcNm
        , [lrCon, brCon] <- tyConDataCons treeTc
        -> reduce (mkRTree te sc lrCon brCon argTy len (replicate (2 ^ len) (valToTerm (last args))))
      _ -> Nothing
  ]
