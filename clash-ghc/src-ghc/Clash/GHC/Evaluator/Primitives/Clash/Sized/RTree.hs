{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitives.Clash.Sized.RTree
  ( primitives
  ) where

import           Control.Monad.Trans.Except (runExcept)
import qualified Data.Either         as Either
import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import Clash.Core.HasType (piResultTys)
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type
  (Type (..),
   LitTy (..),
   TypeView (..),
   mkTyConApp,
   splitFunForallTy,
   tyView)
import Clash.Core.TyCon (tyConDataCons)
import Clash.Core.Util (mkRTree, tyNatSize)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (textNameLit)

import qualified Clash.Sized.RTree

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
--------
-- RTree
--------
  [ primStepEntry $(textNameLit 'Clash.Sized.RTree.textract) $ \case
      PrimStepContext{..}
        | isSubj
        , [DC _ tArgs] <- args
        -> reduceWHNF (Either.lefts tArgs !! 1)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.RTree.tsplit) $ \case
      PrimStepContext{..}
        | isSubj
        , dTy : aTy : _ <- tys
        , [DC _ tArgs] <- args
        , (tyArgs,tyView -> TyConApp tupTcNm _) <- splitFunForallTy ty
        , TyConApp treeTcNm _ <- tyView (Either.rights tyArgs !! 0)
        -> let (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc]      = tyConDataCons tupTc
           in  reduce $
               mkApps (Data tupDc)
                      [Right (mkTyConApp treeTcNm [dTy,aTy])
                      ,Right (mkTyConApp treeTcNm [dTy,aTy])
                      ,Left (Either.lefts tArgs !! 1)
                      ,Left (Either.lefts tArgs !! 2)
                      ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.RTree.tdfold) $ \case
      PrimStepContext{..}
        | isSubj
        , pTy : kTy : aTy : _ <- tys
        , _ : p : f : g : ts : _ <- args
        , DC _ tArgs <- ts
        , Right k' <- runExcept (tyNatSize tcm kTy)
        -> case k' of
             0 -> reduceWHNF (mkApps (valToTerm f) [Left (Either.lefts tArgs !! 1)])
             _ -> let k'ty = LitTy (NumTy (k'-1))
                      (tyArgs,_)  = splitFunForallTy ty
                      (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 3)
                      TyConApp snatTcNm _ = tyView (Either.rights tyArgs' !! 0)
                      Just snatTc = UniqMap.lookup snatTcNm tcm
                      [snatDc]    = tyConDataCons snatTc
                  in  reduceWHNF $
                      mkApps (valToTerm g)
                             [Right k'ty
                             ,Left (mkApps (Data snatDc)
                                           [Right k'ty
                                           ,Left (Literal (NaturalLiteral (k'-1)))])
                             ,Left (mkApps (Prim pInfo)
                                           [Right pTy
                                           ,Right k'ty
                                           ,Right aTy
                                           ,Left (Literal (NaturalLiteral (k'-1)))
                                           ,Left (valToTerm p)
                                           ,Left (valToTerm f)
                                           ,Left (valToTerm g)
                                           ,Left (Either.lefts tArgs !! 1)
                                           ])
                             ,Left (mkApps (Prim pInfo)
                                           [Right pTy
                                           ,Right k'ty
                                           ,Right aTy
                                           ,Left (Literal (NaturalLiteral (k'-1)))
                                           ,Left (valToTerm p)
                                           ,Left (valToTerm f)
                                           ,Left (valToTerm g)
                                           ,Left (Either.lefts tArgs !! 2)
                                           ])
                             ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.RTree.treplicate) $ \case
      PrimStepContext{..}
        | isSubj
        , let ty' = piResultTys tcm ty tys
        , (_,tyView -> TyConApp treeTcNm [lenTy,argTy]) <- splitFunForallTy ty'
        , Right len <- runExcept (tyNatSize tcm lenTy)
        -> let (Just treeTc) = UniqMap.lookup treeTcNm tcm
               [lrCon,brCon] = tyConDataCons treeTc
           in  reduce (mkRTree lrCon brCon argTy len (replicate (2^len) (valToTerm (last args))))
      _ -> Nothing

  ]
