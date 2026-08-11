{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitives.GHC.Int
  ( primitives
  ) where

import           Data.Text           (Text)
import           GHC.Int

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (textNameLit)

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Int.I8#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Int8Literal i)] <- args
        ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                (Just intTc) = UniqMap.lookup intTcNm tcm
                [intDc] = tyConDataCons intTc
            in  reduce (mkApps (Data intDc) [Left (Literal (Int8Literal i))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Int.I16#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Int16Literal i)] <- args
        ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                (Just intTc) = UniqMap.lookup intTcNm tcm
                [intDc] = tyConDataCons intTc
            in  reduce (mkApps (Data intDc) [Left (Literal (Int16Literal i))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Int.I32#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Int32Literal i)] <- args
        ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                (Just intTc) = UniqMap.lookup intTcNm tcm
                [intDc] = tyConDataCons intTc
            in  reduce (mkApps (Data intDc) [Left (Literal (Int32Literal i))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Int.I64#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Int64Literal i)] <- args
        ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                (Just intTc) = UniqMap.lookup intTcNm tcm
                [intDc] = tyConDataCons intTc
            in  reduce (mkApps (Data intDc) [Left (Literal (Int64Literal i))])
      _ -> Nothing

  ]
