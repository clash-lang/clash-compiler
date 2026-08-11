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

module Clash.GHC.Evaluator.Primitives.GHC.Word
  ( primitives
  ) where

import           Data.Text           (Text)
import           GHC.Word

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
  [ primStepEntry $(textNameLit 'GHC.Word.W8#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Word8Literal c)] <- args
        ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                (Just wordTc) = UniqMap.lookup wordTcNm tcm
                [wordDc] = tyConDataCons wordTc
            in  reduce (mkApps (Data wordDc) [Left (Literal (Word8Literal c))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Word.W16#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Word16Literal c)] <- args
        ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                (Just wordTc) = UniqMap.lookup wordTcNm tcm
                [wordDc] = tyConDataCons wordTc
            in  reduce (mkApps (Data wordDc) [Left (Literal (Word16Literal c))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Word.W32#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Word32Literal c)] <- args
        ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                (Just wordTc) = UniqMap.lookup wordTcNm tcm
                [wordDc] = tyConDataCons wordTc
            in  reduce (mkApps (Data wordDc) [Left (Literal (Word32Literal c))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Word.W64#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Word64Literal c)] <- args
        ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                (Just wordTc) = UniqMap.lookup wordTcNm tcm
                [wordDc] = tyConDataCons wordTc
            in  reduce (mkApps (Data wordDc) [Left (Literal (Word64Literal c))])
      _ -> Nothing

  ]
