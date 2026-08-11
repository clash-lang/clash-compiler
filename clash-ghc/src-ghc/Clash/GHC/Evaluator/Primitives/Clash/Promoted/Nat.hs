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

module Clash.GHC.Evaluator.Primitives.Clash.Promoted.Nat
  ( primitives
  ) where

import           Control.Monad.Trans.Except (runExcept)
import           Data.Bits
import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (Type (..), LitTy (..), TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TyCon (tyConDataCons)
import Clash.Core.Util (tyNatSize)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (clogBase, flogBase, textNameLit)

import qualified Clash.Promoted.Nat

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ primStepEntry $(textNameLit 'Clash.Promoted.Nat.powSNat) $ \case
      PrimStepContext{..}
        | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
        -> let c = case a of
                     2 -> 1 `shiftL` (fromInteger b)
                     _ -> a ^ b
               (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
               (Just snatTc) = UniqMap.lookup snatTcNm tcm
               [snatDc] = tyConDataCons snatTc
           in  reduce $
               mkApps (Data snatDc) [ Right (LitTy (NumTy c))
                                    , Left (Literal (NaturalLiteral c))]
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Promoted.Nat.flogBaseSNat) $ \case
      PrimStepContext{..}
        | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
        , Just c <- flogBase a b
        , let c' = toInteger c
        -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
               (Just snatTc) = UniqMap.lookup snatTcNm tcm
               [snatDc] = tyConDataCons snatTc
           in  reduce $
               mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                    , Left (Literal (NaturalLiteral c'))]
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Promoted.Nat.clogBaseSNat) $ \case
      PrimStepContext{..}
        | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
        , Just c <- clogBase a b
        , let c' = toInteger c
        -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
               (Just snatTc) = UniqMap.lookup snatTcNm tcm
               [snatDc] = tyConDataCons snatTc
           in  reduce $
               mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                    , Left (Literal (NaturalLiteral c'))]
        | otherwise
        -> error ("clogBaseSNat: args = " <> show args <> ", tys = " <> show tys)

  , primStepEntry $(textNameLit 'Clash.Promoted.Nat.logBaseSNat) $ \case
      PrimStepContext{..}
        | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
        , Just c <- flogBase a b
        , let c' = toInteger c
        -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
               (Just snatTc) = UniqMap.lookup snatTcNm tcm
               [snatDc] = tyConDataCons snatTc
           in  reduce $
               mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                    , Left (Literal (NaturalLiteral c'))]
      _ -> Nothing

  ]
