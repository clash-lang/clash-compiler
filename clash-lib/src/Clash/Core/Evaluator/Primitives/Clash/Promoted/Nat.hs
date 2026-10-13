{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Clash.Promoted.Nat" in the
  evaluator. See "Clash.Core.Evaluator.Primitives" for how to write
  them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Core.Evaluator.Primitives.Clash.Promoted.Nat
  ( primitives
  ) where

import Control.Monad.Trans.Except (runExcept)
import Data.Bits
import Data.Text (Text)

import Clash.Core.Basic (LitTy (..))
import Clash.Core.Literal (Literal (..))
import Clash.Core.TyCon (tyConDataCons)
import Clash.Util (clogBase, curLoc, flogBase, textNameLit)

import qualified Clash.Promoted.Nat

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax
import Clash.Core.Term (mkApps)
import Clash.Core.Type (SplitFunForall (..), TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TypeEnv (TypeEnv, lookupTyCon)

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'Clash.Promoted.Nat.powSNat) $ \case
      PrimStepContext{..}
        | [Right a, Right b] <- map (runExcept . tyNatSize te sc) tys
        -> let c = case a of
                     2 -> 1 `shiftL` (fromInteger b)
                     _ -> a ^ b
           in  reduce (mkSNatLit te ty c)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Promoted.Nat.flogBaseSNat) $ \case
      PrimStepContext{..}
        | [Right a, Right b] <- map (runExcept . tyNatSize te sc) tys
        , Just c <- flogBase a b
        , let c' = toInteger c
        -> reduce (mkSNatLit te ty c')
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Promoted.Nat.clogBaseSNat) $ \case
      PrimStepContext{..}
        | [Right a, Right b] <- map (runExcept . tyNatSize te sc) tys
        , Just c <- clogBase a b
        , let c' = toInteger c
        -> reduce (mkSNatLit te ty c')
        | otherwise
        -> error ("clogBaseSNat: args = " <> show args <> ", tys = " <> show tys)

  , primStepEntry $(textNameLit 'Clash.Promoted.Nat.logBaseSNat) $ \case
      PrimStepContext{..}
        | [Right a, Right b] <- map (runExcept . tyNatSize te sc) tys
        , Just c <- flogBase a b
        , let c' = toInteger c
        -> reduce (mkSNatLit te ty c')
      _ -> Nothing

  ]

-- | An @SNat@ of the given number, the result type of the given type of a
-- primitive
mkSNatLit :: TypeEnv -> Type m -> Integer -> Term n
mkSNatLit te ty c = case splitFunForallTy ty of
  SplitFunForall _ (tyView -> TyConApp snatTcNm _)
    | Just snatTc <- lookupTyCon te snatTcNm
    , [snatDc] <- tyConDataCons snatTc
    -> mkApps (Data snatDc) [ Right (LitTy (NumTy c))
                            , Left (Literal (NaturalLiteral c))]
  _ -> error ($(curLoc) ++ "the result type is not SNat")
