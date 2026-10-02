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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.Evaluator.Primitives.GHC.Num.BigNat
  ( primitives
  ) where

import qualified Data.Primitive.ByteArray as BA
import           Data.Text           (Text)
import           GHC.Num.BigNat      (bigNatEq#)
import GHC.Num.Integer (Integer (..))

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.Core.TyCon (tyConDataCons)
import Clash.Core.Type (TypeView (..), tyView)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (textNameLit)

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Num.BigNat.bigNatEq#) $ \case
      PrimStepContext{..}
        | [ Lit (ByteArrayLiteral (BA.ByteArray i))
          , Lit (ByteArrayLiteral (BA.ByteArray j))] <- args
        -> reduce (Literal (IntLiteral (IS (bigNatEq# i j))))
      _ -> Nothing

  -- These constants are NOINLINE CAFs in GHC 10.0. Evaluating their allocation
  -- code would require mutable byte arrays and state tokens.
  , primStepEntry "GHC.Internal.Bignum.BigNat.bigNatZero"
      (bigNatConstant (BA.byteArrayFromList ([] :: [Word])))
  , primStepEntry "GHC.Internal.Bignum.BigNat.bigNatOne"
      (bigNatConstant (BA.byteArrayFromList [1 :: Word]))
  ]

bigNatConstant :: BA.ByteArray -> PrimStepContext -> Maybe Machine
bigNatConstant value PrimStepContext{..}
  | [] <- args
  , TyConApp tcName [] <- tyView ty
  , Just tc <- UniqMap.lookup tcName tcm
  , [dc] <- tyConDataCons tc
  = reduce (App (Data dc) (Literal (ByteArrayLiteral value)))
  | otherwise = Nothing
