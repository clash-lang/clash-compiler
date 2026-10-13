{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Float" in the scoped evaluator. See
  "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.Core.Evaluator.Primitives.GHC.Float
  ( primitives
  ) where

import Data.Bifunctor (first)
import Data.Proxy (Proxy)
import Data.Reflection (reifyNat)
import Data.Text (Text)
import Data.Text.Extra (showt)
import GHC.Float
import GHC.Real (Ratio (..))
import GHC.TypeLits (KnownNat)

import Clash.Core.Basic (LitTy (..))
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (Name (..))
import Clash.Util (curLoc, textNameLit)
import Clash.Sized.Internal.Signed (Signed (..))

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax
import Clash.Core.Term (mkApps)
import Clash.Core.Type (TypeView (..), tyView)

primitives :: [(Text, PrimReduction)]
primitives =
  -- GHC.Float.asinh  -- XXX: Very fragile
  --  $w$casinh is the Double specialisation of asinh
  --  $w$casinh1 is the Float specialisation of asinh
  [ primStepEntry "GHC.Float.$w$casinh" $ \case
      PrimStepContext{..} | Just r <- liftDD go args
        -> reduce r
        where go f = case asinh (D# f) of
                       D# f' -> f'
      _ -> Nothing

  , primStepEntry "GHC.Float.$w$casinh1" $ \case
      PrimStepContext{..} | Just r <- liftFF go args
        -> reduce r
        where go f = case asinh (F# f) of
                       F# f' -> f'
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Float.integerToFloat#) $ \case
      PrimStepContext{..}
        | [v] <- args
        , Just i <- integerLiteral v
        -> reduce . Literal . FloatLiteral . castFloatToWord32 $ F# (integerToFloat# i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Float.integerToDouble#) $ \case
      PrimStepContext{..}
        | [v] <- args
        , Just i <- integerLiteral v
        -> reduce . Literal . DoubleLiteral . castDoubleToWord64 $ D# (integerToDouble# i)
      _ -> Nothing

  , primStepEntry "GHC.Float.$w$sfromRat''" $ \case
      PrimStepContext{..} -- XXX: Very fragile
        | [Lit (IntLiteral _minEx)
          ,Lit (IntLiteral matDigs)
          ,nV
          ,dV] <- args
        , [n,d] <- integerLiterals' [nV,dV]
        -> case fromInteger matDigs of
              matDigs'
                | matDigs' == floatDigits (undefined :: Float)
                -> reduce (Literal (FloatLiteral (castFloatToWord32 (fromRational (n :% d)))))
                | matDigs' == floatDigits (undefined :: Double)
                -> reduce (Literal (DoubleLiteral (castDoubleToWord64 (fromRational (n :% d)))))
              _ -> error $ $(curLoc) ++ "GHC.Float.$w$sfromRat'': Not a Float or Double"
      _ -> Nothing

  , primStepEntry "GHC.Float.$w$sfromRat''1" $ \case
      PrimStepContext{..} -- XXX: Very fragile
        | [Lit (IntLiteral _minEx)
          ,Lit (IntLiteral matDigs)
          ,nV
          ,dV] <- args
        , [n,d] <- integerLiterals' [nV,dV]
        -> case fromInteger matDigs of
              matDigs'
                | matDigs' == floatDigits (undefined :: Float)
                -> reduce (Literal (FloatLiteral (castFloatToWord32 (fromRational (n :% d)))))
                | matDigs' == floatDigits (undefined :: Double)
                -> reduce (Literal (DoubleLiteral (castDoubleToWord64 (fromRational (n :% d)))))
              _ -> error $ $(curLoc) ++ "GHC.Float.$w$sfromRat'': Not a Float or Double"
      _ -> Nothing

  , primStepEntry "GHC.Float.$wproperFractionDouble" $ \case
      PrimStepContext{..}
        | _ : Lit (DoubleLiteral d) : _ <- args
        , [sty@(tyView -> TyConApp signedTcNm [nTy@(LitTy (NumTy kn))])] <- tys
        , nameOcc signedTcNm == showt ''Signed
        -- The type arguments of the tuple are those of the result type of the
        -- instantiated type of the primitive. Taking them from the type of the
        -- primitive itself would leave @b@ free in
        -- @forall b. Integral b => Double# -> (# b, Double #)@.
        , (tupDc, tyArgs@(_:_)) <- resultDataCon te sc ty tys
        -> let (sn, d1) = reifyNat kn (\p -> first toInteger (op p (castWord64ToDouble d)))
               ret = mkApps (Data tupDc) (map Right tyArgs ++
                      [ Left (mkSignedLit sty nTy kn sn)
                      , Left (mkDoubleCLit te (castDoubleToWord64 d1) (last tyArgs))
                      ])
            in reduce ret
        where
          op :: KnownNat n => Proxy n -> Double -> (Signed n, Double)
          op _ = properFraction
      _ -> Nothing

  ]
