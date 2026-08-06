{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitives.GHC.Internal.Float
  ( primitives
  ) where

import           Data.Proxy          (Proxy)
import           Data.Reflection     (reifyNat)
import           Data.Text           (Text)
import           Data.Text.Extra     (showt)
import           GHC.Float
import           GHC.TypeLits        (KnownNat)
import           Data.Bifunctor      (first)

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Name (Name (..))
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (Type (..), LitTy (..), TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Sized.Internal.Signed   (Signed   (..))

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( "GHC.Internal.Float.$wproperFractionDouble"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | _ : Lit (DoubleLiteral d) : _ <- args
            , [sty@(tyView -> TyConApp signedTcNm [nTy@(LitTy (NumTy kn))])] <- tys
            , nameOcc signedTcNm == showt ''Clash.Sized.Internal.Signed.Signed
            , (_, tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
            , Just tupTc <- UniqMap.lookup tupTcNm tcm
            , [tupDc] <- tyConDataCons tupTc
            -> let (sn, d1) = reifyNat kn (\p -> first toInteger (op p (castWord64ToDouble d)))
                   ret = mkApps (Data tupDc) (map Right tyArgs ++
                          [ Left (mkSignedLit sty nTy kn sn)
                          , Left (mkDoubleCLit tcm (castDoubleToWord64 d1) (last tyArgs))
                          ])
                in reduce ret
            where
              op :: KnownNat n => Proxy n -> Double -> (Signed n, Double)
              op _ = properFraction
          _ -> Nothing
    )
  ]
