{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Internal.Float" in the scoped
  evaluator: ports of @Clash.GHC.Evaluator.Primitives.GHC.Internal.Float@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.GHC.Internal.Float
  ( primitives
  ) where

import Data.Bifunctor (first)
import Data.Proxy (Proxy)
import Data.Reflection (reifyNat)
import Data.Text (Text)
import Data.Text.Extra (showt)
import GHC.Float
import GHC.TypeLits (KnownNat)

import Clash.Core.Basic (LitTy (..))
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (Name (..))

import Clash.Sized.Internal.Signed (Signed (..))

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (mkApps)
import Clash.Core.Scoped.Type (TypeView (..), tyView)

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry "GHC.Internal.Float.$wproperFractionDouble" $ \case
      PrimStepContext{..}
        | _ : Lit (DoubleLiteral d) : _ <- args
        , [sty@(tyView -> TyConApp signedTcNm [nTy@(LitTy (NumTy kn))])] <- tys
        , nameOcc signedTcNm == showt ''Clash.Sized.Internal.Signed.Signed
        -- The type arguments of the tuple are those of the result type of the
        -- instantiated type of the primitive. The reduction of Clash Core takes
        -- them from the type of the primitive itself, which leaves @b@ free in
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
