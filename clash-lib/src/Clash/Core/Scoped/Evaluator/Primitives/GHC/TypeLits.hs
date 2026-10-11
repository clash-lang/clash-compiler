{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.TypeLits" in the scoped evaluator:
  ports of @Clash.GHC.Evaluator.Primitives.GHC.TypeLits@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.GHC.TypeLits
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Core.Literal (Literal (..))
import Clash.Util (textNameLit)

import qualified GHC.TypeLits

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types

primitives :: [(Text, PrimReduction)]
primitives =
  -- XXX: Does it make sense to match on a @NaturalLiteral@ here?
  [ primStepEntry $(textNameLit 'GHC.TypeLits.natVal) $ \case
      PrimStepContext{..}
        | [Lit (NaturalLiteral n), _] <- args
        -> reduce (integerToIntegerLiteral n)
      _ -> Nothing


  -- XXX: Does it make sense to match on a @NaturalLiteral@ here?
  , primStepEntry $(textNameLit 'GHC.TypeLits.someNatVal) $ \case
      PrimStepContext{..}
        | [Lit (NaturalLiteral n)] <- args
        -> let resTy = getResultTy te sc ty tys
            in reduce (mkSomeNat te n resTy)
      _ -> Nothing

  ]
