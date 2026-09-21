{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.GHC.Evaluator.Primitives.GHC.TypeLits
  ( primitives,
  )
where

import Clash.Core.Evaluator.Types
import Clash.Core.Literal (Literal (..))
import Clash.GHC.Evaluator.Primitive.Util
import Clash.Util (textNameLit)
import Data.Text (Text)
import qualified GHC.TypeLits

primitives :: [(Text, PrimStep)]
primitives =
  -- XXX: Does it make sense to match on a @NaturalLiteral@ here?
  [ primStepEntry $(textNameLit 'GHC.TypeLits.natVal) $ \case
      PrimStepContext {..}
        | [Lit (NaturalLiteral n), _] <- args ->
            reduce (integerToIntegerLiteral n)
      _ -> Nothing,

    -- XXX: Does it make sense to match on a @NaturalLiteral@ here?
    primStepEntry $(textNameLit 'GHC.TypeLits.someNatVal) $ \case
      PrimStepContext {..}
        | [Lit (NaturalLiteral n)] <- args ->
            let resTy = getResultTy tcm ty tys
             in reduce (mkSomeNat tcm n resTy)
      _ -> Nothing
  ]
