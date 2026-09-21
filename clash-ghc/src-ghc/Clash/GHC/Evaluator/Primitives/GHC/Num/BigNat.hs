{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.GHC.Evaluator.Primitives.GHC.Num.BigNat
  ( primitives,
  )
where

import Clash.Core.Evaluator.Types
import Clash.Core.Literal (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.GHC.Evaluator.Primitive.Util
import Clash.Util (textNameLit)
import qualified Data.Primitive.ByteArray as BA
import Data.Text (Text)
import GHC.Num.BigNat (bigNatEq#)
import GHC.Num.Integer (Integer (..))

primitives :: [(Text, PrimStep)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Num.BigNat.bigNatEq#) $ \case
      PrimStepContext {..}
        | [ Lit (ByteArrayLiteral (BA.ByteArray i)),
            Lit (ByteArrayLiteral (BA.ByteArray j))
            ] <-
            args ->
            reduce (Literal (IntLiteral (IS (bigNatEq# i j))))
      _ -> Nothing
  ]
