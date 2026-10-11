{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Clash.Sized.Internal.Unsigned" in the
  scoped evaluator: ports of
  @Clash.GHC.Evaluator.Primitives.Clash.Sized.Internal.Unsigned@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.Unsigned
  ( primitives
  ) where

import Data.Reflection (reifyNat)
import Data.Text (Text)

import qualified Clash.Sized.Internal.Unsigned as Unsigned
import Clash.Util (textNameLit)

import qualified Clash.Sized.Internal.Unsigned

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types

primitives :: [(Text, PrimReduction)]
primitives =
-- BitPack
  [ primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.pack#) $ \case
      PrimStepContext{..}
        | Just (nTy, kn) <- extractKnownNat te sc tys
        , [i] <- unsignedLiterals' args
        -> reduce (mkBitVectorLit ty nTy kn 0 i)
      _ -> Nothing

-- Num
  , primStepEntry $(textNameLit '(Clash.Sized.Internal.Unsigned.+#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.+#) ty te sc tys args)
        -> reduce val
      _ -> Nothing
  ]
