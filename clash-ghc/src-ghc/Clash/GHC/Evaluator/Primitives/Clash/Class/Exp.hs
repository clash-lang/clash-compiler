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
module Clash.GHC.Evaluator.Primitives.Clash.Class.Exp
  ( primitives,
  )
where

import qualified Clash.Class.Exp
import Clash.Core.Evaluator.Types
import Clash.Core.Type (LitTy (..), Type (..))
import Clash.GHC.Evaluator.Primitive.Util
import Clash.Util (textNameLit)
import Data.Text (Text)

primitives :: [(Text, PrimStep)]
primitives =
  -- expIndex#
  --   :: KnownNat m
  --   => Index m
  --   -> SNat n
  --   -> Index (n^m)
  [ primStepEntry $(textNameLit 'Clash.Class.Exp.expIndex#) $ \case
      PrimStepContext {..}
        | [b] <- indexLiterals' args,
          [(_mTy, km), (_, e)] <- extractKnownNats tcm tys ->
            reduce (mkIndexLit ty (LitTy (NumTy (km ^ e))) (km ^ e) (b ^ e))
      _ -> Nothing,

    -- expSigned#
    --   :: KnownNat m
    --   => Signed m
    --   -> SNat n
    --   -> Signed (n*m)
    primStepEntry $(textNameLit 'Clash.Class.Exp.expSigned#) $ \case
      PrimStepContext {..}
        | [b] <- signedLiterals' args,
          [(_mTy, km), (_, e)] <- extractKnownNats tcm tys ->
            reduce (mkSignedLit ty (LitTy (NumTy (km * e))) (km * e) (b ^ e))
      _ -> Nothing,

    -- expUnsigned#
    --   :: KnownNat m
    --   => Unsigned m
    --   -> SNat n
    --   -> Unsigned m
    primStepEntry $(textNameLit 'Clash.Class.Exp.expUnsigned#) $ \case
      PrimStepContext {..}
        | [b] <- unsignedLiterals' args,
          [(_mTy, km), (_, e)] <- extractKnownNats tcm tys ->
            reduce (mkUnsignedLit ty (LitTy (NumTy (km * e))) (km * e) (b ^ e))
      _ -> Nothing
  ]
