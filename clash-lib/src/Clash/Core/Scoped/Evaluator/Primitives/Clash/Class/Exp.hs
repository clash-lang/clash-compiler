{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Clash.Class.Exp" in the scoped evaluator:
  ports of @Clash.GHC.Evaluator.Primitives.Clash.Class.Exp@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.Core.Scoped.Evaluator.Primitives.Clash.Class.Exp
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Core.Basic (LitTy (..))
import Clash.Util (textNameLit)

import qualified Clash.Class.Exp

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  -- expIndex#
  --   :: KnownNat m
  --   => Index m
  --   -> SNat n
  --   -> Index (n^m)
  [ primStepEntry $(textNameLit 'Clash.Class.Exp.expIndex#) $ \case
      PrimStepContext{..}
        | [b] <- indexLiterals' args
        , [(_mTy, km), (_, e)] <- extractKnownNats te sc tys
        -> reduce (mkIndexLit ty (LitTy (NumTy (km^e))) (km^e) (b^e))
      _ -> Nothing


  -- expSigned#
  --   :: KnownNat m
  --   => Signed m
  --   -> SNat n
  --   -> Signed (n*m)
  , primStepEntry $(textNameLit 'Clash.Class.Exp.expSigned#) $ \case
      PrimStepContext{..}
        | [b] <- signedLiterals' args
        , [(_mTy, km), (_, e)] <- extractKnownNats te sc tys
        -> reduce (mkSignedLit ty (LitTy (NumTy (km*e))) (km*e) (b^e))
      _ -> Nothing


  -- expUnsigned#
  --   :: KnownNat m
  --   => Unsigned m
  --   -> SNat n
  --   -> Unsigned m
  , primStepEntry $(textNameLit 'Clash.Class.Exp.expUnsigned#) $ \case
      PrimStepContext{..}
        | [b] <- unsignedLiterals' args
        , [(_mTy, km), (_, e)] <- extractKnownNats te sc tys
        -> reduce (mkUnsignedLit ty (LitTy (NumTy (km*e))) (km*e) (b^e))
      _ -> Nothing

  ]
