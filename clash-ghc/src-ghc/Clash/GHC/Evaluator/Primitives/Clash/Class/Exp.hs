{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.Evaluator.Primitives.Clash.Class.Exp
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import Clash.Core.Type (Type (..), LitTy (..))
import Clash.Util (textNameLit)

import qualified Clash.Class.Exp

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  -- expIndex#
  --   :: KnownNat m
  --   => Index m
  --   -> SNat n
  --   -> Index (n^m)
  [ ( $(textNameLit 'Clash.Class.Exp.expIndex#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [b] <- indexLiterals' args
            , [(_mTy, km), (_, e)] <- extractKnownNats tcm tys
            -> reduce (mkIndexLit ty (LitTy (NumTy (km^e))) (km^e) (b^e))
          _ -> Nothing
    )

  -- expSigned#
  --   :: KnownNat m
  --   => Signed m
  --   -> SNat n
  --   -> Signed (n*m)
  , ( $(textNameLit 'Clash.Class.Exp.expSigned#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [b] <- signedLiterals' args
            , [(_mTy, km), (_, e)] <- extractKnownNats tcm tys
            -> reduce (mkSignedLit ty (LitTy (NumTy (km*e))) (km*e) (b^e))
          _ -> Nothing
    )

  -- expUnsigned#
  --   :: KnownNat m
  --   => Unsigned m
  --   -> SNat n
  --   -> Unsigned m
  , ( $(textNameLit 'Clash.Class.Exp.expUnsigned#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [b] <- unsignedLiterals' args
            , [(_mTy, km), (_, e)] <- extractKnownNats tcm tys
            -> reduce (mkUnsignedLit ty (LitTy (NumTy (km*e))) (km*e) (b^e))
          _ -> Nothing
    )
  ]
