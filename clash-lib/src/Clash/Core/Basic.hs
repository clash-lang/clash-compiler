{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The parts of Clash Core's syntax that don't mention terms or types: type
  constants and literals, name modifiers, and what is known about primitives.
  Also the names of the primitives that stand for undefined values.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Core.Basic
  ( -- * Types
    ConstTy (..)
  , LitTy (..)
    -- * Terms
  , NameMod (..)
  , IsMultiPrim (..)
  , WorkInfo (..)
    -- * Special primitives
  , removedArgName
  , undefinedName
  , undefinedXName
  , undefinedPrims
  , undefinedXPrims
  ) where

import Control.DeepSeq (NFData)
import Control.Exception.Base (patError)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Extra (showt)
import GHC.Generics (Generic)
import GHC.Prim.Panic (absentError)
import GHC.Real
  (divZeroError, overflowError, ratioZeroDenominatorError, underflowError)

import {-# SOURCE #-} Clash.Core.TyCon (TyConName)
import Clash.XException (errorX)

-- | Type Constants
data ConstTy
  = TyCon !TyConName -- ^ TyCon type
  | Arrow            -- ^ Function type
  deriving (Eq,Ord,Show,Generic,NFData,Hashable,Binary)

-- | Literal Types
data LitTy
  = NumTy !Integer
  | SymTy !String
  | CharTy !Char
  deriving (Eq,Ord,Show,Generic,NFData,Hashable,Binary)

-- | Tag to indicate which instance/register name modifier was used
data NameMod
  = PrefixName
  -- ^ @Clash.Magic.prefixName@
  | SuffixName
  -- ^ @Clash.Magic.suffixName@
  | SuffixNameP
  -- ^ @Clash.Magic.suffixNameP@
  | SetName
  -- ^ @Clash.Magic.setName@
  deriving (Eq,Ord,Show,Generic,NFData,Hashable,Binary)

-- | Whether a primitive returns its results as one value, or assigns each of
-- them to a variable it is applied to
data IsMultiPrim
  = SingleResult
  | MultiResult
  deriving (Show, Generic, NFData, Eq, Hashable, Binary)

data WorkInfo
  = WorkConstant
  -- ^ Ignores its arguments, and outputs a constant
  | WorkNever
  -- ^ Never adds any work
  | WorkVariable
  -- ^ Does work when the arguments are variable
  | WorkAlways
  -- ^ Performs work regardless of whether the variables are constant or
  -- variable; these are things like clock or reset generators
  | WorkIdentity Int [Int]
  -- ^ A more restrictive version of 'WorkNever', where the value is the
  -- argument at the given position if all arguments for the given list of
  -- positions are also 'WorkIdentity'
  deriving (Eq,Show,Generic,NFData,Hashable,Binary)

-- | The name of the primitive that represents an argument which is
-- computationally irrelevant, and has been removed from the circuit. Its
-- blackbox is in @Clash_Normalize_Primitives.primitives.yaml@.
removedArgName :: Text
removedArgName = "Clash.Normalize.Primitives.removedArg"

-- | The name of the primitive that represents an undefined value that was
-- identified during normalization
undefinedName :: Text
undefinedName = "Clash.Normalize.Primitives.undefined"

-- | The name of the primitive that represents an X-exception throwing value
-- that was identified during normalization
undefinedXName :: Text
undefinedXName = "Clash.Normalize.Primitives.undefinedX"

-- | The primitives that are undefined values
undefinedPrims :: [Text]
undefinedPrims =
  undefinedName : fmap showt
  [ 'patError
  , 'error
  , 'errorWithoutStackTrace
  , 'undefined
  , 'absentError
  , 'divZeroError
  , 'overflowError
  , 'ratioZeroDenominatorError
  , 'underflowError
  ]

-- | The primitives that are X-exception throwing values
undefinedXPrims :: [Text]
undefinedXPrims = undefinedXName : fmap showt [ 'errorX ]
