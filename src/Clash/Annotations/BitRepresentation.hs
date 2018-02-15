{-|
Copyright  :  (C) 2017, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

This module houses annotations allowing custom bit representations for (custom)
data types.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Annotations.BitRepresentation
 ( DataRepr(..)
 , ConstrRepr(..)
 ) where

import qualified Language.Haskell.TH.Syntax as TH

import Data.Data (Data)
import Data.Typeable (Typeable)

type BitMask  = Word
type Value    = Word
type Size     = Word

type FieldAnn = BitMask

-- | Type annotation (Data)
data DataRepr a =
  DataRepr
    Size
    -- ^ Size of type
    [ConstrRepr]
    -- ^ Constructors
      deriving (Data, Typeable)

-- | Constructor annotation.
data ConstrRepr =
  ConstrRepr
    TH.Name
    -- ^ Constructor name
    BitMask
    -- ^ Bits relevant for this constructor
    Value
    -- ^ data & mask should be equal to..
    [FieldAnn]
    -- ^ Masks for fields. Indicates where fields are stored.
      deriving (Data, Typeable)


