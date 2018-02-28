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
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Annotations.BitRepresentation
 ( DataRepr(..)
 , DataReprAnn(..)
 , ConstrRepr(..)
 , reprType
 ) where

import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH

import Data.Data (Data)
import Data.Typeable (Typeable)

type BitMask  = Integer
type Value    = Integer
type Size     = Integer

type FieldAnn = BitMask

reprType :: TH.TypeQ -> TH.ExpQ
reprType qty = qty >>= TH.lift

deriving instance TH.Lift TH.Type

-- NOTE: Don't import these from Language.Haskell.TH.Instances
--       Doing so will also import `instance Lift Exp`
--       And that changes certain TH mistakes from compile to run time errors.
deriving instance TH.Lift TH.TyVarBndr
deriving instance TH.Lift TH.TyLit
deriving instance TH.Lift TH.Name
deriving instance TH.Lift TH.OccName
deriving instance TH.Lift TH.NameFlavour
deriving instance TH.Lift TH.ModName
deriving instance TH.Lift TH.NameSpace
deriving instance TH.Lift TH.PkgName


-- | Type annotation for inline annotations. Example usage:
--
-- @
-- data Color = R | G | B
-- {-# ANN module (DataReprAnn $(typeOf [t|Color|]) 2 [...]) #-}
-- @
--
-- Or if we want to annotate
-- `Maybe Color`:
--
-- @
-- {-# ANN module ( DataReprAnn
--                    $(typeOf [t|Maybe Color|])
--                    2
--                    [...] ) #-}
-- @
data DataReprAnn =
  DataReprAnn
    TH.Type
    -- ^ Type this annotation is for
    Size
    -- ^ Size of type
    [ConstrRepr]
    -- ^ Constructors
      deriving (Show, Data, Typeable)

-- | Type annotation for annotations specified in a separate file, interpreted
-- by Clash using '-fclash-custom-reprs <path>'.
--
-- @
-- data Color = R | G | B
-- colorAnn = DataRepr 2 [...] :: DataRepr Color
-- @
--
-- To annotate composed types, simply extend /colorAnn/s type. For example, if
-- we want to annotate `Maybe Color`:
--
-- @
-- data Color = R | G | B
-- colorAnn = DataRepr 2 [...] :: DataRepr (Maybe Color)
-- @

data DataRepr a =
  DataRepr
    Size
    -- ^ Size of type
    [ConstrRepr]
    -- ^ Constructors
      deriving (Show, Data, Typeable)

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
      deriving (Show, Data, Typeable)
