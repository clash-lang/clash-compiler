{-|
Copyright  :  (C) 2018, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Using /ANN/ pragma's you can tell the Clash compiler to use a custom
bit representation for a data type. See @DataReprAnn@ for documentation.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Annotations.BitRepresentation
 (
 -- * Data structures to express a custom bit representation
   DataReprAnn(..)
 , ConstrRepr(..)
 -- * Convenience type synonyms for Integer
 , BitMask
 , Value
 , Size
 , FieldAnn

 -- * Functions
 , liftQ
 ) where

import           Data.Data                  (Data)
import           Data.Typeable              (Typeable)
import           Language.Haskell.TH.Instances ()
import qualified Language.Haskell.TH.Lift   ()
import qualified Language.Haskell.TH.Syntax as TH
import           GHC.Generics               (Generic)

type BitMask  = Integer
type Value    = Integer
type Size     = Int

-- | BitMask used to mask fields
type FieldAnn = BitMask

-- | Lift values inside of 'TH.Q' to a Template Haskell expression
liftQ :: TH.Lift a => TH.Q a -> TH.Q TH.Exp
liftQ = (>>= TH.lift)

-- NOTE: The following instances are imported from Language.Haskell.TH.Lift.
-- This module also implements 'instance Lift Exp', which might make debugging
-- template haskell more difficult. Please uncomment these instances and the
-- import of TH.Lift whenever it suits you.
--
--deriving instance TH.Lift TH.Name
--deriving instance TH.Lift TH.OccName
--deriving instance TH.Lift TH.NameFlavour
--deriving instance TH.Lift TH.ModName
--deriving instance TH.Lift TH.NameSpace
--deriving instance TH.Lift TH.PkgName


-- | Annotation for custom bit representations of data types
--
-- Using /ANN/ pragma's you can tell the Clash compiler to use a custom
-- bit-representation for a data type.
--
-- For example:
--
-- @
-- data Color = R | G | B
-- {-# ANN module (DataReprAnn
--                   $(liftQ [t|Color|])
--                   2
--                   [ ConstrRepr 'R 0b11 0b00 []
--                   , ConstrRepr 'G 0b11 0b01 []
--                   , ConstrRepr 'B 0b11 0b10 []
--                   ]) #-}
-- @
--
-- This specifies that @R@ should be encoded as 0b00, @G@ as 0b01, and
-- @B@ as 0b10. The first binary value in every @ConstrRepr@ in this example
-- is a mask, indicating which bits in the data type are relevant. In this case
-- all of the bits are.
--
-- Or if we want to annotate @Maybe Color@:
--
-- @
-- {-# ANN module ( DataReprAnn
--                    $(liftQ [t|Maybe Color|])
--                    2
--                    [ ConstrRepr 'Nothing 0b11 0b11 []
--                    , ConstrRepr 'Just 0b00 0b00 [0b11]
--                    ] ) #-}
-- @
--
-- By default, @Maybe Color@ is a data type which consumes 3 bits. A single bit
-- to indicate the constructor (either @Just@ or @Nothing@), and two bits to encode
-- the first field of @Just@. Notice that we saved a single bit by exploiting
-- the fact that @Color@ only uses three values (0, 1, 2), but takes two bits
-- to encode it. We can therefore use the last - unused - value (3), to encode
-- one of the constructors of @Maybe@. We indicate which bits encode the
-- underlying @Color@ field of @Just@ by passing /[0b11]/ to ConstrRepr. This
-- indicates that the first field is encoded in the first and second bit of the
-- whole datatype (0b11).
data DataReprAnn =
  DataReprAnn
    -- Type this annotation is for:
    TH.Type
    -- Size of type:
    Size
    -- Constructors:
    [ConstrRepr]
      deriving (Show, Data, Typeable, Eq, Generic, TH.Lift)

-- | Annotation for constructors. Indicates how to match this constructor based
-- off of the whole datatype.
data ConstrRepr =
  ConstrRepr
    -- Constructor name:
    TH.Name
    -- Bits relevant for this constructor:
    BitMask
    -- data & mask should be equal to..:
    Value
    -- Masks for fields. Indicates where fields are stored:
    [FieldAnn]
      deriving (Show, Data, Typeable, Eq, Generic, TH.Lift)
