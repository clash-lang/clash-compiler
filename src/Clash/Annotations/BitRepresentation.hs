{-|
Copyright  :  (C) 2018, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Using /ANN/ pragma's you can tell the Clash compiler to use a custom
bit-representation for a data type. See @DataReprAnn@ for documentation.

-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Annotations.BitRepresentation
 ( DataRepr'(..)
 , ConstrRepr'(..)
 , DataReprAnn(..)
 , ConstrRepr(..)
 , CustomReprs
 , Type'(..)
 , BitMask
 , Value
 , Size
 , reprType
 , buildCustomReprs
 , dataReprAnnToDataRepr'
 , getConstrRepr
 , getDataRepr
 , thTypeToType'
 ) where

import           Control.DeepSeq            (NFData)
import           Data.Data                  (Data)
import           Data.Hashable              (Hashable)
import qualified Data.Map                   as Map
import qualified Data.Text.Lazy             as Text
import           Data.Typeable              (Typeable)
import qualified Language.Haskell.TH.Lib    as TH
import qualified Language.Haskell.TH.Lift   ()
import qualified Language.Haskell.TH.Syntax as TH
import           GHC.Generics               (Generic)


type BitMask  = Integer
type Value    = Integer
type Size     = Integer

type FieldAnn = BitMask

reprType :: TH.TypeQ -> TH.ExpQ
reprType qty = qty >>= TH.lift

deriving instance TH.Lift TH.Type
deriving instance TH.Lift TH.TyVarBndr
deriving instance TH.Lift TH.TyLit

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
--                   $(reprType [t|Color|])
--                   2
--                   [ ConstrRepr 'R 0b11 0b00 []
--                   , ConstrRepr 'G 0b11 0b01 []
--                   , ConstrRepr 'B 0b11 0b10 []
--                   ]) #-}
-- @
--
-- This specifies that @R@ should be encoded as 0b00, @G@ as 0b01, and
-- @B@ as 0b10. The first binary value in every @ConstRepr@ in this example
-- is a mask, indicating which bits in the data type are relevant. In this case
-- all of the bits are.
--
-- Or if we want to annotate @Maybe Color@:
--
-- @
-- {-# ANN module ( DataReprAnn
--                    $(reprType [t|Maybe Color|])
--                    2
--                    [ ConstRepr 'Nothing 0b11 0b11 []
--                    , ConstRepr 'Just 0b00 0b00 [0b11]
--                    ] ) #-}
-- @
--
-- By default, @Maybe Color@ is a data type which consumes 3 bits. A single bit
-- to indicate the constructor (either @Just@ or @Nothing@), and two bits to encode
-- the first field of @Just@. Notice that we saved a single bit, by exploiting
-- the fact that @Color@ only uses three values (0, 1, 2), but takes two bits
-- to encode it. We can therefore use the last - unused - value (3), to encode
-- one of the constructors of @Maybe@. We indicate which bits encode the
-- underlying @Color@ by passing /[0b11]/ to ConstRepr. This indicates that the
-- first field is encoded in the first and second bit of the whole datatype (0b11).
data DataReprAnn =
  DataReprAnn
    -- Type this annotation is for:
    TH.Type
    -- Size of type:
    Size
    -- Constructors:
    [ConstrRepr]
      deriving (Show, Data, Typeable)

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
      deriving (Show, Data, Typeable)


-- | Simple version of template haskell type. Used to
data Type'
  = AppTy' Type' Type'
  -- ^ Type application
  | ConstTy' Text.Text
  -- ^ Qualified name of type
  | LitTy' Integer
  -- ^ Numeral literal (used in BitVector 10, for example)
    deriving (Generic, NFData, Eq, Typeable, Hashable, Ord, Show)

-- | Internal version of DataRepr
data DataRepr' =
  DataRepr'
    -- Qualified name of type (recursive):
    Type'
    -- Size of data type:
    Size
    -- Constructors:
    [ConstrRepr']
      deriving (Show, Generic, NFData, Eq, Typeable, Hashable, Ord)

-- | Internal version of ConstRepr
data ConstrRepr' =
  ConstrRepr'
    -- Qualified name of constructor:
    Text.Text
    -- Syntactical position in the custom representations definition:
    Int
    -- Mask needed to determine constructor:
    BitMask
    -- Value after applying mask:
    Value
    -- Indicates where fields are stored:
    [FieldAnn]
      deriving (Show, Generic, NFData, Eq, Typeable, Ord, Hashable)

dataReprAnnToDataRepr' :: DataReprAnn -> DataRepr'
dataReprAnnToDataRepr' (DataReprAnn typ size constrs) =
  DataRepr' (thTypeToType' typ) size (zipWith toConstrRepr' [0..] constrs)
    where
      toConstrRepr' :: Int -> ConstrRepr -> ConstrRepr'
      toConstrRepr' n (ConstrRepr name mask value fieldanns) =
        ConstrRepr' (thToText name) n mask value (map fromIntegral fieldanns)

thToText :: TH.Name -> Text.Text
thToText (TH.Name (TH.OccName name') (TH.NameG _namespace _pkgName (TH.ModName modName))) =
  Text.pack $ modName ++ "." ++ name'
thToText name' = error $ "Unexpected pattern: " ++ show name'

-- | Convert template haskell type to simple representation of type
thTypeToType' :: TH.Type -> Type'
thTypeToType' ty = go ty
  where
    go (TH.ConT name')   = ConstTy' (thToText name')
    go (TH.AppT ty1 ty2) = AppTy' (go ty1) (go ty2)
    go (TH.LitT (TH.NumTyLit n)) = LitTy' n
    go _ = error $ "Unsupported type: " ++ show ty

-- | Convenience type for index built by buildCustomReprs
type CustomReprs =
  ( Map.Map Type' DataRepr'
  , Map.Map Text.Text ConstrRepr'
  )

-- | Lookup data type representation based on name
getDataRepr :: Type' -> CustomReprs -> Maybe DataRepr'
getDataRepr name (reprs, _) = Map.lookup name reprs

-- | Lookup constructor representation based on name
getConstrRepr :: Text.Text -> CustomReprs -> Maybe ConstrRepr'
getConstrRepr name (_, reprs) = Map.lookup name reprs

-- | Add CustomRepr to existing index
buildCustomRepr :: CustomReprs -> DataRepr' -> CustomReprs
buildCustomRepr (dMap, cMap) d@(DataRepr' name _size constrReprs) =
  let insertConstr c@(ConstrRepr' name' _ _ _ _) cMap' = Map.insert name' c cMap' in
  (Map.insert name d dMap, foldr insertConstr cMap constrReprs)

-- | Create indices based on names of constructors and data types
buildCustomReprs :: [DataRepr'] -> CustomReprs
buildCustomReprs = foldl buildCustomRepr (Map.empty, Map.empty)
