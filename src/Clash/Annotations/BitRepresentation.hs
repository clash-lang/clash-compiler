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
 , DataRepr'(..)
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
 ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import Data.Hashable (Hashable)
import Data.Data (Data)

import qualified Data.Map as Map
import qualified Data.Text.Lazy as Text
import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lift ()

type BitMask  = Integer
type Value    = Integer
type Size     = Integer

type FieldAnn = BitMask

reprType :: TH.TypeQ -> TH.ExpQ
reprType qty = qty >>= TH.lift

deriving instance TH.Lift TH.Type
deriving instance TH.Lift TH.TyVarBndr
deriving instance TH.Lift TH.TyLit

-- NOTE: The following instances are imported from Language.Haskell.TH.Instances.
-- This module also implements 'instance Lift Exp', which might make debugging
-- template haskell more difficult. Please uncomment these instnaces and the
-- import of TH.Instances whenever it suits you.
--
--deriving instance TH.Lift TH.Name
--deriving instance TH.Lift TH.OccName
--deriving instance TH.Lift TH.NameFlavour
--deriving instance TH.Lift TH.ModName
--deriving instance TH.Lift TH.NameSpace
--deriving instance TH.Lift TH.PkgName


-- | Type annotation for inline annotations. Example usage:
--
-- @
-- data Color = R | G | B
-- {-# ANN module (DataReprAnn $(reprType [t|Color|]) 2 [...]) #-}
-- @
--
-- Or if we want to annotate
-- `Maybe Color`:
--
-- @
-- {-# ANN module ( DataReprAnn
--                    $(reprType [t|Maybe Color|])
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


-- |
data Type' = AppTy' Type' Type'
           | ConstTy' Text.Text
           | LitTy' Integer
    deriving (Generic, NFData, Eq, Typeable, Hashable, Ord, Show)

-- |
data DataRepr' =
  DataRepr'
    Type'
    -- ^ Qualified name of type (recursive)
    Size
    -- ^ Size of data type
    [ConstrRepr']
    -- ^ Constructors
      deriving (Show, Generic, NFData, Eq, Typeable, Hashable, Ord)

-- |
data ConstrRepr' =
  ConstrRepr'
    Text.Text
    -- ^ Qualified name of constructor
    Int
    -- ^ Syntactical position in the custom representations definition.
    BitMask
    -- ^ Mask needed to determine constructor
    Value
    -- ^ Value after applying mask
    [FieldAnn]
    --
      deriving (Show, Generic, NFData, Eq, Typeable, Ord, Hashable)

dataReprAnnToDataRepr' :: DataReprAnn -> DataRepr'
dataReprAnnToDataRepr' (DataReprAnn typ size constrs) =
  DataRepr' (toType' typ) size (zipWith toConstrRepr' [0..] constrs)
    where
      toConstrRepr' :: Int -> ConstrRepr -> ConstrRepr'
      toConstrRepr' n (ConstrRepr name mask value fieldanns) =
        ConstrRepr' (thToText name) n (fromIntegral mask) value (map fromIntegral fieldanns)

      thToText :: TH.Name -> Text.Text
      thToText (TH.Name (TH.OccName name') (TH.NameG _namespace _pkgName (TH.ModName modName))) =
        Text.pack $ modName ++ "." ++ name'
      thToText name' = error $ {-$(curLoc) ++-} "Unexpected pattern: " ++ show name'

      toType' :: TH.Type -> Type'
      toType' ty = go ty
        where
          go (TH.ConT name')   = ConstTy' (thToText name')
          go (TH.AppT ty1 ty2) = AppTy' (go ty1) (go ty2)
          go (TH.LitT (TH.NumTyLit n)) = LitTy' n
          go _ = error $ {-$(curLoc) ++-} "Unsupported type: " ++ show ty


type CustomReprs = ( Map.Map Type' DataRepr'
                   , Map.Map Text.Text ConstrRepr'
                   )

getDataRepr :: Type' -> CustomReprs -> Maybe DataRepr'
getDataRepr name (reprs, _) = Map.lookup name reprs

getConstrRepr :: Text.Text -> CustomReprs -> Maybe ConstrRepr'
getConstrRepr name (_, reprs) = Map.lookup name reprs

buildCustomRepr :: DataRepr' -> CustomReprs -> CustomReprs
buildCustomRepr d@(DataRepr' name _size constrReprs) (dMap, cMap) =
  let insertConstr c@(ConstrRepr' name' _ _ _ _) cMap' = Map.insert name' c cMap' in
  (Map.insert name d dMap, foldr insertConstr cMap constrReprs)

buildCustomReprs :: [DataRepr'] -> CustomReprs
buildCustomReprs = foldr buildCustomRepr (Map.empty, Map.empty)