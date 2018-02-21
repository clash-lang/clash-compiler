{-|
Copyright  :  (C) 2017, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Annotations.BitRepresentation.Internal
  ( DataRepr'(..)
  , ConstrRepr'(..)
  , TypeName'(..)
  , CustomReprs
  , getDataRepr
  , getConstrRepr
  , buildCustomReprs
  , coreToTypeName'
  ) where

import qualified Data.Map       as Map
import qualified Data.Text.Lazy as Text

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import Data.Hashable (Hashable)

import Clash.Util (curLoc)
import Clash.Core.Name (name2String)
import Clash.Core.Type (Type (..), ConstTy (TyCon))

import Prelude

type BitMask' = Word
type Value'   = Word
type Size'    = Word

type FieldAnn' = BitMask'

type CustomReprs = ( Map.Map TypeName' DataRepr'
                   , Map.Map Text.Text ConstrRepr'
                   )

getDataRepr :: TypeName' -> CustomReprs -> Maybe DataRepr'
getDataRepr name (reprs, _) = Map.lookup name reprs

getConstrRepr :: Text.Text -> CustomReprs -> Maybe ConstrRepr'
getConstrRepr name (_, reprs) = Map.lookup name reprs

buildCustomRepr :: DataRepr' -> CustomReprs -> CustomReprs
buildCustomRepr d@(DataRepr' name _size constrReprs) (dMap, cMap) =
  let insertConstr c@(ConstrRepr' name' _ _ _ _) cMap' = Map.insert name' c cMap' in
  (Map.insert name d dMap, foldr insertConstr cMap constrReprs)

buildCustomReprs :: [DataRepr'] -> CustomReprs
buildCustomReprs = foldr buildCustomRepr (Map.empty, Map.empty)

coreToTypeName' :: Type -> Either String TypeName'
coreToTypeName' (AppTy t1 t2) = do
  (TypeName' name names) <- coreToTypeName' t1
  t2typeName            <- coreToTypeName' t2
  return $ TypeName' name (names ++ [t2typeName])
coreToTypeName' (ConstTy (TyCon name)) =
  return $ TypeName' (Text.pack $ name2String name) []
coreToTypeName' e =
  Left $ $(curLoc) ++ "Unexpected type: " ++ show e

-- |
data TypeName' =
  TypeName' Text.Text [TypeName']
    deriving (Generic, NFData, Eq, Typeable, Hashable, Ord)

showTypeName'
  :: Bool
  -- ^ Wrap in parentheses?
  -> TypeName'
  -- ^ TypeName' to pretty print
  -> Text.Text
-- Terminal: ignore request for parentheses
showTypeName' _ (TypeName' name []) =
  name
-- Wrap in parentheses, and move on:
showTypeName' True typeName =
  Text.concat ["(", showTypeName' False typeName, ")"]
-- Separate names by spaces:
showTypeName' False (TypeName' name names) =
  Text.intercalate " " $ name : map (showTypeName' True) names

instance Show TypeName' where
  show tn = concat [ "TypeName'<", Text.unpack $ showTypeName' False tn, ">" ]

-- |
data DataRepr' =
  DataRepr'
    TypeName'
    -- ^ Qualified name of type (recursive)
    Size'
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
    BitMask'
    -- ^ Mask needed to determine constructor
    Value'
    -- ^ Value after applying mask
    [FieldAnn']
    --
      deriving (Show, Generic, NFData, Eq, Typeable, Ord, Hashable)


