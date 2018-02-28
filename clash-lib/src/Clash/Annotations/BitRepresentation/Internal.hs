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
  , Type'(..)
  , BitMask'
  , CustomReprs
  , getDataRepr
  , getConstrRepr
  , buildCustomReprs
  , coreToType'
  ) where

import qualified Data.Map       as Map
import qualified Data.Text.Lazy as Text

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import Data.Hashable (Hashable)

import Clash.Util (curLoc)
import Clash.Core.Name (name2String)
import Clash.Core.Type (Type (..), ConstTy (TyCon), LitTy(..))

import Prelude

type BitMask' = Integer
type Value'   = Integer
type Size'    = Integer

type FieldAnn' = BitMask'

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

coreToType' :: Type -> Either String Type'
coreToType' (AppTy t1 t2) = AppTy' <$> coreToType' t1 <*> coreToType' t2
coreToType' (ConstTy (TyCon name)) =
   return $ ConstTy' (Text.pack $ name2String name)
coreToType' (LitTy (NumTy n)) =
   return $ LitTy' n
coreToType' e =
  Left $ $(curLoc) ++ "Unexpected type: " ++ show e


-- |
data Type' = AppTy' Type' Type'
           | ConstTy' Text.Text
           | LitTy' Integer
    deriving (Generic, NFData, Eq, Typeable, Hashable, Ord    , Show)

-- showType'
--   :: Bool
--   -- ^ Wrap in parentheses?
--   -> Type'
--   -- ^ Type' to pretty print
--   -> Text.Text
-- -- Terminal: ignore request for parentheses
-- showType' _ (Type' name []) =
--   name
-- -- Wrap in parentheses, and move on:
-- showType' True typeName =
--   Text.concat ["(", showType' False typeName, ")"]
-- -- Separate names by spaces:
-- showType' False (Type' name names) =
--   Text.intercalate " " $ name : map (showType' True) names
--
-- instance Show Type' where
--   show tn = concat [ "Type'<", Text.unpack $ showType' False tn, ">" ]

-- |
data DataRepr' =
  DataRepr'
    Type'
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
