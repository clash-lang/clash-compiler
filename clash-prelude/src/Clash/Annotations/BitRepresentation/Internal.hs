{-|
Copyright  :  (C) 2018, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}

-- since GHC 8.6 we can haddock individual contructor fields \o/
#if __GLASGOW_HASKELL__ >= 806
#define FIELD ^
#endif

module Clash.Annotations.BitRepresentation.Internal
  ( buildCustomReprs
  , dataReprAnnToDataRepr'
  , constrReprToConstrRepr'
  , getConstrRepr
  , getDataRepr
  , thTypeToType'
  , ConstrRepr'(..)
  , DataRepr'(..)
  , Type'(..)
  , CustomReprs
  ) where

import           Clash.Annotations.BitRepresentation
  (BitMask, Value, Size, FieldAnn, DataReprAnn(..), ConstrRepr(..))
import           Control.DeepSeq                          (NFData)
import           Data.Hashable                            (Hashable)
import qualified Data.Map                                 as Map
import qualified Data.Text                                as Text
import           Data.Typeable                            (Typeable)
import qualified Language.Haskell.TH.Syntax               as TH
import           GHC.Generics                             (Generic)
import qualified TextShow                                 as TS
import qualified TextShow.Generic                         as TS


-- | Simple version of template haskell type. Used internally to match on.
data Type'
  = AppTy' Type' Type'
  -- ^ Type application
  | ConstTy' Text.Text
  -- ^ Qualified name of type
  | LitTy' Integer
  -- ^ Numeral literal (used in BitVector 10, for example)
    deriving (Generic, NFData, Eq, Typeable, Hashable, Ord, Show)
    deriving TS.TextShow via TS.FromGeneric (Type')

-- | Internal version of DataRepr
data DataRepr' =
  DataRepr'
    Type'         -- FIELD Simple representation of data type
    Size          -- FIELD Size of data type
    [ConstrRepr'] -- FIELD Constructors
      deriving (Show, Generic, NFData, Eq, Typeable, Hashable, Ord)

-- | Internal version of ConstrRepr
data ConstrRepr' =
  ConstrRepr'
    Text.Text  -- FIELD Qualified name of constructor:
    Int        -- FIELD Syntactical position in the custom representations definition:
    BitMask    -- FIELD Mask needed to determine constructor:
    Value      -- FIELD Value after applying mask:
    [FieldAnn] -- FIELD Indicates where fields are stored:
      deriving (Show, Generic, NFData, Eq, Typeable, Ord, Hashable)

constrReprToConstrRepr' :: Int -> ConstrRepr -> ConstrRepr'
constrReprToConstrRepr' n (ConstrRepr name mask value fieldanns) =
  ConstrRepr' (thToText name) n mask value (map fromIntegral fieldanns)

dataReprAnnToDataRepr' :: DataReprAnn -> DataRepr'
dataReprAnnToDataRepr' (DataReprAnn typ size constrs) =
  DataRepr' (thTypeToType' typ) size (zipWith constrReprToConstrRepr' [0..] constrs)

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
addCustomRepr :: CustomReprs -> DataRepr' -> CustomReprs
addCustomRepr (dMap, cMap) d@(DataRepr' name _size constrReprs) =
  let insertConstr c@(ConstrRepr' name' _ _ _ _) cMap' = Map.insert name' c cMap' in
  (Map.insert name d dMap, foldr insertConstr cMap constrReprs)

-- | Create indices based on names of constructors and data types
buildCustomReprs :: [DataRepr'] -> CustomReprs
buildCustomReprs = foldl addCustomRepr (Map.empty, Map.empty)
