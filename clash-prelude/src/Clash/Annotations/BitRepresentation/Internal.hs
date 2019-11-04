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

module Clash.Annotations.BitRepresentation.Internal
  ( buildCustomReprs
  , dataReprAnnToDataRepr'
  , constrReprToConstrRepr'
  , getConstrRepr
  , uncheckedGetConstrRepr
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
import           Data.Maybe                               (fromMaybe)
import qualified Data.Text                                as Text
import           Data.Typeable                            (Typeable)
import qualified Language.Haskell.TH.Syntax               as TH
import           GHC.Generics                             (Generic)
import           GHC.Stack                                (HasCallStack)
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
data DataRepr' = DataRepr'
  { drType :: Type'
  -- ^ Simple representation of data type
  , drSize :: Size
  -- ^ Size of data type
  , drConstrs :: [ConstrRepr']
  -- ^ Constructors
  }
  deriving (Show, Generic, NFData, Eq, Typeable, Hashable, Ord)

-- | Internal version of ConstrRepr
data ConstrRepr' = ConstrRepr'
  { crName :: Text.Text
  -- ^ Qualified name of constructor
  , crPosition :: Int
  -- ^ Syntactical position in the custom representations definition
  , crMask :: BitMask
  -- ^ Mask needed to determine constructor
  , crValue :: Value
  -- ^ Value after applying mask
  , crFieldAnns :: [FieldAnn]
  -- ^ Indicates where fields are stored
  }
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

-- | Unchecked version of getConstrRepr
uncheckedGetConstrRepr
  :: HasCallStack
  => Text.Text
  -> CustomReprs
  -> ConstrRepr'
uncheckedGetConstrRepr name (_, reprs) =
  fromMaybe
    (error ("Could not find custom representation for" ++ Text.unpack name))
    (Map.lookup name reprs)

-- | Add CustomRepr to existing index
addCustomRepr :: CustomReprs -> DataRepr' -> CustomReprs
addCustomRepr (dMap, cMap) d@(DataRepr' name _size constrReprs) =
  let insertConstr c@(ConstrRepr' name' _ _ _ _) cMap' = Map.insert name' c cMap' in
  (Map.insert name d dMap, foldr insertConstr cMap constrReprs)

-- | Create indices based on names of constructors and data types
buildCustomReprs :: [DataRepr'] -> CustomReprs
buildCustomReprs = foldl addCustomRepr (Map.empty, Map.empty)
