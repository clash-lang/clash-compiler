{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Data Constructors in CoreHW
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Core.DataCon
  ( DataCon (..)
  , DcName
  , ConTag
  )
where

import Control.DeepSeq                        (NFData(..))
import Data.Binary                            (Binary)
import Data.Coerce                            (coerce)
import Data.GenericTrie.Internal              (TrieKey (..), Trie (..))
import Data.Hashable                          (Hashable)
import Data.IntMap                            (IntMap)
import qualified Data.IntMap                  as IntMap
import qualified Data.Text                    as Text
import GHC.Generics                           (Generic)

import Clash.Core.Name                        (Name (..), NameSort (..))
import {-# SOURCE #-} Clash.Core.Type         (Type)
import Clash.Core.Var                         (TyVar)
import Clash.Unique
import Clash.Util

-- | Data Constructor
data DataCon
  = MkData
  { dcName :: !DcName
  -- ^ Name of the DataCon
  , dcUniq :: {-# UNPACK #-} !Unique
  -- ^ Invariant: forall x . dcUniq x ~ nameUniq (dcName x)
  , dcTag :: !ConTag
  -- ^ Syntactical position in the type definition
  , dcType :: Type
  -- ^ Type of the 'DataCon
  , dcUnivTyVars :: [TyVar]
  -- ^ Universally quantified type-variables, these type variables are also part
  -- of the result type of the DataCon
  , dcExtTyVars :: [TyVar]
  -- ^ Existentially quantified type-variables, these type variables are not
  -- part of the result of the DataCon, but only of the arguments.
  , dcArgTys :: [Type]
  -- ^ Argument types
  , dcFieldLabels :: [Text.Text]
  -- ^ Names of fields. Used when data constructor is referring to a record type.
  } deriving (Generic,NFData,Hashable,Binary)

instance Show DataCon where
  show = show . dcName

instance Eq DataCon where
  (==) = (==) `on` dcUniq
  (/=) = (/=) `on` dcUniq

instance Ord DataCon where
  compare = compare `on` dcUniq

instance Uniquable DataCon where
  getUnique = dcUniq
  setUnique dc u = dc {dcUniq=u, dcName=(dcName dc){nameUniq=u}}

instance TrieKey DataCon where
  type TrieRep DataCon = IntMap
  trieLookup k (MkTrie x)       = IntMap.lookup (dcUniq k) x
  trieInsert k v (MkTrie t)     = MkTrie (IntMap.insert (dcUniq k) v t)
  trieDelete k (MkTrie t)       = MkTrie (IntMap.delete (dcUniq k) t)
  trieEmpty                     = MkTrie IntMap.empty
  trieSingleton k v             = MkTrie (IntMap.singleton (dcUniq k) v)
  trieNull (MkTrie x)           = IntMap.null x
  trieMap f (MkTrie x)          = MkTrie (IntMap.map f x)
  trieTraverse f (MkTrie x)     = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  trieMapMaybeWithKey f (MkTrie x)  = MkTrie (IntMap.mapMaybeWithKey (withDCKey f) x)
  trieFoldWithKey f z (MkTrie x)    = IntMap.foldrWithKey (withDCKey f) z x
  trieTraverseWithKey f (MkTrie x)  = fmap MkTrie (IntMap.traverseWithKey (withDCKey f) x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (IntMap.mergeWithKey (withDCKey f) (coerce g) (coerce h) x y)
  {-# INLINABLE trieEmpty #-}
  {-# INLINABLE trieInsert #-}
  {-# INLINABLE trieLookup #-}
  {-# INLINABLE trieDelete #-}
  {-# INLINABLE trieSingleton #-}
  {-# INLINABLE trieFoldWithKey #-}
  {-# INLINABLE trieShowsPrec #-}
  {-# INLINABLE trieTraverse #-}
  {-# INLINABLE trieTraverseWithKey #-}
  {-# INLINABLE trieNull #-}
  {-# INLINABLE trieMap #-}
  {-# INLINABLE trieMergeWithKey #-}
  {-# INLINABLE trieMapMaybeWithKey #-}

withDCKey
  :: (DataCon -> r)
  -> (Unique -> r)
withDCKey f k = f (MkData { dcName = Name Internal "" k noSrcSpan
                          , dcUniq = k
                          , dcTag  = 0
                          , dcType = error "withDCKey"
                          , dcUnivTyVars = []
                          , dcExtTyVars = []
                          , dcArgTys = []
                          , dcFieldLabels = []
                          }
                  )
{-# INLINE withDCKey #-}

-- | Syntactical position of the DataCon in the type definition
type ConTag = Int
-- | DataCon reference
type DcName = Name DataCon
