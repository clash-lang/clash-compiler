{-
Copyright   : (C) 2016-2021 QBayLogic B.V.
                       2022 Alexander McKenna
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Data.UniqMap
  ( UniqMap(..)
  , empty
  , singleton
  , singletonUnique
  , null
  , insert
  , insertUnique
  , insertWith
  , insertMany
  , lookup
  , find
  , elem
  , notElem
  , filter
  , mapMaybe
  , foldrWithUnique
  , foldlWithUnique'
  , delete
  , deleteMany
  , unionWith
  , difference
  , disjoint
  , submap
  , fromList
  , toList
  , keys
  , elems
  ) where

import           Prelude hiding (elem, filter, lookup, notElem, null)

import           Control.DeepSeq (NFData)
import           Data.Binary (Binary (..))
import           Data.Bifunctor (first)
import           Data.Function (on)
#if MIN_VERSION_ghc(9,8,4)
import           GHC.Data.Word64Map.Strict (Word64Map)
import qualified GHC.Data.Word64Map.Strict as IntMap
#else
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
#endif
import qualified Data.List as List (foldl')

#if !MIN_VERSION_containers(0,6,2)
import qualified Data.IntMap.Extra as IntMap
#endif

#if MIN_VERSION_prettyprinter(1,7,0)
import           Prettyprinter
#else
import           Data.Text.Prettyprint.Doc
#endif

import           Clash.Pretty
import           Clash.Unique (Unique, Uniquable(getUnique))

-- | A map indexed by a 'Unique'. Typically the elements of this map are also
-- uniqueable and provide their own key, however a unique can be associated
-- with any value.
newtype UniqMap a
#if MIN_VERSION_ghc(9,8,4)
  = UniqMap { uniqMapToIntMap :: Word64Map a }
#else
  = UniqMap { uniqMapToIntMap :: IntMap a }
#endif
  deriving stock Traversable
  deriving newtype
    ( Foldable
    , Functor
    , Monoid
    , NFData
    , Semigroup
    , Show
    )
#if MIN_VERSION_ghc(9,8,4)
instance Binary a => Binary (UniqMap a) where
  put (UniqMap m) = put (IntMap.size m) <> mapM_ put (IntMap.toAscList m)
  get             = fmap (UniqMap . IntMap.fromDistinctAscList) get
#else
  deriving newtype Binary
#endif

instance ClashPretty a => ClashPretty (UniqMap a) where
  clashPretty xs =
    brackets $ fillSep $ punctuate comma $
      [ fromPretty k <+> ":->" <+> clashPretty v
      | (k, v) <- toList xs
      ]

-- | An empty map.
empty :: UniqMap a
empty =
  UniqMap IntMap.empty

{-# SPECIALIZE singleton :: Unique -> b -> UniqMap b #-}
-- | A map containing a single value indexed by the given key's unique.
singleton :: Uniquable a => a -> b -> UniqMap b
singleton k v =
  UniqMap (IntMap.singleton (getUnique k) v)

{-# SPECIALIZE singletonUnique :: Unique -> UniqMap Unique #-}
-- | A map containing a single value indexed by the value's unique.
singletonUnique :: Uniquable a => a -> UniqMap a
singletonUnique v =
  singleton (getUnique v) v

-- | Check if the map is empty.
null :: UniqMap a -> Bool
null =
  IntMap.null . uniqMapToIntMap

{-# SPECIALIZE insert :: Unique -> b -> UniqMap b -> UniqMap b #-}
-- | Insert a new key-value pair into the map.
insert :: Uniquable a => a -> b -> UniqMap b -> UniqMap b
insert k v =
  UniqMap . IntMap.insert (getUnique k) v . uniqMapToIntMap

{-# SPECIALIZE insertUnique :: Unique -> UniqMap Unique -> UniqMap Unique #-}
-- | Insert a new value into the map, using the unique of the value as the key.
insertUnique :: Uniquable a => a -> UniqMap a -> UniqMap a
insertUnique v =
  insert (getUnique v) v

-- | Insert a new key-value pair into the map, using the given combining
-- function if there is already an entry with the same unique in the map.
insertWith :: Uniquable a => (b -> b -> b) -> a -> b -> UniqMap b -> UniqMap b
insertWith f k v =
  UniqMap . IntMap.insertWith f (getUnique k) v . uniqMapToIntMap

-- | Insert a list of key-value pairs into the map.
insertMany :: Uniquable a => [(a, b)] -> UniqMap b -> UniqMap b
insertMany kvs xs =
  List.foldl' (\acc (k, v) -> insert k v acc) xs kvs

{-# SPECIALIZE lookup :: Unique -> UniqMap b -> Maybe b #-}
-- | Lookup an item in the map, using the unique of the given key.
lookup :: Uniquable a => a -> UniqMap b -> Maybe b
lookup k =
  IntMap.lookup (getUnique k) . uniqMapToIntMap

{-# SPECIALIZE find :: Unique -> UniqMap b -> b #-}
-- | Lookup and item in the map, using the unique of the given key. If the item
-- is not found in the map an error is raised.
find :: Uniquable a => a -> UniqMap b -> b
find k =
  let notFound =
        error ("find: Key " <> show (getUnique k) <> " is not in the UniqMap")
   in IntMap.findWithDefault notFound (getUnique k) . uniqMapToIntMap

{-# SPECIALIZE elem :: Unique -> UniqMap b -> Bool #-}
-- | Check if there is an entry in the map for the unique of the given value.
elem :: Uniquable a => a -> UniqMap b -> Bool
elem k =
  IntMap.member (getUnique k) . uniqMapToIntMap

{-# SPECIALIZE notElem :: Unique -> UniqMap b -> Bool #-}
-- | Check if there is not an entry in the map for the unique of the given
-- value.
notElem :: Uniquable a => a -> UniqMap b -> Bool
notElem k =
  IntMap.notMember (getUnique k) . uniqMapToIntMap

-- | Filter all elements in the map according to some predicate.
filter :: (b -> Bool) -> UniqMap b -> UniqMap b
filter p =
  UniqMap . IntMap.filter p . uniqMapToIntMap

-- | Apply a function to all elements in the map, keeping those where the
-- result is not @Nothing@.
mapMaybe :: (a -> Maybe b) -> UniqMap a -> UniqMap b
mapMaybe f =
  UniqMap . IntMap.mapMaybe f . uniqMapToIntMap

-- | Lazily right-fold over the map using the given function.
foldrWithUnique :: (Unique -> a -> b -> b) -> b -> UniqMap a -> b
foldrWithUnique f x =
  IntMap.foldrWithKey f x . uniqMapToIntMap

-- | Strictly left-fold over the map using the given function.
foldlWithUnique' :: (b -> Unique -> a -> b) -> b -> UniqMap a -> b
foldlWithUnique' f x =
  IntMap.foldlWithKey' f x . uniqMapToIntMap

{-# SPECIALIZE delete :: Unique -> UniqMap b -> UniqMap b #-}
-- | Delete the entry in the map indexed by the unique of the given value.
delete :: Uniquable a => a -> UniqMap b -> UniqMap b
delete k =
  UniqMap . IntMap.delete (getUnique k) . uniqMapToIntMap

-- | Delete all entries in the map indexed by the uniques of the given values.
deleteMany :: Uniquable a => [a] -> UniqMap b -> UniqMap b
deleteMany ks xs =
  List.foldl' (\acc k -> delete k acc) xs ks

-- | Merge two unique maps, using the given combining funcion if a value with
-- the same unique key exists in both maps.
unionWith :: (b -> b -> b) -> UniqMap b -> UniqMap b -> UniqMap b
unionWith f xs ys =
  UniqMap ((IntMap.unionWith f `on` uniqMapToIntMap) xs ys)

-- | Filter the first map to only contain keys which are not in the second map.
difference :: UniqMap b -> UniqMap b -> UniqMap b
difference xs ys =
  UniqMap ((IntMap.difference `on` uniqMapToIntMap) xs ys)

-- | Check if there are no common keys between two maps.
disjoint :: UniqMap b -> UniqMap b -> Bool
disjoint =
  IntMap.disjoint `on` uniqMapToIntMap

-- | Check if one map is a submap of another.
submap :: UniqMap b -> UniqMap b -> Bool
submap =
  -- We only check that the keys of the map make it a submap, the elements do
  -- not need to be equal. Maybe this should be changed?
  IntMap.isSubmapOfBy (\_ _ -> True) `on` uniqMapToIntMap

{-# SPECIALIZE fromList :: [(Unique, b)] -> UniqMap b #-}
-- | Convert a list of key-value pairs to a map.
fromList :: Uniquable a => [(a, b)] -> UniqMap b
fromList =
  UniqMap . IntMap.fromList . fmap (first getUnique)

-- | Convert a map to a list of unique-value pairs.
toList :: UniqMap b -> [(Unique, b)]
toList =
  IntMap.toList . uniqMapToIntMap

-- | Get the unique keys of a map.
keys :: UniqMap b -> [Unique]
keys =
  IntMap.keys . uniqMapToIntMap

-- | Get the values of a map.
elems :: UniqMap b -> [b]
elems =
  IntMap.elems . uniqMapToIntMap
