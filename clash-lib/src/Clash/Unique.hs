{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Unique
  ( -- * Unique
    Unique
  , Uniquable (..)
    -- * UniqMap
  , UniqMap
    -- ** Accessors
    -- *** Size information
  , nullUniqMap
    -- *** Indexing
  , lookupUniqMap
  , lookupUniqMap'
    -- ** Construction
  , emptyUniqMap
  , unitUniqMap
    -- ** Modification
  , extendUniqMap
  , extendUniqMapWith
  , extendListUniqMap
  , delUniqMap
  , delListUniqMap
  , unionUniqMap
  , unionUniqMapWith
  , differenceUniqMap
    -- ** Element-wise operations
    -- *** Mapping
  , mapUniqMap
  , mapMaybeUniqMap
    -- ** Working with predicates
    -- *** Filtering
  , filterUniqMap
    -- *** Searching
  , elemUniqMap
  , notElemUniqMap
  , elemUniqMapDirectly
    -- ** Folding
  , foldrWithUnique
  , foldlWithUnique'
    -- ** Conversions
    -- *** Lists
  , eltsUniqMap
  , keysUniqMap
  , listToUniqMap
  , toListUniqMap
    -- *** UniqSet
  , uniqMapToUniqSet
    -- * UniqSet
  , UniqSet
    -- ** Accessors
    -- *** Size information
  , nullUniqSet
    -- *** Indexing
  , lookupUniqSet
    -- ** Construction
  , emptyUniqSet
  , unitUniqSet
    -- ** Modifications
  , extendUniqSet
  , unionUniqSet
  , delUniqSetDirectly
    -- ** Working with predicates
    -- *** Searching
  , elemUniqSet
  , notElemUniqSet
  , elemUniqSetDirectly
    -- *** Misc
  , subsetUniqSet
  , differenceUniqSet
    -- ** Conversions
    -- *** Lists
  , mkUniqSet
  , eltsUniqSet
  )
where

import           Control.DeepSeq (NFData)
import           Data.Binary (Binary)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List   as List
import           Data.Text.Prettyprint.Doc
import           GHC.Stack

import           Clash.Pretty

type Unique = Int

class Uniquable a where
  getUnique :: a -> Unique
  setUnique :: a -> Unique -> a

instance Uniquable Int where
  getUnique i = i
  setUnique _i0 i1 = i1

-- | Map indexed by a 'Uniquable' key
newtype UniqMap a = UniqMap (IntMap a)
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, NFData, Binary)

instance ClashPretty a => ClashPretty (UniqMap a) where
  clashPretty (UniqMap env) =
    brackets $ fillSep $ punctuate comma $
      [ fromPretty uq <+> ":->" <+> clashPretty elt
      | (uq,elt) <- IntMap.toList env
      ]

instance ClashPretty a => Show (UniqMap a) where
  show = showDoc . clashPretty

-- | The empty map
emptyUniqMap
  :: UniqMap a
emptyUniqMap = UniqMap IntMap.empty

-- | Map with a single key-value pair
unitUniqMap
  :: Uniquable a
  => a
  -> b
  -> UniqMap b
unitUniqMap k v = UniqMap (IntMap.singleton (getUnique k) v)

-- | Check whether the map is empty
nullUniqMap
  :: UniqMap a
  -> Bool
nullUniqMap (UniqMap m) = IntMap.null m

-- | Extend the map with a new key-value pair. If the key already exists in the
-- associated value will be overwritten
extendUniqMap
  :: Uniquable a
  => a
  -> b
  -> UniqMap b
  -> UniqMap b
extendUniqMap k x (UniqMap m) = UniqMap (IntMap.insert (getUnique k) x m)

-- | Extend the map with a new key-value pair. If the key already exists in the
-- associated value will be combined with the new value using the function
-- provided
extendUniqMapWith
  :: Uniquable a
  => a
  -> b
  -> (b -> b -> b)
  -> UniqMap b
  -> UniqMap b
extendUniqMapWith k x f (UniqMap m) =
  UniqMap (IntMap.insertWith f (getUnique k) x m)

-- | Extend the map with a list of key-value pairs. Positions with existing
-- keys will be overwritten with the new values
extendListUniqMap
  :: Uniquable a
  => UniqMap b
  -> [(a, b)]
  -> UniqMap b
extendListUniqMap (UniqMap env) xs =
  UniqMap (List.foldl' (\m (k, v) -> IntMap.insert (getUnique k) v m) env xs)

-- | Look up a value in the map
lookupUniqMap
  :: Uniquable a
  => a
  -> UniqMap b
  -> Maybe b
lookupUniqMap k (UniqMap m) = IntMap.lookup (getUnique k) m

-- | Like 'lookupUniqMap'', but errors out when the key is not present
lookupUniqMap'
  :: (HasCallStack, Uniquable a)
  => UniqMap b
  -> a
  -> b
lookupUniqMap' (UniqMap m) k =
  IntMap.findWithDefault d k' m
 where
  k' = getUnique k
  d  = error ("lookupUniqMap': key " ++ show k' ++ " is not an element of the map")

-- | Check whether a key is in the map
elemUniqMap
  :: Uniquable a
  => a
  -> UniqMap b
  -> Bool
elemUniqMap k = elemUniqMapDirectly (getUnique k)

-- | Check whether an element exists in the uniqmap based on a given `Unique`
elemUniqMapDirectly
  :: Unique
  -> UniqMap b
  -> Bool
elemUniqMapDirectly k (UniqMap m) = k `IntMap.member` m
{-# INLINE elemUniqMapDirectly #-}

-- | Check whether a key is not in the map
notElemUniqMap
  :: Uniquable a
  => a
  -> UniqMap b
  -> Bool
notElemUniqMap k (UniqMap m) = IntMap.notMember (getUnique k) m

-- | Derive a map where all the elements adhere to the predicate
filterUniqMap
  :: (b -> Bool)
  -> UniqMap b
  -> UniqMap b
filterUniqMap f (UniqMap m) = UniqMap (IntMap.filter f m)

-- | Remove a key-value pair from the map
delUniqMap
  :: Uniquable a
  => UniqMap b
  -> a
  -> UniqMap b
delUniqMap (UniqMap env) v = UniqMap (IntMap.delete (getUnique v) env)

-- | Remove a list of key-value pairs from the map
delListUniqMap
  :: Uniquable a
  => UniqMap b
  -> [a]
  -> UniqMap b
delListUniqMap (UniqMap env) vs =
  UniqMap (List.foldl' (\m v -> IntMap.delete (getUnique v) m) env vs)

-- | A (left-biased) union of two maps
unionUniqMap
  :: UniqMap a
  -> UniqMap a
  -> UniqMap a
unionUniqMap (UniqMap m1) (UniqMap m2) = UniqMap (IntMap.union m1 m2)

-- | A union of two maps, key-value pairs with the same key will be merged using
-- the given function
unionUniqMapWith
  :: (a -> a -> a)
  -> UniqMap a
  -> UniqMap a
  -> UniqMap a
unionUniqMapWith f (UniqMap m1) (UniqMap m2) = UniqMap (IntMap.unionWith f m1 m2)

-- | Get the difference between two maps
differenceUniqMap
  :: UniqMap a
  -> UniqMap a
  -> UniqMap a
differenceUniqMap (UniqMap m1) (UniqMap m2) = UniqMap (IntMap.difference m1 m2)

-- | Convert a list of key-value pairs to a map
listToUniqMap
  :: Uniquable a
  => [(a,b)]
  -> UniqMap b
listToUniqMap xs =
  UniqMap (List.foldl' (\m (k, v) -> IntMap.insert (getUnique k) v m) IntMap.empty xs)

-- | Convert a map to a list of key-value pairs
toListUniqMap
  :: UniqMap a
  -> [(Unique,a)]
toListUniqMap (UniqMap m) = IntMap.toList m

-- | Extract the elements of a map into a list
eltsUniqMap
  :: UniqMap a
  -> [a]
eltsUniqMap (UniqMap m) = IntMap.elems m

-- | Apply a function to every element in the map
mapUniqMap
  :: (a -> b)
  -> UniqMap a
  -> UniqMap b
mapUniqMap f (UniqMap m) = UniqMap (IntMap.map f m)

-- | Extract the keys of a map into a list
keysUniqMap
  :: UniqMap a
  -> [Unique]
keysUniqMap (UniqMap m) = IntMap.keys m

-- | Apply a function to every element in the map. When the function returns
-- 'Nothing', the key-value pair will be removed
mapMaybeUniqMap
  :: (a -> Maybe b)
  -> UniqMap a
  -> UniqMap b
mapMaybeUniqMap f (UniqMap m) = UniqMap (IntMap.mapMaybe f m)

-- | Right-fold over a map using both the key and value
foldrWithUnique
  :: (Unique -> a -> b -> b)
  -> b
  -> UniqMap a
  -> b
foldrWithUnique f s (UniqMap m) = IntMap.foldrWithKey f s m

-- | Strict left-fold over a map using both the key and the value
foldlWithUnique'
  :: (a -> Unique -> b -> a)
  -> a
  -> UniqMap b
  -> a
foldlWithUnique' f s (UniqMap m) = IntMap.foldlWithKey' f s m

-- | Set of things that have a 'Unique'
--
-- Invariant: they keys in the map are the uniques of the values
newtype UniqSet a = UniqSet (IntMap a)
  deriving (Foldable, Semigroup, Monoid, NFData, Binary)

instance ClashPretty a => ClashPretty (UniqSet a) where
  clashPretty (UniqSet env) =
    braces (fillSep (map clashPretty (IntMap.elems env)))

-- | The empty set
emptyUniqSet
  :: UniqSet a
emptyUniqSet = UniqSet IntMap.empty

-- | Set with a single element
unitUniqSet
  :: Uniquable a
  => a
  -> UniqSet a
unitUniqSet a = UniqSet (IntMap.singleton (getUnique a) a)

-- | Add an element to the set
extendUniqSet
  :: Uniquable a
  => UniqSet a
  -> a
  -> UniqSet a
extendUniqSet (UniqSet env) a = UniqSet (IntMap.insert (getUnique a) a env)

-- | Union two sets
unionUniqSet
  :: UniqSet a
  -> UniqSet a
  -> UniqSet a
unionUniqSet (UniqSet env1) (UniqSet env2) = UniqSet (IntMap.union env1 env2)

-- | Check whether an element exists in the set
elemUniqSet
  :: Uniquable a
  => a
  -> UniqSet a
  -> Bool
elemUniqSet a (UniqSet env) = IntMap.member (getUnique a) env

-- | Check whether an element does not exist in the set
notElemUniqSet
  :: Uniquable a
  => a
  -> UniqSet a
  -> Bool
notElemUniqSet a (UniqSet env) = IntMap.notMember (getUnique a) env

-- | Check whether an element exists in the set based on the `Unique` contained
-- in that element
elemUniqSetDirectly
  :: Unique
  -> UniqSet a
  -> Bool
elemUniqSetDirectly k (UniqSet m) = k `IntMap.member` m

-- | Check whether a set is empty
nullUniqSet
  :: UniqSet a
  -> Bool
nullUniqSet (UniqSet env) = IntMap.null env

-- | Look up an element in the set, returns it if it exists
lookupUniqSet
  :: Uniquable a
  => a
  -> UniqSet b
  -> Maybe b
lookupUniqSet a (UniqSet env) = IntMap.lookup (getUnique a) env

-- | Remove an element based on the `Unique` it contains
delUniqSetDirectly
  :: Unique
  -> UniqSet b
  -> UniqSet b
delUniqSetDirectly k (UniqSet env) = UniqSet (IntMap.delete k env)

-- | Get the elements of the set as a list
eltsUniqSet
  :: UniqSet a
  -> [a]
eltsUniqSet (UniqSet env) = IntMap.elems env

-- | Create a set out of a list of elements that contain a 'Unique'
mkUniqSet
  :: Uniquable a
  => [a]
  -> UniqSet a
mkUniqSet m = UniqSet (IntMap.fromList (map (\x -> (getUnique x,x)) m))

-- | Convert a 'UniqMap' to a 'UniqSet'
uniqMapToUniqSet
  :: UniqMap a
  -> UniqSet a
uniqMapToUniqSet (UniqMap m) = UniqSet m

-- | Check whether a A is a subset of B
subsetUniqSet
  :: UniqSet a
  -- ^ Set A
  -> UniqSet a
  -- ^ Set B
  -> Bool
subsetUniqSet (UniqSet e1) (UniqSet e2) = IntMap.null (IntMap.difference e1 e2)

-- | Take the difference of two sets
differenceUniqSet
  :: UniqSet a
  -> UniqSet a
  -> UniqSet a
differenceUniqSet (UniqSet e1) (UniqSet e2) = UniqSet (IntMap.difference e1 e2)
