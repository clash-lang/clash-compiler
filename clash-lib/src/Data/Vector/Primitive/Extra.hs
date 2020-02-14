{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Primitive.Extra
  (module Data.Vector.Primitive
  )
where

import Data.Hashable
import Data.HashMap.Strict as HashMap
import Data.Vector.Primitive
import Data.GenericTrie.Internal
import Data.Primitive.ByteArray

instance Hashable (Vector a) where
  hashWithSalt salt (Vector off len (ByteArray ba)) =
    hashByteArrayWithSalt ba off len salt

instance (Show a, Prim a, Eq a) => TrieKey (Vector a) where
  type TrieRep (Vector a)       = HashMap (Vector a)
  trieLookup k (MkTrie x)       = HashMap.lookup k x
  trieInsert k v (MkTrie t)     = MkTrie (HashMap.insert k v t)
  trieDelete k (MkTrie t)       = MkTrie (HashMap.delete k t)
  trieEmpty                     = MkTrie HashMap.empty
  trieSingleton k v             = MkTrie (HashMap.singleton k v)
  trieNull (MkTrie x)           = HashMap.null x
  trieMap f (MkTrie x)          = MkTrie (HashMap.map f x)
  trieTraverse f (MkTrie x)     = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  trieMapMaybeWithKey           = error "Vector.trieMapMaybeWithKey not implemented"
  trieFoldWithKey               = error "Vector.trieFoldWithKey not implemented"
  trieTraverseWithKey           = error "Vector.trieTraverseWithKey not implemented"
  trieMergeWithKey              = error "Vector.trieMergeWithKey not implemented"
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
