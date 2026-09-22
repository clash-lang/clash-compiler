{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Primitive.ByteArray.Extra where

import Data.Binary (Binary (..))
import Data.Primitive.ByteArray (ByteArray)
import GHC.Exts (IsList (..))

#if !MIN_VERSION_primitive(0,7,1)
import Control.DeepSeq (NFData (..))

#if !MIN_VERSION_primitive(0,8,0)
#ifdef DEFINE_HASHABLE_BYTEARRAY

-- hashable 1.4.2 defines Hashable for Data.Array.Byte.ByteArray, either from
-- base or from the data-array-byte compat package for GHC < 9.4.
-- primitive 0.8.0.0 re-exports this ByteArray.
-- In primitive < 0.8.0.0, its ByteArray is a distinct type from
-- Data.Array.Byte.ByteArray (insofar as the latter even exists).
#define DEFINE_HASHABLE_BYTEARRAY

import Data.Hashable (Hashable (..))

instance NFData ByteArray where
  rnf x = x `seq` ()

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList

instance Hashable ByteArray where
  hashWithSalt salt = hashWithSalt salt . toList
#else

-- hashable 1.4.2 defines Hashable for Data.Array.Byte.ByteArray, either from
-- base or from the data-array-byte compat package for GHC < 9.4.
-- primitive 0.8.0.0 re-exports this ByteArray.
-- In primitive < 0.8.0.0, its ByteArray is a distinct type from
-- Data.Array.Byte.ByteArray (insofar as the latter even exists).
#define DEFINE_HASHABLE_BYTEARRAY

instance NFData ByteArray where
  rnf x = x `seq` ()

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList
#endif
#else
#ifdef DEFINE_HASHABLE_BYTEARRAY

-- hashable 1.4.2 defines Hashable for Data.Array.Byte.ByteArray, either from
-- base or from the data-array-byte compat package for GHC < 9.4.
-- primitive 0.8.0.0 re-exports this ByteArray.
import Data.Hashable (Hashable (..))

instance NFData ByteArray where
  rnf x = x `seq` ()

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList

instance Hashable ByteArray where
  hashWithSalt salt = hashWithSalt salt . toList
#else

-- hashable 1.4.2 defines Hashable for Data.Array.Byte.ByteArray, either from
-- base or from the data-array-byte compat package for GHC < 9.4.
-- primitive 0.8.0.0 re-exports this ByteArray.
instance NFData ByteArray where
  rnf x = x `seq` ()

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList
#endif
#endif
#else
#if !MIN_VERSION_primitive(0,8,0)
#ifdef DEFINE_HASHABLE_BYTEARRAY

-- hashable 1.4.2 defines Hashable for Data.Array.Byte.ByteArray, either from
-- base or from the data-array-byte compat package for GHC < 9.4.
-- primitive 0.8.0.0 re-exports this ByteArray.
-- In primitive < 0.8.0.0, its ByteArray is a distinct type from
-- Data.Array.Byte.ByteArray (insofar as the latter even exists).
#define DEFINE_HASHABLE_BYTEARRAY

import Data.Hashable (Hashable (..))

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList

instance Hashable ByteArray where
  hashWithSalt salt = hashWithSalt salt . toList
#else

-- hashable 1.4.2 defines Hashable for Data.Array.Byte.ByteArray, either from
-- base or from the data-array-byte compat package for GHC < 9.4.
-- primitive 0.8.0.0 re-exports this ByteArray.
-- In primitive < 0.8.0.0, its ByteArray is a distinct type from
-- Data.Array.Byte.ByteArray (insofar as the latter even exists).
#define DEFINE_HASHABLE_BYTEARRAY

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList
#endif
#else
#ifdef DEFINE_HASHABLE_BYTEARRAY

-- hashable 1.4.2 defines Hashable for Data.Array.Byte.ByteArray, either from
-- base or from the data-array-byte compat package for GHC < 9.4.
-- primitive 0.8.0.0 re-exports this ByteArray.
import Data.Hashable (Hashable (..))

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList

instance Hashable ByteArray where
  hashWithSalt salt = hashWithSalt salt . toList
#else

-- hashable 1.4.2 defines Hashable for Data.Array.Byte.ByteArray, either from
-- base or from the data-array-byte compat package for GHC < 9.4.
-- primitive 0.8.0.0 re-exports this ByteArray.
instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList
#endif
#endif
#endif
