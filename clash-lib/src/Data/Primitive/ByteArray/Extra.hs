{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Primitive.ByteArray.Extra where

import Data.Binary (Binary(..))
import Data.Primitive.ByteArray (ByteArray)
import GHC.Exts (IsList(..))

#if !MIN_VERSION_primitive(0,7,1)
import Control.DeepSeq (NFData(..))
#endif

#if !MIN_VERSION_hashable(1,4,1)
-- Hashable <1.4.1 doesn't define hashable instances at all
#define DEFINE_HASHABLE_BYTEARRAY
#elif !MIN_VERSION_hashable(1,4,2)
-- Hashable 1.4.1 defines hashable for _some_ base versions
#if !MIN_VERSION_base(4,17,0)
#define DEFINE_HASHABLE_BYTEARRAY
#endif
#endif

#ifdef DEFINE_HASHABLE_BYTEARRAY
import Data.Hashable (Hashable(..))
#endif

#if !MIN_VERSION_primitive(0,7,1)
instance NFData ByteArray where
  rnf x = x `seq` ()
#endif

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList

#ifdef DEFINE_HASHABLE_BYTEARRAY
instance Hashable ByteArray where
  hashWithSalt salt = hashWithSalt salt . toList
#endif
