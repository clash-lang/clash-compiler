{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Primitive.ByteArray.Extra where

import Data.Binary (Binary(..))
import Data.Primitive.ByteArray (ByteArray)
import GHC.Exts (IsList(..))

#if !MIN_VERSION_primitive(0,7,1)
import Control.DeepSeq (NFData(..))
#endif

#if !MIN_VERSION_hashable(1,4,2)
import Data.Hashable (Hashable(..))
#endif

#if !MIN_VERSION_primitive(0,7,1)
instance NFData ByteArray where
  rnf x = x `seq` ()
#endif

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList

#if !MIN_VERSION_hashable(1,4,2)
instance Hashable ByteArray where
  hashWithSalt salt = hashWithSalt salt . toList
#endif
