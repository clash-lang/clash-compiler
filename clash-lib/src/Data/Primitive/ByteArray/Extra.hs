{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Primitive.ByteArray.Extra where

#if !MIN_VERSION_primitive(0,7,1)
import Control.DeepSeq (NFData(..))
#endif
import Data.Binary (Binary(..))
import Data.Hashable (Hashable(..))
import Data.Primitive.ByteArray (ByteArray)
import GHC.Exts (IsList(..))

#if !MIN_VERSION_primitive(0,7,1)
instance NFData ByteArray where
  rnf x = x `seq` ()
#endif

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList

instance Hashable ByteArray where
  hashWithSalt salt = hashWithSalt salt . toList

