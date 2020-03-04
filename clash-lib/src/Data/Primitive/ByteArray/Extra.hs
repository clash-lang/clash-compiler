{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Primitive.ByteArray.Extra where

import Control.DeepSeq (NFData(..))
import Data.Binary (Binary(..))
import Data.Hashable (Hashable(..))
import Data.Primitive.ByteArray (ByteArray)
import GHC.Exts (IsList(..))

instance NFData ByteArray where
  rnf x = x `seq` ()

instance Binary ByteArray where
  get = fmap fromList get
  put = put . toList

instance Hashable ByteArray where
  hashWithSalt salt = hashWithSalt salt . toList

