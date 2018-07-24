{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Primitive.Extra
  (module Data.Vector.Primitive
  )
where

import Data.Hashable
import Data.Vector.Primitive
import Data.Primitive.ByteArray

instance Hashable (Vector a) where
  hashWithSalt salt (Vector off len (ByteArray ba)) =
    hashByteArrayWithSalt ba off len salt
