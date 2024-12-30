{-|
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Clash.Unique
  ( Unique
  , pattern Unique#
  , Unique#
  , Uniquable (..)
  , fromGhcUnique
  ) where

import Data.Word (Word64)
#if MIN_VERSION_ghc(9,10,0)
import GHC.Word (Word64(W64#))
import GHC.Exts (Word64#)
#else
import GHC.Int (Int(I#))
import GHC.Exts (Int#)
#endif
#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Types.Unique as GHC
#else
import qualified Unique as GHC
#endif

#if MIN_VERSION_ghc(9,10,0)
type Unique = Word64
type Unique# = Word64#

pattern Unique#
  :: Unique#
  -- ^ Type of signal
  -> Unique
pattern Unique# u <- W64# u
  where
    Unique# u = W64# u
#else
type Unique = Int
type Unique# = Int#

pattern Unique#
  :: Unique#
  -- ^ Type of signal
  -> Unique
pattern Unique# u <- I# u
  where
    Unique# u = I# u
#endif

class Uniquable a where
  getUnique :: a -> Unique
  setUnique :: a -> Unique -> a

instance Uniquable Unique where
  getUnique = id
  setUnique = flip const

#if !MIN_VERSION_ghc(9,10,0)
instance Uniquable Word64 where
  getUnique = fromIntegral
  setUnique _ = fromIntegral
#endif

#if MIN_VERSION_ghc(9,10,0)
fromGhcUnique :: GHC.Unique -> Unique
fromGhcUnique = fromIntegral . GHC.getKey
#else
fromGhcUnique :: GHC.Unique -> Unique
fromGhcUnique = id . GHC.getKey
#endif
