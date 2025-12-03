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

#if MIN_VERSION_ghc(9,8,4) || (MIN_VERSION_ghc(9,6,7) && !MIN_VERSION_ghc(9,8,0))
#define UNIQUE_IS_WORD64
#endif

import Data.Word (Word64)

#ifdef UNIQUE_IS_WORD64
import GHC.Word (Word64(W64#))
import GHC.Exts (Word64#)
#else
import GHC.Int (Int(I#))
import GHC.Exts (Int#)
#endif
import qualified GHC.Types.Unique as GHC

#ifdef UNIQUE_IS_WORD64
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

#ifndef UNIQUE_IS_WORD64
instance Uniquable Word64 where
  getUnique = fromIntegral
  setUnique _ = fromIntegral
#endif

fromGhcUnique :: GHC.Unique -> Unique
fromGhcUnique = GHC.getKey
