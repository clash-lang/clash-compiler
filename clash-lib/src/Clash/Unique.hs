{-|
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Clash.Unique
  ( Unique
  , Uniquable (..)
  , fromGhcUnique
  ) where

import Data.Word (Word64)
#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Types.Unique as GHC
#else
import qualified Unique as GHC
#endif

type Unique = Int

class Uniquable a where
  getUnique :: a -> Unique
  setUnique :: a -> Unique -> a

instance Uniquable Unique where
  getUnique = id
  setUnique = flip const

instance Uniquable Word64 where
  getUnique = fromIntegral
  setUnique _ = fromIntegral

#if MIN_VERSION_ghc(9,10,0)
fromGhcUnique :: GHC.Unique -> Unique
fromGhcUnique = fromIntegral . GHC.getKey
#else
fromGhcUnique :: GHC.Unique -> Unique
fromGhcUnique = id . GHC.getKey
#endif
