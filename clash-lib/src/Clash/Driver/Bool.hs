{-|
  Copyright   :  (C) 2024, Martijn Bastiaan
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}

module Clash.Driver.Bool where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import qualified GHC.Data.Bool as Ghc

data OverridingBool = Auto | Never | Always
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Hashable, Generic, NFData)

toGhcOverridingBool :: OverridingBool -> Ghc.OverridingBool
toGhcOverridingBool Auto = Ghc.Auto
toGhcOverridingBool Never = Ghc.Never
toGhcOverridingBool Always = Ghc.Always

fromGhcOverridingBool :: Ghc.OverridingBool -> OverridingBool
fromGhcOverridingBool Ghc.Auto = Auto
fromGhcOverridingBool Ghc.Never = Never
fromGhcOverridingBool Ghc.Always = Always
