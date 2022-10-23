{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Clash.Data.OverridingBool (OverridingBool(..)) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data OverridingBool
  = Auto
  | Always
  | Never
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)
