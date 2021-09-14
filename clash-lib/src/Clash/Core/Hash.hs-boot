{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Core.Hash where

import Data.Hashable (Hashable)
import {-# SOURCE #-} Clash.Core.Type (Type)

instance Hashable Type
