{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Map.Ordered.Extra where

import Control.DeepSeq (NFData(rnf))
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OMap

instance (NFData k, NFData v) => NFData (OMap k v) where
  rnf = rnf . OMap.assocs
