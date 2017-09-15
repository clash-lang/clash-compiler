{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Annotations.TopEntity.Extra where

import Clash.Annotations.TopEntity (TopEntity, PortName)
import Data.Hashable               (Hashable)

instance Hashable TopEntity
instance Hashable PortName
