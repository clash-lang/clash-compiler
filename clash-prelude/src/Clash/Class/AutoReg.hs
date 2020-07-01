{-|
  Copyright   :  (C) 2019, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module Clash.Class.AutoReg
  ( AutoReg (autoReg, autoDelay)
  , deriveAutoReg
  ) where

import Clash.Class.AutoReg.Internal
import Clash.Class.AutoReg.Instances ()
