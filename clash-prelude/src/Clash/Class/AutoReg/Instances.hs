{-|
  Copyright   :  (C) 2019, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Class.AutoReg.Instances where

import           Clash.Class.AutoReg.Internal
import           Clash.CPP                           (maxTupleSize)

import           Data.Complex (Complex)
import           Data.Ord (Down)
import           Data.Ratio (Ratio)

deriveAutoReg ''Complex
deriveAutoReg ''Down
deriveAutoReg ''Ratio

deriveAutoRegTuples [2..maxTupleSize]
