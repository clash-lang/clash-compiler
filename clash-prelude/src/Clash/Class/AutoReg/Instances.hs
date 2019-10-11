{-|
  Copyright   :  (C) 2019, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Class.AutoReg.Instances where

import           Clash.Class.AutoReg.Internal
import           Clash.CPP                           (maxTupleSize)

#if MIN_VERSION_base(4,12,0)
import           Data.Complex (Complex)
import           Data.Ord (Down)
#endif

import           Data.Ratio (Ratio)

#if MIN_VERSION_base(4,12,0)
deriveAutoReg ''Complex
deriveAutoReg ''Down
#endif

deriveAutoReg ''Ratio

-- TODO: this needs NFDataX instances to maxTupleSize that in turn need
-- TODO: generic instances for large tuples, but this slows GHC down by a
-- TODO: lot. See: https://phabricator.haskell.org/D2899. Maybe we could add
-- TODO: -flarge-tuples to clash-prelude?
-- deriveAutoRegTuples [2..maxTupleSize]
deriveAutoRegTuples [2..min maxTupleSize 15]
