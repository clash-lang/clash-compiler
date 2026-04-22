{-# LANGUAGE DataKinds #-}

module CheckedLiteralsIndexFail where

import Clash.Prelude

topEntity :: Signal System (Index 4)
topEntity = -1
