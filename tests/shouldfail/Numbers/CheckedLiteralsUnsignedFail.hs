{-# LANGUAGE DataKinds #-}

module CheckedLiteralsUnsignedFail where

import Clash.Prelude

topEntity :: Signal System (Unsigned 2)
topEntity = 4
