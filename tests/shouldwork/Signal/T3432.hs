{-# LANGUAGE NoImplicitPrelude #-}

module T3432 where

import Clash.Prelude

topEntity :: Signal System ()
topEntity = ensureSpine undefined
