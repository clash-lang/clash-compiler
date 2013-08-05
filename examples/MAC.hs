{-# LANGUAGE DataKinds #-}
module MAC where

import CLaSH.Prelude

topEntity :: Signal (Signed 16) -> Signal (Signed 16) -> Signal (Signed 16)
topEntity x y = acc
  where
    acc = register 3 (x*y + acc)
