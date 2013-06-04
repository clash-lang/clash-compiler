{-# LANGUAGE DataKinds #-}
module MAC where

import CLaSH.Prelude

topEntity :: Sync (Signed 16) -> Sync (Signed 16) -> Sync (Signed 16)
topEntity x y = acc
  where
    acc = register 3 (x*y + acc)
