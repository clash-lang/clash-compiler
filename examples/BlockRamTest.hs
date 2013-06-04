{-# LANGUAGE DataKinds #-}
module BlockRamTest where

import CLaSH.Prelude

topEntity :: Sync (Unsigned 7)
          -> Sync (Unsigned 7)
          -> Sync (Bool)
          -> Sync (Unsigned 4)
          -> Sync (Unsigned 4)
topEntity = blockRamPow2 (sing :: Sing 128)
