module T1063 where

import Clash.Prelude

{-# ANN topEntity (defSyn "top")
  {t_inputs = [PortProduct "wrong" [PortName "one", PortName "two"]]}
  #-}
topEntity :: Int -> Int
topEntity = id
