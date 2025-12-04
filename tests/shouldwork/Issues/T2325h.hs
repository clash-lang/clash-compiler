{-# LANGUAGE CPP #-}

module T2325h where

import Clash.Prelude

h :: Unsigned 8 -> Unsigned 8
h = (+ 5)
{-# OPAQUE h #-}
{-# ANN h (defSyn "h") #-}
