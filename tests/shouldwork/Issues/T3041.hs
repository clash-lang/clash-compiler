{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE ViewPatterns #-}

module T3041 where

import Clash.Explicit.Prelude

-- From #3041: GHC vs Clash inlining produces blackbox instantiation failure.
unpackPack :: Bool -> Bool
unpackPack = unpack . pack
{-# INLINE unpackPack #-}

f ::
  Clock System ->
  Reset System ->
  Enable System ->
  Bool ->
  Signal System Bool ->
  ( Signal System Bool
  , Bool
  )
f clk rst ena initial i =
  ( register clk rst ena False i
  , unpackPack initial
  )

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System Bool
topEntity clk rst ena i = (s0, s1, notInitial) `seqX` register clk rst ena initial i
 where
  (s0, initial) = g clk rst ena False i
  (s1, not -> notInitial) = g clk rst ena False i
  g = f
{-# OPAQUE topEntity #-}
