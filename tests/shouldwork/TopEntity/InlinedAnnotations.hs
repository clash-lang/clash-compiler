module InlinedAnnotations where

import           Clash.Prelude
import qualified Clash.Explicit.Testbench as E

f :: Int -> Int -> Int
f a b = g a b * g b a
{-# ANN f (Synthesize
    { t_name   = "f"
    , t_inputs = [PortName "f_x", PortName "f_y"]
    , t_output = PortName "result"
    }) #-}

g :: Int -> Int -> Int
g = setName @"foo" h

h :: Int -> Int -> Int
h x y = if signum (x - y) == 1 then x else y
{-# ANN h (Synthesize
    { t_name   = "h"
    , t_inputs = [PortName "x", PortName "y"]
    , t_output = PortName "diff"
    }) #-}

