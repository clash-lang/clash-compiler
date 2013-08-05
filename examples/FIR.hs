{-# LANGUAGE DataKinds, TemplateHaskell #-}
module FIR where

import CLaSH.Prelude

dotp as bs = vfoldl (+) 0 (vzipWith (*) as bs)

fir coeffs x_t = y_t
  where
    y_t = dotp coeffs xs
    xs  = window x_t

topEntity :: Signal (Signed 16) -> Signal (Signed 16)
topEntity = fir $(v [0::Signal (Signed 16),1,2,3])
