{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module SymbiYosys where

import           Clash.Class.Counter
import           Clash.Prelude
import           Clash.Verification  (RenderAs (YosysFormal), assert, checkI,
                                      cover, eventually)

topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Bool, Bool, Bool)
topEntity = exposeClockResetEnable go

go :: HiddenClockResetEnable dom => Signal dom (Bool, Bool, Bool)
go =
  let -- oops, 'b' is never lit
      c   = register (0 :: Index 15) (countSucc <$> c)
      r   = (< 10) <$> c
      g   = ((>= 10) .&&. (< 15)) <$> c
      b   = (>= 15) <$> c
      out = bundle (r, g, b)
      p   = isPrimary <$> out
  in  checkI "isPrimary" YosysFormal (assert p)
        . checkI "R" YosysFormal (cover r)
        . checkI "G" YosysFormal (cover g)
        . checkI "B" YosysFormal (cover b)
        $ out

isPrimary :: (Bool, Bool, Bool) -> Bool
isPrimary (r, g, b) = r `xor` g `xor` b
