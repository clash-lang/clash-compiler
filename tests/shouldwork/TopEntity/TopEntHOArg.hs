{-# LANGUAGE CPP #-}

module TopEntHOArg where

import Clash.Prelude

f :: Bit
  -> (Bool,(Bit,Bool),Maybe Bit)
  -> Bool
  -> (Bit,Bool,Maybe Bit,(Bit, Bool),Bool)
f z (a,b,c) d = (z,a,c,b,d)
{-# ANN f Synthesize {t_name = "f", t_inputs = [PortName "z",PortProduct "" []], t_output = PortProduct "" []} #-}
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE f #-}

g
  :: Bit
  -> Vec 2 (Bool,(Bit,Bool),Maybe Bit)
  -> Vec 2 Bool
  -> Vec 2 (Bit,Bool,Maybe Bit,(Bit, Bool), Bool)
g b = zipWith (f b)
{-# ANN g Synthesize {t_name = "g", t_inputs = [PortName "z", PortProduct "" []], t_output = PortProduct "" []} #-}
