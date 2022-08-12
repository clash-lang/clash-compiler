{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -O -fno-worker-wrapper -fexpose-all-unfoldings #-}

module T2289 where

import Clash.Prelude

import T2289.Core

type M12L16161A_5T = 'SDRAMSpec 1

m12l16161a_5t :: SDRAMModule M12L16161A_5T
m12l16161a_5t = SDRAMModule

sDRAMCoreT'
  :: forall (s :: SDRAMSpec)
   . SDRAMModule s
  -> SDRAMState s
  -> SDRAMState s
sDRAMCoreT' SDRAMModule s = sDRAMCoreT s

topEntity :: Signal System (SDRAMState M12L16161A_5T) -> Signal System (SDRAMState M12L16161A_5T)
topEntity s = fmap (sDRAMCoreT' m12l16161a_5t) s
