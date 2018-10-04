module TypeFamilyReduction where

import Clash.Prelude

-- See: https://github.com/clash-lang/clash-compiler/issues/352

div'
  :: Vec (Div 4 2) Bit
  -> Vec (Div 4 2) Bit
div' = id
{-# NOINLINE div' #-}

mod'
  :: Vec (Mod 6 4) Bit
  -> Vec (Mod 6 4) Bit
mod' = id
{-# NOINLINE mod' #-}

lcm'
  :: Vec (LCM 1 2) Bit
  -> Vec (LCM 1 2) Bit
lcm' = id
{-# NOINLINE lcm' #-}

gcd'
  :: Vec (GCD 4 6) Bit
  -> Vec (GCD 4 6) Bit
gcd' = id
{-# NOINLINE gcd' #-}

log'
  :: Vec (Log 8 64) Bit
  -> Vec (Log 8 64) Bit
log' = id
{-# NOINLINE log' #-}

clog'
  :: Vec (CLog 8 63) Bit
  -> Vec (CLog 8 63) Bit
clog' = id
{-# NOINLINE clog' #-}

flog'
  :: Vec (FLog 8 65) Bit
  -> Vec (FLog 8 65) Bit
flog' = id
{-# NOINLINE flog' #-}

topEntity :: Vec 2 Bit -> Vec 2 Bit
topEntity = div' . mod' . lcm' . gcd' . log' . clog' . flog'
