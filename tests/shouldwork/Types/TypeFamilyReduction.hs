{-# LANGUAGE CPP #-}

module TypeFamilyReduction where

import Clash.Prelude

-- See: https://github.com/clash-lang/clash-compiler/issues/352

div'
  :: Vec (Div 4 2) Bit
  -> Vec (Div 4 2) Bit
div' = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE div' #-}

mod'
  :: Vec (Mod 6 4) Bit
  -> Vec (Mod 6 4) Bit
mod' = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE mod' #-}

lcm'
  :: Vec (LCM 1 2) Bit
  -> Vec (LCM 1 2) Bit
lcm' = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE lcm' #-}

gcd'
  :: Vec (GCD 4 6) Bit
  -> Vec (GCD 4 6) Bit
gcd' = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE gcd' #-}

log'
  :: Vec (Log 8 64) Bit
  -> Vec (Log 8 64) Bit
log' = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE log' #-}

clog'
  :: Vec (CLog 8 63) Bit
  -> Vec (CLog 8 63) Bit
clog' = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE clog' #-}

flog'
  :: Vec (FLog 8 65) Bit
  -> Vec (FLog 8 65) Bit
flog' = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE flog' #-}

topEntity :: Vec 2 Bit -> Vec 2 Bit
topEntity = div' . mod' . lcm' . gcd' . log' . clog' . flog'
