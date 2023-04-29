module XpmCdcHandshake where

import Clash.Cores.Xilinx.Xpm.Cdc.Handshake
import Clash.Explicit.Prelude
import Data.Proxy

import XpmCdcHandshakeTypes (D3, D5, D10, D11)

import qualified XpmCdcHandshakeTypes as Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0
{-# NOINLINE topEntity #-}

tb1 = done
 where
  --                          src dst srcStages dstStages samples                                               init
  done =             Types.tb @D3 @D5 @2        @2        @100     Proxy Proxy (XpmCdcHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D3 @D5 @2        @2        @100     Proxy Proxy (XpmCdcHandshakeConfig SNat SNat True) SNat)
{-# ANN tb1 (TestBench 'topEntity) #-}
{-# NOINLINE tb1 #-}

tb2 = done
 where
  --                          src dst srcStages dstStages samples                                               init
  done =             Types.tb @D5 @D3 @2        @2        @100     Proxy Proxy (XpmCdcHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D5 @D3 @2        @2        @100     Proxy Proxy (XpmCdcHandshakeConfig SNat SNat True) SNat)
{-# ANN tb2 (TestBench 'topEntity) #-}
{-# NOINLINE tb2 #-}

tb3 = done
 where
  --                          src dst srcStages dstStages samples                                               init
  done =             Types.tb @D5 @D3 @2        @2        @100     Proxy Proxy (XpmCdcHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D5 @D3 @2        @2        @100     Proxy Proxy (XpmCdcHandshakeConfig SNat SNat True) SNat)
{-# ANN tb3 (TestBench 'topEntity) #-}
{-# NOINLINE tb3 #-}

tb4 = done
 where
  --                          src dst  srcStages dstStages samples                                               init
  done =             Types.tb @D5 @D10 @2        @2        @100     Proxy Proxy (XpmCdcHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D5 @D10 @2        @2        @100     Proxy Proxy (XpmCdcHandshakeConfig SNat SNat True) SNat)
{-# ANN tb4 (TestBench 'topEntity) #-}
{-# NOINLINE tb4 #-}

tb5 = done
 where
  --                          src dst  srcStages dstStages samples                                               init
  done =             Types.tb @D3 @D11 @2        @2        @100     Proxy Proxy (XpmCdcHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D3 @D11 @2        @2        @100     Proxy Proxy (XpmCdcHandshakeConfig SNat SNat True) SNat)
{-# ANN tb5 (TestBench 'topEntity) #-}
{-# NOINLINE tb5 #-}
