module XpmCdcSyncRst where

import Clash.Explicit.Prelude

import Data.Proxy

import XpmTestCommon (D3, D5, D10, D11)

import qualified XpmCdcSyncRstTypes as Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tb0 = done
 where
  --                          src dst stages  samples              init
  done =             Types.tb @D3 @D5 @4      @100     Proxy Proxy Nothing SNat expected
  expected = $(Types.expected @D3 @D5 @4      @100     Proxy Proxy Nothing SNat SNat)
{-# ANN tb0 (TestBench 'topEntity) #-}

tb1 = done
 where
  --                          src dst stages  samples              init
  done =             Types.tb @D5 @D3 @4      @100     Proxy Proxy Nothing SNat expected
  expected = $(Types.expected @D5 @D3 @4      @100     Proxy Proxy Nothing SNat SNat)
{-# ANN tb1 (TestBench 'topEntity) #-}

tb2 = done
 where
  --                          src dst stages  samples              init
  done =             Types.tb @D3 @D5 @10     @100     Proxy Proxy (Just True) SNat expected
  expected = $(Types.expected @D3 @D5 @10     @100     Proxy Proxy (Just True) SNat SNat)
{-# ANN tb2 (TestBench 'topEntity) #-}

tb3 = done
 where
  --                          src dst stages  samples              init
  done =             Types.tb @D3 @D5 @2      @100     Proxy Proxy (Just False) SNat expected
  expected = $(Types.expected @D3 @D5 @2      @100     Proxy Proxy (Just False) SNat SNat)
{-# ANN tb3 (TestBench 'topEntity) #-}

tb4 = done
 where
  --                          src dst  stages  samples              init
  done =             Types.tb @D5 @D10 @2      @100     Proxy Proxy Nothing SNat expected
  expected = $(Types.expected @D5 @D10 @2      @100     Proxy Proxy Nothing SNat SNat)
{-# ANN tb4 (TestBench 'topEntity) #-}

tb5 = done
 where
  --                          src dst  stages  samples              init
  done =             Types.tb @D10 @D5 @2      @100     Proxy Proxy Nothing SNat expected
  expected = $(Types.expected @D10 @D5 @2      @100     Proxy Proxy Nothing SNat SNat)
{-# ANN tb5 (TestBench 'topEntity) #-}

tb6 = done
 where
  --                          src dst  stages  samples              init
  done =             Types.tb @D5 @D11 @2      @100     Proxy Proxy (Just True) SNat expected
  expected = $(Types.expected @D5 @D11 @2      @100     Proxy Proxy (Just True) SNat SNat)
{-# ANN tb6 (TestBench 'topEntity) #-}

tb7 = done
 where
  --                          src dst  stages  samples              init
  done =             Types.tb @D11 @D5 @2      @100     Proxy Proxy (Just False) SNat expected
  expected = $(Types.expected @D11 @D5 @2      @100     Proxy Proxy (Just False) SNat SNat)
{-# ANN tb7 (TestBench 'topEntity) #-}
