module XpmCdcAsyncRst where

import Clash.Explicit.Prelude

import Data.Proxy

import XpmTestCommon (Da3, Da5, Da10, Da11)

import qualified XpmCdcAsyncRstTypes as Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tb0 = done
 where
  --                          src dst stages  samples              init
  done =             Types.tb @Da3 @Da5 @4      @100     Proxy Proxy False SNat expected
  expected = $(Types.expected @Da3 @Da5 @4      @100     Proxy Proxy False SNat SNat)
{-# ANN tb0 (TestBench 'topEntity) #-}

tb1 = done
 where
  --                          src dst stages  samples              init
  done =             Types.tb @Da5 @Da3 @4      @100     Proxy Proxy False SNat expected
  expected = $(Types.expected @Da5 @Da3 @4      @100     Proxy Proxy False SNat SNat)
{-# ANN tb1 (TestBench 'topEntity) #-}

tb2 = done
 where
  --                          src dst stages  samples              init
  done =             Types.tb @Da3 @Da5 @10     @100     Proxy Proxy True SNat expected
  expected = $(Types.expected @Da3 @Da5 @10     @100     Proxy Proxy True SNat SNat)
{-# ANN tb2 (TestBench 'topEntity) #-}

tb3 = done
 where
  --                          src dst stages  samples              init
  done =             Types.tb @Da3 @Da5 @2      @100     Proxy Proxy True SNat expected
  expected = $(Types.expected @Da3 @Da5 @2      @100     Proxy Proxy True SNat SNat)
{-# ANN tb3 (TestBench 'topEntity) #-}

tb4 = done
 where
  --                          src dst  stages  samples              init
  done =             Types.tb @Da5 @Da10 @2      @100     Proxy Proxy False SNat expected
  expected = $(Types.expected @Da5 @Da10 @2      @100     Proxy Proxy False SNat SNat)
{-# ANN tb4 (TestBench 'topEntity) #-}

tb5 = done
 where
  --                          src dst  stages  samples              init
  done =             Types.tb @Da10 @Da5 @2      @100     Proxy Proxy False SNat expected
  expected = $(Types.expected @Da10 @Da5 @2      @100     Proxy Proxy False SNat SNat)
{-# ANN tb5 (TestBench 'topEntity) #-}

tb6 = done
 where
  --                          src dst  stages  samples              init
  done =             Types.tb @Da5 @Da11 @2      @100     Proxy Proxy True SNat expected
  expected = $(Types.expected @Da5 @Da11 @2      @100     Proxy Proxy True SNat SNat)
{-# ANN tb6 (TestBench 'topEntity) #-}

tb7 = done
 where
  --                          src dst  stages  samples              init
  done =             Types.tb @Da11 @Da5 @2      @100     Proxy Proxy True SNat expected
  expected = $(Types.expected @Da11 @Da5 @2      @100     Proxy Proxy True SNat SNat)
{-# ANN tb7 (TestBench 'topEntity) #-}
