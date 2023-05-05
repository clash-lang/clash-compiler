{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module XpmCdcAsyncRst where

import Clash.Explicit.Prelude
import Data.Proxy

import XpmCdcAsyncRstTypes (D3, D5)

import qualified XpmCdcAsyncRstTypes as Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0
{-# NOINLINE topEntity #-}

tb1 :: Signal D5 Bool
tb1 = done
 where
  --                          src dst stages              init
  done =             Types.tb @D3 @D5 @2      Proxy Proxy False SNat expected
  expected = $(Types.expected @D3 @D5 @2      Proxy Proxy False SNat)
{-# ANN tb1 (TestBench 'topEntity) #-}

tb2 :: Signal D3 Bool
tb2 = done
 where
  --                          src dst stages              init
  done =             Types.tb @D3 @D3 @2      Proxy Proxy False SNat expected
  expected = $(Types.expected @D3 @D3 @2      Proxy Proxy False SNat)
{-# ANN tb2 (TestBench 'topEntity) #-}

tb3 :: Signal D3 Bool
tb3 = done
 where
  --                          src dst stages              init
  done =             Types.tb @D5 @D3 @2      Proxy Proxy False SNat expected
  expected = $(Types.expected @D5 @D3 @2      Proxy Proxy False SNat)
{-# ANN tb3 (TestBench 'topEntity) #-}
