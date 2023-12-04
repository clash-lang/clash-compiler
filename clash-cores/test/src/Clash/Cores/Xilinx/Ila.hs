{-|
Copyright  :  (C) 2023, Google Inc,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for [Xilinx Integrated Logic Analyzer v6.2](https://docs.xilinx.com/v/u/en-US/pg172-ila).
An Integrated Logic Analyzer (ILA) is a feature provided by Xilinx in its design
tools, notably Vivado, that allows designers to debug their FPGA logic in
real-time. It stores the signals it samples in a ring buffer, allowing users to
see values before and after a trigger point.

It is necessary to read the product guide linked above in order to effectively
use the IP. Clash simulation is not applicable for this IP.

When using the generated ILAs make sure you have set the correct JTAG clock speed:
[/"For non-Versal architectures, if your design contains debug cores, ensure that the JTAG clock is 2.5 times slower than the debug hub clock."/](https://www.xilinx.com/content/dam/xilinx/support/documents/sw_manuals/xilinx2022_2/ug908-vivado-programming-debugging.pdf)

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns#-}

-- See [Note: eta port names for trueDualPortBlockRam]
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}

module Clash.Cores.Xilinx.Ila
  ( ila

  -- * Config
  , IlaConfig(..)
  , ilaConfig
  , ProbeType(..)
  , Depth(..)

  -- * Utilities
  , Ila(..)
  ) where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive))

import Data.String.Interpolate (__i)

import Clash.Cores.Xilinx.Ila.Internal

-- | A default ILA config that:
--
--  * Configures no pipeline registers
--  * Stores 4096 samples
--  * Sets 2 comparators per probe
--  * Sets all probes to be suited for DATA and TRIGGER
--  * Enables capture control
--
-- See 'IlaConfig' for more information.
ilaConfig :: Vec n String -> IlaConfig n
ilaConfig names = IlaConfig
  { stages = 0
  , depth = D4096
  , comparators = Left 2
  , probeTypes = Left DataAndTrigger
  , probeNames = names
  , captureControl = True
  , advancedTriggers = False
  }


class Ila (dom :: Domain) a where
  ilaX :: a

instance Ila dom (Signal dom ()) where
  ilaX = pure ()

instance Ila dom a => Ila dom (Signal dom i -> a) where
  ilaX !_i = ilaX @dom @a

-- | A [polyvariadic](https://github.com/AJFarmar/haskell-polyvariadic) function
-- that instantiates a Xilinx Integrated Logic Analyzer (ILA).
--
-- Example invocation:
--
-- @
-- myAdder ::
--   forall dom .
--   'Signal' dom ('Unsigned' 8) ->
--   'Signal' dom ('Unsigned' 8) ->
--   'Signal' dom ('Unsigned' 8)
-- myAdder a b = ilaOut \`'hwSeqX'\` c
-- where
--  c = a + b
--
--  ilaOut :: Signal dom ()
--  ilaOut = 'ila' ('ilaConfig' ("a" :> "b" :> "add_result" :> Nil)) clk a b c
-- @
--
-- Note that signal names do not have to correspond to names passed to the ILA.
--
-- __N.B.__ Use 'Clash.XException.hwSeqX' to make sure the ILA does not get
--          optimized away by GHC.
ila ::
  forall dom a n .
  (KnownDomain dom, Ila dom a, 1 <= n) =>
  IlaConfig n ->
  -- | Clock to sample inputs on. Note that this is not necessarily the clock
  -- Xilinx's debug hub will run at, if multiple ILAs are instantiated.
  Clock dom ->
  -- | Any number of 'Signal' arguments. The result will always be
  -- @Signal dom ()@. You need to make sure this does not get optimized away by
  -- GHC by using 'Clash.XException.hwSeqX'.
  a
ila conf clk =
  ila# @dom @a conf clk
{-# CLASH_OPAQUE ila #-}

-- | Primitive for 'ila'. Defining a wrapper like this makes the ILA
-- instantiation be rendered in its own module to reduce naming collision
-- probabilities.
ila# ::
  forall dom a n .
  (KnownDomain dom, Ila dom a, 1 <= n) =>
  IlaConfig n ->
  Clock dom ->
  a
ila# !_conf !_clk = ilaX @dom @a
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE ila# #-}
{-# ANN ila# (
    let primName = 'ila#
        tfName = 'ilaBBF
    in InlineYamlPrimitive [minBound..] [__i|
         BlackBoxHaskell:
             name: #{primName}
             templateFunction: #{tfName}
             workInfo: Always
         |]) #-}
