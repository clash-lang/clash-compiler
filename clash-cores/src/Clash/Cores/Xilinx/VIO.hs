{-|
Copyright  :  (C) 2022-2023, Google Inc,
                  2023,      QBayLogic B.V.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for [Xilinx VIO Probes v3.0](https://docs.xilinx.com/v/u/en-US/pg159-vio).

It is necessary to read the product guide linked above in order to effectively
use the IP.

Note that VIOs offers an interactive hardware debugging feature that
lets you read from signals attached to input probes and write to
signals attached to output probes while, running your design on an
actual FPGA. Software offered by the IP vendor is required for using
this debugging feature. There is no respective software equivalent
build into Clash.

Clash simulation is not applicable for this IP.

The IP supports up to 256 input probes and up to 256 output probes,
each with a maximal width of 256 bits. If these limits are exceeded,
then Clash cannot instantiate the IP and raises an error.

When using the generated VIO probes make sure you have set the correct
JTAG clock speed:

[/"For non-Versal architectures, if your design contains debug cores, ensure that the JTAG clock is 2.5 times slower than the debug hub clock."/](https://www.xilinx.com/content/dam/xilinx/support/documents/sw_manuals/xilinx2022_2/ug908-vivado-programming-debugging.pdf)

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- See [Note: eta port names for trueDualPortBlockRam]
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}

module Clash.Cores.Xilinx.VIO
  ( vioProbe
  , VIO(..)
  ) where

import Clash.Explicit.Prelude
import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive))

import Data.String.Interpolate (__i)
import GHC.Magic (lazy)

import Clash.Cores.Xilinx.VIO.Internal.BlackBoxes

class VIO (dom :: Domain) a res | a -> res where
  vioX :: a

-- results are virtual outputs
instance VIO dom (Signal dom o) o where
  vioX = pure undefined

-- arguments are virtual inputs
instance VIO dom a o => VIO dom (Signal dom i -> a) o where
  vioX !_i = vioX @dom @a @o

-- | VIO Probes are available in Clash via the polyvariadic function
-- 'vioProbe'. If 'vioProbe' has more than one additional argument,
-- then every argument is turned into a single input probe, where the
-- probe's width is determined according to the argument's bit
-- size. If 'vioProbe' has only one additional argument, which
-- consists of multiple elements that are boxed within a vector or
-- product type, then each of these inner elements is assigned to a
-- single probe instead (with the probe widths matching the elements'
-- bit sizes accordingly). In any other case, the single argument is
-- assigned to a single probe. The output of 'vioProbe' is either
-- assigned to a single probe, or to multiple probes if boxed within a
-- vector or product type, similar to a single input argument.
--
-- Example incarnations are:
--
-- @
-- someProbe :: 'KnownDomain' dom => 'Clock' dom -> 'Signal' dom 'Bit' -> 'Signal' dom ('Unsigned' 8) -> 'Signal' dom ('Bool', 'Maybe' ('Signed' 8))
-- someProbe = 'vioProbe' ("in_b" :> "in_u8" :> Nil) ("out_b" :> "out_mu8" :> Nil) ('False', 'Nothing')
-- @
--
-- Creates VIO IP with two input probes of bit widths 1 and 8,
-- and two output probes of bit widths 1 and 9, respectively. The
-- output probes are both initialized to 0.
--
-- @
-- otherProbe :: 'KnownDomain' dom => 'Clock' dom -> 'Signal' dom ('Unsigned' 4, 'Unsigned' 2, 'Bit') -> 'Signal' dom ('Vec' 3 'Bit')
-- otherProbe = 'vioProbe' ("in_u4" :> "in_u2" :> "in_b" :> Nil) ("out_b1" :> "out_b2" :> "out_b3" :> Nil) ('repeat' 'high')
-- @
--
-- Creates VIO IP with three input probes of bit widths 4, 2, and 1,
-- and three output probes, all a single bit wide. The output probes
-- are all initialized to 1.
--
-- Note that under certain conditions Clash may optimize 'vioProbe' instances
-- away, especially if the VIO is only used to monitor some inputs and
-- produces no output. Utilizing 'Clash.XException.hwSeqX' may be helpful
-- in this case to enforce the VIO to be rendered in HDL.
vioProbe ::
  forall dom a o n m.
  (KnownDomain dom, VIO dom a o) =>
  Vec n String ->
  Vec m String ->
  o ->
  Clock dom ->
  a
vioProbe inputNames outputNames initialOutputProbeValues clk =
  vioProbe# @dom @a @o inputNames outputNames initialOutputProbeValues clk
{-# CLASH_OPAQUE vioProbe #-}

-- | Primitive for 'vioProbe'. Defining a wrapper like this makes the VIO probe
-- instantiation be rendered in its own module to reduce naming collision
-- probabilities.
vioProbe# ::
  forall dom a o n m.
  (KnownDomain dom, VIO dom a o) =>
  Vec n String ->
  Vec m String ->
  o ->
  Clock dom ->
  a
vioProbe# !_inputNames !_outputNames !_initialOutputProbeValues clk =
  lazy clk `seq` -- Ensure clk is considered used, but not marked "used once".
                 -- This makes makes GHC not inline the clock argument.
                 --
                 -- Fixes #2532
  vioX @dom @a @o
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE vioProbe# #-}
{-# ANN vioProbe# (
    let primName = 'vioProbe#
        tfName = 'vioProbeBBF
    in InlineYamlPrimitive [minBound..] [__i|
         BlackBoxHaskell:
             name: #{primName}
             templateFunction: #{tfName}
             workInfo: Always
         |]) #-}
