{-|
  Copyright   :  (C) 2023 QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Xilinx block RAM primitives
-}

{-# LANGUAGE CPP #-}

-- See [Note: eta port names for tdpbram]
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}

module Clash.Cores.Xilinx.BlockRam (tdpbram) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Clock(Clock))

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.BlockRam.Internal (tdpbram#)

-- | Instantiates a Block Memory Generator IP as described in
-- [PG058](https://docs.xilinx.com/v/u/en-US/pg058-blk-mem-gen).
--
-- Any value that is being written on a particular port is also the
-- value that will be read on that port, i.e. the same-port read/write behavior
-- is: WriteFirst. For mixed-port read/write, when port A writes to the address
-- port B reads from, the output of port B is undefined, and vice versa.
tdpbram ::
  forall nAddrs domA domB nBytes a .
  ( HasCallStack
  , KnownNat nAddrs
  , KnownNat nBytes
  , BitSize a ~ (8 * nBytes)
  , NFDataX a
  , BitPack a
  ) =>

  Clock domA ->
  -- | Port enable
  Enable domA ->
  -- | Address
  Signal domA (Index nAddrs) ->
  -- | Write byte enable
  Signal domA (BitVector nBytes) ->
  -- | Write data
  Signal domA a ->

  Clock domB ->
  -- | Port enable
  Enable domB ->
  -- | Address
  Signal domB (Index nAddrs) ->
  -- | Write byte enable
  Signal domB (BitVector nBytes) ->
  -- | Write data
  Signal domB a ->

  ( Signal domA a
  , Signal domB a
  )
tdpbram clkA@(Clock{}) enA addrA byteEnaA datA clkB@(Clock{}) enB addrB byteEnaB datB =
  -- [Note: eta port names for tdpbram]
  --
  -- By naming all the arguments and setting the -fno-do-lambda-eta-expansion GHC
  -- option for this module, the generated HDL also contains names based on the
  -- argument names used here. This greatly improves readability of the HDL.
  case (activeEdge @domA, activeEdge @domB) of
    (SRising, SRising) ->
      tdpbram#
        clkA (fromEnable enA) addrA byteEnaA datA
        clkB (fromEnable enB) addrB byteEnaB datB
    (SFalling, SFalling) ->
      clashCompileError "tdpbram: domain A and B need a rising active edge"
    (SFalling, _) ->
      clashCompileError "tdpbram: domain A needs a rising active edge"
    (_, SFalling) ->
      clashCompileError "tdpbram: domain B needs a rising active edge"
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE tdpbram #-}
