{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

ROMs
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.ROM
  ( -- * Synchronous ROM synchronized to an arbitrary clock
    rom
  , romPow2
    -- * Internal
  , rom#
  )
where

import Data.Array             (listArray)
import Data.Array.Base        (unsafeAt)
import GHC.Stack              (withFrozenCallStack)
import GHC.TypeLits           (KnownNat, type (^))
import Prelude hiding         (length)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Signal.Internal
  (Clock (..), ZKnownDomain, Signal (..), Enable, fromEnable)
import Clash.Sized.Unsigned   (Unsigned)
import Clash.Sized.Vector     (Vec, length, toList)
import Clash.XException       (deepErrorX, seqX, NFDataX)

import Clash.Annotations.Primitive(Primitive (InlineYamlPrimitive), HDL(..))
import Data.List.Infinite (Infinite((:<)), (...))
import Data.String.Interpolate (__i)

-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Explicit.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs.
-- * A large 'Vec' for the content may be too inefficient, depending on how it
-- is constructed. See 'Clash.Explicit.ROM.File.romFilePow2' and
-- 'Clash.Explicit.ROM.Blob.romBlobPow2' for different approaches that scale
-- well.
romPow2
  :: (KnownNat n, NFDataX a)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> Vec (2^n) a
  -- ^ ROM content
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom a
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romPow2 = rom
{-# INLINE romPow2 #-}

-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Explicit.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs.
-- * A large 'Vec' for the content may be too inefficient, depending on how it
-- is constructed. See 'Clash.Explicit.ROM.File.romFile' and
-- 'Clash.Explicit.ROM.Blob.romBlob' for different approaches that scale well.
rom
  :: (KnownNat n, NFDataX a, Enum addr)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> Vec n a
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom a
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
rom = \clk en content rd -> rom# clk en content (fromEnum <$> rd)
{-# INLINE rom #-}

-- | ROM primitive
rom#
  :: forall dom n a
   . (ZKnownDomain dom, KnownNat n, NFDataX a)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> Vec n a
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom Int
  -- ^ Read address @rd@
  -> Signal dom a
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
rom# !_ en content =
  go
    (withFrozenCallStack (deepErrorX "rom: initial value undefined"))
    (fromEnable en)
 where
  szI = length content
  arr = listArray (0,szI-1) (toList content)

  go o (e :- es) rd@(~(r :- rs)) =
    let o1 = if e then safeAt r else o
    -- See [Note: register strictness annotations]
    in  o `seqX` o :- (rd `seq` go o1 es rs)

  safeAt :: Int -> a
  safeAt i =
    if (0 <= i) && (i < szI) then
      unsafeAt arr i
    else
      withFrozenCallStack
        (deepErrorX ("rom: address " ++ show i ++
                     " not in range [0.." ++ show szI ++ ")"))
  {-# INLINE safeAt #-}
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE rom# #-}
{-# ANN rom# hasBlackBox #-}
{-# ANN rom# (
  let
    bbName = show 'rom#
    _arg0 :< _arg1 :< _arg2 :< arg3 :< arg4 :< arg5 :< arg6 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [SystemVerilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          rom\# :: ( ZKnownDomain dom        ARG[0]
                   , KnownNat n    --       ARG[1]
                   , Undefined a ) --       ARG[2]
                => Clock dom       -- clk,  ARG[3]
                => Enable dom      -- en,   ARG[4]
                -> Vec n a         -- init, ARG[5]
                -> Signal dom Int  -- rd,   ARG[6]
                -> Signal dom a
        template: |-
          // rom begin
          ~SIGD[~GENSYM[ROM][1]][#{arg5}];
          assign ~SYM[1] = ~LIT[#{arg5}];

          logic [~SIZE[~TYPO]-1:0] ~GENSYM[~RESULT_q][2];~IF ~ISACTIVEENABLE[#{arg4}] ~THEN
          always @(~IF ~ACTIVEEDGE[Rising][#{arg3}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg3}]) begin : ~GENSYM[~COMPNAME_rom][3]
            if (~ARG[#{arg4}]) begin
              ~SYM[2] <= ~SYM[1][~ARG[#{arg6}]];
            end
          end~ELSE
          always @(~IF ~ACTIVEEDGE[Rising][#{arg3}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg3}]) begin : ~SYM[3]
            ~SYM[2] <= ~SYM[1][~ARG[#{arg6}]];
          end~FI

          assign ~RESULT = ~FROMBV[~SYM[2]][~TYPO];
          // rom end
    |]) #-}
{-# ANN rom# (
  let
    bbName = show 'rom#
    _arg0 :< arg1 :< _arg2 :< arg3 :< arg4 :< arg5 :< arg6 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [Verilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        outputUsage: NonBlocking
        type: |-
          rom\# :: ( ZKnownDomain dom        ARG[0]
                   , KnownNat n    --       ARG[1]
                   , Undefined a ) --       ARG[2]
                => Clock dom       -- clk,  ARG[3]
                -> Enable dom      -- en,   ARG[4]
                -> Vec n a         -- init, ARG[5]
                -> Signal dom Int  -- rd,   ARG[6]
                -> Signal dom a
        template: |-
          // rom begin
          reg ~TYPO ~GENSYM[ROM][1] [0:~LIT[#{arg1}]-1];

          reg ~TYP[#{arg5}] ~GENSYM[rom_init][3];
          integer ~GENSYM[i][4];
          initial begin
            ~SYM[3] = ~LIT[#{arg5}];
            for (~SYM[4]=0; ~SYM[4] < ~LIT[#{arg1}]; ~SYM[4] = ~SYM[4] + 1) begin
              ~SYM[1][~LIT[#{arg1}]-1-~SYM[4]] = ~SYM[3][~SYM[4]*~SIZE[~TYPO]+:~SIZE[~TYPO]];
            end
          end
          ~IF ~ISACTIVEENABLE[#{arg4}] ~THEN
          always @(~IF ~ACTIVEEDGE[Rising][#{arg3}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg3}]) begin : ~GENSYM[~COMPNAME_rom][5]
            if (~ARG[#{arg4}]) begin
              ~RESULT <= ~SYM[1][~ARG[#{arg6}]];
            end
          end~ELSE
          always @(~IF ~ACTIVEEDGE[Rising][#{arg3}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg3}]) begin : ~SYM[5]
            ~RESULT <= ~SYM[1][~ARG[#{arg6}]];
          end~FI
          // rom end
    |]) #-}
{-# ANN rom# (
  let
    bbName = show 'rom#
    _arg0 :< arg1 :< _arg2 :< arg3 :< arg4 :< arg5 :< arg6 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        outputUsage: NonBlocking
        type: |-
          rom\# :: ( ZKnownDomain dom        ARG[0]
                   , KnownNat n    --       ARG[1]
                   , Undefined a ) --       ARG[2]
                => Clock dom       -- clk,  ARG[3]
                -> Enable dom      -- en,   ARG[4]
                -> Vec n a         -- init, ARG[5]
                -> Signal dom Int  -- rd,   ARG[6]
                -> Signal dom a
        template: |-
          -- rom begin
          ~GENSYM[~COMPNAME_rom][1] : block
            signal ~GENSYM[ROM][2] : ~TYP[#{arg5}];
            signal ~GENSYM[rd][3]  : integer range 0 to ~LIT[#{arg1}]-1;
          begin
            ~SYM[2] <= ~CONST[#{arg5}];

            ~SYM[3] <= to_integer(~VAR[rdI][#{arg6}](31 downto 0))
            -- pragma translate_off
                          mod ~LIT[#{arg1}]
            -- pragma translate_on
                          ;
            ~GENSYM[romSync][6] : process (~ARG[#{arg3}])
            begin
              if (~IF ~ACTIVEEDGE[Rising][#{arg3}] ~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{arg3}])~IF ~ISACTIVEENABLE[#{arg4}] ~THEN and ~ARG[#{arg4}] ~ELSE ~FI) then~IF ~VIVADO ~THEN
                ~RESULT <= ~FROMBV[~SYM[2](~SYM[3])][~TYPO];~ELSE
                ~RESULT <= ~SYM[2](~SYM[3]);~FI
              end if;
            end process;
          end block;
          -- rom end
    |]) #-}
