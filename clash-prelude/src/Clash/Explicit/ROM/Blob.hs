{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

= Efficient bundling of ROM content with the compiled code

Leveraging Template Haskell, the content for the ROM components in this module
is stored alongside the compiled Haskell code. It covers use cases where passing
the initial content as a 'Clash.Sized.Vector.Vec' turns out to be
problematically slow.

The data is stored efficiently, with very little overhead (worst-case 7%, often
no overhead at all).

Unlike "Clash.Explicit.ROM.File", "Clash.Explicit.ROM.Blob" generates
practically the same HDL as "Clash.Explicit.ROM" and is compatible with all
tools consuming the generated HDL.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.ROM.Blob
  ( -- * ROMs defined by a 'MemBlob'
    romBlob
  , romBlobPow2
    -- * Creating and inspecting 'MemBlob'
  , MemBlob
  , createMemBlob
  , memBlobTH
  , unpackMemBlob
    -- * Internal
  , romBlob#
  ) where

import Data.Array (listArray)
import Data.Array.Base (unsafeAt)
import GHC.Stack (withFrozenCallStack)
import GHC.TypeLits (KnownNat, type (^))

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Explicit.BlockRam.Blob (createMemBlob, memBlobTH)
import Clash.Explicit.BlockRam.Internal (MemBlob(..), unpackMemBlob)
import Clash.Promoted.Nat (natToNum)
import Clash.Signal.Internal
  (Clock (..), Signal (..), Enable, fromEnable)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned (Unsigned)
import Clash.XException (deepErrorX, seqX)

import Clash.Annotations.Primitive(Primitive (InlineYamlPrimitive), HDL(..))
import Data.List.Infinite (Infinite((:<)), (...))
import Data.String.Interpolate (__i)

-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and
-- "Clash.Explicit.BlockRam#usingrams" for ideas on how to use ROMs and RAMs.
romBlob
  :: forall dom addr m n
   . ( Enum addr
     )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> MemBlob n m
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romBlob = \clk en content rd -> romBlob# clk en content (fromEnum <$> rd)
{-# INLINE romBlob #-}

-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and
-- "Clash.Explicit.BlockRam#usingrams" for ideas on how to use ROMs and RAMs.
romBlobPow2
  :: forall dom m n
   . ( KnownNat n
     )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> MemBlob (2^n) m
  -- ^ ROM content, also determines the size, 2^@n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romBlobPow2 = romBlob
{-# INLINE romBlobPow2 #-}

-- | ROM primitive
romBlob#
  :: forall dom m n
   . Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> MemBlob n m
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom Int
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romBlob# !_ en content@MemBlob{} =
  go
    (withFrozenCallStack (deepErrorX "romBlob: initial value undefined"))
    (fromEnable en)
 where
  szI = natToNum @n @Int
  arr = listArray (0,szI-1) $ unpackMemBlob content

  go o (e :- es) rd@(~(r :- rs)) =
    let o1 = if e then safeAt r else o
    -- See [Note: register strictness annotations]
    in  o `seqX` o :- (rd `seq` go o1 es rs)

  safeAt :: Int -> BitVector m
  safeAt i =
    if (0 <= i) && (i < szI) then
      unsafeAt arr i
    else
      withFrozenCallStack
        (deepErrorX ("romBlob: address " ++ show i ++
                     " not in range [0.." ++ show szI ++ ")"))
  {-# INLINE safeAt #-}
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE romBlob# #-}
{-# ANN romBlob# hasBlackBox #-}
{-# ANN romBlob# (
  let
    bbName = show 'romBlob#
    arg1 :< arg2 :< arg3 :< arg4 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [SystemVerilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          romBlob\#
            :: Clock dom        -- clk,  ARG[1]
            -> Enable dom       -- en,   ARG[2]
            -> MemBlob n m      -- init, ARG[3]
            -> Signal dom Int   -- rd,   ARG[4]
            -> Signal dom (BitVector m)
        template: |-
          // romBlob begin
          ~SIGD[~GENSYM[ROM][1]][#{arg3}];
          assign ~SYM[1] = ~CONST[#{arg3}];

          logic [~SIZE[~TYPO]-1:0] ~GENSYM[~RESULT_q][2];~IF ~ISACTIVEENABLE[#{arg2}] ~THEN
          always @(~IF ~ACTIVEEDGE[Rising][#{arg1}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg1}]) begin : ~GENSYM[~COMPNAME_rom][3]
            if (~ARG[#{arg2}]) begin
              ~SYM[2] <= ~SYM[1][~ARG[#{arg4}]];
            end
          end~ELSE
          always @(~IF ~ACTIVEEDGE[Rising][#{arg1}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg1}]) begin : ~SYM[3]
            ~SYM[2] <= ~SYM[1][~ARG[#{arg4}]];
          end~FI

          assign ~RESULT = ~SYM[2];
          // rom end
    |]) #-}
{-# ANN romBlob# (
  let
    bbName = show 'romBlob#
    arg1 :< arg2 :< arg3 :< arg4 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [Verilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        outputUsage: NonBlocking
        type: |-
          romBlob\#
            :: Clock dom        -- clk,  ARG[1]
            -> Enable dom       -- en,   ARG[2]
            -> MemBlob n m      -- init, ARG[3]
            -> Signal dom Int   -- rd,   ARG[4]
            -> Signal dom (BitVector m)
        template: |-
          // romBlob begin
          reg ~TYPO ~GENSYM[ROM][1] [0:~LENGTH[~TYP[#{arg3}]]-1];

          reg ~TYP[#{arg3}] ~GENSYM[rom_init][3];
          integer ~GENSYM[i][4];
          initial begin
            ~SYM[3] = ~CONST[#{arg3}];
            for (~SYM[4]=0; ~SYM[4] < ~LENGTH[~TYP[#{arg3}]]; ~SYM[4] = ~SYM[4] + 1) begin
              ~SYM[1][~LENGTH[~TYP[#{arg3}]]-1-~SYM[4]] = ~SYM[3][~SYM[4]*~SIZE[~TYPO]+:~SIZE[~TYPO]];
            end
          end
          ~IF ~ISACTIVEENABLE[#{arg2}] ~THEN
          always @(~IF ~ACTIVEEDGE[Rising][#{arg1}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg1}]) begin : ~GENSYM[~COMPNAME_rom][5]
            if (~ARG[#{arg2}]) begin
              ~RESULT <= ~SYM[1][~ARG[#{arg4}]];
            end
          end~ELSE
          always @(~IF ~ACTIVEEDGE[Rising][#{arg1}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg1}]) begin : ~SYM[5]
            ~RESULT <= ~SYM[1][~ARG[#{arg4}]];
          end~FI
          // romBlob end
    |]) #-}
{-# ANN romBlob# (
  let
    bbName = show 'romBlob#
    arg1 :< arg2 :< arg3 :< arg4 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        outputUsage: NonBlocking
        type: |-
          romBlob\#
            :: Clock dom        -- clk,  ARG[1]
            -> Enable dom       -- en,   ARG[2]
            -> MemBlob n m      -- init, ARG[3]
            -> Signal dom Int   -- rd,   ARG[4]
            -> Signal dom (BitVector m)
        template: |-
          -- romBlob begin
          ~GENSYM[~COMPNAME_rom][1] : block
            signal ~GENSYM[ROM][2] : ~TYP[#{arg3}];
            signal ~GENSYM[rd][3]  : integer range 0 to ~LENGTH[~TYP[#{arg3}]]-1;
          begin
            ~SYM[2] <= ~CONST[#{arg3}];

            ~SYM[3] <= to_integer(~VAR[rdI][#{arg4}](31 downto 0))
            -- pragma translate_off
                          mod ~LENGTH[~TYP[#{arg3}]]
            -- pragma translate_on
                          ;
            ~GENSYM[romSync][6] : process (~ARG[#{arg1}])
            begin
              if (~IF ~ACTIVEEDGE[Rising][#{arg1}] ~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{arg1}])~IF ~ISACTIVEENABLE[#{arg2}] ~THEN and ~ARG[#{arg2}]~ELSE~FI) then
                ~RESULT <= ~SYM[2](~SYM[3]);
              end if;
            end process;
          end block;
          -- romBlob end
    |]) #-}
