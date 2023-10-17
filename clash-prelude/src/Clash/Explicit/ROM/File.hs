{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.,
                  2019     , Myrtle Software Ltd.,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

= Initializing a ROM with a data file #usingromfiles#

ROMs initialized with a data file. The BNF grammar for this data file is simple:

@
FILE = LINE+
LINE = BIT+
BIT  = '0'
     | '1'
@

Consecutive @LINE@s correspond to consecutive memory addresses starting at @0@.
For example, a data file @memory.bin@ containing the 9-bit unsigned numbers
@7@ to @13@ looks like:

@
000000111
000001000
000001001
000001010
000001011
000001100
000001101
@

Such a file can be produced with 'memFile':

@
writeFile "memory.bin" (memFile Nothing [7 :: Unsigned 9 .. 13])
@

We can instantiate a synchronous ROM using the contents of the file above like
so:

@
f :: Clock  dom
  -> Enable dom
  -> Signal dom (Unsigned 3)
  -> Signal dom (Unsigned 9)
f clk en rd = 'Clash.Class.BitPack.unpack' '<$>' 'romFile' clk en d7 \"memory.bin\" rd
@

And see that it works as expected:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ f systemClockGen (fromList [3..5])__
[10,11,12]
@

However, we can also interpret the same data as a tuple of a 6-bit unsigned
number, and a 3-bit signed number:

@
g :: Clock  dom
  -> Signal dom (Unsigned 3)
  -> Signal dom (Unsigned 6,Signed 3)
g clk en rd = 'Clash.Class.BitPack.unpack' '<$>' 'romFile' clk en d7 \"memory.bin\" rd
@

And then we would see:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ g systemClockGen (fromList [3..5])__
[(1,2),(1,3)(1,-4)]
@
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.ROM.File
  ( -- * Synchronous ROM synchronized to an arbitrary clock
    romFile
  , romFilePow2
    -- * Producing files
  , memFile
    -- * Internal
  , romFile#
  )
where

import Data.Array                   (listArray)
import Data.Array.Base              (unsafeAt)
import GHC.TypeLits                 (KnownNat)
import System.IO.Unsafe             (unsafePerformIO)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Explicit.BlockRam.File (initMem, memFile)
import Clash.Promoted.Nat           (SNat (..), pow2SNat, snatToNum)
import Clash.Sized.BitVector        (BitVector)
import Clash.Explicit.Signal        (Clock, Enable, Signal, ZKnownDomain, delay)
import Clash.Sized.Unsigned         (Unsigned)
import Clash.XException             (NFDataX(deepErrorX))

import Clash.Annotations.Primitive(Primitive (InlineYamlPrimitive), HDL(..))
import Data.List.Infinite (Infinite((:<)), (...))
import Data.String.Interpolate (__i)

-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
-- +----------------+----------+----------+---------------+
-- |                | VHDL     | Verilog  | SystemVerilog |
-- +================+==========+==========+===============+
-- | Altera/Quartus | Broken   | Works    | Works         |
-- +----------------+----------+----------+---------------+
-- | Xilinx/ISE     | Works    | Works    | Works         |
-- +----------------+----------+----------+---------------+
-- | ASIC           | Untested | Untested | Untested      |
-- +----------------+----------+----------+---------------+
--
-- === See also:
--
-- * See "Clash.Explicit.ROM.File#usingromfiles" for more information on how
-- to instantiate a ROM with the contents of a data file.
-- * See 'memFile' for creating a data file with Clash.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for more ideas on how to create
-- your own data files.
romFilePow2
  :: forall dom  n m
   . (KnownNat m, KnownNat n)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> FilePath
  -- ^ File describing the content of the ROM
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romFilePow2 = \clk en -> romFile clk en (pow2SNat (SNat @n))
{-# INLINE romFilePow2 #-}

-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
-- +----------------+----------+----------+---------------+
-- |                | VHDL     | Verilog  | SystemVerilog |
-- +================+==========+==========+===============+
-- | Altera/Quartus | Broken   | Works    | Works         |
-- +----------------+----------+----------+---------------+
-- | Xilinx/ISE     | Works    | Works    | Works         |
-- +----------------+----------+----------+---------------+
-- | ASIC           | Untested | Untested | Untested      |
-- +----------------+----------+----------+---------------+
--
-- === See also:
--
-- * See "Clash.Explicit.ROM.File#usingromfiles" for more information on how
-- to instantiate a ROM with the contents of a data file.
-- * See 'memFile' for creating a data file with Clash.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
romFile
  :: (KnownNat m, Enum addr)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> SNat n
  -- ^ Size of the ROM
  -> FilePath
  -- ^ File describing the content of the ROM
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romFile = \clk en sz file rd -> romFile# clk en sz file (fromEnum <$> rd)
{-# INLINE romFile #-}

-- | romFile primitive
romFile#
  :: forall m dom n
   . (KnownNat m, ZKnownDomain dom)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> SNat n
  -- ^ Size of the ROM
  -> FilePath
  -- ^ File describing the content of the ROM
  -> Signal dom Int
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romFile# clk en sz file rd =
  delay clk en (deepErrorX "First value of romFile is undefined")
        (safeAt <$> rd)
 where
  mem     = unsafePerformIO (initMem file)
  content = listArray (0,szI-1) mem
  szI     = snatToNum sz

  safeAt :: Int -> BitVector m
  safeAt i =
    if (0 <= i) && (i < szI) then
      unsafeAt content i
    else
      deepErrorX ("romFile: address " ++ show i ++
                  " not in range [0.." ++ show szI ++ ")")
  {-# INLINE safeAt #-}
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE romFile# #-}
{-# ANN romFile# hasBlackBox #-}
{-# ANN romFile# (
  let
    bbName = show 'romFile#
    _arg0 :< _arg1 :< arg2 :< arg3 :< arg4 :< arg5 :< arg6 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [SystemVerilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          romFile\# :: ( KnownNat m             --       ARG[0]
                       , ZKnownDomain dom )      --       ARG[1]
                    => Clock dom                -- clk,  ARG[2]
                    -> Enable dom               -- en,   ARG[3]
                    -> SNat n                   -- sz,   ARG[4]
                    -> FilePath                 -- file, ARG[5]
                    -> Signal dom Int           -- rd,   ARG[6]
                    -> Signal dom (BitVector m)
        template: |-
          // romFile begin
          ~SIGDO[~GENSYM[ROM][0]] [0:~LIT[#{arg4}]-1];

          initial begin
            $readmemb(~FILE[~LIT[#{arg5}]],~SYM[0]);
          end

          ~SIGDO[~GENSYM[~RESULT_q][1]];~IF ~ISACTIVEENABLE[#{arg3}] ~THEN
          always @(~IF ~ACTIVEEDGE[Rising][#{arg2}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg2}]) begin : ~GENSYM[~COMPNAME_romFile][2]
            if (~ARG[#{arg3}]) begin
              ~SYM[1] <= ~SYM[0][~ARG[#{arg6}]];
            end
          end~ELSE
          always @(~IF ~ACTIVEEDGE[Rising][#{arg2}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg2}]) begin : ~SYM[2]
            ~SYM[1] <= ~SYM[0][~ARG[#{arg6}]];
          end~FI

          assign ~RESULT = ~SYM[1];
          // romFile end
    |]) #-}
{-# ANN romFile# (
  let
    bbName = show 'romFile#
    _arg0 :< _arg1 :< arg2 :< arg3 :< arg4 :< arg5 :< arg6 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [Verilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        outputUsage: NonBlocking
        type: |-
          romFile\# :: ( KnownNat m             --       ARG[0]
                       , ZKnownDomain dom      ) --       ARG[1]
                    => Clock dom                -- clk,  ARG[2]
                    -> Enable dom               -- en,   ARG[3]
                    -> SNat n                   -- sz,   ARG[4]
                    -> FilePath                 -- file, ARG[5]
                    -> Signal dom Int           -- rd,   ARG[6]
                    -> Signal dom (BitVector m)
        template: |-
          // romFile begin
          reg ~TYPO ~GENSYM[ROM][0] [0:~LIT[#{arg4}]-1];

          initial begin
            $readmemb(~FILE[~LIT[#{arg5}]],~SYM[0]);
          end
          ~IF ~ISACTIVEENABLE[#{arg3}] ~THEN
          always @(~IF ~ACTIVEEDGE[Rising][#{arg2}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg2}]) begin : ~GENSYM[~COMPNAME_romFile][2]
            if (~ARG[#{arg3}]) begin
              ~RESULT <= ~SYM[0][~ARG[#{arg6}]];
            end
          end~ELSE
          always @(~IF ~ACTIVEEDGE[Rising][#{arg2}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg2}]) begin : ~SYM[2]
            ~RESULT <= ~SYM[0][~ARG[#{arg6}]];
          end~FI
          // romFile end
    |]) #-}
{-# ANN romFile# (
  let
    bbName = show 'romFile#
    arg0 :< _arg1 :< arg2 :< arg3 :< arg4 :< arg5 :< arg6 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        outputUsage: NonBlocking
        type: |-
          romFile\# :: ( KnownNat m           --       ARG[0]
                       , ZKnownDomain dom      --       ARG[1]
                    => Clock dom              -- clk,  ARG[2]
                    -> Enable dom             -- en,   ARG[3]
                    -> SNat n                 -- sz,   ARG[4]
                    -> FilePath               -- file, ARG[5]
                    -> Signal dom Int         -- rd,   ARG[6]
                    -> Signal dom (BitVector m)
        template: |-
          -- romFile begin
          ~GENSYM[~COMPNAME_romFile][0] : block
            type ~GENSYM[RomType][4] is array(natural range <>) of bit_vector(~LIT[#{arg0}]-1 downto 0);

            impure function ~GENSYM[InitRomFromFile][1] (RomFileName : in string) return ~SYM[4] is
              FILE RomFile : text open read_mode is RomFileName;
              variable RomFileLine : line;
              variable ROM : ~SYM[4](0 to ~LIT[#{arg4}]-1);
            begin
              for i in ROM'range loop
                readline(RomFile,RomFileLine);
                read(RomFileLine,ROM(i));
              end loop;
              return ROM;
            end function;

            signal ~GENSYM[ROM][2] : ~SYM[4](0 to ~LIT[#{arg4}]-1) := ~SYM[1](~FILE[~LIT[#{arg5}]]);
            signal ~GENSYM[rd][3] : integer range 0 to ~LIT[#{arg4}]-1;
          begin
            ~SYM[3] <=to_integer(~VAR[rdI][#{arg6}](31 downto 0))
            -- pragma translate_off
                          mod ~LIT[#{arg4}]
            -- pragma translate_on
                          ;
            ~IF ~ISACTIVEENABLE[#{arg3}] ~THEN
            ~GENSYM[romFileSync][7] : process (~ARG[#{arg2}])
            begin
              if (~IF ~ACTIVEEDGE[Rising][#{arg2}] ~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{arg2}])) then
                if ~ARG[#{arg3}] then
                  ~RESULT <= to_stdlogicvector(~SYM[2](~SYM[3]));
                end if;
              end if;
            end process;~ELSE
            ~SYM[7] : process (~ARG[#{arg2}])
            begin
              if (~IF ~ACTIVEEDGE[Rising][#{arg2}] ~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{arg2}])) then
                ~RESULT <= to_stdlogicvector(~SYM[2](~SYM[3]));
              end if;
            end process;~FI
          end block;
          -- romFile end
    |]) #-}
