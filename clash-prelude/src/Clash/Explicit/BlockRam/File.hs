{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

= Initializing a BlockRAM with a data file #usingramfiles#

BlockRAM primitives that can be initialized with a data file. The BNF grammar
for this data file is simple:

@
FILE = LINE+
LINE = BIT+
BIT  = '0'
     | '1'
@

Consecutive @LINE@s correspond to consecutive memory addresses starting at @0@.
For example, a data file @memory.bin@ containing the 9-bit unsigned number
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

We can instantiate a BlockRAM using the content of the above file like so:

@
f :: Clock  dom
  -> Enable dom
  -> Signal dom (Unsigned 3)
  -> Signal dom (Unsigned 9)
f clk ena rd = 'Clash.Class.BitPack.unpack' '<$>' 'blockRamFile' clk ena d7 \"memory.bin\" rd (signal Nothing)
@

In the example above, we basically treat the BlockRAM as an synchronous ROM.
We can see that it works as expected:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ f systemClockGen enableGen (fromList [3..5])__
[10,11,12]
@

However, we can also interpret the same data as a tuple of a 6-bit unsigned
number, and a 3-bit signed number:

@
g :: Clock  dom
  -> Enable dom
  -> Signal dom (Unsigned 3)
  -> Signal dom (Unsigned 6,Signed 3)
g clk ena rd = 'Clash.Class.BitPack.unpack' '<$>' 'blockRamFile' clk ena d7 \"memory.bin\" rd (signal Nothing)
@

And then we would see:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ g systemClockGen enableGen (fromList [3..5])__
[(1,2),(1,3)(1,-4)]
@

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

module Clash.Explicit.BlockRam.File
  ( -- * BlockRAM synchronized to an arbitrary clock
    blockRamFile
  , blockRamFilePow2
    -- * Internal
  , blockRamFile#
  , initMem
  )
where

import Data.Char             (digitToInt)
import Data.Maybe            (isJust, listToMaybe)
import qualified Data.Sequence as Seq
import GHC.Stack             (HasCallStack, withFrozenCallStack)
import GHC.TypeLits          (KnownNat)
import Numeric               (readInt)
import System.IO.Unsafe      (unsafePerformIO)

import Clash.Promoted.Nat    (SNat (..), pow2SNat)
import Clash.Sized.BitVector (BitVector)
import Clash.Signal.Internal
  (Clock(..), Signal (..), Enable, KnownDomain, fromEnable, (.&&.))
import Clash.Signal.Bundle   (unbundle)
import Clash.Sized.Unsigned  (Unsigned)
import Clash.XException      (errorX, maybeIsX, seqX, fromJustX)


-- | Create a blockRAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
--     @
--                    | VHDL     | Verilog  | SystemVerilog |
--     ===============+==========+==========+===============+
--     Altera/Quartus | Broken   | Works    | Works         |
--     Xilinx/ISE     | Works    | Works    | Works         |
--     ASIC           | Untested | Untested | Untested      |
--     ===============+==========+==========+===============+
--     @
--
-- Additional helpful information:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'Clash.Explicit.BlockRam.readNew' for obtaining write-before-read semantics like this: @'Clash.Explicit.BlockRam.readNew' clk rst en (blockRamFilePow2' clk en file) rd wrM@.
-- * See "Clash.Explicit.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "Clash.Explicit.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFilePow2
  :: forall dom n m
   . (KnownDomain dom, KnownNat m, KnownNat n, HasCallStack)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> FilePath
  -- ^ File describing the initial content of the blockRAM
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom (Maybe (Unsigned n, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal dom (BitVector m)
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRamFilePow2 = \clk en file rd wrM -> withFrozenCallStack
  (blockRamFile clk en (pow2SNat (SNat @n)) file rd wrM)
{-# INLINE blockRamFilePow2 #-}

-- | Create a blockRAM with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
--     @
--                    | VHDL     | Verilog  | SystemVerilog |
--     ===============+==========+==========+===============+
--     Altera/Quartus | Broken   | Works    | Works         |
--     Xilinx/ISE     | Works    | Works    | Works         |
--     ASIC           | Untested | Untested | Untested      |
--     ===============+==========+==========+===============+
--     @
--
-- Additional helpful information:
--
-- * See "Clash.Explicit.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'Clash.Explicit.BlockRam.readNew' for obtaining write-before-read semantics like this: @'Clash.Explicit.BlockRam.readNew' clk rst en ('blockRamFile' clk en size file) rd wrM@.
-- * See "Clash.Explicit.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFile
  :: (KnownDomain dom, KnownNat m, Enum addr, HasCallStack)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> SNat n
  -- ^ Size of the blockRAM
  -> FilePath
  -- ^ File describing the initial content of the blockRAM
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal dom (BitVector m)
  -- ^ Value of the @blockRAM@ at address @r@ from the previous
  -- clock cycle
blockRamFile = \clk gen sz file rd wrM ->
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJustX <$> wrM)
  in  withFrozenCallStack
      (blockRamFile# clk gen sz file (fromEnum <$> rd) en (fromEnum <$> wr) din)
{-# INLINE blockRamFile #-}

-- | blockRamFile primitive
blockRamFile#
  :: forall m dom n
   . (KnownDomain dom, KnownNat m, HasCallStack)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> SNat n
  -- ^ Size of the blockRAM
  -> FilePath
  -- ^ File describing the initial content of the blockRAM
  -> Signal dom Int
  -- ^ Read address @r@
  -> Signal dom Bool
  -- ^ Write enable
  -> Signal dom Int
  -- ^ Write address @w@
  -> Signal dom (BitVector m)
  -- ^ Value to write (at address @w@)
  -> Signal dom (BitVector m)
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRamFile# (Clock _) ena !_sz file rd wen =
  go
    ramI
    (withFrozenCallStack (errorX "blockRamFile#: intial value undefined"))
    (fromEnable ena)
    rd
    (fromEnable ena .&&. wen)
  where
    -- clock enable
    go
      :: Seq.Seq (BitVector m)
      -> BitVector m
      -> Signal dom Bool
      -> Signal dom Int
      -> Signal dom Bool
      -> Signal dom Int
      -> Signal dom (BitVector m)
      -> Signal dom (BitVector m)
    go !ram o (re :- res) (r :- rs) (e :- en) (w :- wr) (d :- din) =
      let ram' = upd ram e (fromEnum w) d
          o'   = if re then ram `Seq.index` r else o
      in  o `seqX` o :- go ram' o' res rs en wr din

    upd ram we waddr d = case maybeIsX we of
      Nothing -> case maybeIsX waddr of
        Nothing -> fmap (const (seq waddr d)) ram
        Just wa -> Seq.update wa d ram
      Just True -> case maybeIsX waddr of
        Nothing -> fmap (const (seq waddr d)) ram
        Just wa -> Seq.update wa d ram
      _ -> ram

    content = unsafePerformIO (initMem file)

    ramI :: Seq.Seq (BitVector m)
    ramI = Seq.fromList content
{-# NOINLINE blockRamFile# #-}

-- | __NB:__ Not synthesizable
initMem :: KnownNat n => FilePath -> IO [BitVector n]
initMem = fmap (map parseBV . lines) . readFile
  where
    parseBV s = case parseBV' s of
                  Just i  -> fromInteger i
                  Nothing -> error ("Failed to parse: " ++ s)
    parseBV' = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
{-# NOINLINE initMem #-}
