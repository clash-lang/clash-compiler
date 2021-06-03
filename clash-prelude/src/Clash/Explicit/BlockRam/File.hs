{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.,
                  2019     , Myrtle Software Ltd,
                  2021     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

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

Such a file can be produced with 'memFile':

@
writeFile "memory.bin" (memFile Nothing [7 :: Unsigned 9 .. 13])
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
    -- * Producing files
  , memFile
    -- * Internal
  , blockRamFile#
  , initMem
  )
where

import Data.Bits             ((.&.), (.|.), shiftL, xor)
import Data.Char             (digitToInt)
import Data.Maybe            (isJust, listToMaybe)
import qualified Data.Sequence as Seq
import GHC.Stack             (HasCallStack, withFrozenCallStack)
import GHC.TypeLits          (KnownNat)
import Numeric               (readInt)
import System.IO.Unsafe      (unsafePerformIO)

import Clash.Class.BitPack   (BitPack, BitSize, pack)
import Clash.Promoted.Nat    (SNat (..), pow2SNat, natToNum)
import Clash.Sized.Internal.BitVector (Bit(..), BitVector(..))
import Clash.Signal.Internal
  (Clock(..), Signal (..), Enable, KnownDomain, fromEnable, (.&&.))
import Clash.Signal.Bundle   (unbundle)
import Clash.Sized.Unsigned  (Unsigned)
import Clash.XException      (errorX, maybeIsX, seqX, fromJustX)

-- $setup
-- >>> import Clash.Prelude.BlockRam.File


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
-- * See 'memFile' for creating a data file with Clash.
-- * See "Clash.Explicit.Fixed#creatingdatafiles" for more ideas on how to
-- create your own data files.
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
-- * See 'memFile' for creating a data file with Clash.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for more ideas on how to create
-- your own data files.
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

-- | Convert data to the String contents of a memory file.
--
-- * __NB:__ Not synthesizable
-- * The following document the several ways to instantiate components with
-- files:
--
--     * "Clash.Prelude.BlockRam.File#usingramfiles"
--     * "Clash.Prelude.ROM.File#usingromfiles"
--     * "Clash.Explicit.BlockRam.File#usingramfiles"
--     * "Clash.Explicit.ROM.File#usingromfiles"
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" for more ideas on how to create
-- your own data files.
--
-- = Example
--
-- The @Maybe@ datatype has don't care bits, where the actual value does not
-- matter. But the bits need a defined value in the memory. Either 0 or 1 can be
-- used, and both are valid representations of the data.
--
-- >>> let es = [ Nothing, Just (7 :: Unsigned 8), Just 8]
-- >>> mapM_ (putStrLn . show . pack) es
-- 0b0_...._....
-- 0b1_0000_0111
-- 0b1_0000_1000
-- >>> putStr (memFile (Just 0) es)
-- 000000000
-- 100000111
-- 100001000
-- >>> putStr (memFile (Just 1) es)
-- 011111111
-- 100000111
-- 100001000
--
memFile
  :: forall a f
   . ( BitPack a
     , Foldable f
     , HasCallStack)
  => Maybe Bit
  -- ^ Value to map don't care bits to. Nothing means throwing an error on
  -- don't care bits.
  -> f a
  -- ^ Values to convert.
  -> String
  -- ^ Contents of the memory file.
memFile care = foldr (\e -> showsBV $ pack e) ""
 where
  showsBV :: BitVector (BitSize a) -> String -> String
  showsBV (BV mask val) s =
    if n == 0 then
      '0' : '\n' : s
    else
      case care of
        Just (Bit 0 0) -> go n (val .&. (mask `xor` fullMask)) ('\n' : s)
        Just (Bit 0 1)  -> go n (val .|. mask) ('\n' : s)
        _ -> if mask /= 0 then
               err
             else
               go n val ('\n' : s)
   where
    n = natToNum @(BitSize a) @Int
    fullMask = (1 `shiftL` n) - 1
    err = withFrozenCallStack $ error $
            "memFile: cannot convert don't-care values. "
            ++ "Please specify mapping to definite value."
    go 0  _ s0 = s0
    go n0 v s0 =
      let (!v0, !vBit) = quotRem v 2
      in if vBit == 0 then
           go (n0 - 1) v0 $ '0' : s0
         else
           go (n0 - 1) v0 $ '1' : s0

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
