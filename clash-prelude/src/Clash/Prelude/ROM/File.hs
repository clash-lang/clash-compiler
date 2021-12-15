{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

= Initializing a ROM with a data file #usingromfiles#

ROMs initialized with a data file. The BNF grammar for this data file is simple:

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

We can instantiate a synchronous ROM using the content of the above file like
so:

@
f :: (HiddenClock dom, HiddenEnable dom)
   => Signal dom (Unsigned 3)
   -> Signal dom (Unsigned 9)
f rd = 'Clash.Class.BitPack.unpack' '<$>' 'romFile' d7 \"memory.bin\" rd
@

And see that it works as expected:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ f (fromList [3..5])__
[10,11,12]
@

However, we can also interpret the same data as a tuple of a 6-bit unsigned
number, and a 3-bit signed number:

@
g :: (HiddenClock dom, HiddenEnable dom)
  => Signal dom (Unsigned 3)
  -> Signal dom (Unsigned 6,Signed 3)
g rd = 'Clash.Class.BitPack.unpack' '<$>' 'romFile' d7 \"memory.bin\" rd
@

And then we would see:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ g (fromList [3..5])__
[(1,2),(1,3)(1,-4)]
@
-}

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Prelude.ROM.File
  ( -- * Asynchronous ROM
    asyncRomFile
  , asyncRomFilePow2
    -- * Synchronous ROM synchronized to an arbitrary clock
  , romFile
  , romFilePow2
    -- * Internal
  , asyncRomFile#
  )
where

import           Data.Array                   (listArray,(!))
import           GHC.TypeLits                 (KnownNat)
import           System.IO.Unsafe             (unsafePerformIO)

import           Clash.Explicit.BlockRam.File (initMem)
import qualified Clash.Explicit.ROM.File      as E
import           Clash.Promoted.Nat           (SNat (..), pow2SNat, snatToNum)
import           Clash.Signal
import           Clash.Sized.BitVector        (BitVector)
import           Clash.Sized.Unsigned         (Unsigned)

-- | An asynchronous/combinational ROM with space for @n@ elements
--
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
-- * See "Clash.Prelude.ROM.File#usingromfiles" for more information on how
-- to instantiate a ROM with the contents of a data file.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
-- * When you notice that 'asyncRomFile' is significantly slowing down your
-- simulation, give it a /monomorphic/ type signature. So instead of leaving
-- the type to be inferred:
--
--     @
--     myRomData = asyncRomFile d512 "memory.bin"
--     @
--
--     or giving it a /polymorphic/ type signature:
--
--     @
--     myRomData :: Enum addr => addr -> BitVector 16
--     myRomData = asyncRomFile d512 "memory.bin"
--     @
--
--     you __should__ give it a /monomorphic/ type signature:
--
--     @
--     myRomData :: Unsigned 9 -> BitVector 16
--     myRomData = asyncRomFile d512 "memory.bin"
--     @
asyncRomFile
  :: (KnownNat m, Enum addr)
  => SNat n
  -- ^ Size of the ROM
  -> FilePath
  -- ^ File describing the content of the ROM
  -> addr
  -- ^ Read address @rd@
  -> BitVector m
  -- ^ The value of the ROM at address @rd@
asyncRomFile sz file = asyncRomFile# sz file . fromEnum
-- Leave 'asyncRomFile#' eta-reduced, see Note [Eta-reduction and unsafePerformIO initMem]
{-# INLINE asyncRomFile #-}

-- Note [Eta-reduction and unsafePerformIO initMem]
--
-- The 'initMem' function initializes a @[BitVector n]@ from file. Ideally,
-- we want this IO action to happen only once. When we call 'unsafePerformIO'
-- on @initMem file@, it becomes a thunk in that function, so is hence evaluated
-- only once. However, me must ensure that any code calling using of the
-- @unsafePerformIO (initMem file)@ thunk also becomes a thunk. We do this by
-- eta-reducing function where needed so that a thunk is returned.
--
-- For example, instead of writing:
--
-- > asyncRomFile# sz file rd = (content ! rd)
-- >   where
-- >     mem = unsafePerformIO (initMem file)
-- >     content = listArray (0,szI-1) mem
-- >     szI     = snatToNum sz
--
-- We write:
--
-- > asyncRomFile# sz file = (content !)
-- >   where
-- >     mem     = unsafePerformIO (initMem file)
-- >     content = listArray (0,szI-1) mem
-- >     szI     = snatToNum sz
--
-- Where instead of returning the BitVector defined by @(content ! rd)@, we
-- return the function (thunk) @(content !)@.

-- | An asynchronous/combinational ROM with space for 2^@n@ elements
--
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
-- * See "Clash.Prelude.ROM.File#usingromfiles" for more information on how
-- to instantiate a ROM with the contents of a data file.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
-- * When you notice that 'asyncRomFilePow2' is significantly slowing down your
-- simulation, give it a /monomorphic/ type signature. So instead of leaving the
-- type to be inferred:
--
--     @
--     myRomData = asyncRomFilePow2 "memory.bin"
--     @
--
--     you __should__ give it a /monomorphic/ type signature:
--
--     @
--     myRomData :: Unsigned 9 -> BitVector 16
--     myRomData = asyncRomFilePow2 "memory.bin"
--     @
asyncRomFilePow2
  :: forall n m
   . (KnownNat m, KnownNat n)
  => FilePath
  -- ^ File describing the content of the ROM
  -> Unsigned n
  -- ^ Read address @rd@
  -> BitVector m
  -- ^ The value of the ROM at address @rd@
asyncRomFilePow2 = asyncRomFile (pow2SNat (SNat @n))
{-# INLINE asyncRomFilePow2 #-}

-- | asyncROMFile primitive
asyncRomFile#
  :: KnownNat m
  => SNat n
  -- ^ Size of the ROM
  -> FilePath
  -- ^ File describing the content of the ROM
  -> Int
  -- ^ Read address @rd@
  -> BitVector m
  -- ^ The value of the ROM at address @rd@
asyncRomFile# sz file = (content !) -- Leave "(content !)" eta-reduced, see
  where                             -- Note [Eta-reduction and unsafePerformIO initMem]
    mem     = unsafePerformIO (initMem file)
    content = listArray (0,szI-1) mem
    szI     = snatToNum sz
{-# NOINLINE asyncRomFile# #-}

-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
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
-- * See "Clash.Prelude.ROM.File#usingromfiles" for more information on how
-- to instantiate a ROM with the contents of a data file.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
romFile
  :: ( KnownNat m
     , KnownNat n
     , HiddenClock dom
     , HiddenEnable dom
     , Enum addr
     )
  => SNat n
  -- ^ Size of the ROM
  -> FilePath
  -- ^ File describing the content of the ROM
  -> Signal dom addr
  -- ^ Read address @rd@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFile = hideEnable (hideClock E.romFile)
{-# INLINE romFile #-}

-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
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
-- * See "Clash.Prelude.ROM.File#usingromfiles" for more information on how
-- to instantiate a ROM with the contents of a data file.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
romFilePow2
  :: forall n m dom
   . ( KnownNat m
     , KnownNat n
     , HiddenClock dom
     , HiddenEnable dom
     )
  => FilePath
  -- ^ File describing the content of the ROM
  -> Signal dom (Unsigned n)
  -- ^ Read address @rd@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFilePow2 = hideEnable (hideClock E.romFilePow2)
{-# INLINE romFilePow2 #-}
