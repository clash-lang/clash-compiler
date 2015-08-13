{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeOperators    #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

BlockRAM primitives

= Using RAMs #usingrams#

We will show a rather elaborate example on how you can, and why you might want
to use 'blockRam's. We will build a \"small\" CPU+Memory+Program ROM where we
will slowly evolve to using blockRams. Note that the code is /not/ meant as a
de-facto standard on how to do CPU design in CλaSH.

We start with the definition of the Instructions, Register names and machine
codes:

@
{\-\# LANGUAGE RecordWildCards \#-\}
module CPU where

import CLaSH.Prelude
import qualified Data.List as L

type InstrAddr = Unsigned 8
type MemAddr   = Unsigned 5
type Value     = Signed 8

data Instruction
  = Compute Operator Reg Reg Reg
  | Branch Reg Value
  | Jump Value
  | Load MemAddr Reg
  | Store Reg MemAddr
  | Nop
  deriving (Eq,Show)

data Reg
  = Zero
  | PC
  | RegA
  | RegB
  | RegC
  | RegD
  | RegE
  deriving (Eq,Show,Enum)

data Operator = Add | Sub | Incr | Imm | CmpGt
  deriving (Eq,Show)

data MachCode
  = MachCode
  { inputX  :: Reg
  , inputY  :: Reg
  , result  :: Reg
  , aluCode :: Operator
  , ldReg   :: Reg
  , rdAddr  :: MemAddr
  , wrAddr  :: MemAddr
  , wrEn    :: Bool
  , jmpM    :: Maybe Value
  }

nullCode = MachCode { inputX = Zero, inputY = Zero, result = Zero, aluCode = Imm
                    , ldReg = Zero, wrAddr = 0, rdAddr = 0, wrEn = False
                    , jmpM = Nothing
                    }
@

Next we define the CPU and its ALU:

@
cpu :: Vec 7 Value          -- ^ Register bank
    -> (Value,Instruction)  -- ^ (Memory output, Current instruction)
    -> ( Vec 7 Value
       , (MemAddr,MemAddr,Bool,Value,InstrAddr)
       )
cpu regbank (memOut,instr) = (regbank',(rdAddr,wrAddr,wrEn,aluOut,fromIntegral ipntr))
  where
    -- Current instruction pointer
    ipntr = regbank '!!' PC

    -- Decoder
    (MachCode {..}) = case instr of
      Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
      Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
      Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
      Load a r             -> nullCode {ldReg=r,rdAddr=a}
      Store r a            -> nullCode {inputX=r,wrAddr=a,wrEn=True}
      Nop                  -> nullCode

    -- ALU
    regX   = regbank '!!' inputX
    regY   = regbank '!!' inputY
    aluOut = alu aluCode regX regY

    -- next instruction
    nextPC = case jmpM of
               Just a | aluOut /= 0 -> ipntr + a
               _                    -> ipntr + 1

    -- update registers
    regbank' = 'replace' Zero   0
             $ 'replace' PC     nextPC
             $ 'replace' result aluOut
             $ 'replace' ldReg  memOut
             $ regbank

alu Add   x y = x + y
alu Sub   x y = x - y
alu Incr  x _ = x + 1
alu Imm   x _ = x
alu CmpGt x y = if x > y then 1 else 0
@

We initially create a memory out of simple registers:

@
dataMem :: Signal MemAddr -- ^ Read address
        -> Signal MemAddr -- ^ Write address
        -> Signal Bool    -- ^ Write enable
        -> Signal Value   -- ^ data in
        -> Signal Value   -- ^ data out
dataMem wr rd en din = 'CLaSH.Prelude.Mealy.mealy' dataMemT ('replicate' d32 0) (bundle (wr,rd,en,din))
  where
    dataMemT mem (wr,rd,en,din) = (mem',dout)
      where
        dout = mem '!!' rd
        mem' | en        = 'replace' wr din mem
             | otherwise = mem
@

And then connect everything:

@
system :: KnownNat n => Vec n Instruction -> Signal Value
system instrs = memOut
  where
    memOut = dataMem wrAddr rdAddr wrEn aluOut
    (rdAddr,wrAddr,wrEn,aluOut,ipntr) = 'CLaSH.Prelude.Mealy.mealyB' cpu ('replicate' d7 0) (memOut,instr)
    instr  = 'CLaSH.Prelude.ROM.asyncRom' instrs '<$>' ipntr
@

Create a simple program that calculates the GCD of 4 and 6:

@
-- Compute GCD of 4 and 6
prog = -- 0 := 4
       Compute Incr Zero RegA RegA :>
       replicate d3 (Compute Incr RegA Zero RegA) ++
       Store RegA 0 :>
       -- 1 := 6
       Compute Incr Zero RegA RegA :>
       replicate d5 (Compute Incr RegA Zero RegA) ++
       Store RegA 1 :>
       -- A := 4
       Load 0 RegA :>
       -- B := 6
       Load 1 RegB :>
       -- start
       Compute CmpGt RegA RegB RegC :>
       Branch RegC 4 :>
       Compute CmpGt RegB RegA RegC :>
       Branch RegC 4 :>
       Jump 5 :>
       -- (a > b)
       Compute Sub RegA RegB RegA :>
       Jump (-6) :>
       -- (b > a)
       Compute Sub RegB RegA RegB :>
       Jump (-8) :>
       -- end
       Store RegA 2 :>
       Load 2 RegC :>
       Nil
@

And test our system:

@
__>>> sampleN 31 $ system prog__
[0,0,0,0,0,4,4,4,4,4,4,4,4,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2]
@

to see that our system indeed calculates that the GCD of 6 and 4 is 2.

=== Improvement 1: using @asyncRam@

As you can see, it's fairly straightforward to build a memory using registers
and read ('!!') and write ('replace') logic. This might however not result in
the most efficient hardware structure, especially when building an ASIC.

Instead it is preferable to use the 'CLaSH.Prelude.RAM.asyncRam' function which
has the potential to be translated to a more efficient structure:

@
system2 :: KnownNat n => Vec n Instruction -> Signal Value
system2 instrs = memOut
  where
    memOut = 'CLaSH.Prelude.RAM.asyncRam' d32 wrAddr rdAddr wrEn aluOut
    (rdAddr,wrAddr,wrEn,aluOut,ipntr) = 'mealyB' cpu ('replicate' d7 0) (memOut,instr)
    instr  = 'CLaSH.Prelude.ROM.asyncRom' instrs '<$>' ipntr
@

Again, we can simulate our system and see that it works. This time however,
we need to drop the first few output samples, because the initial content of an
'CLaSH.Prelude.RAM.asyncRam' is 'undefined', and consequently, the first few
output samples are also 'undefined'.

@
__>>> L.drop 5 $ sampleN 31 $ system2 prog__
[4,4,4,4,4,4,4,4,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2]
@

=== Improvement 2: using @blockRam@

Finally we get to using 'blockRam'. On FPGAs, 'CLaSH.Prelude.RAM.asyncRam' will
be implemented in terms of LUTs, and therefore take up logic resources. FPGAs
also have large(r) memory structures called /Block RAMs/, which are preferred,
especially as the memories we need for our application get bigger. The
'blockRam' function will be translated to such a /Block RAM/.

One important aspect of Block RAMs have a /synchronous/ read port, meaning that,
unlike the behaviour of 'CLaSH.Prelude.RAM.asyncRam', given a read address @r@
at time @t@, the value @v@ in the RAM at address @r@ is only available at time
@t+1@.

For us that means we need to change the design of our CPU. Right now, upon a
load instruction we generate a read address for the memory, and the value at
that read address is immediately available to be put in the register bank.
Because we will be using a BlockRAM, the value is delayed until the next cycle.
We hence need to also delay the register address to which the memory address
is loaded:

@
cpu2 :: (Vec 7 Value,Reg)    -- ^ (Register bank, Load reg addr)
     -> (Value,Instruction)  -- ^ (Memory output, Current instruction)
     -> ( (Vec 7 Value,Reg)
        , (MemAddr,MemAddr,Bool,Value,InstrAddr)
        )
cpu2 (regbank,ldRegD) (memOut,instr) = ((regbank',ldRegD'),(rdAddr,wrAddr,wrEn,aluOut,fromIntegral ipntr))
  where
    -- Current instruction pointer
    ipntr = regbank '!!' PC

    -- Decoder
    (MachCode {..}) = case instr of
      Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
      Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
      Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
      Load a r             -> nullCode {ldReg=r,rdAddr=a}
      Store r a            -> nullCode {inputX=r,wrAddr=a,wrEn=True}
      Nop                  -> nullCode

    -- ALU
    regX   = regbank '!!' inputX
    regY   = regbank '!!' inputY
    aluOut = alu aluCode regX regY

    -- next instruction
    nextPC = case jmpM of
               Just a | aluOut /= 0 -> ipntr + a
               _                    -> ipntr + 1

    -- update registers
    ldRegD'  = ldReg -- Delay the ldReg by 1 cycle
    regbank' = 'replace' Zero   0
             $ 'replace' PC     nextPC
             $ 'replace' result aluOut
             $ 'replace' ldRegD memOut
             $ regbank
@

We can now finally instantiate our system with a 'blockRam':

@
system3 :: KnownNat n => Vec n Instruction -> Signal Value
system3 instrs = memOut
  where
    memOut = 'blockRam' (replicate d32 0) wrAddr rdAddr wrEn aluOut
    (rdAddr,wrAddr,wrEn,aluOut,ipntr) = 'mealyB' cpu2 (('replicate' d7 0),Zero) (memOut,instr)
    instr  = 'CLaSH.Prelude.ROM.asyncRom' instrs '<$>' ipntr
@

We are, however, not done. We will also need to update our program. The reason
being that values that we try to load in our registers won't be loaded into the
register until the next cycle. This is a problem when the next instruction
immediately depended on this memory value. In our case, this was only the case
when the loaded the value @6@, which was stored at address @1@, into @RegB@.
Our updated program is thus:

@
prog2 = -- 0 := 4
       Compute Incr Zero RegA RegA :>
       replicate d3 (Compute Incr RegA Zero RegA) ++
       Store RegA 0 :>
       -- 1 := 6
       Compute Incr Zero RegA RegA :>
       replicate d5 (Compute Incr RegA Zero RegA) ++
       Store RegA 1 :>
       -- A := 4
       Load 0 RegA :>
       -- B := 6
       Load 1 RegB :>
       Nop :> -- Extra NOP
       -- start
       Compute CmpGt RegA RegB RegC :>
       Branch RegC 4 :>
       Compute CmpGt RegB RegA RegC :>
       Branch RegC 4 :>
       Jump 5 :>
       -- (a > b)
       Compute Sub RegA RegB RegA :>
       Jump (-6) :>
       -- (b > a)
       Compute Sub RegB RegA RegB :>
       Jump (-8) :>
       -- end
       Store RegA 2 :>
       Load 2 RegC :>
       Nil
@

When we simulate our system we see that it works. This time however,
we need to drop the first few sample, because the initial output of a
'blockRam' is 'undefined', and consequently, the first output sample is
also 'undefined'.

@
__>>> L.tail $ sampleN 33 $ system3 prog2__
[0,0,0,0,0,4,4,4,4,4,4,4,4,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2]
@

This concludes the short introduction to using 'blockRam'.

-}
module CLaSH.Prelude.BlockRam
  ( -- * BlockRAM synchronised to the system clock
    blockRam
  , blockRamPow2
    -- * BlockRAM synchronised to an arbitrary clock
  , blockRam'
  , blockRamPow2'
    -- * Internal
  , blockRam#
  )
where

import Control.Monad          (when)
import Control.Monad.ST.Lazy  (ST,runST)
import Data.Array.MArray.Safe (newListArray,readArray,writeArray)
import Data.Array.ST.Safe     (STArray)
import GHC.TypeLits           (KnownNat, type (^))

import CLaSH.Signal           (Signal)
import CLaSH.Signal.Explicit  (Signal', SClock, register', systemClock)
import CLaSH.Signal.Bundle    (bundle')
import CLaSH.Sized.Unsigned   (Unsigned)
import CLaSH.Sized.Vector     (Vec, maxIndex, toList)

{-# INLINE blockRam #-}
-- | Create a blockRAM with space for @n@ elements.
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- @
-- bram40 :: 'Signal' ('Unsigned' 6) -> Signal ('Unsigned' 6) -> 'Signal' Bool
--        -> 'Signal' 'CLaSH.Sized.BitVector.Bit' -> Signal 'CLaSH.Sized.BitVector.Bit'
-- bram40 = 'blockRam' ('CLaSH.Sized.Vector.replicate' d40 1)
-- @
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
blockRam :: (KnownNat n, Enum addr)
         => Vec n a     -- ^ Initial content of the BRAM, also
                        -- determines the size, @n@, of the BRAM.
                        --
                        -- __NB__: __MUST__ be a constant.
         -> Signal addr -- ^ Write address @w@
         -> Signal addr -- ^ Read address @r@
         -> Signal Bool -- ^ Write enable
         -> Signal a    -- ^ Value to write (at address @w@)
         -> Signal a
         -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
         -- cycle
blockRam = blockRam' systemClock

{-# INLINE blockRamPow2 #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- @
-- bram32 :: 'Signal' ('Unsigned' 5) -> Signal ('Unsigned' 5) -> 'Signal' Bool
--        -> 'Signal' 'CLaSH.Sized.BitVector.Bit' -> 'Signal' 'CLaSH.Sized.BitVector.Bit'
-- bram32 = 'blockRamPow2' ('CLaSH.Sized.Vector.replicate' d32 1)
-- @
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
blockRamPow2 :: (KnownNat (2^n), KnownNat n)
             => Vec (2^n) a         -- ^ Initial content of the BRAM, also
                                    -- determines the size, @2^n@, of the BRAM.
                                    --
                                    -- __NB__: __MUST__ be a constant.
             -> Signal (Unsigned n) -- ^ Write address @w@
             -> Signal (Unsigned n) -- ^ Read address @r@
             -> Signal Bool         -- ^ Write enable
             -> Signal a            -- ^ Value to write (at address @w@)
             -> Signal a
             -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
             -- cycle
blockRamPow2 = blockRam

{-# INLINE blockRam' #-}
-- | Create a blockRAM with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- @
-- type ClkA = Clk \"A\" 100
--
-- clkA100 :: SClock ClkA
-- clkA100 = 'CLaSH.Signal.Explicit.sclock'
--
-- bram40 :: 'Signal'' ClkA ('Unsigned' 6) -> 'Signal'' ClkA ('Unsigned' 6)
--        -> 'Signal'' ClkA Bool -> 'Signal'' ClkA 'CLaSH.Sized.BitVector.Bit' -> ClkA 'Signal'' 'CLaSH.Sized.BitVector.Bit'
-- bram40 = 'blockRam'' clkA100 ('CLaSH.Sized.Vector.replicate' d40 1)
-- @
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
blockRam' :: (KnownNat n, Enum addr)
          => SClock clk       -- ^ 'Clock' to synchronize to
          -> Vec n a          -- ^ Initial content of the BRAM, also
                              -- determines the size, @n@, of the BRAM.
                              --
                              -- __NB__: __MUST__ be a constant.
          -> Signal' clk addr -- ^ Write address @w@
          -> Signal' clk addr -- ^ Read address @r@
          -> Signal' clk Bool -- ^ Write enable
          -> Signal' clk a    -- ^ Value to write (at address @w@)
          -> Signal' clk a
          -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
          -- cycle
blockRam' clk content wr rd en din = blockRam# clk content (fromEnum <$> wr)
                                               (fromEnum <$> rd) en din

{-# INLINE blockRamPow2' #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- @
-- type ClkA = Clk \"A\" 100
--
-- clkA100 :: SClock ClkA
-- clkA100 = 'CLaSH.Signal.Explicit.sclock'
--
-- bram32 :: 'Signal'' ClkA ('Unsigned' 5) -> Signal' ClkA ('Unsigned' 5)
--        -> 'Signal'' ClkA Bool -> 'Signal'' ClkA 'CLaSH.Sized.BitVector.Bit' -> Signal' ClkA 'CLaSH.Sized.BitVector.Bit'
-- bram32 = 'blockRamPow2'' clkA100 ('CLaSH.Sized.Vector.replicate' d32 1)
-- @
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
blockRamPow2' :: (KnownNat n, KnownNat (2^n))
              => SClock clk               -- ^ 'Clock' to synchronize to
              -> Vec (2^n) a              -- ^ Initial content of the BRAM, also
                                          -- determines the size, @2^n@, of
                                          -- the BRAM.
                                          --
                                          -- __NB__: __MUST__ be a constant.
              -> Signal' clk (Unsigned n) -- ^ Write address @w@
              -> Signal' clk (Unsigned n) -- ^ Read address @r@
              -> Signal' clk Bool         -- ^ Write enable
              -> Signal' clk a            -- ^ Value to write (at address @w@)
              -> Signal' clk a
              -- ^ Value of the @blockRAM@ at address @r@ from the previous
              -- clock cycle
blockRamPow2' = blockRam'

{-# NOINLINE blockRam# #-}
-- | blockRAM primitive
blockRam# :: KnownNat n
          => SClock clk       -- ^ 'Clock' to synchronize to
          -> Vec n a          -- ^ Initial content of the BRAM, also
                              -- determines the size, @n@, of the BRAM.
                              --
                              -- __NB__: __MUST__ be a constant.
          -> Signal' clk Int  -- ^ Write address @w@
          -> Signal' clk Int  -- ^ Read address @r@
          -> Signal' clk Bool -- ^ Write enable
          -> Signal' clk a    -- ^ Value to write (at address @w@)
          -> Signal' clk a
          -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
          -- cycle
blockRam# clk content wr rd en din = register' clk undefined dout
  where
    szI  = fromInteger $ maxIndex content
    dout = runST $ do
      arr <- newListArray (0,szI) (toList content)
      traverse (ramT arr) (bundle' clk (wr,rd,en,din))

    ramT :: STArray s Int e -> (Int,Int,Bool,e) -> ST s e
    ramT ram (w,r,e,d) = do
      d' <- readArray ram r
      when e (writeArray ram w d)
      return d'
