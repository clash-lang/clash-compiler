{-|
Copyright  :  (C) 2013-2016, University of Twente
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
{\-\# LANGUAGE RecordWildCards, TupleSections \#-\}
module CPU where

import CLaSH.Prelude

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
  , wrAddrM :: Maybe MemAddr
  , jmpM    :: Maybe Value
  }

nullCode = MachCode { inputX = Zero, inputY = Zero, result = Zero, aluCode = Imm
                    , ldReg = Zero, rdAddr = 0, wrAddrM = Nothing
                    , jmpM = Nothing
                    }
@

Next we define the CPU and its ALU:

@
cpu :: Vec 7 Value          -- ^ Register bank
    -> (Value,Instruction)  -- ^ (Memory output, Current instruction)
    -> ( Vec 7 Value
       , (MemAddr, Maybe (MemAddr,Value), InstrAddr)
       )
cpu regbank (memOut,instr) = (regbank',(rdAddr,(,aluOut) '<$>' wrAddrM,fromIntegral ipntr))
  where
    -- Current instruction pointer
    ipntr = regbank '!!' PC

    -- Decoder
    (MachCode {..}) = case instr of
      Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
      Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
      Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
      Load a r             -> nullCode {ldReg=r,rdAddr=a}
      Store r a            -> nullCode {inputX=r,wrAddrM=Just a}
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
dataMem :: Signal MemAddr                 -- ^ Read address
        -> Signal (Maybe (MemAddr,Value)) -- ^ (write address, data in)
        -> Signal Value                   -- ^ data out
dataMem rd wrM = 'CLaSH.Prelude.Mealy.mealy' dataMemT ('replicate' d32 0) (bundle (rd,wrM))
  where
    dataMemT mem (rd,wrM) = (mem',dout)
      where
        dout = mem '!!' rd
        mem' = case wrM of
                 Just (wr,din) -> 'replace' wr din mem
                 _ -> mem
@

And then connect everything:

@
system :: KnownNat n => Vec n Instruction -> Signal Value
system instrs = memOut
  where
    memOut = dataMem rdAddr dout
    (rdAddr,dout,ipntr) = 'CLaSH.Prelude.Mealy.mealyB' cpu ('replicate' d7 0) (memOut,instr)
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
>>> sampleN 31 $ system prog
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
    memOut = 'CLaSH.Prelude.RAM.asyncRam' d32 rdAddr dout
    (rdAddr,dout,ipntr) = 'mealyB' cpu ('replicate' d7 0) (memOut,instr)
    instr  = 'CLaSH.Prelude.ROM.asyncRom' instrs '<$>' ipntr
@

Again, we can simulate our system and see that it works. This time however,
we need to disregard the first few output samples, because the initial content of an
'CLaSH.Prelude.RAM.asyncRam' is 'undefined', and consequently, the first few
output samples are also 'undefined'. We use the utility function 'printX' to conveniently
filter out the undefinedness and replace it with the string "X" in the few leading outputs.

@
>>> printX $ sampleN 31 $ system2 prog
[X,X,X,X,X,4,4,4,4,4,4,4,4,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2]

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
        , (MemAddr, Maybe (MemAddr,Value), InstrAddr)
        )
cpu2 (regbank,ldRegD) (memOut,instr) = ((regbank',ldRegD'),(rdAddr,(,aluOut) '<$>' wrAddrM,fromIntegral ipntr))
  where
    -- Current instruction pointer
    ipntr = regbank '!!' PC

    -- Decoder
    (MachCode {..}) = case instr of
      Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
      Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
      Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
      Load a r             -> nullCode {ldReg=r,rdAddr=a}
      Store r a            -> nullCode {inputX=r,wrAddrM=Just a}
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
    memOut = 'blockRam' (replicate d32 0) rdAddr dout
    (rdAddr,dout,ipntr) = 'mealyB' cpu2 (('replicate' d7 0),Zero) (memOut,instr)
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

When we simulate our system we see that it works. This time again,
we need to disregard the first sample, because the initial output of a
'blockRam' is 'undefined'. We use the utility function 'printX' to conveniently
filter out the undefinedness and replace it with the string "X".

@
>>> printX $ sampleN 33 $ system3 prog2
[X,0,0,0,0,0,4,4,4,4,4,4,4,4,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2]

@

This concludes the short introduction to using 'blockRam'.

-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

module CLaSH.Prelude.BlockRam
  ( -- * BlockRAM synchronised to the system clock
    blockRam
  , blockRamPow2
    -- * BlockRAM synchronised to an arbitrary clock
  , blockRam'
  , blockRamPow2'
    -- * Read/Write conflict resolution
  , readNew
  , readNew'
    -- * Internal
  , blockRam#
  )
where

import Data.Maybe             (fromJust, isJust)
import qualified Data.Vector  as V
import GHC.TypeLits           (KnownNat, type (^))
import Prelude                hiding (length)

import CLaSH.Signal           (Signal, mux)
import CLaSH.Signal.Explicit  (SClock, register', systemClock)
import CLaSH.Signal.Internal  (Signal' (..))
import CLaSH.Signal.Bundle    (unbundle)
import CLaSH.Sized.Unsigned   (Unsigned)
import CLaSH.Sized.Vector     (Vec, toList)
import CLaSH.XException       (errorX)

{- $setup
>>> import CLaSH.Prelude as C
>>> import qualified Data.List as L
>>> :set -XDataKinds -XRecordWildCards -XTupleSections
>>> type InstrAddr = Unsigned 8
>>> type MemAddr = Unsigned 5
>>> type Value = Signed 8
>>> :{
data Reg
  = Zero
  | PC
  | RegA
  | RegB
  | RegC
  | RegD
  | RegE
  deriving (Eq,Show,Enum)
:}

>>> :{
data Operator = Add | Sub | Incr | Imm | CmpGt
  deriving (Eq,Show)
:}

>>> :{
data Instruction
  = Compute Operator Reg Reg Reg
  | Branch Reg Value
  | Jump Value
  | Load MemAddr Reg
  | Store Reg MemAddr
  | Nop
  deriving (Eq,Show)
:}

>>> :{
data MachCode
  = MachCode
  { inputX  :: Reg
  , inputY  :: Reg
  , result  :: Reg
  , aluCode :: Operator
  , ldReg   :: Reg
  , rdAddr  :: MemAddr
  , wrAddrM :: Maybe MemAddr
  , jmpM    :: Maybe Value
  }
:}

>>> :{
nullCode = MachCode { inputX = Zero, inputY = Zero, result = Zero, aluCode = Imm
                    , ldReg = Zero, rdAddr = 0, wrAddrM = Nothing
                    , jmpM = Nothing
                    }
:}

>>> :{
alu Add   x y = x + y
alu Sub   x y = x - y
alu Incr  x _ = x + 1
alu Imm   x _ = x
alu CmpGt x y = if x > y then 1 else 0
:}

>>> :{
cpu :: Vec 7 Value          -- ^ Register bank
    -> (Value,Instruction)  -- ^ (Memory output, Current instruction)
    -> ( Vec 7 Value
       , (MemAddr,Maybe (MemAddr,Value),InstrAddr)
       )
cpu regbank (memOut,instr) = (regbank',(rdAddr,(,aluOut) <$> wrAddrM,fromIntegral ipntr))
  where
    -- Current instruction pointer
    ipntr = regbank C.!! PC
    -- Decoder
    (MachCode {..}) = case instr of
      Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
      Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
      Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
      Load a r             -> nullCode {ldReg=r,rdAddr=a}
      Store r a            -> nullCode {inputX=r,wrAddrM=Just a}
      Nop                  -> nullCode
    -- ALU
    regX   = regbank C.!! inputX
    regY   = regbank C.!! inputY
    aluOut = alu aluCode regX regY
    -- next instruction
    nextPC = case jmpM of
               Just a | aluOut /= 0 -> ipntr + a
               _                    -> ipntr + 1
    -- update registers
    regbank' = replace Zero   0
             $ replace PC     nextPC
             $ replace result aluOut
             $ replace ldReg  memOut
             $ regbank
:}

>>> :{
dataMem :: Signal MemAddr
        -> Signal (Maybe (MemAddr,Value))
        -> Signal Value
dataMem rd wrM = mealy dataMemT (C.replicate d32 0) (bundle (rd,wrM))
  where
    dataMemT mem (rd,wrM) = (mem',dout)
      where
        dout = mem C.!! rd
        mem' = case wrM of
                 Just (wr,din) -> replace wr din mem
                 Nothing       -> mem
:}

>>> :{
system :: KnownNat n => Vec n Instruction -> Signal Value
system instrs = memOut
  where
    memOut = dataMem rdAddr dout
    (rdAddr,dout,ipntr) = mealyB cpu (C.replicate d7 0) (memOut,instr)
    instr  = asyncRom instrs <$> ipntr
:}

>>> :{
-- Compute GCD of 4 and 6
prog = -- 0 := 4
       Compute Incr Zero RegA RegA :>
       C.replicate d3 (Compute Incr RegA Zero RegA) C.++
       Store RegA 0 :>
       -- 1 := 6
       Compute Incr Zero RegA RegA :>
       C.replicate d5 (Compute Incr RegA Zero RegA) C.++
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
:}

>>> :{
system2 :: KnownNat n => Vec n Instruction -> Signal Value
system2 instrs = memOut
  where
    memOut = asyncRam d32 rdAddr dout
    (rdAddr,dout,ipntr) = mealyB cpu (C.replicate d7 0) (memOut,instr)
    instr  = asyncRom instrs <$> ipntr
:}

>>> :{
cpu2 :: (Vec 7 Value,Reg)    -- ^ (Register bank, Load reg addr)
     -> (Value,Instruction)  -- ^ (Memory output, Current instruction)
     -> ( (Vec 7 Value,Reg)
        , (MemAddr,Maybe (MemAddr,Value),InstrAddr)
        )
cpu2 (regbank,ldRegD) (memOut,instr) = ((regbank',ldRegD'),(rdAddr,(,aluOut) <$> wrAddrM,fromIntegral ipntr))
  where
    -- Current instruction pointer
    ipntr = regbank C.!! PC
    -- Decoder
    (MachCode {..}) = case instr of
      Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
      Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
      Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
      Load a r             -> nullCode {ldReg=r,rdAddr=a}
      Store r a            -> nullCode {inputX=r,wrAddrM=Just a}
      Nop                  -> nullCode
    -- ALU
    regX   = regbank C.!! inputX
    regY   = regbank C.!! inputY
    aluOut = alu aluCode regX regY
    -- next instruction
    nextPC = case jmpM of
               Just a | aluOut /= 0 -> ipntr + a
               _                    -> ipntr + 1
    -- update registers
    ldRegD'  = ldReg -- Delay the ldReg by 1 cycle
    regbank' = replace Zero   0
             $ replace PC     nextPC
             $ replace result aluOut
             $ replace ldRegD memOut
             $ regbank
:}

>>> :{
system3 :: KnownNat n => Vec n Instruction -> Signal Value
system3 instrs = memOut
  where
    memOut = blockRam (C.replicate d32 0) rdAddr dout
    (rdAddr,dout,ipntr) = mealyB cpu2 ((C.replicate d7 0),Zero) (memOut,instr)
    instr  = asyncRom instrs <$> ipntr
:}

>>> :{
prog2 = -- 0 := 4
       Compute Incr Zero RegA RegA :>
       C.replicate d3 (Compute Incr RegA Zero RegA) C.++
       Store RegA 0 :>
       -- 1 := 6
       Compute Incr Zero RegA RegA :>
       C.replicate d5 (Compute Incr RegA Zero RegA) C.++
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
:}

-}

{-# INLINE blockRam #-}
-- | Create a blockRAM with space for @n@ elements.
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- @
-- bram40 :: 'Signal' ('Unsigned' 6)
--        -> 'Signal' (Maybe ('Unsigned' 6, 'CLaSH.Sized.BitVector.Bit'))
--        -> 'Signal' 'CLaSH.Sized.BitVector.Bit'
-- bram40 = 'blockRam' ('CLaSH.Sized.Vector.replicate' d40 1)
-- @
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew' for obtaining write-before-read semantics like this: @readNew (blockRam inits) rd wrM@.
blockRam :: Enum addr
         => Vec n a     -- ^ Initial content of the BRAM, also
                        -- determines the size, @n@, of the BRAM.
                        --
                        -- __NB__: __MUST__ be a constant.
         -> Signal addr -- ^ Read address @r@
         -> Signal (Maybe (addr, a))
          -- ^ (write address @w@, value to write)
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
-- bram32 :: 'Signal' ('Unsigned' 5)
--        -> 'Signal' (Maybe ('Unsigned' 5, 'CLaSH.Sized.BitVector.Bit'))
--        -> 'Signal' 'CLaSH.Sized.BitVector.Bit'
-- bram32 = 'blockRamPow2' ('CLaSH.Sized.Vector.replicate' d32 1)
-- @
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew' for obtaining write-before-read semantics like this: @readNew (blockRamPow2 inits) rd wrM@.
blockRamPow2 :: KnownNat n
             => Vec (2^n) a         -- ^ Initial content of the BRAM, also
                                    -- determines the size, @2^n@, of the BRAM.
                                    --
                                    -- __NB__: __MUST__ be a constant.
             -> Signal (Unsigned n) -- ^ Read address @r@
             -> Signal (Maybe (Unsigned n, a))
             -- ^ (write address @w@, value to write)
             -> Signal a
             -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
             -- cycle
blockRamPow2 = blockRamPow2' systemClock

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
-- bram40 :: 'Signal'' ClkA ('Unsigned' 6)
--        -> 'Signal'' ClkA (Maybe ('Unsigned' 6, 'CLaSH.Sized.BitVector.Bit'))
--        -> 'Signal'' ClkA 'CLaSH.Sized.BitVector.Bit'
-- bram40 = 'blockRam'' clkA100 ('CLaSH.Sized.Vector.replicate' d40 1)
-- @
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew'' for obtaining write-before-read semantics like this: @readNew' clk (blockRam' clk inits) rd wrM@.
blockRam' :: Enum addr
          => SClock clk       -- ^ 'Clock' to synchronize to
          -> Vec n a          -- ^ Initial content of the BRAM, also
                              -- determines the size, @n@, of the BRAM.
                              --
                              -- __NB__: __MUST__ be a constant.
          -> Signal' clk addr -- ^ Read address @r@
          -> Signal' clk (Maybe (addr, a))
          -- ^ (write address @w@, value to write)
          -> Signal' clk a
          -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
          -- cycle
blockRam' clk content rd wrM =
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJust <$> wrM)
  in  blockRam# clk content (fromEnum <$> rd) en (fromEnum <$> wr) din

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
-- bram32 :: 'Signal'' ClkA ('Unsigned' 5)
--        -> 'Signal'' ClkA (Maybe ('Unsigned' 5, 'CLaSH.Sized.BitVector.Bit'))
--        -> 'Signal'' ClkA 'CLaSH.Sized.BitVector.Bit'
-- bram32 = 'blockRamPow2'' clkA100 ('CLaSH.Sized.Vector.replicate' d32 1)
-- @
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew'' for obtaining write-before-read semantics like this: @readNew' clk (blockRamPow2' clk inits) rd wrM@.
blockRamPow2' :: KnownNat n
              => SClock clk               -- ^ 'Clock' to synchronize to
              -> Vec (2^n) a              -- ^ Initial content of the BRAM, also
                                          -- determines the size, @2^n@, of
                                          -- the BRAM.
                                          --
                                          -- __NB__: __MUST__ be a constant.
              -> Signal' clk (Unsigned n) -- ^ Read address @r@
              -> Signal' clk (Maybe (Unsigned n, a))
              -- ^ (Write address @w@, value to write)
              -> Signal' clk a
              -- ^ Value of the @blockRAM@ at address @r@ from the previous
              -- clock cycle
blockRamPow2' = blockRam'

-- | blockRAM primitive
blockRam# :: SClock clk       -- ^ 'Clock' to synchronize to
          -> Vec n a          -- ^ Initial content of the BRAM, also
                              -- determines the size, @n@, of the BRAM.
                              --
                              -- __NB__: __MUST__ be a constant.
          -> Signal' clk Int  -- ^ Read address @r@
          -> Signal' clk Bool -- ^ Write enable
          -> Signal' clk Int  -- ^ Write address @w@
          -> Signal' clk a    -- ^ Value to write (at address @w@)
          -> Signal' clk a
          -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
          -- cycle
blockRam# _clk content =
    go (V.fromList (toList content)) (errorX "blockRam#: intial value undefined")
  where
    go !ram o (r :- rs) (e :- en) (w :- wr) (d :- din) =
      let ram' = upd ram e w d
          o'   = ram V.! r
      in  o :- go ram' o' rs en wr din

    upd ram True  addr d = ram V.// [(addr,d)]
    upd ram False _    _ = ram
{-# NOINLINE blockRam# #-}

-- | Create read-after-write blockRAM from a read-before-write one (synchronised to specified clock)
--
readNew' :: Eq addr
         => SClock clk
         -> (Signal' clk addr -> Signal' clk (Maybe (addr, a)) -> Signal' clk a)
         -- ^ The @ram@ component
         -> Signal' clk addr              -- ^ Read address @r@
         -> Signal' clk (Maybe (addr, a)) -- ^ (Write address @w@, value to write)
         -> Signal' clk a
         -- ^ Value of the @ram@ at address @r@ from the previous clock
         -- cycle
readNew' clk ram rdAddr wrM = mux wasSame wasWritten $ ram rdAddr wrM
  where readNewT rd (Just (wr, wrdata)) = (wr == rd, wrdata)
        readNewT _  Nothing             = (False   , undefined)

        (wasSame,wasWritten) = unbundle (register' clk (False,undefined)
                                                       (readNewT <$> rdAddr <*> wrM))

-- | Create read-after-write blockRAM from a read-before-write one (synchronised to system clock)
--
-- >>> import CLaSH.Prelude
-- >>> :t readNew (blockRam (0 :> 1 :> Nil))
-- readNew (blockRam (0 :> 1 :> Nil))
--   :: ... =>
--      Signal addr -> Signal (Maybe (addr, a)) -> Signal a
readNew :: Eq addr
        => (Signal addr -> Signal (Maybe (addr, a)) -> Signal a)
        -- ^ The @ram@ component
        -> Signal addr              -- ^ Read address @r@
        -> Signal (Maybe (addr, a)) -- ^ (Write address @w@, value to write)
        -> Signal a
        -- ^ Value of the @ram@ at address @r@ from the previous clock
        -- cycle
readNew = readNew' systemClock
