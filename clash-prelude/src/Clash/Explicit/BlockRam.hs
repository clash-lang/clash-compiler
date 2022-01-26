{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016-2017, Myrtle Software Ltd,
                  2017     , Google Inc.,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

BlockRAM primitives

= Using RAMs #usingrams#

We will show a rather elaborate example on how you can, and why you might want
to use 'blockRam's. We will build a \"small\" CPU+Memory+Program ROM where we
will slowly evolve to using blockRams. Note that the code is /not/ meant as a
de-facto standard on how to do CPU design in Clash.

We start with the definition of the Instructions, Register names and machine
codes:

@
{\-\# LANGUAGE RecordWildCards, TupleSections, DeriveAnyClass \#-\}

module CPU where

import Clash.Explicit.Prelude

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
cpu
  :: Vec 7 Value
  -- ^ Register bank
  -> (Value,Instruction)
  -- ^ (Memory output, Current instruction)
  -> ( Vec 7 Value
     , (MemAddr, Maybe (MemAddr,Value), InstrAddr)
     )
cpu regbank (memOut,instr) = (regbank',(rdAddr,(,aluOut) '<$>' wrAddrM,fromIntegral ipntr))
  where
    -- Current instruction pointer
    ipntr = regbank 'Clash.Sized.Vector.!!' PC

    -- Decoder
    (MachCode {..}) = case instr of
      Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
      Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
      Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
      Load a r             -> nullCode {ldReg=r,rdAddr=a}
      Store r a            -> nullCode {inputX=r,wrAddrM=Just a}
      Nop                  -> nullCode

    -- ALU
    regX   = regbank 'Clash.Sized.Vector.!!' inputX
    regY   = regbank 'Clash.Sized.Vector.!!' inputY
    aluOut = alu aluCode regX regY

    -- next instruction
    nextPC = case jmpM of
               Just a | aluOut /= 0 -> ipntr + a
               _                    -> ipntr + 1

    -- update registers
    regbank' = 'Clash.Sized.Vector.replace' Zero   0
             $ 'Clash.Sized.Vector.replace' PC     nextPC
             $ 'Clash.Sized.Vector.replace' result aluOut
             $ 'Clash.Sized.Vector.replace' ldReg  memOut
             $ regbank

alu Add   x y = x + y
alu Sub   x y = x - y
alu Incr  x _ = x + 1
alu Imm   x _ = x
alu CmpGt x y = if x > y then 1 else 0
@

We initially create a memory out of simple registers:

@
dataMem
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom MemAddr
  -- ^ Read address
  -> Signal dom (Maybe (MemAddr,Value))
  -- ^ (write address, data in)
  -> Signal dom Value
  -- ^ data out
dataMem clk rst en rd wrM = 'Clash.Explicit.Mealy.mealy' clk rst en dataMemT ('Clash.Sized.Vector.replicate' d32 0) (bundle (rd,wrM))
  where
    dataMemT mem (rd,wrM) = (mem',dout)
      where
        dout = mem 'Clash.Sized.Vector.!!' rd
        mem' = case wrM of
                 Just (wr,din) -> 'Clash.Sized.Vector.replace' wr din mem
                 _ -> mem
@

And then connect everything:

@
system
  :: ( KnownDomain dom
     , KnownNat n )
  => Vec n Instruction
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Value
system instrs clk rst en = memOut
  where
    memOut = dataMem clk rst en rdAddr dout
    (rdAddr,dout,ipntr) = 'Clash.Explicit.Mealy.mealyB' clk rst en cpu ('Clash.Sized.Vector.replicate' d7 0) (memOut,instr)
    instr  = 'Clash.Explicit.Prelude.asyncRom' instrs '<$>' ipntr
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
>>> sampleN 32 $ system prog systemClockGen resetGen enableGen
[0,0,0,0,0,0,4,4,4,4,4,4,4,4,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2]

@

to see that our system indeed calculates that the GCD of 6 and 4 is 2.

=== Improvement 1: using @asyncRam@

As you can see, it's fairly straightforward to build a memory using registers
and read ('Clash.Sized.Vector.!!') and write ('Clash.Sized.Vector.replace')
logic. This might however not result in the most efficient hardware structure,
especially when building an ASIC.

Instead it is preferable to use the 'Clash.Prelude.RAM.asyncRam' function which
has the potential to be translated to a more efficient structure:

@
system2
  :: ( KnownDomain dom
     , KnownNat n )
  => Vec n Instruction
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Value
system2 instrs clk rst en = memOut
  where
    memOut = 'Clash.Explicit.RAM.asyncRam' clk clk en d32 rdAddr dout
    (rdAddr,dout,ipntr) = 'Clash.Explicit.Prelude.mealyB' clk rst en cpu ('Clash.Sized.Vector.replicate' d7 0) (memOut,instr)
    instr  = 'Clash.Prelude.ROM.asyncRom' instrs '<$>' ipntr
@

Again, we can simulate our system and see that it works. This time however,
we need to disregard the first few output samples, because the initial content of an
'Clash.Prelude.RAM.asyncRam' is /undefined/, and consequently, the first few
output samples are also /undefined/. We use the utility function
'Clash.XException.printX' to conveniently filter out the undefinedness and
replace it with the string @\"undefined\"@ in the first few leading outputs.

@
>>> printX $ sampleN 32 $ system2 prog systemClockGen resetGen enableGen
[undefined,undefined,undefined,undefined,undefined,undefined,4,4,4,4,4,4,4,4,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2]

@

=== Improvement 2: using @blockRam@

Finally we get to using 'blockRam'. On FPGAs, 'Clash.Prelude.RAM.asyncRam' will
be implemented in terms of LUTs, and therefore take up logic resources. FPGAs
also have large(r) memory structures called /Block RAMs/, which are preferred,
especially as the memories we need for our application get bigger. The
'blockRam' function will be translated to such a /Block RAM/.

One important aspect of Block RAMs have a /synchronous/ read port, meaning that,
unlike the behavior of 'Clash.Prelude.RAM.asyncRam', given a read address @r@
at time @t@, the value @v@ in the RAM at address @r@ is only available at time
@t+1@.

For us that means we need to change the design of our CPU. Right now, upon a
load instruction we generate a read address for the memory, and the value at
that read address is immediately available to be put in the register bank.
Because we will be using a BlockRAM, the value is delayed until the next cycle.
We hence need to also delay the register address to which the memory address
is loaded:

@
cpu2
  :: (Vec 7 Value,Reg)
  -- ^ (Register bank, Load reg addr)
  -> (Value,Instruction)
  -- ^ (Memory output, Current instruction)
  -> ( (Vec 7 Value,Reg)
     , (MemAddr, Maybe (MemAddr,Value), InstrAddr)
     )
cpu2 (regbank,ldRegD) (memOut,instr) = ((regbank',ldRegD'),(rdAddr,(,aluOut) '<$>' wrAddrM,fromIntegral ipntr))
  where
    -- Current instruction pointer
    ipntr = regbank 'Clash.Sized.Vector.!!' PC

    -- Decoder
    (MachCode {..}) = case instr of
      Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
      Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
      Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
      Load a r             -> nullCode {ldReg=r,rdAddr=a}
      Store r a            -> nullCode {inputX=r,wrAddrM=Just a}
      Nop                  -> nullCode

    -- ALU
    regX   = regbank 'Clash.Sized.Vector.!!' inputX
    regY   = regbank 'Clash.Sized.Vector.!!' inputY
    aluOut = alu aluCode regX regY

    -- next instruction
    nextPC = case jmpM of
               Just a | aluOut /= 0 -> ipntr + a
               _                    -> ipntr + 1

    -- update registers
    ldRegD'  = ldReg -- Delay the ldReg by 1 cycle
    regbank' = 'Clash.Sized.Vector.replace' Zero   0
             $ 'Clash.Sized.Vector.replace' PC     nextPC
             $ 'Clash.Sized.Vector.replace' result aluOut
             $ 'Clash.Sized.Vector.replace' ldRegD memOut
             $ regbank
@

We can now finally instantiate our system with a 'blockRam':

@
system3
  :: ( KnownDomain dom
     , KnownNat n )
  => Vec n Instruction
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Value
system3 instrs clk rst en = memOut
  where
    memOut = 'blockRam' clk en (replicate d32 0) rdAddr dout
    (rdAddr,dout,ipntr) = 'Clash.Explicit.Prelude.mealyB' clk rst en cpu2 (('Clash.Sized.Vector.replicate' d7 0),Zero) (memOut,instr)
    instr  = 'Clash.Explicit.Prelude.asyncRom' instrs '<$>' ipntr
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
'blockRam' is /undefined/. We use the utility function 'Clash.XException.printX'
to conveniently filter out the undefinedness and replace it with the string @\"undefined\"@.

@
>>> printX $ sampleN 34 $ system3 prog2 systemClockGen resetGen enableGen
[undefined,0,0,0,0,0,0,4,4,4,4,4,4,4,4,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2]

@

This concludes the short introduction to using 'blockRam'.

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- Prevent generation of eta port names for trueDualPortBlockRam
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

module Clash.Explicit.BlockRam
  ( -- * BlockRAM synchronized to the system clock
    blockRam
  , blockRamPow2
  , blockRamU
  , blockRam1
  , trueDualPortBlockRam
  , RamOp(..)
  , ResetStrategy(..)
    -- * Read/Write conflict resolution
  , readNew
    -- * Internal
  , blockRam#
  , trueDualPortBlockRam#
  )
where

import           Clash.HaskellPrelude

import           Control.Exception      (catch, throw)
import           Control.Monad          (forM_)
import           Control.Monad.ST       (ST, runST)
import           Control.Monad.ST.Unsafe (unsafeInterleaveST, unsafeIOToST, unsafeSTToIO)
import           Data.Array.MArray      (newListArray)
import qualified Data.List              as L
import           Data.Either            (isLeft)
import           Data.Maybe             (isJust, fromMaybe)
import           GHC.Arr
  (STArray, unsafeReadSTArray, unsafeWriteSTArray)
import qualified Data.Sequence          as Seq
import           Data.Sequence          (Seq)
import           Data.Tuple             (swap)
import           GHC.Stack              (HasCallStack, withFrozenCallStack)
import           GHC.TypeLits           (KnownNat, type (^), type (<=))
import           Unsafe.Coerce          (unsafeCoerce)

import           Clash.Annotations.Primitive
  (hasBlackBox)
import           Clash.Class.Num        (SaturationMode(SatBound), satSucc)
import           Clash.Explicit.Signal  (KnownDomain, Enable, register, fromEnable)
import           Clash.Signal.Internal
  (Clock(..), Reset, Signal (..), invertReset, (.&&.), mux)
import           Clash.Promoted.Nat     (SNat(..), snatToNum, natToNum)
import           Clash.Signal.Bundle    (unbundle, bundle)
import           Clash.Signal.Internal.Ambiguous (clockPeriod)
import           Clash.Sized.Unsigned   (Unsigned)
import           Clash.Sized.Index      (Index)
import           Clash.Sized.Vector     (Vec, replicate, iterateI)
import qualified Clash.Sized.Vector     as CV
import           Clash.XException
  (maybeIsX, NFDataX, deepErrorX, defaultSeqX, fromJustX, undefined,
   XException (..), seqX, isX)

-- start benchmark only
-- import GHC.Arr (listArray, unsafeThawSTArray)
-- end benchmark only

{- $setup
>>> import Clash.Explicit.Prelude as C
>>> import qualified Data.List as L
>>> :set -XDataKinds -XRecordWildCards -XTupleSections -XDeriveAnyClass -XDeriveGeneric
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
  deriving (Eq,Show,Enum,C.Generic,NFDataX)
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
let cpu :: Vec 7 Value          -- ^ Register bank
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
let dataMem
      :: KnownDomain dom
      => Clock  dom
      -> Reset  dom
      -> Enable dom
      -> Signal dom MemAddr
      -> Signal dom (Maybe (MemAddr,Value))
      -> Signal dom Value
    dataMem clk rst en rd wrM = mealy clk rst en dataMemT (C.replicate d32 0) (bundle (rd,wrM))
      where
        dataMemT mem (rd,wrM) = (mem',dout)
          where
            dout = mem C.!! rd
            mem' = case wrM of
                     Just (wr,din) -> replace wr din mem
                     Nothing       -> mem
:}

>>> :{
let system
      :: ( KnownDomain dom
         , KnownNat n )
      => Vec n Instruction
      -> Clock dom
      -> Reset dom
      -> Enable dom
      -> Signal dom Value
    system instrs clk rst en = memOut
      where
        memOut = dataMem clk rst en rdAddr dout
        (rdAddr,dout,ipntr) = mealyB clk rst en cpu (C.replicate d7 0) (memOut,instr)
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
let system2
      :: ( KnownDomain dom
         , KnownNat n )
      => Vec n Instruction
      -> Clock dom
      -> Reset dom
      -> Enable dom
      -> Signal dom Value
    system2 instrs clk rst en = memOut
      where
        memOut = asyncRam clk clk en d32 rdAddr dout
        (rdAddr,dout,ipntr) = mealyB clk rst en cpu (C.replicate d7 0) (memOut,instr)
        instr  = asyncRom instrs <$> ipntr
:}

>>> :{
let cpu2 :: (Vec 7 Value,Reg)    -- ^ (Register bank, Load reg addr)
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
let system3
      :: ( KnownDomain dom
         , KnownNat n )
      => Vec n Instruction
      -> Clock dom
      -> Reset dom
      -> Enable dom
      -> Signal dom Value
    system3 instrs clk rst en = memOut
      where
        memOut = blockRam clk en (C.replicate d32 0) rdAddr dout
        (rdAddr,dout,ipntr) = mealyB clk rst en cpu2 ((C.replicate d7 0),Zero) (memOut,instr)
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

-- | Create a blockRAM with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'XException'
--
-- @
-- bram40
--   :: 'Clock'  dom
--   -> 'Enable'  dom
--   -> 'Signal' dom ('Unsigned' 6)
--   -> 'Signal' dom (Maybe ('Unsigned' 6, 'Clash.Sized.BitVector.Bit'))
--   -> 'Signal' dom 'Clash.Sized.BitVector.Bit'
-- bram40 clk en = 'blockRam' clk en ('Clash.Sized.Vector.replicate' d40 1)
-- @
--
-- Additional helpful information:
--
-- * See "Clash.Explicit.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew' for obtaining write-before-read semantics like this: @'readNew' clk rst ('blockRam' clk inits) rd wrM@.
blockRam
  :: ( KnownDomain dom
     , HasCallStack
     , NFDataX a
     , Enum addr )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> Vec n a
  -- ^ Initial content of the BRAM, also determines the size, @n@, of the BRAM.
   --
   -- __NB__: __MUST__ be a constant.
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, a))
  -- ^ (write address @w@, value to write)
  -> Signal dom a
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRam = \clk gen content rd wrM ->
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJustX <$> wrM)
  in  withFrozenCallStack
      (blockRam# clk gen content (fromEnum <$> rd) en (fromEnum <$> wr) din)
{-# INLINE blockRam #-}

-- | Create a blockRAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'XException'
--
-- @
-- bram32
--   :: 'Clock' dom
--   -> 'Enable' dom
--   -> 'Signal' dom ('Unsigned' 5)
--   -> 'Signal' dom (Maybe ('Unsigned' 5, 'Clash.Sized.BitVector.Bit'))
--   -> 'Signal' dom 'Clash.Sized.BitVector.Bit'
-- bram32 clk en = 'blockRamPow2' clk en ('Clash.Sized.Vector.replicate' d32 1)
-- @
--
-- Additional helpful information:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew' for obtaining write-before-read semantics like this: @'readNew' clk rst ('blockRamPow2' clk inits) rd wrM@.
blockRamPow2
  :: ( KnownDomain dom
     , HasCallStack
     , NFDataX a
     , KnownNat n )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> Vec (2^n) a
  -- ^ Initial content of the BRAM
  --
  -- __NB__: __MUST__ be a constant.
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom (Maybe (Unsigned n, a))
  -- ^ (Write address @w@, value to write)
  -> Signal dom a
  -- ^ Value of the @blockRAM@ at address @r@ from the previous
  -- clock cycle
blockRamPow2 = \clk en cnt rd wrM -> withFrozenCallStack
  (blockRam clk en cnt rd wrM)
{-# INLINE blockRamPow2 #-}

data ResetStrategy (r :: Bool) where
  ClearOnReset :: ResetStrategy 'True
  NoClearOnReset :: ResetStrategy 'False

-- | Version of blockram that has no default values set. May be cleared to an
-- arbitrary state using a reset function.
blockRamU
   :: forall n dom a r addr
   . ( KnownDomain dom
     , HasCallStack
     , NFDataX a
     , Enum addr
     , 1 <= n )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Reset dom
  -- ^ 'Reset' line to listen to. Needs to be held at least /n/ cycles in order
  -- for the BRAM to be reset to its initial state.
  -> Enable dom
  -- ^ Global enable
  -> ResetStrategy r
  -- ^ Whether to clear BRAM on asserted reset ('ClearOnReset') or
  -- not ('NoClearOnReset'). Reset needs to be asserted at least /n/ cycles to
  -- clear the BRAM.
  -> SNat n
  -- ^ Number of elements in BRAM
  -> (Index n -> a)
  -- ^ If applicable (see 'ResetStrategy' argument), reset BRAM using this function.
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, a))
  -- ^ (write address @w@, value to write)
  -> Signal dom a
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRamU clk rst0 en rstStrategy n@SNat initF rd0 mw0 =
  case rstStrategy of
    ClearOnReset ->
      -- Use reset infrastructure
      blockRamU# clk en n rd1 we1 wa1 w1
    NoClearOnReset ->
      -- Ignore reset infrastructure, pass values unchanged
      blockRamU# clk en n
        (fromEnum <$> rd0)
        we0
        (fromEnum <$> wa0)
        w0
 where
  rstBool = register clk rst0 en True (pure False)
  rstInv = invertReset rst0

  waCounter :: Signal dom (Index n)
  waCounter = register clk rstInv en 0 (satSucc SatBound <$> waCounter)

  wa0 = fst . fromJustX <$> mw0
  w0  = snd . fromJustX <$> mw0
  we0 = isJust <$> mw0

  rd1 = mux rstBool 0 (fromEnum <$> rd0)
  we1 = mux rstBool (pure True) we0
  wa1 = mux rstBool (fromInteger . toInteger <$> waCounter) (fromEnum <$> wa0)
  w1  = mux rstBool (initF <$> waCounter) w0

-- | blockRAMU primitive
blockRamU#
  :: forall n dom a
   . ( KnownDomain dom
     , HasCallStack
     , NFDataX a )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global Enable
  -> SNat n
  -- ^ Number of elements in BRAM
  -> Signal dom Int
  -- ^ Read address @r@
  -> Signal dom Bool
  -- ^ Write enable
  -> Signal dom Int
  -- ^ Write address @w@
  -> Signal dom a
  -- ^ Value to write (at address @w@)
  -> Signal dom a
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRamU# clk en SNat =
  -- TODO: Generalize to single BRAM primitive taking an initialization function
  blockRam#
    clk
    en
    (CV.map
      (\i -> deepErrorX $ "Initial value at index " <> show i <> " undefined.")
      (iterateI @n succ (0 :: Int)))
{-# NOINLINE blockRamU# #-}
{-# ANN blockRamU# hasBlackBox #-}

-- | Version of blockram that is initialized with the same value on all
-- memory positions.
blockRam1
   :: forall n dom a r addr
   . ( KnownDomain dom
     , HasCallStack
     , NFDataX a
     , Enum addr
     , 1 <= n )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Reset dom
  -- ^ 'Reset' line to listen to. Needs to be held at least /n/ cycles in order
  -- for the BRAM to be reset to its initial state.
  -> Enable dom
  -- ^ Global enable
  -> ResetStrategy r
  -- ^ Whether to clear BRAM on asserted reset ('ClearOnReset') or
  -- not ('NoClearOnReset'). Reset needs to be asserted at least /n/ cycles to
  -- clear the BRAM.
  -> SNat n
  -- ^ Number of elements in BRAM
  -> a
  -- ^ Initial content of the BRAM (replicated /n/ times)
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, a))
  -- ^ (write address @w@, value to write)
  -> Signal dom a
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRam1 clk rst0 en rstStrategy n@SNat a rd0 mw0 =
  case rstStrategy of
    ClearOnReset ->
      -- Use reset infrastructure
      blockRam1# clk en n a rd1 we1 wa1 w1
    NoClearOnReset ->
      -- Ignore reset infrastructure, pass values unchanged
      blockRam1# clk en n a
        (fromEnum <$> rd0)
        we0
        (fromEnum <$> wa0)
        w0
 where
  rstBool = register clk rst0 en True (pure False)
  rstInv = invertReset rst0

  waCounter :: Signal dom (Index n)
  waCounter = register clk rstInv en 0 (satSucc SatBound <$> waCounter)

  wa0 = fst . fromJustX <$> mw0
  w0  = snd . fromJustX <$> mw0
  we0 = isJust <$> mw0

  rd1 = mux rstBool 0 (fromEnum <$> rd0)
  we1 = mux rstBool (pure True) we0
  wa1 = mux rstBool (fromInteger . toInteger <$> waCounter) (fromEnum <$> wa0)
  w1  = mux rstBool (pure a) w0

-- | blockRAM1 primitive
blockRam1#
  :: forall n dom a
   . ( KnownDomain dom
     , HasCallStack
     , NFDataX a )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global Enable
  -> SNat n
  -- ^ Number of elements in BRAM
  -> a
  -- ^ Initial content of the BRAM (replicated /n/ times)
  -> Signal dom Int
  -- ^ Read address @r@
  -> Signal dom Bool
  -- ^ Write enable
  -> Signal dom Int
  -- ^ Write address @w@
  -> Signal dom a
  -- ^ Value to write (at address @w@)
  -> Signal dom a
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRam1# clk en n a =
  -- TODO: Generalize to single BRAM primitive taking an initialization function
  blockRam# clk en (replicate n a)
{-# NOINLINE blockRam1# #-}
{-# ANN blockRam1# hasBlackBox #-}

-- | blockRAM primitive
blockRam#
  :: forall dom a n
   . ( KnownDomain dom
     , HasCallStack
     , NFDataX a )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> Vec n a
  -- ^ Initial content of the BRAM, also
  -- determines the size, @n@, of the BRAM.
  --
  -- __NB__: __MUST__ be a constant.
  -> Signal dom Int
  -- ^ Read address @r@
  -> Signal dom Bool
  -- ^ Write enable
  -> Signal dom Int
  -- ^ Write address @w@
  -> Signal dom a
  -- ^ Value to write (at address @w@)
  -> Signal dom a
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRam# (Clock _) gen content = \rd wen waS wd -> runST $ do
  ramStart <- newListArray (0,szI-1) contentL
  -- start benchmark only
  -- ramStart <- unsafeThawSTArray ramArr
  -- end benchmark only
  go
    ramStart
    (withFrozenCallStack (deepErrorX "blockRam: intial value undefined"))
    (fromEnable gen)
    rd
    (fromEnable gen .&&. wen)
    waS
    wd
 where
  contentL = unsafeCoerce content :: [a]
  szI = L.length contentL
  -- start benchmark only
  -- ramArr = listArray (0,szI-1) contentL
  -- end benchmark only

  go :: STArray s Int a -> a -> Signal dom Bool -> Signal dom Int
     -> Signal dom Bool -> Signal dom Int -> Signal dom a
     -> ST s (Signal dom a)
  go !ram o ret@(~(re :- res)) rt@(~(r :- rs)) et@(~(e :- en)) wt@(~(w :- wr)) dt@(~(d :- din)) = do
    o `seqX` (o :-) <$> (ret `seq` rt `seq` et `seq` wt `seq` dt `seq`
      unsafeInterleaveST
        (do o' <- unsafeIOToST
                    (catch (if re then unsafeSTToIO (ram `safeAt` r) else pure o)
                    (\err@XException {} -> pure (throw err)))
            d `defaultSeqX` upd ram e (fromEnum w) d
            go ram o' res rs en wr din))

  upd :: STArray s Int a -> Bool -> Int -> a -> ST s ()
  upd ram we waddr d = case maybeIsX we of
    Nothing -> case maybeIsX waddr of
      Nothing -> -- Put the XException from `waddr` as the value in all
                 -- locations of `ram`.
                 forM_ [0..(szI-1)] (\i -> unsafeWriteSTArray ram i (seq waddr d))
      Just wa -> -- Put the XException from `we` as the value at address
                 -- `waddr`.
                 safeUpdate wa (seq we d) ram
    Just True -> case maybeIsX waddr of
      Nothing -> -- Put the XException from `waddr` as the value in all
                 -- locations of `ram`.
                 forM_ [0..(szI-1)] (\i -> unsafeWriteSTArray ram i (seq waddr d))
      Just wa -> safeUpdate wa d ram
    _ -> return ()

  safeAt :: HasCallStack => STArray s Int a -> Int -> ST s a
  safeAt s i =
    if (0 <= i) && (i < szI) then
      unsafeReadSTArray s i
    else pure $
      withFrozenCallStack
        (deepErrorX ("blockRam: read address " <> show i <>
                     " not in range [0.." <> show szI <> ")"))
  {-# INLINE safeAt #-}

  safeUpdate :: HasCallStack => Int -> a -> STArray s Int a -> ST s ()
  safeUpdate i a s =
    if (0 <= i) && (i < szI) then
      unsafeWriteSTArray s i a
    else
      let d = withFrozenCallStack
                (deepErrorX ("blockRam: write address " <> show i <>
                             " not in range [0.." <> show szI <> ")"))
       in forM_ [0..(szI-1)] (\j -> unsafeWriteSTArray s j d)
  {-# INLINE safeUpdate #-}
{-# ANN blockRam# hasBlackBox #-}
{-# NOINLINE blockRam# #-}

-- | Create read-after-write blockRAM from a read-before-write one
readNew
  :: ( KnownDomain dom
     , NFDataX a
     , Eq addr )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> (Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a)
  -- ^ The @ram@ component
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, a))
  -- ^ (Write address @w@, value to write)
  -> Signal dom a
  -- ^ Value of the @ram@ at address @r@ from the previous clock cycle
readNew clk rst en ram rdAddr wrM = mux wasSame wasWritten $ ram rdAddr wrM
  where readNewT rd (Just (wr, wrdata)) = (wr == rd, wrdata)
        readNewT _  Nothing             = (False   , undefined)

        (wasSame,wasWritten) =
          unbundle (register clk rst en (False, undefined)
                             (readNewT <$> rdAddr <*> wrM))


data RamOp n a = RamRead (Index n) | RamWrite (Index n) a

ramOpAddr :: RamOp n a -> Index n
ramOpAddr (RamRead addr)    = addr
ramOpAddr (RamWrite addr _) = addr

isRamWrite :: RamOp n a -> Bool
isRamWrite (RamRead {})  = False
isRamWrite (RamWrite {}) = True

ramOpWriteVal :: RamOp n a -> Maybe a
ramOpWriteVal (RamRead {})     = Nothing
ramOpWriteVal (RamWrite _ val) = Just val

-- | Produces vendor-agnostic HDL that will be inferred as a true, dual port
-- block ram. Any values that's being written on a particular port is also the
-- value that will be read on that port, i.e. the same-port read/write behavior
-- is: WriteFirst. For mixed port read/write, when both ports have the same
-- address, when there is a write on the port A, the output of port B is
-- undefined, and visa versa. Implicitly clocked version is
-- `Clash.Prelude.BlockRam.trueDualPortBlockRam`
trueDualPortBlockRam ::
  forall nAddrs domA domB a .
  ( HasCallStack
  , KnownNat nAddrs
  , KnownDomain domA
  , KnownDomain domB
  , NFDataX a
  )
  => Clock domA
  -- ^ Clock for port A
  -> Clock domB
  -- ^ Clock for port B
  -> Signal domA (RamOp nAddrs a)
  -- ^ ram operation for port A
  -> Signal domB (RamOp nAddrs a)
  -- ^ ram operation for port B
  -> (Signal domA a, Signal domB a)
  -- ^ Outputs data on /next/ cycle. When writing, the data written
  -- will be echoed. When reading, the read data is returned.

{-# INLINE trueDualPortBlockRam #-}
trueDualPortBlockRam = \clkA clkB opA opB ->
  trueDualPortBlockRamWrapper
    clkA (isRamWrite <$> opA) (ramOpAddr <$> opA) (fromJustX . ramOpWriteVal <$> opA)
    clkB (isRamWrite <$> opB) (ramOpAddr <$> opB) (fromJustX . ramOpWriteVal <$> opB)

toMaybeX :: a -> MaybeX a
toMaybeX a =
  case isX a of
    Left _ -> IsX
    Right _ -> IsDefined a

data MaybeX a = IsX | IsDefined !a

data Conflict = Conflict
  { cfWrite :: !(MaybeX Bool)
  , cfAddress :: !(MaybeX Int) }

instance Semigroup Conflict where
  (<>) = mergeConflicts

-- | "Stronger" conflict wins:
--
--   * Writes over read
--   * Undefineds over anything
--
mergeConflicts :: Conflict -> Conflict -> Conflict
mergeConflicts conflict1 conflict2 = Conflict
  { cfWrite = mergeWrite (cfWrite conflict1) (cfWrite conflict2)
  , cfAddress = mergeAddress (cfAddress conflict1) (cfAddress conflict2) }
 where
  mergeX _ IsX _ = IsX
  mergeX _ _ IsX = IsX
  mergeX f (IsDefined a) (IsDefined b) = IsDefined (f a b)

  mergeWrite a b = mergeX (||) a b
  mergeAddress a b = mergeX const a b

trueDualPortBlockRamWrapper clkA weA addrA datA clkB weB addrB datB =
  trueDualPortBlockRam# clkA weA addrA datA clkB weB addrB datB
{-# NOINLINE trueDualPortBlockRamWrapper #-}

-- | Primitive of 'trueDualPortBlockRam'.
trueDualPortBlockRam#, trueDualPortBlockRamWrapper ::
  forall nAddrs domA domB a .
  ( HasCallStack
  , KnownNat nAddrs
  , KnownDomain domA
  , KnownDomain domB
  , NFDataX a
  , (a ~ a)   -- BitPack a -- temp hack to avoid renumbering ~ARGs
  )
  => Clock domA
  -- ^ Clock for port A
  -> Signal domA Bool
  -- ^ Write enable for port A
  -> Signal domA (Index nAddrs)
  -- ^ Address to read from or write to on port A
  -> Signal domA a
  -- ^ Data in for port A; ignored when /write enable/ is @False@

  -> Clock domB
  -- ^ Clock for port B
  -> Signal domB Bool
  -- ^ Write enable for port B
  -> Signal domB (Index nAddrs)
  -- ^ Address to read from or write to on port B
  -> Signal domB a
  -- ^ Data in for port B; ignored when /write enable/ is @False@

  -> (Signal domA a, Signal domB a)
  -- ^ Outputs data on /next/ cycle. If write enable is @True@, the data written
  -- will be echoed. If write enable is @False@, the read data is returned.
trueDualPortBlockRam# clkA weA addrA datA clkB weB addrB datB
  | snatToNum @Int (clockPeriod @domA) < snatToNum @Int (clockPeriod @domB)
  = swap (trueDualPortBlockRamModel clkB weB addrB datB clkA weA addrA datA)
  | otherwise
  =       trueDualPortBlockRamModel clkA weA addrA datA clkB weB addrB datB
{-# NOINLINE trueDualPortBlockRam# #-}
{-# ANN trueDualPortBlockRam# hasBlackBox #-}


-- | Haskell model for the primitive 'trueDualPortBlockRam#'.
--
-- Warning: this model only works if @domFast@'s clock is faster (or equal to)
-- @domSlow@'s clock.
trueDualPortBlockRamModel ::
  forall nAddrs domFast domSlow a .
  ( HasCallStack
  , KnownNat nAddrs
  , KnownDomain domSlow
  , KnownDomain domFast
  , NFDataX a
  ) =>

  Clock domSlow ->
  Signal domSlow Bool ->
  Signal domSlow (Index nAddrs) ->
  Signal domSlow a ->

  Clock domFast ->
  Signal domFast Bool ->
  Signal domFast (Index nAddrs) ->
  Signal domFast a ->

  (Signal domSlow a, Signal domFast a)
trueDualPortBlockRamModel !_clkA weA addrA datA !_clkB weB addrB datB =
  ( deepErrorX "trueDualPortBlockRam: Port A: First value undefined" :- outA
  , deepErrorX "trueDualPortBlockRam: Port B: First value undefined" :- outB )
 where
  (outA, outB) =
    go
      Nothing
      (Seq.fromFunction (natToNum @nAddrs) initElement)
      tA -- ensure 'go' hits fast clock first
      (bundle (weA, fromIntegral <$> addrA, datA))
      (bundle (weB, fromIntegral <$> addrB, datB))

  tA = snatToNum @Int (clockPeriod @domSlow)
  tB = snatToNum @Int (clockPeriod @domFast)

  initElement :: Int -> a
  initElement n =
    deepErrorX ("Unknown initial element; position " <> show n)

  unknownEnableAndAddr :: Int -> a
  unknownEnableAndAddr n =
    deepErrorX ("Write enable and data unknown; position " <> show n)

  unknownAddr :: Int -> a
  unknownAddr n =
    deepErrorX ("Write enabled, but address unknown; position " <> show n)

  getConflict :: Bool -> Int -> Bool -> Int -> Maybe Conflict
  getConflict enableA addrA_ enableB addrB_ =
    -- If port A or port B is writing on (potentially!) the same address,
    -- there's a conflict
    if sameAddr then maybeConflict else Nothing
   where
    conflict = Conflict
      { cfWrite = toMaybeX enableA
      , cfAddress = toMaybeX addrA_ }

    maybeConflict =
      case (isX enableA, isX enableB) of
        (Left _, _)     -> Just conflict
        (Right True, _) -> Just conflict
        (_, Left _)     -> Just conflict
        (_, Right True) -> Just conflict
        _               -> Nothing

    sameAddr =
      case (isX addrA_, isX addrB_) of
        (Left _, _) -> True
        (_, Left _) -> True
        _           -> addrA_ == addrB_

  writeRam :: Bool -> Int -> a -> Seq a -> (Maybe a, Seq a)
  writeRam enable addr dat mem
    | enableUndefined && addrUndefined
    = ( Just (deepErrorX "Unknown enable and address")
      , Seq.fromFunction (natToNum @nAddrs) unknownEnableAndAddr )
    | addrUndefined
    = ( Just (deepErrorX "Unknown address")
      , Seq.fromFunction (natToNum @nAddrs) unknownAddr )
    | enableUndefined
    = writeRam True addr (deepErrorX ("Write unknown; position" <> show addr)) mem
    | enable
    = (Just dat, Seq.update addr dat mem)
    | otherwise
    = (Nothing, mem)
   where
    enableUndefined = isLeft (isX enable)
    addrUndefined = isLeft (isX addr)

  go ::
    Maybe Conflict ->
    Seq a ->
    Int ->
    Signal domSlow (Bool, Int, a) ->
    Signal domFast (Bool, Int, a) ->
    (Signal domSlow a, Signal domFast a)
  go conflict0 ram0 relativeTime as0 bs0 =
    if relativeTime <= 0 then goSlow else goFast
   where
    (weA_, addrA_, datA_) :- as1 = as0
    (weB_, addrB_, datB_) :- bs1 = bs0

    -- 1 iteration here, as this is the slow clock.
    goSlow = out1 `seqX` (out1 :- as2, bs2)
     where
      (wrote, !ram1) = writeRam weA_ addrA_ datA_ ram0
      out0 = fromMaybe (ram1 `Seq.index` addrA_) wrote
      (as2, bs2) = go Nothing ram1 (relativeTime + tA) as1 bs0
      out1 =
        case conflict0 of
          Just Conflict{cfWrite=IsDefined True} ->
            deepErrorX "trueDualPortBlockRam: conflicting read/write queries"
          Just Conflict{cfWrite=IsX} ->
            deepErrorX "trueDualPortBlockRam: conflicting read/write queries"
          _ -> out0

    -- 1 or more iterations here, as this is the fast clock. First iteration
    -- happens here.
    goFast = out1 `seqX` (as2, out1 :- bs2)
     where
      conflict1 = getConflict weB_ addrB_ weA_ addrA_
      (wrote, !ram1) = writeRam weB_ addrB_ datB_ ram0
      out0 = fromMaybe (ram1 `Seq.index` addrB_) wrote
      conflict2 = conflict0 <> conflict1
      (as2, bs2) = go conflict2 ram1 (relativeTime - tB) as0 bs1
      out1 =
        case conflict1 of
          Just Conflict{cfWrite=IsDefined False} ->
            deepErrorX "trueDualPortBlockRam: conflicting read/write queries"
          Just Conflict{cfWrite=IsX} ->
            deepErrorX "trueDualPortBlockRam: conflicting read/write queries"
          _ ->
            out0
