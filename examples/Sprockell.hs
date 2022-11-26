{-# LANGUAGE DataKinds, RecordWildCards, TypeOperators #-}
module Sprockell where

import Clash.Prelude hiding (Word, And)

{-------------------------------------------------------------
| SPROCKELL: Simple PROCessor in hasKELL :-)
|
| j.kuper@utwente.nl
| October 28, 2012
-------------------------------------------------------------}


-- Types
type Word       = Signed 16

type RegBankSize = 8
type ProgMemSize = 128
type DataMemSize = 128

type RegBank    = Vec RegBankSize Word
type ProgMem    = Vec ProgMemSize Assembly
type DataMem    = Vec DataMemSize Word

type RegBankAddr = Unsigned 3
type ProgMemAddr = Unsigned 7
type DataMemAddr = Unsigned 7


-- value to be put in Register Bank
data RegValue   = RAddr DataMemAddr
                | RImm Word
                deriving (Eq,Show)

-- value to be put in data memory
data MemValue   = MAddr RegBankAddr
                | MImm Word
                deriving (Eq,Show)

data LdCode     = NoLoad
                | LdImm
                | LdAddr
                | LdAlu
                deriving (Eq,Show)

data StCode     = NoStore
                | StImm
                | StReg
                deriving (Eq,Show)

data SPCode     = None
                | Up
                | Down
                deriving (Eq,Show)

data JmpCode    = NoJump            -- No jump
                | UA                -- UnConditional - Absolute
                | UR                -- UnConditional - Relative
                | CA                -- Conditional   - Absolute
                | CR                -- Conditional   - Relative
                | Back              -- Back from subroutine
                deriving (Eq,Show)

data MachCode   = MachCode { ldCode     :: LdCode       -- 0/1: load from dmem to rbank?
                , stCode     :: StCode        -- storeCode
                , spCode     :: SPCode
                , opCode     :: OpCode        -- opCode
                , immvalueR  :: Word          -- value from Immediate - to regbank
                , immvalueS  :: Word          -- value from Immediate - to store
                , fromreg0   :: RegBankAddr   -- ibid, first parameter of Compute
                , fromreg1   :: RegBankAddr   -- ibid, second parameter of Compute
                , fromaddr   :: DataMemAddr   -- address in dmem
                , toreg      :: RegBankAddr   -- ibid, third parameter of Compute
                , toaddr     :: DataMemAddr   -- address in dmem
                , wen        :: Bool          -- enable signal for store
                , jmpCode    :: JmpCode       -- 0/1: indicates a jump
                , jumpN      :: ProgMemAddr   -- which instruction to jump to
                }
                deriving (Eq,Show)


data OpCode     = NoOp | Id  | Incr | Decr                              -- no corresponding functions in prog.language
                | Neg  | Not                                            -- unary operations
                | Add  | Sub | Mul | Equal | NEq | Gt | Lt | And | Or   -- binary operations
                deriving (Eq,Show)

data Assembly   = Compute OpCode RegBankAddr RegBankAddr RegBankAddr    -- Compute opCode r0 r1 r2: go to "alu",
                                                --      do "opCode" on regs r0, r1, and put result in reg r2
                | Jump JmpCode ProgMemAddr      -- JumpAbs n: set program counter to n
                | Load  RegValue RegBankAddr    -- Load (Addr a) r : from "memory a" to "regbank r"
                                                -- Load (Imm  v) r : put "Int v" in "regbank r"
                | Store MemValue DataMemAddr    -- Store (Addr r) a: from "regbank r" to "memory a"
                                                -- Store (Imm  v) r: put "Int v" in "memory r"
                | Push RegBankAddr              -- push a value on the stack
                | Pop RegBankAddr                       -- pop a value from the stack
                | EndProg                       -- end of program, handled bij exec function
                | Debug Word
                deriving (Eq,Show)

--record type for internal state of processor
data  PState = PState { regbank    :: RegBank      -- register bank
                        , dmem     :: DataMem       -- main memory, data memory
                        , cnd      :: Bool          -- condition register (whether condition was true)
                        , pc       :: ProgMemAddr
                        , sp       :: DataMemAddr
                        }
                        deriving (Eq, Show)

-- move reg0 reg1  = Compute Id reg0 zeroreg reg1
-- wait            = Jump UR 0

nullcode    = MachCode { ldCode = NoLoad
                , stCode        = NoStore
                , spCode        = None
                , opCode        = NoOp
                , immvalueR     = 0
                , immvalueS     = 0
                , fromreg0      = 0
                , fromreg1      = 0
                , fromaddr      = 0
                , toreg         = 0
                , toaddr        = 0
                , wen           = False
                , jmpCode       = NoJump
                , jumpN         = 0
                }

-- {-------------------------------------------------------------
-- | some constants
-- -------------------------------------------------------------}
-- zeroreg =  0 :: RegBankAddr
-- regA    =  1 :: RegBankAddr
-- regB    =  2 :: RegBankAddr
-- endreg  =  3 :: RegBankAddr  -- for FOR-loop
-- stepreg =  4 :: RegBankAddr  -- ibid
jmpreg  =  5 :: RegBankAddr  -- for jump instructions
-- pcreg   =  7 :: RegBankAddr  -- pc is added at the end of the regbank => regbank0

-- sp0 = 20 :: DataMemAddr      -- TODO: get sp0 from compiler, add OS

tobit True  = 1
tobit False = 0

oddB = (== 1) . lsb
-- wmax :: Word -> Word -> Word
-- wmax w1 w2 = if w1 > w2 then w1 else w2

-- (<~) :: RegBank -> (RegBankAddr, Word) -> RegBank
-- xs <~ (0, x) = xs
-- xs <~ (7, x) = xs
-- xs <~ (i, x) = xs'
--     where
--         addr = i
--         xs' = vreplace xs (fromUnsigned addr) x

-- (<~~) :: DataMem -> (Bool, DataMemAddr, Word) -> DataMem
-- xs <~~ (False, i, x) = xs
-- xs <~~ (True, i , x) = vreplace xs i x


{-------------------------------------------------------------
| The actual Sprockell
-------------------------------------------------------------}

decode :: (ProgMemAddr, DataMemAddr) -> Assembly -> MachCode
decode (pc, sp) instr  = case instr of
    Compute c i0 i1 i2  ->  nullcode {ldCode  = LdAlu,  opCode    = c,        fromreg0 = i0, fromreg1=i1, toreg=i2}
    Jump jc n           ->  nullcode {jmpCode = jc,     fromreg0  = jmpreg,   jumpN    = n}
    Load  (RImm  n) j   ->  nullcode {ldCode  = LdImm,  immvalueR = n,        toreg    = j}
    Load  (RAddr i) j   ->  nullcode {ldCode  = LdAddr, fromaddr  = i,        toreg    = j}
    Store (MAddr i) j   ->  nullcode {stCode  = StReg,  fromreg0  = i,        toaddr   = j, wen = True}
    Store (MImm  n) j   ->  nullcode {stCode  = StImm,  immvalueS = n,        toaddr   = j, wen = True}
    Push r              ->  nullcode {stCode  = StReg,  fromreg0  = r,        toaddr   = sp + 1, spCode = Up, wen = True}
    Pop r               ->  nullcode {ldCode  = LdAddr, fromaddr  = sp,       toreg    = r, spCode = Down}
    EndProg             ->  nullcode
    Debug _             ->  nullcode

alu :: OpCode -> (Word, Word) -> (Word, Bool)
alu opCode (x, y) = (z, cnd)
    where
        (z, cnd)   = (app opCode x y, oddB z)
        app opCode = case opCode of
            Id      -> \x y -> x        -- identity function on first argument
            Incr    -> \x y -> x + 1    -- increment first argument with 1
            Decr    -> \x y -> x - 1    -- decrement first argument with 1
            Neg     -> \x y -> -x
            Add     -> (+)          -- goes without saying
            Sub     -> (-)
            Mul     -> noDeDup (*)
            Equal   -> (tobit.).(==)    -- test for equality; result 0 or 1
            NEq     -> (tobit.).(/=)    -- test for inequality
            Gt      -> (tobit.).(>)
            Lt      -> (tobit.).(<)
            And     -> noDeDup (*)
            Or      -> \x y -> 0
            Not     -> \x y -> 1-x
            NoOp    -> \x y -> 0        -- result will always be 0

-- load :: RegBank -> LdCode -> RegBankAddr -> (Word, Word, Word) -> RegBank
-- load regbank ldCode toreg (immvalueR, mval, z) = regbank'
--     where
--         v =  case ldCode of
--             NoLoad  -> 0
--             LdImm   -> immvalueR
--             LdAddr  -> mval
--             LdAlu   -> z
--         regbank'  =  regbank <~ (toreg, v)

-- store :: DataMem -> StCode -> (Bool, DataMemAddr) -> (Word, Word) -> DataMem
-- store dmem stCode (wen, toaddr) (immvalueS, x) = dmem'
--         where
--           v = case stCode of
--             NoStore -> 0
--             StImm   -> immvalueS
--             StReg   -> x
--           dmem' = dmem <~~ (wen, toaddr, v)

-- pcUpd :: (JmpCode, Bool) -> (ProgMemAddr, ProgMemAddr, Word) -> ProgMemAddr
-- pcUpd (jmpCode, cnd) (pc, jumpN, x) = pc'
--     where
--         pc' =  case jmpCode of
--             NoJump  -> inc pc
--             UA      -> jumpN
--             UR      -> pc + jumpN
--             CA      -> if cnd then jumpN      else inc pc
--             CR      -> if cnd then pc + jumpN else inc pc
--             Back    -> bv2u (vdrop d9 (s2bv x))
--         inc i = i + 1

-- spUpd :: SPCode -> DataMemAddr -> DataMemAddr
-- spUpd spCode sp = case spCode of
--                     Up      -> sp + 1
--                     Down    -> sp - 1
--                     None    -> sp

-- -- ======================================================================================
-- -- Putting it all together

-- sprockell :: ProgMem -> (State PState) -> Bit -> (State PState, Bit)
-- sprockell  prog  (State state)  inp    =   (State (PState {dmem = dmem',regbank = regbank',cnd = cnd',pc = pc',sp = sp'}), outp)
--   where
--     PState{..}   = state
--     MachCode{..} = decode (pc,sp) (prog ! (fromUnsigned pc))
--     regbank0     = vreplace regbank (fromUnsigned pcreg) (pc2wrd pc)
--     (x,y)        = (regbank0 ! (fromUnsigned fromreg0) , regbank0 ! (fromUnsigned fromreg1))
--     mval         = dmem ! fromaddr
--     (z,cnd')     = alu    opCode         (x,y)
--     regbank'     = load   regbank ldCode toreg  (immvalueR,mval,z)
--     dmem'        = store  dmem    stCode (wen,toaddr)   (immvalueS,x)
--     pc'          = pcUpd  (jmpCode,cnd)  (pc,jumpN,x)
--     sp'          = spUpd  spCode     sp
--     outp         = inp
--     pc2wrd pca   = bv2s (u2bv (resizeUnsigned pca :: Unsigned 16))


-- prog1 = vcopy EndProg

-- initstate = PState {
--                 regbank = vcopy 0,
--                 dmem    = vcopy 0,
--                 cnd     = False,
--                 pc      = 0,
--                 sp      = sp0
--                 }

-- sprockellL = sprockell prog1 ^^^ initstate

topEntity = alu
