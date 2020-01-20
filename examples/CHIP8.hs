{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
module CHIP8 where

-- Thanks to Dr. Gergő Érdi:
--   https://github.com/clash-lang/clash-compiler/issues/461

import Clash.Prelude hiding (lift)
import Control.Monad.State
import Data.Word
import Control.Monad.RWS
import Data.Monoid

{-# NOINLINE topEntity #-}
topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> ( Signal System Bit
       )
topEntity = exposeClockResetEnable output
  where
    cpuIn = pure CPUIn{ cpuInMem = 0x00 }
    cpuOut = mealyState (runCPU defaultOut cpu) initState cpuIn
    output = boolToBit . (== 0x00) . cpuOutMemAddr <$> cpuOut

mealyState
  :: ( HiddenClockResetEnable tag
     , NFDataX s )
  => (i -> State s o)
  -> s
  -> (Signal tag i -> Signal tag o)
mealyState f = mealy $ \s x -> let (y, s') = runState (f x) s in (s', y)

data Phase
    = Init
    | Fetch1
    | Exec
    deriving (Generic, NFDataX)

data CPUIn = CPUIn
    { cpuInMem :: Word8
    }

data CPUState = CPUState
    { pc :: Word8
    , phase :: Phase
    }
    deriving (Generic, NFDataX)

initState :: CPUState
initState = CPUState
    { pc = 0x20
    , phase = Init
    }

data CPUOut = CPUOut
    { cpuOutMemAddr :: Word8
    }

defaultOut :: CPUState -> CPUOut
defaultOut CPUState{..} = CPUOut{..}
  where
    cpuOutMemAddr = pc

cpu :: CPU CPUIn CPUState CPUOut ()
cpu = do
    CPUIn{..} <- input
    CPUState{..} <- get

    case phase of
        Init -> goto Fetch1
        Fetch1 -> goto Init
  where
    goto ph = modify $ \s -> s{ phase = ph }

newtype CPU i s o a = CPU{ unCPU :: RWS i (Endo o) s a }
                    deriving newtype (Functor, Applicative, Monad, MonadState s)

input :: CPU i s o i
input = CPU ask

runCPU :: (s -> o) -> CPU i s o () -> (i -> State s o)
runCPU mkDef cpu inp = do
    s <- get
    let (s', f) = execRWS (unCPU cpu) inp s
    put s'
    def <- gets mkDef
    return $ appEndo f def
