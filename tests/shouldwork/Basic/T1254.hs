{-# LANGUAGE BangPatterns       #-}
module T1254 where

import           Clash.Prelude
import           Control.DeepSeq (NFData)
import           Data.Word
import           GHC.Generics (Generic)

data IndexedWord32 n
  = IndexedWord32
  { getIndex :: !(Index n)
  , getWord  :: !Word32
  } deriving (Generic, Show, Eq, NFData, NFDataX)

data RWOp i = ReadOp !(Index i) | WriteOp !(IndexedWord32 i)
  deriving (Generic, Eq, Show, NFData, NFDataX)

data Operations
  = NoOperation
  | Operation1 (RWOp 1)
  | Operation2 (RWOp 2)
  deriving (Generic, Eq, Show, NFData, NFDataX)

topEntity ::
    Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Operations)
  -> Signal System (Word32)
topEntity clk rst ena inputs = x <$> inputs
  where
    x (Operation1 (WriteOp iw32x)) = getWord $ iw32x
    x (Operation2 (WriteOp iw32y)) = getWord $ iw32y
    x NoOperation                = 0
