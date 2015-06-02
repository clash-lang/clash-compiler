{-# LANGUAGE RecordWildCards #-}
module Reducer where

import CLaSH.Prelude

-- =======================================
-- = System size configuration variables =
-- =======================================
type DataSize       = 32
type IndexSize      = 16
type DiscrSize      = 7
type AdderDepth     = 12

-- Derived configuration variables
type DiscrRange     = 2 ^ DiscrSize

-- ================
-- = Type Aliases =
-- ================
type Shift          = Unsigned 2
type DataInt        = Signed DataSize
type ArrayIndex     = Unsigned IndexSize
type Discr          = Unsigned DiscrSize
type OutputSignal   = Maybe (DataInt, ArrayIndex)

-- =================================
-- = Cell Definition and Accessors =
-- =================================
type Cell = Maybe (DataInt, Discr)

valid :: Cell -> Bool
valid (Just _ ) = True
valid _         = False

value :: Cell -> DataInt
value (Just (v,_)) = v
value _            = 0

discr :: Cell -> Discr
discr (Just (_,d)) = d
discr _            = 0

equalDiscr :: Cell -> Cell -> Bool
equalDiscr (Just (_,d1)) (Just (_,d2)) = d1 == d2
equalDiscr _             _             = False

-- =======================
-- = Reducer State types =
-- =======================
data DiscrState = Discr { prevIndex :: ArrayIndex
                        , curDiscr  :: Unsigned DiscrSize
                        }

type InputState = Vec (AdderDepth + 1) Cell

type FpState    = Vec AdderDepth Cell

data ResState   = Res { cellMem  :: Vec DiscrRange Cell
                      , indexMem :: Vec DiscrRange ArrayIndex
                      }

-- ===========================================================
-- = Discrimintor: Hands out new discriminator to the system =
-- ===========================================================
discriminator :: DiscrState
              -> ArrayIndex
              -> (DiscrState, (Discr, Bool))
discriminator (Discr {..}) index = ( Discr { prevIndex = index
                                           , curDiscr  = curDiscr'
                                           }
                                   , (curDiscr', newDiscr)
                                   )
  where
    newDiscr              = index /= prevIndex
    curDiscr' | newDiscr  = curDiscr + 1
              | otherwise = curDiscr

-- =====================================================
-- = Input Buffer: Buffers incoming inputs when needed =
-- =====================================================
inputBuffer :: InputState
            -> (Discr,DataInt,Shift)
            -> (InputState, (Cell,Cell))
inputBuffer buf (discr, dataInt, shift) = (buf', (cell1, cell2))
  where
    -- Write new input value
    nextValids        = (map valid buf) <<+ True
    buf''             = zipWith selects buf nextValids
    -- Shift buffer values
    buf' | shift == 2 = Nothing +>> (Nothing +>> buf'')
         | shift == 1 = Nothing +>> buf''
         | otherwise  = buf''
    -- Read cells
    cell1             = last buf
    cell2             = last (init buf)

    selects :: Cell -> Bool -> Cell
    selects Nothing True = Just (dataInt, discr)
    selects cell    _    = cell

-- ============================================
-- = Simulated pipelined floating point adder =
-- ============================================
fpAdder :: FpState
        -> (Cell, Cell)
        -> (FpState, Cell)
fpAdder pipe (cell1, cell2) = (pipe', out)
  where
    newHead | valid cell1 = Just (value cell1 + value cell2, discr cell1)
            | otherwise   = Nothing
    pipe'                 = newHead +>> pipe
    out                   = last pipe

-- =========================
-- = Partial Result Buffer =
-- =========================
resBuffer :: ResState
          -> (Bool, Discr, ArrayIndex, Cell, Cell)
          -> (ResState, (Cell, OutputSignal))
resBuffer (Res {..}) (newDiscr, newDiscrVal, index, pipeCell, newCell) = ( Res { cellMem = cellMem'
                                                                               , indexMem = indexMem'
                                                                               }
                                                                         , (resMemOut, redOut)
                                                                         )
  where
    -- Purge completely reduced results from the system
    cleanMem  | newDiscr                      = replace newDiscrVal Nothing cellMem
              | otherwise                     = cellMem
    -- If a partial is fed  back to the pipeline, make its location invalid
    cellMem'                                  = replace (discr pipeCell) newCell cleanMem
    -- Update Index LUT when new Discr enters circuit
    indexMem' | newDiscr                      = replace newDiscrVal index indexMem
              | otherwise                     = indexMem
    -- Value fed back into circuit
    resMemOut | valid pipeCell                = cellMem !! (discr pipeCell)
              | otherwise                     = Nothing
    -- Value purged from the circuit
    redOut    | valid (cellMem !! newDiscrVal) = Just (value (cellMem !! newDiscrVal), indexMem !! newDiscrVal)
              | otherwise                      = Nothing

-- ================================================================
-- = Controller guides correct inputs to the floating point adder =
-- ================================================================
controller :: (Cell, Cell, Cell, Cell)
           -> (Cell, Cell, Shift, Cell)
controller (inp1, inp2, pipe, fromResMem) = (arg1, arg2, shift, toResMem)
  where
    (arg1, arg2, shift, toResMem)
      | equalDiscr pipe fromResMem = (pipe   , fromResMem, 0, Nothing)
      | equalDiscr pipe inp1       = (pipe   , inp1      , 1, Nothing)
      | equalDiscr inp1 inp2       = (inp1   , inp2      , 2, pipe)
      | valid inp1                 = (inp1   , Nothing   , 1, pipe)
      | otherwise                  = (Nothing, Nothing   , 0, pipe)


-- =============================================
-- = Reducer: Wrap up all the above components =
-- =============================================
reducer :: (Signal DataInt, Signal ArrayIndex) -> Signal OutputSignal
reducer (dataIn,index) = redOut
  where
    (newDiscrVal,newDiscr)     = mealyB discriminator initDiscrState index
    (inp1,inp2)                = mealyB inputBuffer   initInputState  (newDiscrVal,dataIn,shift)
    pipe                       = mealyB fpAdder       initPipeState (arg1,arg2)
    (fromResMem,redOut)        = mealyB resBuffer     initResState (newDiscr,newDiscrVal,index,pipe,toResMem)
    (arg1,arg2,shift,toResMem) = fmapB controller (inp1, inp2, pipe, fromResMem)

topEntity = reducer

fmapB f = unbundle . fmap f . bundle

initDiscrState :: DiscrState
initDiscrState = Discr { prevIndex = 255
                       , curDiscr  = 127
                       }

initInputState :: InputState
initInputState = repeat Nothing

initPipeState :: FpState
initPipeState  = repeat Nothing

initResState :: ResState
initResState = Res { cellMem  = repeat Nothing
                   , indexMem = repeat 0
                   }
