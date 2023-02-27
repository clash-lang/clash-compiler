module Simulate where

import Prelude hiding (Word, print, putStr, putStrLn)

import Data.Int (Int64)
import Data.Coerce (Coercible)
import Data.Typeable (Typeable)
import Data.Bits (complement)
import Data.List (intercalate, zip5)
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free)

import qualified Data.ByteString.Char8 as B

import Clash.Prelude
  ( Lift, Generic, BitPack, Signed, Bit, SNat(..)
  , low, high, pack, unpack, resize
  )

import Clash.FFI.Monad
import Clash.FFI.VPI.Info
import Clash.FFI.VPI.IO
import Clash.FFI.VPI.Callback
import Clash.FFI.VPI.Module
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Port

type Word = Signed 4
data OPC a = ADD | MUL | Imm a | Pop | Push
  deriving (Show, Lift, Generic, BitPack)

data State =
  State
    { top     :: Module
    , clkIn   :: Port
    , rstIn   :: Port
    , enbIn   :: Port
    , dataIn  :: Port
    , dataOut :: Port
    , steps   :: Int
    , clock   :: Bit
    }

foreign export ccall "clash_ffi_main"
  ffiMain :: IO ()

ffiMain :: IO ()
ffiMain = runSimAction $ do
  --------------------------
  -- print simulator info --
  --------------------------
  putStrLn "[ Simulator Info ]"
  Info{..} <- receiveSimulatorInfo
  simPutStrLn infoProduct
  simPutStrLn infoVersion
  putStrLn ""

  -----------------------
  -- print top modules --
  -----------------------
  putStrLn "[ Top Modules ]"
  tops <- topModules
  topNames <- mapM (receiveProperty Name) tops
  mapM_ simPutStrLn topNames
  putStrLn ""

  -- iverilog runs into problems if iterated objects are used as a
  -- long-term reference. Hence, they only should be used for
  -- analyzing the architecture upfront. For long-term references to
  -- be reusable during simulation, the objects should be queried via
  -- their architectural name reference instead.
  top <- getByName (Nothing @Object) $ head topNames

  -----------------
  -- print ports --
  -----------------
  putStrLn "[ Ports ]"
  ports <- modulePorts top
  -- Note that values of composed types, like `String`/`CString`, must
  -- be "received", while value of core types, such as `Int`/`CInt`,
  -- can by "get". The reason is that "receivable" types need to be
  -- memory copied on the heap, while "gettable" types live on the
  -- stack. Clash-FFI only offers to either "receive" or to "get"
  -- values for supported types at the moment, so take care that the
  -- right methodology is used.
  names   <- mapM (receiveProperty Name)      ports
  sizes   <- mapM (getProperty     Size)      ports
  indices <- mapM (getProperty     PortIndex) ports
  dirs    <- mapM (getProperty     Direction) ports
  let realNames = [ "CLK", "RST", "ENB", "OPC", "RESULT" ]
  mapM_ printPort $ zip5 (map B.unpack names) sizes indices dirs realNames
  putStrLn ""

  -- get long-term references for all input and output ports
  [ clkIn, rstIn, enbIn, dataIn, dataOut ] <- mapM (getByName $ Just top) names

  let ?state = State {steps = 7, clock = low, ..}

  ---------------------------------
  -- start the actual simulation --
  ---------------------------------
  putStrLn "[ Simulation start ]"
  putStrLn ""
  putStrLn " STEP ; CLK  ; RST  ; ENB  ;         OPC          ;        RESULT"
  putStrLn "------;------;------;------;----------------------;----------------------"

  void $ registerCallback
    CallbackInfo
      { cbReason  = EndOfSimulation
      , cbRoutine = const $ do
          runSimAction (putStrLn "" >> putStrLn "[ Simulation done ]")
          return 0
      , cbIndex   = 0
      , cbData    = B.empty
      }

  nextCB ReadWriteSynch 0 assignInputs

 where
  printPort (n, s, i, d, r) =
    let str = show i <> ": " <> n <> "[" <> show (s - 1) <> ":0]"
    in putStrLn $ str <> replicate (14 - length str) ' ' <> printDir d <> " " <> r

  printDir = \case
    1 -> "<="  -- input
    2 -> "=>"  -- output
    3 -> "<=>" -- inout
    4 -> "<=>" -- mixed input-output
    _ -> "x"   -- no direction

assignInputs :: (?state :: State) => SimAction
assignInputs = do
  SimTime time <- receiveTime Sim $ Just top

  clkUpd <- sendV clkIn clock

  (rstUpd, enbUpd) <-
    if clock == low && steps == 7
    then (,) <$> sendV rstIn low <*> sendV enbIn high
    else (,) <$> return Nothing  <*> return Nothing

  inUpd <-
    if clock == low
    then case steps of
      7 -> sendV dataIn (Imm 1)
      6 -> sendV dataIn Push
      5 -> sendV dataIn (Imm 2)
      4 -> sendV dataIn Push
      3 -> sendV dataIn Pop
      2 -> sendV dataIn Pop
      1 -> sendV dataIn Pop
      0 -> sendV dataIn ADD
      _ -> return Nothing
    else
      return Nothing

  print updates { time, clkUpd, rstUpd, enbUpd, inUpd }

  let ?state = ?state { clock = complement clock }

  if clock == low
  then nextCB ReadWriteSynch 1 assignInputs
  else nextCB ReadOnlySynch 1 readOutputs

 where
  State{..} = ?state

  sendV port v = do
    sendValue port (BitVectorVal SNat $ pack v) $ InertialDelay $ SimTime 0
    return $ Just v

readOutputs :: (?state :: State) => SimAction
readOutputs = do
  SimTime time <- receiveTime Sim $ Just top
  receiveValue VectorFmt dataOut >>= \case
    BitVectorVal SNat v ->
      print updates
        { time
        , outUpd = Just $ unpack $ resize v
        }
    _ -> return ()

  when (steps > 0) $ do
    let ?state = ?state { steps = steps - 1 }
    nextCB ReadWriteSynch 1 assignInputs

 where
  State{..} = ?state

data Updates =
  Updates
    { time   :: Int64
    , clkUpd :: Maybe Bit
    , rstUpd :: Maybe Bit
    , enbUpd :: Maybe Bit
    , inUpd  :: Maybe (OPC Word)
    , outUpd :: Maybe (Maybe Word)
    }

instance Show Updates where
  show Updates{..} =
    intercalate ";"
      [ "   " <> (if time < 10 then " " else "") <> show time <> " "
      , maybe (replicate  6 ' ') printBit               clkUpd
      , maybe (replicate  6 ' ') printBit               rstUpd
      , maybe (replicate  6 ' ') printBit               enbUpd
      , maybe (replicate 22 ' ') (printValue 22 " <= ") inUpd
      , maybe (replicate 22 ' ') (printValue 22 " => ") outUpd
      ]
   where
    printBit b
      | b == high = " <= 1 "
      | otherwise = " <= 0 "

    printValue n dir x =
      let
        s1 = show x <> ": "
        s2 = show (pack x) <> " "
        m = n - length s1 - length s2 - 4
      in
        dir <> s1 <> replicate m ' ' <> s2

updates :: Updates
updates = Updates 0 Nothing Nothing Nothing Nothing Nothing

nextCB ::
  (Maybe Object -> Time -> CallbackReason) ->
  Int64 ->
  SimAction ->
  SimAction
nextCB reason time action =
  void $ registerCallback
    CallbackInfo
      { cbReason  = reason Nothing (SimTime time)
      , cbRoutine = const (runSimAction action >> return 0)
      , cbIndex   = 0
      , cbData    = B.empty
      }

getByName ::
  (Coercible a Object, Show a, Typeable a, Coercible Object b) =>
  Maybe a -> B.ByteString -> SimCont o b
getByName m name = do
  ref <- liftIO $ newCString $ B.unpack name
  obj <- getChild ref m
  liftIO $ free ref
  return obj

putStr :: String -> SimAction
putStr = simPutStr . B.pack

putStrLn :: String -> SimAction
putStrLn = simPutStrLn . B.pack

print :: Show a => a -> SimAction
print = simPutStrLn . B.pack . show
