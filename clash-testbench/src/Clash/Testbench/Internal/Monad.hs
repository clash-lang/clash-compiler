module Clash.Testbench.Internal.Monad where

import Algebra.PartialOrd
import Control.Arrow (second)
import Control.Monad.State.Lazy (StateT, liftIO, get, gets, modify, forM, evalStateT)
import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (uncons, partition)
import Data.Maybe (catMaybes)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A

import Clash.Signal.Internal (Signal(..), head#, tail#)
import Clash.Prelude
  ( KnownDomain(..), BitPack(..), NFDataX, Enable, Clock, Reset
  , toEnable, unsafeToReset
  )

import Clash.Testbench.Signal
import Clash.Testbench.Internal.ID
import Clash.Testbench.Internal.Signal hiding (TBSignal, TBClock, TBReset, TBEnable)
import qualified Clash.Testbench.Internal.Signal as Internal

-- | Simulation mode
data Simulator =
    Internal
    -- ^ Internal pure Haskell based simulation
  | External
    -- ^ Co-Simulation using an external simulator

data DomainSpecificIDSource =
    DSClock  { domainFromDS :: Int }
  | DSReset  { domainFromDS :: Int }
  | DSEnable { domainFromDS :: Int }

instance Eq DomainSpecificIDSource where
  DSClock{}  == DSClock{}  = True
  DSReset{}  == DSReset{}  = True
  DSEnable{} == DSEnable{} = True
  _          == _          = False

instance Ord DomainSpecificIDSource where
  compare DSClock{}  DSClock{}  = EQ
  compare DSClock{}  _          = GT
  compare _          DSClock{}  = LT
  compare DSReset{}  DSReset{}  = EQ
  compare DSEnable{} DSEnable{} = EQ
  compare DSEnable{} _          = LT
  compare _          DSEnable{} = GT

type KnownSignals (s :: Stage) = S.Set (SomeSignal s)

data ST =
  ST
    { idCount    :: ID 'USER Int
    , signals    :: KnownSignals 'USER
    , monitors   :: KnownSignals 'USER
    , simStepRef :: IORef Int
    , simMode    :: IORef Simulator
    , domIds     :: M.Map String (S.Set DomainSpecificIDSource)
    }

data Testbench =
  Testbench
    { tbSignals    :: [SomeSignal 'FINAL]
    , tbLookupID   :: ID 'FINAL () -> SomeSignal 'FINAL
    , tbSimStepRef :: IORef Int
    , tbSimMode    :: IORef Simulator
    }

instance Show ST where
  show ST{..} =
    "ST {"
    <> show idCount <> ", "
    <> show (S.toAscList signals)
    <> "}"

-- | The 'TB' monad defines the context in which the test bench gets
-- be created by the user. To this end, the user can lift any Clash
-- 'Clash.Signal.Signal' or signal function into 'TB' using the '@@'
-- operator. The lifted signal / signal function then can be applied
-- to 'IO' driven inputs or the outputs can be post-processed inside
-- 'IO'.
--
-- Note that 'TB' offers a construction environment, i.e., it is used
-- to describe the test bench structure. However, the test bench is
-- not executed inside 'TB'.
type TB a = StateT ST IO a

nextFreeID :: TB (ID 'USER Int)
nextFreeID = do
  i <- gets idCount
  modify $ \st -> st { idCount = i + 1 }
  return i

mind ::
  (KnownDomain dom, NFDataX a, BitPack a) =>
  (TBSignal dom a -> SomeSignal 'USER) ->
  TBSignal dom a ->
  TB (TBSignal dom a)
mind t s = case signalId s of
  NoID -> do
    FreeID i <- nextFreeID
    let s' = s { signalId = SignalID i }
    modify $ \st@ST{..} -> st { signals = S.insert (t s') signals }
    return s'
  _ -> do
    modify $ \st@ST{..} ->
      st { signals = S.insert (t s) $ case S.lookupIndex (t s) signals of
             Nothing -> signals
             Just i  -> S.deleteAt i signals
         }
    return s

mindSignal ::
  (KnownDomain dom, NFDataX a, BitPack a) =>
  TBSignal dom a ->
  TB (TBSignal dom a)
mindSignal = mind SomeSignal

monitor ::
  KnownDomain dom =>
  TBSignal dom Bool ->
  TB (TBSignal dom Bool)
monitor = mind SomeMonitor

type family ArgOf a where
  ArgOf (a -> b) = a

-- | Lift clash circuitry into 'TB'.
class LiftTB a where
  -- | The operator lifts a signal or signal function into 'TB'. As
  -- the operator is polyvariadic lifting functions of any arity and
  -- shape is supported. Additionally, every lifted signal / signal
  -- function must be given a name, which is used to identify the top
  -- module in case the resulting test bench gets simulated using an
  -- external simulator.
  (@@) :: String -> a

  liftTB :: String -> [ID 'USER ()]
    -> IO (IORef b, IO (ArgOf a, (ArgOf a -> ArgOf a) -> b -> b)) -> a


defTBLift :: (LiftTB a, a ~ (ArgOf a -> b)) => String -> ArgOf a -> b
defTBLift name x =
  liftTB name [] ((\r -> (r, (,($)) <$> readIORef r)) <$> newIORef x) x

instance
  ( KnownDomain domA, KnownDomain domB
  , domA ~ domB, a ~ a'
  , NFDataX a, BitPack a
  ) => LiftTB (Signal domA a -> TB (TBSignal domB a'))
 where
  (@@) = defTBLift

  liftTB signalName (reverse -> dependencies) exec origin = do
    mode <- simMode <$> get
    extVal <- liftIO $ newIORef Nothing
    expectations <- liftIO $ newIORef []

    ST{..} <- get
    (signalRef, run) <- liftIO exec
    simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

    let
      signalCurVal = do
        readIORef mode >>= \case
          Internal -> do
            (head# -> x, step) <- run
            local <- readIORef simStepRef
            world <- readIORef simStepCache
            -- THOUGHT: one could also use an individual simulation
            -- counter per domain allowing for multiple steps to be
            -- simulated at once, if necessary.
            if local == world
            then return x
            else do
              modifyIORef signalRef $ step tail#
              writeIORef simStepCache world
              return x
          External -> readIORef extVal >>= \case
            Nothing -> error "No Value"
            Just x  -> return x

    mind SomeSignal $ Internal.SimSignal
      { signalId     = NoID
      , signalUpdate = Just (writeIORef extVal . Just)
      , signalExpect = modifyIORef expectations . (:)
      , signalVerify = do
          step <- readIORef simStepRef
          value <- signalCurVal
          expct <- readIORef expectations

          let
            (cur, later) =
              partition (flip leq $ Expectation (step + 1, undefined)) expct

          writeIORef expectations later

          return$ fmap fst $ uncons $ catMaybes
            $ map ((value &) . snd . expectation) cur

      , signalPrint  = Nothing
      , vpiInstance  = Nothing
      , ..
      }

instance
  ( KnownDomain dom, LiftTB (b -> c)
  , arg ~ TBSignal dom a
  ) => LiftTB ((Signal dom a -> b) -> arg -> c)
 where
  (@@) = defTBLift

  liftTB name deps exec sf s =
    flip (liftTB name (SomeID (signalId s) : deps)) (sf $ origin s)
      $ (<$> exec) $ second $ (=<<) $ \(sf', cont) -> do
        v <- signalCurVal s
        return (sf' $ pure v, cont . (\f sf'' -> f . sf'' . (v :-)))

instance
  ( KnownDomain dom, LiftTB (b -> c)
  , arg ~ TBClock dom
  ) => LiftTB ((Clock dom -> b) -> arg -> c)
 where
  (@@) = defTBLift

  liftTB name deps exec sf c =
    flip (liftTB name (SomeID (clockId c) : deps)) (sf $ clock c)
      $ (<$> exec) $ second $ (=<<) $ \(sf', cont) ->
        return (sf' $ clock c, cont . (.))

instance
  ( KnownDomain dom, LiftTB (b -> c)
  , arg ~ TBReset dom
  ) => LiftTB ((Reset dom -> b) -> arg -> c)
 where
  (@@) = defTBLift

  liftTB name deps exec sf r =
    flip (liftTB name (SomeID (resetId r) : deps)) (sf $ reset r)
      $ (<$> exec) $ second $ (=<<) $ \(sf', cont) -> do
        v <- resetCurVal r
        return (sf' $ unsafeToReset $ pure v, cont . (.))

instance
  ( KnownDomain dom, LiftTB (b -> c)
  , arg ~ TBEnable dom
  ) => LiftTB ((Enable dom -> b) -> arg -> c)
 where
  (@@) = defTBLift

  liftTB name deps exec sf e =
    flip (liftTB name (SomeID (enableId e) : deps)) (sf $ enable e)
      $ (<$> exec) $ second $ (=<<) $ \(sf', cont) -> do
        v <- enableCurVal e
        return (sf' $ toEnable $ pure v, cont . (.))

runTB :: Simulator -> TB a -> IO (a, Testbench)
runTB mode testbench = do
  simStepRef <- newIORef 0
  simMode <- newIORef mode
  evalStateT (testbench >>= finalize) ST
    { idCount  = 0
    , signals  = S.empty
    , monitors = S.empty
    , domIds   = M.empty
    , ..
    }
 where
  finalize r = do
    ST { signals, simStepRef, simMode } <- get
    tbSignals <- forM (S.toAscList signals) $ \case
      SomeSignal s  -> SomeSignal <$> finalizeSignal s
      SomeMonitor s -> SomeMonitor <$> finalizeSignal s

    FreeID n <- gets idCount
    let a :: A.Array Int (SomeSignal 'FINAL)
        a = A.array (0, n-1)
          $ map (\s -> ((idToInt . signalId) `onAllSignalTypes` s, s))
            tbSignals

    return
      ( r
      , Testbench
          { tbSimStepRef = simStepRef
          , tbSimMode = simMode
          , tbLookupID = (a A.!) . idToInt
          , ..
          }
      )

  finalizeSignal ::
    Internal.TBSignal 'USER dom a ->
    TB (Internal.TBSignal 'FINAL dom a)

  finalizeSignal = \case
    SimSignal{..} -> do
      deps <- mapM fixAutoDomIds dependencies
      return $ SimSignal
        { signalId   = case signalId of
            NoID       -> NoID
            SignalID x -> SignalID x
        , dependencies = deps
        , ..
        }
    IOInput{..} ->
      return $ IOInput
        { signalId = case signalId of
            NoID       -> NoID
            SignalID x -> SignalID x
        , ..
        }
    Internal.TBSignal{..} ->
      return $ Internal.TBSignal
        { signalId = case signalId of
            NoID       -> NoID
            SignalID x -> SignalID x
        , ..
        }

  fixAutoDomIds :: ID 'USER () -> TB (ID 'FINAL ())
  fixAutoDomIds (SomeID s) = case s of
    NoID       -> return $ SomeID $ NoID
    SignalID x -> return $ SomeID $ SignalID x
    ClockID x  -> updAutoDom DSClock  (SomeID . ClockID)  x
    ResetID x  -> updAutoDom DSReset  (SomeID . ResetID)  x
    EnableID x -> updAutoDom DSEnable (SomeID . EnableID) x

  updAutoDom ds c = \case
    UserDef x   -> return $ c x
    AutoDom str -> do
      sm <- gets domIds
      case M.lookup str sm of
        Just s -> case S.lookupIndex (ds 0) s of
          Just i  -> return $ c $ domainFromDS $ S.elemAt i s
          Nothing -> nextAutoDomId ds c str sm (`S.insert` s)
        Nothing -> nextAutoDomId ds c str sm S.singleton

  nextAutoDomId ds c str sm upd = do
    FreeID x <- nextFreeID
    modify $ \st -> st { domIds = M.insert str (upd $ ds x) sm }
    return $ c x
