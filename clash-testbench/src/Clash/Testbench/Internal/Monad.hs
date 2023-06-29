{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

The monadic 'Clash.Testbench.Simulate.TB' context used for test
bench creation (internal module).
-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
module Clash.Testbench.Internal.Monad
 ( KnownSignals
 , KnownDomains
 , Testbench(..)
 , TB
 , ST(..)
 , LiftAcc(..)
 , ArgOf
 , LiftTBSignalConstraints
 , LiftTB(..)
 , runTB
 , tbDomain
 , mind
 , progressCheck
 , newHistory
 , memorize
 , history
 ) where

import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.Type.Equality
import Algebra.PartialOrd
import Control.Monad.Extra ((<?>), (<:>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Lazy
  (StateT, liftIO, get, gets, modify, evalStateT, forM_, void, when)
import Data.Function ((&))
import Data.Array.IO (newArray, writeArray, getElems)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (partition, sort, sortBy, groupBy)
import Data.Maybe (catMaybes)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Array as A

import Clash.Prelude
  ( KnownDomain(..), BitPack(..), SDomainConfiguration(..), NFDataX
  , Enable, Clock, Reset
  , ssymbolToString, clockGen, resetGen, enableGen, sameDomain
  , unsafeToReset, unsafeFromReset, toEnable, fromEnable
  )
import Clash.Signal.Internal
  ( Signal(..), head#, tail#
  )

import Clash.Testbench.Signal
import Clash.Testbench.Internal.ID
import Clash.Testbench.Internal.Signal hiding (TBSignal, TBClock, TBReset, TBEnable)
import Clash.Testbench.Internal.Signal
  ( pattern TBSignal, pattern TBClock, pattern TBReset, pattern TBEnable
  )
import qualified Clash.Testbench.Internal.Signal as Internal

-- | The test bench signals that have been captured during test
-- bench creation.
type KnownSignals (s :: Stage) = S.Set (SomeSignal s)

-- | The test bench domains that have been captured during test bench
-- creation.
type KnownDomains (s :: Stage) = M.Map String (SomeDomain s)

-- | The internal state that is manipulated during test bench
-- creation.
data ST =
  ST
    { idSigCount :: ID Int
    -- ^ Counter for generating free IDs to be assigned to signal
    -- (functions)
    , signals :: KnownSignals 'USER
    -- ^ Captured signal (functions)
    , idDomCount :: ID Int
    -- ^ Counter for generating free IDs to be assigned to domains
    , domains :: KnownDomains 'USER
    -- ^ Captured domains
    , simSteps :: Int
    -- ^ Simulation step preset
    , defaultHistorySize :: Int
    -- ^ Default size of the history for all simulated signals, as
    -- long as not explicitly overwritten per signal.
    }

instance Show ST where
  show ST{..} =
    "ST {"
    <> show idSigCount <> ", "
    <> show (S.toAscList signals) <> ", "
    <> show idDomCount <> ", "
    <> show (M.toAscList domains)
    <> "}"

-- | A 'Testbench' is the result of finalizing the test bench creation
-- environment inside the 'TB' context.
data Testbench =
  Testbench
    { tbSignals :: [SomeSignal 'FINAL]
    -- ^ All captured signals
    , tbSignalLookup :: A.Array (ID SIGNAL) (SomeSignal 'FINAL)
    -- ^ Signal lookup via ID (partial array)
    , tbDomains :: [(SomeDomain 'FINAL, [ID SIGNAL])]
    -- ^ All captured domains + references to the captured signals
    -- that are driven by this domain
    , tbDomainLookup :: A.Array (ID DOMAIN) (SomeDomain 'FINAL)
    -- ^ Domain lookup via ID (partial array)
    , tbSimSteps :: Int
    -- ^ Simulation step preset
    }
instance Show Testbench where
  show Testbench{..} =
    "Testbench {"
      <> show tbSignals <> ", "
      <> show tbDomains <> "}"

-- | The 'TB' monad defines the context in which the test bench gets
-- created by the user. Within the 'TB' context, the user can lift any
-- Clash 'Clash.Signal.Signal' or signal function into the context
-- using the '@@' operator. The lifted signal (function) then can be
-- applied to 'IO' driven inputs or the outputs of the lifted signal
-- can be post-processed inside 'IO'.
--
-- Note that 'TB' offers a construction environment, i.e., it is used
-- to describe the test bench structure. The test bench is not
-- executed inside 'TB'.
type TB a = StateT ST IO a

-- | Some type family to access the argument of a function.
type family ArgOf a where
  ArgOf (a -> b) = a

-- | The accumulator state is used to redirect the input for the
-- arguments (resulting from the execution of some 'IO') to the signal
-- transformer. Due to the polyvariadic nature of the 'LiftTB' class
-- (used to support lifting signal functions of any arity), the
-- arguments must be processed in a tail recursive fashion. Moreover,
-- the 'IO' that produces the values to be passed to the signal
-- transformer cannot be executed nor can time proceed until we have
-- processed all arguments. Therefore, to accomplish the
-- transformation, we instead build a transformer, which is extended
-- for each argument (step by step) and is finally applied to the
-- signal (function) at once. We call this operation the "continuation
-- transformation", as it captures the application of the signal
-- function on it's inputs at the current point in time and the signal
-- transformation to be applied at the next point in time.
--
-- Moreover, the accumulator state captures some information that is
-- collected initially and during traversal of the arguments to be
-- available for creation of the lifted signal (function) in the end.
data LiftAcc a b =
  LiftAcc
    { name :: String
      -- ^ The name of the lifted signal (function)
    , deps :: [ID ()]
      -- ^ The dependencies of the lifted signal (function)
    , sfRef :: IORef b
      -- ^ Some IO reference to the lifted signal (function)
    , cont :: IO (a, (a -> a) -> b -> b)
      -- ^ The continuation transformation of the lifted signal
      -- (function)
    }

-- | Lift clash circuitry into 'TB'.
class LiftTB a where
  -- | The operator lifts a signal or signal function into 'TB'. As
  -- the operator is polyvariadic lifting functions of any arity and
  -- shape is supported. Additionally, every lifted signal (function)
  -- must be given a name, which is used to identify the top module in
  -- case the resulting test bench gets simulated using an external
  -- simulator.
  (@@) :: String -> a

  -- | Internal lift for traversing the arguments and the result of
  -- the given signal function.
  liftTB :: TB (LiftAcc (ArgOf a) b) -> a

-- | 'LiftTB' instance constraints for lifting a Clash
-- 'Clash.Signal.Signal' into a test bench
-- 'Clash.Testbench.Signal.TBSignal'.
type LiftTBSignalConstraints domA domB a a' =
  ( KnownDomain domA, KnownDomain domB
  , domA ~ domB, a ~ a'
  , NFDataX a, BitPack a
  , Show a
  )

instance
  LiftTBSignalConstraints domA domB a a' =>
  LiftTB (Signal domA a -> TB (TBSignal domB a'))
 where
  (@@) = initializeLiftTB

  liftTB exec signalOrigin = do
    extVal <- liftIO $ newIORef Nothing
    expectations <- liftIO $ newIORef []

    LiftAcc{..} <- exec
    TBDomain{..} <- tbDomain @domA
    -- Initial progress ensures that the value reference and the
    -- signal function reference are updated immediately after the
    -- first call to `signalCurVal`, which is required for the first
    -- continuation transformation to be applied on the initial
    -- values.
    ifProgress <- progressCheck simStepRef True
    vRef <- liftIO $ newIORef undefined
    signalHistory <- newHistory

    let
      signalCurVal = \case
        Internal -> ifProgress
          <?> do
            (head# -> x, step) <- cont
            writeIORef vRef x
            modifyIORef sfRef $ step tail#

            -- update the history
            memorize signalHistory x

            return x
          <:>
            readIORef vRef

        External -> readIORef extVal >>= \case
          Nothing -> error "No Value @Signal"
          Just x  -> ifProgress
            <?> memorize signalHistory x >> return x
            <:> return x

    mind SomeSignal $ Internal.SimSignal
      { signalId = NoID
      , signalDeps = reverse deps
      , signalName = name
      , signalUpdate = Just (writeIORef extVal . Just)
      , signalExpect = modifyIORef expectations . (:)
      , signalVerify = \sMode -> Verifier $ \vMode -> do
          curStep <- liftIO $ readIORef simStepRef
          value <- liftIO $ signalCurVal sMode
          expcts <- liftIO $ readIORef expectations

          let sepAt n = partition (`leq` Expectation (n, undefined))
              (xs, later) = sepAt curStep expcts
              (_, current) = sepAt (curStep - 1) xs

          liftIO $ writeIORef expectations (current <> later)
          mapM_ ((`verifier` vMode) . (value &) . snd . expectation) current
      , signalPrint = Nothing
      , signalPlug = Nothing
      , ..
      }

instance
  ( KnownDomain dom, LiftTB (b -> c)
  , arg ~ TBSignal dom a
  ) => LiftTB ((Signal dom a -> b) -> arg -> c)
 where
  (@@) = initializeLiftTB

  liftTB a sf s = liftTB (upd <$> a) $ sf $ signalOrigin s
   where
    upd acc@LiftAcc{..} =
      acc { deps = SomeID (signalId s) : deps
          , cont = extendVia cont
                     (signalCurVal s Internal)
                     pure
                     (\v f sf' -> f . sf' . (v :-))
          }

instance
  ( KnownDomain dom, LiftTB (b -> c)
  , arg ~ TBClock dom
  ) => LiftTB ((Clock dom -> b) -> arg -> c)
 where
  (@@) = initializeLiftTB

  liftTB a sf c = liftTB (upd <$> knownClock c <*> a) $ sf $ clock c
   where
    knownClock = \case
      tbc@TBClock{} -> return tbc
      AutoClock     -> do
        tbd@TBDomain{..} <- tbDomain
        case domainClock of
          Just tbc -> return tbc
          Nothing  -> do
            clockId <- nextFreeID ClockID
            let tbc = TBClock { clock = clockGen
                              , clockSource = return clockGen
                              , ..
                              }
            updDomain tbd { domainClock = Just tbc }
            return tbc

    upd tbc acc@LiftAcc{..} =
      acc { deps = SomeID (clockId tbc) : deps
          , cont = extendVia cont
                     (pure $ clock tbc)
                     id
                     (const (.))
          }

instance
  ( KnownDomain dom, LiftTB (b -> c)
  , arg ~ TBReset dom
  ) => LiftTB ((Reset dom -> b) -> arg -> c)
 where
  (@@) = initializeLiftTB

  liftTB a sf r = liftTB (upd <$> knownReset r <*> a) $ sf $ reset r
   where
    knownReset = \case
      tbr@TBReset{} -> return tbr
      AutoReset     -> do
        tbd@TBDomain{..} <- tbDomain
        case domainReset of
          Just tbr -> return tbr
          Nothing -> do
            let reset = resetGen

            resetId <- nextFreeID ResetID
            extVal <- liftIO $ newIORef Nothing
            signalRef <- liftIO $ newIORef $ unsafeFromReset reset
            ifProgress <- progressCheck simStepRef False

            let
              resetCurVal = \case
                Internal -> do
                  x :- xr <- readIORef signalRef
                  ifProgress
                    <?> do
                      writeIORef signalRef xr
                      return $ head# xr
                    <:>
                      return x

                External -> readIORef extVal >>= \case
                  Nothing -> error "No Value @Reset"
                  Just x  -> return x

              resetUpdate =
                writeIORef extVal . Just

              tbr = TBReset{..}

            updDomain tbd { domainReset = Just tbr }
            return tbr

    upd tbr acc@LiftAcc{..} =
      acc { deps = SomeID (resetId tbr) : deps
          , cont = extendVia cont
                     (resetCurVal tbr Internal)
                     (unsafeToReset . pure)
                     (const (.))
          }

instance
  ( KnownDomain dom, LiftTB (b -> c)
  , arg ~ TBEnable dom
  ) => LiftTB ((Enable dom -> b) -> arg -> c)
 where
  (@@) = initializeLiftTB

  liftTB a sf e = liftTB (upd <$> knownEnable e <*> a) $ sf $ enable e
   where
    knownEnable = \case
      tbe@TBEnable{} -> return tbe
      AutoEnable     -> do
        tbd@TBDomain{..} <- tbDomain
        case domainEnable of
          Just tbe -> return tbe
          Nothing  -> do
            let enable = enableGen

            enableId <- nextFreeID EnableID
            extVal <- liftIO $ newIORef Nothing
            signalRef <- liftIO $ newIORef (fromEnable enable)
            ifProgress <- progressCheck simStepRef False

            let
              enableCurVal = \case
                Internal -> do
                  x :- xr <- readIORef signalRef
                  ifProgress
                    <?> do
                      writeIORef signalRef xr
                      return $ head# xr
                    <:>
                      return x

                External -> readIORef extVal >>= \case
                  Nothing -> error "No Value @Enable"
                  Just x  -> return x

              enableUpdate =
                writeIORef extVal . Just

              tbe = TBEnable{..}

            updDomain tbd { domainEnable = Just tbe }
            return tbe

    upd tbe acc@LiftAcc{..} =
      acc { deps = SomeID (enableId tbe) : deps
          , cont = extendVia cont
                     (enableCurVal tbe Internal)
                     (toEnable . pure)
                     (const (.))
          }

-- | Initializes the lift of a signal (function).
--
-- Note: this primarily serves as the default implementation of the
-- '@@' operator for the 'LiftTB' class. The implementation is kept
-- separate, however, to not obfuscate users with the additional
-- constraints that are required for this kind of uniform
-- implementation.
initializeLiftTB :: (LiftTB a, a ~ (ArgOf a -> b)) => String -> ArgOf a -> b
initializeLiftTB name x = liftTB accInit x
 where
  accInit = do
   sfRef <- liftIO $ newIORef x
   return LiftAcc
     { deps = []
     , cont = (,($)) <$> readIORef sfRef
     , ..
     }

-- | Creates a new simulation step reference, against which the global
-- reference is compared on execution of the returned progress
-- check. The local reference gets automatically updated to the global
-- one when checking for progress and progress is detected. The
-- boolean argument determines whether progress gets immediately
-- triggered at startup (@True@) or with the first clock change
-- (@False@).
progressCheck :: IORef Int -> Bool -> TB (IO Bool)
progressCheck simStepRef initialProgress = do
  simStepCache <- liftIO ((offset <$> readIORef simStepRef) >>= newIORef)
  return $ do
    globalRef <- readIORef simStepRef
    localRef  <- readIORef simStepCache

    when (globalRef > localRef) $
      writeIORef simStepCache globalRef

    return $ globalRef > localRef
 where
  offset
    | initialProgress = (+ (-1))
    | otherwise       = id

-- | Creates a new 'History' container.
newHistory ::
  TB (History a)
newHistory = do
  size <- gets defaultHistorySize
  historySize <- liftIO $ newIORef size
  historyBufferPos <- liftIO $ newIORef 0
  historyBuffer <- liftIO $ newIORef Nothing
  return History{..}

-- | Memorizes a value inside the given 'History' container.
memorize :: MonadIO m => History a -> a -> m ()
memorize History{..} value =
  liftIO $ readIORef historySize >>= \case
    0 -> return ()
    n -> do
      pos <- readIORef historyBufferPos
      buf <- readIORef historyBuffer >>= \case
        Just a  -> return a
        Nothing -> do
          a <- newArray (0, n-1) Nothing
          writeIORef historyBuffer $ Just a
          return a

      writeArray buf pos $ Just value
      writeIORef historyBufferPos $ pos + 1

-- | Reveals the history of a test bench signal. The returned list is
-- given in temporal order.
history ::
  (KnownDomain dom, MonadIO m) =>
  TBSignal dom a ->
  m [a]
history s = liftIO $ readIORef historyBuffer >>= \case
  Nothing  -> return []
  Just buf -> do
    pos <- readIORef historyBufferPos
    catMaybes . uncurry (flip (<>)) . splitAt pos <$> getElems buf
 where
  History{..} = signalHistory s

-- | Some generalized extender for the accumulated continuation.
extendVia ::
  Monad m =>
  -- the continuation accumulator executed inside the monad @m@
  m (b -> c, e -> f) ->
  -- the monadic action from which the runtime value is taken
  m a ->
  -- a transformer to convert the runtime value to the application domain
  (a -> b) ->
  -- the extension of the continuation resulting from the application
  -- of the given runtime value
  (a -> d -> e) ->
  -- the resulting continuation accumulator
  m (c, d -> f)
extendVia contAcc valueM f g = do
  v <- valueM
  (sf, step) <- contAcc
  return (sf $ f v, step . g v)

-- | Query the next free 'ID' based on the 'ID' context.
class NextFreeID a where
  nextFreeID :: (Int -> ID a) -> TB (ID a)

instance NextFreeID SIGNAL where
  nextFreeID c = do
    i@(FreeID x) <- gets idSigCount
    modify $ \st -> st { idSigCount = i + 1 }
    return $ c x

instance NextFreeID DOMAIN where
  nextFreeID c = do
    i@(FreeID x) <- gets idDomCount
    modify $ \st -> st { idDomCount = i + 1 }
    return $ c x

-- | Adds a test bench signal to the set of known signals
-- automatically assigning it a the next free
-- 'Clash.Testbench.Internal.ID', if the signal does not have some
-- 'Clash.Testbench.Internal.ID' already.
mind ::
  (KnownDomain dom, NFDataX a, BitPack a) =>
  (TBSignal dom a -> SomeSignal 'USER) ->
  TBSignal dom a ->
  TB (TBSignal dom a)
mind t s = case signalId s of
  NoID -> do
    i <- nextFreeID SignalID
    let s' = s { signalId = i }
    modify $ \st@ST{..} -> st { signals = S.insert (t s') signals }
    return s'
  _ -> do
    modify $ \st@ST{..} ->
      st { signals = S.insert (t s) $ case S.lookupIndex (t s) signals of
             Nothing -> signals
             Just i  -> S.deleteAt i signals
         }
    return s

-- | Query the current 'TBDomain' according to the context. If the
-- domain has not already been captured, a new entry gets created
-- automatically.
tbDomain ::
  forall dom.
  KnownDomain dom =>
  TB (TBDomain 'USER dom)
tbDomain = case knownDomain @dom of
  SDomainConfiguration (ssymbolToString -> domainName) _ _ _ _ _ -> do
    M.lookup domainName <$> gets domains >>= \case
      Just (SomeDomain (d :: TBDomain 'USER dom')) -> case sameDomain @dom @dom' of
        Just (Refl :: dom :~: dom') -> return d
        Nothing -> mindDomain domainName
      Nothing -> mindDomain domainName

mindDomain :: forall dom. KnownDomain dom => String -> TB (TBDomain 'USER dom)
mindDomain domainName = do
  simStepRef <- liftIO $ newIORef 0
  let
    domain :: TBDomain 'USER dom
    domain = TBDomain { domainClock = Nothing
                      , domainReset = Nothing
                      , domainEnable = Nothing
                      , ..
                      }
  modify $ \st@ST{..} -> st
    { domains = M.insert domainName (SomeDomain domain) domains
    }
  return domain

updDomain ::
  forall dom.
  KnownDomain dom =>
  TBDomain 'USER dom ->
  TB ()
updDomain domain = case knownDomain @dom of
  SDomainConfiguration domainName _ _ _ _ _ -> do
    idx <- M.findIndex (ssymbolToString domainName) <$> gets domains
    modify $ \st@ST{..} -> st
      { domains = M.updateAt (const $ const $ Just $ SomeDomain domain) idx domains
      }

-- | Finalizes a test bench that has been created inside the 'TB'
-- monad.
runTB :: SimMode -> TB a -> IO (a, Testbench)
runTB mode testbench =
  evalStateT (testbench >>= finalize) ST
    { idSigCount         = 1
    , signals            = S.empty
    , idDomCount         = 0
    , domains            = M.empty
    , simSteps           = 0
    , defaultHistorySize = 100
    }
 where
  finalize r = do
    -- finalize the signals first
    tbSignals <-
      map (finalizeSignal `onAllSignalTypes`) . S.toAscList
        <$> gets signals

    let
      -- group the known signals according to their domains
      tbSignalDoms =
          map (\xs -> (someSignalDomain $ head xs, xs))
        $ groupBy ((==) `on` someSignalDomain)
        $ sortBy (compare `on` someSignalDomain)
          tbSignals

    -- mind domains that may not have been captured yet (just to be sure)
    forM_ tbSignalDoms $ \(_, x : _) ->
      (`onAllSignalTypes` x) $ \(_ :: Internal.TBSignal 'FINAL dom b) ->
        void $ tbDomain @dom

    -- all of the internal state is final at this point
    ST { idSigCount = FreeID n
       , idDomCount = FreeID m
       , simSteps = tbSimSteps
       , ..
       } <- get

    let
      -- finalize the domains
      tbDomains = (`map` tbSignalDoms) $ bimap
        ((finalizeDomain `onAllDomainTypes`) . (M.!) domains)
        (sort . map (signalId `onAllSignalTypes`))

      -- create efficient lookup tables
      tbSignalLookup =
        A.array (NoID, SignalID (n-1)) $ flip map tbSignals $ \s ->
          (signalId `onAllSignalTypes` s, s)

      tbDomainLookup =
        A.array (ClockID 0, ClockID (m-1))
          $ flip concatMap tbDomains $ \(fst -> d) ->
            (, d) <$> (domIds `onAllDomainTypes` d)

    return(r, Testbench{..})

  domIds TBDomain{..} =
    catMaybes
      [ clockId  <$> domainClock
      , resetId  <$> domainReset
      , enableId <$> domainEnable
      ]

  finalizeSignal ::
    (KnownDomain dom, NFDataX a, BitPack a) =>
    Internal.TBSignal 'USER dom a ->
    SomeSignal 'FINAL
  finalizeSignal = SomeSignal . \case
    SimSignal{..} ->
      SimSignal
        { signalCurVal = signalCurVal mode
        , signalVerify = signalVerify mode
        , ..
        }
    IOInput{..} ->
      IOInput
        { signalCurVal = signalCurVal mode
        , ..
        }
    TBSignal{..} ->
      TBSignal
        { signalCurVal = signalCurVal mode
        , ..
        }

  finalizeDomain ::
    KnownDomain dom =>
    TBDomain 'USER dom ->
    SomeDomain 'FINAL
  finalizeDomain = SomeDomain . \case
    TBDomain{..} ->
      TBDomain
        { domainClock = (<$> domainClock) $ \TBClock{..} ->
            TBClock
              { ..
              }
        , domainReset = (<$> domainReset) $ \TBReset{..} ->
            TBReset
              { resetCurVal = resetCurVal mode
              , ..
              }
        , domainEnable = (<$> domainEnable) $ \TBEnable{..} ->
            TBEnable
              { enableCurVal = enableCurVal mode
              , ..
              }
        , ..
        }

someSignalDomain :: SomeSignal s -> String
someSignalDomain = onAllSignalTypes $ \(_ :: Internal.TBSignal s dom a) ->
  case knownDomain @dom of
    SDomainConfiguration (ssymbolToString -> domainName) _ _ _ _ _ ->
      domainName
