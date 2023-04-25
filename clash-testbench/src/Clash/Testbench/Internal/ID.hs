module Clash.Testbench.Internal.ID
  ( Source(..)
  , Stage(..)
  , AnyStage
  , SIGNAL
  , CLOCK
  , RESET
  , ENABLE
  , IDT
  , IDSource
  , ID(..)
  , idToInt
  , isSignalID
  , isClockID
  , isResetID
  , isEnableID
  ) where

import Clash.Prelude (Type)

-- | Source of identification
data Source =
    AutoDom String
    -- ^ Implicit source determined through the domain
    -- (given in reified form here)
  | UserDef Int
    -- ^ User defined source that has some modeled given by the user
  deriving (Eq, Ord)

instance Show Source where
  show = \case
    AutoDom str -> '@' : str
    UserDef i   -> show i

data Stage :: Type where
  -- | The test bench is created in the USER stage. The elements of
  -- the test bench are setup by the user inside the TB monad during
  -- this stage.
  USER  :: Stage
  -- | The FINAL stage is reached once the test bench has been created
  -- and all elements of the setup are known. Furthermore,
  -- post-processing of the setup has passed
  -- successfully. Post-processing also introduces the switch from
  -- USER to FINAL on the type level.
  FINAL :: Stage

-- | ID reference for the standard Clash 'Signal' type.
data SIGNAL
-- | ID reference for the special Clash 'Clock' type.
data CLOCK
-- | ID reference for the special Clash 'Reset' type.
data RESET
-- | ID reference for the special Clash 'Enable' type.
data ENABLE

-- | Some closed type family used for capturing the available ID types.
type family IDT a where
  IDT CLOCK  = CLOCK
  IDT RESET  = RESET
  IDT ENABLE = ENABLE
  IDT a      = SIGNAL

-- | Closed type family, which determines the underlying ID type for
-- each of the different stages.
type family IDSource (s :: Stage) a where
  -- at the final stage all ids must be of type Int
  IDSource 'FINAL a      = Int
  -- clocks, resets and enable signals may have been introduced on the
  -- fly and still need to get some unique id during post-processing.
  IDSource 'USER  CLOCK  = Source
  IDSource 'USER  RESET  = Source
  IDSource 'USER  ENABLE = Source
  -- everything has a known id already
  IDSource s      a      = Int

-- | The ID data constructors for holding the different ID types.
data ID (stage :: Stage) a where
  -- the pool of free IDs is only available a the USER stage and gets
  -- closed at later stages
  FreeID   ::                IDSource 'USER Int    -> ID 'USER Int
  -- the different ID types
  SignalID ::                IDSource stage SIGNAL -> ID stage SIGNAL
  ClockID  ::                IDSource stage CLOCK  -> ID stage CLOCK
  ResetID  ::                IDSource stage RESET  -> ID stage RESET
  EnableID ::                IDSource stage ENABLE -> ID stage ENABLE
  -- wrapper type for passing different ID types around. Note that IDs
  -- of the free id pool cannot be passed around this way.
  SomeID   :: (a ~ IDT a) => ID stage a            -> ID stage ()

-- | This class collects some operations that are available during all
-- stages. It is mostly used to defined the remaining type class
-- instances of 'ID'.
class AnyStage (s :: Stage) where
  mapID :: (Either Int Source -> b) -> ID s a -> b

instance AnyStage 'USER where
  mapID f = \case
    FreeID x   -> f $ Left x
    SignalID x -> f $ Left x
    ClockID  x -> f $ Right x
    ResetID x  -> f $ Right x
    EnableID x -> f $ Right x
    SomeID s   -> mapID f s

instance AnyStage 'FINAL where
  mapID f = \case
    SignalID x -> f $ Left x
    ClockID  x -> f $ Left x
    ResetID x  -> f $ Left x
    EnableID x -> f $ Left x
    SomeID s   -> mapID f s

instance Num (ID 'USER Int) where
  FreeID x + FreeID y   = FreeID $ x + y
  FreeID x - FreeID y   = FreeID $ x - y
  FreeID x * FreeID y   = FreeID $ x * y
  abs (FreeID x)    = FreeID $ abs x
  signum (FreeID x) = FreeID $ signum x
  fromInteger   = FreeID . fromInteger

instance AnyStage s => Eq (ID s a) where
  x == y = mapID (mapID (==) x) y

instance AnyStage s => Ord (ID s a) where
  compare x = mapID (mapID compare x)

instance Show (ID s Int) where
  show (FreeID x) = show x

instance Show (ID s SIGNAL) where
  show (SignalID x) = 's' : show x

instance AnyStage s => Show (ID s CLOCK) where
  show x = 'c' : mapID showEither x

instance AnyStage s => Show (ID s RESET) where
  show x = 'r' : mapID showEither x

instance AnyStage s => Show (ID s ENABLE) where
  show x = 'e' : mapID showEither x

instance AnyStage s => Show (ID s ()) where
  show (SomeID x) = case x of
    SignalID{} -> show x
    ClockID{}  -> show x
    ResetID{}  -> show x
    EnableID{} -> show x

showEither :: (Show a, Show b) => Either a b -> String
showEither = \case
  Left x  -> show x
  Right x -> show x

-- | At the final stage all IDs are of type Int.
idToInt :: ID 'FINAL a -> Int
idToInt = \case
  SignalID x -> x
  ClockID x  -> x
  ResetID x  -> x
  EnableID x -> x
  SomeID s   -> idToInt s

-- | Checks whether the given ID is a signal identifier.
isSignalID :: ID s a -> Bool
isSignalID = \case
  SignalID{} -> True
  SomeID s   -> case s of
    SignalID{} -> True
    _          -> False
  _          -> False

-- | Checks whether the given ID is a clock identifier.
isClockID :: ID s a -> Bool
isClockID = \case
  ClockID{} -> True
  SomeID s  -> case s of
    ClockID{} -> True
    _         -> False
  _         -> False

-- | Checks whether the given ID is a reset identifier.
isResetID :: ID s a -> Bool
isResetID = \case
  ResetID{} -> True
  SomeID s  -> case s of
    ResetID{} -> True
    _         -> False
  _         -> False

-- | Checks whether the given ID is an enable identifier.
isEnableID :: ID s a -> Bool
isEnableID = \case
  ResetID{} -> True
  SomeID s  -> case s of
    ResetID{} -> True
    _         -> False
  _         -> False
