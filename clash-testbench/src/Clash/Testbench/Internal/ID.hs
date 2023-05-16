{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

'Clash.Testbench.Simulate.TB' lifted signals.
-}

module Clash.Testbench.Internal.ID
  ( SIGNAL
  , DOMAIN
  , IDT
  , ID(..)
  , MID(..)
  , idToInt
  ) where

import GHC.Arr (Ix(..))

-- | ID reference for the standard Clash 'Clash.Signal.Signal' type.
data SIGNAL
-- | ID reference for domain specific special Clash types like
-- 'Clash.Internal.Signal.Clock', 'Clash.Internal.Signal.Reset', or
-- 'Clash.Internal.Signal.Enable'.
data DOMAIN

-- | Some closed type family used for capturing the available ID types.
type family IDT a where
  IDT DOMAIN = DOMAIN
  IDT a      = SIGNAL

-- | The ID data constructors for holding the different ID types.
data ID a where
  -- the pool of free IDs
  FreeID   :: Int -> ID Int
  -- the different ID types
  SignalID :: Int -> ID SIGNAL
  ClockID  :: Int -> ID DOMAIN
  ResetID  :: Int -> ID DOMAIN
  EnableID :: Int -> ID DOMAIN
  -- signals that result from higher order transformations may not be
  -- tracked explicitly
  NoID     :: ID SIGNAL
  -- wrapper type for passing different ID types around. Note that IDs
  -- of the free id pool are excluded here.
  SomeID   :: (a ~ IDT a) => ID a -> ID ()

-- | Accesses the encapsulated 'Int' of an 'ID'. Note that 'NoID' is
-- mapped to zero. Hence, 'SignalID' should only be used on positive
-- values to ensure proper behavior.
idToInt :: ID a -> Int
idToInt = \case
  FreeID x   -> x
  SignalID x -> x
  ClockID x  -> x
  ResetID x  -> x
  EnableID x -> x
  NoID       -> 0
  SomeID x   -> idToInt x

-- | ID context switch, guarded via 'Maybe'.
class MID a where
  mID :: ID b -> Maybe (ID a)

instance MID () where
  mID = \case
    x@SomeID{} -> Just x
    _          -> Nothing

instance MID Int where
  mID = \case
    x@FreeID{} -> Just x
    _          -> Nothing

instance MID SIGNAL where
  mID = \case
    x@NoID{}     -> Just x
    x@SignalID{} -> Just x
    SomeID x     -> mID x
    _            -> Nothing

instance MID DOMAIN where
  mID = \case
    x@ClockID{}  -> Just x
    x@ResetID{}  -> Just x
    x@EnableID{} -> Just x
    SomeID x     -> mID x
    _            -> Nothing

instance Num (ID Int) where
  FreeID x + FreeID y   = FreeID $ x + y
  FreeID x - FreeID y   = FreeID $ x - y
  FreeID x * FreeID y   = FreeID $ x * y
  abs (FreeID x)    = FreeID $ abs x
  signum (FreeID x) = FreeID $ signum x
  fromInteger       = FreeID . fromInteger

instance Eq (ID a) where
  (==) = \case
    FreeID x   -> \case
      FreeID y -> x == y
    SignalID x -> \case
      SignalID y -> x == y
      _          -> False
    NoID       -> \case
      NoID -> True
      _    -> False
    ClockID x  -> \case
      ClockID y -> x == y
      _         -> False
    ResetID x  -> \case
      ResetID y -> x == y
      _         -> False
    EnableID x -> \case
      EnableID y -> x == y
      _         -> False
    SomeID x   -> \case
      SomeID y -> case x of
        z@SignalID{} -> (==) (Just z) $ mID y
        z@NoID{}     -> (==) (Just z) $ mID y
        z@ClockID{}  -> (==) (Just z) $ mID y
        z@ResetID{}  -> (==) (Just z) $ mID y
        z@EnableID{} -> (==) (Just z) $ mID y

instance Ord (ID a) where
  compare = \case
    FreeID x     -> \case
      FreeID y -> compare x y
    SignalID x -> \case
      SignalID y -> compare x y
      NoID       -> GT
    NoID       -> \case
      NoID       -> EQ
      SignalID{} -> LT
    ClockID x  -> \y -> case compare x $ idToInt y of
      EQ -> case y of
        ClockID{} -> EQ
        _         -> LT
      v -> v
    ResetID x  -> \y -> case compare x $ idToInt y of
      EQ -> case y of
        ClockID{}  -> GT
        ResetID{}  -> EQ
        EnableID{} -> LT
      v -> v
    EnableID x -> \y -> case compare x $ idToInt y of
      EQ -> case y of
        EnableID{} -> EQ
        _          -> GT
      v -> v
    SomeID x   -> \case
      SomeID y -> case x of
        z@SignalID{} -> maybe LT (compare z) $ mID y
        z@NoID{}     -> maybe LT (compare z) $ mID y
        z@ClockID{}  -> maybe GT (compare z) $ mID y
        z@ResetID{}  -> maybe GT (compare z) $ mID y
        z@EnableID{} -> maybe GT (compare z) $ mID y

instance Show (ID a) where
  show = \case
    FreeID x   -> show x
    SignalID x -> 's' : show x
    NoID       -> "-"
    ClockID x  -> 'c' : show x
    ResetID x  -> 'r' : show x
    EnableID x -> 'e' : show x
    SomeID x   -> show x

instance Ix (ID SIGNAL) where
  {-# INLINE range #-}
  range (NoID,       NoID      ) = [NoID]
  range (NoID,       SignalID x) = NoID : map SignalID (range (1,x))
  range (SignalID x, SignalID y) = map SignalID (range (x,y))
  range (SignalID _, NoID      ) = []

  {-# INLINE unsafeIndex #-}
  unsafeIndex _ = \case
    NoID       -> 0
    SignalID x -> x

  {-# INLINE index #-}
  index b i
    | inRange b i = unsafeIndex b i
    | otherwise   = error $ "Index " <> show i <> " out of range: " <> show b


  {-# INLINE inRange #-}
  inRange (NoID,       NoID)       = (NoID ==)
  inRange (NoID, SignalID x)       = \case
    NoID       -> True
    SignalID i -> inRange (1, x) i
  inRange (SignalID x, SignalID y) = \case
    NoID       -> False
    SignalID i -> inRange (x, y) i
  inRange (SignalID _, NoID)       = const False

instance Ix (ID DOMAIN) where
  {-# INLINE range #-}
  range (x, y) = map ClockID $ range (idToInt x, idToInt y)

  {-# INLINE unsafeIndex #-}
  unsafeIndex = const idToInt

  {-# INLINE index #-}
  index b i
    | inRange b i = unsafeIndex b i
    | otherwise   = error $ "Index " <> show i <> " out of range: " <> show b

  {-# INLINE inRange #-}
  inRange (x, y) = inRange (idToInt x, idToInt y) . idToInt
