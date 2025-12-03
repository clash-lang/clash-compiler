{-# LANGUAGE CPP #-}

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE GADTs, UndecidableInstances #-}
module T1591 where

import Clash.Prelude

import Data.Singletons
import Data.Singletons.TH

#if MIN_VERSION_singletons(3,0,0)
import Prelude.Singletons
#else
import Data.Singletons.Prelude
#endif

$(singletons [d|
    countStates :: Nat -> Nat -> Nat
    countStates a b = 1 + aux a where
      aux :: Nat -> Nat
      aux s
        | s == 0       = 0
        | s >= max a b = aux (s - max a b)
        | otherwise    = 1 + aux (s + a)
  |])
data NextLineMask this that =
  NextLineMask
  { _nextIndex    :: Index (CountStates this that)
  } deriving (Generic)

nextLineMask :: forall this that. (KnownNat this, KnownNat that) => Index (CountStates this that) -> NextLineMask this that
nextLineMask x = NextLineMask x

topEntity :: Signal System (Index (CountStates 3 4))
          -> Signal System (NextLineMask 3 4)
topEntity = fmap nextLineMask
