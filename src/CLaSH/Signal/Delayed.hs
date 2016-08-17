{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Signal.Delayed
  ( -- * Delay-annotated synchronous signals
    DSignal
  , delay
  , delayI
  , feedback
    -- * Signal \<-\> DSignal conversion
  , fromSignal
  , toSignal
    -- * List \<-\> DSignal conversion (not synthesisable)
  , dfromList
    -- ** lazy versions
  , dfromList_lazy
    -- * Experimental
  , unsafeFromSignal
  , antiDelay
  )
where

import Data.Default                  (Default(..))
import GHC.TypeLits                  (KnownNat, Nat, type (+))
import Prelude                       hiding (head, length, repeat)

import CLaSH.Sized.Vector            (Vec)
import CLaSH.Signal.Explicit         (SystemClock, systemClock)
import CLaSH.Signal.Delayed.Explicit (DSignal', dfromList, dfromList_lazy,
                                      delay', delayI', feedback, fromSignal,
                                      toSignal, unsafeFromSignal, antiDelay)

{- $setup
>>> :set -XDataKinds
>>> :set -XTypeOperators
>>> import CLaSH.Prelude
>>> let delay3 = delay (0 :> 0 :> 0 :> Nil)
>>> let delay2 = delayI :: DSignal n Int -> DSignal (n + 2) Int
>>> :{
let mac :: DSignal 0 Int -> DSignal 0 Int -> DSignal 0 Int
    mac x y = feedback (mac' x y)
      where
        mac' :: DSignal 0 Int -> DSignal 0 Int -> DSignal 0 Int
             -> (DSignal 0 Int, DSignal 1 Int)
        mac' a b acc = let acc' = a * b + acc
                       in  (acc, delay (singleton 0) acc')
:}

-}

-- | A synchronized signal with samples of type @a@, synchronized to \"system\"
-- clock (period 1000), that has accumulated @delay@ amount of samples delay
-- along its path.
type DSignal (delay :: Nat) a = DSignal' SystemClock delay a

-- | Delay a 'DSignal' for @d@ periods.
--
-- @
-- delay3 :: 'DSignal' n Int -> 'DSignal' (n + 3) Int
-- delay3 = 'delay' (0 ':>' 0 ':>' 0 ':>' 'Nil')
-- @
--
-- >>> sampleN 6 (delay3 (dfromList [1..]))
-- [0,0,0,1,2,3]
delay :: forall a n d . KnownNat d
      => Vec d a
      -> DSignal n a
      -> DSignal (n + d) a
delay = delay' systemClock

-- | Delay a 'DSignal' for @m@ periods, where @m@ is derived from the context.
--
-- @
-- delay2 :: 'DSignal' n Int -> 'DSignal' (n + 2) Int
-- delay2 = 'delayI'
-- @
--
-- >>> sampleN 6 (delay2 (dfromList [1..]))
-- [0,0,1,2,3,4]
delayI :: (Default a, KnownNat d)
       => DSignal n a
       -> DSignal (n + d) a
delayI = delayI' systemClock
