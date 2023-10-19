{-|
  Copyright   :  (C) 2019     , Myrtle Software Ltd.
                     2018     , @blaxill
                     2018-2019, QBayLogic B.V.
                     2021     , LUMI GUIDE FIETSDETECTIE B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Signal.Delayed.Internal
  ( -- * Delay-annotated synchronous signals
    DSignal(..)
  , feedback
  , fromSignal
    -- * List \<-\> DSignal conversion (not synthesizable)
  , dfromList
    -- ** lazy versions
  , dfromList_lazy
    -- * Experimental
  , unsafeFromSignal
  , antiDelay
  , forward
  )
where

import Data.Coerce                (coerce)
import Data.Default.Class         (Default(..))
import GHC.TypeLits               (Nat, type (+))
import Language.Haskell.TH.Syntax (Lift)
import Test.QuickCheck            (Arbitrary, CoArbitrary)

import Clash.Promoted.Nat         (SNat)
import Clash.Signal.Internal      (Signal, Domain, fromList, fromList_lazy)
import Clash.XException           (NFDataX)

{- $setup
>>> :set -XDataKinds
>>> :set -XTypeOperators
>>> import Clash.Explicit.Prelude
>>> import qualified Clash.Signal.Delayed.Bundle as DB
>>> :{
let mac
      :: forall dom
       . Clock dom
      -> Reset dom
      -> Enable dom
      -> DSignal dom 0 Int
      -> DSignal dom 0 Int
      -> DSignal dom 0 Int
    mac clk rst en x y = feedback (mac' x y)
      where
        mac'
          :: DSignal dom 0 Int
          -> DSignal dom 0 Int
          -> DSignal dom 0 Int
          -> (DSignal dom 0 Int, DSignal dom 1 Int)
        mac' a b acc = let acc' = a * b + acc
                       in  (acc, delayedI clk rst en 0 acc')
:}

>>> :{
let numbers
      :: forall dom
       . Clock dom
      -> Reset dom
      -> Enable dom
      -> DSignal dom 5 (Int, Int)
    numbers clk rst en = DB.bundle (forward d1 s1, s2)
      where
        s1 :: DSignal dom 4 Int
        s1 = delayed clk rst en (100 :> 10 :> 5 :> 1 :> Nil) (pure 200)
        s2 :: DSignal dom 5 Int
        s2 = fmap (2*) $ delayN d1 0 en clk s1
:}

-}

-- | A synchronized signal with samples of type @a@, synchronized to clock
-- @clk@, that has accumulated @delay@ amount of samples delay along its path.
--
-- DSignal has the <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/roles.html type role>
--
-- >>> :i DSignal
-- type role DSignal nominal nominal representational
-- ...
--
-- as it is safe to coerce the values in the signal, but not safe to coerce the
-- synthesis domain or delay in the signal.
type role DSignal nominal nominal representational
newtype DSignal (dom :: Domain) (delay :: Nat) a =
    DSignal { toSignal :: Signal dom a
              -- ^ Strip a 'DSignal' of its delay information.
            }
  deriving ( Show, Default, Functor, Applicative, Num, Fractional
           , Foldable, Traversable, Arbitrary, CoArbitrary, Lift )

-- | Create a 'DSignal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (toSignal (dfromList [1,2,3,4,5]))
-- [1,2]
--
-- __NB__: This function is not synthesizable
dfromList :: NFDataX a => [a] -> DSignal dom 0 a
dfromList = coerce . fromList

-- | Create a 'DSignal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (toSignal (dfromList [1,2,3,4,5]))
-- [1,2]
--
-- __NB__: This function is not synthesizable
dfromList_lazy :: [a] -> DSignal dom 0 a
dfromList_lazy = coerce . fromList_lazy

-- | Feed the delayed result of a function back to its input:
--
-- @
-- mac
--   :: forall dom
--    . Clock dom
--   -> Reset dom
--   -> Enable dom
--   -> 'DSignal' dom 0 Int
--   -> 'DSignal' dom 0 Int
--   -> 'DSignal' dom 0 Int
-- mac clk rst en x y = 'feedback' (mac' x y)
--   where
--     mac'
--       :: 'DSignal' dom 0 Int
--       -> 'DSignal' dom 0 Int
--       -> 'DSignal' dom 0 Int
--       -> ('DSignal' dom 0 Int, 'DSignal' dom 1 Int)
--     mac' a b acc = let acc' = a * b + acc
--                    in  (acc, 'Clash.Explicit.Signal.Delayed.delayedI' clk rst en 0 acc')
-- @
--
-- >>> sampleN 7 (toSignal (mac systemClockGen systemResetGen enableGen (dfromList [0..]) (dfromList [0..])))
-- [0,0,1,5,14,30,55]
feedback
  :: (DSignal dom n a -> (DSignal dom n a,DSignal dom (n + m + 1) a))
  -> DSignal dom n a
feedback f = let (o,r) = f (coerce r) in o

-- | 'Signal's are not delayed
fromSignal :: Signal dom a -> DSignal dom 0 a
fromSignal = coerce

-- | __EXPERIMENTAL__
--
-- __Unsafely__ convert a 'Signal' to a 'DSignal' with an arbitrary @delay@.
--
-- __NB__: Should only be used to interface with functions specified in terms of
-- 'Signal'.
unsafeFromSignal :: Signal dom a -> DSignal dom n a
unsafeFromSignal = DSignal

-- | __EXPERIMENTAL__
--
-- Access a /delayed/ signal from the future in the present. Often required
-- When writing a circuit that requires feedback from itself.
--
-- @
-- mac
--   :: Clock dom
--   -> Reset dom
--   -> Enable dom
--   -> 'DSignal' dom 0 Int
--   -> 'DSignal' dom 0 Int
--   -> 'DSignal' dom 0 Int
-- mac clk rst en x y = acc'
--   where
--     acc' = (x * y) + 'antiDelay' d1 acc
--     acc  = 'Clash.Explicit.Signal.Delayed.delayedI' clk rst en 0 acc'
-- @
antiDelay :: SNat d -> DSignal dom (n + d) a -> DSignal dom n a
antiDelay _ = coerce

-- | __EXPERIMENTAL__
--
-- Access a /delayed/ signal from the past in the present. In contrast with
-- 'Clash.Explicit.Signal.Delayed.delayed' and friends forward does not insert
-- any logic. This means using this function violates the delay invariant of
-- 'DSignal'. This is sometimes useful when combining unrelated delayed signals
-- where inserting logic is not wanted or when abstracting over internal
-- delayed signals where the internal delay information should not be leaked.
--
-- For example, the circuit below returns a sequence of numbers as a pair
-- but the internal delay information between the elements of the pair
-- should not leak into the type.
--
-- @
-- numbers
--   :: forall dom
--    . Clock dom
--   -> Reset dom
--   -> Enable dom
--   -> 'DSignal' dom 5 (Int, Int)
-- numbers clk rst en = DB.bundle (forward d1 s1, s2)
--   where
--     s1 :: 'DSignal' dom 4 Int
--     s1 = 'Clash.Explicit.Signal.Delayed.delayed' clk rst en (100 :> 10 :> 5 :> 1 :> Nil) (pure 200)
--     s2 :: 'DSignal' dom 5 Int
--     s2 = fmap (2*) $ 'Clash.Explicit.Signal.Delayed.delayN' d1 0 en clk s1
-- @
--
-- >>> sampleN 8 (toSignal (numbers systemClockGen systemResetGen enableGen))
-- [(1,0),(1,2),(5,2),(10,10),(100,20),(200,200),(200,400),(200,400)]

forward :: SNat d -> DSignal dom n a -> DSignal dom (n + d) a
forward _ = coerce
