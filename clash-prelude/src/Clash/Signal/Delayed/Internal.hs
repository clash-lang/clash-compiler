{-|
  Copyright   :  (C) 2019     , Myrtle Software Ltd.
                     2018     , @blaxill
                     2018-2019, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

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
>>> :{
let mac :: Clock System
        -> Reset System
        -> Enable System
        -> DSignal System 0 Int -> DSignal System 0 Int
        -> DSignal System 0 Int
    mac clk rst en x y = feedback (mac' x y)
      where
        mac' :: DSignal System 0 Int -> DSignal System 0 Int
             -> DSignal System 0 Int
             -> (DSignal System 0 Int, DSignal System 1 Int)
        mac' a b acc = let acc' = a * b + acc
                       in  (acc, delayed clk rst en (singleton 0) acc')
:}

-}

-- | A synchronized signal with samples of type @a@, synchronized to clock
-- @clk@, that has accumulated @delay@ amount of samples delay along its path.
newtype DSignal (dom :: Domain) (delay :: Nat) a =
    DSignal { toSignal :: Signal dom a
              -- ^ Strip a 'DSignal' from its delay information.
            }
  deriving ( Show, Default, Functor, Applicative, Num, Fractional
           , Foldable, Traversable, Arbitrary, CoArbitrary, Lift )

-- | Create a 'DSignal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (dfromList [1,2,3,4,5])
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
-- >>> sampleN 2 (dfromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesizable
dfromList_lazy :: [a] -> DSignal dom 0 a
dfromList_lazy = coerce . fromList_lazy

-- | Feed the delayed result of a function back to its input:
--
-- @
-- mac :: Clock dom -> Reset dom -> Enable dom
--     -> 'DSignal' dom 0 Int -> 'DSignal' dom 0 Int -> 'DSignal' dom 0 Int
-- mac clk rst en x y = 'feedback' (mac' x y)
--   where
--     mac' :: 'DSignal' dom 0 Int -> 'DSignal' dom 0 Int -> 'DSignal' dom 0 Int
--          -> ('DSignal' dom 0 Int, 'DSignal' dom 1 Int)
--     mac' a b acc = let acc' = a * b + acc
--                    in  (acc, 'delay' clk rst en ('singleton' 0) acc')
-- @
--
-- >>> sampleN 7 (mac systemClockGen systemResetGen enableGen (dfromList [0..]) (dfromList [0..]))
-- [0,0,1,5,14,30,55]
feedback
  :: (DSignal dom n a -> (DSignal dom n a,DSignal dom (n + m + 1) a))
  -> DSignal dom n a
feedback f = let (o,r) = f (coerce r) in o

-- | 'Signal's are not delayed
--
-- > sample s == dsample (fromSignal s)
fromSignal :: Signal dom a -> DSignal dom 0 a
fromSignal = coerce

-- | __EXPERIMENTAL__
--
-- __Unsafely__ convert a 'Signal' to /any/ 'DSignal' clk'.
--
-- __NB__: Should only be used to interface with functions specified in terms of
-- 'Signal'.
unsafeFromSignal :: Signal dom a -> DSignal dom n a
unsafeFromSignal = DSignal

-- | __EXPERIMENTAL__
--
-- Access a /delayed/ signal in the present.
--
-- @
-- mac :: Clock dom -> Reset dom -> Enable dom
--     -> 'DSignal' dom 0 Int -> 'DSignal' dom 0 Int -> 'DSignal' dom 0 Int
-- mac clk rst en x y = acc'
--   where
--     acc' = (x * y) + 'antiDelay' d1 acc
--     acc  = 'delay' clk rst en ('singleton' 0) acc'
-- @
antiDelay :: SNat d -> DSignal dom (n + d) a -> DSignal dom n a
antiDelay _ = coerce
