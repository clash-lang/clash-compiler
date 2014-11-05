{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module CLaSH.Signal.Delayed
  ( -- * Delay-annotated synchronous signals
    DSignal
  , dsignal
  , delay
  , delayI
  , feedback
    -- * Signal \<-\> DSignal conversion
  , fromSignal
  , toSignal
  , unsafeFromSignal
    -- * List \<-\> DSignal conversion (not synthesisable)
  , dsample
  , dsampleN
  , dfromList
  )
where

import Data.Coerce                (coerce)
import Data.Default               (Default(..))
import Control.Applicative        (Applicative (..))
import GHC.TypeLits               (KnownNat, Nat, type (-))
import Language.Haskell.TH.Syntax (Lift)
import Prelude                    hiding (head, length, repeat)

import CLaSH.Sized.Vector         (Vec, head, length, repeat, shiftInAt0,
                                   singleton)

import CLaSH.Signal               (Signal, fromList, register, sample, sampleN,
                                   bundle', unbundle')

-- | A synchronized signal with samples of type @a@, synchronized to \"system\"
-- clock (period 1000), only produces a valid output after @delay@ samples.
newtype DSignal (delay :: Nat) a = DSignal { toSignal :: Signal a }
  deriving (Show,Default,Lift,Functor,Applicative,Num)

-- | Create a 'DSignal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> dsampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesisable
dfromList :: [a] -> DSignal 0 a
dfromList = coerce . fromList

-- | Get an infinite list of samples from a 'DSignal'
--
-- The elements in the list correspond to the values of the 'DSignal' at
-- consecutive clock cycles
--
-- > dsample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesisable
dsample :: DSignal t a -> [a]
dsample = sample . coerce

-- | Get a list of @n@ samples from a 'DSignal'
--
-- The elements in the list correspond to the values of the 'DSignal' at
-- consecutive clock cycles
--
-- > dsampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesisable
dsampleN :: Int -> DSignal t a -> [a]
dsampleN n = sampleN n . coerce


-- | Create a constant 'DSignal' from a combinational value
--
-- >>> dsample (dsignal 4)
-- [4, 4, 4, 4, ...
dsignal :: a -> DSignal n a
dsignal = pure

-- | Delay a 'DSignal' for @m@ periods.
--
-- > delay3 :: DSignal (n - 3) Int -> DSignal n Int
-- > delay3 = delay d3
--
-- >>> dsampleN 6 (delay3 (dfromList [1..]))
-- [0,0,0,1,2,3]
delay :: forall a n m . KnownNat m
      => Vec m a
      -> DSignal (n - m) a
      -> DSignal n a
delay m ds = coerce (delay' (coerce ds))
  where
    delay' :: Signal a -> Signal a
    delay' s = case length m of
      0 -> s
      _ -> let (r',o) = shiftInAt0 (unbundle' r) (singleton s)
               r      = register m (bundle' r')
           in  head o

-- | Delay a 'DSignal' for @m@ periods, where @m@ is derived from the context.
--
-- > delay2 :: DSignal (n - 2) Int -> DSignal n Int
-- > delay2 = delayI
--
-- >>> dsampleN 6 (delay2 (dfromList [1..])
-- [0,0,1,2,3,4]
delayI :: (Default a, KnownNat m)
       => DSignal (n - m) a
       -> DSignal n a
delayI = delay (repeat def)

-- | Feed the delayed result of a function back to its input:
--
-- @
-- mac :: DSignal 0 Int -> DSignal 0 Int -> DSignal 0 Int
-- mac x y = 'feedback' (mac' x y)
--   where
--     mac' :: DSignal 0 Int -> DSignal 0 Int -> DSignal 0 Int
--          -> (DSignal 0 Int, DSignal 1 Int)
--     mac' a b acc = let acc' = a * b + acc
--                    in  (acc, delay (singleton 0) acc')
-- @
--
-- >>> dsampleN 6 (mac (dfromList [1..]) (dfromList [1..]))
-- [0,1,5,14,30,55]
feedback :: (DSignal (n - m - 1) a -> (DSignal (n - m - 1) a,DSignal n a))
         -> DSignal (n - m - 1) a
feedback f = let (o,r) = f (coerce r) in o

-- | 'Signal's are not delayed
--
-- > sample s == dsample (fromSignal s)
fromSignal :: Signal a -> DSignal 0 a
fromSignal = coerce


-- | __Unsafely__ convert a 'Signal' to /any/ 'DSignal'.
--
-- __NB__: Should only be used to interface with functions specified in terms of
-- 'Signal'.
unsafeFromSignal :: Signal a -> DSignal n a
unsafeFromSignal = DSignal
