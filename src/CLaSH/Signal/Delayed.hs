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
    -- * List \<-\> DSignal conversion (not synthesisable)
  , dsample
  , dsampleN
  , dfromList
  )
where

import Data.Coerce                (coerce)
import Data.Default               (Default(..))
import Control.Applicative        (Applicative (..), (<$>))
import GHC.TypeLits               (KnownNat, Nat, type (-))
import Language.Haskell.TH.Syntax (Lift)
import Prelude                    hiding (head, replicate)

import CLaSH.Promoted.Nat         (SNat, snatToInteger, withSNat)
import CLaSH.Sized.Vector         (head, replicate, shiftInAt0, singleton)

import CLaSH.Signal               (Signal, fromList, register, sample, sampleN,
                                   sUnwrap, sWrap)
import CLaSH.Signal.Internal      (signal#)

-- | A synchronized signal with samples of type @a@, synchronized to \"system\"
-- clock (period 1000), only produces a valid output after @delay@ samples.
newtype DSignal (delay :: Nat) a = DSignal (Signal a)
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
dsignal a = coerce (signal# a)

-- | Delay a 'DSignal' for @m@ periods.
--
-- > delay3 :: DSignal (n - 3) Int -> DSignal n Int
-- > delay3 = delay d3
--
-- >>> dsampleN 6 (delay3 (dfromList [1..]))
-- [0,0,0,1,2,3]
delay :: forall a n m . (Default a, KnownNat m)
      => SNat m -- ^ Number of periods, @m@, to delay the signal
      -> DSignal (n - m) a
      -> DSignal n a
delay m ds = coerce (delay' (coerce ds))
  where
    delay' :: Signal a -> Signal a
    delay' s = case snatToInteger m of
                 0 -> s
                 _ -> let (r',o) = shiftInAt0 (sWrap r) (singleton s)
                          r      = register (replicate m def) (sUnwrap r')
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
delayI = withSNat delay

-- | Feed the delayed result of a function back to its input:
--
-- @
-- mac :: DSignal 0 Int -> DSignal 0 Int -> DSignal 1 Int
-- mac x y = 'feedback' (mac' x y)
--   where
--     mac' :: DSignal 0 Int -> DSignal 0 Int -> DSignal 0 Int
--          -> DSignal 1 Int
--     mac' a b acc = 'delay' d1 (a * b + acc)
-- @
--
-- >>> dsampleN 6 (mac (dfromList [1..]) (dfromList [1..]))
-- [0,1,5,14,30,55]
feedback :: (DSignal (n - m - 1) a -> DSignal n a) -> DSignal n a
feedback f = let r = f (coerce r) in r

-- | 'Signal's are not delayed
--
-- > sample s == dsample (fromSignal s)
fromSignal :: Signal a -> DSignal 0 a
fromSignal = coerce

-- | Filter out the samples [0..@m@] from a @'DSignal' m a@ signal.
--
-- >>> sampleN 4 (toSignal d2 (delay d2 (dfromList [1..])))
-- [Nothing,Nothing,Just 1, Just 2]
toSignal :: SNat m -> DSignal m a -> Signal (Maybe a)
toSignal m s = count (coerce s)
  where
    count s' = o
      where
        r      = register (snatToInteger m) r'
        (r',o) = sWrap (cntr <$> r <*> s')

        cntr 0 v = (0,Just v)
        cntr k _ = (k-1,Nothing)
