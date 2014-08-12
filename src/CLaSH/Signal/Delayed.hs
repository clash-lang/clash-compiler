{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module CLaSH.Signal.Delayed
  ( -- * Delay-annotated synchronous signals
    DSignal
  , dsignal
  , delay
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
import GHC.TypeLits               (type (-))
import Prelude                    hiding (replicate)

import CLaSH.Promoted.Nat         (SNat, snatToInteger)
import CLaSH.Sized.Vector         (shiftInAt0, replicate)

import CLaSH.Signal               (fromList, register, sample, sampleN, unwrap,
                                   wrap)
import CLaSH.Signal.Internal      (DSignal (..), Signal, dsignal)

-- | Create a 'DSignal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> dsampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesisable
dfromList :: [a] -> DSignal t a
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

delay :: forall a n m . Default a
      => SNat m
      -> DSignal (n - m) a
      -> DSignal n a
delay m = coerce . delay' . coerce
  where
    delay' :: Signal a -> Signal a
    delay' s = case snatToInteger m of
                 0 -> s
                 _ -> let (r',o) = shiftInAt0 s (wrap r)
                          r      = register (replicate m def) (unwrap r')
                      in  o

feedback :: (DSignal (n - m - 1) a -> DSignal n a) -> DSignal (n - m - 1) a
feedback f = let (DSignal r) = f (DSignal r) in (DSignal r)

fromSignal :: Signal a -> DSignal 0 a
fromSignal = coerce

toSignal :: SNat m -> DSignal m a -> Signal (Maybe a)
toSignal m s = count (coerce s)
  where
    count s' = o
      where
        r      = register (snatToInteger m) r'
        (r',o) = wrap (cntr <$> r <*> s')

        cntr 0 v = (0,Just v)
        cntr k _ = (k-1,Nothing)
