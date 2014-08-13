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

import CLaSH.Promoted.Nat         (SNat, snatToInteger)
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


-- | Create a constant 'DSignal' from a combinational value
--
-- >>> dsample (dsignal 4)
-- [4, 4, 4, 4, ...
dsignal :: a -> DSignal n a
dsignal a = coerce (signal# a)

delay :: forall a n m . (Default a, KnownNat m)
      => SNat m
      -> DSignal (n - m) a
      -> DSignal n a
delay m = coerce . delay' . coerce
  where
    delay' :: Signal a -> Signal a
    delay' s = case snatToInteger m of
                 0 -> s
                 _ -> let (r',o) = shiftInAt0 (sWrap r) (singleton s)
                          r      = register (replicate m def) (sUnwrap r')
                      in  head o

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
        (r',o) = sWrap (cntr <$> r <*> s')

        cntr 0 v = (0,Just v)
        cntr k _ = (k-1,Nothing)
