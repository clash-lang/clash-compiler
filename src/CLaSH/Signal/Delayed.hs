{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Signal.Delayed
  ( -- * Delay-annotated synchronous signals
    DSignal
  , delay
  , delayI
  -- , feedback
    -- * Signal \<-\> DSignal conversion
  , fromSignal
  , toSignal
    -- * List \<-\> DSignal conversion (not synthesisable)
  , dfromList
    -- * Experimental
  , unsafeFromSignal
  , antiDelay
  )
where

import Data.Bits                  (Bits, FiniteBits)
import Data.Coerce                (coerce)
import Data.Default               (Default(..))
import Control.Applicative        (liftA2)
import GHC.TypeLits               (KnownNat, Nat, type (-))
import Language.Haskell.TH.Syntax (Lift)
import Prelude                    hiding (head, length, repeat)

import CLaSH.Class.Num            (ExtendingNum (..), SaturatingNum)
import CLaSH.Promoted.Nat         (SNat)
import CLaSH.Sized.Vector         (Vec, head, length, repeat, shiftInAt0,
                                   singleton)
import CLaSH.Signal               (Signal, fromList, register, bundle, unbundle)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> import CLaSH.Prelude
-- >>> let delay3 = delay (0 :> 0 :> 0 :> Nil)
-- >>> let delay2 = delayI :: DSignal (n - 2) Int -> DSignal n Int
-- >>> :{
-- let mac x y = feedback (mac' x y)
--       where
--         mac' :: DSignal 0 Int -> DSignal 0 Int -> DSignal 0 Int
--              -> (DSignal 0 Int, DSignal 1 Int)
--         mac' a b acc = let acc' = a * b + acc
--                        in  (acc, delay (singleton 0) acc')
-- :}
--

-- | A synchronized signal with samples of type @a@, synchronized to \"system\"
-- clock (period 1000), that has accumulated @delay@ amount of samples delay
-- along its path.
newtype DSignal (delay :: Nat) a =
    DSignal { -- | Strip a 'DSignal' from its delay information.
              toSignal :: Signal a
            }
  deriving (Show,Default,Lift,Functor,Applicative,Num,Bounded,Fractional,
            Real,Integral,SaturatingNum,Eq,Ord,Enum,Bits,FiniteBits,Foldable,
            Traversable)

instance ExtendingNum a b => ExtendingNum (DSignal n a) (DSignal n b) where
  type AResult (DSignal n a) (DSignal n b) = DSignal n (AResult a b)
  plus  = liftA2 plus
  minus = liftA2 minus
  type MResult (DSignal n a) (DSignal n b) = DSignal n (MResult a b)
  times = liftA2 times

-- | Create a 'DSignal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (dfromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesisable
dfromList :: [a] -> DSignal 0 a
dfromList = coerce . fromList

-- | Delay a 'DSignal' for @m@ periods.
--
-- @
-- delay3 :: 'DSignal' (n - 3) Int -> 'DSignal' n Int
-- delay3 = 'delay' (0 ':>' 0 ':>' 0 ':>' 'Nil')
-- @
--
-- >>> sampleN 6 (delay3 (dfromList [1..]))
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
      _ -> let (r',o) = shiftInAt0 (unbundle r) (singleton s)
               r      = register m (bundle r')
           in  head o

-- | Delay a 'DSignal' for @m@ periods, where @m@ is derived from the context.
--
-- @
-- delay2 :: 'DSignal' (n - 2) Int -> 'DSignal' n Int
-- delay2 = 'delayI'
-- @
--
-- >>> sampleN 6 (delay2 (dfromList [1..]))
-- [0,0,1,2,3,4]
delayI :: (Default a, KnownNat m)
       => DSignal (n - m) a
       -> DSignal n a
delayI = delay (repeat def)

-- | Feed the delayed result of a function back to its input:
--
-- @
-- mac :: 'DSignal' 0 Int -> 'DSignal' 0 Int -> 'DSignal' 0 Int
-- mac x y = 'feedback' (mac' x y)
--   where
--     mac' :: 'DSignal' 0 Int -> 'DSignal' 0 Int -> 'DSignal' 0 Int
--          -> ('DSignal' 0 Int, 'DSignal' 1 Int)
--     mac' a b acc = let acc' = a * b + acc
--                    in  (acc, 'delay' ('singleton' 0) acc')
-- @
--
-- >>> sampleN 6 (mac (dfromList [1..]) (dfromList [1..]))
-- [0,1,5,14,30,55]
-- feedback :: (DSignal (n - m - 1) a -> (DSignal (n - m - 1) a,DSignal n a))
--          -> DSignal (n - m - 1) a
-- feedback f = let (o,r) = f (coerce r) in o

-- | 'Signal's are not delayed
--
-- > sample s == dsample (fromSignal s)
fromSignal :: Signal a -> DSignal 0 a
fromSignal = coerce

-- | __EXPERIMENTAL__
--
-- __Unsafely__ convert a 'Signal' to /any/ 'DSignal'.
--
-- __NB__: Should only be used to interface with functions specified in terms of
-- 'Signal'.
unsafeFromSignal :: Signal a -> DSignal n a
unsafeFromSignal = DSignal

-- | __EXPERIMENTAL__
--
-- Access a /delayed/ signal in the present.
--
-- @
-- mac :: 'DSignal' 0 Int -> 'DSignal' 0 Int -> 'DSignal' 0 Int
-- mac x y = acc'
--   where
--     acc' = (x * y) + 'antiDelay' d1 acc
--     acc  = 'delay' ('singleton' 0) acc'
-- @
antiDelay :: SNat d -> DSignal n a -> DSignal (n - d) a
antiDelay _ = coerce
