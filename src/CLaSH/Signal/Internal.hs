{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}
module CLaSH.Signal.Internal
  ( -- * Datatypes
    Clock (..)
  , SClock (..)
  , CSignal (..)
  , SystemClock
  , Signal
  , DSignal (..)
    -- * Construction
  , csignal
  , cmapSignal
  , cappSignal
  , dsignal
  )
where

import Data.Coerce                (coerce)
import Data.Default               (Default (..))
import Control.Applicative        (Applicative (..), liftA2)
import GHC.TypeLits               (Nat, Symbol)
import Language.Haskell.TH.Syntax (Lift (..))

import CLaSH.Promoted.Nat         (SNat)
import CLaSH.Promoted.Symbol      (SSymbol)

-- | A clock with a name ('Symbol') and period ('Nat')
data Clock = Clk Symbol Nat

-- | Singleton value for a type-level 'Clock' with the given @name@ and @period@
data SClock (clk :: Clock)
  where
    SClock :: SSymbol name -> SNat period -> SClock (Clk name period)

infixr 5 :-
-- | A synchronized signal with samples of type @a@, explicitly synchronized to
-- a clock @clk@
data CSignal (clk :: Clock) a = a :- CSignal clk a

-- | The standard system clock with a period of 1000
type SystemClock = Clk "system" 1000

-- | Signal synchronised to the \"system\" clock, which has a period of 1000.
type Signal a = CSignal SystemClock a

-- | A synchronized signal with samples of type @a@, synchronized to \"system\"
-- clock (period 1000), only produces a valid output after @delay@ samples.
newtype DSignal (delay :: Nat) a = DSignal (Signal a)
  deriving (Show,Default,Lift,Functor,Applicative)

instance Show a => Show (CSignal clk a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (CSignal clk a) where
  lift ~(x :- _) = [| csignal x |]

instance Default a => Default (CSignal clk a) where
  def = csignal def


{-# NOINLINE csignal    #-}
-- | Create a constant 'CSignal' from a combinational value
--
-- >>> csample (csignal 4)
-- [4, 4, 4, 4, ...
csignal :: a -> CSignal clk a
csignal a = let s = a :- s in s

{-# NOINLINE cmapSignal #-}
cmapSignal :: (a -> b) -> CSignal clk a -> CSignal clk b
cmapSignal f (a :- as) = f a :- cmapSignal f as

{-# NOINLINE cappSignal #-}
cappSignal :: CSignal clk (a -> b) -> CSignal clk a -> CSignal clk b
cappSignal (f :- fs) ~(a :- as) = f a :- cappSignal fs as

instance Functor (CSignal clk) where
  fmap = cmapSignal

instance Applicative (CSignal clk) where
  pure  = csignal
  (<*>) = cappSignal

-- | Create a constant 'DSignal' from a combinational value
--
-- >>> dsample (dsignal 4)
-- [4, 4, 4, 4, ...
dsignal :: a -> DSignal n a
dsignal a = coerce (csignal a)

instance Num a => Num (CSignal clk a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = csignal . fromInteger

instance Num a => Num (DSignal delay a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = dsignal . fromInteger
