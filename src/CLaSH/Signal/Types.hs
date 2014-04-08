{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}
module CLaSH.Signal.Types where

import Data.Coerce                (coerce)
import Data.Default               (Default (..))
import Control.Applicative        (Applicative (..), liftA2)
import GHC.TypeLits               (Nat)
import Language.Haskell.TH.Syntax (Lift (..))

import CLaSH.Promoted.Nat  (SNat)

infixr 5 :-
-- | A synchronized signal with samples of type @a@, implicitly synchronized to
-- an unnamed global clock
data Signal a = a :- Signal a

-- | A synchronized signal with samples of type @a@, explicitly synchronized to
-- a clock with period @clk@
newtype CSignal (clk :: Nat) a = CSignal (Signal a)
  deriving (Show,Default,Lift,Functor,Applicative)

-- | A clock with period @clk@
newtype Clock (clk :: Nat) = Clock (SNat clk)

instance Show a => Show (Signal a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (Signal a) where
  lift ~(x :- _) = [| signal x |]

instance Default a => Default (Signal a) where
  def = signal def

{-# NOINLINE signal    #-}
{-# NOINLINE mapSignal #-}
{-# NOINLINE appSignal #-}

-- | Create a constant 'Signal' from a combinational value
--
-- >>> sample (signal 4)
-- [4, 4, 4, 4, ...
signal :: a -> Signal a
signal a = let s = a :- s in s

mapSignal :: (a -> b) -> Signal a -> Signal b
mapSignal f (a :- as) = f a :- mapSignal f as

appSignal :: Signal (a -> b) -> Signal a -> Signal b
appSignal (f :- fs) ~(a :- as) = f a :- appSignal fs as

instance Functor Signal where
  fmap = mapSignal

instance Applicative Signal where
  pure  = signal
  (<*>) = appSignal

shead :: Signal a -> a
shead (x :- _)  = x

stail :: Signal a -> Signal a
stail (_ :- xs) = xs

mkCSignal :: a -> CSignal clk a -> CSignal clk a
mkCSignal a (CSignal s) = CSignal (a :- s)

cstail :: CSignal t a -> CSignal t a
cstail (CSignal s) = CSignal (stail s)

-- | Create a constant 'CSignal' from a combinational value
--
-- >>> csample (csignal 4)
-- [4, 4, 4, 4, ...
csignal :: a -> CSignal t a
csignal a = coerce (signal a)

instance Num a => Num (Signal a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = signal . fromInteger

instance Num a => Num (CSignal t a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = csignal . fromInteger
