{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
module CLaSH.Signal.Internal
  ( -- * Datatypes
    Clock (..)
  , SClock (..)
  , CSignal (..)
    -- * Type classes
    -- ** Functor
  , mapSignal#
    -- ** Applicative
  , signal#
  , appSignal#
    -- ** Foldable
  , foldr#
    -- ** Traversable
  , traverse#
    -- * Basic circuits
  , register#
  )
where

import Control.Applicative        (Applicative (..), (<$>),liftA2)
import Data.Default               (Default (..))
import Data.Foldable              (Foldable (..))
import Data.Traversable           (Traversable (..))
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
--
-- __NB__: The constructor, @(':-')@, is __not__ synthesisable.
data CSignal (clk :: Clock) a = a :- CSignal clk a

instance Show a => Show (CSignal clk a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (CSignal clk a) where
  lift ~(x :- _) = [| signal# x |]

instance Default a => Default (CSignal clk a) where
  def = signal# def

instance Functor (CSignal clk) where
  fmap = mapSignal#

{-# NOINLINE mapSignal# #-}
mapSignal# :: (a -> b) -> CSignal clk a -> CSignal clk b
mapSignal# f (a :- as) = f a :- mapSignal# f as

instance Applicative (CSignal clk) where
  pure  = signal#
  (<*>) = appSignal#

{-# NOINLINE signal# #-}
signal# :: a -> CSignal clk a
signal# a = let s = a :- s in s

{-# NOINLINE appSignal# #-}
appSignal# :: CSignal clk (a -> b) -> CSignal clk a -> CSignal clk b
appSignal# (f :- fs) ~(a :- as) = f a :- appSignal# fs as

instance Num a => Num (CSignal clk a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = signal# . fromInteger

-- | __NB__: Not synthesisable
--
-- Is used in conversions to lists
instance Foldable (CSignal clk) where
  foldr = foldr#

-- | In \"@'foldr# f z s@\" the @z@ element never used.
--
-- __NB__: Not synthesisable
foldr# :: (a -> b -> b) -> b -> CSignal clk a -> b
foldr# f z (a :- s) = a `f` (foldr# f z s)

-- | __NB__: Not synthesisable
--
-- Is used to define the 'CLaSH.Signal.Wrap' instance for 'CLaSH.Sized.Vec'
instance Traversable (CSignal clk) where
  traverse = traverse#

-- | __NB__: Not synthesisable
traverse# :: Applicative f => (a -> f b) -> CSignal clk a -> f (CSignal clk b)
traverse# f (a :- s) = (:-) <$> f a <*> traverse# f s

{-# NOINLINE register# #-}
register# :: SClock clk -> a -> CSignal clk a -> CSignal clk a
register# _ i s = i :- s
