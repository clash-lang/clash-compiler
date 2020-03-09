{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

module Clash.Verification.Internal
 ( AssertionResult(..)
 , Property(..)
 , Assertion(..)
 , RenderAs(..)
 , IsTemporal(..)
 , AssertionValue(toAssertionValue)
 , Assertion'(..)
 , Property'(..)
 , toTemporal
 , isTemporal
 , assertion
 )
 where

import           Data.Text                      (Text)

import Clash.Annotations.BitRepresentation
  (ConstrRepr(..), DataReprAnn(..), liftQ)
import           Clash.Signal.Internal          (Domain, Signal)

-- | Render target for HDL
data RenderAs
  = PSL
  -- ^ Property Specification Language
  | SVA
  -- ^ SystemVerilog Assertions
  | AutoRenderAs
  -- ^ Use SVA for SystemVerilog, PSL for others
  deriving (Show, Eq)

data IsTemporal
  = IsNotTemporal
  | IsTemporal
  deriving (Eq, Ord)

-- | Internal version of 'Assertion'.
data Assertion' a
  = CvPure a
  -- ^ (Bootstrapping) signal of booleans
  | CvToTemporal (Assertion' a)
  -- ^ Tag to force a non-temporal assertion to a temporal one
  | CvLit Bool
  -- ^ Boolean literal

  | CvNot (Assertion' a)
  -- ^ Logical not
  | CvAnd (Assertion' a) (Assertion' a)
  -- ^ Logical and
  | CvOr (Assertion' a) (Assertion' a)
  -- ^ Logical or
  | CvImplies (Assertion' a) (Assertion' a)
  -- ^ Logical implies

  | CvNext Word (Assertion' a)
  -- ^ Moves start point of assertion /n/ cycles forward
  | CvBefore (Assertion' a) (Assertion' a)
  -- ^ Before @CvBefore a b@ is the same as @CvAnd a (CvNext 1 b)@
  | CvTemporalImplies Word (Assertion' a) (Assertion' a)
  -- ^ Temporal implies @CvTemporalImplies n a b@:
  --
  --   n | n == 0    -> same as @CvImplies a b@
  --     | otherwise -> same as @CvImplies a (CvNextN n b)@
  --
  | CvAlways (Assertion' a)
  -- ^ Assertion should _always_ hold
  | CvNever (Assertion' a)
  -- ^ Assertion should _never_ hold (not supported by SVA)
  deriving (Show, Functor, Foldable, Traversable)

-- | Internal version of 'Property'. All user facing will instantiate @a@
-- with @(Maybe Text, Signal dom Bool)@. Blackboxes will instantiate it with
-- @(Maybe Text, Term)@ instead.
data Property' a
  = CvAssert (Assertion' a)
  | CvCover (Assertion' a)
  deriving (Show, Functor, Foldable, Traversable)

data Assertion (dom :: Domain) =
  Assertion IsTemporal (Assertion' (Maybe Text, Signal dom Bool))

toTemporal :: Assertion dom -> Assertion' (Maybe Text, Signal dom Bool)
toTemporal (Assertion IsTemporal a) = a
toTemporal (Assertion IsNotTemporal a) = CvToTemporal a
{-# INLINE toTemporal #-}

isTemporal :: Assertion dom -> IsTemporal
isTemporal (Assertion it _assert) = it
{-# INLINE isTemporal #-}

assertion :: Assertion dom -> Assertion' (Maybe Text, Signal dom Bool)
assertion (Assertion _it assert) = assert
{-# INLINE assertion #-}

-- | A property is a temporal or basic assertion that's specified to either
-- used as an _assert_ or _cover_ statement. See
-- 'Clash.Explicit.Verification.assert' and 'Clash.Explicit.Verification.cover'.
newtype Property (dom :: Domain) =
  Property (Property' (Maybe Text, Signal dom Bool))

-- | A result of some property. Besides carrying the actual boolean result, it
-- carries some properties used to make reports.
data AssertionResult = AssertionResult
  { cvPropName :: !String  -- I'd like text, but Clash complains :[
  -- ^ Name of property belonging to this result
  , cvPass :: !Bool
  -- ^ False whenever property is violated, True otherwise
  }
  deriving (Eq)
{-# ANN module (
  DataReprAnn
    $(liftQ [t| AssertionResult |])
    0
    [ ConstrRepr 'AssertionResult 0 0 [0b0, 0b0]
    ]) #-}
{- Marked as zero-width so Clash won't stumble on the fact it's unrepresentable. ^ -}

-- | An AssertionValue is a bool-like value or stream that can be used in
-- property specifications. Clash implements two: a stream of booleans
-- (Signal dom Bool), and the result of a property expression (Assertion
-- dom).
class AssertionValue dom a | a -> dom where
  -- | Convert given type into a Assertion.
  toAssertionValue :: a -> Assertion dom

-- | Stream of booleans, originating from a circuit
instance AssertionValue dom (Signal dom Bool) where
  toAssertionValue s = Assertion IsNotTemporal (CvPure (Nothing, s))
  {-# INLINE toAssertionValue #-}

-- | Result of a property specification
instance AssertionValue dom (Assertion dom) where
  toAssertionValue = id
  {-# INLINE toAssertionValue #-}
