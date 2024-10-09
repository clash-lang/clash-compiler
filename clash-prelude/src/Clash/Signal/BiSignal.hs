{-|
Copyright  :  (C) 2017, Google Inc.
                  2019, Myrtle Software Ltd
                  2022-2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Wires are fundamentally bidirectional, and in traditional HDLs we can exploit
this aspect by explicitly marking the endpoint, or port, of such a wire as
/inout/, thereby making this port function as both a source and a drain for the
signals flowing over the wire.

Clash has support for @inout@ ports through the implementation of /BiSignal/s.
To cleanly map to functions (and thus support software simulation using Haskell),
a /BiSignal/ comes in two parts; the __in__ part:

@
'BiSignalIn' (ds :: 'BiSignalDefault') (dom :: 'Domain') (n :: Nat)
@

and the __out__ part:

@
'BiSignalOut' (ds :: 'BiSignalDefault') (dom :: 'Domain') (n :: Nat)
@

Where:

  * The internal representation is a 'BitVector'
  * /n/ indicates the number of bits in the 'BitVector'
  * /dom/ is the /clock-/ (and /reset-/) domain to which the memory elements
    manipulating these BiSignals belong.
  * Lastly, /ds/ indicates the default behavior for the BiSignal if nothing is
    being written (pull-down, pull-up, or undefined).

'BiSignalIn' is used by Clash to generate the @inout@ ports on a HDL level,
while 'BiSignalOut' is only used for simulation purposes and generally discarded
by the compiler.

= Example

The following describes a system where two circuits, in alternating fashion,
read the current value from the /bus/, increment it, and write it on the next
cycle.

@
import Clash.Explicit.Prelude
import Clash.Signal.BiSignal

-- | Alternatingly read / increment+write
counter
  :: (Bool, Int)
  -- ^ Internal flip + previous read
  -> Int
  -- ^ Int from inout
  -> ((Bool, Int), Maybe Int)
counter (write, prevread) i = ((write', prevread'), output)
  where
    output    = if write then Just (succ prevread) else Nothing
    prevread' = if write then prevread else i
    write' = not write

-- | Write on odd cyles
f :: Clock System
  -> Reset System
  -> Enable System
  -> BiSignalIn  'Floating System (BitSize Int)
  -> BiSignalOut 'Floating System (BitSize Int)
f clk rst en s = writeToBiSignal s (mealy clk rst en counter (False, 0) (readFromBiSignal s))

-- | Write on even cyles
g :: Clock System
  -> Reset System
  -> Enable System
  -> BiSignalIn  'Floating System (BitSize Int)
  -> BiSignalOut 'Floating System (BitSize Int)
g clk rst en s = writeToBiSignal s (mealy clk rst en counter (True, 0) (readFromBiSignal s))


-- | Connect the \/f\/ and \/g\/ circuits to the same bus
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Int
topEntity clk rst en = readFromBiSignal bus'
  where
    bus  = mergeBiSignalOuts $ f clk rst en bus' :> g clk rst en bus' :> Nil
    bus' = veryUnsafeToBiSignalIn bus
@
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Signal.BiSignal (
    BiSignalIn()
  , BiSignalOut()
  , BiSignalDefault(..)
  , SBiSignalDefault(..)
  , HasBiSignalDefault(..)
  , mergeBiSignalOuts
  , readFromBiSignal
  , writeToBiSignal
  , veryUnsafeToBiSignalIn
  ) where

import           Data.Kind                  (Type)
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe,isJust)

import           Clash.Annotations.Primitive (hasBlackBox)
import           Clash.Class.HasDomain
import           Clash.Class.BitPack        (BitPack (..))
import           Clash.Sized.BitVector      (BitVector)
import qualified Clash.Sized.Vector         as V
import           Clash.Sized.Vector         (Vec)
import           Clash.Signal.Internal      (Signal(..), Domain, head#, tail#)
import           Clash.XException           (errorX, fromJustX, NFDataX)

import           GHC.TypeLits               (KnownNat, Nat)
import           GHC.Stack                  (HasCallStack)
import           Data.Reflection            (Given (..))

-- | Used to specify the /default/ behavior of a \"BiSignal\" in Haskell simulation, i.e.
-- what value is read when no value is being written to it.
data BiSignalDefault
  = PullUp
  -- ^ __inout__ port behaves as if connected to a pull-up resistor
  | PullDown
  -- ^ __inout__ port behaves as if connected to a pull-down resistor
  | Floating
  -- ^ __inout__ port behaves as if is /floating/. Reading a /floating/
  -- \"BiSignal\" value in simulation will yield an errorX (undefined value).
  deriving (Show)

-- | Singleton versions of 'BiSignalDefault'
data SBiSignalDefault :: BiSignalDefault -> Type where
  SPullUp   :: SBiSignalDefault 'PullUp
  SPullDown :: SBiSignalDefault 'PullDown
  SFloating :: SBiSignalDefault 'Floating

instance Given (SBiSignalDefault 'PullUp) where
  given = SPullUp

instance Given (SBiSignalDefault 'PullDown) where
  given = SPullDown

instance Given (SBiSignalDefault 'Floating) where
  given = SFloating

-- | Type class for 'BiSignalDefault':
--   can be used as a constraint and for obtaining the pull-up mode
class HasBiSignalDefault (ds :: BiSignalDefault) where
  pullUpMode :: BiSignalIn ds dom n -> SBiSignalDefault ds

instance HasBiSignalDefault 'PullUp where
  pullUpMode _ = SPullUp

instance HasBiSignalDefault 'PullDown where
  pullUpMode _ = SPullDown

instance HasBiSignalDefault 'Floating where
  pullUpMode _ = SFloating

type role BiSignalIn nominal nominal nominal

-- | The /in/ part of an __inout__ port.
-- BiSignalIn has the <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/roles.html type role>
--
-- >>> :i BiSignalIn
-- type role BiSignalIn nominal nominal nominal
-- ...
--
-- as it is not safe to coerce the default behaviour, synthesis domain or width
-- of the data in the signal.
data BiSignalIn (ds :: BiSignalDefault) (dom :: Domain) (n :: Nat)
  = BiSignalIn (SBiSignalDefault ds) (Signal dom (Maybe (BitVector n)))

type role BiSignalOut nominal nominal nominal

-- | The /out/ part of an __inout__ port
--
-- Wraps (multiple) writing signals. The semantics are such that only one of
-- the signals may write at a single time step.
--
-- BiSignalOut has the <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/roles.html type role>
--
-- >>> :i BiSignalOut
-- type role BiSignalOut nominal nominal nominal
-- ...
--
-- as it is not safe to coerce the default behaviour, synthesis domain or width
-- of the data in the signal.
#if MIN_VERSION_base(4,15,0) && !MIN_VERSION_base(4,17,0)
data BiSignalOut (ds :: BiSignalDefault) (dom :: Domain) (n :: Nat)
  = BiSignalOut ![Signal dom (Maybe (BitVector n))]
#else
newtype BiSignalOut (ds :: BiSignalDefault) (dom :: Domain) (n :: Nat)
  = BiSignalOut [Signal dom (Maybe (BitVector n))]
#endif

type instance HasDomain dom1 (BiSignalOut ds dom2 n) = DomEq dom1 dom2
type instance TryDomain t (BiSignalOut ds dom n) = 'Found dom

-- | __NB__: Not synthesizable
instance Semigroup (BiSignalOut defaultState dom n) where
  (BiSignalOut b1) <> (BiSignalOut b2) = BiSignalOut (b1 ++ b2)

-- | Monoid instance to support concatenating
--
-- __NB__: Not synthesizable
instance Monoid (BiSignalOut defaultState dom n) where
  mempty = BiSignalOut []

-- /Lazily/ prepend a value to a 'BiSignalIn'.
--
-- Uses a /reified/ 'SBiSignalDefault', the 'Given' constraint, so we can fully
-- create 'BiSignalIn' "out of nowhere" when dealing with circular definitions.
prepend#
  :: Given (SBiSignalDefault ds)
  => Maybe (BitVector n)
  -> BiSignalIn ds d n
  -> BiSignalIn ds d n
prepend# a ~(BiSignalIn _ as) = BiSignalIn given (a :- as)

readFromBiSignal#
  :: ( HasCallStack
     , KnownNat n)
  => BiSignalIn ds d n
  -> Signal d (BitVector n)
readFromBiSignal# (BiSignalIn ds s) =
  case ds of
    SFloating -> fromMaybe (errorX " undefined value on BiSignalIn") <$> s
    SPullDown  -> fromMaybe minBound <$> s
    SPullUp    -> fromMaybe maxBound <$> s
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE readFromBiSignal# #-}
{-# ANN readFromBiSignal# hasBlackBox #-}

-- | Read the value from an __inout__ port
readFromBiSignal
  :: ( HasCallStack
     , BitPack a)
  => BiSignalIn ds d (BitSize a)
  -- ^ A 'BiSignalIn' with a number of bits needed to represent /a/
  -> Signal d a
readFromBiSignal = fmap unpack . readFromBiSignal#

-- | Combine several __inout__ signals into one.
mergeBiSignalOuts
  :: ( HasCallStack
     , KnownNat n
     )
  => Vec n (BiSignalOut defaultState dom m)
  -> BiSignalOut defaultState dom m
mergeBiSignalOuts = mconcat . V.toList
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE mergeBiSignalOuts #-}
{-# ANN mergeBiSignalOuts hasBlackBox #-}

writeToBiSignal#
  :: HasCallStack
  => BiSignalIn ds d n
  -> Signal d (Maybe (BitVector n))
  -> Signal d Bool
  -> Signal d (BitVector n)
  -> BiSignalOut ds d n
writeToBiSignal# bIn maybeSignal wEn val = BiSignalOut [bIn `seq` wEn `seq` val `seq` maybeSignal]
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE writeToBiSignal# #-}
{-# ANN writeToBiSignal# hasBlackBox #-}

-- | Write to an __inout__ port
writeToBiSignal
  :: (HasCallStack, BitPack a, NFDataX a)
  => BiSignalIn ds d (BitSize a)
  -> Signal d (Maybe a)
  -- ^ Value to write
  --
  --   * /Just a/ writes an /a/ value
  --   * /Nothing/ puts the port in a /high-impedance/ state
  -> BiSignalOut ds d (BitSize a)
writeToBiSignal input writes =
  writeToBiSignal#
    input
    (fmap pack <$> writes)
    (isJust <$> writes)
    (pack . fromJustX <$> writes)
{-# INLINE writeToBiSignal #-}

-- | Converts the @out@ part of a BiSignal to an @in@ part. In simulation it
-- checks whether multiple components are writing and will error accordingly.
-- Make sure this is only called ONCE for every BiSignal.
veryUnsafeToBiSignalIn
  :: ( HasCallStack
     , KnownNat n
     , Given (SBiSignalDefault ds)
     )
  => BiSignalOut ds d n
  -> BiSignalIn ds d n
veryUnsafeToBiSignalIn (BiSignalOut signals) = prepend# result biSignalOut'
  where
    -- Enforce that only one component is writing
    result = case filter (isJust . head#) signals of
      []  -> Nothing
      [w] -> head# w
      _   -> errorX err

    err = unwords
      [ "Multiple components wrote to the BiSignal. This is undefined behavior"
      , "in hardware and almost certainly a logic error. The components wrote:\n"
      , intercalate "\n  " (map (show . head#) signals)
      ]

    -- Recursive step
    biSignalOut' = veryUnsafeToBiSignalIn $ BiSignalOut $ map tail# signals
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE veryUnsafeToBiSignalIn #-}
{-# ANN veryUnsafeToBiSignalIn hasBlackBox #-}
