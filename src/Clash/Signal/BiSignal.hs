{-|
Copyright  :  (C) 2017, Google Inc.
                  2017, QBayLogic
License    :  BSD2 (see the file LICENSE)
Author     :  Martijn Bastiaan <martijn@qbaylogic.com>
Maintainer :  Christiaan Baaij <christiaan@qbaylogic.com>

Although wires in hardware are fundamentally bidirectional, most HDLs and simulation software distinguishes between
'in' and 'out' signals. Wires which are explicitly marked as 'inout' will remain bidirectional however. Clash has
support for 'inout' ports through the implementation of /BiSignal/s. To cleanly map to functions (and thus support
software simulation using Haskell), a BiSignal comes in two parts; the in part:

@
'BiSignalIn' (ds :: 'BiSignalDefault') (dom :: 'Domain') a
@

and the out part:

@
'BiSignalOut' (ds :: 'BiSignalDefault') (dom :: 'Domain') a
@

Where /a/ is the type of the value of the BiSignal, for example /Int/ or /Bool/, and /domain/ is the /clock-/
(and /reset-/) domain to which the memory elements manipulating these BiSignals belong. Lastly, /ds/ indicates
the default behavior for the BiSignal if nothing is being written (pull-down, pull-up, or undefined).

/BiSignalIn/ is used by Clash to generate the 'inout' ports on a HDL level, while /BiSignalOut/ is only used
for simulation purposes and generally discarded by the compiler.

-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Signal.BiSignal (
    BiSignalIn()
  , BiSignalOut()
  , BiSignalDefault(..)
  , mergeBiSignalOuts
  , readFromBiSignal
  , writeToBiSignal
  , veryUnsafeToBiSignalIn
  ) where

import           Type.Reflection            (Typeable, typeOf, splitApps)
import           Data.Maybe                 (Maybe,fromMaybe,fromJust,isJust)
import           Data.List                  (intercalate)

import qualified Clash.Sized.Vector         as V
import           Clash.Sized.Vector         (Vec)
import           Clash.Signal.Internal      (Signal(..), Domain, foldr#)
import           Clash.Class.BitPack        (BitPack, BitSize, unpack)
import           Clash.XException           (errorX)

import           GHC.TypeLits               (KnownNat)
import           GHC.Stack                  (HasCallStack)

-- | Used to specify default behaviour on reading from a BiSignal
data BiSignalDefault = High
                     -- ^ inout behaves as if connected to a pull-up resistor
                     | Low
                     -- ^ inout behaves as if connected to a pull-down resistor
                     | Undefined
                     -- ^ inout behaves as if it has a floating signal if not written to. Evaluation such a value in
                     --   simulation will yield an errorX (undefined value).
                      deriving (Typeable,Show)

-- |
data BiSignalIn (ds :: BiSignalDefault) (dom :: Domain) a where
    BiSignalIn :: ( KnownNat m
                  , m ~ BitSize a
                  , BitPack a
                  -- Typeable instances in order to implement default#:
                  , Typeable a
                  , Typeable ds
                  , Typeable dom
                  )
               => Signal dom (Maybe a)
               -> BiSignalIn ds dom a
                    deriving (Typeable)

-- | Wraps (multiple) writing signals. The semantics are such that only one of the signals may write
--   at a single time step.
newtype BiSignalOut (ds :: BiSignalDefault) (dom :: Domain) a = BiSignalOut [Signal dom (Maybe a)]

-- | Monoid instance to support concatting
instance Monoid (BiSignalOut defaultState dom a) where
   mempty                                    = BiSignalOut []
   mappend (BiSignalOut b1) (BiSignalOut b2) = BiSignalOut $ b1 ++ b2

-- | Foldable instance to support sampling from BiSignals
instance Foldable (BiSignalIn ds dom) where
  foldr f z bi = foldr# f z (readFromBiSignal bi)

-- Helper functions for this module only:
prepend# :: ( KnownNat m
            , m ~ BitSize a
            , BitPack a
            , Typeable a
            , Typeable ds
            , Typeable d
            )
         => Maybe a
         -> BiSignalIn ds d a
         -> BiSignalIn ds d a
prepend# a bsi = BiSignalIn (a :- as)
  where
    -- Move deconstruction to increase laziness:
    as = case bsi of (BiSignalIn as') -> as'

head# :: Signal dom a -> a
head# (x' :- _ )  = x'

tail# :: Signal dom a -> Signal dom a
tail# (_  :- xs') = xs'

-- | Will determine the default value of an "undefined" value based on the type of the
--   BiSignal given using reflection.
default# :: BiSignalIn ds d a -> BiSignalDefault
default# t@(BiSignalIn _) = let (_, [ds, _, _]) = splitApps (typeOf t) in case tail $ show ds of
                                                                              "High"      -> High
                                                                              "Low"       -> Low
                                                                              "Undefined" -> Undefined
                                                                              _           -> error "Unreachable code in default# in Clash.Signal.BiSignal"
-- / Helper functions

{-# NOINLINE readFromBiSignal #-}
readFromBiSignal :: HasCallStack
                 => BiSignalIn ds d a
                 -> Signal d a
readFromBiSignal t@(BiSignalIn s) =
  case default# t of
    Undefined -> fromMaybe (errorX " undefined value on BiSignalIn") <$> s
    Low       -> fromMaybe (unpack minBound) <$> s
    High      -> fromMaybe (unpack maxBound) <$> s


{-# NOINLINE mergeBiSignalOuts #-}
-- | Combine several inout signals into one.
mergeBiSignalOuts :: ( HasCallStack
                     , Show a
                     , KnownNat n
                     )
                  => Vec n (BiSignalOut defaultState dom a)
                  -> BiSignalOut defaultState dom a
mergeBiSignalOuts = mconcat . V.toList

{-# NOINLINE writeToBiSignal# #-}
writeToBiSignal# :: HasCallStack
                 => BiSignalIn ds d a
                 -> Signal d (Maybe a)
                 -> Signal d Bool
                 -> Signal d a
                 -> BiSignalOut ds d a
writeToBiSignal# _ maybeSignal _ _ = BiSignalOut [maybeSignal]

-- | Create a BiSignalOut
writeToBiSignal :: HasCallStack
                => BiSignalIn ds d a
                -> Signal d (Maybe a)
                -- ^ Value to write
                -> BiSignalOut ds d a
writeToBiSignal input writes = writeToBiSignal# input writes (isJust <$> writes) (fromJust <$> writes)

{-# NOINLINE veryUnsafeToBiSignalIn #-}
-- | Converts the 'out' part of a BiSignal to an 'in' part. In simulation it checks whether multiple components are
--   writing and will err accordingly. Make sure this is only called ONCE for every BiSignal.
veryUnsafeToBiSignalIn :: ( HasCallStack
                          , Show a
                          , KnownNat m
                          , m ~ BitSize a
                          , BitPack a
                          , Typeable a
                          , Typeable ds
                          , Typeable d
                          )
                       => BiSignalOut ds d a
                       -> BiSignalIn ds d a
veryUnsafeToBiSignalIn (BiSignalOut signals) = prepend# result biSignalOut'
  where
    -- Enforce that only one component is writing
    writing = filter (isJust . head#) signals
    result | null writing        = Nothing
           | length writing == 1 = head# $ head writing
           | otherwise           = error err

    -- FIXME: Use ClashException? Use errorX? Use error?
    err = "Multiple components wrote to the BiSignal. This is undefined behavior in "
       ++ "hardware and almost certainly a logic error. The components wrote: \n  "
       ++ intercalate "\n  " (map (show . head#) signals)

    -- Recursive step
    biSignalOut' = veryUnsafeToBiSignalIn $ BiSignalOut $ map tail# signals
