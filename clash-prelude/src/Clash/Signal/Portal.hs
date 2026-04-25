{-|
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Signal portals let a signal be marked at one point in a design and used at
another point without manually threading it through all intermediate layers.

In simulation, portals use a process-wide registry implemented with
'unsafePerformIO'. This is safe under the intended discipline: a portal name
has exactly one source at a given type, and the source is evaluated before the
sink is sampled. In synthesized HDL, Clash rewrites matching portal sources and
sinks into ordinary ports and wires. The portal source is a pass-through value;
use the returned signal in the design so the source remains live.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Signal.Portal
  ( portalSource
  , portalSink

    -- * Internal
  , PortalMap
  , portalMap#
  , portalSource#
  , portalSink#
  ) where

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Class.BitPack (BitPack, BitSize, pack, unpack)
import Clash.Promoted.Nat (SNat(..), snatToNum)
import Clash.Signal.Internal (Signal, fromList, sample)
import Clash.Sized.Internal.BitVector (BitVector(BV))
import Clash.XException (NFDataX)

import Data.Binary (encode)
import Data.ByteString.Lazy (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import GHC.Natural (Natural)
import GHC.TypeLits (KnownNat)
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (Typeable, typeRep)

type Value = (Natural, Natural) -- (Mask, Value)
type Width = Int
type TypeRepBS = ByteString

type PortalMap = Map.Map String (TypeRepBS, Width, [Value])

portalMap# :: IORef PortalMap
portalMap# = unsafePerformIO (newIORef Map.empty)
{-# OPAQUE portalMap# #-}

mkTrace
  :: (BitPack a, NFDataX a)
  => Signal dom a
  -> [Value]
mkTrace signal = sample (unsafeToTup . pack <$> signal)
 where
  unsafeToTup (BV mask value) = (mask, value)

fromTrace
  :: forall a dom n
   . (BitPack a, KnownNat n, n ~ BitSize a, NFDataX a)
  => [Value]
  -> Signal dom a
fromTrace values =
  fromList [unpack (BV mask value) | (mask, value) <- values]

-- | Register a portal source. In simulation this records the signal in a
-- process-wide registry. In HDL generation Clash rewrites matching portal
-- sources and sinks to ordinary ports and wires.
portalSource#
  :: forall dom a
   . ( BitPack a
     , NFDataX a
     , Typeable a )
  => IORef PortalMap
  -> String
  -> Signal dom a
  -> IO (Signal dom a)
portalSource# portalMap portalName signal =
  atomicModifyIORef' portalMap $ \m ->
    case Map.lookup portalName m of
      Just (typeRep0, width0, _samples)
        | typeRep0 /= typeRep1 || width0 /= width ->
            error $ "Already registered a portal source with the name: '" ++ portalName ++
              "', but with a different type or width."
      _ ->
        ( Map.insert portalName trace m
        , signal)
 where
  typeRep1 = encode (typeRep @a)
  width = snatToNum (SNat @(BitSize a))
  trace =
    ( typeRep1
    , width
    , mkTrace signal)
{-# OPAQUE portalSource# #-}

-- | Read a portal sink from the process-wide simulation registry.
portalSink#
  :: forall dom a n
   . ( BitPack a
     , KnownNat n
     , n ~ BitSize a
     , NFDataX a
     , Typeable a )
  => IORef PortalMap
  -> String
  -> IO (Signal dom a)
portalSink# portalMap portalName = do
  m <- readIORef portalMap
  case Map.lookup portalName m of
    Nothing ->
      error $ "No portal source found with the name: '" ++ portalName ++ "'."
    Just (typeRep0, width0, samples)
      | typeRep0 /= typeRep1 || width0 /= width ->
          error $ "Portal source with the name: '" ++ portalName ++
            "' has a different type or width."
      | otherwise ->
          pure (fromTrace samples)
 where
  typeRep1 = encode (typeRep @a)
  width = snatToNum (SNat @(BitSize a))
{-# OPAQUE portalSink# #-}

-- | Mark a signal as a portal source.
--
-- The portal name must be statically known for synthesis, and each portal name
-- may have at most one source in a design hierarchy.
portalSource
  :: ( BitPack a
     , NFDataX a
     , Typeable a )
  => String
  -> Signal dom a
  -> Signal dom a
portalSource portalName signal =
  unsafePerformIO (portalSource# portalMap# portalName signal)
{-# OPAQUE portalSource #-}
{-# ANN portalSource hasBlackBox #-}

-- | Use a signal marked by 'portalSource'.
--
-- The portal name must be statically known for synthesis.
portalSink
  :: ( BitPack a
     , NFDataX a
     , Typeable a )
  => String
  -> Signal dom a
portalSink portalName =
  unsafePerformIO (portalSink# portalMap# portalName)
{-# OPAQUE portalSink #-}
{-# ANN portalSink hasBlackBox #-}
