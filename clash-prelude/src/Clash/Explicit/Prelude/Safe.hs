{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

__This is the <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/safe_haskell.html Safe> API only of "Clash.Explicit.Prelude"__

This module defines the explicitly clocked counterparts of the functions
defined in "Clash.Prelude".
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions, not-home #-}

module Clash.Explicit.Prelude.Safe
  ( -- * Creating synchronous sequential circuits
    mealy
  , mealyB
  , moore
  , mooreB
  , registerB
    -- * Synchronizer circuits for safe clock domain crossing
  , dualFlipFlopSynchronizer
  , asyncFIFOSynchronizer
    -- * ROMs
  , asyncRom
  , asyncRomPow2
  , rom
  , romPow2
    -- ** ROMs defined by a 'MemBlob'
  , asyncRomBlob
  , asyncRomBlobPow2
  , romBlob
  , romBlobPow2
    -- * RAM primitives with a combinational read port
  , asyncRam
  , asyncRamPow2
    -- * BlockRAM primitives
  , blockRam
  , blockRamPow2
    -- ** BlockRAM primitives initialized with a 'MemBlob'
  , blockRamBlob
  , blockRamBlobPow2
    -- *** Creating and inspecting 'MemBlob'
  , MemBlob
  , createMemBlob
  , memBlobTH
  , unpackMemBlob
    -- ** BlockRAM read/write conflict resolution
  , readNew
    -- * Utility functions
  , isRising
  , isFalling
  , riseEvery
  , oscillate
    -- * Exported modules
    -- ** Synchronous signals
  , module Clash.Explicit.Signal
  , module Clash.Explicit.Signal.Delayed
    -- ** Datatypes
    -- *** Bit vectors
  , module Clash.Sized.BitVector
    -- *** Arbitrary-width numbers
  , module Clash.Sized.Signed
  , module Clash.Sized.Unsigned
  , module Clash.Sized.Index
    -- *** Fixed point numbers
  , module Clash.Sized.Fixed
    -- *** Fixed size vectors
  , module Clash.Sized.Vector
    -- *** Perfect depth trees
  , module Clash.Sized.RTree
    -- ** Annotations
  , module Clash.Annotations.TopEntity
    -- ** Generics type-classes
  , Generic
  , Generic1
    -- ** Type-level natural numbers
  , module GHC.TypeLits
  , module GHC.TypeLits.Extra
  , module Clash.Promoted.Nat
  , module Clash.Promoted.Nat.Literals
  , module Clash.Promoted.Nat.TH
    -- ** Type-level strings
  , module Clash.Promoted.Symbol
    -- ** Type classes
    -- *** Clash
  , module Clash.Class.BitPack
  , module Clash.Class.Num
  , module Clash.Class.Resize
    -- *** Other
  , module Control.Applicative
  , module Data.Bits
      -- ** Exceptions
  , module Clash.XException
    -- ** Named types
  , module Clash.NamedTypes
    -- ** Haskell Prelude
    -- $hiding
  , module Clash.HaskellPrelude
  )
where

import Control.Applicative
import Data.Bits
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits
import GHC.TypeLits.Extra
import Clash.HaskellPrelude
import qualified Prelude

import Clash.Annotations.TopEntity
import Clash.Class.BitPack
import Clash.Class.Num
import Clash.Class.Resize
import Clash.NamedTypes

import Clash.Explicit.BlockRam
import Clash.Explicit.BlockRam.Blob
import Clash.Explicit.Mealy
import Clash.Explicit.Moore
import Clash.Explicit.RAM
import Clash.Explicit.ROM
import Clash.Explicit.ROM.Blob
import Clash.Explicit.Signal
import Clash.Explicit.Signal.Delayed
import Clash.Explicit.Synchronizer
  (dualFlipFlopSynchronizer, asyncFIFOSynchronizer)
import Clash.Prelude.ROM.Blob (asyncRomBlob, asyncRomBlobPow2)
import Clash.Prelude.ROM             (asyncRom, asyncRomPow2)
import Clash.Promoted.Nat
import Clash.Promoted.Nat.TH
import Clash.Promoted.Nat.Literals
import Clash.Promoted.Symbol
import Clash.Sized.BitVector
import Clash.Sized.Fixed
import Clash.Sized.Index
import Clash.Sized.RTree
import Clash.Sized.Signed
import Clash.Sized.Unsigned
import Clash.Sized.Vector hiding (fromList, unsafeFromList)
import Clash.XException

{- $setup
>>> :set -XDataKinds
>>> :m -Prelude
>>> import Clash.Explicit.Prelude.Safe
>>> let rP clk rst en = registerB clk rst en (8::Int,8::Int)
-}

{- $hiding
"Clash.Explicit.Prelude.Safe" re-exports most of the Haskell "Prelude" with the
exception of those functions that the Clash API defines to work on 'Vec' from
"Clash.Sized.Vector" instead of on lists as the Haskell Prelude does. In
addition, for the 'Clash.Class.Parity.odd' and 'Clash.Class.Parity.even'
functions a type class called 'Clash.Class.Parity.Parity' is available at
"Clash.Class.Parity".
-}

-- | Create a 'register' function for product-type like signals (e.g.
-- @('Signal' a, 'Signal' b)@)
--
-- @
-- rP :: Clock dom -> Reset dom -> Enable dom
--    -> ('Signal' dom Int, 'Signal' dom Int)
--    -> ('Signal' dom Int, 'Signal' dom Int)
-- rP clk rst en = 'registerB' clk rst en (8,8)
-- @
--
-- >>> simulateB (rP systemClockGen systemResetGen enableGen) [(1,1),(1,1),(2,2),(3,3)] :: [(Int,Int)]
-- [(8,8),(8,8),(1,1),(2,2),(3,3)...
-- ...
registerB
  :: ( KnownDomain dom
     , NFDataX a
     , Bundle a )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> a
  -> Unbundled dom a
  -> Unbundled dom a
registerB clk rst en i =
  unbundle Prelude.. register clk rst en i Prelude.. bundle
{-# INLINE registerB #-}

-- | Give a pulse when the 'Signal' goes from 'minBound' to 'maxBound'
isRising
  :: ( KnownDomain dom
     , NFDataX a
     , Bounded a
     , Eq a )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> a -- ^ Starting value
  -> Signal dom a
  -> Signal dom Bool
isRising clk rst en is s = liftA2 edgeDetect prev s
  where
    prev = register clk rst en is s
    edgeDetect old new = old == minBound && new == maxBound
{-# INLINABLE isRising #-}

-- | Give a pulse when the 'Signal' goes from 'maxBound' to 'minBound'
isFalling
  :: ( KnownDomain dom
     , NFDataX a
     , Bounded a
     , Eq a )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> a -- ^ Starting value
  -> Signal dom a
  -> Signal dom Bool
isFalling clk rst en is s = liftA2 edgeDetect prev s
  where
    prev = register clk rst en is s
    edgeDetect old new = old == maxBound && new == minBound
{-# INLINABLE isFalling #-}

-- | Give a pulse every @n@ clock cycles. This is a useful helper function when
-- combined with functions like @'Clash.Explicit.Signal.regEn'@ or
-- @'Clash.Explicit.Signal.mux'@, in order to delay a register by a known amount.
riseEvery
  :: forall dom  n
   . KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> SNat n
  -> Signal dom Bool
riseEvery clk rst en SNat = moore clk rst en transfer output 0 (pure ())
  where
    output :: Index n -> Bool
    output = (== maxBound)

    transfer :: Index n -> () -> Index n
    transfer s _ = if (s == maxBound) then 0 else s+1
{-# INLINEABLE riseEvery #-}

-- | Oscillate a @'Bool'@ for a given number of cycles, given the starting state.
oscillate
  :: forall dom  n
   . KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Bool
  -> SNat n
  -> Signal dom Bool
oscillate clk rst en begin SNat =
  moore clk rst en transfer snd (0, begin) (pure ())
 where
  transfer :: (Index n, Bool) -> () -> (Index n, Bool)
  transfer (s, i) _ =
    if s == maxBound
      then (0,   not i) -- reset state and oscillate output
      else (s+1, i)     -- hold current output
{-# INLINEABLE oscillate #-}
