{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2017-2019, Myrtle Software Ltd
                     2017     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Clash is a functional hardware description language that borrows both its
  syntax and semantics from the functional programming language Haskell. The
  merits of using a functional language to describe hardware comes from the fact
  that combinational circuits can be directly modeled as mathematical functions
  and that functional languages lend themselves very well at describing and
  (de-)composing mathematical functions.

  This package provides:

  * Prelude library containing datatypes and functions for circuit design

  To use the library:

  * Import "Clash.Prelude"; by default clock and reset lines are implicitly
    routed for all the components found in "Clash.Prelude". You can read more
    about implicit clock and reset lines in "Clash.Signal#implicitclockandreset"
  * Alternatively, if you want to explicitly route clock and reset ports,
    for more straightforward multi-clock designs, you can import the
    "Clash.Explicit.Prelude" module. Note that you should not import
    "Clash.Prelude" and "Clash.Explicit.Prelude" at the same time as they
    have overlapping definitions.

  For now, "Clash.Prelude" is also the best starting point for exploring the
  library. A preliminary version of a tutorial can be found in "Clash.Tutorial".
  Some circuit examples can be found in "Clash.Examples".
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions, not-home #-}

module Clash.Prelude
  ( -- * Creating synchronous sequential circuits
    mealy
  , mealyB
  , (<^>)
  , moore
  , mooreB
  , registerB
#ifdef CLASH_MULTIPLE_HIDDEN
    -- * Synchronizer circuits for safe clock domain crossings
  , dualFlipFlopSynchronizer
  , asyncFIFOSynchronizer
#endif
    -- * ROMs
  , asyncRom
  , asyncRomPow2
  , rom
  , romPow2
    -- ** ROMs initialized with a data file
  , asyncRomFile
  , asyncRomFilePow2
  , romFile
  , romFilePow2
    -- * RAM primitives with a combinational read port
  , asyncRam
  , asyncRamPow2
    -- * BlockRAM primitives
  , blockRam
  , blockRamPow2
  , blockRamU
  , blockRam1
  , E.ResetStrategy(..)
    -- ** BlockRAM primitives initialized with a data file
  , blockRamFile
  , blockRamFilePow2
    -- ** BlockRAM read/write conflict resolution
  , readNew
    -- * Utility functions
  , window
  , windowD
  , isRising
  , isFalling
  , riseEvery
  , oscillate
    -- * Tracing
    -- ** Simple
  , traceSignal1
  , traceVecSignal1
    -- ** Tracing in a multi-clock environment
  , traceSignal
  , traceVecSignal
    -- ** VCD dump functions
  , dumpVCD
    -- * Exported modules
    -- ** Synchronous signals
  , module Clash.Signal
  , module Clash.Signal.Delayed
    -- ** Datatypes
    -- *** Bit vectors
  , module Clash.Sized.BitVector
  , module Clash.Prelude.BitIndex
  , module Clash.Prelude.BitReduction
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
    -- ** Template Haskell
  , Lift (..)
    -- ** Type classes
    -- *** Clash
  , AutoReg, autoReg, deriveAutoReg
  , module Clash.Class.BitPack
  , module Clash.Class.Exp
  , module Clash.Class.Num
  , module Clash.Class.Parity
  , module Clash.Class.Resize
    -- *** Other
  , module Control.Applicative
  , module Data.Bits
  , module Data.Default.Class
  , module Data.Kind
    -- ** Exceptions
  , module Clash.XException
    -- ** Named types
  , module Clash.NamedTypes
    -- ** Hidden arguments
  , module Clash.Hidden
    -- ** Magic
  , module Clash.Magic
    -- ** Haskell Prelude
    -- $hiding
  , module Clash.HaskellPrelude
  )
where

import           Control.Applicative
import           Data.Bits
import           Data.Default.Class
import           Data.Kind (Type, Constraint)
import           GHC.Stack                   (HasCallStack)
import           GHC.TypeLits
import           GHC.TypeLits.Extra
import           Language.Haskell.TH.Syntax  (Lift(..))
import           Clash.HaskellPrelude

import           Clash.Annotations.TopEntity
import           Clash.Class.AutoReg         (AutoReg, deriveAutoReg)
import           Clash.Class.BitPack hiding  (GBitPack(..))
import           Clash.Class.Exp
import           Clash.Class.Num
import           Clash.Class.Parity
import           Clash.Class.Resize
import qualified Clash.Explicit.Prelude      as E
import           Clash.Hidden
import           Clash.Magic
import           Clash.NamedTypes
import           Clash.Prelude.BitIndex
import           Clash.Prelude.BitReduction
import           Clash.Prelude.BlockRam
import           Clash.Prelude.BlockRam.File
import           Clash.Prelude.ROM.File
import           Clash.Prelude.Safe
#ifdef CLASH_MULTIPLE_HIDDEN
import           Clash.Prelude.Synchronizer
#endif
import           Clash.Promoted.Nat
import           Clash.Promoted.Nat.TH
import           Clash.Promoted.Nat.Literals
import           Clash.Promoted.Symbol
import           Clash.Sized.BitVector
import           Clash.Sized.Fixed
import           Clash.Sized.Index
import           Clash.Sized.RTree
import           Clash.Sized.Signed
import           Clash.Sized.Unsigned
import           Clash.Sized.Vector hiding (fromList, unsafeFromList)
import           Clash.Signal hiding
  (HiddenClockName, HiddenResetName, HiddenEnableName)
import           Clash.Signal.Delayed
import           Clash.Signal.Trace
import           Clash.XException

{- $setup
>>> :set -XDataKinds -XFlexibleContexts -XTypeApplications
>>> let window4  = window  :: HiddenClockResetEnable dom  => Signal dom Int -> Vec 4 (Signal dom Int)
>>> let windowD3 = windowD :: HiddenClockResetEnable dom  => Signal dom Int -> Vec 3 (Signal dom Int)
-}

{- $hiding
"Clash.Prelude" re-exports most of the Haskell "Prelude" with the exception of
those functions that the Clash API defines to work on 'Vec' from
"Clash.Sized.Vector" instead of on lists as the Haskell Prelude does.
In addition, for the 'Clash.Class.Parity.odd' and 'Clash.Class.Parity.even'
functions a type class called 'Clash.Class.Parity.Parity' is available at
"Clash.Class.Parity".
-}

-- | Give a window over a 'Signal'
--
-- > window4 :: HiddenClockResetEnable dom
-- >         => Signal dom Int -> Vec 4 (Signal dom Int)
-- > window4 = window
--
-- >>> simulateB @System window4 [1::Int,2,3,4,5] :: [Vec 4 Int]
-- [<1,0,0,0>,<2,1,0,0>,<3,2,1,0>,<4,3,2,1>,<5,4,3,2>...
-- ...
window
  :: ( HiddenClockResetEnable dom
     , KnownNat n
     , Default a
     , NFDataX a )
  => Signal dom a
  -- ^ Signal to create a window over
  -> Vec (n + 1) (Signal dom a)
  -- ^ Window of at least size 1
window = hideClockResetEnable E.window
{-# INLINE window #-}

-- | Give a delayed window over a 'Signal'
--
-- > windowD3
-- >   :: HiddenClockResetEnable dom
-- >   => Signal dom Int
-- >   -> Vec 3 (Signal dom Int)
-- > windowD3 = windowD
--
-- >>> simulateB @System windowD3 [1::Int,2,3,4] :: [Vec 3 Int]
-- [<0,0,0>,<1,0,0>,<2,1,0>,<3,2,1>,<4,3,2>...
-- ...
windowD
  :: ( HiddenClockResetEnable dom
     , KnownNat n
     , Default a
     , NFDataX a )
  => Signal dom a
  -- ^ Signal to create a window over
  -> Vec (n + 1) (Signal dom a)
  -- ^ Window of at least size 1
windowD = hideClockResetEnable E.windowD
{-# INLINE windowD #-}

-- | Implicit version of 'Clash.Class.AutoReg.autoReg'
autoReg
  :: (HasCallStack, HiddenClockResetEnable dom, AutoReg a)
  => a
  -> Signal dom a
  -> Signal dom a
autoReg = hideClockResetEnable E.autoReg
