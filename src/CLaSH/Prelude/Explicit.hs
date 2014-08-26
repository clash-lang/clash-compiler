{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

{- |
  This module defines the explicitly clocked counterparts of the functions
  defined in "CLaSH.Prelude".

  This module uses the explicitly clocked 'CSignal's synchronous signals, as
  opposed to the implicitly clocked 'Signal's used in "CLaSH.Prelude". Take a
  look at "CLaSH.Signal.Explicit" to see how you can make multi-clock designs
  using explicitly clocked signals.
-}
module CLaSH.Prelude.Explicit
  ( -- * Creating synchronous sequential circuits
    sync
  , cregisterP
    -- * BlockRAM primitives
  , cblockRam
  , cblockRamPow2
    -- * Utility functions
  , cwindow
  , cwindowD
    -- * Testbench functions
  , csassert
  , cstimuliGenerator
  , coutputVerifier
    -- * Exported modules
    -- ** Explicitly clocked synchronous signals
  , module CLaSH.Signal.Explicit
  )
where

import Data.Default            (Default (..))
import GHC.TypeLits            (KnownNat, type (+), natVal)
import Prelude                 hiding (repeat)

import CLaSH.Prelude.BlockRam  (cblockRam, cblockRamPow2)
import CLaSH.Prelude.Mealy     (sync)
import CLaSH.Prelude.Testbench (csassert, cstimuliGenerator, coutputVerifier)
import CLaSH.Signal.Explicit
import CLaSH.Sized.Vector      (Vec (..), (+>>), asNatProxy, repeat)

{-# INLINE cregisterP #-}
-- | Create a 'register' function for product-type like signals (e.g.
-- '(Signal a, Signal b)')
--
-- > clk100 = Clock d100
-- >
-- > rP :: (CSignal 100 Int, CSignal 100 Int) -> (CSignal 100 Int, CSignal 100 Int)
-- > rP = cregisterP d100 (8,8)
--
-- >>> csimulateP clk100 clk100 rP [(1,1),(2,2),(3,3),...
-- [(8,8),(1,1),(2,2),(3,3),...
cregisterP :: Wrap a => SClock clk -> a -> Wrapped clk a -> Wrapped clk a
cregisterP clk i = wrap clk Prelude.. cregister clk i Prelude.. unwrap clk

{-# INLINABLE cwindow #-}
-- | Give a window over a 'CSignal'
--
-- > window4 :: Signal Int -> Vec 4 (Signal Int)
-- > window4 = window
--
-- >>> csimulateP window4 [1,2,3,4,5,...
-- [<1,0,0,0>, <2,1,0,0>, <3,2,1,0>, <4,3,2,1>, <5,4,3,2>,...
cwindow :: (KnownNat n, Default a)
        => SClock clk                  -- ^ Clock to which the incoming
                                       -- signal is synchronized
        -> CSignal clk a               -- ^ Signal to create a window over
        -> Vec (n + 1) (CSignal clk a) -- ^ Window of at least size 1
cwindow clk x = res
  where
    res  = x :> prev
    prev = case natVal (asNatProxy prev) of
             0 -> repeat def
             _ -> let next = x +>> prev
                  in  cregisterP clk (repeat def) next

{-# INLINABLE cwindowD #-}
-- | Give a delayed window over a 'CSignal'
--
-- > windowD3 :: Signal Int -> Vec 3 (Signal Int)
-- > windowD3 = windowD
--
-- >>> csimulateP windowD3 [1,2,3,4,...
-- [<0,0,0>, <1,0,0>, <2,1,0>, <3,2,1>, <4,3,2>,...
cwindowD :: (KnownNat (n + 1), Default a)
         => SClock clk                   -- ^ Clock to which the incoming signal
                                         -- is synchronized
         -> CSignal clk a                -- ^ Signal to create a window over
         -> Vec (n + 1) (CSignal clk a)  -- ^ Window of at least size 1
cwindowD clk x = prev
  where
    prev = cregisterP clk (repeat def) next
    next = x +>> prev
