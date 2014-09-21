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
    cmealy
  , cmealyB
  , cregisterB
    -- * BlockRAM primitives
  , cblockRam
  , cblockRamPow2
    -- * Utility functions
  , cwindow
  , cwindowD
  , cisRising
  , cisFalling
    -- * Testbench functions
  , csassert
  , cstimuliGenerator
  , coutputVerifier
    -- * Exported modules
    -- ** Explicitly clocked synchronous signals
  , module CLaSH.Signal.Explicit
  )
where

import Control.Applicative     (liftA2)
import Data.Default            (Default (..))
import GHC.TypeLits            (KnownNat, type (+), natVal)
import Prelude                 hiding (repeat)

import CLaSH.Prelude.BlockRam  (cblockRam, cblockRamPow2)
import CLaSH.Prelude.Mealy     (cmealy, cmealyB)
import CLaSH.Prelude.Testbench (csassert, cstimuliGenerator, coutputVerifier)
import CLaSH.Signal.Explicit
import CLaSH.Sized.Vector      (Vec (..), (+>>), asNatProxy, repeat)

{-# INLINE cregisterB #-}
-- | Create a 'register' function for product-type like signals (e.g.
-- '(Signal a, Signal b)')
--
-- > clk100 = Clock d100
-- >
-- > rP :: (CSignal 100 Int, CSignal 100 Int) -> (CSignal 100 Int, CSignal 100 Int)
-- > rP = cregisterB d100 (8,8)
--
-- >>> csimulateB clk100 clk100 rP [(1,1),(2,2),(3,3),...
-- [(8,8),(1,1),(2,2),(3,3),...
cregisterB :: Bundle a => SClock clk -> a -> Bundled clk a -> Bundled clk a
cregisterB clk i = unbundle clk Prelude.. cregister clk i Prelude.. bundle clk

{-# INLINABLE cwindow #-}
-- | Give a window over a 'CSignal'
--
-- > window4 :: Signal Int -> Vec 4 (Signal Int)
-- > window4 = window
--
-- >>> csimulateB window4 [1,2,3,4,5,...
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
                  in  cregisterB clk (repeat def) next

{-# INLINABLE cwindowD #-}
-- | Give a delayed window over a 'CSignal'
--
-- > windowD3 :: Signal Int -> Vec 3 (Signal Int)
-- > windowD3 = windowD
--
-- >>> csimulateB windowD3 [1,2,3,4,...
-- [<0,0,0>, <1,0,0>, <2,1,0>, <3,2,1>, <4,3,2>,...
cwindowD :: (KnownNat (n + 1), Default a)
         => SClock clk                   -- ^ Clock to which the incoming signal
                                         -- is synchronized
         -> CSignal clk a                -- ^ Signal to create a window over
         -> Vec (n + 1) (CSignal clk a)  -- ^ Window of at least size 1
cwindowD clk x = prev
  where
    prev = cregisterB clk (repeat def) next
    next = x +>> prev

{-# INLINABLE cisRising #-}
-- | Give a pulse when the 'CSignal' goes from 'minBound' to 'maxBound'
cisRising :: (Bounded a, Eq a)
          => SClock clk
          -> a -- ^ Starting value
          -> CSignal clk a
          -> CSignal clk Bool
cisRising clk is s = liftA2 edgeDetect prev s
  where
    prev = cregister clk is s
    edgeDetect old new = old == minBound && new == maxBound

{-# INLINABLE cisFalling #-}
-- | Give a pulse when the 'CSignal' goes from 'maxBound' to 'minBound'
cisFalling :: (Bounded a, Eq a)
           => SClock clk
           -> a -- ^ Starting value
           -> CSignal clk a
           -> CSignal clk Bool
cisFalling clk is s = liftA2 edgeDetect prev s
  where
    prev = cregister clk is s
    edgeDetect old new = old == maxBound && new == minBound
