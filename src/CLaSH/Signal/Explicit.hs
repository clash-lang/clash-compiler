{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module CLaSH.Signal.Explicit
  ( -- * Explicitly clocked synchronous signal
    CSignal
    -- * Clock domain crossing
  , Clock (..)
  , veryUnsafeSynchronizer
  , fromImplicit
  , fromExplicit
  -- * Basic circuit functions
  , csignal
  , cregister
  , PackC (..)
  -- * Simulation functions
  , csimulate
  , csimulateP
    -- * List \<-\> CSignal conversion
  , csample
  , csampleN
  , cfromList
  )
where

import Data.Coerce
import Control.Applicative        (Applicative (..), (<$>), liftA2)
import GHC.TypeLits               (Nat)

import CLaSH.Bit                  (Bit)
import CLaSH.Promoted.Nat         (snatToInteger)
import CLaSH.Sized.Fixed          (Fixed)
import CLaSH.Sized.Signed         (Signed)
import CLaSH.Sized.Unsigned       (Unsigned)
import CLaSH.Sized.Vector         (Vec(..), vmap, vhead, vtail)

import CLaSH.Signal.Implicit
import CLaSH.Signal.Types

{-# NOINLINE cregister  #-}

{-# NOINLINE veryUnsafeSynchronizer #-}
{-# NOINLINE fromImplicit           #-}
{-# NOINLINE fromExplicit           #-}

-- | Create a 'Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
cfromList :: [a] -> CSignal t a
cfromList = coerce . fromList

-- | Get an infinite list of samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal' at
-- consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
csample :: CSignal t a -> [a]
csample = sample . coerce

-- | Get a list of @n@ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal' at
-- consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
csampleN :: Int -> CSignal t a -> [a]
csampleN n = sampleN n . coerce

-- | 'register' @i s@ delays the values in 'Signal' @s@ for one cycle, and sets
-- the value at time 0 to @i@
--
-- >>> sampleN 3 (register 8 (fromList [1,2,3,4]))
-- [8,1,2]
cregister :: a -> CSignal t a -> CSignal t a
cregister i s = coerce (register i (coerce s))

-- | Simulate a ('Signal' -> 'Signal') function given a list of samples
--
-- >>> simulate (register 8) [1, 2, 3, ...
-- [8, 1, 2, 3, ...
csimulate :: (CSignal t a -> CSignal t b) -> [a] -> [b]
csimulate f = csample . f . cfromList

-- | Conversion between a 'Signal' of a product type (e.g. a tuple) and a
-- product type of 'Signal's
class PackC a where
  type CSignalP (clk :: Nat) a
  type CSignalP clk a = CSignal clk a
  -- | > pack :: (Signal a, Signal b) -> Signal (a,b)
  -- However:
  --
  -- > pack :: Signal Bit -> Signal Bit
  cpack   :: Clock clk -> CSignalP clk a -> CSignal clk a
  -- | > unpack :: Signal (a,b) -> (Signal a, Signal b)
  -- However:
  --
  -- > unpack :: Signal Bit -> Signal Bit
  cunpack :: Clock clk -> CSignal clk a -> CSignalP clk a

-- | Simulate a ('SignalP' -> 'SignalP') function given a list of samples
--
-- >>> simulateP (unpack . register (8,8) . pack) [(1,1), (2,2), (3,3), ...
-- [(8,8), (1,1), (2,2), (3,3), ...
csimulateP :: (PackC a, PackC b) => Clock t -> (CSignalP t a -> CSignalP t b) -> [a] -> [b]
csimulateP clk f = csimulate (cpack clk . f . cunpack clk)

instance PackC Bit where
  cpack   _ = id
  cunpack _ = id

instance PackC (Signed n) where
  cpack   _ = id
  cunpack _ = id

instance PackC (Unsigned n) where
  cpack   _ = id
  cunpack _ = id

instance PackC (Fixed frac rep size) where
  cpack   _ = id
  cunpack _ = id

instance PackC Bool where
  cpack   _ = id
  cunpack _ = id

instance PackC Integer where
  cpack   _ = id
  cunpack _ = id

instance PackC Int where
  cpack   _ = id
  cunpack _ = id

instance PackC Float where
  cpack   _ = id
  cunpack _ = id

instance PackC Double where
  cpack   _ = id
  cunpack _ = id

instance PackC () where
  cpack   _ = id
  cunpack _ = id

instance PackC (a,b) where
  type CSignalP t (a,b) = (CSignal t a, CSignal t b)
  cpack _       = uncurry (liftA2 (,))
  cunpack _ tup = (fmap fst tup, fmap snd tup)

instance PackC (a,b,c) where
  type CSignalP t (a,b,c) = (CSignal t a, CSignal t b, CSignal t c)
  cpack   _ (a,b,c) = (,,) <$> a <*> b <*> c
  cunpack _ tup     = (fmap (\(x,_,_) -> x) tup
                      ,fmap (\(_,x,_) -> x) tup
                      ,fmap (\(_,_,x) -> x) tup
                      )

instance PackC (a,b,c,d) where
  type CSignalP t (a,b,c,d) = (CSignal t a, CSignal t b, CSignal t c, CSignal t d)
  cpack   _ (a,b,c,d) = (,,,) <$> a <*> b <*> c <*> d
  cunpack _ tup       = (fmap (\(x,_,_,_) -> x) tup
                        ,fmap (\(_,x,_,_) -> x) tup
                        ,fmap (\(_,_,x,_) -> x) tup
                        ,fmap (\(_,_,_,x) -> x) tup
                        )

instance PackC (a,b,c,d,e) where
  type CSignalP t (a,b,c,d,e) = (CSignal t a, CSignal t b, CSignal t c, CSignal t d, CSignal t e)
  cpack _ (a,b,c,d,e) = (,,,,) <$> a <*> b <*> c <*> d <*> e
  cunpack _ tup       = (fmap (\(x,_,_,_,_) -> x) tup
                        ,fmap (\(_,x,_,_,_) -> x) tup
                        ,fmap (\(_,_,x,_,_) -> x) tup
                        ,fmap (\(_,_,_,x,_) -> x) tup
                        ,fmap (\(_,_,_,_,x) -> x) tup
                        )

instance PackC (a,b,c,d,e,f) where
  type CSignalP t (a,b,c,d,e,f) = (CSignal t a, CSignal t b, CSignal t c, CSignal t d, CSignal t e, CSignal t f)
  cpack   _ (a,b,c,d,e,f) = (,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
  cunpack _ tup           = (fmap (\(x,_,_,_,_,_) -> x) tup
                            ,fmap (\(_,x,_,_,_,_) -> x) tup
                            ,fmap (\(_,_,x,_,_,_) -> x) tup
                            ,fmap (\(_,_,_,x,_,_) -> x) tup
                            ,fmap (\(_,_,_,_,x,_) -> x) tup
                            ,fmap (\(_,_,_,_,_,x) -> x) tup
                            )

instance PackC (a,b,c,d,e,f,g) where
  type CSignalP t (a,b,c,d,e,f,g) = (CSignal t a, CSignal t b, CSignal t c, CSignal t d, CSignal t e, CSignal t f, CSignal t g)
  cpack   _ (a,b,c,d,e,f,g) = (,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g
  cunpack _ tup             = (fmap (\(x,_,_,_,_,_,_) -> x) tup
                              ,fmap (\(_,x,_,_,_,_,_) -> x) tup
                              ,fmap (\(_,_,x,_,_,_,_) -> x) tup
                              ,fmap (\(_,_,_,x,_,_,_) -> x) tup
                              ,fmap (\(_,_,_,_,x,_,_) -> x) tup
                              ,fmap (\(_,_,_,_,_,x,_) -> x) tup
                              ,fmap (\(_,_,_,_,_,_,x) -> x) tup
                              )

instance PackC (a,b,c,d,e,f,g,h) where
  type CSignalP t (a,b,c,d,e,f,g,h) = (CSignal t a, CSignal t b, CSignal t c, CSignal t d, CSignal t e, CSignal t f, CSignal t g, CSignal t h)
  cpack   _ (a,b,c,d,e,f,g,h) = (,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h
  cunpack _ tup               = (fmap (\(x,_,_,_,_,_,_,_) -> x) tup
                                ,fmap (\(_,x,_,_,_,_,_,_) -> x) tup
                                ,fmap (\(_,_,x,_,_,_,_,_) -> x) tup
                                ,fmap (\(_,_,_,x,_,_,_,_) -> x) tup
                                ,fmap (\(_,_,_,_,x,_,_,_) -> x) tup
                                ,fmap (\(_,_,_,_,_,x,_,_) -> x) tup
                                ,fmap (\(_,_,_,_,_,_,x,_) -> x) tup
                                ,fmap (\(_,_,_,_,_,_,_,x) -> x) tup
                                )

instance PackC (Vec n a) where
  type CSignalP t (Vec n a) = Vec n (CSignal t a)
  cpack clk vs = mkCSignal (vmap (shead . coerce) vs) (cpack clk (vmap cstail vs))
  cunpack _      (CSignal (Nil :- _))      = Nil
  cunpack clk vs@(CSignal ((_ :> _) :- _)) = fmap vhead vs :> cunpack clk (fmap vtail vs)

-- | Synchronisation function that is basically a represented by a wire in hardware.
--
-- Should only be used as part of a proper synchronisation component, such as
-- a dual flip-flop synchronizer, or a FIFO with an asynchronous memory element.
--
-- Currently oversimplifies oversampling and compression: oversampling and
-- compression rate are calculated using an integer division.
veryUnsafeSynchronizer :: Clock clk1
                       -> Clock clk2
                       -> CSignal clk1 a
                       -> CSignal clk2 a
veryUnsafeSynchronizer (Clock clk1) (Clock clk2) s = s'
  where
    t1    = snatToInteger clk1
    t2    = snatToInteger clk2
    s' | t1 < t2   = compress   (t2 `div` t1) s -- Integer devision is erroneous
       | t1 > t2   = oversample (t2 `div` t1) s -- Integer devision is erroneous
       | otherwise = same s

same :: CSignal clk1 a -> CSignal clk2 a
same (CSignal s) = CSignal s

-- Probably needs more parametrization
oversample :: Integer -> CSignal clk1 a -> CSignal clk2 a
oversample i (CSignal (s :- ss)) = (prefix (replicate (fromInteger i) s) (oversample i (CSignal ss)))
  where
    prefix []     s' = s'
    prefix (x:xs) s' = mkCSignal x (prefix xs s')

-- Probably needs more parametrization
compress :: Integer -> CSignal clk1 a -> CSignal clk2 a
compress i (CSignal x) = CSignal (compress' i x)
  where
    compress' 1 (s :- ss) = s :- compress' i ss
    compress' n (_ :- ss) = compress' (n-1) ss

-- -- | Implicitly clocked signals are synchronized to clock 1000
fromImplicit :: Signal a -> CSignal 1000 a
fromImplicit s = CSignal s

-- | Implicitly clocked signals are synchronized to clock 1000
fromExplicit :: CSignal 1000 a -> Signal a
fromExplicit (CSignal s) = s
