{-|
Copyright  :  (C) 2018-2022, Google Inc
                  2019,      Myrtle Software Ltd
                  2023,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Clocks.Deriving (Clocks(..), deriveClocksInstances) where

import Control.Monad.Extra (concatMapM)
import Data.Kind (Constraint)
import GHC.TypeLits (Nat)
import Language.Haskell.TH

import Clash.CPP (haddockOnly)
import Clash.Explicit.Signal (unsafeSynchronizer)
import Clash.Promoted.Symbol (SSymbol(..))
import Clash.Signal.Internal
  (clockGen, Clock(..), KnownDomain, Reset, Signal, unsafeToActiveLow)

-- | __NB__: The documentation only shows instances up to /3/ output clocks. By
-- default, instances up to and including /18/ clocks will exist.
class Clocks t where
  type ClocksCxt t :: Constraint
  type NumOutClocks t :: Nat

  clocks ::
    (KnownDomain domIn, ClocksCxt t) =>
    Clock domIn ->
    Reset domIn ->
    t

-- Derive instance for /n/ clocks
deriveClocksInstance :: Int -> DecsQ
deriveClocksInstance n =
  [d| instance Clocks $instType where
        type ClocksCxt $instType = $cxtType
        type NumOutClocks $instType = $numOutClocks

        clocks (Clock _ Nothing) $(varP rst) = $funcImpl
        clocks _ _ = error "clocks: dynamic clocks unsupported"
        {-# CLASH_OPAQUE clocks #-}
    |]
 where
  clkTyVar m = varT $ mkName $ "c" <> show m
  clkTypes = map (\m -> [t| Clock $(clkTyVar m) |]) [1..n]
  lockTyVar = varT $ mkName "pllLock"
  -- (Clock c1, Clock c2, ..., Signal pllLock Bool)
  instType = foldl appT (tupleT $ n + 1) $
               clkTypes <> [ [t| Signal $lockTyVar Bool |] ]
  clkKnownDoms = map (\m -> [t| KnownDomain $(clkTyVar m) |]) [1..n]
  -- (KnownDomain c1, KnownDomain c2, ..., KnownDomain pllLock)
  cxtType = foldl appT (tupleT $ n + 1) $
              clkKnownDoms <> [ [t| KnownDomain $lockTyVar |] ]
  numOutClocks = litT . numTyLit $ toInteger n

  -- 'clocks' function
  rst = mkName "rst"
  lockImpl = [| unsafeSynchronizer clockGen clockGen
                  (unsafeToActiveLow $(varE rst)) |]
  clkImpls = replicate n [| Clock SSymbol Nothing |]
  funcImpl = tupE $ clkImpls <> [lockImpl]

-- Derive instances for up to and including 18 clocks, except when we are
-- generating Haddock
deriveClocksInstances :: DecsQ
deriveClocksInstances = concatMapM deriveClocksInstance [1..n]
 where
  n | haddockOnly = 3
    | otherwise   = 18
