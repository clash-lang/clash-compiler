{-|
Copyright  :  (C) 2018-2022, Google Inc
                  2019,      Myrtle Software Ltd
                  2023,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Clocks.Deriving
  ( Clocks(..)
  , deriveClocksInstances
  , ClocksSync(..)
  , deriveClocksSyncInstances
  ) where

import Control.Monad.Extra (concatMapM)
import Data.Kind (Constraint, Type)
import Language.Haskell.TH hiding (Type)

import Clash.Explicit.Reset (resetSynchronizer)
import Clash.Explicit.Signal (unsafeSynchronizer)
import Clash.Promoted.Symbol (SSymbol(..))
import Clash.Signal.Internal
  (clockGen, Clock(..), Domain, KnownDomain, Reset, Signal, unsafeFromActiveLow,
   unsafeToActiveLow)

class Clocks t where
  type ClocksCxt t :: Constraint

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

  -- 'clocks' function
  rst = mkName "rst"
  lockImpl = [| unsafeSynchronizer clockGen clockGen
                  (unsafeToActiveLow $(varE rst)) |]
  clkImpls = replicate n [| Clock SSymbol Nothing |]
  funcImpl = tupE $ clkImpls <> [lockImpl]

-- Derive instances for up to and including to /n/ clocks
deriveClocksInstances :: Int -> DecsQ
deriveClocksInstances n = concatMapM deriveClocksInstance [1..n]

class ClocksSync t where
  type ClocksSyncClocksInst t (domIn :: Domain) :: Type
  type ClocksResetSynchronizerCxt t :: Constraint

  clocksResetSynchronizer ::
    ( KnownDomain domIn
    , ClocksResetSynchronizerCxt t
    ) =>
    ClocksSyncClocksInst t domIn ->
    Clock domIn ->
    t

-- Derive instance for /n/ clocks
deriveClocksSyncInstance :: Int -> DecsQ
deriveClocksSyncInstance n =
  [d|
    instance ClocksSync $instType where
      type ClocksSyncClocksInst $instType $domInTyVar = $clocksInstType
      type ClocksResetSynchronizerCxt $instType = $cxtType

      clocksResetSynchronizer pllOut $(varP clkIn) =
        let $pllPat = pllOut
        in $instTuple
  |]
 where
  clkVarName m = mkName $ "c" <> show m
  clkTyVar :: Int -> TypeQ
  clkTyVar m = varT $ clkVarName m
  clkAndRstTy m = [ [t| Clock $(clkTyVar m) |]
                  , [t| Reset $(clkTyVar m) |]
                  ]
  -- (Clock c1, Reset c1, Clock c2, Reset c2, ...)
  instType = foldl appT (tupleT $ n * 2) $
               concatMap clkAndRstTy [1..n]
  domInTyVar = varT $ mkName "domIn"
  clkTypes = map (\m -> [t| Clock $(clkTyVar m) |]) [1..n]
  -- (Clock c1, Clock c2, ..., Signal domIn Bool)
  clocksInstType = foldl appT (tupleT $ n + 1) $
                     clkTypes <> [ [t| Signal $domInTyVar Bool |] ]
  -- (KnownDomain c1, KnownDomain c2, ...)
  cxtType
    | n == 1
    = [t| KnownDomain $(clkTyVar 1) |]
    | otherwise
    = foldl appT (tupleT n) $
        map (\m -> [t| KnownDomain $(clkTyVar m) |]) [1..n]

  -- 'clocksResetSynchronizer' function
  clkIn = mkName "clkIn"
  pllLock = mkName "pllLock"
  -- (c1, c2, ..., pllLock)
  pllPat = tupP $ map (varP . clkVarName) [1..n] <> [varP pllLock]
  syncImpl m =
    [|
      resetSynchronizer $(varE $ clkVarName m)
        (unsafeFromActiveLow
          (unsafeSynchronizer $(varE clkIn) $(varE $ clkVarName m)
                              $(varE pllLock)))
    |]
  clkAndRstExp m = [ varE $ clkVarName m
                   , syncImpl m
                   ]
  -- (c1, r1, c2, r2, ...) where rN is the synchronized reset for clock N
  instTuple = tupE $ concatMap clkAndRstExp [1..n]

-- Derive instances for up to and including to /n/ clocks
deriveClocksSyncInstances :: Int -> DecsQ
deriveClocksSyncInstances n = concatMapM deriveClocksSyncInstance [1..n]
