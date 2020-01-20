{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Clash.CoSim.Types
Description : Base types

Types shared between Clash.CoSim.{Simulator,CodeGeneration}. Should ideally
be housed in Simulator.hs, but FFI code crashes Template Haskell.
-}
module Clash.CoSim.Types where

import Data.Data (Data, Typeable)

import Clash.Prelude (BitPack, BitSize, KnownNat)
import Clash.XException (NFDataX)

-- | Settings passed to the simulator. Does not affect synthetization.
data CoSimSettings = CoSimSettings
    { simulator    :: CoSimulator
    -- ^ Simulator to use for simulation
    , period       :: Int
    -- ^ Number of cycles the simulator runs in a single sample
    , resetFase    :: Bool
    -- ^ TODO: ?
    , files        :: [String]
    -- ^ Include files while running simulator
    , enableStdout :: Bool
    -- ^ Print verbose output to stdout
    } deriving (Show, Typeable, Data)

-- | Default simulator settings use Icarus, have a period of 20 and all other
-- options disabled.
defaultSettings :: CoSimSettings
defaultSettings = CoSimSettings
    { simulator    = Icarus
    , period       = 20
    , resetFase    = False
    , files        = []
    , enableStdout = False
    }

-- | Convenience type alias; only things we know the bit representation of
-- can be simultated.
type ClashType a = ( BitPack a
                   , KnownNat (BitSize a)
                   , NFDataX a
                   )

-- | Supported simulators
data CoSimulator = Icarus
                -- ^ https://github.com/steveicarus/iverilog
                 | ModelSim
                -- ^ https://www.mentor.com/products/fv/modelsim/
                   deriving (Show, Eq, Typeable, Data)
