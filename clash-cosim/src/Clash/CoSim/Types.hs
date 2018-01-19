{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Clash.CoSim.Types where

import Data.Data (Data, Typeable)
import Control.DeepSeq (NFData)

import Clash.Prelude (BitPack, BitSize, KnownNat)

data CoSimSettings = CoSimSettings
    { simulator    :: CoSimulator
    , period       :: Int
    , resetFase    :: Bool
    , files        :: [String]
    , enableStdout :: Bool
    } deriving (Show, Typeable, Data)


defaultSettings :: CoSimSettings
defaultSettings = CoSimSettings
    { simulator    = Icarus
    , period       = 20
    , resetFase    = False
    , files        = []
    , enableStdout = False
    }

type ClashType a = ( BitPack a
                   , KnownNat (BitSize a)
                   , NFData a
                   --, KnownNat (BitSize a + 1)
                   --, KnownNat (BitSize a + 2)
                   -- But gwhy (TODO).
                   )

data CoSimulator = Icarus
                -- ^ https://github.com/steveicarus/iverilog
                 | ModelSim
                -- ^ https://www.mentor.com/products/fv/modelsim/
                   deriving (Show, Eq, Typeable, Data)
