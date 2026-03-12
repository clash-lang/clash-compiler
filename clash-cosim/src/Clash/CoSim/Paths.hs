{- ORMOLU_DISABLE -}
{-# LANGUAGE CPP #-}

module Clash.CoSim.Paths (getDataFileName) where

#ifdef CABAL
import Paths_clash_cosim (getDataFileName)
#else
import Clash.CoSim.Paths_clash_cosim (getDataFileName)
#endif
