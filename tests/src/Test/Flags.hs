{- ORMOLU_DISABLE -}
{-# LANGUAGE CPP #-}

module Test.Flags
  ( outputTest
  , intelVerilog
  , clashWorkaroundGhcMmapCrash
  , cosimEnabled
  ) where

outputTest :: Bool
#ifdef OUTPUTTEST
outputTest = True
#else
outputTest = False
#endif

intelVerilog :: Bool
#ifdef INTEL_VERILOG
intelVerilog = True
#else
intelVerilog = False
#endif

clashWorkaroundGhcMmapCrash :: Bool
#ifdef CLASH_WORKAROUND_GHC_MMAP_CRASH
clashWorkaroundGhcMmapCrash = True
#else
clashWorkaroundGhcMmapCrash = False
#endif

cosimEnabled :: Bool
#ifdef COSIM
cosimEnabled = True
#else
cosimEnabled = False
#endif
