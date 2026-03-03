{- ORMOLU_DISABLE -}
{-# LANGUAGE CPP #-}

module Clash.Prelude.Paths (clashPreludeVersion) where

#ifdef CABAL
import qualified Data.Version as Version
import qualified Paths_clash_prelude as Paths_clash_prelude

clashPreludeVersion :: String
clashPreludeVersion = Version.showVersion Paths_clash_prelude.version
#else
clashPreludeVersion :: String
clashPreludeVersion = "development"
#endif
