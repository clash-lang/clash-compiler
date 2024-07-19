{-|
Copyright   :  (C) 2024, Martijn Bastiaan
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities to detect and report GHC / operating system combinations that are
known to be buggy.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Driver.BrokenGhcs where

import Data.Maybe (listToMaybe)
import Data.Version (Version(Version, versionBranch))
import GHC.Platform (OS(..))

#if __GLASGOW_HASKELL__ > 810
import System.Info (fullCompilerVersion)
#endif

import qualified Clash.Util.Interpolate as I
import qualified System.Info

#if __GLASGOW_HASKELL__ <= 810
fullCompilerVersion :: Version
fullCompilerVersion = System.Info.compilerVersion
#endif

-- | Current OS. Currently only recognizes Linux, Windows, and macOS.
os :: OS
os = case System.Info.os of
  "darwin" -> OSDarwin
  "linux" -> OSLinux
  "mingw32" -> OSMinGW32
  _ -> OSUnknown

-- | What OS GHC is broken on (or all)
data BrokenOn = All | SomeOs OS

data GhcVersion = Ghc
  { major0 :: Int
  , major1 :: Int
  , patch :: Int
  }
  deriving (Eq, Ord)

data GhcRange = GhcRange
  { from :: GhcVersion
  -- ^ Start of range, inclusive
  , to :: GhcVersion
  -- ^ End of range, exclusive
  }

-- | Check if a 'GhcVersion' is within a 'GhcRange'
ghcInRange :: GhcVersion -> GhcRange -> Bool
ghcInRange ghc GhcRange{from, to} = from <= ghc && ghc < to

-- | Construct a range of all GHC versions matching a major version
ghcMajor :: Int -> Int -> GhcRange
ghcMajor major0 major1 = GhcRange
  { from=Ghc major0 major1 0
  , to=Ghc major0 (major1 + 1) 0
  }

data Why = Why
  { what :: String
    -- ^ What is broken
  , solution :: String
    -- ^ What can be done to work around or solve the issue
  , issue :: String
    -- ^ Link to issue
  , brokenOn :: [(BrokenOn, GhcRange)]
    -- ^ What operation systems are affected
  }

-- | Get current GHC version expressed as a triple. It probably does something
-- non-sensible on unreleased GHCs.
ghcVersion :: GhcVersion
ghcVersion = Ghc{major0, major1, patch}
 where
  (major0, major1, patch) =
    case fullCompilerVersion of
      Version{versionBranch} ->
        case versionBranch of
          [] -> (0, 0, 1)
          [a] -> (a, 0, 1)
          [a, b] -> (a, b, 1)
          [a, b, c] -> (a, b, c)
          (a:b:c:_) -> (a, b, c)

-- | Pretty print 'Why' into an error message
whyPp :: Why -> String
whyPp Why{what, solution, issue}= [I.i|
  Clash has known issues on #{major0}.#{major1}.#{patch} on your current
  OS. While not completely preventing the compiler from working, we recommend
  switching to another GHC version. Symptoms:

    #{what}

  Consider the following work around or solution:

    #{solution}

  More information can be found at:

    #{issue}

  If you want to ignore this message, pass the following flag to Clash:

    -fclash-ignore-broken-ghcs
  |]
 where
  Ghc{major0, major1, patch} = ghcVersion

-- | Which GHCs are broken and why
brokenGhcs :: [Why]
brokenGhcs = [brokenClashCores, brokenTypeErrors, slowStarts]
 where
  brokenClashCores = Why
    { what = "GHC is known to fail compilation of libraries used by the Clash compiler test suite"
    , solution = "Upgrade to GHC 9.4 or downgrade to GHC 8.10"
    , issue = "<no link>"
    , brokenOn = [(SomeOs OSMinGW32, ghcMajor 9 0)]
    }

  brokenTypeErrors = Why
    { what = "Clash type error messages are indecipherable"
    , solution = "Upgrade to GHC 9.4 or downgrade to GHC 9.0"
    , issue = "<no link>"
    , brokenOn = [(All, ghcMajor 9 2)]
    }

  slowStarts = Why
    { what = "Clash starts really slowly from GHC 9.4.8 up to and including 9.6.2"
    , solution = "Upgrade to GHC 9.6.3 or newer, or downgrade to GHC 9.4.7"
    , issue = "https://github.com/clash-lang/clash-compiler/issues/2710"
    , brokenOn = [(All, GhcRange{from=Ghc 9 4 8, to=Ghc 9 6 3})]
    }

-- | Given a 'BrokenOn', determine whether current OS matches
matchOs :: BrokenOn -> Bool
matchOs All = True
matchOs (SomeOs brokenOs) = os == brokenOs

-- | Given a 'BrokenOn' and 'GhcVersion', determine whether it matches current OS and GHC
matchBroken :: (BrokenOn, GhcRange) -> Bool
matchBroken (brokenOs, brokenRange) = matchOs brokenOs && ghcInRange ghcVersion brokenRange

-- | Get first reason for GHC/OS being broken, if any
broken :: Maybe Why
broken = listToMaybe [why | why <- brokenGhcs, any matchBroken (brokenOn why)]

-- | Throw an error if current OS / GHC version is known to be buggy
assertWorking :: IO ()
assertWorking = case broken of
  Nothing -> pure ()
  Just why -> error (whyPp why)
