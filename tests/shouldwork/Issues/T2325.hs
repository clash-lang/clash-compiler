{-# LANGUAGE OverloadedStrings #-}

module T2325 where

import qualified Prelude as P
import Clash.Explicit.Prelude

import Clash.Driver
import Clash.Driver.Manifest
import GHC.Stack
import System.Environment
import System.FilePath

import qualified Control.Exception as Exception
import qualified Data.List as L

type T = Unsigned 8 -> Unsigned 8

f :: T
f = g . h
{-# NOINLINE f #-}
{-# ANN f (defSyn "f") #-}

g :: T
g = h . j
{-# NOINLINE g #-}
{-# ANN g (defSyn "g") #-}

h :: T
h = (+ 5)
{-# NOINLINE h #-}
{-# ANN h (defSyn "h") #-}

j :: T
j = (+ 10)
{-# NOINLINE j #-}
{-# ANN j (defSyn "j") #-}

assertBool :: HasCallStack => Bool -> IO ()
assertBool b = Exception.assert b pure ()

assertJustTrue :: HasCallStack => Maybe Bool -> IO ()
assertJustTrue = assertBool . (== Just True)

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  Just manifest <- readManifest (topDir </> show 'f </> "clash-manifest.json")
  let
    deps = transitiveDependencies manifest
    a `before` b = do
      aIx <- L.elemIndex a deps
      bIx <- L.elemIndex b deps
      Just (aIx < bIx)

  assertJustTrue $ "T2325.j" `before` "T2325.g"
  assertJustTrue $ "T2325.h" `before` "T2325.g"
