{-# LANGUAGE OverloadedStrings #-}

module T2325f where

import qualified Prelude as P
import Clash.Explicit.Prelude

import Clash.Driver
import Clash.Driver.Manifest
import GHC.Stack
import System.Environment
import System.FilePath
import T2325g
import T2325h

import qualified Control.Exception as Exception
import qualified Data.List as L

f :: Unsigned 8 -> Unsigned 8
f = g . h
{-# NOINLINE f #-}
{-# ANN f (defSyn "f") #-}

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

  assertJustTrue $ "T2325j.j" `before` "T2325g.g"
  assertJustTrue $ "T2325h.h" `before` "T2325g.g"
