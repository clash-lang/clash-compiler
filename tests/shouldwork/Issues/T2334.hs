{-# LANGUAGE CPP #-}

module T2334 where

import qualified Prelude as P
import Clash.Explicit.Prelude

import Clash.Driver
import Clash.Driver.Manifest
import GHC.Stack
import System.Environment
import System.FilePath

import qualified Control.Exception as Exception

topEntity ::
  Clock System ->
  Reset System ->
  Signal System Int ->
  Signal System Int ->
  Signal System Int ->
  Signal System Int
topEntity clk rst = assert clk rst "FileOrder"
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

assertBool :: HasCallStack => Bool -> IO ()
assertBool b = Exception.assert b pure ()

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  Just manifest <- readManifest (topDir </> show 'topEntity </> "clash-manifest.json")
  let ([sdc, slv2string, types, top], _hashes) = P.unzip (fileNames manifest)

  assertBool (sdc == "topEntity.sdc")
  assertBool (P.take 20 slv2string == "topEntity_slv2string")
  assertBool (types == "T2334_topEntity_types.vhdl")
  assertBool (top == "topEntity.vhdl")
