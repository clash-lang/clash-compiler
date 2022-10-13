{-|
Copyright   :  (C) 2022     , Google Inc.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

This module provides a way to access static files that are useful when working
with Clash designs.
-}

{-# LANGUAGE CPP #-}

module Clash.DataFiles where

import System.FilePath ((</>))

import Paths_clash_lib (getDataFileName)

-- | A Tcl script that can parse Clash manifest JSON files and emit the correct
-- commands for loading the design into Vivado.
tclConnector :: IO FilePath
tclConnector = getDataFileName $ "data-files" </> "tcl" </> "clashConnector.tcl"

-- | The root directory to primitives bundled with @clash-lib@.
primsRoot :: IO FilePath
#ifdef CABAL
primsRoot = getDataFileName "prims"
#else
primsRoot = "clash-lib" </> "prims"
#endif
