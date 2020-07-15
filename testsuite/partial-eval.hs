{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

#include "MachDeps.h"

module Main where

import Control.Concurrent.Supply
-- import Control.Monad
-- import Data.List as List (find, sortBy)
import Data.Text
import System.Environment
import System.FilePath
import System.IO

import Util

import Clash.Backend as Backend
import Clash.Backend.VHDL
import Clash.Core.Name
import Clash.Core.Pretty
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types
import Clash.GHC.GenerateBindings
import Clash.Netlist.BlackBox.Types (HdlSyn(Other))
import Clash.Unique

#if EXPERIMENTAL_EVALUATOR
import Clash.Core.Evaluator.Models
import Clash.Core.Termination
import Clash.GHC.PartialEval
#else
import Clash.Core.Evaluator.Types
import Clash.GHC.Evaluator
#endif

opts :: ClashOpts
opts = defClashOpts
  { opt_cachehdl = False
  , opt_errorExtra = True
  }

runPE :: FilePath -> IO ()
runPE src = do
  let srcEnt = takeBaseName src
  let backend = initBackend @VHDLState WORD_SIZE_IN_BITS Other True Nothing
  ps  <- primDirs backend
  ids <- newSupply
  (bm, tcm, _, _, _, _, _) <- generateBindings Auto ps ["clash-lib/prims/common", "clash-lib/prims/vhdl"] [] (hdlKind backend) src Nothing
  let idsTerms = fmap (\b -> (bindingId b, bindingTerm b)) (eltsUniqMap bm)

  let srcEnts = Prelude.filter (isPrefixOf (pack srcEnt) . nameOcc . varName . fst) idsTerms

--case List.find (isSuffixOf "topEntity" . nameOcc . varName . fst) idsTerms of
  flip mapM_ srcEnts $
    \(i, t) -> do
      putStrLn $ "Evaluating " <> show (nameOcc (varName i))
      tracePprM t
      putStrLn ""
#if EXPERIMENTAL_EVALUATOR
      let ri   = mkRecInfo bm
      let env  = mkGlobalEnv i bm ri 10 (mempty, 0) tcm emptyInScopeSet ids
      tracePprM . asTerm $ fst (runEval env (evaluateNf ghcEvaluator t))
#else
      let get (_, _, x) = x
      tracePprM $ get (whnf' evaluator bm tcm (mempty, 0) ids emptyInScopeSet False t)
#endif

      putStrLn ""
      hFlush stdout
      hFlush stderr

{-
    Nothing ->
      error "No topEntity in module"
-}

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  getArgs >>= mapM_ runPE

