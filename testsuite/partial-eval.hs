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
import Control.Monad
import Data.List (sortBy)
import Data.Ord
import System.Environment
import System.IO

import Util

import Clash.Backend as Backend
import Clash.Backend.VHDL
import Clash.Core.Name
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types
import Clash.GHC.GenerateBindings
import Clash.Netlist.BlackBox.Types (HdlSyn(Other))
import Clash.Unique

#if EXPERIMENTAL_EVALUATOR
import Clash.Core.Evaluator.Models
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
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  let backend = initBackend @VHDLState WORD_SIZE_IN_BITS Other True Nothing
  ps  <- primDirs backend
  ids <- newSupply
  (bm, tcm, _, _, _, _) <- generateBindings Auto ps ["clash-lib/prims/common", "clash-lib/prims/vhdl"] [] (hdlKind backend) src Nothing
  let idsTerms = fmap (\b -> (bindingId b, bindingTerm b)) (eltsUniqMap bm)

  forM_ (sortBy (comparing (nameOcc . varName . fst)) idsTerms) $ \(i,_) -> do
    print (nameOcc $ varName i)

  forM_ (sortBy (comparing (nameOcc . varName . fst)) idsTerms) $ \(i,t) -> do
    putStrLn $ "Evaluating " <> show (nameOcc (varName i))
    print t
#if EXPERIMENTAL_EVALUATOR
    let get (x, _, _) = asTerm x
    print $ get (nf ghcEvaluator bm (mempty, 0) tcm emptyInScopeSet ids t)
#else
    let get (_, _, x) = x
    print $ get (whnf' evaluator bm tcm (mempty, 0) ids emptyInScopeSet False t)
#endif

    hFlush stdout
    hFlush stderr

main :: IO ()
main = do
  srcs <- getArgs
  mapM_ runPE srcs

