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
import Clash.Core.Evaluator.Models
import Clash.Core.Name
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types
import Clash.GHC.GenerateBindings
import Clash.GHC.PartialEval
import Clash.Netlist.BlackBox.Types (HdlSyn(Other))
import Clash.Unique

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
  (bm, tcm, _, _, _, _) <- generateBindings Auto ps ["."] [] (hdlKind backend) src Nothing
  let idsTerms = fmap (\b -> (bindingId b, bindingTerm b)) (eltsUniqMap bm)

  forM_ (sortBy (comparing (nameOcc . varName . fst)) idsTerms) $ \(i,_) -> do
    print (nameOcc $ varName i)

  forM_ (sortBy (comparing (nameOcc . varName . fst)) idsTerms) $ \(i,t) -> do
    putStrLn $ "Evaluating " <> show (nameOcc (varName i))
    print t
    print $ get (nf ghcEvaluator bm (mempty, 0) tcm emptyInScopeSet ids t)

    hFlush stdout
    hFlush stderr
 where
  get (x, _, _) = x

main :: IO ()
main = do
  srcs <- getArgs
  mapM_ runPE srcs

