{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Clash.Backend
import           Clash.Backend.VHDL (VHDLState)
import           Clash.Core.Name
import           Clash.Core.Scoped.Syntax     (globalIdName)
import           Clash.Driver.Types
import           Clash.Netlist
import           Clash.Netlist.Scoped (genNetlist)
import           Clash.Netlist.Types          hiding (backend, hdlDir)

import           Clash.GHC.NetlistTypes       (ghcTypeToHWType)

import           Control.DeepSeq              (deepseq)
import           Data.List                    (partition)
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as Text
import           System.Environment           (getArgs)
import           System.FilePath              ((</>))

import           RestorePrimitives            (restorePrimMap)
import           SerialiseInstances
import           BenchmarkCommon

main :: IO ()
main = do
  args <- getArgs
  let (idirs0,tests0) = partition ((== "-i") . take 2) args
  let tests1 | null tests0 = defaultTests
             | otherwise = tests0
      idirs1 = ".":map (drop 2) idirs0

  res <- mapM (benchFile idirs1) tests1
  deepseq res $ return ()

benchFile :: [FilePath] -> FilePath -> IO ()
benchFile idirs src = do
  (transformedBindings, topEntities, primMap, tcm, reprs, topEntity, domains) <- setupEnv src
  (prims, _) <- restorePrimMap primMap
  putStrLn $ "Doing netlist generation of " ++ src

  let env = ClashEnv
                   { envOpts = opts idirs
                   , envTyConMap = tcm
                   , envTupleTyCons = mempty
                   , envPrimitives = prims
                   , envCustomReprs = reprs
                   , envDomains = domains
                   }

      topEntityS = Text.unpack (nameOcc (globalIdName topEntity))
      modName    = takeWhile (/= '.') topEntityS
      hdlState'  = setModName (Text.pack modName) (initBackend @VHDLState (envOpts env))
      (compNames, seen) = genTopNames (envOpts env) hdl topEntities
      topEntityMap = topEntityEnv topEntities
      prefixM    = Nothing
      ite        = ifThenElseExpr hdlState'
      hdlDir     = fromMaybe "." (opt_hdlDir (envOpts env)) </>
                         Clash.Backend.name hdlState' </>
                         takeWhile (/= '.') topEntityS
  (netlist,_,_) <-
    genNetlist env False transformedBindings topEntityMap compNames
               (ghcTypeToHWType (opt_intWidth (envOpts env)))
               ite (SomeBackend hdlState') seen hdlDir prefixM topEntity
  netlist `deepseq` putStrLn ".. done\n"

setupEnv
  :: FilePath
  -> IO NetlistInputs
setupEnv src = do
  let bin = src ++ ".net.bin"
  putStrLn $ "Reading from: " ++ bin
  readInputs bin
