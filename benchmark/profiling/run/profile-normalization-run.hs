{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

import           Clash.Driver
import           Clash.Driver.Types           (ClashEnv(..), ClashOpts(opt_intWidth), GlobalBindings)
import qualified Clash.Util.Supply            as Supply

import           Clash.GHC.NetlistTypes       (ghcTypeToHWType)

import           Control.DeepSeq              (deepseq)
import           Data.List                    (partition)
import           System.Directory             (createDirectoryIfMissing)
import           System.Environment           (getArgs, lookupEnv)
import           System.FilePath              ((</>), takeFileName)

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
  supplyN <- Supply.newSupply
  (bindingsMap, tcm, tupTcm, primMap, reprs, topEntityNames, topEntity, domains) <- setupEnv src
  putStrLn $ "Doing normalization of " ++ src

  let clashEnv = ClashEnv
                   { envOpts = opts idirs
                   , envTyConMap = tcm
                   , envTupleTyCons = tupTcm
                   , envPrimitives = fmap (fmap unremoveBBfunc) primMap
                   , envCustomReprs = reprs
                   , envDomains = domains
                   }

  res <- normalizeEntity clashEnv bindingsMap
                   (ghcTypeToHWType (opt_intWidth (envOpts clashEnv)))
                   topEntityNames supplyN topEntity
  res `deepseq` putStrLn ".. done\n"
  writeNormalized src res

-- | Write the normalized bindings, if CLASH_NORMALIZED_OUT is set: to
-- @<dir>/<design>.norm.bin@, for @clash-differential normalized@
writeNormalized :: FilePath -> GlobalBindings -> IO ()
writeNormalized src res =
  lookupEnv "CLASH_NORMALIZED_OUT" >>= \case
    Just dir@(_:_) -> do
      createDirectoryIfMissing True dir
      let out = dir </> takeFileName src ++ ".norm.bin"
      writeInputs out res
      putStrLn ("Wrote the normalized bindings to " ++ out)
    _ -> pure ()

setupEnv :: FilePath -> IO NormalizationInputs
setupEnv src = do
  let bin = src ++ ".bin"
  putStrLn $ "Reading from: " ++ bin
  readInputs bin
