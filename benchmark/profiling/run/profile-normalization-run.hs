{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

import           Clash.Driver
import           Clash.Rewrite.Capture        (takeCaptureLog)
import           Clash.Driver.Types           (ClashEnv(..), ClashOpts(opt_intWidth))
import qualified Clash.Util.Supply            as Supply

import           Clash.GHC.Evaluator
import           Clash.GHC.NetlistTypes       (ghcTypeToHWType)

import           Control.DeepSeq              (deepseq)
import           Data.Binary                  (decode)
import           Data.List                    (partition)
import           System.Directory             (createDirectoryIfMissing)
import           System.Environment           (getArgs, lookupEnv)
import           System.FilePath              ((</>), takeFileName)
import           Data.Time.Clock              (diffUTCTime, getCurrentTime)
import           Control.Monad                (when)
import           GHC.RTS.Flags                (getGCFlags, minAllocAreaSize)
import           System.IO                    (hClose, hPutStrLn, stderr)
import           System.Process
  (StdStream (CreatePipe), createProcess, proc, std_in, waitForProcess)

import qualified Data.ByteString.Lazy as B

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
  inputs@(bindingsMap, tcm, tupTcm, primMap, reprs, topEntityNames, topEntity, domains) <- setupEnv src
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
                   evaluator
                   topEntityNames supplyN topEntity
  res `deepseq` putStrLn ".. done\n"
  writeCorpus src inputs

-- | Write the captured sites at which transformations fired, if
-- CLASH_CAPTURE_REWRITES is set: see "Clash.Rewrite.Capture"
writeCorpus :: FilePath -> NormalizationInputs -> IO ()
writeCorpus src (_, tcm, tupTcm, primMap, reprs, topEntityNames, topEntity, domains) =
  lookupEnv "CLASH_CAPTURE_REWRITES" >>= \case
    Just dir@(_:_) -> takeCaptureLog >>= \case
      Just cl -> do
        -- Encoding the captures keeps many 'StableName's alive, which every
        -- garbage collection traverses: collect less often
        allocArea <- (* 4096) . minAllocAreaSize <$> getGCFlags
        when (allocArea < 64 * 1024 * 1024) $
          hPutStrLn stderr "Writing captures is a lot faster with a bigger allocation area: +RTS -A64m"
        createDirectoryIfMissing True dir
        let out = dir </> takeFileName src ++ ".corpus.zst"
            -- The bindings at the end of normalization are in the log
            inputs = (mempty, tcm, tupTcm, primMap, reprs, topEntityNames, topEntity, domains)
        -- Captures repeat types and terms a lot, so compress them: by a factor
        -- of 100 or more
        start <- getCurrentTime
        (Just h, _, _, p) <- createProcess (proc "zstd" ["-q", "-f", "-o", out]) {std_in = CreatePipe}
        hPutCorpus h inputs cl
        hClose h
        _ <- waitForProcess p
        end <- getCurrentTime
        putStrLn ("Wrote captures to " ++ out ++ " in " ++ show (diffUTCTime end start))
      Nothing -> putStrLn "No captures: is CLASH_CAPTURE_REWRITES set at startup?"
    _ -> pure ()

setupEnv :: FilePath -> IO NormalizationInputs
setupEnv src = do
  let bin = src ++ ".bin"
  putStrLn $ "Reading from: " ++ bin
  decode <$> B.readFile bin
