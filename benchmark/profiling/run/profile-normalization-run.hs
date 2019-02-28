import           Clash.Annotations.TopEntity
import           Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import           Clash.Core.TyCon
import           Clash.Core.Var
import           Clash.Driver
import           Clash.Driver.Types
import           Clash.GHC.Evaluator          (reduceConstant)

import qualified Control.Concurrent.Supply    as Supply
import           Control.DeepSeq              (deepseq)
import           Control.Exception            (finally)
import           Data.Binary                  (decode)
import           Data.IntMap.Strict           (IntMap)
import           System.Directory             (removeDirectoryRecursive)
import           System.Environment           (getArgs)
import           System.FilePath              (FilePath)

import qualified Data.ByteString.Lazy as B

import           SerialiseInstances
import           BenchmarkCommon

main :: IO ()
main = do
  args <- getArgs
  let tests | null args = defaultTests
            | otherwise = args

  tmpDir <- createTemporaryClashDirectory

  finally (do
    res <- mapM (benchFile tmpDir) tests
    deepseq res $ return ()
   ) (
    removeDirectoryRecursive tmpDir
   )

benchFile :: FilePath -> FilePath -> IO ()
benchFile tmpDir src = do
  supplyN <- Supply.newSupply
  env <- setupEnv src
  putStrLn $ "Doing normalization of " ++ src
  let (bindingsMap,tcm,tupTcm,_topEntities,primMap,reprs,topEntityNames,topEntity) = env
      primMap' = fmap (fmap unremoveBBfunc) primMap
      res :: BindingMap
      res = fst $ normalizeEntity reprs bindingsMap primMap' tcm tupTcm typeTrans reduceConstant
                   topEntityNames (opts tmpDir) supplyN topEntity
  res `deepseq` putStrLn ".. done\n"

setupEnv :: FilePath -> IO (BindingMap,TyConMap,IntMap TyConName
                           ,[(Id, Maybe TopEntity, Maybe Id)],CompiledPrimMap'
                           ,CustomReprs,[Id],Id)
setupEnv src = do
  let bin = src ++ ".bin"
  putStrLn $ "Reading from: " ++ bin
  decode <$> B.readFile bin
