import           Clash.Annotations.TopEntity
import           Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import           Clash.Backend
import           Clash.Core.Name
import           Clash.Core.TyCon
import           Clash.Core.Var
import           Clash.Core.VarEnv
import           Clash.Driver
import           Clash.Driver.Types
import           Clash.Netlist

import           Control.DeepSeq              (deepseq)
import           Control.Exception            (finally)
import           Control.Monad.State          (evalState)
import           Data.Binary                  (decode)
import qualified Data.HashMap.Strict          as HashMap
import           Data.List                    (partition)
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as Text
import           System.Directory             (removeDirectoryRecursive)
import           System.Environment           (getArgs)
import           System.FilePath              (FilePath, (</>))

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

  tmpDir <- createTemporaryClashDirectory

  finally (do
    res <- mapM (benchFile tmpDir idirs1) tests1
    deepseq res $ return ()
   ) (
    removeDirectoryRecursive tmpDir
   )

benchFile :: FilePath -> [FilePath] -> FilePath -> IO ()
benchFile tmpDir idirs src = do
  env <- setupEnv src
  putStrLn $ "Doing netlist generation of " ++ src
  let (transformedBindings,is0,topEntities,primMap,tcm,reprs,topEntity) = env
      primMap'   = fmap (fmap unremoveBBfunc) primMap
      opts1      = opts tmpDir idirs
      iw         = opt_intWidth opts1
      topEntityS = Text.unpack (nameOcc (varName topEntity))
      modName    = takeWhile (/= '.') topEntityS
      hdlState'  = setModName (Text.pack modName) backend
      mkId1      = evalState mkIdentifier hdlState'
      extId      = evalState extendIdentifier hdlState'
      prefixM    = (Nothing,Nothing)
      seen       = HashMap.empty
      hdlDir     = fromMaybe "." (opt_hdlDir opts1) </>
                         Clash.Backend.name hdlState' </>
                         takeWhile (/= '.') topEntityS
  (netlist,_) <-
    genNetlist False opts1 reprs transformedBindings is0 topEntities primMap'
               tcm typeTrans iw mkId1 extId seen hdlDir prefixM topEntity
  netlist `deepseq` putStrLn ".. done\n"

setupEnv
  :: FilePath
  -> IO (BindingMap
        ,InScopeSet
        ,[(Id, Maybe TopEntity, Maybe Id)]
        ,CompiledPrimMap'
        ,TyConMap
        ,CustomReprs
        ,Id
        )
setupEnv src = do
  let bin = src ++ ".bin"
  putStrLn $ "Reading from: " ++ bin
  decode <$> B.readFile bin
