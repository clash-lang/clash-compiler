import           Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import           Clash.Backend
import           Clash.Core.Name
import           Clash.Core.TyCon
import           Clash.Core.Var
import           Clash.Core.VarEnv (mkVarEnv)
import           Clash.Driver.Types
import           Clash.Netlist
import           Clash.Netlist.Types          hiding (backend, hdlDir)

import           Control.DeepSeq              (deepseq)
import           Data.Binary                  (decode)
import           Data.List                    (partition)
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as Text
import           System.Environment           (getArgs)
import           System.FilePath              ((</>))

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
  env <- setupEnv src
  putStrLn $ "Doing netlist generation of " ++ src
  let (transformedBindings,topEntities,primMap,tcm,reprs,topEntity) = env
      primMap'   = fmap (fmap unremoveBBfunc) primMap
      opts1      = opts idirs
      iw         = opt_intWidth opts1
      topEntityS = Text.unpack (nameOcc (varName topEntity))
      modName    = takeWhile (/= '.') topEntityS
      hdlState'  = setModName (Text.pack modName) backend
      (compNames, seen) = genTopNames Nothing True False hdl topEntities
      topEntityMap = mkVarEnv (zip (map topId topEntities) topEntities)
      prefixM    = Nothing
      ite        = ifThenElseExpr hdlState'
      hdlDir     = fromMaybe "." (opt_hdlDir opts1) </>
                         Clash.Backend.name hdlState' </>
                         takeWhile (/= '.') topEntityS
  (netlist,_,_) <-
    genNetlist False opts1 reprs transformedBindings topEntityMap compNames primMap'
               tcm typeTrans iw ite (SomeBackend hdlState') seen hdlDir prefixM topEntity
  netlist `deepseq` putStrLn ".. done\n"

setupEnv
  :: FilePath
  -> IO (BindingMap
        ,[TopEntityT]
        ,CompiledPrimMap'
        ,TyConMap
        ,CustomReprs
        ,Id
        )
setupEnv src = do
  let bin = src ++ ".bin"
  putStrLn $ "Reading from: " ++ bin
  decode <$> B.readFile bin
