{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module BenchmarkCommon where

import Clash.Annotations.Primitive (HDL(VHDL))
import Clash.Backend
import Clash.Backend.VHDL
import Clash.Core.Var
import Clash.Driver
import Clash.Driver.Types
import Clash.Netlist.Types (TopEntityT(topId))
import Clash.Util.Supply as Supply

import Clash.GHC.PartialEval
import Clash.GHC.Evaluator
import Clash.GHC.GenerateBindings
import Clash.GHC.NetlistTypes

import qualified Control.Concurrent.MVar as MVar

#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Driver.Monad as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Driver.Env.Types as GHC
import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Settings as GHC
import qualified GHC.Utils.Fingerprint as GHC
#elif MIN_VERSION_ghc(9,0,0)
import qualified GHC.Driver.Monad as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Driver.Types as GHC
import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Settings as GHC
import qualified GHC.Utils.Fingerprint as GHC
#endif

defaultTests :: [FilePath]
defaultTests =
  [ "examples/FIR.hs"
  , "examples/Reducer.hs"
  , "examples/Queens.hs"
  , "benchmark/tests/BundleMapRepeat.hs"
  , "benchmark/tests/PipelinesViaFolds.hs"
  , "tests/shouldwork/Basic/AES.hs"
  , "tests/shouldwork/Basic/T1354B.hs"
  ]

opts :: [FilePath] -> ClashOpts
opts idirs =
  defClashOpts{
      opt_cachehdl=False
    , opt_clear=True
    , opt_errorExtra = True
    , opt_importPaths=idirs
    , opt_specLimit=100 -- For "ManyEntitiesVaried"
    }

hdl :: HDL
hdl = VHDL

runInputStage
  :: [FilePath]
  -> FilePath
  -> IO (ClashEnv, ClashDesign)
runInputStage idirs src = do
  let o = opts idirs
  let backend = initBackend @VHDLState o
  pds <- primDirs backend
  generateBindings o action pds (opt_importPaths o) [] (hdlKind backend) src Nothing
 where
#if MIN_VERSION_ghc(9,0,0)
  action = do
    env <- GHC.getSession
    let df0 = GHC.hsc_dflags env
#if MIN_VERSION_ghc(9,4,0)
        df1 = addOptP "-DCLASH_OPAQUE=OPAQUE" df0
#else
        df1 = addOptP "-DCLASH_OPAQUE=NOINLINE" df0
#endif
        df2 = GHC.xopt_set df1 LangExt.Cpp
    GHC.setSession (env {GHC.hsc_dflags = df2})

  addOptP :: String -> GHC.DynFlags -> GHC.DynFlags
  addOptP   f = alterToolSettings $ \s -> s
            { GHC.toolSettings_opt_P   = f : GHC.toolSettings_opt_P s
            , GHC.toolSettings_opt_P_fingerprint = fingerprintStrings (f : GHC.toolSettings_opt_P s)
            }

  alterToolSettings :: (GHC.ToolSettings -> GHC.ToolSettings) -> GHC.DynFlags -> GHC.DynFlags
  alterToolSettings f dynFlags = dynFlags { GHC.toolSettings = f (GHC.toolSettings dynFlags) }

  fingerprintStrings :: [String] -> GHC.Fingerprint
  fingerprintStrings ss = GHC.fingerprintFingerprints $ map GHC.fingerprintString ss
#else
  action = return ()
#endif

runNormalisationStage
  :: [FilePath]
  -> String
  -> IO (ClashEnv, ClashDesign, Id)
runNormalisationStage idirs src = do
  supplyN <- Supply.newSupply
  lock <- MVar.newMVar ()
  (env, design) <- runInputStage idirs src
  let topEntityNames = fmap topId (designEntities design)
  case topEntityNames of
    topEntity:_ -> do
      transformedBindings <-
            normalizeEntity env (designBindings design)
              (ghcTypeToHWType (opt_intWidth (opts idirs)))
              ghcEvaluator
              evaluator
              lock
              topEntityNames supplyN topEntity
      return (env, design{designBindings=transformedBindings},topEntity)
    _ -> error "no top entities"
