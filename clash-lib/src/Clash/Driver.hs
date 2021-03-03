{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017     , QBayLogic, Google Inc.
                     2020       QBayLogic

  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Module that connects all the parts of the Clash compiler library
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Driver where

import qualified Control.Concurrent.Supply        as Supply
import           Control.DeepSeq
import           Control.Exception                (tryJust, bracket, throw)
import           Control.Lens                     (view, _4)
import qualified Control.Monad                    as Monad
import           Control.Monad                    (guard, when, unless, foldM)
import           Control.Monad.Catch              (MonadMask)
import           Control.Monad.Extra              (whenM)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.State              (evalState, get)
import           Control.Monad.State.Strict       (State)
import qualified Control.Monad.State.Strict       as State
import           Data.Coerce                      (coerce)
import           Data.Default
import           Data.Hashable                    (hash)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import qualified Data.HashSet                     as HashSet
import           Data.Proxy                       (Proxy(..))
import           Data.IntMap                      (IntMap)
import           Data.List                        (intercalate)
import           Data.Maybe                       (fromMaybe)
import           Data.Semigroup.Monad
import qualified Data.Set                         as Set
import qualified Data.Text
import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy                   as Text
import qualified Data.Text.Lazy.IO                as Text
import           Data.Text.Prettyprint.Doc        (pretty)
import           Data.Text.Prettyprint.Doc.Extra
  (Doc, LayoutOptions (..), PageWidth (..) , layoutPretty, renderLazy,
   renderOneLine)
import qualified Data.Time.Clock                  as Clock
import qualified Language.Haskell.Interpreter     as Hint
import qualified Language.Haskell.Interpreter.Extension as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Hint
import qualified System.Directory                 as Directory
import           System.Environment               (getExecutablePath)
import           System.FilePath                  ((</>), (<.>))
import qualified System.FilePath                  as FilePath
import qualified System.IO                        as IO
import           System.IO.Error                  (isDoesNotExistError)
import           System.IO.Temp
  (getCanonicalTemporaryDirectory, withTempDirectory)
import           Text.Trifecta.Result
  (Result(Success, Failure), _errDoc)
import           Text.Read                        (readMaybe)

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Builtin.Names                 (eqTyConKey, ipClassKey)
import           GHC.Types.Unique                  (getKey)

import           GHC.Types.SrcLoc                  (SrcSpan)
import           GHC.Utils.Misc                    (OverridingBool(Auto))
#else
import           PrelNames               (eqTyConKey, ipClassKey)
import           Unique                  (getKey)

import           SrcLoc                           (SrcSpan)
import           Util                             (OverridingBool(Auto))
#endif
import           GHC.BasicTypes.Extra             ()

import           Clash.Annotations.Primitive
  (HDL (..))
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs)
import           Clash.Annotations.TopEntity
  (TopEntity (..), PortName(PortName, PortProduct))
import           Clash.Annotations.TopEntity.Extra ()
import           Clash.Backend

#if EXPERIMENTAL_EVALUATOR
import           Clash.Core.PartialEval           (Evaluator)
#else
import           Clash.Core.Evaluator.Types       (Evaluator)
#endif

import           Clash.Core.Name                  (Name (..))
import           Clash.Core.Pretty                (PrettyOptions(..), showPpr')
import           Clash.Core.Term                  (Term)
import           Clash.Core.Type
  (Type(ForAllTy, LitTy, AnnType), TypeView(..), tyView, mkFunTy, LitTy(SymTy))
import           Clash.Core.TyCon                 (TyConMap, TyConName)
import           Clash.Core.Util                  (shouldSplit)
import           Clash.Core.Var
  (Id, varName, varUniq, varType)
import           Clash.Core.VarEnv
  (VarEnv, elemVarEnv, eltsVarEnv, emptyVarEnv, lookupVarEnv, lookupVarEnv', mkVarEnv)
import           Clash.Debug                      (debugIsOn)
import           Clash.Driver.Types
import           Clash.Edalize.Edam
import           Clash.Netlist                    (genNetlist, genTopNames)
import           Clash.Netlist.BlackBox.Parser    (runParse)
import           Clash.Netlist.BlackBox.Types     (BlackBoxTemplate, BlackBoxFunction)
import qualified Clash.Netlist.Id                 as Id
import           Clash.Netlist.Types
  (IdentifierText, BlackBox (..), Component (..), FilteredHWType, HWMap, SomeBackend (..),
   TopEntityT(..), TemplateFunction, findClocks)
import           Clash.Normalize                  (checkNonRecursive, cleanupGraph,
                                                   normalize, runNormalization)
import           Clash.Normalize.Util             (callGraph, tvSubstWithTyEq)
import qualified Clash.Primitives.Sized.ToInteger as P
import qualified Clash.Primitives.Sized.Vector    as P
import qualified Clash.Primitives.GHC.Int         as P
import qualified Clash.Primitives.GHC.Word        as P
import qualified Clash.Primitives.Intel.ClockGen  as P
import qualified Clash.Primitives.Verification    as P
import           Clash.Primitives.Types
import           Clash.Primitives.Util            (hashCompiledPrimMap)
import           Clash.Signal.Internal
import           Clash.Unique
  (Unique, keysUniqMap, lookupUniqMap')
import           Clash.Util.Interpolate           (i)
import           Clash.Util
  (ClashException(..), HasCallStack, first, reportTimeDiff,
   wantedLanguageExtensions, unwantedLanguageExtensions, curLoc)
import           Clash.Util.Graph                 (reverseTopSort)

-- | Worker function of 'splitTopEntityT'
splitTopAnn
  :: TyConMap
  -> SrcSpan
  -- ^ Source location of top entity (for error reporting)
  -> Type
  -- ^ Top entity body
  -> TopEntity
  -- ^ Port annotations for top entity
  -> TopEntity
  -- ^ New top entity with split ports (or the old one if not applicable)
splitTopAnn tcm sp typ@(tyView -> FunTy {}) t@Synthesize{t_inputs} =
  t{t_inputs=go typ t_inputs}
 where
  go :: Type -> [PortName] -> [PortName]
  go _ [] = []
  go (tyView -> FunTy a res) (p:ps)
   | shouldNotHavePortName a
     -- Insert dummy PortName for args for which the user shouldn't have
     -- to provide a name.
     -- Ideally this would be any (non Hidden{Clock,Reset,Enable}) constraint.
     -- But because we can't properly detect constraints,
     -- we only skip some specific one. see "shouldNotHavePortName"
     = PortName "" : go res (p:ps)
   | otherwise =
    case shouldSplit tcm a of
      Just (_,argTys@(_:_:_)) ->
        -- Port must be split up into 'n' pieces.. can it?
        case p of
          PortProduct nm portNames0 ->
            let
              n = length argTys
              newPortNames = map (PortName . show) [(0::Int)..]
              portNames1 = map (prependName nm) (portNames0 ++ newPortNames)
              newLam = foldr1 mkFunTy (argTys ++ [res])
            in
              go newLam (take n portNames1 ++ ps)
          PortName nm ->
            throw (flip (ClashException sp) Nothing $ [i|
              Couldn't separate clock, reset, or enable from a product type due
              to a malformed Synthesize annotation. All clocks, resets, and
              enables should be given a unique port name. Type to be split:

                #{showPpr' (PrettyOptions False True False False) a}

              Given port annotation: #{p}. You might want to use the
              following instead: PortProduct #{show nm} []. This allows Clash to
              autogenerate names based on the name #{show nm}.
            |])
      _ ->
        -- No need to split the port, carrying on..
        p : go res ps
  go (ForAllTy _tyVar ty) ps = go ty ps
  go _ty ps = ps

  prependName :: String -> PortName -> PortName
  prependName "" pn = pn
  prependName p (PortProduct nm ps) = PortProduct (p ++ "_" ++ nm) ps
  prependName p (PortName nm) = PortName (p ++ "_" ++ nm)

  -- Returns True for
  --   * type equality constraints (~)
  --   * HasCallStack
  shouldNotHavePortName :: Type -> Bool
  shouldNotHavePortName (tyView -> TyConApp (nameUniq -> tcUniq) tcArgs)
    | tcUniq == getKey eqTyConKey = True
    | tcUniq == getKey ipClassKey
    , [LitTy (SymTy "callStack"), _] <- tcArgs = True
  shouldNotHavePortName _ = False

splitTopAnn tcm sp (ForAllTy _tyVar typ) t = splitTopAnn tcm sp typ t
splitTopAnn tcm sp (AnnType _anns typ) t = splitTopAnn tcm sp typ t
splitTopAnn _tcm _sp _typ t = t

-- When splitting up a single argument into multiple arguments (see docs of
-- 'separateArguments') we should make sure to update TopEntity annotations
-- accordingly. See: https://github.com/clash-lang/clash-compiler/issues/1033
splitTopEntityT
  :: HasCallStack
  => TyConMap
  -> BindingMap
  -> TopEntityT
  -> TopEntityT
splitTopEntityT tcm bindingsMap tt@(TopEntityT id_ (Just t@(Synthesize {})) _) =
  case lookupVarEnv id_ bindingsMap of
    Just (Binding _id sp _ _ _) ->
      tt{topAnnotation=Just (splitTopAnn tcm sp (varType id_) t)}
    Nothing ->
      error "Internal error in 'splitTopEntityT'. Please report as a bug."
splitTopEntityT _ _ t = t

-- | Remove constraints such as 'a ~ 3'.
removeForAll :: TopEntityT -> TopEntityT
removeForAll (TopEntityT var annM tbM) =
  TopEntityT var{varType=tvSubstWithTyEq (varType var)} annM tbM

-- | Given a list of all found top entities and _maybe_ a top entity (+dependencies)
-- passed in by '-main-is', return the list of top entities Clash needs to
-- compile.
selectTopEntities :: [TopEntityT] -> Maybe (TopEntityT, [TopEntityT]) -> [TopEntityT]
selectTopEntities topEntities mainTopEntity =
  maybe topEntities (uncurry (:)) mainTopEntity

-- | Get modification data of current clash binary.
getClashModificationDate :: IO Clock.UTCTime
getClashModificationDate = Directory.getModificationTime =<< getExecutablePath

hdlFromBackend :: forall backend. Backend backend => Proxy backend -> HDL
hdlFromBackend _ = hdlKind (undefined :: backend)

-- | Create a set of target HDL files for a set of functions
generateHDL
  :: forall backend . Backend backend
  => CustomReprs
  -> HashMap Data.Text.Text VDomainConfiguration
  -- ^ Known domains to configurations
  -> BindingMap
  -- ^ Set of functions
  -> Maybe backend
  -> CompiledPrimMap
  -- ^ Primitive / BlackBox Definitions
  -> TyConMap
  -- ^ TyCon cache
  -> IntMap TyConName
  -- ^ Tuple TyCon cache
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcoded 'Type' -> 'HWType' translator
  -> Evaluator
  -- ^ Hardcoded evaluator for partial evaluation
  -> [TopEntityT]
  -- ^ All topentities and associated testbench
  -> Maybe (TopEntityT, [TopEntityT])
  -- ^ Main top entity to compile. If Nothing, all top entities in previous
  -- argument will be compiled.
  -> ClashOpts
  -- ^ Debug information level for the normalization process
  -> (Clock.UTCTime,Clock.UTCTime)
  -> IO ()
generateHDL reprs domainConfs bindingsMap hdlState primMap tcm tupTcm typeTrans eval
  topEntities0 mainTopEntity opts (startTime,prepTime) = do
    case opt_dbgRewriteHistoryFile opts of
      Nothing -> pure ()
      Just histFile -> whenM (Directory.doesFileExist histFile) (Directory.removeFile histFile)
    let (tes, deps) = sortTop bindingsMap topEntities1
     in go prepTime initIs HashMap.empty deps tes
 where
  (compNames, initIs) = genTopNames topPrefixM escpIds lwIds hdl topEntities1
  topEntityMap = mkVarEnv (fmap (\x -> (topId x, x)) topEntities1)
  topPrefixM = opt_componentPrefix opts
  hdl = hdlFromBackend (Proxy @backend)
  escpIds = opt_escapedIds opts
  lwIds = opt_lowerCaseBasicIds opts
  topEntities1 =
    map
      (removeForAll . splitTopEntityT tcm bindingsMap)
      (selectTopEntities topEntities0 mainTopEntity)

  go
    :: Clock.UTCTime
    -> Id.IdentifierSet
    -> HashMap Unique [EdamFile]
    -> HashMap Unique [Unique]
    -> [TopEntityT]
    -> IO ()
  go prevTime _ _ _ [] = putStrLn $ "Clash: Total compilation took " ++
                                reportTimeDiff prevTime startTime

  -- Process the next TopEntity
  go prevTime seen0 edamFiles deps (TopEntityT topEntity annM benchM:topEntities') = do
  let topEntityS = Data.Text.unpack (nameOcc (varName topEntity))
  putStrLn $ "Clash: Compiling " ++ topEntityS

  -- Some initial setup
  let modName1 = takeWhile (/= '.') topEntityS
      topNm = lookupVarEnv' compNames topEntity
      (modNameS, fmap Data.Text.pack -> prefixM) = case topPrefixM of
        Just (Data.Text.unpack -> p)
          | not (null p) -> case annM of
            -- Prefix top names with 'p', prefix other with 'p_tname'
            Just ann ->
              let nm = p <> "_" <> t_name ann in
              (nm, Just nm)
            -- Prefix top names with 'p', prefix other with 'p'
            _ -> (p <> "_" <> modName1, Just p)
          | Just ann <- annM -> case hdl of
              -- Prefix other with 't_name'
              VHDL -> (t_name ann, Just modNameS)
              _    -> (t_name ann, Nothing)
        _ -> case annM of
          Just ann -> case hdlKind (undefined :: backend) of
            VHDL -> (t_name ann, Nothing)
            -- Prefix other with 't_name'
            _ -> (t_name ann, Just modNameS)
          _ -> (modName1, Nothing)
      modNameT  = Data.Text.pack modNameS
      iw        = opt_intWidth opts
      hdlsyn    = opt_hdlSyn opts
      forceUnd  = opt_forceUndefined opts
      xOpt      = coerce (opt_aggressiveXOptBB opts)
      hdlState' = setModName modNameT
                $ fromMaybe (initBackend iw hdlsyn escpIds lwIds forceUnd xOpt :: backend) hdlState
      hdlDir    = fromMaybe "." (opt_hdlDir opts) </>
                        Clash.Backend.name hdlState' </>
                        takeWhile (/= '.') topEntityS
      ite       = ifThenElseExpr hdlState'
      topNmT    = Id.toText topNm
      topNmU    = Data.Text.unpack topNmT

  unless (opt_cachehdl opts) $ putStrLn "Clash: Ignoring .manifest files"

  -- Calculate the hash over the callgraph and the topEntity annotation
  (useCacheTop,useCacheBench,manifest) <- do
    clashModDate <- getClashModificationDate

    let primMapHash = hashCompiledPrimMap primMap

    let optsHash = hash opts { -- Ignore the following settings, they don't
                               -- affect the generated HDL:
                               -- 1. Debug
                               opt_dbgLevel           = DebugNone
                             , opt_dbgTransformations = Set.empty
                             , opt_dbgRewriteHistoryFile = Nothing
                               -- 2. Caching
                             , opt_cachehdl           = True
                               -- 3. Warnings
                             , opt_primWarn           = True
                             , opt_color              = Auto
                             , opt_errorExtra         = False
                             , opt_checkIDir          = True
                               -- 4. Optional output
                             , opt_edalize            = False
                               -- Ignore the following settings, they don't
                               -- affect the generated HDL. However, they do
                               -- influence whether HDL can be generated at all.
                               --
                               -- So later on we check whether the new flags
                               -- changed in such a way that they could affect
                               -- successful compilation, and use that information
                               -- to decide whether to use caching or not.
                               --
                               -- 1. termination measures
                             , opt_inlineLimit       = 20
                             , opt_specLimit         = 20
                               -- 2. Float support
                             , opt_floatSupport      = False
                               -- Finally, also ignore the HDL dir setting,
                               -- because when a user moves the entire dir
                               -- with generated HDL, they probably still want
                               -- to use that as a cache
                             , opt_hdlDir            = Nothing
                             }

    let
      topHash =
        hash ( annM
             , primMapHash
             , show clashModDate
             , callGraphBindings bindingsMap topEntity
             , optsHash
             )

    let
      benchHashM =
        case benchM of
          Nothing -> Nothing
          Just bench ->
            let terms = callGraphBindings bindingsMap bench in
            Just (hash (annM, primMapHash, show clashModDate, terms, optsHash))

    let successFlagsI = (opt_inlineLimit opts,opt_specLimit opts,opt_floatSupport opts)
        manifestI    = Manifest (topHash,benchHashM) successFlagsI [] [] [] [] [] []

    let
      manFile =
        case annM of
          Nothing -> hdlDir </> topNmU <.> "manifest"
          _       -> hdlDir </> topNmU </> topNmU <.> "manifest"

    manM <- if not (opt_cachehdl opts)
            then return Nothing -- ignore manifest file because -fclash-nocache
            else (>>= readMaybe) . either (const Nothing) Just <$>
              tryJust (guard . isDoesNotExistError) (readFile manFile)
    return (maybe (False,False,manifestI)
                  (\man ->
                    let allowCache (inl0,spec0,fl0) (inl1,spec1,fl1) =
                          inl0 <= inl1 && spec0 <= spec1 && (not (fl0 && not fl1))
                        flagsAllowCache = allowCache (successFlags man) successFlagsI
                    in  (flagsAllowCache && fst (manifestHash man) == topHash
                        ,flagsAllowCache && snd (manifestHash man) == benchHashM
                        ,man { manifestHash = (topHash,benchHashM)
                             , successFlags  = if flagsAllowCache
                                                 then successFlags man
                                                 else successFlagsI
                             }
                        ))
                  manM)

  (supplyN,supplyTB) <- Supply.splitSupply
                    . snd
                    . Supply.freshId
                   <$> Supply.newSupply
  let topEntityNames = map topId topEntities1

  (topTime,manifest',seen2,edamFiles') <- if useCacheTop
    then do
      putStrLn ("Clash: Using cached result for: " ++ Data.Text.unpack (nameOcc (varName topEntity)))
      topTime <- Clock.getCurrentTime
      let edamFiles' = HashMap.insert (varUniq topEntity) (asEdamFile topNm <$> fileNames manifest) edamFiles
      let seen1 = State.execState (mapM_ Id.addRaw (componentNames manifest)) seen0
      return (topTime, manifest, seen1, edamFiles')
    else do
      -- 1. Normalize topEntity
      let transformedBindings = normalizeEntity reprs bindingsMap primMap tcm tupTcm
                                  typeTrans eval topEntityNames opts supplyN
                                  topEntity

      normTime <- transformedBindings `deepseq` Clock.getCurrentTime
      let prepNormDiff = reportTimeDiff normTime prevTime
      putStrLn $ "Clash: Normalization took " ++ prepNormDiff

      -- 2. Generate netlist for topEntity

      -- [Note] Create HDL dir before netlist generation
      --
      -- Already create the directory where the HDL ends up being generated, as
      -- we use directories relative to this final directory to find manifest
      -- files belonging to other top entities. Failing to do so leads to #463
      let dir = hdlDir </> maybe "" (const modNameS) annM
      prepareDir (opt_cleanhdl opts) (extension hdlState') dir
      -- Now start the netlist generation
      (topComponent, netlist, seen2) <-
        genNetlist False opts reprs transformedBindings topEntityMap compNames primMap
                   tcm typeTrans iw ite (SomeBackend hdlState') seen0 hdlDir prefixM topEntity

      netlistTime <- netlist `deepseq` Clock.getCurrentTime
      let normNetDiff = reportTimeDiff netlistTime normTime
      putStrLn $ "Clash: Netlist generation took " ++ normNetDiff

      -- 3. Generate topEntity wrapper
      let (hdlDocs, manifest', dfiles, mfiles) =
             createHDL hdlState' modNameT seen2 netlist domainConfs (Just topComponent) (topNmT, Right manifest)
      mapM_ (writeHDL dir) hdlDocs
      copyDataFiles (opt_importPaths opts) dir dfiles
      writeMemoryDataFiles dir mfiles

      topTime <- hdlDocs `seq` Clock.getCurrentTime
      let edamFiles' = HashMap.insert (varUniq topEntity) (asEdamFile topNm <$> fileNames manifest') edamFiles
      return (topTime,manifest',seen2, edamFiles')

  -- Generate EDAM if needed
  if opt_edalize opts
    then let dir      = hdlDir </> maybe "" (const modNameS) annM
             topFiles = fileNames manifest'
             incFiles = concatMap (\u -> HashMap.lookupDefault [] u edamFiles') $ HashMap.lookupDefault [] (varUniq topEntity) deps
             edamInfo = createEDAM topNm incFiles topFiles
          in writeHDL dir ("edam.py", pprEdam edamInfo)

    else pure ()

  benchTime <- case benchM of
    Just tb | not useCacheBench -> do
      putStrLn $ "Clash: Compiling " ++ Data.Text.unpack (nameOcc (varName tb))

      let modName' = Id.toText (lookupVarEnv' compNames tb)
          hdlState2 = setModName modName' hdlState'

      -- 1. Normalise testBench
      let transformedBindings = normalizeEntity reprs bindingsMap primMap tcm tupTcm
                                  typeTrans eval topEntityNames opts supplyTB tb
      normTime <- transformedBindings `deepseq` Clock.getCurrentTime
      let prepNormDiff = reportTimeDiff normTime topTime
      putStrLn $ "Clash: Testbench normalization took " ++ prepNormDiff

      -- 2. Generate netlist for topEntity

      -- See [Note] Create HDL dir before netlist generation
      let dir = hdlDir </> maybe "" t_name annM </> Data.Text.unpack modName'
      prepareDir (opt_cleanhdl opts) (extension hdlState2) dir
      -- Now start the netlist generation
      (_topName, netlist, seen3) <-
        genNetlist True opts reprs transformedBindings topEntityMap compNames primMap
                   tcm typeTrans iw ite (SomeBackend hdlState') seen2 hdlDir prefixM tb

      netlistTime <- netlist `deepseq` Clock.getCurrentTime
      let normNetDiff = reportTimeDiff netlistTime normTime
      putStrLn $ "Clash: Testbench netlist generation took " ++ normNetDiff

      -- 3. Write HDL
      let (hdlDocs,_,dfiles,mfiles) =
            createHDL hdlState2 modName' seen3 netlist domainConfs Nothing
                           (topNmT, Left manifest')
      writeHDL (hdlDir </> maybe "" t_name annM) (head hdlDocs)
      mapM_ (writeHDL dir) (tail hdlDocs)
      copyDataFiles (opt_importPaths opts) dir dfiles
      writeMemoryDataFiles dir mfiles

      hdlDocs `seq` Clock.getCurrentTime

    Just tb -> do
      let tb' = Data.Text.unpack (nameOcc (varName tb))
      putStrLn ("Clash: Compiling: " ++ tb')
      putStrLn ("Clash: Using cached result for: " ++ tb')
      return topTime

    Nothing -> return topTime

  go benchTime seen2 edamFiles' deps topEntities'

-- | Interpret a specific function from a specific module. This action tries
-- two things:
--
--   1. Interpret without explicitly loading the module. This will succeed if
--      the module was already loaded through a package database (set using
--      'interpreterArgs').
--
--   2. If (1) fails, it does try to load it explicitly. If this also fails,
--      an error is returned.
--
loadImportAndInterpret
  :: (MonadIO m, MonadMask m)
  => [String]
  -- ^ Extra search path (usually passed as -i)
  -> [String]
  -- ^ Interpreter args
  -> String
  -- ^ The folder in which the GHC bootstrap libraries (base, containers, etc.)
  -- can be found
  -> Hint.ModuleName
  -- ^ Module function lives in
  -> String
  -- ^ Function name
  -> String
  -- ^ Type name ('BlackBoxFunction' or 'TemplateFunction')
  -> m (Either Hint.InterpreterError a)
loadImportAndInterpret iPaths0 interpreterArgs topDir qualMod funcName typ = do
  Hint.liftIO $ Monad.when debugIsOn $
    putStr "Hint: Interpreting " >> putStrLn (qualMod ++ "." ++ funcName)
  -- Try to interpret function *without* loading module explicitly. If this
  -- succeeds, the module was already in the global package database(s).
  bbfE <- Hint.unsafeRunInterpreterWithArgsLibdir interpreterArgs topDir $ do
    iPaths1 <- (++iPaths0) <$> Hint.get Hint.searchPath
    Hint.set [Hint.searchPath Hint.:= iPaths1]
    Hint.setImports [ "Clash.Netlist.Types", "Clash.Netlist.BlackBox.Types", qualMod]
    Hint.unsafeInterpret funcName typ

  case bbfE of
    Left _ -> do
      -- Try to interpret module as a local module, not yet present in the
      -- global package database(s).
      Hint.unsafeRunInterpreterWithArgsLibdir interpreterArgs topDir $ do
        Hint.reset
        iPaths1 <- (iPaths0++) <$> Hint.get Hint.searchPath
        Hint.set [ Hint.searchPath Hint.:= iPaths1
                 , Hint.languageExtensions Hint.:= langExts]
        Hint.loadModules [qualMod]
        Hint.setImports [ "Clash.Netlist.BlackBox.Types", "Clash.Netlist.Types", qualMod]
        Hint.unsafeInterpret funcName typ
    Right _ -> do
      return bbfE
 where
   langExts = map Hint.asExtension $
                map show wantedLanguageExtensions ++
                map ("No" ++ ) (map show unwantedLanguageExtensions)

-- | List of known BlackBoxFunctions used to prevent Hint from firing. This
--  improves Clash startup times.
knownBlackBoxFunctions :: HashMap String BlackBoxFunction
knownBlackBoxFunctions =
  HashMap.fromList $ map (first show) $
    [ ('P.checkBBF, P.checkBBF)
    , ('P.bvToIntegerVHDL, P.bvToIntegerVHDL)
    , ('P.bvToIntegerVerilog, P.bvToIntegerVerilog)
    , ('P.foldBBF, P.foldBBF)
    , ('P.indexIntVerilog, P.indexIntVerilog)
    , ('P.indexToIntegerVerilog, P.indexToIntegerVerilog)
    , ('P.indexToIntegerVHDL, P.indexToIntegerVHDL)
    , ('P.intTF, P.intTF)
    , ('P.iterateBBF, P.iterateBBF)
    , ('P.signedToIntegerVerilog, P.signedToIntegerVerilog)
    , ('P.signedToIntegerVHDL, P.signedToIntegerVHDL)
    , ('P.unsignedToIntegerVerilog, P.unsignedToIntegerVerilog)
    , ('P.unsignedToIntegerVHDL, P.unsignedToIntegerVHDL)
    , ('P.wordTF, P.wordTF)
    ]

-- | List of known TemplateFunctions used to prevent Hint from firing. This
--  improves Clash startup times.
knownTemplateFunctions :: HashMap String TemplateFunction
knownTemplateFunctions =
  HashMap.fromList $ map (first show) $
    [ ('P.altpllQsysTF, P.altpllQsysTF)
    , ('P.alteraPllQsysTF, P.alteraPllQsysTF)
    , ('P.alteraPllTF, P.alteraPllTF)
    , ('P.altpllTF, P.altpllTF)
    ]

-- | Compiles blackbox functions and parses blackbox templates.
compilePrimitive
  :: [FilePath]
  -- ^ Import directories (-i flag)
  -> [FilePath]
  -- ^ Package databases
  -> FilePath
  -- ^ The folder in which the GHC bootstrap libraries (base, containers, etc.)
  -- can be found
  -> ResolvedPrimitive
  -- ^ Primitive to compile
  -> IO CompiledPrimitive
compilePrimitive idirs pkgDbs topDir (BlackBoxHaskell bbName wf usedArgs multiRes bbGenName source) = do
  bbFunc <-
    -- TODO: Use cache for hint targets. Right now Hint will fire multiple times
    -- TODO: if multiple functions use the same blackbox haskell function.
    case HashMap.lookup fullName knownBlackBoxFunctions of
      Just f -> pure f
      Nothing -> do
        Monad.when debugIsOn (putStr "Hint: interpreting " >> putStrLn (show fullName))
        let interpreterArgs = concatMap (("-package-db":) . (:[])) pkgDbs
        -- Compile a blackbox template function or fetch it from an already compiled file.
        r <- go interpreterArgs source
        processHintError
          (show bbGenName)
          bbName
          id
          r

  pure (BlackBoxHaskell bbName wf usedArgs multiRes bbGenName (hash source, bbFunc))
 where
    fullName = qualMod ++ "." ++ funcName
    qualMod = intercalate "." modNames
    BlackBoxFunctionName modNames funcName = bbGenName

    -- | Create directory based on base name and directory. Return path
    -- of directory just created.
    createDirectory'
      :: FilePath
      -> FilePath
      -> IO FilePath
    createDirectory' base sub =
      let new = base </> sub in
      Directory.createDirectory new >> return new

    go
      :: [String]
      -> Maybe Text
      -> IO (Either Hint.InterpreterError BlackBoxFunction)
    go args (Just source') = do
      -- Create a temporary directory with user module in it, add it to the
      -- list of import direcotries, and run as if it were a "normal" compiled
      -- module.
      tmpDir0 <- getCanonicalTemporaryDirectory
      withTempDirectory tmpDir0 "clash-prim-compile" $ \tmpDir1 -> do
        modDir <- foldM createDirectory' tmpDir1 (init modNames)
        Text.writeFile (modDir </> (last modNames ++ ".hs")) source'
        loadImportAndInterpret (tmpDir1:idirs) args topDir qualMod funcName "BlackBoxFunction"

    go args Nothing = do
      loadImportAndInterpret idirs args topDir qualMod funcName "BlackBoxFunction"

compilePrimitive idirs pkgDbs topDir
  (BlackBox pNm wf rVoid multiRes tkind () oReg libM imps fPlural incs rM riM templ) = do
  libM'  <- mapM parseTempl libM
  imps'  <- mapM parseTempl imps
  incs'  <- mapM (traverse parseBB) incs
  templ' <- parseBB templ
  rM'    <- traverse parseBB rM
  riM'   <- traverse parseBB riM
  return (BlackBox pNm wf rVoid multiRes tkind () oReg libM' imps' fPlural incs' rM' riM' templ')
 where
  iArgs = concatMap (("-package-db":) . (:[])) pkgDbs

  parseTempl
    :: Applicative m
    => Text
    -> m BlackBoxTemplate
  parseTempl t = case runParse t of
    Failure errInfo
      -> error ("Parsing template for blackbox " ++ Data.Text.unpack pNm ++ " failed:\n"
               ++ show (_errDoc errInfo))
    Success t'
      -> pure t'

  parseBB
    :: ((TemplateFormat,BlackBoxFunctionName), Maybe Text)
    -> IO BlackBox
  parseBB ((TTemplate,_),Just t)     = BBTemplate <$> parseTempl t
  parseBB ((TTemplate,_),Nothing)    =
    error ("No template specified for blackbox: " ++ show pNm)
  parseBB ((THaskell,bbGenName),Just source) = do
    let BlackBoxFunctionName modNames funcName = bbGenName
        qualMod = intercalate "." modNames
    tmpDir <- getCanonicalTemporaryDirectory
    r <- withTempDirectory tmpDir "clash-prim-compile" $ \tmpDir' -> do
      let modDir = foldl (</>) tmpDir' (init modNames)
      Directory.createDirectoryIfMissing True modDir
      Text.writeFile (modDir </> last modNames <.>  "hs") source
      loadImportAndInterpret (tmpDir':idirs) iArgs topDir qualMod funcName "TemplateFunction"
    let hsh = hash (qualMod, source)
    processHintError (show bbGenName) pNm (BBFunction (Data.Text.unpack pNm) hsh) r
  parseBB ((THaskell,bbGenName),Nothing) = do
    let BlackBoxFunctionName modNames funcName = bbGenName
        qualMod = intercalate "." modNames
        hsh     = hash qualMod
        fullName = qualMod ++ "." ++ funcName
    tf <-
      case HashMap.lookup fullName knownTemplateFunctions of
        Just f -> pure f
        Nothing -> do
          r <- loadImportAndInterpret idirs iArgs topDir qualMod funcName "TemplateFunction"
          processHintError (show bbGenName) pNm id r
    pure (BBFunction (Data.Text.unpack pNm) hsh tf)

compilePrimitive _ _ _ (Primitive pNm wf typ) =
  return (Primitive pNm wf typ)
{-# SCC compilePrimitive #-}

processHintError
  :: Monad m
  => String
  -> Data.Text.Text
  -> (t -> r)
  -> Either Hint.InterpreterError t
  -> m r
processHintError fun bb go r = case r of
  Left (Hint.GhcException err) ->
    error' "GHC Exception" err
  Left (Hint.NotAllowed err) ->
    error' "NotAllowed error" err
  Left (Hint.UnknownError err) ->
    error' "an unknown error" err
  Left (Hint.WontCompile ghcErrs) ->
    error' "compilation errors" (intercalate "\n\n" $ map Hint.errMsg ghcErrs)
  Right f ->
    return $ go f
 where
  error' errType report =
    error $ unwords [ "Encountered", errType, "while compiling blackbox template"
                    , "function", show fun, "for function", show bb ++ "."
                    , "Compilation reported: \n\n" ++ report ]

-- | Pretty print Components to HDL Documents
createHDL
  :: Backend backend
  => backend
  -- ^ Backend
  -> IdentifierText
  -- ^ Module hierarchy root
  -> Id.IdentifierSet
  -- ^ Component names
  -> VarEnv ([Bool],SrcSpan,Id.IdentifierSet,Component)
  -- ^ List of components
  -> HashMap Data.Text.Text VDomainConfiguration
  -- ^ Known domains to configurations
  -> Maybe Component
  -- ^ Top component
  -> (IdentifierText, Either Manifest Manifest)
  -- ^ Name of the manifest file
  -- + Either:
  --   * Left manifest:  Only write/update the hashes of the @manifest@
  --   * Right manifest: Update all fields of the @manifest@
  -> ([(String,Doc)],Manifest,[(String,FilePath)],[(String,String)])
  -- ^ The pretty-printed HDL documents
  -- + The update manifest file
  -- + The data files that need to be copied
createHDL backend modName seen components domainConfs mtop (topName,manifestE) = flip evalState backend $ getMon $ do
  let componentsL = eltsVarEnv components
  (hdlNmDocs,incs) <-
    unzip <$> mapM (\(_wereVoids,sp,ids,comp) ->
                      genHDL modName sp (Id.union seen ids) comp)
              componentsL
  hwtys <- HashSet.toList <$> extractTypes <$> Mon get
  typesPkg <- mkTyPackage modName hwtys
  dataFiles <- Mon getDataFiles
  memFiles  <- Mon getMemoryDataFiles
  let hdl   = map (first (<.> Clash.Backend.extension backend)) (typesPkg ++ hdlNmDocs)
      qincs = concat incs
      topFiles = hdl ++ qincs

  let sdc = case mtop of
              Just top ->
                let topClks = findClocks top
                    sdcInfo = fmap findDomainConfig <$> topClks
                    sdcFile = Data.Text.unpack topName <.> "sdc"
                    sdcDoc  = (sdcFile, pprSDC (SdcInfo sdcInfo))
                 in if null sdcInfo then Nothing else Just sdcDoc

              Nothing -> Nothing

  manifest <- either return (\m -> do
      let top = fromMaybe (error "Top entity needed to update manifest") mtop
      let topInNames = map fst (inputs top)
      topInTypes  <- mapM (fmap (Text.toStrict . renderOneLine) .
                           hdlType (External topName) . snd) (inputs top)
      let topOutNames = map (fst . (\(_,x,_) -> x)) (outputs top)
      topOutTypes <- mapM (fmap (Text.toStrict . renderOneLine) .
                           hdlType (External topName) . snd . (\(_,x,_) -> x)) (outputs top)
      let compNames = map (componentName . view _4) componentsL
      return (m { portInNames    = map Id.toText topInNames
                , portInTypes    = topInTypes
                , portOutNames   = map Id.toText topOutNames
                , portOutTypes   = topOutTypes
                , componentNames = map Id.toText compNames
                , fileNames      = maybe (fmap fst topFiles) (\x -> fst x : fmap fst topFiles) sdc
                })
    ) manifestE
  let manDoc = ( Data.Text.unpack topName <.> "manifest"
               , pretty (Text.pack (show manifest)))

  let topDocs = maybe (manDoc:topFiles) (:manDoc:topFiles) sdc
  return (topDocs, manifest, dataFiles, memFiles)
 where
  findDomainConfig dom =
    HashMap.lookupDefault
      (error $ $(curLoc) ++ "Unknown synthesis domain: " ++ show dom)
      dom
      domainConfs

-- | Create an Edalize metadata file for using Edalize to build the project.
--
createEDAM :: Id.Identifier -> [EdamFile] -> [FilePath] -> Edam
createEDAM topName incFiles files = Edam
  { edamProjectName = Id.toText topName
  , edamTopEntity   = Id.toText topName
  , edamFiles       = fmap (asEdamFile topName) files <> fmap asIncFile incFiles
  , edamToolOptions = def
  }
 where
  asIncFile f =
    f { efName = ".." </> Data.Text.unpack (efLogicalName f) </> efName f }

asEdamFile :: Id.Identifier -> FilePath -> EdamFile
asEdamFile topName path =
  EdamFile path edamFileType (Id.toText topName)
 where
  edamFileType =
    case FilePath.takeExtension path of
      ".vhdl" -> VhdlSource
      ".v" -> VerilogSource
      ".sv" -> SystemVerilogSource
      ".tcl" -> TclSource
      ".qsys" -> QSYS
      ".sdc" -> SDC
      _ -> Clash.Edalize.Edam.Unknown

-- | Prepares the directory for writing HDL files. This means creating the
--   dir if it does not exist and removing all existing .hdl files from it.
prepareDir :: Bool -- ^ Remove existing HDL files
           -> String -- ^ File extension of the HDL files.
           -> String
           -> IO ()
prepareDir cleanhdl ext dir = do
  -- Create the dir if needed
  Directory.createDirectoryIfMissing True dir
  -- Clean the directory when needed
  when cleanhdl $ do
    -- Find all HDL files in the directory
    files <- Directory.getDirectoryContents dir
    let to_remove = filter ((==ext) . FilePath.takeExtension) files
    -- Prepend the dirname to the filenames
    let abs_to_remove = map (FilePath.combine dir) to_remove
    -- Remove the files
    mapM_ Directory.removeFile abs_to_remove

-- | Writes a HDL file to the given directory
writeHDL :: FilePath -> (String, Doc) -> IO ()
writeHDL dir (cname, hdl) = do
  let rendered = renderLazy (layoutPretty (LayoutOptions (AvailablePerLine 120 0.4)) hdl)
      -- remove blank lines to keep things clean
      clean = Text.unlines
            . map (\t -> if Text.all (==' ') t then Text.empty else t)
            . Text.lines
  bracket (IO.openFile (dir </> cname) IO.WriteMode) IO.hClose $ \h -> do
    Text.hPutStr h (clean rendered)
    Text.hPutStr h (Text.pack "\n")

-- | Copy given files
writeMemoryDataFiles
    :: FilePath
    -- ^ Directory to copy  files to
    -> [(String, String)]
    -- ^ (filename, content)
    -> IO ()
writeMemoryDataFiles dir files =
    mapM_
      (uncurry writeFile)
      [(dir </> fname, content) | (fname, content) <- files]

copyDataFiles
    :: [FilePath]
    -> FilePath
    -> [(String,FilePath)]
    -> IO ()
copyDataFiles idirs dir = mapM_ (copyFile' idirs)
  where
    copyFile' dirs (nm,old) = do
      oldExists <- Directory.doesFileExist old
      if oldExists
        then Directory.copyFile old new
        else goImports dirs
      where
        new = dir FilePath.</> nm

        goImports [] = do
          oldExists <- Directory.doesFileExist old
          if oldExists
            then Directory.copyFile old new
            else unless (null old) (putStrLn ("WARNING: file " ++ show old ++ " does not exist"))
        goImports (d:ds) = do
          let old2 = d FilePath.</> old
          old2Exists <- Directory.doesFileExist old2
          if old2Exists
            then Directory.copyFile old2 new
            else goImports ds

-- | Get all the terms corresponding to a call graph
callGraphBindings
  :: BindingMap
  -- ^ All bindings
  -> Id
  -- ^ Root of the call graph
  -> [Term]
callGraphBindings bindingsMap tm =
  map (bindingTerm . (bindingsMap `lookupUniqMap'`)) (keysUniqMap cg)
  where
    cg = callGraph bindingsMap tm

-- | Normalize a complete hierarchy
normalizeEntity
  :: CustomReprs
  -> BindingMap
  -- ^ All bindings
  -> CompiledPrimMap
  -- ^ BlackBox HDL templates
  -> TyConMap
  -- ^ TyCon cache
  -> IntMap TyConName
  -- ^ Tuple TyCon cache
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcoded 'Type' -> 'HWType' translator
  -> Evaluator
  -- ^ Hardcoded evaluator for partial evaluation
  -> [Id]
  -- ^ TopEntities
  -> ClashOpts
  -- ^ Debug information level for the normalization process
  -> Supply.Supply
  -- ^ Unique supply
  -> Id
  -- ^ root of the hierarchy
  -> BindingMap
normalizeEntity reprs bindingsMap primMap tcm tupTcm typeTrans eval topEntities
  opts supply tm = transformedBindings
  where
    doNorm = do norm <- normalize [tm]
                let normChecked = checkNonRecursive norm
                cleaned <- cleanupGraph tm normChecked
                return cleaned
    transformedBindings = runNormalization opts supply bindingsMap
                            typeTrans reprs tcm tupTcm eval primMap emptyVarEnv
                            topEntities doNorm

-- | topologically sort the top entities
sortTop
  :: BindingMap
  -> [TopEntityT]
  -> ([TopEntityT], HashMap Unique [Unique])
sortTop bindingsMap topEntities =
  let (nodes,edges) = unzip (map go topEntities)
      edges' = concat edges
  in  case reverseTopSort nodes edges' of
        Left msg   -> error msg
        Right tops -> (tops, mapFrom edges')
 where
  go t@(TopEntityT topE _ tbM) =
    let topRefs = goRefs topE topE
        tbRefs  = maybe [] (goRefs topE) tbM
    in  ((varUniq topE,t)
         ,map ((\top -> (varUniq topE, varUniq (topId top)))) (tbRefs ++ topRefs))

  goRefs top i_ =
    let cg = callGraph bindingsMap i_
    in
      filter
        (\t -> topId t /= top && topId t /= i_ && topId t `elemVarEnv` cg)
        topEntities

  mapFrom = HashMap.fromListWith mappend . fmap (fmap pure)
