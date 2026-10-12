{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                          2017, QBayLogic, Google Inc.,
                     2021-2022, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.GHC.GenerateBindings
  (generateBindings)
where

import           Control.Arrow           ((***))
import           Control.DeepSeq         (NFData, deepseq)
import           Control.Lens            ((%~),(&),(.~),(^.))
import           Control.Monad           (forM, forM_, unless, when)
import qualified Control.Monad.State     as State
import qualified Control.Monad.RWS.Strict as RWS
import           Data.Coerce             (coerce)
import           Data.Either             (partitionEithers, lefts ,rights)
import           Data.Foldable           (traverse_)
import           Data.IntMap.Strict      (IntMap)
import qualified Data.IntMap.Strict      as IMS
import qualified Data.IntSet             as IS
import qualified Data.HashMap.Strict     as HashMap
#if !MIN_VERSION_base(4,20,0)
import           Data.List               (foldl')
#endif
import           Data.List               (isPrefixOf)
import           Data.List.Split         (chunksOf)
import           Data.Maybe              (listToMaybe)
import qualified Data.Text               as Text
import qualified Data.Time.Clock         as Clock
import           System.Environment      (lookupEnv)
import           GHC.Conc                (numCapabilities, par, pseq)

import qualified GHC                     as GHC (Ghc)
import qualified GHC.Types.SourceText    as GHC
import qualified GHC.Utils.Panic         as GHC
import qualified GHC.Types.Basic         as GHC
import qualified GHC.Core                as GHC
import qualified GHC.Types.Demand        as GHC
import qualified GHC.Driver.Session      as GHC
import qualified GHC.Types.Id.Info       as GHC
import qualified GHC.Types.Name          as GHC hiding (varName)
import qualified GHC.Core.FamInstEnv     as GHC
import qualified GHC.Core.TyCon          as GHC
import qualified GHC.Core.Type           as GHC
import qualified GHC.Builtin.Types       as GHC
import qualified GHC.Settings.Constants  as GHC
import qualified GHC.Types.Var           as GHC
import qualified GHC.Types.Var.Set       as GHC (emptyVarSet, mkVarSet)
import qualified GHC.Types.SrcLoc        as GHC
import           GHC.BasicTypes.Extra (isOpaque)

import           Clash.Annotations.BitRepresentation.Internal (buildCustomReprs)
import           Clash.Annotations.Primitive (HDL, extractPrim)

import           Clash.Core.Name         (mkUnsafeInternalName, nameOcc)
import           Clash.Core.Subst        (extendGblSubstList, mkSubst, substTm)
import           Clash.Core.Term         (Term (..), mkLams, mkTyLams)
import           Clash.Core.Type         (Type (..), TypeView (..), mkFunTy, splitFunForallTy, tyView)
import           Clash.Core.DataCon      (dcExtTyVars)
import           Clash.Core.TyCon        (TyConMap, TyConName, isNewTypeTc, tyConDataCons)
import           Clash.Core.TysPrim      (tysPrimMap)
import           Clash.Core.Util         (dataConInstArgTys, mkInternalVar, mkSelectorCase)
import           Clash.Core.Var          (Var (..), Id, IdScope (..), setIdScope)
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, emptyInScopeSet, extendInScopeSet, mkInScopeSet
  ,mkVarEnv, unionVarEnv, elemVarSet, mkVarSet)
import           Clash.Core.Scoped.Convert (fromClashTermClosed)
import           Clash.Core.Scoped.NameMap (globalEnvFromList)
import           Clash.Core.Scoped.Syntax (S (VoidS), mkGlobalId)
import qualified Clash.Core.Scoped.Syntax as S (Term)
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Driver            (compilePrimitives)
import           Clash.Driver.Bool       (toGhcOverridingBool)
import           Clash.Driver.Types      (Binding(..), IsPrim(..), ClashEnv(..), ClashDesign(..), ClashOpts(..))
import           Clash.Driver.Warning    (warnAbout, warnAboutM)
import           Clash.GHC.GHC2Core
  (C2C, GHC2CoreState, GHC2CoreEnv (..), tyConMap, coreToId, coreToName, coreToTerm,
   makeAllTyCons, pendingWarnings, qualifiedNameString, emptyGHC2CoreState,
   srcSpan)
import           Clash.GHC.LoadModules   (ghcLibDir, loadModules)
import           Clash.Core.Scoped.Builder
  (TmB, caseB, clashTyB, dataAltB, lamB, letrecB, runClosedTerm, tyLamClashB, varB)
import           Clash.GHC.Scoped.Check  (Outcome (..), compareTerms, primAtTypeShape)
import           Clash.GHC.Scoped.GHC2Core (coreToTermB, coreToTypeB, varKey)
import           Clash.Netlist.BlackBox.Util (getUsedArguments)
import           Clash.Netlist.Types     (TopEntityT(..))
import           Clash.Primitives.Types
  (Primitive (..), CompiledPrimMap)
import           Clash.Primitives.Util   (generatePrimMap)
import           Clash.Unique            (Unique)
import           Clash.Util              (reportTimeDiff)
import qualified Clash.Util.Interpolate as I
import           Clash.Warning           (ClashWarning(WarnPrimitiveDefinition))

indexMaybe :: [a] -> Int -> Maybe a
indexMaybe [] _     = Nothing
indexMaybe (x:_)  0 = Just x
indexMaybe (_:xs) n = indexMaybe xs (n-1)

generateBindings
  :: ClashOpts
  -> GHC.Ghc ()
  -- ^ Allows us to have some initial action, such as sharing a linker state
  -- See https://github.com/clash-lang/clash-compiler/issues/1686 and
  -- https://mail.haskell.org/pipermail/ghc-devs/2021-March/019605.html
  -> [FilePath]
  -- ^ primitives (blackbox) directories
  -> [FilePath]
  -- ^ import directories (-i flag)
  -> [FilePath]
  -- ^ Package database
  -> HDL
  -- ^ HDL target
  -> String
  -> Maybe GHC.DynFlags
  -> IO (ClashEnv, ClashDesign)
generateBindings opts startAction primDirs importDirs dbs hdl modName dflagsM = do
  (  bindings
   , clsOps
   , unlocatable
   , fiEnvs
   , topEntities
   , partitionEithers -> (unresolvedPrims, pFP)
   , customBitRepresentations
   , primGuards
   , domainConfs ) <- loadModules startAction (toGhcOverridingBool (opt_color opts)) hdl modName dflagsM importDirs
  startTime <- Clock.getCurrentTime
  primMapR <- generatePrimMap unresolvedPrims primGuards (concat [pFP, primDirs, importDirs])
  tdir <- maybe ghcLibDir (pure . GHC.topDir) dflagsM
  primMapC <- compilePrimitives importDirs dbs tdir primMapR
  let c2cEnv = GHC2CoreEnv GHC.noSrcSpan fiEnvs opts
      ((bindingsMap,clsVMap),tcMap,_) =
        RWS.runRWS (mkBindings (processBind primMapC unlocatable) bindings clsOps)
                   c2cEnv
                   emptyGHC2CoreState
      (tcMap',tupTcCache)           = mkTupTyCons opts tcMap
      tcCache                       = makeAllTyCons opts tcMap' fiEnvs
      allTcCache                    = tysPrimMap <> tcCache
                                    -- Recursion info is always False for class
                                    -- selectors, no need to check free vars.
      clsMap =
        fmap (\(v,i) ->
               (Binding v GHC.noSrcSpan (GHC.Inline GHC.NoSourceText) IsFun
                  (mkClassSelector allTcCache (varType v) i) False))
             clsVMap
      -- Build the bodies lazily: most bindings are never looked at
      allBindings                   = fmap (fmap runClosedTerm) bindingsMap `unionVarEnv` clsMap
      topEntities'                  =
        (\m -> fst (RWS.evalRWS m (GHC2CoreEnv GHC.noSrcSpan fiEnvs opts) tcMap')) $
          mapM (\(topEnt,annM,isTb) -> do
            topEnt' <- coreToName GHC.varName GHC.varUnique qualifiedNameString topEnt
            return (topEnt', annM, isTb)) topEntities
      topEntities'' =
        map (\(topEnt, annM, isTb) ->
                case UniqMap.lookup topEnt allBindings of
                  Just b -> TopEntityT (bindingId b) annM isTb
                  Nothing -> GHC.pgmError [I.i|
                    No top entity called '#{topEnt}' found. Make sure you are
                    compiling with the '-fexpose-all-unfoldings' flag.
                  |]
            ) topEntities'
  -- Parsing / compiling primitives:
  prepTime  <- startTime `deepseq` primMapC `seq` Clock.getCurrentTime
  let prepStartDiff = reportTimeDiff prepTime startTime
  putStrLn $ "Clash: Parsing and compiling primitives took " ++ prepStartDiff

  let allBindings' = setNoInlineTopEntities allBindings topEntities''

  -- 'C2C' is pure, so the warnings it ran into were collected rather than
  -- printed. Report them now, in the order the binders were converted.
  traverse_ (warnAbout opts) (tcMap ^. pendingWarnings)

  -- Compare with the conversion through Clash Core, see 'checkGHC2Core'
  checkLog <- lookupEnv "CLASH_DEBUG_CHECK_GHC2CORE"
  forM_ checkLog $ \logFile ->
    checkGHC2Core logFile modName c2cEnv primMapC bindings clsOps unlocatable
      allTcCache tcMap allBindings

  return
    ( ClashEnv
        { envOpts = opts
        , envTyConMap = allTcCache
        , envTupleTyCons = tupTcCache
        , envPrimitives = primMapC
        , envCustomReprs = buildCustomReprs customBitRepresentations
        , envDomains = domainConfs
        }
    , ClashDesign
        { designEntities = topEntities''
        , designBindings =
            globalEnvFromList
              [ (mkGlobalId (bindingId b), b) | b <- UniqMap.elems allBindings' ]
        }
    )

setNoInlineTopEntities
  :: VarEnv (Binding a)
  -> [TopEntityT]
  -> VarEnv (Binding a)
setNoInlineTopEntities bm tes =
  fmap go bm
 where
  ids = mkVarSet (fmap topId tes)

  go b@Binding{bindingId}
    | bindingId `elemVarSet` ids
    = b { bindingSpec = GHC.Opaque GHC.NoSourceText }
    | otherwise = b

-- TODO This function should be changed to provide the information that
-- Clash.Core.Termination.mkRecInfo provides. To achieve this, it should also
-- be changed to no longer flatten recursive groups (see the documentation for
-- mkRecInfo for an explanation of these).
--
-- | Convert the bindings with the given conversion of a binder group, see
-- 'processBind', and the class operations.
mkBindings
  :: NFData a
  => (GHC.CoreBind -> C2C [(Id, Binding a)])
  -> [GHC.CoreBind]
  -- Binders
  -> [(GHC.CoreBndr,Int)]
  -- Class operations
  -> C2C ( VarEnv (Binding a)
         , VarEnv (Id,Int)
         )
mkBindings convBind bindings clsOps = do
  -- Converting each binder is independent: 'GHC2CoreState' only accumulates a
  -- 'TyCon' map and a name cache, both of which are pure (deterministic per
  -- key) memo tables. We therefore convert every binder from a fresh state in
  -- parallel and merge the resulting 'TyCon' maps afterwards. See 'parRunC2C'.
  env <- RWS.ask
  let
    bindingsList = parRunC2C env (map convBind bindings)
    clsOpList    = parRunC2C env (map processClsOp clsOps)
    states       = map snd bindingsList ++ map snd clsOpList
  -- Merge the 'TyCon' maps discovered while converting, and collect the
  -- warnings the conversions reported; the name caches are not used after this
  -- point, so they are dropped. 'makeAllTyCons' later recomputes over the
  -- merged map, so a plain union suffices.
  RWS.modify $ \st -> st
    & tyConMap %~ (\tcm0 -> foldl' (\acc s -> acc <> (s ^. tyConMap)) tcm0 states)
    & pendingWarnings %~ (<> foldMap (^. pendingWarnings) states)

  return ( mkVarEnv (concatMap fst bindingsList)
         , mkVarEnv (map fst clsOpList) )

-- | Convert a single (possibly recursive) binder group to bindings of scoped
-- Core. See 'mkBindings' for how these conversions are run in parallel.
--
-- The bindings hold the builders of their bodies, see
-- "Clash.Core.Scoped.Builder": 'mkBindings' runs the conversion of GHC Core in
-- parallel, and 'generateBindings' builds the bodies lazily.
processBind
  :: CompiledPrimMap
  -> [GHC.CoreBndr]
  -> GHC.CoreBind
  -> C2C [(Id, Binding TmB)]
processBind primMap unlocatable = \case
  GHC.NonRec v e -> do
    b <- convBinding GHC.emptyVarSet False v e
    return [(bindingId b, b)]
  GHC.Rec [(v, e)] -> do
    b <- convBinding GHC.emptyVarSet True v e
    return [(bindingId b, b)]
  -- A group of more than one binder refers to its binders as local variables,
  -- see NOTE [bindings in recursive groups]
  GHC.Rec bes -> do
    let locals = GHC.mkVarSet (map fst bes)
    bs <- forM bes (\(v, e) -> convBinding locals True v e)
    tys <- forM bes (coreToTypeB . GHC.varType . fst)
    let lbs = [ (varKey v, varName (bindingId b), ty, bindingTerm b)
              | ((v, _), b, ty) <- zip3 bes bs tys ]
    return
      [ (bindingId b, b { bindingTerm = letrecB lbs (varB (varKey v))
                        , bindingRecursive = False })
      | ((v, _), b) <- zip bes bs ]
 where
  convBinding locals r v e = do
    let sp  = GHC.getSrcSpan v
        inl = GHC.inlinePragmaSpec . GHC.inlinePragInfo $ GHC.idInfo v
    tm <- RWS.local (srcSpan .~ sp) (coreToTermB primMap unlocatable locals e)
    v' <- coreToId v
    nm <- qualifiedNameString (GHC.varName v)
    let pr = if HashMap.member nm primMap then IsPrim else IsFun
    checkPrimitive primMap v
    return (Binding v' sp inl pr tm r)

-- | Convert a single (possibly recursive) binder group to Clash Core bindings,
-- for 'checkGHC2Core'.
processBindClash
  :: CompiledPrimMap
  -> [GHC.CoreBndr]
  -> GHC.CoreBind
  -> C2C [(Id, Binding Term)]
processBindClash primMap unlocatable = \case
  GHC.NonRec v e -> do
    let sp = GHC.getSrcSpan v
        inl = GHC.inlinePragmaSpec . GHC.inlinePragInfo $ GHC.idInfo v
    tm <- RWS.local (srcSpan .~ sp) (coreToTerm primMap unlocatable e)
    v' <- coreToId v
    nm <- qualifiedNameString (GHC.varName v)
    let pr = if HashMap.member nm primMap then IsPrim else IsFun
    checkPrimitive primMap v
    return [(v', (Binding v' sp inl pr tm False))]
  GHC.Rec bs -> do
    tms <- forM bs $ \(v,e) -> do
      let sp  = GHC.getSrcSpan v
          inl = GHC.inlinePragmaSpec . GHC.inlinePragInfo $ GHC.idInfo v
      tm <- RWS.local (srcSpan .~ sp) (coreToTerm primMap unlocatable e)
      v' <- coreToId v
      nm <- qualifiedNameString (GHC.varName v)
      let pr = if HashMap.member nm primMap then IsPrim else IsFun
      checkPrimitive primMap v
      return (Binding v' sp inl pr tm True)
    case tms of
      [Binding v sp inl pr tm r] -> return [(v, Binding v sp inl pr tm r)]

      -- Rewrite the bindings to avoid triggering the recursion check.
      -- See NOTE [bindings in recursive groups]
      _ -> let vsL   = map (setIdScope LocalId . bindingId) tms
               vsV   = map Var vsL
               subst = extendGblSubstList (mkSubst emptyInScopeSet) (zip vsL vsV)
               lbs   = zipWith (\b vL -> (vL,substTm "mkBindings" subst (bindingTerm b))) tms vsL
               tms1  = zipWith (\b (i, _) -> (bindingId b, b { bindingTerm = Letrec lbs (Var i), bindingRecursive = False })) tms lbs
           in  return tms1

-- | Convert a single class operation. See 'processBind'.
processClsOp :: (GHC.CoreBndr, Int) -> C2C (Id, (Id, Int))
processClsOp (v,i) = do
  v' <- coreToId v
  return (v', (v',i))

-- | Compare the bindings that "Clash.Core.Scoped.GHC2Core" gives with the
-- conversion of the bindings of 'Clash.GHC.GHC2Core.coreToTerm' to scoped
-- Core, see "Clash.GHC.Scoped.Check". Also compares the maps of type
-- constructors that the two find, and the warnings they report.
--
-- Runs when the environment variable @CLASH_DEBUG_CHECK_GHC2CORE@ is set, to
-- a file to which it appends a summary. Errors if a term is not
-- alpha-equivalent. Temporary, see "Clash.GHC.Scoped.Check".
checkGHC2Core
  :: FilePath
  -> String
  -> GHC2CoreEnv
  -> CompiledPrimMap
  -> [GHC.CoreBind]
  -> [(GHC.CoreBndr,Int)]
  -> [GHC.CoreBndr]
  -> TyConMap
  -> GHC2CoreState
  -> VarEnv (Binding (S.Term 'VoidS))
  -> IO ()
checkGHC2Core logFile modName env primMap bindings clsOps unlocatable tcm st new = do
  let
    ((bindingsMap, clsVMap), stClash, _) =
      RWS.runRWS (mkBindings (processBindClash primMap unlocatable) bindings clsOps)
                 env emptyGHC2CoreState
    inScope0 = mkInScopeSet (
                    (fmap (coerce . bindingId) bindingsMap) <>
                    (fmap (coerce . bindingId) clsMap))
    clsMap =
      fmap (\(v,i) ->
             (Binding v GHC.noSrcSpan (GHC.Inline GHC.NoSourceText) IsFun
                (mkClassSelectorClash inScope0 tcm (varType v) i) False))
           clsVMap
    old = bindingsMap `unionVarEnv` clsMap

    outcome b1 b2
      | bindingId b1 /= bindingId b2
        || bindingLoc b1 /= bindingLoc b2
        || bindingIsPrim b1 /= bindingIsPrim b2
        || bindingRecursive b1 /= bindingRecursive b2
        || show (bindingSpec b1) /= show (bindingSpec b2)
      = Different "binding metadata"
      | otherwise
      = compareTerms (fromClashTermClosed (primAtTypeShape (bindingTerm b1))) (bindingTerm b2)

    outcomes =
      [ (nameOcc (varName (bindingId b1)), o)
      | b1 <- UniqMap.elems old
      , let o = maybe (Different "missing") (outcome b1) (UniqMap.lookup (bindingId b1) new)
      ]
    missing = length (UniqMap.keys new) - length (UniqMap.keys old)
    identical = length [ () | (_, Identical) <- outcomes ]
    alpha = [ (nm, d) | (nm, AlphaEquivalent d) <- outcomes ]
    different = [ (nm, d) | (nm, Different d) <- outcomes ]
    tcsClash = UniqMap.keys (stClash ^. tyConMap)
    tcs = UniqMap.keys (st ^. tyConMap)
    tyConsSame = IS.fromList (map fromIntegral tcsClash) == IS.fromList (map fromIntegral tcs)
    warnings = (length (stClash ^. pendingWarnings), length (st ^. pendingWarnings))
    ok = null different && tyConsSame && missing == 0 && uncurry (==) warnings
    detail kind (nm, d) = kind <> " " <> Text.unpack nm <> ": " <> d
    report = unlines $
      [ modName <> ": " <> show (length outcomes) <> " bindings, "
          <> show identical <> " identical, "
          <> show (length alpha) <> " alpha-equivalent, "
          <> show (length different) <> " different, "
          <> show missing <> " extra; type constructors "
          <> (if tyConsSame then "equal" else "DIFFERENT") <> "; warnings "
          <> show warnings ]
      <> map (detail "  ALPHA") (take 5 alpha)
      <> map (detail "  DIFFERENT") (take 20 different)
  appendFile logFile report
  unless ok (error ("checkGHC2Core: " <> report))

-- | Run a list of independent 'C2C' computations, each starting from an empty
-- 'GHC2CoreState', forcing their results to normal form in parallel across the
-- available capabilities. Returns each computation's result paired with the
-- state it produced (so the caller can merge the accumulated 'TyCon' maps).
--
-- This only runs in parallel when the RTS has more than one capability, i.e.,
-- when the executable is run with @+RTS -N@ or built with @-with-rtsopts=-N@.
-- With a single capability it degrades to sequential evaluation.
parRunC2C :: NFData a => GHC2CoreEnv -> [C2C a] -> [(a, GHC2CoreState)]
parRunC2C env ms = parListChunk chunkSize forceResult (map runC2C ms)
 where
  runC2C m = case RWS.runRWS m env emptyGHC2CoreState of
    (a, s, _w) -> (a, s)
  -- Force the converted result to normal form (this is the expensive
  -- 'coreToTerm' work we want to parallelize). The state is left to be forced
  -- lazily when its 'TyCon' map is merged; its entries are cheap GHC 'TyCon'
  -- references (the heavy 'makeAllTyCons' conversion happens later).
  forceResult p@(a, _s) = a `deepseq` p
  -- Keep chunks small so the work stays balanced even when a few binders are
  -- far larger than the rest; ~16 measured as a good size on large downstream
  -- designs. For small designs we make chunks finer still, scaling with the
  -- number of capabilities, but never below 1.
  chunkSize = min 16 (max 1 (length ms `div` (numCapabilities * 4)))

-- | Evaluate a list in parallel, in chunks, using only @base@ (@par@/@pseq@).
-- Equivalent in spirit to @Control.Parallel.Strategies@' @parListChunk n@ with
-- a caller-supplied forcing function, but avoids adding a dependency.
parListChunk :: Int -> (a -> a) -> [a] -> [a]
parListChunk n forceElem = concat . go . chunksOf n
 where
  forceChunk c = foldr (\x xs -> forceElem x `pseq` xs) () c `pseq` c
  go []     = []
  go (c:cs) =
    let c'  = forceChunk c
        cs' = go cs
    in  c' `par` (cs' `pseq` (c' : cs'))

{-
NOTE [bindings in recursive groups]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After normalization, Clash currently checks that all normalized entities are
not recursive and errors if any are. However, this check only applies to
recursion on global identifiers. Since GHC may present recursive groups of
global binders, these are rewritten to only be locally recursive, i.e.

  f[GlobalId] = g[GlobalId] ...
  g[GlobalId] = f[GlobalId] ...

will be rewritten (and inserted into the map of global bindings) as

  f[GlobalId] = let f[LocalId] = g[LocalId] ...
                    g[LocalId] = f[LocalId] ...
                 in f[LocalId]

  g[GlobalId] = let f[LocalId] = g[LocalId] ...
                    g[LocalId] = f[LocalId] ...
                 in g[LocalId]

In scoped Core, the local binders of the group are a 'Rec' telescope of
fresh local variables, which 'Clash.GHC.Scoped.GHC2Core.coreToTermB' refers
to instead of the global binders of the group.

Recursive groups with only a single binding (i.e. global self-recursive
definitions do not need this rewriting. This is because normalization can
trivially spot these self-recursive bindings and where necessary lift them to
global bindings when they appear in a term.
-}

-- | If this CoreBndr is a primitive, check it's Haskell definition
--   for potential problems.
--
-- Warns when a primitive:
--   * isn't marked NOINLINE
--   * produces an error when evaluating its result to WHNF
--   * isn't using all its arguments
checkPrimitive :: CompiledPrimMap -> GHC.CoreBndr -> C2C ()
checkPrimitive primMap v = do
  nm <- qualifiedNameString (GHC.varName v)
  case HashMap.lookup nm primMap >>= extractPrim of
    Just (BlackBox{resultNames, resultInits, template, includes}) -> do
      let
        info = GHC.idInfo v
        inline = GHC.inlinePragmaSpec $ GHC.inlinePragInfo info
        strictness = GHC.dmdSigInfo info
        ty = GHC.varType v
        (argTys,_resTy) = GHC.splitFunTys (snd (GHC.splitForAllTyCoVars ty))
        (dmdArgs,_dmdRes) = GHC.splitDmdSig strictness
        nrOfArgs = length argTys
        warnIf cond msg =
          when cond (warnAboutM WarnPrimitiveDefinition (GHC.getSrcSpan v) msg)
      qName <- Text.unpack <$> qualifiedNameString (GHC.varName v)
      let primStr = "primitive " ++ qName ++ " "
      let usedArgs = concat [ concatMap getUsedArguments resultNames
                            , concatMap getUsedArguments resultInits
                            , getUsedArguments template
                            , concatMap (getUsedArguments . snd) includes
                            ]

      let warnArgs [] = return ()
          warnArgs (x:xs) = do
            warnIf (maybe False GHC.isAbsDmd (indexMaybe dmdArgs x))
              ("The Haskell implementation of " ++ primStr ++ "isn't using argument #" ++
               show x ++ ", but the corresponding primitive blackbox does.\n" ++
               "This can lead to incorrect HDL output because GHC can replace these " ++
               "arguments by an undefined value.")
            warnArgs xs

      unless (qName == "Clash.XException.errorX" || "GHC." `isPrefixOf` qName) $ do
        warnIf (not (isOpaque inline))
          (primStr ++ "isn't marked OPAQUE."
          ++ "\nThis might make Clash ignore this primitive.")
        warnIf (GHC.isDeadEndAppSig strictness nrOfArgs)
          ("The Haskell implementation of " ++ primStr
          ++ "produces a result that always results in an error.\n"
          ++ "This can lead to compile failures because GHC can replace entire "
          ++ "calls to this primitive by an undefined value.")
        warnArgs usedArgs
    _ -> return ()

-- | The body of the selector of the @sel@-th method of a class, given the
-- type of the selector:
--
-- > /\tvs. \dict -> case dict of C:Class .. sel .. -> sel
--
-- or, for a class that is a newtype, @/\tvs. \dict -> dict@.
--
-- The selector is a closed term, built like the conversion of the term of
-- 'Clash.Core.Util.mkSelectorCase' to scoped Core would be, so with the same
-- names.
mkClassSelector
  :: TyConMap
  -> Type
  -> Int
  -> S.Term 'VoidS
mkClassSelector tcm ty sel = runClosedTerm newExpr
  where
    -- TODO: why can't we just use partitionEithers here?
    (tvs,dicts) = (lefts *** rights)
                . span (\l -> case l of {Left _ -> True; _ -> False})
                $ fst (splitFunForallTy ty)
    lams dictTy body =
      foldr tyLamClashB (lamB 0 (mkUnsafeInternalName "dict" 0) (clashTyB dictTy) body) tvs
    newExpr = case listToMaybe dicts of
      Just dictTy@(tyView -> TyConApp tcNm args)
        | Just tc <- UniqMap.lookup tcNm tcm
        , not (isNewTypeTc tc)
        -> lams dictTy (selectorCase tcNm tc args)
      Just (tyView -> FunTy arg res) -> lams (mkFunTy arg res) (varB 0)
      Just dictTy -> lams dictTy (varB 0)
      Nothing -> error "mkClassSelector: expected at least one dictionary argument"

    -- The case-expression of 'Clash.Core.Util.mkSelectorCase', on the variable
    -- with key 0. The fields get keys from 1.
    selectorCase tcNm tc args = case tyConDataCons tc of
      [dc] | null (dcExtTyVars dc)
           , Just fieldTys <- dataConInstArgTys dc args
           , sel < length fieldTys
           -> let fields = [ (i + 1, mkUnsafeInternalName (if i == sel then "sel" else "wild") 0, clashTyB fTy)
                           | (i, fTy) <- zip [0..] fieldTys ]
              in caseB (varB 0) (clashTyB (fieldTys !! sel))
                   [dataAltB dc [] fields (varB (sel + 1))]
      _ -> error ("mkClassSelector: can't select field " ++ show sel ++ " of " ++ show tcNm)

mkClassSelectorClash
  :: InScopeSet
  -> TyConMap
  -> Type
  -> Int
  -> Term
mkClassSelectorClash inScope0 tcm ty sel = newExpr
  where
    -- TODO: why can't we just use partitionEithers here?
    (tvs,dicts) = (lefts *** rights)
                . span (\l -> case l of {Left _ -> True; _ -> False})
                $ fst (splitFunForallTy ty)
    newExpr = case listToMaybe dicts of
      Just dictTy@(tyView -> TyConApp tcNm _)
        | Just tc <- UniqMap.lookup tcNm tcm
        , not (isNewTypeTc tc)
        -> flip State.evalState (0 :: Unique) $ do
              dcId <- mkInternalVar inScope0 "dict" dictTy
              let inScope1 = extendInScopeSet inScope0 dcId
              selE <- mkSelectorCase "mkClassSelector" inScope1 tcm (Var dcId) 1 sel
              return (mkTyLams (mkLams selE [dcId]) tvs)
      Just (tyView -> FunTy arg res) -> flip State.evalState (0 :: Unique) $ do
              dcId <- mkInternalVar inScope0 "dict" (mkFunTy arg res)
              return (mkTyLams (mkLams (Var dcId) [dcId]) tvs)
      Just dictTy -> flip State.evalState (0 :: Unique) $ do
              dcId <- mkInternalVar inScope0 "dict" dictTy
              return (mkTyLams (mkLams (Var dcId) [dcId]) tvs)
      Nothing -> error "mkClassSelector: expected at least one dictionary argument"

mkTupTyCons :: ClashOpts -> GHC2CoreState -> (GHC2CoreState,IntMap TyConName)
mkTupTyCons opts tcMap = (tcMap'',tupTcCache)
  where
    tupTyCons        = GHC.boolTyCon : GHC.promotedTrueDataCon : GHC.promotedFalseDataCon
                     : map (GHC.tupleTyCon GHC.Boxed) [2..GHC.mAX_TUPLE_SIZE]
    (tcNames,tcMap',_) =
      RWS.runRWS (mapM (\tc -> coreToName GHC.tyConName GHC.tyConUnique
                                          qualifiedNameString tc) tupTyCons)
                 (GHC2CoreEnv GHC.noSrcSpan GHC.emptyFamInstEnvs opts)
                 tcMap
    tupTcCache       = IMS.fromList (zip [2..GHC.mAX_TUPLE_SIZE] (drop 3 tcNames))
    tupHM            = UniqMap.fromList (zip tcNames tupTyCons)
    tcMap''          = tcMap' & tyConMap %~ (<> tupHM)
