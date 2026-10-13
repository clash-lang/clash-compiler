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

import           Control.DeepSeq         (NFData, deepseq)
import           Control.Lens            ((%~),(&),(.~),(^.))
import           Control.Monad (forM, unless, when)
import qualified Control.Monad.RWS.Strict as RWS
import           Data.Either (partitionEithers)
import           Data.Foldable           (traverse_)
import           Data.IntMap.Strict      (IntMap)
import qualified Data.IntMap.Strict      as IMS
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.HashSet            as HashSet
#if !MIN_VERSION_base(4,20,0)
import           Data.List               (foldl')
#endif
import           Data.List               (isPrefixOf)
import           Data.List.Split         (chunksOf)
import qualified Data.Text               as Text
import qualified Data.Time.Clock         as Clock
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

import           Clash.Core.Name (mkUnsafeInternalName)
import           Clash.Core.TyCon (TyConMap, TyConName, isNewTypeTc)
import           Clash.Core.Scoped.TysPrim (tysPrimMap)
import           Clash.Data.UniqMap (UniqMap)
import           Clash.Core.Scoped.NameMap (globalEnvFromList)
import           Clash.Core.Scoped.Syntax
  ( GlobalId, S (VoidS), globalIdName, globalIdType )
import           Clash.Core.Scoped.Subst (openTyBinder, withFreshIdBinder)
import qualified Clash.Core.Scoped.Syntax as S
import qualified Clash.Core.Scoped.Type as S (TypeView (..), tyView)
import           Clash.Core.Scoped.TypeEnv (lookupTyCon, mkTypeEnv)
import qualified Clash.Core.Scoped.Util as S (mkSelectorCase)
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Driver            (compilePrimitives)
import           Clash.Driver.Bool       (toGhcOverridingBool)
import           Clash.Driver.Types      (Binding(..), IsPrim(..), ClashEnv(..), ClashDesign(..), ClashOpts(..))
import           Clash.Driver.Warning    (warnAbout, warnAboutM)
import           Clash.GHC.GHC2Core
  (C2C, GHC2CoreState, GHC2CoreEnv (..), tyConMap, coreToGlobalIdB, coreToName,
   makeAllTyCons, pendingWarnings, qualifiedNameString, emptyGHC2CoreState,
   srcSpan)
import           Clash.GHC.LoadModules   (ghcLibDir, loadModules)
import           Clash.Core.Scoped.Builder
  (TmB, letrecB, runClosedTerm, varB)
import           Clash.GHC.Scoped.GHC2Core (coreToTermB, coreToTypeB, varKey)
import           Clash.Netlist.BlackBox.Util (getUsedArguments)
import           Clash.Netlist.Types     (TopEntityT(..))
import           Clash.Primitives.Types
  (Primitive (..), CompiledPrimMap)
import           Clash.Primitives.Util   (generatePrimMap)
import           Clash.Util              (reportTimeDiff)
import qualified Clash.Util.Interpolate as I
import           Clash.Warning           (ClashWarning(WarnPrimitiveDefinition))

indexMaybe :: [a] -> Int -> Maybe a
indexMaybe [] _     = Nothing
indexMaybe (x:_)  0 = Just x
indexMaybe (_:xs) n = indexMaybe xs (n-1)

-- | Render the package database and package visibility flags of a session as
-- command line arguments for the Hint session that loads blackbox functions.
-- This way, blackbox functions can be loaded from exactly the packages the
-- design itself is loaded from. For example, from a hidden package exposed
-- with @-package-id@ (see issue #3105).
--
-- The package environment file (if any) is already merged into these flags by
-- GHC, so we tell Hint not to load one again with @-package-env -@.
hintPackageArgs :: GHC.DynFlags -> [String]
hintPackageArgs dflags = concat
  -- NB: GHC stores these flags in reverse order
  [ ["-package-env", "-"]
  , concatMap dbArgs (reverse (GHC.packageDBFlags dflags))
  , ["-hide-all-packages" | GHC.gopt GHC.Opt_HideAllPackages dflags]
  , concatMap ignoreArgs (reverse (GHC.ignorePackageFlags dflags))
  , concatMap pkgArgs (reverse (GHC.packageFlags dflags))
  ]
 where
  dbArgs = \case
    GHC.PackageDB GHC.GlobalPkgDb -> ["-global-package-db"]
    GHC.PackageDB GHC.UserPkgDb -> ["-user-package-db"]
    GHC.PackageDB (GHC.PkgDbPath p) -> ["-package-db", p]
    GHC.NoUserPackageDB -> ["-no-user-package-db"]
    GHC.NoGlobalPackageDB -> ["-no-global-package-db"]
    GHC.ClearPackageDBs -> ["-clear-package-db"]

  ignoreArgs (GHC.IgnorePackage p) = ["-ignore-package", p]

  pkgArgs = \case
    -- GHC keeps the original flag, e.g. @-package-id foo@ or
    -- @-package foo (Foo as Bar)@, which we split at the first space.
    GHC.ExposePackage flag _ _ -> case break (== ' ') flag of
      (f, _:arg) -> [f, arg]
      (f, []) -> [f]
    GHC.HidePackage p -> ["-hide-package", p]

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
  -> HDL
  -- ^ HDL target
  -> String
  -> Maybe GHC.DynFlags
  -> IO (ClashEnv, ClashDesign)
generateBindings opts startAction primDirs importDirs hdl modName dflagsM = do
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
  let hintArgs = maybe [] hintPackageArgs dflagsM
  primMapC <- compilePrimitives importDirs hintArgs tdir primMapR
  let c2cEnv = GHC2CoreEnv GHC.noSrcSpan fiEnvs opts
      ((bindingsMap,clsOps'),tcMap,_) =
        RWS.runRWS (mkBindings (processBind primMapC unlocatable) processClsOp bindings clsOps)
                   c2cEnv
                   emptyGHC2CoreState
      (tcMap',tupTcCache)           = mkTupTyCons opts tcMap
      tcCache                       = makeAllTyCons opts tcMap' fiEnvs
      allTcCache                    = tysPrimMap <> tcCache
                                    -- Recursion info is always False for class
                                    -- selectors, no need to check free vars.
      clsMap =
        UniqMap.fromList
          [ (globalIdName v, Binding v GHC.noSrcSpan (GHC.Inline GHC.NoSourceText) IsFun
                               (mkClassSelector allTcCache (globalIdType v) i) False)
          | (v, i) <- clsOps' ]
      -- Build the bodies lazily: most bindings are never looked at
      allBindings                   = fmap (fmap runClosedTerm) bindingsMap <> clsMap
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
              [ (bindingId b, b) | b <- UniqMap.elems allBindings' ]
        }
    )

setNoInlineTopEntities
  :: UniqMap (Binding a)
  -> [TopEntityT]
  -> UniqMap (Binding a)
setNoInlineTopEntities bm tes =
  fmap go bm
 where
  ids = HashSet.fromList (fmap topId tes)

  go b@Binding{bindingId}
    | bindingId `HashSet.member` ids
    = b { bindingSpec = GHC.Opaque GHC.NoSourceText }
    | otherwise = b

-- TODO This function should be changed to provide the information that
-- Clash.Core.Termination.mkRecInfo provides. To achieve this, it should also
-- be changed to no longer flatten recursive groups (see the documentation for
-- mkRecInfo for an explanation of these).
--
-- | Convert the bindings with the given conversion of a binder group, see
-- 'processBind', and the class operations with the given conversion, see
-- 'processClsOp'.
mkBindings
  :: (NFData a, NFData c)
  => (GHC.CoreBind -> C2C [Binding a])
  -> ((GHC.CoreBndr,Int) -> C2C c)
  -> [GHC.CoreBind]
  -- Binders
  -> [(GHC.CoreBndr,Int)]
  -- Class operations
  -> C2C ( UniqMap (Binding a)
         , [c]
         )
mkBindings convBind convClsOp bindings clsOps = do
  -- Converting each binder is independent: 'GHC2CoreState' only accumulates a
  -- 'TyCon' map and a name cache, both of which are pure (deterministic per
  -- key) memo tables. We therefore convert every binder from a fresh state in
  -- parallel and merge the resulting 'TyCon' maps afterwards. See 'parRunC2C'.
  env <- RWS.ask
  let
    bindingsList = parRunC2C env (map convBind bindings)
    clsOpList    = parRunC2C env (map convClsOp clsOps)
    states       = map snd bindingsList ++ map snd clsOpList
  -- Merge the 'TyCon' maps discovered while converting, and collect the
  -- warnings the conversions reported; the name caches are not used after this
  -- point, so they are dropped. 'makeAllTyCons' later recomputes over the
  -- merged map, so a plain union suffices.
  RWS.modify $ \st -> st
    & tyConMap %~ (\tcm0 -> foldl' (\acc s -> acc <> (s ^. tyConMap)) tcm0 states)
    & pendingWarnings %~ (<> foldMap (^. pendingWarnings) states)

  return ( UniqMap.fromList [ (globalIdName (bindingId b), b) | b <- concatMap fst bindingsList ]
         , map fst clsOpList )

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
  -> C2C [Binding TmB]
processBind primMap unlocatable = \case
  GHC.NonRec v e -> (:[]) <$> convBinding GHC.emptyVarSet False v e
  GHC.Rec [(v, e)] -> (:[]) <$> convBinding GHC.emptyVarSet True v e
  -- A group of more than one binder refers to its binders as local variables,
  -- see NOTE [bindings in recursive groups]
  GHC.Rec bes -> do
    let locals = GHC.mkVarSet (map fst bes)
    bs <- forM bes (\(v, e) -> convBinding locals True v e)
    tys <- forM bes (coreToTypeB . GHC.varType . fst)
    let lbs = [ (varKey v, globalIdName (bindingId b), ty, bindingTerm b)
              | ((v, _), b, ty) <- zip3 bes bs tys ]
    return
      [ b { bindingTerm = letrecB lbs (varB (varKey v)), bindingRecursive = False }
      | ((v, _), b) <- zip bes bs ]
 where
  convBinding locals r v e = do
    let sp  = GHC.getSrcSpan v
        inl = GHC.inlinePragmaSpec . GHC.inlinePragInfo $ GHC.idInfo v
    tm <- RWS.local (srcSpan .~ sp) (coreToTermB primMap unlocatable locals e)
    v' <- coreToGlobalIdB v
    nm <- qualifiedNameString (GHC.varName v)
    let pr = if HashMap.member nm primMap then IsPrim else IsFun
    checkPrimitive primMap v
    return (Binding v' sp inl pr tm r)

-- | Convert a single class operation: its binder and the index of its method.
-- See 'processBind'.
processClsOp :: (GHC.CoreBndr, Int) -> C2C (GlobalId, Int)
processClsOp (v,i) = do
  v' <- coreToGlobalIdB v
  return (v',i)

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
  -- 'Clash.GHC.Scoped.GHC2Core.coreToTermB' work we want to parallelize). The
  -- state is left to be forced lazily when its 'TyCon' map is merged; its
  -- entries are cheap GHC 'TyCon' references (the heavy 'makeAllTyCons'
  -- conversion happens later).
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
mkClassSelector
  :: TyConMap
  -> S.Type 'VoidS
  -> Int
  -> S.Term 'VoidS
mkClassSelector tcm ty0 sel = go S.emptyScope ty0
  where
    tenv = mkTypeEnv tcm

    go :: S.Distinct n => S.Scope n -> S.Type n -> S.Term n
    go sc (S.ForAllTy b ty) = openTyBinder sc b ty $ \sc1 b1 ty1 -> S.TyLam b1 (go sc1 ty1)
    go sc ty = case S.tyView ty of
      S.FunTy dictTy _ ->
        withFreshIdBinder sc (mkUnsafeInternalName "dict" 0) dictTy $ \sc1 b ->
          S.Lam b (selector sc1 (S.sink dictTy) (S.idBinderVar b))
      _ -> error "mkClassSelector: expected at least one dictionary argument"

    selector :: S.Distinct n => S.Scope n -> S.Type n -> S.Term n -> S.Term n
    selector sc dictTy dict = case S.tyView dictTy of
      S.TyConApp tcNm _
        | Just tc <- lookupTyCon tenv tcNm
        , not (isNewTypeTc tc)
        -> S.mkSelectorCase "mkClassSelector" sc tenv dict 1 sel
      _ -> dict

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
