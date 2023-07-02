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
import           Control.DeepSeq         (deepseq)
import           Control.Lens            ((%~),(&),(.~))
import           Control.Monad           (unless)
import qualified Control.Monad.State     as State
import qualified Control.Monad.RWS.Strict as RWS
import           Data.Coerce             (coerce)
import           Data.Either             (partitionEithers, lefts ,rights)
import           Data.IntMap.Strict      (IntMap)
import qualified Data.IntMap.Strict      as IMS
import qualified Data.HashMap.Strict     as HashMap
import           Data.List               (isPrefixOf)
import           Data.Maybe              (listToMaybe)
import qualified Data.Text               as Text
import qualified Data.Time.Clock         as Clock

import qualified GHC                     as GHC (Ghc)
#if MIN_VERSION_ghc(9,0,0)
#if MIN_VERSION_ghc(9,4,0)
import qualified GHC.Types.SourceText    as GHC
#endif
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Utils.Panic         as GHC
#endif
import qualified GHC.Types.Basic         as GHC
import qualified GHC.Core                as GHC
import qualified GHC.Types.Demand        as GHC
import qualified GHC.Driver.Session      as GHC
import qualified GHC.Types.Id.Info       as GHC
import qualified GHC.Utils.Outputable    as GHC
import qualified GHC.Types.Name          as GHC hiding (varName)
import qualified GHC.Core.FamInstEnv     as GHC
import qualified GHC.Core.TyCon          as GHC
import qualified GHC.Core.Type           as GHC
import qualified GHC.Builtin.Types       as GHC
import qualified GHC.Settings.Constants  as GHC
import qualified GHC.Types.Var           as GHC
import qualified GHC.Types.SrcLoc        as GHC
#else
import qualified BasicTypes              as GHC
import qualified Constants               as GHC
import qualified CoreSyn                 as GHC
import qualified Demand                  as GHC
import qualified DynFlags                as GHC
import qualified FamInstEnv              as GHC
import qualified IdInfo                  as GHC
import qualified Outputable              as GHC
import qualified Name                    as GHC hiding (varName)
import qualified TyCon                   as GHC
import qualified Type                    as GHC
import qualified TysWiredIn              as GHC
import qualified Var                     as GHC
import qualified SrcLoc                  as GHC
#endif
import           GHC.BasicTypes.Extra (isOpaque)

import           Clash.Annotations.BitRepresentation.Internal (buildCustomReprs)
import           Clash.Annotations.Primitive (HDL, extractPrim)

import           Clash.Core.Subst        (extendGblSubstList, mkSubst, substTm)
import           Clash.Core.Term         (Term (..), mkLams, mkTyLams)
import           Clash.Core.Type         (Type (..), TypeView (..), mkFunTy, splitFunForallTy, tyView)
import           Clash.Core.TyCon        (TyConMap, TyConName, isNewTypeTc)
import           Clash.Core.TysPrim      (tysPrimMap)
import           Clash.Core.Util         (mkInternalVar, mkSelectorCase)
import           Clash.Core.Var          (Var (..), Id, IdScope (..), setIdScope)
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, emptyInScopeSet, extendInScopeSet, mkInScopeSet
  ,mkVarEnv, unionVarEnv, elemVarSet, mkVarSet)
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Debug             (traceIf)
import           Clash.Driver            (compilePrimitive)
import           Clash.Driver.Types      (BindingMap, Binding(..), IsPrim(..), ClashEnv(..), ClashDesign(..), ClashOpts(..))
import           Clash.GHC.GHC2Core
  (C2C, GHC2CoreState, GHC2CoreEnv (..), tyConMap, coreToId, coreToName, coreToTerm,
   makeAllTyCons, qualifiedNameString, emptyGHC2CoreState, srcSpan)
import           Clash.GHC.LoadModules   (ghcLibDir, loadModules)
import           Clash.Netlist.BlackBox.Util (getUsedArguments)
import           Clash.Netlist.Types     (TopEntityT(..))
import           Clash.Primitives.Types
  (Primitive (..), CompiledPrimMap)
import           Clash.Primitives.Util   (generatePrimMap)
import           Clash.Util              (reportTimeDiff)
import qualified Clash.Util.Interpolate as I

-- | Safe indexing, returns a 'Nothing' if the index does not exist
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
   , domainConfs ) <- loadModules startAction (opt_color opts) hdl modName dflagsM importDirs
  startTime <- Clock.getCurrentTime
  primMapR <- generatePrimMap unresolvedPrims primGuards (concat [pFP, primDirs, importDirs])
  tdir <- maybe ghcLibDir (pure . GHC.topDir) dflagsM
  primMapC <-
    sequence $ HashMap.map
                 (sequence . fmap (compilePrimitive importDirs dbs tdir))
                 primMapR
  let ((bindingsMap,clsVMap),tcMap,_) =
        RWS.runRWS (mkBindings primMapC bindings clsOps unlocatable)
                   (GHC2CoreEnv GHC.noSrcSpan fiEnvs)
                   emptyGHC2CoreState
      (tcMap',tupTcCache)           = mkTupTyCons tcMap
      tcCache                       = makeAllTyCons tcMap' fiEnvs
      allTcCache                    = tysPrimMap <> tcCache
      inScope0 = mkInScopeSet (
                      (fmap (coerce . bindingId) bindingsMap) <>
                      (fmap (coerce . bindingId) clsMap))
                                    -- Recursion info is always False for class
                                    -- selectors, no need to check free vars.
      clsMap =
        fmap (\(v,i) ->
#if MIN_VERSION_ghc(9,4,0)
               (Binding v GHC.noSrcSpan (GHC.Inline GHC.NoSourceText) IsFun
#else
               (Binding v GHC.noSrcSpan GHC.Inline IsFun
#endif
                  (mkClassSelector inScope0 allTcCache (varType v) i) False))
             clsVMap
      allBindings                   = bindingsMap `unionVarEnv` clsMap
      topEntities'                  =
        (\m -> fst (RWS.evalRWS m (GHC2CoreEnv GHC.noSrcSpan fiEnvs) tcMap')) $
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

  return
    ( ClashEnv
        { envOpts = opts
        , envTyConMap = allTcCache
        , envTupleTyCons = tupTcCache
        , envPrimitives = primMapC
        , envCustomReprs = buildCustomReprs customBitRepresentations
        }
    , ClashDesign
        { designEntities = topEntities''
        , designDomains = domainConfs
        , designBindings = allBindings'
        }
    )

setNoInlineTopEntities
  :: BindingMap
  -> [TopEntityT]
  -> BindingMap
setNoInlineTopEntities bm tes =
  fmap go bm
 where
  ids = mkVarSet (fmap topId tes)

  go b@Binding{bindingId}
    | bindingId `elemVarSet` ids
#if MIN_VERSION_ghc(9,4,0)
    = b { bindingSpec = GHC.Opaque GHC.NoSourceText }
#else
    = b { bindingSpec = GHC.NoInline }
#endif
    | otherwise = b

-- TODO This function should be changed to provide the information that
-- Clash.Core.Termination.mkRecInfo provides. To achieve this, it should also
-- be changed to no longer flatten recursive groups (see the documentation for
-- mkRecInfo for an explanation of these).
--
mkBindings
  :: CompiledPrimMap
  -> [GHC.CoreBind]
  -- Binders
  -> [(GHC.CoreBndr,Int)]
  -- Class operations
  -> [GHC.CoreBndr]
  -- Unlocatable Expressions
  -> C2C ( BindingMap
         , VarEnv (Id,Int)
         )
mkBindings primMap bindings clsOps unlocatable = do
  bindingsList <- mapM (\case
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
      tms <- mapM (\(v,e) -> do
                    let sp  = GHC.getSrcSpan v
                        inl = GHC.inlinePragmaSpec . GHC.inlinePragInfo $ GHC.idInfo v
                    tm <- RWS.local (srcSpan .~ sp) (coreToTerm primMap unlocatable e)
                    v' <- coreToId v
                    nm <- qualifiedNameString (GHC.varName v)
                    let pr = if HashMap.member nm primMap then IsPrim else IsFun
                    checkPrimitive primMap v
                    return (Binding v' sp inl pr tm True)
                  ) bs
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
    ) bindings
  clsOpList    <- mapM (\(v,i) -> do
                          v' <- coreToId v
                          return (v', (v',i))
                       ) clsOps

  return (mkVarEnv (concat bindingsList), mkVarEnv clsOpList)

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
#if MIN_VERSION_ghc(9,4,0)
        strictness = GHC.dmdSigInfo info
#else
        strictness = GHC.strictnessInfo info
#endif
        ty = GHC.varType v
#if MIN_VERSION_ghc(9,2,0)
        (argTys,_resTy) = GHC.splitFunTys (snd (GHC.splitForAllTyCoVars ty))
#else
        (argTys,_resTy) = GHC.splitFunTys . snd . GHC.splitForAllTys $ ty
#endif
#if MIN_VERSION_ghc(9,4,0)
        (dmdArgs,_dmdRes) = GHC.splitDmdSig strictness
#else
        (dmdArgs,_dmdRes) = GHC.splitStrictSig strictness
#endif
        nrOfArgs = length argTys
        loc = case GHC.getSrcLoc v of
                GHC.UnhelpfulLoc _ -> ""
#if MIN_VERSION_ghc(9,0,0)
                GHC.RealSrcLoc l _ -> showPpr l ++ ": "
#else
                GHC.RealSrcLoc l   -> showPpr l ++ ": "
#endif
        warnIf cond msg = traceIf cond ("\n"++loc++"Warning: "++msg) return ()
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
#if MIN_VERSION_ghc(9,4,0)
          (primStr ++ "isn't marked OPAQUE."
#else
          (primStr ++ "isn't marked NOINLINE."
#endif
          ++ "\nThis might make Clash ignore this primitive.")
#if MIN_VERSION_ghc(9,2,0)
        warnIf (GHC.isDeadEndAppSig strictness nrOfArgs)
#elif MIN_VERSION_ghc(9,0,0)
        warnIf (GHC.appIsDeadEnd strictness nrOfArgs)
#else
        warnIf (GHC.appIsBottom strictness nrOfArgs)
#endif
          ("The Haskell implementation of " ++ primStr
          ++ "produces a result that always results in an error.\n"
          ++ "This can lead to compile failures because GHC can replace entire "
          ++ "calls to this primitive by an undefined value.")
        warnArgs usedArgs
    _ -> return ()
  where
    showPpr :: GHC.Outputable a => a -> String
    showPpr = GHC.showSDocUnsafe . GHC.ppr

mkClassSelector
  :: InScopeSet
  -> TyConMap
  -> Type
  -> Int
  -> Term
mkClassSelector inScope0 tcm ty sel = newExpr
  where
    -- TODO: why can't we just use partitionEithers here?
    (tvs,dicts) = (lefts *** rights)
                . span (\l -> case l of {Left _ -> True; _ -> False})
                $ fst (splitFunForallTy ty)
    newExpr = case listToMaybe dicts of
      Just dictTy@(tyView -> TyConApp tcNm _)
        | Just tc <- UniqMap.lookup tcNm tcm
        , not (isNewTypeTc tc)
        -> flip State.evalState (0 :: Int) $ do
              dcId <- mkInternalVar inScope0 "dict" dictTy
              let inScope1 = extendInScopeSet inScope0 dcId
              selE <- mkSelectorCase "mkClassSelector" inScope1 tcm (Var dcId) 1 sel
              return (mkTyLams (mkLams selE [dcId]) tvs)
      Just (tyView -> FunTy arg res) -> flip State.evalState (0 :: Int) $ do
              dcId <- mkInternalVar inScope0 "dict" (mkFunTy arg res)
              return (mkTyLams (mkLams (Var dcId) [dcId]) tvs)
      Just dictTy -> flip State.evalState (0 :: Int) $ do
              dcId <- mkInternalVar inScope0 "dict" dictTy
              return (mkTyLams (mkLams (Var dcId) [dcId]) tvs)
      Nothing -> error "mkClassSelector: expected at least one dictionary argument"

mkTupTyCons :: GHC2CoreState -> (GHC2CoreState,IntMap TyConName)
mkTupTyCons tcMap = (tcMap'',tupTcCache)
  where
    tupTyCons        = GHC.boolTyCon : GHC.promotedTrueDataCon : GHC.promotedFalseDataCon
                     : map (GHC.tupleTyCon GHC.Boxed) [2..GHC.mAX_TUPLE_SIZE]
    (tcNames,tcMap',_) =
      RWS.runRWS (mapM (\tc -> coreToName GHC.tyConName GHC.tyConUnique
                                          qualifiedNameString tc) tupTyCons)
                 (GHC2CoreEnv GHC.noSrcSpan GHC.emptyFamInstEnvs)
                 tcMap
    tupTcCache       = IMS.fromList (zip [2..GHC.mAX_TUPLE_SIZE] (drop 3 tcNames))
    tupHM            = UniqMap.fromList (zip tcNames tupTyCons)
    tcMap''          = tcMap' & tyConMap %~ (<> tupHM)
