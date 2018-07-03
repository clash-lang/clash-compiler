{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                          2017, QBayLogic, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Clash.GHC.GenerateBindings
  (generateBindings)
where

import           Control.Lens            ((%~),(&),(^.),_1,_2)
import           Control.Monad           (unless, when)
import           Control.Monad.State     (State)
import qualified Control.Monad.State     as State
import           Data.Either             (lefts, rights)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.IntMap.Strict      (IntMap)
import qualified Data.IntMap.Strict      as IM
import           Data.List               (foldl')
import qualified Data.Text               as Text
import qualified Data.Set                as Set
import qualified Data.Set.Lens           as Lens
import           Unbound.Generics.LocallyNameless (bind,embed,rec,runFreshM,unembed)

import qualified BasicTypes              as GHC
import qualified CoreSyn                 as GHC
import qualified Demand                  as GHC
import qualified DynFlags                as GHC
import qualified IdInfo                  as GHC
import qualified Outputable              as GHC
import qualified Name                    as GHC hiding (varName)
import qualified TyCon                   as GHC
import qualified Type                    as GHC
import qualified TysWiredIn              as GHC
import qualified Var                     as GHC
import qualified SrcLoc                  as GHC

import           Clash.Annotations.BitRepresentation.Internal (DataRepr')
import           Clash.Annotations.TopEntity (TopEntity)
import           Clash.Annotations.Primitive (HDL)

import           Clash.Core.FreeVars     (termFreeIds)
import           Clash.Core.Name         (Name (..), string2SystemName)
import           Clash.Core.Term         (Term (..), TmName, TmOccName)
import           Clash.Core.Type         (Type (..), TypeView (..), mkFunTy, splitFunForallTy, tyView)
import           Clash.Core.TyCon        (TyCon, TyConName, TyConOccName)
import           Clash.Core.TysPrim      (tysPrimMap)
import           Clash.Core.Subst        (substTms)
import           Clash.Core.Util         (mkLams, mkTyLams, termType)
import           Clash.Core.Var          (Var (..))
import           Clash.Driver.Types      (BindingMap)
import           Clash.GHC.GHC2Core      (GHC2CoreState, tyConMap, coreToId, coreToName, coreToTerm,
                                          makeAllTyCons, qualfiedNameString, emptyGHC2CoreState)
import           Clash.GHC.LoadModules   (loadModules)
import           Clash.Primitives.Types  (PrimMap, ResolvedPrimMap, Primitive)
import           Clash.Primitives.Util   (generatePrimMap)
import           Clash.Rewrite.Util      (mkInternalVar, mkSelectorCase)
import           Clash.Util              ((***),first,traceIf)

generateBindings
  :: [FilePath]
  -- ^ primitives (blackbox) directories
  -> [FilePath]
  -- ^ import directories (-i flag)
  -> HDL
  -- ^ HDL target
  -> String
  -> Maybe  (GHC.DynFlags)
  -> IO ( BindingMap
        , HashMap TyConOccName TyCon
        , IntMap TyConName
        , [( TmName          -- topEntity bndr
           , Type            -- type of the topEntity bndr
           , Maybe TopEntity -- (maybe) TopEntity annotation
           , Maybe TmName    -- (maybe) associated testbench
           )]
        , ResolvedPrimMap  -- The primitives found in '.' and 'primDir'
        , [DataRepr']
        )
generateBindings primDirs importDirs hdl modName dflagsM = do
  (bindings,clsOps,unlocatable,fiEnvs,topEntities,pFP,reprs) <- loadModules hdl modName dflagsM
  primMap <- generatePrimMap $ concat [pFP, primDirs, importDirs]
  let ((bindingsMap,clsVMap),tcMap) = State.runState (mkBindings primMap bindings clsOps unlocatable) emptyGHC2CoreState
      (tcMap',tupTcCache)           = mkTupTyCons tcMap
      tcCache                       = makeAllTyCons tcMap' fiEnvs
      allTcCache                    = tysPrimMap `HashMap.union` tcCache
      clsMap                        = HashMap.map (\(nm,ty,i) -> (nm,ty,GHC.noSrcSpan,GHC.Inline,mkClassSelector allTcCache ty i)) clsVMap
      allBindings                   = bindingsMap `HashMap.union` clsMap
      topEntities'                  =
        flip State.evalState tcMap' $ mapM (\(topEnt,annM,benchM) -> do
          topEnt' <- coreToName GHC.varName GHC.varUnique qualfiedNameString topEnt
          benchM' <- traverse (coreToName GHC.varName GHC.varUnique qualfiedNameString) benchM
          return (topEnt',annM,benchM')) topEntities
      retypedBindings               = retypeBindings allTcCache allBindings topEntities'
      topEntities''                 = map (\(topEnt,annM,benchM) -> case HashMap.lookup (nameOcc topEnt) retypedBindings of
                                              Just (_,ty,_,_,_) -> (topEnt,ty,annM,benchM)
                                              Nothing       -> error "This shouldn't happen"
                                          ) topEntities'

  return (retypedBindings, allTcCache, tupTcCache, topEntities'', primMap, reprs)

retypeBindings
  :: HashMap TyConOccName TyCon
  -> BindingMap
  -> [(TmName,Maybe TopEntity,Maybe TmName)]
  -> BindingMap
retypeBindings allTcCache = foldl' go
  where
    go allBindings (topEnt,_,benchM) = bBindings
      where
        topEntity = do e <- HashMap.lookup (nameOcc topEnt) allBindings
                       return (nameOcc topEnt,e)
        bench     = do t <- benchM
                       e <- HashMap.lookup (nameOcc t) allBindings
                       return (nameOcc t,e)

        tBindings = maybe allBindings (retype' allBindings) topEntity
        bBindings = maybe tBindings (retype' tBindings) bench
        retype' d (t,_) = snd (retype allTcCache ([],d) t)

-- | clean up cast-removal mess
retype
  :: HashMap TyConOccName TyCon
  -> ([TmOccName], BindingMap) -- (visited, bindings)
  -> TmOccName                 -- top
  -> ([TmOccName], BindingMap)
retype tcm (visited,bindings) current = (visited', HashMap.insert current (nm,ty',sp,inl,tm') bindings')
  where
    (nm,_,sp,inl,tm)     = bindings HashMap.! current
    used                 = Set.toList $ Lens.setOf termFreeIds tm
    (visited',bindings') = foldl (retype tcm) (current:visited,bindings) (filter (`notElem` visited) used)
    used'                = map ((^. _1) . (bindings' HashMap.!)) used
    usedTys              = map ((^. _2) . (bindings' HashMap.!)) used
    usedVars             = zipWith Var usedTys used'
    tm'                  = substTms (zip used usedVars) tm
    ty'                  = runFreshM (termType tcm tm')

mkBindings
  :: PrimMap (Primitive a b c)
  -> [GHC.CoreBind]
  -- Binders
  -> [(GHC.CoreBndr,Int)]
  -- Class operations
  -> [GHC.CoreBndr]
  -- Unlocatable Expressions
  -> State GHC2CoreState
           ( BindingMap
           , HashMap TmOccName (TmName,Type,Int)
           )
mkBindings primMap bindings clsOps unlocatable = do
  bindingsList <- mapM (\case
                          GHC.NonRec v e -> do
                            let sp = GHC.getSrcSpan v
                                inl = GHC.inlinePragmaSpec . GHC.inlinePragInfo $ GHC.idInfo v
                            tm <- coreToTerm primMap unlocatable sp e
                            v' <- coreToId v
                            checkPrimitive primMap v
                            return [(nameOcc (varName v'), (varName v',unembed (varType v'), sp, inl, tm))]
                          GHC.Rec bs -> do
                            tms <- mapM (\(v,e) -> do
                                          let sp = GHC.getSrcSpan v
                                          tm <- coreToTerm primMap unlocatable sp e
                                          v' <- coreToId v
                                          checkPrimitive primMap v
                                          return (v',sp,tm)
                                        ) bs
                            case tms of
                              [(v,sp,tm)] -> return [(nameOcc (varName v), (varName v,unembed (varType v), sp, GHC.NoInline, tm))]
                              _ ->
                                return $ map (\(v,sp,e) -> (nameOcc (varName v),(varName v,unembed (varType v),sp,GHC.NoInline
                                                  ,Letrec (bind (rec (map (\(x,_,y) -> (x,embed y)) tms)) e)))) tms
                       ) bindings
  clsOpList    <- mapM (\(v,i) -> do
                          v' <- coreToId v
                          let ty = unembed $ varType v'
                          return (nameOcc (varName v'), (varName v',ty,i))
                       ) clsOps

  return (HashMap.fromList (concat bindingsList), HashMap.fromList clsOpList)

-- | If this CoreBndr is a primitive, check it's Haskell definition
--   for potential problems.
--
-- Warns when a primitive:
--   * isn't marked NOINLINE
--   * produces an error when evaluating its result to WHNF
--   * isn't using all its arguments
checkPrimitive :: PrimMap a -> GHC.CoreBndr -> State GHC2CoreState ()
checkPrimitive primMap v = do
  name' <- Text.pack <$> qualfiedNameString (GHC.varName v)
  when (name' `HashMap.member` primMap) $ do
    let
      info = GHC.idInfo v
      inline = GHC.inlinePragmaSpec $ GHC.inlinePragInfo info
      strictness = GHC.strictnessInfo info
      ty = GHC.varType v
      (argTys,_resTy) = GHC.splitFunTys . snd . GHC.splitForAllTys $ ty
      (dmdArgs,_dmdRes) = GHC.splitStrictSig strictness
      nrOfArgs = length argTys
      loc = case GHC.getSrcLoc v of
              GHC.UnhelpfulLoc _ -> ""
              GHC.RealSrcLoc l   -> showPpr l ++ ": "
      warnIf cond msg = traceIf cond ("\n"++loc++"Warning: "++msg) return ()
    qName <- qualfiedNameString (GHC.varName v)
    let primStr = "primitive " ++ qName ++ " "
    unless (qName `elem` [ "Clash.XException.errorX"
                         , "GHC.Err.error"
                         , "GHC.Err.errorWithoutStackTrace"
                         , "GHC.Err.undefined" -- doesn't seem to need it
                         , "GHC.Natural.underflowError"
                         , "GHC.Real.divZeroError"
                         , "GHC.Real.overflowError"
                         , "GHC.Real.ratioZeroDenominatorError"
                         ]) $ do
      warnIf (inline /= GHC.NoInline)
        (primStr ++ "isn't marked NOINLINE."
        ++ "\nThis might make Clash ignore this primitive.")
      warnIf (GHC.appIsBottom strictness nrOfArgs)
        (primStr
        ++ "produces a result that can't be evaluated to WHNF, "
        ++ "because it results in an error."
        ++ "\nThis might make Clash ignore this primitive.")
      warnIf (any GHC.isAbsDmd dmdArgs)
        (primStr
        ++ "isn't using all its arguments, some might be optimized away.")
  where
    showPpr :: GHC.Outputable a => a -> String
    showPpr = GHC.showSDocUnsafe . GHC.ppr

mkClassSelector
  :: HashMap TyConOccName TyCon
  -> Type
  -> Int
  -> Term
mkClassSelector tcm ty sel = newExpr
  where
    ((tvs,dictTy:_),_) = first (lefts *** rights)
                       $ first (span (\l -> case l of Left _ -> True
                                                      _      -> False))
                       $ splitFunForallTy ty
    newExpr = case tyView dictTy of
      (TyConApp _ _) -> runFreshM $ flip State.evalStateT (0 :: Int) $ do
                          (dcId,dcVar) <- mkInternalVar (string2SystemName "dict") dictTy
                          selE         <- mkSelectorCase "mkClassSelector" tcm dcVar 1 sel
                          return (mkTyLams (mkLams selE [dcId]) tvs)
      (FunTy arg res) -> runFreshM $ flip State.evalStateT (0 :: Int) $ do
                           (dcId,dcVar) <- mkInternalVar (string2SystemName "dict") (mkFunTy arg res)
                           return (mkTyLams (mkLams dcVar [dcId]) tvs)
      (OtherType oTy) -> runFreshM $ flip State.evalStateT (0 :: Int) $ do
                           (dcId,dcVar) <- mkInternalVar (string2SystemName "dict") oTy
                           return (mkTyLams (mkLams dcVar [dcId]) tvs)

mkTupTyCons :: GHC2CoreState -> (GHC2CoreState,IntMap TyConName)
mkTupTyCons tcMap = (tcMap'',tupTcCache)
  where
    tupTyCons        = map (GHC.tupleTyCon GHC.Boxed) [2..62]
    (tcNames,tcMap') = State.runState (mapM (\tc -> coreToName GHC.tyConName GHC.tyConUnique qualfiedNameString tc) tupTyCons) tcMap
    tupTcCache       = IM.fromList (zip [2..62] tcNames)
    tupHM            = HashMap.fromList (zip (map nameOcc tcNames) tupTyCons)
    tcMap''          = tcMap' & tyConMap %~ (`HashMap.union` tupHM)
