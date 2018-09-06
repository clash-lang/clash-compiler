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

import           Control.Lens            ((%~),(&))
import           Control.Monad.State     (State)
import qualified Control.Monad.State     as State
import           Data.Either             (lefts, rights)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.IntMap.Strict      (IntMap)
import qualified Data.IntMap.Strict      as IM
import           Unbound.Generics.LocallyNameless (bind,embed,rec,runFreshM,unembed)

import qualified BasicTypes              as GHC
import qualified CoreSyn                 as GHC
import qualified DynFlags                as GHC
import qualified IdInfo                  as GHC
import qualified Name                    as GHC hiding (varName)
import qualified TyCon                   as GHC
import qualified TysWiredIn              as GHC
import qualified Var                     as GHC
import qualified SrcLoc                  as GHC

import           Clash.Annotations.BitRepresentation.Internal (DataRepr')
import           Clash.Annotations.TopEntity (TopEntity)
import           Clash.Annotations.Primitive (HDL)

import           Clash.Core.Name         (Name (..), string2SystemName)
import           Clash.Core.Term         (Term (..), TmName, TmOccName)
import           Clash.Core.Type         (Type (..), TypeView (..), mkFunTy, splitFunForallTy, tyView)
import           Clash.Core.TyCon        (TyCon, TyConName, TyConOccName)
import           Clash.Core.TysPrim      (tysPrimMap)
import           Clash.Core.Util         (mkLams, mkTyLams)
import           Clash.Core.Var          (Var (..))
import           Clash.Driver.Types      (BindingMap)
import           Clash.GHC.GHC2Core      (GHC2CoreState, tyConMap, coreToId, coreToName, coreToTerm,
                                          makeAllTyCons, qualfiedNameString, emptyGHC2CoreState)
import           Clash.GHC.LoadModules   (loadModules)
import           Clash.Primitives.Types  (PrimMap, ResolvedPrimMap, Primitive)
import           Clash.Primitives.Util   (generatePrimMap)
import           Clash.Rewrite.Util      (mkInternalVar, mkSelectorCase)
import           Clash.Util              ((***),first)

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
      topEntities''                 = map (\(topEnt,annM,benchM) -> case HashMap.lookup (nameOcc topEnt) allBindings of
                                              Just (_,ty,_,_,_) -> (topEnt,ty,annM,benchM)
                                              Nothing       -> error "This shouldn't happen"
                                          ) topEntities'

  return (allBindings, allTcCache, tupTcCache, topEntities'', primMap, reprs)

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
                            return [(nameOcc (varName v'), (varName v',unembed (varType v'), sp, inl, tm))]
                          GHC.Rec bs -> do
                            tms <- mapM (\(v,e) -> do
                                          let sp = GHC.getSrcSpan v
                                          tm <- coreToTerm primMap unlocatable sp e
                                          v' <- coreToId v
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
