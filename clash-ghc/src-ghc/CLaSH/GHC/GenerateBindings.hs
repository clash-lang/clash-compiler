{-|
  Copyright   :  (C) 2013-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module CLaSH.GHC.GenerateBindings
  (generateBindings)
where

import           Control.Lens            ((%~),(&),(^.),_1)
import           Control.Monad.State     (State)
import qualified Control.Monad.State     as State
import           Data.Either             (lefts, rights)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.IntMap.Strict      (IntMap)
import qualified Data.IntMap.Strict      as IM
import           Data.Text.Lazy          (Text)
import qualified Data.Set                as Set
import qualified Data.Set.Lens           as Lens
import           Unbound.Generics.LocallyNameless (runFreshM, unembed)

import qualified BasicTypes              as GHC
import qualified CoreSyn                 as GHC
import qualified DynFlags                as GHC
import qualified Name                    as GHC hiding (varName)
import qualified TyCon                   as GHC
import qualified TysWiredIn              as GHC
import qualified Var                     as GHC
import qualified SrcLoc                  as GHC

import           CLaSH.Annotations.TopEntity (TopEntity)
import           CLaSH.Core.FreeVars     (termFreeIds)
import           CLaSH.Core.Term         (Term (..), TmName)
import           CLaSH.Core.Type         (Type, TypeView (..), mkFunTy, splitFunForallTy, tyView)
import           CLaSH.Core.TyCon        (TyCon, TyConName)
import           CLaSH.Core.TysPrim      (tysPrimMap)
import           CLaSH.Core.Subst        (substTms)
import           CLaSH.Core.Util         (mkLams, mkTyLams, termType)
import           CLaSH.Core.Var          (Var (..))
import           CLaSH.Driver.Types      (BindingMap)
import           CLaSH.GHC.GHC2Core      (GHC2CoreState, tyConMap, coreToId, coreToName, coreToTerm,
                                          makeAllTyCons, qualfiedNameString, emptyGHC2CoreState)
import           CLaSH.GHC.LoadModules   (loadModules)
import           CLaSH.Normalize.Util
import           CLaSH.Primitives.Types  (PrimMap)
import           CLaSH.Primitives.Util   (generatePrimMap)
import           CLaSH.Rewrite.Util      (mkInternalVar, mkSelectorCase)
import           CLaSH.Util              ((***),first)

generateBindings ::
     FilePath
  -> String
  -> Maybe  (GHC.DynFlags)
  -> IO (BindingMap,HashMap TyConName TyCon,IntMap TyConName
        ,(TmName, Maybe TopEntity) -- topEntity bndr + (maybe) TopEntity annotation
        ,Maybe TmName              -- testInput bndr
        ,Maybe TmName              -- expectedOutput bndr
        ,PrimMap Text)             -- The primitives found in '.' and 'primDir'
generateBindings primDir modName dflagsM = do
  (bindings,clsOps,unlocatable,fiEnvs,(topEnt,topEntAnn),testInpM,expOutM) <- loadModules modName dflagsM
  primMap <- generatePrimMap [primDir,"."]
  let ((bindingsMap,clsVMap),tcMap) = State.runState (mkBindings primMap bindings clsOps unlocatable) emptyGHC2CoreState
      (tcMap',tupTcCache)           = mkTupTyCons tcMap
      tcCache                       = makeAllTyCons tcMap' fiEnvs
      allTcCache                    = tysPrimMap `HashMap.union` tcCache
      clsMap                        = HashMap.map (\(ty,i) -> (ty,GHC.noSrcSpan,mkClassSelector allTcCache ty i)) clsVMap
      allBindings                   = bindingsMap `HashMap.union` clsMap
      (topEnt',testInpM',expOutM')  = flip State.evalState tcMap' $ do
                                          topEnt'' <- coreToName GHC.varName GHC.varUnique qualfiedNameString topEnt
                                          testInpM'' <- traverse (coreToName GHC.varName GHC.varUnique qualfiedNameString) testInpM
                                          expOutM'' <- traverse (coreToName GHC.varName GHC.varUnique qualfiedNameString) expOutM
                                          return (topEnt'',testInpM'',expOutM'')
      droppedAndRetypedBindings     = dropAndRetypeBindings allTcCache allBindings topEnt' testInpM' expOutM'

  return (droppedAndRetypedBindings,allTcCache,tupTcCache,(topEnt',topEntAnn),testInpM',expOutM',primMap)

dropAndRetypeBindings :: HashMap TyConName TyCon
                      -> BindingMap
                      -> TmName        -- ^ topEntity
                      -> Maybe TmName  -- ^ testInput
                      -> Maybe TmName  -- ^ expectedOutput
                      -> BindingMap

dropAndRetypeBindings allTcCache allBindings topEnt testInpM expOutM = oBindings
  where
    topEntity = do e <- HashMap.lookup topEnt allBindings
                   return (topEnt,e)
    testInput = do t <- testInpM
                   e <- HashMap.lookup t allBindings
                   return (t,e)
    expectedOut = do t <- expOutM
                     e <- HashMap.lookup t allBindings
                     return (t,e)

    tBindings = maybe allBindings (dropAndRetype allBindings) topEntity
    iBindings = maybe tBindings (dropAndRetype tBindings) testInput
    oBindings = maybe iBindings (dropAndRetype iBindings) expectedOut
    dropAndRetype d (t,_) = snd (retype allTcCache ([],lambdaDropPrep d t) t)

-- | clean up cast-removal mess
retype :: HashMap TyConName TyCon
       -> ([TmName], BindingMap) -- (visited, bindings)
       -> TmName                 -- top
       -> ([TmName], BindingMap)
retype tcm (visited,bindings) current = (visited', HashMap.insert current (ty',sp,tm') bindings')
  where
    (_,sp,tm)            = bindings HashMap.! current
    used                 = Set.toList $ Lens.setOf termFreeIds tm
    (visited',bindings') = foldl (retype tcm) (current:visited,bindings) (filter (`notElem` visited) used)
    usedTys              = map ((^. _1) . (bindings' HashMap.!)) used
    usedVars             = zipWith Var usedTys used
    tm'                  = substTms (zip used usedVars) tm
    ty'                  = runFreshM (termType tcm tm')

mkBindings :: PrimMap a
           -> [(GHC.CoreBndr, GHC.CoreExpr)] -- Binders
           -> [(GHC.CoreBndr,Int)]           -- Class operations
           -> [GHC.CoreBndr]                 -- Unlocatable Expressions
           -> State GHC2CoreState
                    ( BindingMap
                    , HashMap TmName (Type,Int)
                    )
mkBindings primMap bindings clsOps unlocatable = do
  bindingsList <- mapM (\(v,e) -> do
                          let sp = GHC.getSrcSpan v
                          tm <- coreToTerm primMap unlocatable sp e
                          v' <- coreToId v
                          return (varName v', (unembed (varType v'), sp, tm))
                       ) bindings
  clsOpList    <- mapM (\(v,i) -> do
                          v' <- coreToId v
                          let ty = unembed $ varType v'
                          return (varName v', (ty,i))
                       ) clsOps

  return (HashMap.fromList bindingsList, HashMap.fromList clsOpList)

mkClassSelector :: HashMap TyConName TyCon
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
                          (dcId,dcVar) <- mkInternalVar "dict" dictTy
                          selE         <- mkSelectorCase "mkClassSelector" tcm dcVar 1 sel
                          return (mkTyLams (mkLams selE [dcId]) tvs)
      (FunTy arg res) -> runFreshM $ flip State.evalStateT (0 :: Int) $ do
                           (dcId,dcVar) <- mkInternalVar "dict" (mkFunTy arg res)
                           return (mkTyLams (mkLams dcVar [dcId]) tvs)
      (OtherType oTy) -> runFreshM $ flip State.evalStateT (0 :: Int) $ do
                           (dcId,dcVar) <- mkInternalVar "dict" oTy
                           return (mkTyLams (mkLams dcVar [dcId]) tvs)

mkTupTyCons :: GHC2CoreState -> (GHC2CoreState,IntMap TyConName)
mkTupTyCons tcMap = (tcMap'',tupTcCache)
  where
    tupTyCons        = map (GHC.tupleTyCon GHC.BoxedTuple) [2..62]
    (tcNames,tcMap') = State.runState (mapM (\tc -> coreToName GHC.tyConName GHC.tyConUnique qualfiedNameString tc) tupTyCons) tcMap
    tupTcCache       = IM.fromList (zip [2..62] tcNames)
    tupHM            = HashMap.fromList (zip tcNames tupTyCons)
    tcMap''          = tcMap' & tyConMap %~ (`HashMap.union` tupHM)
