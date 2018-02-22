{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                          2017, QBayLogic, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.GenerateBindings
  (generateBindings, customRepresentations)
where

import           Control.DeepSeq         (deepseq)
import           Control.Exception       (throw)
import           Control.Lens            ((%~),(&),(^.),_1,_2)
import           Control.Monad.State     (State)
import qualified Control.Monad.State     as State
import           Data.Either             (lefts, rights)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.IntMap.Strict      (IntMap)
import qualified Data.IntMap.Strict      as IM
import           Data.List               (foldl')
import qualified Data.Text.Lazy          as Text
import qualified Data.Set                as Set
import qualified Data.Set.Lens           as Lens
import           Unbound.Generics.LocallyNameless (bind,embed,rec,runFreshM,unembed,unbind)

import qualified BasicTypes              as GHC
import qualified CoreSyn                 as GHC
import qualified DynFlags                as GHC
import qualified IdInfo                  as GHC
import qualified Name                    as GHC hiding (varName)
import qualified TyCon                   as GHC
import qualified TysWiredIn              as GHC
import qualified Var                     as GHC
import qualified SrcLoc                  as GHC

import           Clash.Annotations.BitRepresentation.Internal
import           Clash.Annotations.TopEntity (TopEntity)
import           Clash.Annotations.Primitive (HDL)
import           Clash.Core.FreeVars     (termFreeIds)
import           Clash.Core.Name         (Name (..), string2SystemName, name2String)
import           Clash.Core.Term         (Term (..), TmName, TmOccName)
import           Clash.Core.Type         (Type (..), TypeView (..), mkFunTy, splitFunForallTy, tyView)
import           Clash.Core.TyCon        (TyCon, TyConName, TyConOccName)
import           Clash.Core.Literal      (Literal(IntegerLiteral, StringLiteral))
import           Clash.Core.TysPrim      (tysPrimMap)
import           Clash.Core.Subst        (substTms)
import           Clash.Core.Util         (mkLams, mkTyLams, termType)
import           Clash.Core.Var          (Var (..))
import           Clash.Driver.Types      (BindingMap,ClashException(..))
import           Clash.GHC.GHC2Core      (GHC2CoreState, tyConMap, coreToId, coreToName, coreToTerm,
                                          makeAllTyCons, qualfiedNameString, emptyGHC2CoreState)
import           Clash.GHC.LoadModules   (loadModules)
import           Clash.Primitives.Types  (PrimMap)
import           Clash.Primitives.Util   (generatePrimMap)
import           Clash.Rewrite.Util      (mkInternalVar, mkSelectorCase)
import           Clash.Util              ((***),first,curLoc)

dataReprName :: String
dataReprName = "Clash.Annotations.BitRepresentation.DataRepr"

polymorphicReprErr :: String
polymorphicReprErr = unwords
  [ "Found a custom data representation structure (DataRepr), but it"
  , "it was left polymorphic. We can only deal with monomorphic instances." ]

-- | Determines if a type is a DataRepr. Will throw a ClashException
-- if it encounters a polymorphic DataRepr.
isDataRepr
  :: GHC.SrcSpan
  -> Type
  -> Bool
isDataRepr srcspan (tyView -> ty) =
  case ty of
    (TyConApp name _) ->
      dataReprName == name2String name
    -- If user left DataRepr polymorphic, throw an error:
    (OtherType (ForAllTy boundedType)) ->
      let (_, ty') = runFreshM (unbind boundedType) in
      case tyView ty' of
        (TyConApp name' _) ->
          if dataReprName == name2String name'
          then
            throw $ ClashException srcspan ($(curLoc) ++ polymorphicReprErr) Nothing
          else
            False
        _ ->
          False
    _ ->
      False

deref
  :: BindingMap
  -> Term
  -> Term
-- Application:
deref bindings (App t1 t2) =
  App (deref bindings t1) (deref bindings t2)

-- Variable reference:
deref bindings (Var _type name)  =
  let (_, _, _, _, term) = bindings HashMap.! (nameOcc name) in
  deref bindings term

-- All others. Should err on Lam / TyLam / LetRec?
deref _bindings t = t

parseTHName :: Term -> Text.Text
parseTHName (App (App _ name) (App _ mod_)) =
  Text.pack $ (parseStringLit module_) ++ "." ++ (parseStringLit name)
parseTHName th = error $ $(curLoc) ++ "Expected THName, but got: " ++ show th

parseStringLit :: Term -> String
parseStringLit (App _ (Literal (StringLiteral s))) = s
parseStringLit sl = error $ $(curLoc) ++ "Expected stringlit, but got: " ++ show sl

parseIntegerLit :: Term -> Integer
parseIntegerLit (Literal (IntegerLiteral n)) = n
parseIntegerLit wl = error $ $(curLoc) ++ "Expected integerlit, but got: " ++ show wl

parseList :: Term -> [Term]
parseList (App (App _type headTerm) tailTerm) = headTerm : parseList tailTerm
parseList (TyApp _ _) = []
parseList l           = error $ $(curLoc) ++ "Expected list, but got: " ++ show l

sanityCheckConstrRepr :: GHC.SrcSpan -> ConstrRepr' -> ConstrRepr'
sanityCheckConstrRepr _src repr@(ConstrRepr' _name _n _mask _value _masks) = repr

parseConstrRepr :: GHC.SrcSpan -> Int -> Term -> ConstrRepr'
parseConstrRepr src n (App (App (App (App _constr thname) mask) value) fieldmasks) =
  sanityCheckConstrRepr src $
  ConstrRepr'
    (parseTHName thname)
    n
    (parseIntegerLit mask)
    (parseIntegerLit value)
    (map parseIntegerLit $ parseList fieldmasks)
parseConstrRepr _src _n cr = error $ $(curLoc) ++ "Expected ConstrRepr, but got: " ++ show cr

stripDataRepr :: TypeName' -> TypeName'
stripDataRepr (TypeName' _dataRepr [tn]) = tn
stripDataRepr e = error $ "Unexpected type: " ++ show e

coreToTypeName'' :: Type -> TypeName'
coreToTypeName'' typ =
  case coreToTypeName' typ of
    Right typ' ->
      typ'
    Left err ->
      error $ concat [ $(curLoc), "Could not translate type: ", show typ, "."
                     , "\n\n", "coreToTypeName' reported: ", err, "." ]

parseDataRepr
  :: BindingMap
  -- HashMap TmOccName (TmName,Type,SrcSpan,InlineSpec,Term)
  -> (TmName, Type, GHC.SrcSpan, GHC.InlineSpec, Term)
  -> DataRepr'
parseDataRepr bindings (_name, typ, srcspan, _, (deref bindings -> reprsTerm)) =
  case reprsTerm of
    (App (App _constr size) reprs) ->
      let reprs' = [parseConstrRepr srcspan i r | (i, r) <- zip [0..] (parseList reprs)] in
      let size'  = parseIntegerLit size in
      let typ'   = stripDataRepr $ coreToTypeName'' typ in
      deepseq (reprs', size', typ') (DataRepr' typ' size' reprs')
    _ ->
      error $ $(curLoc) ++ "Expected DataRepr, but got: " ++ show reprsTerm

customRepresentations
  :: [FilePath]
  -- ^ Import directories
  -> HDL
  -- ^ HDL target
  -> Maybe GHC.DynFlags
  -- ^ GHC flags
  -> FilePath
  -- ^ Module
  -> IO [DataRepr']
customRepresentations _importDirs hdl dflagsM modName = do
  -- Load module with custom representations in it, and convert to Clash types
  (bindings, clsOps, unlocatable, _, _, _, _) <- loadModules hdl modName dflagsM True
  let mkBindings' = mkBindings HashMap.empty bindings clsOps unlocatable
  let ((bindingsMap, _), _) = State.runState mkBindings' emptyGHC2CoreState

  -- TmName,Type,SrcSpan,InlineSpec,Term
  let
    reprs =
      filter
        (\(_, typ, srcspan, _, _) -> isDataRepr srcspan typ)
        (HashMap.elems bindingsMap)

  return $ map (parseDataRepr bindingsMap) reprs

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
        , PrimMap Text.Text  -- The primitives found in '.' and 'primDir'
        , [DataRepr']
        )
generateBindings primDirs importDirs hdl modName dflagsM = do
  (bindings,clsOps,unlocatable,fiEnvs,topEntities,pFP,reprs) <- loadModules hdl modName dflagsM False
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

  return (retypedBindings,allTcCache,tupTcCache,topEntities'',primMap,reprs)

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
  :: PrimMap a
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
