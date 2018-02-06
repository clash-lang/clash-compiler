{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2818, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.GHC.GHC2Core
  ( GHC2CoreState
  , tyConMap
  , coreToTerm
  , coreToId
  , coreToName
  , modNameM
  , qualfiedNameString
  , makeAllTyCons
  , emptyGHC2CoreState
  )
where

-- External Modules
import           Control.Lens                ((^.), (%~), (&), (%=))
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Reader  (ReaderT)
import qualified Control.Monad.Trans.Reader  as Reader
import           Control.Monad.State         (State)
import qualified Control.Monad.State.Lazy    as State
import qualified Data.ByteString.Char8       as Char8
import           Data.Hashable               (Hashable (..))
import           Data.HashMap.Lazy           (HashMap)
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.HashMap.Strict         as HSM
import           Data.Maybe                  (catMaybes,fromMaybe,listToMaybe)
import           Data.Text                   (isInfixOf,pack)
import qualified Data.Traversable            as T
import           Unbound.Generics.LocallyNameless
  (bind, embed, rebind, rec, runFreshM, unbind, unembed)
import qualified Unbound.Generics.LocallyNameless     as Unbound

-- GHC API
import CoAxiom    (CoAxiom (co_ax_branches), CoAxBranch (cab_lhs,cab_rhs),
                   fromBranches)
import Coercion   (coercionType,coercionKind)
import CoreFVs    (exprSomeFreeVars)
import CoreSyn
  (AltCon (..), Bind (..), CoreExpr, Expr (..), Unfolding (..), collectArgs,
   rhssOfAlts, unfoldingTemplate)
import DataCon    (DataCon, dataConExTyVars,
                   dataConName, dataConRepArgTys,
                   dataConTag, dataConTyCon,
                   dataConUnivTyVars, dataConWorkId)
import DynFlags   (unsafeGlobalDynFlags)
import FamInstEnv (FamInst (..), FamInstEnvs,
                   familyInstances)
import FastString (unpackFS)
import Id         (isDataConId_maybe)
import IdInfo     (IdDetails (..), unfoldingInfo)
import Literal    (Literal (..))
#if MIN_VERSION_ghc(8,6,0)
import Literal    (LitNumType (..))
#endif
import Module     (moduleName, moduleNameString)
import Name       (Name, nameModule_maybe,
                   nameOccName, nameUnique, getSrcSpan)
import PrelNames  (tYPETyConKey)
import OccName    (occNameString)
import Outputable (showPpr)
import Pair       (Pair (..))
import SrcLoc     (isGoodSrcSpan)
import TyCon      (AlgTyConRhs (..), TyCon, tyConName,
                   algTyConRhs, isAlgTyCon, isFamilyTyCon,
                   isFunTyCon, isNewTyCon,
                   isPrimTyCon, isTupleTyCon,
                   isClosedSynFamilyTyConWithAxiom_maybe,
                   expandSynTyCon_maybe,
                   tyConArity,
                   tyConDataCons, tyConKind,
                   tyConName, tyConUnique)
import Type       (mkTvSubstPrs, substTy, coreView)
#if MIN_VERSION_ghc(8,2,0)
import TyCoRep    (Coercion (..), TyLit (..), Type (..))
#else
import TyCoRep    (Coercion (..), TyBinder (..), TyLit (..), Type (..))
#endif
import Unique     (Uniquable (..), Unique, getKey, hasKey)
import Var        (Id, TyVar, Var, idDetails,
                   isTyVar, varName, varType,
                   varUnique, idInfo)
#if MIN_VERSION_ghc(8,2,0)
import Var        (TyVarBndr (..))
#endif
import VarSet     (isEmptyVarSet)

-- Local imports
import qualified Clash.Core.DataCon          as C
import qualified Clash.Core.Literal          as C
import qualified Clash.Core.Name             as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.TyCon            as C
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Var              as C
import           Clash.Driver.Types
import           Clash.Primitives.Types
import           Clash.Util

instance Hashable Name where
  hashWithSalt s = hashWithSalt s . getKey . nameUnique

data GHC2CoreState
  = GHC2CoreState
  { _tyConMap :: HashMap C.TyConOccName TyCon
  , _nameMap  :: HashMap Name String
  }

makeLenses ''GHC2CoreState

emptyGHC2CoreState :: GHC2CoreState
emptyGHC2CoreState = GHC2CoreState HSM.empty HSM.empty

makeAllTyCons
  :: GHC2CoreState
  -> FamInstEnvs
  -> HashMap C.TyConOccName C.TyCon
makeAllTyCons hm fiEnvs = go hm hm
  where
    go old new
        | HSM.null (new ^. tyConMap) = HSM.empty
        | otherwise                  = tcm `HSM.union` tcm'
      where
        (tcm,old') = State.runState (T.mapM (makeTyCon fiEnvs) (new ^. tyConMap)) old
        tcm'       = go old' (old' & tyConMap %~ (`HSM.difference` (old ^. tyConMap)))

makeTyCon :: FamInstEnvs
          -> TyCon
          -> State GHC2CoreState C.TyCon
makeTyCon fiEnvs tc = tycon
  where
    tycon
      | isFamilyTyCon tc    = mkFunTyCon
      | isTupleTyCon tc     = mkTupleTyCon
      | isAlgTyCon tc       = mkAlgTyCon
      | isPrimTyCon tc      = mkPrimTyCon
      | tc `hasKey` tYPETyConKey = mkSuperKindTyCon
      | otherwise           = mkVoidTyCon
      where
        tcArity = tyConArity tc

        mkAlgTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          tcRhsM <- makeAlgTyConRhs $ algTyConRhs tc
          case tcRhsM of
            Just tcRhs ->
              return
                C.AlgTyCon
                { C.tyConName   = tcName
                , C.tyConKind   = tcKind
                , C.tyConArity  = tcArity
                , C.algTcRhs    = tcRhs
                }
            Nothing -> return (C.PrimTyCon tcName tcKind tcArity)

        mkFunTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          substs <- case isClosedSynFamilyTyConWithAxiom_maybe tc of
            Nothing -> let instances = familyInstances fiEnvs tc
                       in  mapM famInstToSubst instances
            Just cx -> let bx = fromBranches (co_ax_branches cx)
                       in  mapM (\b -> (,) <$> mapM coreToType (cab_lhs b)
                                           <*> coreToType (cab_rhs b))
                                bx
          return
            C.FunTyCon
            { C.tyConName  = tcName
            , C.tyConKind  = tcKind
            , C.tyConArity = tcArity
            , C.tyConSubst = substs
            }

        mkTupleTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          tcDc   <- fmap (C.DataTyCon . (:[])) . coreToDataCon . head . tyConDataCons $ tc
          return
            C.AlgTyCon
            { C.tyConName   = tcName
            , C.tyConKind   = tcKind
            , C.tyConArity  = tcArity
            , C.algTcRhs    = tcDc
            }

        mkPrimTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          return
            C.PrimTyCon
            { C.tyConName    = tcName
            , C.tyConKind    = tcKind
            , C.tyConArity   = tcArity
            }

        mkSuperKindTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          return C.SuperKindTyCon
                   { C.tyConName = tcName
                   }

        mkVoidTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          return (C.PrimTyCon tcName tcKind tcArity)

        famInstToSubst :: FamInst -> State GHC2CoreState ([C.Type],C.Type)
        famInstToSubst fi = do
          tys <- mapM coreToType  (fi_tys fi)
          ty  <- coreToType (fi_rhs fi)
          return (tys,ty)

makeAlgTyConRhs :: AlgTyConRhs
                -> State GHC2CoreState (Maybe C.AlgTyConRhs)
makeAlgTyConRhs algTcRhs = case algTcRhs of
#if MIN_VERSION_ghc(8,6,0)
  DataTyCon dcs _ _ -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
#else
  DataTyCon dcs _ -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
#endif
#if MIN_VERSION_ghc(8,2,0)
#if MIN_VERSION_ghc(8,6,0)
  SumTyCon dcs _ -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
#else
  SumTyCon dcs -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
#endif
#endif
  NewTyCon dc _ (rhsTvs,rhsEtad) _ -> Just <$> (C.NewTyCon <$> coreToDataCon dc
                                                           <*> ((,) <$> mapM coreToVar rhsTvs
                                                                    <*> coreToType rhsEtad
                                                               )
                                               )
  AbstractTyCon {} -> return Nothing
  TupleTyCon {}    -> error "Cannot handle tuple tycons"

coreToTerm
  :: PrimMap (Primitive a b c)
  -> [Var]
  -> SrcSpan
  -> CoreExpr
  -> State GHC2CoreState C.Term
coreToTerm primMap unlocs srcsp coreExpr = Reader.runReaderT (term coreExpr) srcsp
  where
    term :: CoreExpr -> ReaderT SrcSpan (State GHC2CoreState) C.Term
    term e
      | (Var x,args) <- collectArgs e
      , let nm = State.evalState (qualfiedNameString (varName x)) emptyGHC2CoreState
      = go nm args
      | otherwise
      = term' e
      where
        -- Remove most Signal transformers
        go "Clash.Signal.Internal.mapSignal#"  args
          | length args == 5
          = term (App (args!!3) (args!!4))
        go "Clash.Signal.Internal.signal#"     args
          | length args == 3
          = term (args!!2)
        go "Clash.Signal.Internal.appSignal#"  args
          | length args == 5
          = term (App (args!!3) (args!!4))
        go "Clash.Signal.Internal.joinSignal#" args
          | length args == 3
          = term (args!!2)
        go "Clash.Signal.Bundle.vecBundle#"    args
          | length args == 4
          = term (args!!3)
        --- Remove `$`
        go "GHC.Base.$"                        args
          | length args == 5
          = term (App (args!!3) (args!!4))
        -- Remove most CallStack logic
        go "GHC.Stack.Types.PushCallStack"     args = term (last args)
        go "GHC.Stack.Types.FreezeCallStack"   args = term (last args)
        go "GHC.Stack.withFrozenCallStack"     args
          | length args == 3
          = term (App (args!!2) (args!!1))
        go _ _ = term' e
    term' (Var x)                 = do
      srcsp' <- Reader.ask
      lift (var srcsp' x)
    term' (Lit l)                 = return $ C.Literal (coreToLiteral l)
    term' (App eFun (Type tyArg)) = C.TyApp <$> term eFun <*> lift (coreToType tyArg)
    term' (App eFun eArg)         = C.App   <$> term eFun <*> term eArg
    term' (Lam x e) | isTyVar x   = C.TyLam <$> (bind <$> lift (coreToTyVar x) <*> addUsefull (getSrcSpan x) (term e))
                    | otherwise   = C.Lam   <$> (bind <$> lift (coreToId x)    <*> addUsefull (getSrcSpan x) (term e))
    term' (Let (NonRec x e1) e2)  = do
      x'  <- lift (coreToId x)
      e1' <- addUsefull (getSrcSpan x) (term e1)
      e2' <- term e2
      return $ C.Letrec $ bind (rec [(x', embed e1')]) e2'

    term' (Let (Rec xes) e) = do
      xes' <- mapM (\(x,b) -> (,) <$> lift (coreToId x)
                                  <*> addUsefull (getSrcSpan x)
                                                 (embed <$> term b))
                   xes
      e'   <- term e
      return $ C.Letrec $ bind (rec xes') e'

    term' (Case _ _ ty [])  = C.Prim (pack "EmptyCase") <$> lift (coreToType ty)
    term' (Case e b ty alts) = do
     let usesBndr = any ( not . isEmptyVarSet . exprSomeFreeVars (== b))
                  $ rhssOfAlts alts
     b'  <- lift (coreToId b)
     e'  <- addUsefull (getSrcSpan b) (term e)
     ty' <- lift (coreToType ty)
     let caseTerm v =
             C.Case v ty' <$> mapM (addUsefull (getSrcSpan b) . alt) alts
     if usesBndr
      then do
        ct <- caseTerm (C.Var (unembed $ C.varType b') (C.varName b'))
        return $ C.Letrec $ bind (rec [(b',embed e')]) ct
      else caseTerm e'

    term' (Cast e co) = do
      let (Pair ty1 ty2) = coercionKind co
      hasPrimCoM <- lift (hasPrimCo co)
      ty1_I <- lift (isIntegerTy ty1)
      ty2_I <- lift (isIntegerTy ty2)
      case hasPrimCoM of
        Just _ | ty1_I || ty2_I
          -> C.Cast <$> term e <*> lift (coreToType ty1) <*> lift (coreToType ty2)
        _ -> term e
    term' (Tick _ e)        = term e
    term' (Type t)          = C.Prim (pack "_TY_") <$> lift (coreToType t)
    term' (Coercion co)     = C.Prim (pack "_CO_") <$> lift (coreToType (coercionType co))

    var srcsp' x = do
        xVar   <- coreToVar x
        xPrim  <- coreToPrimVar x
        let xNameS = pack $ C.name2String xPrim
        xType  <- coreToType (varType x)
        case isDataConId_maybe x of
          Just dc -> case HashMap.lookup xNameS primMap of
            Just _  -> return $ C.Prim xNameS xType
            Nothing -> if isDataConWrapId x && not (isNewTyCon (dataConTyCon dc))
              then let xInfo = idInfo x
                       unfolding = unfoldingInfo xInfo
                   in  case unfolding of
                          CoreUnfolding {} -> Reader.runReaderT (term (unfoldingTemplate unfolding)) srcsp'
                          NoUnfolding -> error ("No unfolding for DC wrapper: " ++ showPpr unsafeGlobalDynFlags x)
                          _ -> error ("Unexpected unfolding for DC wrapper: " ++ showPpr unsafeGlobalDynFlags x)
              else C.Data <$> coreToDataCon dc
          Nothing -> case HashMap.lookup xNameS primMap of
            Just (Primitive f _)
              | f == pack "Clash.Signal.Internal.mapSignal#" -> return (mapSignalTerm xType)
              | f == pack "Clash.Signal.Internal.signal#"    -> return (signalTerm xType)
              | f == pack "Clash.Signal.Internal.appSignal#" -> return (appSignalTerm xType)
              | f == pack "Clash.Signal.Internal.traverse#"  -> return (traverseTerm xType)
              | f == pack "Clash.Signal.Internal.joinSignal#" -> return (joinTerm xType)
              | f == pack "Clash.Signal.Bundle.vecBundle#"   -> return (vecUnwrapTerm xType)
              | f == pack "GHC.Base.$"                       -> return (dollarTerm xType)
              | f == pack "GHC.Stack.withFrozenCallStack"    -> return (withFrozenCallStackTerm xType)
              | f == pack "GHC.Magic.lazy"                   -> return (idTerm xType)
              | f == pack "GHC.Magic.runRW#"                 -> return (runRWTerm xType)
              | otherwise                                    -> return (C.Prim xNameS xType)
            Just (BlackBox {}) ->
              return $ C.Prim xNameS xType
            Just (BlackBoxHaskell {}) ->
              return $ C.Prim xNameS xType
            Nothing
              | x `elem` unlocs -> return (C.Prim xNameS xType)
              | pack "$cshow" `isInfixOf` xNameS -> return (C.Prim xNameS xType)
              | otherwise       -> return  (C.Var xType xVar)

    alt (DEFAULT   , _ , e) = bind C.DefaultPat <$> term e
    alt (LitAlt l  , _ , e) = bind (C.LitPat . embed $ coreToLiteral l) <$> term e
    alt (DataAlt dc, xs, e) = case span isTyVar xs of
      (tyvs,tmvs) -> bind <$> (C.DataPat . embed <$>
                                lift (coreToDataCon dc) <*>
                                (rebind <$>
                                  lift (mapM coreToTyVar tyvs) <*>
                                  lift (mapM coreToId tmvs))) <*>
                              term e

    coreToLiteral :: Literal
                  -> C.Literal
    coreToLiteral l = case l of
      MachStr    fs  -> C.StringLiteral (Char8.unpack fs)
      MachChar   c   -> C.CharLiteral c
#if MIN_VERSION_ghc(8,6,0)
      LitNumber lt i _ -> case lt of
        LitNumInteger -> C.IntegerLiteral i
        LitNumNatural -> C.NaturalLiteral i
        LitNumInt     -> C.IntLiteral i
        LitNumInt64   -> C.IntLiteral i
        LitNumWord    -> C.WordLiteral i
        LitNumWord64  -> C.WordLiteral i
#else
      MachInt    i   -> C.IntLiteral i
      MachInt64  i   -> C.IntLiteral i
      MachWord   i   -> C.WordLiteral i
      MachWord64 i   -> C.WordLiteral i
      LitInteger i _ -> C.IntegerLiteral i
#endif
      MachFloat r    -> C.FloatLiteral r
      MachDouble r   -> C.DoubleLiteral r
      MachNullAddr   -> C.StringLiteral []
      MachLabel fs _ _ -> C.StringLiteral (unpackFS fs)

addUsefull :: SrcSpan -> ReaderT SrcSpan (State GHC2CoreState) a
           -> ReaderT SrcSpan (State GHC2CoreState) a
addUsefull x = Reader.local (\r -> if isGoodSrcSpan x then x else r)

isIntegerTy :: Type -> State GHC2CoreState Bool
isIntegerTy (TyConApp tc []) = do
  tcNm <- qualfiedNameString (tyConName tc)
  return (tcNm == "GHC.Integer.Type.Integer")
isIntegerTy _ = return False

hasPrimCo :: Coercion -> State GHC2CoreState (Maybe Type)
hasPrimCo (TyConAppCo _ _ coers) = do
  tcs <- catMaybes <$> mapM hasPrimCo coers
  return (listToMaybe tcs)

hasPrimCo (AppCo co1 co2) = do
  tc1M <- hasPrimCo co1
  case tc1M of
    Just _ -> return tc1M
    _ -> hasPrimCo co2
hasPrimCo (ForAllCo _ _ co) = hasPrimCo co

hasPrimCo co@(AxiomInstCo _ _ coers) = do
    let (Pair ty1 _) = coercionKind co
    ty1PM <- isPrimTc ty1
    if ty1PM
       then return (Just ty1)
       else do
         tcs <- catMaybes <$> mapM hasPrimCo coers
         return (listToMaybe tcs)
  where
    isPrimTc (TyConApp tc _) = do
      tcNm <- qualfiedNameString (tyConName tc)
      return (tcNm `elem` ["Clash.Sized.Internal.BitVector.Bit"
                          ,"Clash.Sized.Internal.BitVector.BitVector"
                          ,"Clash.Sized.Internal.Index.Index"
                          ,"Clash.Sized.Internal.Signed.Signed"
                          ,"Clash.Sized.Internal.Unsigned.Unsigned"
                          ])
    isPrimTc _ = return False

hasPrimCo (SymCo co) = hasPrimCo co

hasPrimCo (TransCo co1 co2) = do
  tc1M <- hasPrimCo co1
  case tc1M of
    Just _ -> return tc1M
    _ -> hasPrimCo co2

hasPrimCo (AxiomRuleCo _ coers) = do
  tcs <- catMaybes <$> mapM hasPrimCo coers
  return (listToMaybe tcs)

#if MIN_VERSION_ghc(8,6,0)
hasPrimCo (NthCo _ _ co)  = hasPrimCo co
#else
hasPrimCo (NthCo _ co)  = hasPrimCo co
#endif
hasPrimCo (LRCo _ co)   = hasPrimCo co
hasPrimCo (InstCo co _) = hasPrimCo co
hasPrimCo (SubCo co)    = hasPrimCo co

hasPrimCo _ = return Nothing

coreToDataCon :: DataCon
              -> State GHC2CoreState C.DataCon
coreToDataCon dc = do
    repTys <- mapM coreToType (dataConRepArgTys dc)
    dcTy   <- coreToType (varType $ dataConWorkId dc)
    mkDc dcTy repTys
  where
    mkDc dcTy repTys = do
      nm   <- coreToName dataConName getUnique qualfiedNameString dc
      uTvs <- mapM coreToVar (dataConUnivTyVars dc)
      eTvs <- mapM coreToVar (dataConExTyVars dc)
      return $ C.MkData
             { C.dcName       = nm
             , C.dcTag        = dataConTag dc
             , C.dcType       = dcTy
             , C.dcArgTys     = repTys
             , C.dcUnivTyVars = uTvs
             , C.dcExtTyVars  = eTvs
             }

typeConstructorToString
  :: TyCon
  -> State GHC2CoreState String
typeConstructorToString constructor =
    C.name2String <$> coreToName tyConName tyConUnique qualfiedNameString constructor

_ATTR_NAME :: String
_ATTR_NAME = "Clash.Annotations.SynthesisAttributes.Attr"

-- | Flatten a list type structure to a list of types.
listTypeToListOfTypes :: Type -> [Type]
listTypeToListOfTypes (TyConApp _ [_, a, as]) = a : listTypeToListOfTypes as
listTypeToListOfTypes _                       = []

-- | Try to determine boolean value by looking at constructor name of type.
boolTypeToBool :: Type -> State GHC2CoreState Bool
boolTypeToBool (TyConApp constructor _args) = do
  constructorName <- typeConstructorToString constructor
  return $ case constructorName of
    "GHC.Types.True"  -> True
    "GHC.Types.False" -> False
    _ -> error $ "Expected boolean constructor, got:" ++ constructorName
boolTypeToBool s =
  error $ unwords [ "Could not unpack given type to bool:"
                  , showPpr unsafeGlobalDynFlags s ]

-- | Returns string of (LitTy (StrTyLit s)) construction.
tyLitToString :: Type -> String
tyLitToString (LitTy (StrTyLit s)) = unpackFS s
tyLitToString s = error $ unwords [ "Could not unpack given type to string:"
                                  , showPpr unsafeGlobalDynFlags s ]

-- | Returns integer of (LitTy (NumTyLit n)) construction.
tyLitToInteger :: Type -> Integer
tyLitToInteger (LitTy (NumTyLit n)) = n
tyLitToInteger s = error $ unwords [ "Could not unpack given type to integer:"
                                   , showPpr unsafeGlobalDynFlags s ]

-- | Try to interpret a Type as an Attr
coreToAttr
  :: Type
  -> State GHC2CoreState C.Attr'
coreToAttr (TyConApp ty args) = do
  let key   = args !! 0
  let value = args !! 1
  name' <- typeConstructorToString ty
  case name' of
    "Clash.Annotations.SynthesisAttributes.StringAttr" ->
        return $ C.StringAttr' (tyLitToString key) (tyLitToString value)
    "Clash.Annotations.SynthesisAttributes.IntegerAttr" ->
        return $ C.IntegerAttr' (tyLitToString key) (tyLitToInteger value)
    "Clash.Annotations.SynthesisAttributes.BoolAttr" -> do
        bool <- boolTypeToBool value
        return $ C.BoolAttr' (tyLitToString key) bool
    "Clash.Annotations.SynthesisAttributes.Attr" ->
        return $ C.Attr' (tyLitToString key)
    _ ->
        error $ unwords [ "Expected StringAttr, IntegerAttr, BoolAttr or Attr"
                        , "constructor, got:" ++ name' ]

coreToAttr t =
  error $ unwords [ "Expected type constructor (TyConApp), but got:"
                  , showPpr unsafeGlobalDynFlags t ]

coreToAttrs'
    :: [Type]
    -> State GHC2CoreState [C.Attr']
coreToAttrs' [annotationType, _star, realType, attributes] = allAttrs
 where
  allAttrs = (++) <$> attrs <*> subAttrs

  subAttrs =
    coreToAttrs realType

  attrs =
    case annotationType of
      TyConApp ty [TyConApp ty' _args'] -> do
        name'  <- typeConstructorToString ty
        name'' <- typeConstructorToString ty'

        let result | name' == "GHC.Types.[]" && name'' == _ATTR_NAME =
                      -- List of attributes
                      sequence $ map coreToAttr (listTypeToListOfTypes attributes)
                   | name' == "GHC.Types.[]" =
                      -- List, but uknown types
                      error $ $(curLoc) ++ unwords [ "Annotate expects an"
                                                   , "Attr or a list of"
                                                   , "Attr's, but got a list"
                                                   , "of:", name'']
                   | otherwise =
                        -- Some unknown nested type
                      error $ $(curLoc) ++ unwords [ "Annotate expects an"
                                                   , "Attr or a list of"
                                                   , "Attr's, but got:"
                                                   , name' ]

        result

      TyConApp ty _args -> do
        name' <- typeConstructorToString ty
        if name' == _ATTR_NAME
          then
            -- Single annotation
            sequence [coreToAttr attributes]
          else do
            -- Annotation to something we don't recognize (not a list,
            -- nor an Attr)
            tystr <- typeConstructorToString ty
            error $ unwords [ "Annotate expects an Attr or a list of Attr's,"
                            , "but got:", tystr ]
      _ ->
        error $ $(curLoc) ++ unwords [ "Expected TyConApp, not:"
                                     , showPpr unsafeGlobalDynFlags annotationType]

coreToAttrs' illegal =
  error $ "Expected list with four items (as Annotate has four arguments), but got: "
      ++ show (map (showPpr unsafeGlobalDynFlags) illegal)

-- | If this type has an annotate type synonym, return list of attributes.
coreToAttrs
  :: Type
  -> State GHC2CoreState [C.Attr']
coreToAttrs (TyConApp tycon kindsOrTypes) = do
  name' <- typeConstructorToString tycon

  if name' == "Clash.Annotations.SynthesisAttributes.Annotate"
    then
      coreToAttrs' kindsOrTypes
    else
      return []

coreToAttrs _ =
    return []

-- | Wrap given type in annotation if is annotated using the contructs
-- defined in Clash.Annotations.SynthesisAttributes.
annotateType
  :: Type
  -> C.Type
  -> State GHC2CoreState C.Type
annotateType ty cty = do
  attrs <- coreToAttrs ty
  case attrs of
    [] -> return cty
    _  -> return $ C.AnnType attrs cty

-- | Converts GHC Type to a Clash Type. Strips newtypes and signals, with the
-- exception of newtypes used as annotations (see: SynthesisAttributes).
coreToType
  :: Type
  -> State GHC2CoreState C.Type
coreToType ty = ty'' >>= annotateType ty
  where
    ty'' =
      case coreView ty of
        Just ty' -> coreToType ty'
        Nothing  -> coreToType' ty

coreToType'
  :: Type
  -> State GHC2CoreState C.Type
coreToType' (TyVarTy tv) = C.VarTy <$> coreToType (varType tv) <*> (coreToVar tv)
coreToType' (TyConApp tc args)
  | isFunTyCon tc = foldl C.AppTy (C.ConstTy C.Arrow) <$> mapM coreToType args
  | otherwise     = case expandSynTyCon_maybe tc args of
                      Just (substs,synTy,remArgs) -> do
                        let substs' = mkTvSubstPrs substs
                            synTy'  = substTy substs' synTy
                        foldl C.AppTy <$> coreToType synTy' <*> mapM coreToType remArgs
                      _ -> do
                        tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
                        tyConMap %= (HSM.insert (C.nameOcc tcName) tc)
                        C.mkTyConApp <$> (pure tcName) <*> mapM coreToType args
#if MIN_VERSION_ghc(8,2,0)
coreToType' (ForAllTy (TvBndr tv _) ty) = C.ForAllTy <$> (bind <$> coreToTyVar tv <*> coreToType ty)
coreToType' (FunTy ty1 ty2)             = C.mkFunTy <$> coreToType ty1 <*> coreToType ty2
#else
coreToType' (ForAllTy (Named tv _) ty) = C.ForAllTy <$> (bind <$> coreToTyVar tv <*> coreToType ty)
coreToType' (ForAllTy (Anon ty1) ty2)  = C.mkFunTy <$> coreToType ty1 <*> coreToType ty2
#endif
coreToType' (LitTy tyLit)    = return $ C.LitTy (coreToTyLit tyLit)
coreToType' (AppTy ty1 ty2)  = C.AppTy <$> coreToType ty1 <*> coreToType' ty2
coreToType' t@(CastTy _ _)   = error ("Cannot handle CastTy " ++ showPpr unsafeGlobalDynFlags t)
coreToType' t@(CoercionTy _) = error ("Cannot handle CoercionTy " ++ showPpr unsafeGlobalDynFlags t)

coreToTyLit :: TyLit
            -> C.LitTy
coreToTyLit (NumTyLit i) = C.NumTy (fromInteger i)
coreToTyLit (StrTyLit s) = C.SymTy (unpackFS s)

coreToTyVar :: TyVar
            -> State GHC2CoreState C.TyVar
coreToTyVar tv =
  C.TyVar <$> (coreToVar tv) <*> (embed <$> coreToType (varType tv))

coreToId :: Id
         -> State GHC2CoreState C.Id
coreToId i =
  C.Id <$> (coreToVar i) <*> (embed <$> coreToType (varType i))

coreToVar :: Var
          -> State GHC2CoreState (C.Name a)
coreToVar = coreToName varName varUnique qualfiedNameStringM

coreToPrimVar :: Var
              -> State GHC2CoreState (C.Name C.Term)
coreToPrimVar = coreToName varName varUnique qualfiedNameString

coreToName :: (b -> Name)
           -> (b -> Unique)
           -> (Name -> State GHC2CoreState String)
           -> b
           -> State GHC2CoreState (C.Name a)
coreToName toName toUnique toString v = do
  ns <- toString (toName v)
  let nm  = Unbound.makeName ns (toInteger . getKey . toUnique $ v)
      loc = getSrcSpan (toName v)
  return (C.Name C.User nm loc)

qualfiedNameString :: Name
                   -> State GHC2CoreState String
qualfiedNameString n = makeCached n nameMap
                     $ return (fromMaybe "_INTERNAL_" (modNameM n) ++ ('.':occName))
  where
    occName = occNameString $ nameOccName n

qualfiedNameStringM :: Name
                    -> State GHC2CoreState String
qualfiedNameStringM n = makeCached n nameMap
                      $ return (maybe occName (\modName -> modName ++ ('.':occName)) (modNameM n))
  where
    occName = occNameString $ nameOccName n

modNameM :: Name
         -> Maybe String
modNameM n = do
      module_ <- nameModule_maybe n
      let moduleNm = moduleName module_
      return (moduleNameString moduleNm)

-- | Given the type:
--
-- @forall a. forall b. forall clk. (a -> b) -> Signal' clk a -> Signal' clk b@
--
-- Generate the term:
--
-- @
-- /\(a:*)./\(b:*)./\(clk:Clock).\(f : (Signal' clk a -> Signal' clk b)).
-- \(x : Signal' clk a).f x
-- @
mapSignalTerm :: C.Type
              -> C.Term
mapSignalTerm (C.ForAllTy tvATy) =
    C.TyLam (bind aTV (
    C.TyLam (bind bTV (
    C.TyLam (bind clkTV (
    C.Lam   (bind fId (
    C.Lam   (bind xId (
    C.App (C.Var fTy fName) (C.Var aTy xName)))))))))))
  where
    (aTV,bTV,clkTV,funTy) = runFreshM $ do
      { (aTV',C.ForAllTy tvBTy)   <- unbind tvATy
      ; (bTV',C.ForAllTy tvClkTy) <- unbind tvBTy
      ; (clkTV',funTy')           <- unbind tvClkTy
      ; return (aTV',bTV',clkTV',funTy')
      }
    (C.FunTy _ funTy'') = C.tyView funTy
    (C.FunTy aTy bTy)   = C.tyView funTy''
    fName = C.string2SystemName "f"
    xName = C.string2SystemName "x"
    fTy   = C.mkFunTy aTy bTy
    fId   = C.Id fName (embed fTy)
    xId   = C.Id xName (embed aTy)

mapSignalTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @forall a. forall clk. a -> Signal' clk a@
--
-- Generate the term
--
-- @/\(a:*)./\(clk:Clock).\(x:Signal' clk a).x@
signalTerm :: C.Type
           -> C.Term
signalTerm (C.ForAllTy tvATy) =
    C.TyLam (bind aTV (
    C.TyLam (bind clkTV (
    C.Lam   (bind xId (
    C.Var   aTy xName))))))
  where
    (aTV,clkTV,funTy) = runFreshM $ do
      { (aTV', C.ForAllTy tvClkTy) <- unbind tvATy
      ; (clkTV', funTy')           <- unbind tvClkTy
      ; return (aTV',clkTV',funTy')
      }
    (C.FunTy _ aTy) = C.tyView funTy
    xName = C.string2SystemName "x"
    xId   = C.Id xName (embed aTy)

signalTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @
-- forall clk. forall a. forall b. Signal' clk (a -> b) -> Signal' clk a ->
-- Signal' clk b
-- @
--
-- Generate the term:
--
-- @
-- /\(clk:Clock)./\(a:*)./\(b:*).\(f : (Signal' clk a -> Signal' clk b)).
-- \(x : Signal' clk a).f x
-- @
appSignalTerm :: C.Type
              -> C.Term
appSignalTerm (C.ForAllTy tvClkTy) =
    C.TyLam (bind clkTV (
    C.TyLam (bind aTV (
    C.TyLam (bind bTV (
    C.Lam   (bind fId (
    C.Lam   (bind xId (
    C.App (C.Var fTy fName) (C.Var aTy xName)))))))))))
  where
    (clkTV,aTV,bTV,funTy) = runFreshM $ do
      { (clkTV',C.ForAllTy tvATy) <- unbind tvClkTy
      ; (aTV',C.ForAllTy tvBTy)   <- unbind tvATy
      ; (bTV',funTy')           <- unbind tvBTy
      ; return (clkTV',aTV',bTV',funTy')
      }
    (C.FunTy _ funTy'') = C.tyView funTy
    (C.FunTy aTy bTy)   = C.tyView funTy''
    fName = C.string2SystemName "f"
    xName = C.string2SystemName "x"
    fTy   = C.mkFunTy aTy bTy
    fId   = C.Id fName (embed fTy)
    xId   = C.Id xName (embed aTy)

appSignalTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @
-- forall t.forall n.forall a.Vec n (Signal' t a) ->
-- Signal' t (Vec n a)
-- @
--
-- Generate the term:
--
-- @
-- /\(t:Clock)./\(n:Nat)./\(a:*).\(vs:Signal' t (Vec n a)).vs
-- @
vecUnwrapTerm :: C.Type
              -> C.Term
vecUnwrapTerm (C.ForAllTy tvTTy) =
    C.TyLam (bind tTV (
    C.TyLam (bind nTV (
    C.TyLam (bind aTV (
    C.Lam   (bind vsId (
    C.Var vsTy vsName))))))))
  where
    (tTV,nTV,aTV,funTy) = runFreshM $ do
      { (tTV',C.ForAllTy tvNTy) <- unbind tvTTy
      ; (nTV',C.ForAllTy tvATy) <- unbind tvNTy
      ; (aTV',funTy')           <- unbind tvATy
      ; return (tTV',nTV',aTV',funTy')
      }
    (C.FunTy _ vsTy) = C.tyView funTy
    vsName           = C.string2SystemName "vs"
    vsId             = C.Id vsName   (embed vsTy)

vecUnwrapTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @
-- forall f.forall a.forall b.forall clk.Applicative f => (a -> f b) ->
-- CSignal clk a -> f (Signal' clk b)
-- @
--
-- Generate the term:
--
-- @
-- /\(f:* -> *)./\(a:*)./\(b:*)./\(clk:Clock).\(dict:Applicative f).
-- \(g:a -> f b).\(x:Signal' clk a).g x
-- @
traverseTerm :: C.Type
             -> C.Term
traverseTerm (C.ForAllTy tvFTy) =
    C.TyLam (bind fTV (
    C.TyLam (bind aTV (
    C.TyLam (bind bTV (
    C.TyLam (bind clkTV (
    C.Lam   (bind dictId (
    C.Lam   (bind gId (
    C.Lam   (bind xId (
    C.App (C.Var gTy gName) (C.Var xTy xName)))))))))))))))
  where
    (fTV,aTV,bTV,clkTV,funTy) = runFreshM $ do
      { (fTV',C.ForAllTy tvATy) <- unbind tvFTy
      ; (aTV',C.ForAllTy tvBTy) <- unbind tvATy
      ; (bTV',C.ForAllTy tvClkTy) <- unbind tvBTy
      ; (clkTV',funTy') <- unbind tvClkTy
      ; return (fTV',aTV',bTV',clkTV',funTy')
      }
    (C.FunTy dictTy funTy1) = C.tyView funTy
    (C.FunTy gTy    funTy2) = C.tyView funTy1
    (C.FunTy xTy    _)      = C.tyView funTy2
    dictName = C.string2SystemName "dict"
    gName    = C.string2SystemName "g"
    xName    = C.string2SystemName "x"
    dictId   = C.Id dictName (embed dictTy)
    gId      = C.Id gName (embed gTy)
    xId      = C.Id xName (embed xTy)

traverseTerm ty = error $ $(curLoc) ++ show ty

-- ∀ (r :: GHC.Types.RuntimeRep)
--   (a :: GHC.Prim.TYPE GHC.Types.PtrRepLifted)
--   (b :: GHC.Prim.TYPE r).
-- (a -> b) -> a -> b


-- | Given the type:
--
-- @forall (r :: Rep) (a :: TYPE Lifted) (b :: TYPE r). (a -> b) -> a -> b@
--
-- Generate the term:
--
-- @/\(r:Rep)/\(a:TYPE Lifted)./\(b:TYPE r).\(f : (a -> b)).\(x : a).f x@
dollarTerm :: C.Type
           -> C.Term
dollarTerm (C.ForAllTy tvRTy) =
    C.TyLam (bind rTV (
    C.TyLam (bind aTV (
    C.TyLam (bind bTV (
    C.Lam   (bind fId (
    C.Lam   (bind xId (
    C.App (C.Var fTy fName) (C.Var aTy xName)))))))))))
  where
    (rTV,aTV,bTV,funTy) = runFreshM $ do
      { (rTV',C.ForAllTy tvATy) <- unbind tvRTy
      ; (aTV',C.ForAllTy tvBTy) <- unbind tvATy
      ; (bTV',funTy')           <- unbind tvBTy
      ; return (rTV',aTV',bTV',funTy')
      }
    (C.FunTy fTy funTy'') = C.tyView funTy
    (C.FunTy aTy _)       = C.tyView funTy''
    fName = C.string2SystemName "f"
    xName = C.string2SystemName "x"
    fId   = C.Id fName (embed fTy)
    xId   = C.Id xName (embed aTy)

dollarTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @forall a. forall clk. Signal' clk (Signal' clk a) -> Signal' clk a@
--
-- Generate the term
--
-- @/\(a:*)./\(clk:Clock).\(x:Signal' clk a).x@
joinTerm :: C.Type
         -> C.Term
joinTerm ty@(C.ForAllTy _) = signalTerm ty
joinTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @forall a. CallStack -> (HasCallStack => a) -> a@
--
-- Generate the term
--
-- @/\(a:*)./\(callStack:CallStack).\(f:HasCallStack => a).f callStack@
withFrozenCallStackTerm
  :: C.Type
  -> C.Term
withFrozenCallStackTerm (C.ForAllTy tvATy) =
  C.TyLam (bind aTV (
  C.Lam   (bind callStackId (
  C.Lam   (bind fId (
  C.App (C.Var fTy fName) (C.Var callStackTy callStackName)))))))
  where
    (aTV,funTy) = runFreshM (unbind tvATy)
    (C.FunTy callStackTy fTy) = C.tyView funTy
    callStackName = C.string2SystemName "callStack"
    fName         = C.string2SystemName "f"
    callStackId   = C.Id callStackName (embed callStackTy)
    fId           = C.Id fName (embed fTy)

withFrozenCallStackTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @forall a. a -> a@
--
-- Generate the term
--
-- @/\(a:*).\(x:a).x@
idTerm
  :: C.Type
  -> C.Term
idTerm (C.ForAllTy tvATy) =
  C.TyLam (bind aTV (
  C.Lam   (bind xId (
  C.Var xTy xName))))
  where
    (aTV,funTy)     = runFreshM (unbind tvATy)
    (C.FunTy xTy _) = C.tyView funTy
    xName           = C.string2SystemName "x"
    xId             = C.Id xName (embed xTy)

idTerm ty = error $ $(curLoc) ++ show ty

-- | Given type type:
--
-- @forall (r :: RuntimeRep) (o :: TYPE r).(State# RealWorld -> o) -> o@
--
-- Genereate the term:
--
-- @/\(r:RuntimeRep)./\(o:TYPE r).\(f:State# RealWord -> o) -> f realWorld#@
runRWTerm
  :: C.Type
  -> C.Term
runRWTerm (C.ForAllTy tvRTy) =
  C.TyLam (bind rTV (
  C.TyLam (bind oTV (
  C.Lam   (bind fId (
  (C.App (C.Var fTy fName) (C.Prim rwNm rwTy))))))))
  where
    (rTV,oTV,funTy) = runFreshM $ do
      { (rTV',C.ForAllTy tvOTy) <- unbind tvRTy
      ; (oTV',funTy')           <- unbind tvOTy
      ; return (rTV',oTV',funTy')
      }
    (C.FunTy fTy _)  = C.tyView funTy
    (C.FunTy rwTy _) = C.tyView fTy
    fName            = C.string2SystemName "f"
    fId              = C.Id fName (embed fTy)
    rwNm             = pack "GHC.Prim.realWorld#"

runRWTerm ty = error $ $(curLoc) ++ show ty

isDataConWrapId :: Id -> Bool
isDataConWrapId v = case idDetails v of
  DataConWrapId {} -> True
  _                -> False
