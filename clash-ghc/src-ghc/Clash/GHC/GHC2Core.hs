{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2818, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.GHC.GHC2Core
  ( C2C
  , GHC2CoreState
  , tyConMap
  , coreToTerm
  , coreToId
  , coreToName
  , modNameM
  , qualifiedNameString
  , qualifiedNameString'
  , makeAllTyCons
  , emptyGHC2CoreState
  )
where

-- External Modules
import           Control.Lens                ((^.), (%~), (&), (%=))
import           Control.Monad.RWS.Strict    (RWS)
import qualified Control.Monad.RWS.Strict    as RWS
import qualified Data.ByteString.Char8       as Char8
import           Data.Char                   (isDigit)
import           Data.Hashable               (Hashable (..))
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Maybe                  (catMaybes,fromMaybe,listToMaybe)
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import           Data.Text                   (Text, isInfixOf,pack)
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8)
import qualified Data.Traversable            as T
import qualified Text.Read                   as Text

-- GHC API
import CoAxiom    (CoAxiom (co_ax_branches), CoAxBranch (cab_lhs,cab_rhs),
                   fromBranches)
import Coercion   (coercionType,coercionKind)
import CoreFVs    (exprSomeFreeVars)
import CoreSyn
  (AltCon (..), Bind (..), CoreExpr, Expr (..), Unfolding (..), Tickish (..),
   collectArgs, rhssOfAlts, unfoldingTemplate)
import DataCon    (DataCon,
#if MIN_VERSION_ghc(8,8,0)
                   dataConExTyCoVars,
#else
                   dataConExTyVars,
#endif
                   dataConName, dataConRepArgTys,
                   dataConTag, dataConTyCon,
                   dataConUnivTyVars, dataConWorkId,
                   dataConFieldLabels, flLabel)
import DynFlags   (unsafeGlobalDynFlags)
import FamInstEnv (FamInst (..), FamInstEnvs,
                   familyInstances)

#if MIN_VERSION_ghc(8,10,0)
import FastString (unpackFS, bytesFS)
#else
import FastString (unpackFS, fastStringToByteString)
#endif

import Id         (isDataConId_maybe)
import IdInfo     (IdDetails (..), unfoldingInfo)
import Literal    (Literal (..))
#if MIN_VERSION_ghc(8,6,0)
import Literal    (LitNumType (..))
#endif
import Module     (moduleName, moduleNameString)
import Name       (Name, nameModule_maybe,
                   nameOccName, nameUnique, getSrcSpan)
import PrelNames  (tYPETyConKey, integerTyConKey, naturalTyConKey)
import OccName    (occNameString)
import Outputable (showPpr)
import Pair       (Pair (..))
import SrcLoc     (SrcSpan (..), isGoodSrcSpan)
import TyCon      (AlgTyConRhs (..), TyCon, tyConName,
                   algTyConRhs, isAlgTyCon, isFamilyTyCon,
                   isFunTyCon, isNewTyCon,
                   isPrimTyCon, isTupleTyCon,
                   isClosedSynFamilyTyConWithAxiom_maybe,
                   expandSynTyCon_maybe,
                   tyConArity,
                   tyConDataCons, tyConKind,
                   tyConName, tyConUnique, isClassTyCon)
import Type       (mkTvSubstPrs, substTy, coreView)
import TyCoRep    (Coercion (..), TyLit (..), Type (..))
import Unique     (Uniquable (..), Unique, getKey, hasKey)
import Var        (Id, TyVar, Var, idDetails,
                   isTyVar, varName, varType,
                   varUnique, idInfo, isGlobalId)
#if MIN_VERSION_ghc(8,8,0)
import Var        (VarBndr (..))
#else
import Var        (TyVarBndr (..))
#endif
import VarSet     (isEmptyVarSet)

-- Local imports
import           Clash.Annotations.Primitive (extractPrim)
import qualified Clash.Core.DataCon          as C
import qualified Clash.Core.Literal          as C
import qualified Clash.Core.Name             as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.TyCon            as C
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Var              as C
import           Clash.Primitives.Types
import qualified Clash.Unique                as C
import           Clash.Util

instance Hashable Name where
  hashWithSalt s = hashWithSalt s . getKey . nameUnique

data GHC2CoreState
  = GHC2CoreState
  { _tyConMap :: C.UniqMap TyCon
  , _nameMap  :: HashMap Name Text
  }

makeLenses ''GHC2CoreState

emptyGHC2CoreState :: GHC2CoreState
emptyGHC2CoreState = GHC2CoreState C.emptyUniqMap HashMap.empty

newtype SrcSpanRB = SrcSpanRB {unSrcSpanRB :: SrcSpan}

instance Semigroup SrcSpanRB where
  (SrcSpanRB l) <> (SrcSpanRB r) =
    if   isGoodSrcSpan r
    then SrcSpanRB r
    else SrcSpanRB l

instance Monoid SrcSpanRB where
  mempty = SrcSpanRB noSrcSpan
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

type C2C = RWS SrcSpan SrcSpanRB GHC2CoreState

makeAllTyCons
  :: GHC2CoreState
  -> FamInstEnvs
  -> C.UniqMap C.TyCon
makeAllTyCons hm fiEnvs = go hm hm
  where
    go old new
        | C.nullUniqMap (new ^. tyConMap) = C.emptyUniqMap
        | otherwise                       = tcm `C.unionUniqMap` tcm'
      where
        (tcm,old', _) = RWS.runRWS (T.mapM (makeTyCon fiEnvs) (new ^. tyConMap)) noSrcSpan old
        tcm'          = go old' (old' & tyConMap %~ (`C.differenceUniqMap` (old ^. tyConMap)))

makeTyCon :: FamInstEnvs
          -> TyCon
          -> C2C C.TyCon
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
          tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          tcRhsM <- makeAlgTyConRhs $ algTyConRhs tc
          case tcRhsM of
            Just tcRhs ->
              return
                C.AlgTyCon
                { C.tyConUniq   = C.nameUniq tcName
                , C.tyConName   = tcName
                , C.tyConKind   = tcKind
                , C.tyConArity  = tcArity
                , C.algTcRhs    = tcRhs
                , C.isClassTc   = isClassTyCon tc
                }
            Nothing -> return (C.PrimTyCon (C.nameUniq tcName) tcName tcKind tcArity)

        mkFunTyCon = do
          tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
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
            { C.tyConUniq  = C.nameUniq tcName
            , C.tyConName  = tcName
            , C.tyConKind  = tcKind
            , C.tyConArity = tcArity
            , C.tyConSubst = substs
            }

        mkTupleTyCon = do
          tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          tcDc   <- fmap (C.DataTyCon . (:[])) . coreToDataCon . head . tyConDataCons $ tc
          return
            C.AlgTyCon
            { C.tyConUniq   = C.nameUniq tcName
            , C.tyConName   = tcName
            , C.tyConKind   = tcKind
            , C.tyConArity  = tcArity
            , C.algTcRhs    = tcDc
            , C.isClassTc   = isClassTyCon tc
            }

        mkPrimTyCon = do
          tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          return
            C.PrimTyCon
            { C.tyConUniq    = C.nameUniq tcName
            , C.tyConName    = tcName
            , C.tyConKind    = tcKind
            , C.tyConArity   = tcArity
            }

        mkSuperKindTyCon = do
          tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
          return C.SuperKindTyCon
                   { C.tyConUniq = C.nameUniq tcName
                   , C.tyConName = tcName
                   }

        mkVoidTyCon = do
          tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          return (C.PrimTyCon (C.nameUniq tcName) tcName tcKind tcArity)

        famInstToSubst :: FamInst -> C2C ([C.Type],C.Type)
        famInstToSubst fi = do
          tys <- mapM coreToType  (fi_tys fi)
          ty  <- coreToType (fi_rhs fi)
          return (tys,ty)

makeAlgTyConRhs :: AlgTyConRhs
                -> C2C (Maybe C.AlgTyConRhs)
makeAlgTyConRhs algTcRhs = case algTcRhs of
#if MIN_VERSION_ghc(8,6,0)
  DataTyCon dcs _ _ -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
#else
  DataTyCon dcs _ -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
#endif
#if MIN_VERSION_ghc(8,6,0)
  SumTyCon dcs _ -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
#else
  SumTyCon dcs -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
#endif

#if MIN_VERSION_ghc(8,10,0)
  NewTyCon dc _ (rhsTvs,rhsEtad) _ _ ->
#else
  NewTyCon dc _ (rhsTvs,rhsEtad) _ ->
#endif
                                      Just <$> (C.NewTyCon <$> coreToDataCon dc
                                                           <*> ((,) <$> mapM coreToTyVar rhsTvs
                                                                    <*> coreToType rhsEtad
                                                               )
                                               )
  AbstractTyCon {} -> return Nothing
  TupleTyCon {}    -> error "Cannot handle tuple tycons"

coreToTerm
  :: CompiledPrimMap
  -> [Var]
  -> CoreExpr
  -> C2C C.Term
coreToTerm primMap unlocs = term
  where
    term :: CoreExpr -> C2C C.Term
    term e
      | (Var x,args) <- collectArgs e
      , let (nm, _) = RWS.evalRWS (qualifiedNameString (varName x))
                                  noSrcSpan
                                  emptyGHC2CoreState
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
        go "GHC.Magic.noinline"                args   -- noinline :: forall a. a -> a
          | [_ty, x] <- args
          = term x
        -- Remove most CallStack logic
        go "GHC.Stack.Types.PushCallStack"     args = term (last args)
        go "GHC.Stack.Types.FreezeCallStack"   args = term (last args)
        go "GHC.Stack.withFrozenCallStack"     args
          | length args == 3
          = term (App (args!!2) (args!!1))
        go "Clash.Class.BitPack.packXWith" args
          | [_nTy,_aTy,_kn,f] <- args
          = term f
        go "Clash.Sized.BitVector.Internal.checkUnpackUndef" args
          | [_nTy,_aTy,_kn,_typ,f] <- args
          = term f
        go "Clash.Magic.prefixName" args
          | [Type nmTy,_aTy,f] <- args
          = C.Tick <$> (C.NameMod C.PrefixName <$> coreToType nmTy) <*> term f
        go "Clash.Magic.suffixName" args
          | [Type nmTy,_aTy,f] <- args
          = C.Tick <$> (C.NameMod C.SuffixName <$> coreToType nmTy) <*> term f
        go "Clash.Magic.suffixNameFromNat" args
          | [Type nmTy,_aTy,f] <- args
          = C.Tick <$> (C.NameMod C.SuffixName <$> coreToType nmTy) <*> term f
        go "Clash.Magic.suffixNameP" args
          | [Type nmTy,_aTy,f] <- args
          = C.Tick <$> (C.NameMod C.SuffixNameP <$> coreToType nmTy) <*> term f
        go "Clash.Magic.suffixNameFromNatP" args
          | [Type nmTy,_aTy,f] <- args
          = C.Tick <$> (C.NameMod C.SuffixNameP <$> coreToType nmTy) <*> term f
        go "Clash.Magic.setName" args
          | [Type nmTy,_aTy,f] <- args
          = C.Tick <$> (C.NameMod C.SetName <$> coreToType nmTy) <*> term f
        go "Clash.Magic.deDup" args
          | [_aTy,f] <- args
          = C.Tick C.DeDup <$> term f
        go "Clash.Magic.noDeDup" args
          | [_aTy,f] <- args
          = C.Tick C.NoDeDup <$> term f
        go "Clash.XException.xToErrorCtx" args
          -- xToErrorCtx :: forall a. String -> a -> a
          | [_ty, _msg, x] <- args
          = term x
        go nm args
          | Just n <- parseBundle "bundle" nm
            -- length args = domain tyvar + signal arg + number of type vars
          , length args == 2 + n
          = term (last args)
        go nm args
          | Just n <- parseBundle "unbundle" nm
            -- length args = domain tyvar + signal arg + number of type vars
          , length args == 2 + n
          = term (last args)
        go _ _ = term' e

    parseBundle :: Text -> Text -> Maybe Int
    parseBundle fNm nm0 = do
      nm1 <- Text.stripPrefix ("Clash.Signal.Bundle." <> fNm) nm0
      nm2 <- Text.stripSuffix "#" nm1
      Text.readMaybe (Text.unpack nm2)

    term' (Var x)                 = var x
    term' (Lit l)                 = return $ C.Literal (coreToLiteral l)
    term' (App eFun (Type tyArg)) = C.TyApp <$> term eFun <*> coreToType tyArg
    term' (App eFun eArg)         = C.App   <$> term eFun <*> term eArg
    term' (Lam x e)
      | isTyVar x
      = C.TyLam <$> coreToTyVar x <*> addUsefull (getSrcSpan x) (term e)
      | otherwise
      = do
        (e',sp) <- termSP (getSrcSpan x) e
        x' <- coreToIdSP sp x
        return (C.Lam  x' e')
    term' (Let (NonRec x e1) e2)  = do
      (e1',sp) <- termSP (getSrcSpan x) e1
      x'  <- coreToIdSP sp x
      e2' <- term e2
      return (C.Letrec [(x', e1')] e2')

    term' (Let (Rec xes) e) = do
      xes' <- mapM go xes
      e'   <- term e
      return (C.Letrec xes' e')
     where
      go (x,b) = do
        (b',sp) <- termSP (getSrcSpan x) b
        x' <- coreToIdSP sp x
        return (x',b')

    term' (Case _ _ ty [])  = C.TyApp (C.Prim (C.PrimInfo (pack "EmptyCase") C.undefinedTy C.WorkNever Nothing))
                                <$> coreToType ty
    term' (Case e b ty alts) = do
     let usesBndr = any ( not . isEmptyVarSet . exprSomeFreeVars (== b))
                  $ rhssOfAlts alts
     (e',sp) <- termSP (getSrcSpan b) e
     b'  <- coreToIdSP sp b
     ty' <- coreToType ty
     let caseTerm v =
             C.Case v ty' <$> mapM (addUsefull sp . alt sp) alts
     if usesBndr
      then do
        ct <- caseTerm (C.Var b')
        return (C.Letrec [(b', e')] ct)
      else caseTerm e'

    term' (Cast e co) = do
      let (Pair ty1 ty2) = coercionKind co
      hasPrimCoM <- hasPrimCo co
      sizedCast <- isSizedCast ty1 ty2
      case hasPrimCoM of
        Just _ | sizedCast
          -> C.Cast <$> term e <*> coreToType ty1 <*> coreToType ty2
        _ -> term e
    term' (Tick (SourceNote rsp _) e) =
      C.Tick (C.SrcSpan (RealSrcSpan rsp)) <$> addUsefull (RealSrcSpan rsp) (term e)
    term' (Tick _ e)        = term e
    term' (Type t)          = C.TyApp (C.Prim (C.PrimInfo (pack "_TY_") C.undefinedTy C.WorkNever Nothing)) <$>
                                coreToType t
    term' (Coercion co)     = C.TyApp (C.Prim (C.PrimInfo (pack "_CO_") C.undefinedTy C.WorkNever Nothing)) <$>
                                coreToType (coercionType co)


    termSP sp = fmap (second unSrcSpanRB) . RWS.listen . addUsefullR sp . term
    coreToIdSP sp = RWS.local (\r -> if isGoodSrcSpan sp then sp else r) . coreToId


    lookupPrim :: Text -> Maybe (Maybe CompiledPrimitive)
    lookupPrim nm = extractPrim <$> HashMap.lookup nm primMap

    var x = do
        xPrim <- if isGlobalId x then coreToPrimVar x else coreToVar x
        let xNameS = C.nameOcc xPrim
        xType  <- coreToType (varType x)
        case isDataConId_maybe x of
          Just dc -> case lookupPrim xNameS of
            Just p  -> return $ C.Prim (C.PrimInfo xNameS xType (maybe C.WorkVariable workInfo p) Nothing)
            Nothing -> if isDataConWrapId x && not (isNewTyCon (dataConTyCon dc))
              then let xInfo = idInfo x
                       unfolding = unfoldingInfo xInfo
                   in  case unfolding of
                          CoreUnfolding {} -> do
                            sp <- RWS.ask
                            RWS.censor (const (SrcSpanRB sp)) (term (unfoldingTemplate unfolding))
                          NoUnfolding -> error ("No unfolding for DC wrapper: " ++ showPpr unsafeGlobalDynFlags x)
                          _ -> error ("Unexpected unfolding for DC wrapper: " ++ showPpr unsafeGlobalDynFlags x)
              else C.Data <$> coreToDataCon dc
          Nothing -> case lookupPrim xNameS of
            Just (Just (Primitive f wi _))
              | Just n <- parseBundle "bundle" f        -> return (bundleUnbundleTerm (n+1) xType)
              | Just n <- parseBundle "unbundle" f      -> return (bundleUnbundleTerm (n+1) xType)
              | f == "Clash.Signal.Internal.mapSignal#" -> return (mapSignalTerm xType)
              | f == "Clash.Signal.Internal.mapSignal#" -> return (mapSignalTerm xType)
              | f == "Clash.Signal.Internal.signal#"    -> return (signalTerm xType)
              | f == "Clash.Signal.Internal.appSignal#" -> return (appSignalTerm xType)
              | f == "Clash.Signal.Internal.traverse#"  -> return (traverseTerm xType)
              | f == "Clash.Signal.Internal.joinSignal#" -> return (joinTerm xType)
              | f == "Clash.Signal.Bundle.vecBundle#"   -> return (vecUnwrapTerm xType)
              | f == "GHC.Base.$"                       -> return (dollarTerm xType)
              | f == "GHC.Stack.withFrozenCallStack"    -> return (withFrozenCallStackTerm xType)
              | f == "GHC.Magic.noinline"               -> return (idTerm xType)
              | f == "GHC.Magic.lazy"                   -> return (idTerm xType)
              | f == "GHC.Magic.runRW#"                 -> return (runRWTerm xType)
              | f == "Clash.Class.BitPack.packXWith"    -> return (packXWithTerm xType)
              | f == "Clash.Sized.Internal.BitVector.checkUnpackUndef" -> return (checkUnpackUndefTerm xType)
              | f == "Clash.Magic.prefixName"
              -> return (nameModTerm C.PrefixName xType)
              | f == "Clash.Magic.postfixName"
              -> return (nameModTerm C.SuffixName xType)
              | f == "Clash.Magic.setName"
              -> return (nameModTerm C.SetName xType)
              | f == "Clash.XException.xToErrorCtx"
              -> return (xToErrorCtxTerm xType)
              | x `elem` unlocs
              -> return (C.Prim (C.PrimInfo xNameS xType wi Nothing))
              | otherwise
              -> do bndr <- coreToId x
                    return (C.Prim (C.PrimInfo xNameS xType wi (Just bndr)))
            Just (Just (BlackBox {workInfo = wi}))
              | x `elem` unlocs
              -> return $ C.Prim (C.PrimInfo xNameS xType wi Nothing)
              | otherwise
              -> do bndr <- coreToId x
                    return (C.Prim (C.PrimInfo xNameS xType wi (Just bndr)))
            Just (Just (BlackBoxHaskell {workInfo = wi}))
              | x `elem` unlocs
              -> return $ C.Prim (C.PrimInfo xNameS xType wi Nothing)
              | otherwise
              -> do bndr <- coreToId x
                    return $ C.Prim (C.PrimInfo xNameS xType wi (Just bndr))
            Just Nothing ->
              -- Was guarded by "DontTranslate". We don't know yet if Clash will
              -- actually use it later on, so we don't err here.
              return $ C.Prim (C.PrimInfo xNameS xType C.WorkVariable Nothing)
            Nothing
              | x `elem` unlocs
              -> return (C.Prim (C.PrimInfo xNameS xType C.WorkVariable Nothing))
              | pack "$cshow" `isInfixOf` xNameS
              -> return (C.Prim (C.PrimInfo xNameS xType C.WorkVariable Nothing))
              | otherwise
              -> C.Var <$> coreToId x

    alt _   (DEFAULT   , _ , e) = (C.DefaultPat,) <$> term e
    alt _   (LitAlt l  , _ , e) = (C.LitPat (coreToLiteral l),) <$> term e
    alt sp0 (DataAlt dc, xs, e) = case span isTyVar xs of
      (tyvs,tmvs) -> do
        (e',sp1) <- termSP sp0 e
        (,) <$> (C.DataPat <$> coreToDataCon dc
                           <*> mapM coreToTyVar tyvs
                           <*> mapM (coreToIdSP sp1) tmvs)
            <*> pure e'

    coreToLiteral :: Literal
                  -> C.Literal
    coreToLiteral l = case l of
#if MIN_VERSION_ghc(8,8,0)
      LitString  fs  -> C.StringLiteral (Char8.unpack fs)
      LitChar    c   -> C.CharLiteral c
      LitRubbish     ->
        error $ "coreToTerm: Encountered LibRubbish. This is a bug in Clash. "
             ++ "Report on https://github.com/clash-lang/clash-compiler/issues."
#else
      MachStr    fs  -> C.StringLiteral (Char8.unpack fs)
      MachChar   c   -> C.CharLiteral c
#endif
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
#if MIN_VERSION_ghc(8,8,0)
      LitFloat r    -> C.FloatLiteral r
      LitDouble r   -> C.DoubleLiteral r
      LitNullAddr   -> C.StringLiteral []
      LitLabel fs _ _ -> C.StringLiteral (unpackFS fs)
#else
      MachFloat r    -> C.FloatLiteral r
      MachDouble r   -> C.DoubleLiteral r
      MachNullAddr   -> C.StringLiteral []
      MachLabel fs _ _ -> C.StringLiteral (unpackFS fs)
#endif

addUsefull :: SrcSpan
           -> C2C a
           -> C2C a
addUsefull x m =
  if isGoodSrcSpan x
  then do a <- RWS.local (const x) m
          RWS.tell (SrcSpanRB x)
          return a
  else m

addUsefullR :: SrcSpan
            -> C2C a
            -> C2C a
addUsefullR x m =
  if isGoodSrcSpan x
  then RWS.local (const x) m
  else m

isSizedCast :: Type -> Type -> C2C Bool
isSizedCast (TyConApp tc1 _) (TyConApp tc2 _) = do
  tc1Nm <- qualifiedNameString (tyConName tc1)
  tc2Nm <- qualifiedNameString (tyConName tc2)
  return
    (or [tc1 `hasKey` integerTyConKey &&
          or [tc2Nm == "Clash.Sized.Internal.Signed.Signed"
             ,tc2Nm == "Clash.Sized.Internal.Index.Index"]
        ,tc2 `hasKey` integerTyConKey &&
          or [tc1Nm == "Clash.Sized.Internal.Signed.Signed"
             ,tc1Nm == "Clash.Sized.Internal.Index.Index"]
        ,tc1 `hasKey` naturalTyConKey &&
          tc2Nm == "Clash.Sized.Internal.Unsigned.Unsigned"
        ,tc2 `hasKey` naturalTyConKey &&
          tc1Nm == "Clash.Sized.Internal.Unsigned.Unsigned"
        ])
isSizedCast _ _ = return False

hasPrimCo :: Coercion -> C2C (Maybe Type)
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
      tcNm <- qualifiedNameString (tyConName tc)
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
              -> C2C C.DataCon
coreToDataCon dc = do
    repTys <- mapM coreToType (dataConRepArgTys dc)
    dcTy   <- coreToType (varType $ dataConWorkId dc)
    mkDc dcTy repTys
  where
    mkDc dcTy repTys = do
#if MIN_VERSION_ghc(8,10,0)
      let decLabel = decodeUtf8 . bytesFS . flLabel
#else
      let decLabel = decodeUtf8 . fastStringToByteString . flLabel
#endif
      let fLabels  = map decLabel (dataConFieldLabels dc)

      nm   <- coreToName dataConName getUnique qualifiedNameString dc
      uTvs <- mapM coreToTyVar (dataConUnivTyVars dc)
#if MIN_VERSION_ghc(8,8,0)
      eTvs <- mapM coreToTyVar (dataConExTyCoVars dc)
#else
      eTvs <- mapM coreToTyVar (dataConExTyVars dc)
#endif
      return $ C.MkData
             { C.dcName        = nm
             , C.dcUniq        = C.nameUniq nm
             , C.dcTag         = dataConTag dc
             , C.dcType        = dcTy
             , C.dcArgTys      = repTys
             , C.dcUnivTyVars  = uTvs
             , C.dcExtTyVars   = eTvs
             , C.dcFieldLabels = fLabels
             }

typeConstructorToString
  :: TyCon
  -> C2C String
typeConstructorToString constructor =
   Text.unpack . C.nameOcc <$> coreToName tyConName tyConUnique qualifiedNameString constructor

_ATTR_NAME :: String
_ATTR_NAME = "Clash.Annotations.SynthesisAttributes.Attr"

-- | Flatten a list type structure to a list of types.
listTypeToListOfTypes :: Type -> [Type]
listTypeToListOfTypes (TyConApp _ [_, a, as]) = a : listTypeToListOfTypes as
listTypeToListOfTypes _                       = []

-- | Try to determine boolean value by looking at constructor name of type.
boolTypeToBool :: Type -> C2C Bool
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
  -> C2C C.Attr'
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
    -> C2C [C.Attr']
coreToAttrs' [annotationType, realType, attributes] = allAttrs
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
                      -- List, but unknown types
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
  error $ "Expected list with three items (as Annotate has three arguments), but got: "
      ++ show (map (showPpr unsafeGlobalDynFlags) illegal)

-- | If this type has an annotate type synonym, return list of attributes.
coreToAttrs
  :: Type
  -> C2C [C.Attr']
coreToAttrs (TyConApp tycon kindsOrTypes) = do
  name' <- typeConstructorToString tycon

  if name' == "Clash.Annotations.SynthesisAttributes.Annotate"
    then
      coreToAttrs' kindsOrTypes
    else
      return []

coreToAttrs _ =
    return []

-- | Wrap given type in an annotation if it is annotated using the constructs
-- defined in Clash.Annotations.SynthesisAttributes.
annotateType
  :: Type
  -> C.Type
  -> C2C C.Type
annotateType ty cty = do
  attrs <- coreToAttrs ty
  case attrs of
    [] -> return cty
    _  -> return $ C.AnnType attrs cty

-- | Converts GHC Type to a Clash Type. Strips newtypes and signals, with the
-- exception of newtypes used as annotations (see: SynthesisAttributes).
coreToType
  :: Type
  -> C2C C.Type
coreToType ty = ty'' >>= annotateType ty
  where
    ty'' =
      case coreView ty of
        Just ty' -> coreToType ty'
        Nothing  -> coreToType' ty

coreToType'
  :: Type
  -> C2C C.Type
coreToType' (TyVarTy tv) = C.VarTy <$> coreToTyVar tv
coreToType' (TyConApp tc args)
  | isFunTyCon tc = foldl C.AppTy (C.ConstTy C.Arrow) <$> mapM coreToType args
  | otherwise     = case expandSynTyCon_maybe tc args of
                      Just (substs,synTy,remArgs) -> do
                        let substs' = mkTvSubstPrs substs
                            synTy'  = substTy substs' synTy
                        foldl C.AppTy <$> coreToType synTy' <*> mapM coreToType remArgs
                      _ -> do
                        tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
                        tyConMap %= (C.extendUniqMap tcName tc)
                        C.mkTyConApp <$> (pure tcName) <*> mapM coreToType args
#if MIN_VERSION_ghc(8,8,0)
coreToType' (ForAllTy (Bndr tv _) ty)   = C.ForAllTy <$> coreToTyVar tv <*> coreToType ty
#else
coreToType' (ForAllTy (TvBndr tv _) ty) = C.ForAllTy <$> coreToTyVar tv <*> coreToType ty
#endif
#if MIN_VERSION_ghc(8,10,0)
-- TODO after we drop 8.8: save the distinction between => and ->
coreToType' (FunTy _ ty1 ty2)             = C.mkFunTy <$> coreToType ty1 <*> coreToType ty2
#else
coreToType' (FunTy ty1 ty2)             = C.mkFunTy <$> coreToType ty1 <*> coreToType ty2
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
            -> C2C C.TyVar
coreToTyVar tv =
  C.mkTyVar <$> coreToType (varType tv) <*> coreToVar tv

coreToId :: Id
         -> C2C C.Id
coreToId i = do
  C.mkId <$> coreToType (varType i) <*> pure scope <*> coreToVar i
 where
  scope = if isGlobalId i then C.GlobalId else C.LocalId

coreToVar :: Var
          -> C2C (C.Name a)
coreToVar = coreToName varName varUnique qualifiedNameStringM

coreToPrimVar :: Var
              -> C2C (C.Name C.Term)
coreToPrimVar = coreToName varName varUnique qualifiedNameString

coreToName
  :: (b -> Name)
  -> (b -> Unique)
  -> (Name -> C2C Text)
  -> b
  -> C2C (C.Name a)
coreToName toName toUnique toString v = do
  ns <- toString (toName v)
  let key  = getKey (toUnique v)
      locI = getSrcSpan (toName v)
      -- Is it one of [ds,ds1,ds2,..]
      isDSX = maybe False (maybe True (isDigit . fst) . Text.uncons) . Text.stripPrefix "ds"
      sort | isDSX ns || Text.isPrefixOf "$" ns
           = C.System
           | otherwise
           = C.User
  locR <- RWS.ask
  let loc = if isGoodSrcSpan locI then locI else locR
  return (C.Name sort ns key loc)

qualifiedNameString'
  :: Name
  -> Text
qualifiedNameString' n =
  fromMaybe "_INTERNAL_" (modNameM n) `Text.append` ('.' `Text.cons` occName)
 where
  occName = pack (occNameString (nameOccName n))

qualifiedNameString
  :: Name
  -> C2C Text
qualifiedNameString n =
  makeCached n nameMap $
  return (fromMaybe "_INTERNAL_" (modNameM n) `Text.append` ('.' `Text.cons` occName))
 where
  occName = pack (occNameString (nameOccName n))

qualifiedNameStringM
  :: Name
  -> C2C Text
qualifiedNameStringM n =
  makeCached n nameMap $
  return (maybe occName (\modName -> modName `Text.append` ('.' `Text.cons` occName)) (modNameM n))
 where
  occName = pack (occNameString (nameOccName n))

modNameM :: Name
         -> Maybe Text
modNameM n = do
  module_ <- nameModule_maybe n
  let moduleNm = moduleName module_
  return (pack (moduleNameString moduleNm))

-- | Given the type:
--
-- @
--     forall dom a0 a1 .. aN
--   . Signal dom (a0, a1, .., aN)
--  -> (Signal dom a0, Signal dom a1, .., Signal dom aN)
-- @
--
-- or the type
--
-- @
--     forall dom a0 a1 .. aN
--   . (Signal dom a0, Signal dom a1, .., Signal dom aN)
--  -> Signal dom (a0, a1, .., aN)
-- @
--
-- Generate the term:
--
-- @/\dom. /\a0. /\a1. .. /\aN. \x -> x@
--
-- In other words: treat "bundle" and "unbundle" primitives as id.
--
bundleUnbundleTerm :: Int -> C.Type -> C.Term
bundleUnbundleTerm nTyVarsExpected = go []
 where
  go :: [C.TyVar] -> C.Type -> C.Term
  go tvs (C.ForAllTy tv typ) = go (tv:tvs) typ
  go tvs (C.tyView -> C.FunTy argTy _resTy) =
    if length tvs /= nTyVarsExpected then
      -- Internal error: should never happen unless we change the type of
      -- bundle / unbundle.
      error $ $(curLoc) ++ show (length tvs) ++ " vs " ++ show nTyVarsExpected
    else
      let sigName = C.mkLocalId argTy (C.mkUnsafeSystemName "c$s" 0) in
      foldr C.TyLam (C.Lam sigName (C.Var sigName)) (reverse tvs)
  go tvs ty = error $ $(curLoc) ++ show ty ++ " " ++ show tvs


-- | Given the type:
--
-- @forall a. forall b. forall clk. (a -> b) -> Signal clk a -> Signal clk b@
--
-- Generate the term:
--
-- @
-- /\(a:*)./\(b:*)./\(clk:Clock).\(f : (Signal clk a -> Signal clk b)).
-- \(x : Signal clk a).f x
-- @
mapSignalTerm :: C.Type
              -> C.Term
mapSignalTerm (C.ForAllTy aTV (C.ForAllTy bTV (C.ForAllTy clkTV funTy))) =
    C.TyLam aTV (
    C.TyLam bTV (
    C.TyLam clkTV (
    C.Lam   fId (
    C.Lam   xId (
    C.App (C.Var fId) (C.Var xId))))))
  where
    (C.FunTy _ funTy'') = C.tyView funTy
    (C.FunTy aTy bTy)   = C.tyView funTy''
    fName = C.mkUnsafeSystemName "f" 0
    xName = C.mkUnsafeSystemName "x" 1
    fTy   = C.mkFunTy aTy bTy
    fId   = C.mkLocalId fTy fName
    xId   = C.mkLocalId aTy xName

mapSignalTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @forall a. forall dom. a -> Signal dom a@
--
-- Generate the term
--
-- @/\(a:*)./\(dom:Domain).\(x:Signal dom a).x@
signalTerm :: C.Type
           -> C.Term
signalTerm (C.ForAllTy aTV (C.ForAllTy domTV funTy)) =
    C.TyLam aTV (
    C.TyLam domTV (
    C.Lam   xId (
    C.Var   xId)))
  where
    (C.FunTy _ saTy) = C.tyView funTy
    xName = C.mkUnsafeSystemName "x" 0
    xId   = C.mkLocalId saTy xName

signalTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @
-- forall dom. forall a. forall b. Signal dom (a -> b) -> Signal dom a ->
-- Signal dom b
-- @
--
-- Generate the term:
--
-- @
-- /\(dom:Domain)./\(a:*)./\(b:*).\(f : (Signal dom a -> Signal dom b)).
-- \(x : Signal dom a).f x
-- @
appSignalTerm :: C.Type
              -> C.Term
appSignalTerm (C.ForAllTy domTV (C.ForAllTy aTV (C.ForAllTy bTV funTy))) =
    C.TyLam domTV (
    C.TyLam aTV (
    C.TyLam bTV (
    C.Lam   fId (
    C.Lam   xId (
    C.App (C.Var fId) (C.Var xId))))))
  where
    (C.FunTy _ funTy'') = C.tyView funTy
    (C.FunTy saTy sbTy)   = C.tyView funTy''
    fName = C.mkUnsafeSystemName "f" 0
    xName = C.mkUnsafeSystemName "x" 1
    fTy   = C.mkFunTy saTy sbTy
    fId   = C.mkLocalId fTy fName
    xId   = C.mkLocalId saTy xName

appSignalTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @
-- forall t.forall n.forall a.Vec n (Signal t a) ->
-- Signal t (Vec n a)
-- @
--
-- Generate the term:
--
-- @
-- /\(t:Domain)./\(n:Nat)./\(a:*).\(vs:Signal t (Vec n a)).vs
-- @
vecUnwrapTerm :: C.Type
              -> C.Term
vecUnwrapTerm (C.ForAllTy tTV (C.ForAllTy nTV (C.ForAllTy aTV funTy))) =
    C.TyLam tTV (
    C.TyLam nTV (
    C.TyLam aTV (
    C.Lam   vsId (
    C.Var vsId))))
  where
    (C.FunTy _ vsTy) = C.tyView funTy
    vsName           = C.mkUnsafeSystemName "vs" 0
    vsId             = C.mkLocalId vsTy vsName

vecUnwrapTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @
-- forall f.forall a.forall b.forall dom.Applicative f => (a -> f b) ->
-- Signal dom a -> f (Signal dom b)
-- @
--
-- Generate the term:
--
-- @
-- /\(f:* -> *)./\(a:*)./\(b:*)./\(dom:Clock).\(dict:Applicative f).
-- \(g:a -> f b).\(x:Signal dom a).g x
-- @
traverseTerm :: C.Type
             -> C.Term
traverseTerm (C.ForAllTy fTV (C.ForAllTy aTV (C.ForAllTy bTV (C.ForAllTy domTV funTy)))) =
    C.TyLam fTV (
    C.TyLam aTV (
    C.TyLam bTV (
    C.TyLam domTV (
    C.Lam   dictId (
    C.Lam   gId (
    C.Lam   xId (
    C.App (C.Var gId) (C.Var xId))))))))
  where
    (C.FunTy dictTy funTy1) = C.tyView funTy
    (C.FunTy gTy    funTy2) = C.tyView funTy1
    (C.FunTy xTy    _)      = C.tyView funTy2
    dictName = C.mkUnsafeSystemName "dict" 0
    gName    = C.mkUnsafeSystemName "g" 1
    xName    = C.mkUnsafeSystemName "x" 2
    dictId   = C.mkLocalId dictTy dictName
    gId      = C.mkLocalId gTy gName
    xId      = C.mkLocalId xTy xName

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
dollarTerm (C.ForAllTy rTV (C.ForAllTy aTV (C.ForAllTy bTV funTy))) =
    C.TyLam rTV (
    C.TyLam aTV (
    C.TyLam bTV (
    C.Lam   fId (
    C.Lam   xId (
    C.App (C.Var fId) (C.Var xId))))))
  where
    (C.FunTy fTy funTy'') = C.tyView funTy
    (C.FunTy aTy _)       = C.tyView funTy''
    fName = C.mkUnsafeSystemName "f" 0
    xName = C.mkUnsafeSystemName "x" 1
    fId   = C.mkLocalId fTy fName
    xId   = C.mkLocalId aTy xName

dollarTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @forall a. forall dom. Signal dom (Signal dom a) -> Signal dom a@
--
-- Generate the term
--
-- @/\(a:*)./\(dom:Domain).\(x:Signal dom a).x@
joinTerm :: C.Type
         -> C.Term
joinTerm ty@(C.ForAllTy {}) = signalTerm ty
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
withFrozenCallStackTerm (C.ForAllTy aTV funTy) =
  C.TyLam  aTV (
  C.Lam    callStackId (
  C.Lam    fId (
  C.App (C.Var fId) (C.Var callStackId))))
  where
    (C.FunTy callStackTy fTy) = C.tyView funTy
    callStackName = C.mkUnsafeSystemName "callStack" 0
    fName         = C.mkUnsafeSystemName "f" 1
    callStackId   = C.mkLocalId callStackTy callStackName
    fId           = C.mkLocalId fTy fName

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
idTerm (C.ForAllTy aTV funTy) =
  C.TyLam aTV (
  C.Lam   xId (
  C.Var xId))
  where
    (C.FunTy xTy _) = C.tyView funTy
    xName           = C.mkUnsafeSystemName "x" 0
    xId             = C.mkLocalId xTy xName

idTerm ty = error $ $(curLoc) ++ show ty

-- | Given type type:
--
-- @forall (r :: RuntimeRep) (o :: TYPE r).(State# RealWorld -> o) -> o@
--
-- Generate the term:
--
-- @/\(r:RuntimeRep)./\(o:TYPE r).\(f:State# RealWord -> o) -> f realWorld#@
runRWTerm
  :: C.Type
  -> C.Term
runRWTerm (C.ForAllTy rTV (C.ForAllTy oTV funTy)) =
  C.TyLam rTV (
  C.TyLam oTV (
  C.Lam   fId (
  (C.App (C.Var fId) (C.Prim (C.PrimInfo rwNm rwTy C.WorkNever Nothing))))))
  where
    (C.FunTy fTy _)  = C.tyView funTy
    (C.FunTy rwTy _) = C.tyView fTy
    fName            = C.mkUnsafeSystemName "f" 0
    fId              = C.mkLocalId fTy fName
    rwNm             = pack "GHC.Prim.realWorld#"

runRWTerm ty = error $ $(curLoc) ++ show ty

-- | Given type type:
--
-- @forall (n :: Nat) (a :: Type) .Knownnat n => (a -> BitVector n) -> a -> BitVector n@
--
-- Generate the term:
--
-- @/\(n:Nat)./\(a:TYPE r).\(kn:KnownNat n).\(f:a -> BitVector n).f@
packXWithTerm
  :: C.Type
  -> C.Term
packXWithTerm (C.ForAllTy nTV (C.ForAllTy aTV funTy)) =
  C.TyLam nTV (
  C.TyLam aTV (
  C.Lam knId (
  C.Lam fId (
  C.Var fId))))
  where
    C.FunTy knTy rTy = C.tyView funTy
    C.FunTy fTy _    = C.tyView rTy
    knName           = C.mkUnsafeSystemName "kn" 0
    fName            = C.mkUnsafeSystemName "f" 1
    knId             = C.mkLocalId knTy knName
    fId              = C.mkLocalId fTy fName

packXWithTerm ty = error $ $(curLoc) ++ show ty

-- | Given type type:
--
-- @forall (n :: Nat) (a :: Type) .Knownnat n => Typeable a => (BitVector n -> a) -> BitVector n -> a@
--
-- Generate the term:
--
-- @/\(n:Nat)./\(a:TYPE r).\(kn:KnownNat n).\(f:a -> BitVector n).f@
checkUnpackUndefTerm
  :: C.Type
  -> C.Term
checkUnpackUndefTerm (C.ForAllTy nTV (C.ForAllTy aTV funTy)) =
  C.TyLam nTV (
  C.TyLam aTV (
  C.Lam knId (
  C.Lam tpId (
  C.Lam fId (
  C.Var fId)))))
  where
    C.FunTy knTy r0Ty = C.tyView funTy
    C.FunTy tpTy r1Ty = C.tyView r0Ty
    C.FunTy fTy _     = C.tyView r1Ty
    knName            = C.mkUnsafeSystemName "kn" 0
    tpName            = C.mkUnsafeSystemName "tp" 1
    fName             = C.mkUnsafeSystemName "f" 2
    knId              = C.mkLocalId knTy knName
    tpId              = C.mkLocalId tpTy tpName
    fId               = C.mkLocalId fTy fName

checkUnpackUndefTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @forall (name :: Symbol) (a :: Type) . a -> (name ::: a)@
--
-- Generate the term:
--
-- @/\(name:Symbol)./\(a:Type).\(x:a) -> <TICK>x@
nameModTerm
  :: C.NameMod
  -> C.Type
  -> C.Term
nameModTerm sa (C.ForAllTy nmTV (C.ForAllTy aTV funTy)) =
  C.TyLam nmTV (
  C.TyLam aTV (
  C.Lam   xId (
  (C.Tick (C.NameMod sa (C.VarTy nmTV)) (C.Var xId)))))
  where
    (C.FunTy xTy _)  = C.tyView funTy
    -- Safe to use `mkUnsafeSystemName` here, because we're building the
    -- identity \x.x, so any shadowing of 'x' would be the desired behavior.
    xName            = C.mkUnsafeSystemName "x" 0
    xId              = C.mkLocalId xTy xName

nameModTerm _ ty = error $ $(curLoc) ++ show ty


-- | Given the type:
--
-- @forall (a :: Type) . String -> a -> a@
--
-- Generate the term:
--
-- @/\(a:Type).\(ctx:String).\(x:a) -> x@
xToErrorCtxTerm
  :: C.Type
  -> C.Term
xToErrorCtxTerm (C.ForAllTy aTV funTy) =
  C.TyLam aTV (
  C.Lam ctxId (
  C.Lam xId (
  C.Var xId)))
  where
    (C.FunTy ctxTy rTy) = C.tyView funTy
    (C.FunTy xTy _)     = C.tyView rTy
    -- Safe to use `mkUnsafeSystemName` here, because we're building the
    -- identity \_ x.x, so any shadowing of 'x' would be the desired behavior.
    ctxName = C.mkUnsafeSystemName "ctx" 0
    ctxId   = C.mkLocalId ctxTy ctxName
    xName   = C.mkUnsafeSystemName "x" 1
    xId     = C.mkLocalId xTy xName

xToErrorCtxTerm ty = error $ $(curLoc) ++ show ty

isDataConWrapId :: Id -> Bool
isDataConWrapId v = case idDetails v of
  DataConWrapId {} -> True
  _                -> False
