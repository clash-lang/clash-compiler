{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.
                     2021-2024, QBayLogic B.V.,
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.GHC.GHC2Core
  ( C2C
  , GHC2CoreState
  , GHC2CoreEnv (..)
  , srcSpan
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
import           Control.Lens                ((^.), (%~), (&), (%=), (.~), view, makeLenses)
import           Control.Applicative         ((<|>))
import           Control.Monad.Extra         (ifM, andM)
import           Control.Monad.RWS.Strict    (RWS)
import qualified Control.Monad.RWS.Strict    as RWS
import           Data.Bifunctor              (second)
import           Data.Binary.IEEE754         (doubleToWord, floatToWord)
import qualified Data.ByteString.Char8       as Char8
import           Data.Char                   (isDigit)
import           Data.Hashable               (Hashable (..))
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Maybe                  (catMaybes,fromMaybe,listToMaybe)
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8)
import qualified Data.Traversable            as T
import           Data.String.Interpolate     (__i)
import qualified Text.Read                   as Text
#if MIN_VERSION_ghc(9,4,0)
import           Data.Primitive.ByteArray    (ByteArray(ByteArray))
import qualified GHC.Data.Strict             as GHC
import           GHC.Num.Integer             (integerToBigNatClamp#)
#endif
#if MIN_VERSION_ghc(9,6,0)
import           Language.Haskell.Syntax.Basic (FieldLabelString (..))
#endif

-- GHC API
#if MIN_VERSION_ghc(9,4,0)
import GHC.Core.Reduction (Reduction(Reduction))
#endif
#if MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Types (falseDataCon)
import GHC.Core.Coercion.Axiom
  (CoAxiom (co_ax_branches), CoAxBranch (cab_lhs,cab_rhs), fromBranches)
import GHC.Core.Coercion (Role (Nominal), coercionType, coercionKind)
import GHC.Core.FVs  (exprSomeFreeVars)
import GHC.Core
  (AltCon (..), Bind (..), CoreExpr, Expr (..), Unfolding (..),
#if MIN_VERSION_ghc(9,2,0)
   Alt(..),
#else
   Tickish (..),
#endif
   collectArgs, rhssOfAlts, unfoldingTemplate)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.Tickish (GenTickish (..))
#endif
import GHC.Core.DataCon
  (DataCon, dataConExTyCoVars, dataConName, dataConRepArgTys, dataConTag,
   dataConTyCon, dataConUnivTyVars, dataConWorkId, dataConFieldLabels, flLabel,
   HsImplBang(..), dataConImplBangs)
import GHC.Core.FamInstEnv
  (FamInst (..), FamInstEnvs, familyInstances, normaliseType, emptyFamInstEnvs)
import GHC.Data.FastString (unpackFS, bytesFS)
import GHC.Types.Id (isDataConId_maybe)
import GHC.Types.Id.Info (IdDetails (..), unfoldingInfo)
import GHC.Types.Literal (Literal (..), LitNumType (..), literalType)
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Types.Name
  (Name, nameModule_maybe, nameOccName, nameUnique, getSrcSpan)
import GHC.Builtin.Names  (integerTyConKey, naturalTyConKey)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Data.Pair (Pair (..))
import GHC.Types.SrcLoc (SrcSpan (..), isGoodSrcSpan)
import GHC.Core.TyCon
  (AlgTyConRhs (..), TyCon, tyConName, algTyConRhs, isAlgTyCon, isFamilyTyCon,
   isNewTyCon, isPrimTyCon, isTupleTyCon,
   isClosedSynFamilyTyConWithAxiom_maybe, expandSynTyCon_maybe, tyConArity,
   tyConDataCons, tyConKind, tyConName, tyConUnique, isClassTyCon, isPromotedDataCon_maybe)
#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.TyCon (ExpandSynResult (..))
import GHC.Core.Type (tyConAppFunTy_maybe)
#else
import GHC.Core.TyCon (isFunTyCon)
#endif
import GHC.Core.Type (mkTvSubstPrs, substTy, coreView)
import GHC.Core.TyCo.Rep (Coercion (..), TyLit (..), Type (..), scaledThing)
import GHC.Types.Unique (Uniquable (..), Unique, getKey, hasKey)
import GHC.Types.Var
  (Id, TyVar, Var, VarBndr (..), idDetails, isTyVar, varName, varType,
   varUnique, idInfo, isGlobalId)
import GHC.Types.Var.Set (isEmptyVarSet)
#else
import CoAxiom    (CoAxiom (co_ax_branches), CoAxBranch (cab_lhs,cab_rhs),
                   fromBranches, Role (Nominal))
import Coercion   (coercionType,coercionKind)
import CoreFVs    (exprSomeFreeVars)
import CoreSyn
  (AltCon (..), Bind (..), CoreExpr, Expr (..), Unfolding (..), Tickish (..),
   collectArgs, rhssOfAlts, unfoldingTemplate)
import TysWiredIn (falseDataCon)
import DataCon    (DataCon, HsImplBang(..),
                   dataConExTyCoVars,
                   dataConName, dataConRepArgTys,
                   dataConTag, dataConTyCon,
                   dataConUnivTyVars, dataConWorkId,
                   dataConFieldLabels, flLabel, dataConImplBangs)
import FamInstEnv (FamInst (..), FamInstEnvs,
                   familyInstances, normaliseType, emptyFamInstEnvs)

import FastString (unpackFS, bytesFS)

import Id         (isDataConId_maybe)
import IdInfo     (IdDetails (..), unfoldingInfo)
import Literal    (Literal (..), LitNumType (..))
import Literal    (literalType)
import Module     (moduleName, moduleNameString)
import Name       (Name, nameModule_maybe,
                   nameOccName, nameUnique, getSrcSpan)
import PrelNames  (integerTyConKey, naturalTyConKey)
import OccName    (occNameString)
import Pair       (Pair (..))
import SrcLoc     (SrcSpan (..), isGoodSrcSpan)
import TyCon      (AlgTyConRhs (..), TyCon, tyConName,
                   algTyConRhs, isAlgTyCon, isFamilyTyCon,
                   isFunTyCon, isNewTyCon, isPromotedDataCon_maybe,
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
import Var        (VarBndr (..))
import VarSet     (isEmptyVarSet)
#endif

-- Local imports
import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Annotations.SynthesisAttributes (Annotate, Attr(..))
import qualified Clash.Core.DataCon          as C
import qualified Clash.Core.Literal          as C
import qualified Clash.Core.Name             as C
import qualified Clash.Core.Pretty           as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.TyCon            as C
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Util             as C (undefinedTy, undefinedXPrims)
import qualified Clash.Core.Var              as C
import qualified Clash.Data.UniqMap          as C
import           Clash.Normalize.Primitives  as C
import           Clash.Primitives.Types      hiding (name)
import           Clash.Unique                (fromGhcUnique)
import           Clash.Util
import           Clash.GHC.Util

instance Hashable Name where
  hashWithSalt s = hashWithSalt s . getKey . nameUnique

data GHC2CoreState
  = GHC2CoreState
  { _tyConMap :: C.UniqMap TyCon
  , _nameMap  :: HashMap Name Text
  }

makeLenses ''GHC2CoreState

data GHC2CoreEnv
  = GHC2CoreEnv
  { _srcSpan :: SrcSpan
  , _famInstEnvs :: FamInstEnvs
  }

makeLenses ''GHC2CoreEnv

emptyGHC2CoreState :: GHC2CoreState
emptyGHC2CoreState = GHC2CoreState mempty HashMap.empty

newtype SrcSpanRB = SrcSpanRB {unSrcSpanRB :: SrcSpan}

instance Semigroup SrcSpanRB where
  (SrcSpanRB l) <> (SrcSpanRB r) =
    if   isGoodSrcSpan r
    then SrcSpanRB r
    else SrcSpanRB l

instance Monoid SrcSpanRB where
  mempty = SrcSpanRB noSrcSpan

type C2C = RWS GHC2CoreEnv SrcSpanRB GHC2CoreState

makeAllTyCons
  :: GHC2CoreState
  -> FamInstEnvs
  -> C.UniqMap C.TyCon
makeAllTyCons hm fiEnvs = go hm hm
  where
    go old new
        | C.null (new ^. tyConMap) = mempty
        | otherwise                = tcm <> tcm'
      where
        (tcm,old', _) = RWS.runRWS (T.mapM makeTyCon (new ^. tyConMap))
                                   (GHC2CoreEnv noSrcSpan fiEnvs)
                                   old
        tcm'          = go old' (old' & tyConMap %~ (`C.difference` (old ^. tyConMap)))

makeTyCon :: TyCon
          -> C2C C.TyCon
makeTyCon tc = tycon
  where
    tycon
      | isFamilyTyCon tc    = mkFunTyCon
      | isTupleTyCon tc     = mkTupleTyCon
      | isAlgTyCon tc       = mkAlgTyCon
      | isPrimTyCon tc      = mkPrimTyCon
      | Just dc <- isPromotedDataCon_maybe tc = mkPromotedDataCon dc
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
            Nothing -> do
                       instances <- familyInstances <$> view famInstEnvs <*> pure tc
                       mapM famInstToSubst instances
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
          case tyConDataCons tc of
            dc:_ -> do
              tcDc   <- fmap (C.DataTyCon . (:[])) (coreToDataCon dc)
              return
                C.AlgTyCon
                { C.tyConUniq   = C.nameUniq tcName
                , C.tyConName   = tcName
                , C.tyConKind   = tcKind
                , C.tyConArity  = tcArity
                , C.algTcRhs    = tcDc
                , C.isClassTc   = isClassTyCon tc
                }
            _ -> error "impossible"

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

        mkPromotedDataCon dc = do
          tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          tcData <- coreToDataCon dc

          return
            C.PromotedDataCon
            { C.tyConUniq   = C.nameUniq tcName
            , C.tyConName   = tcName
            , C.tyConKind   = tcKind
            , C.tyConArity  = tcArity
            , C.tyConData   = tcData
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
  DataTyCon {data_cons = dcs} -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
  SumTyCon dcs _ -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs

  NewTyCon dc _ (rhsTvs,rhsEtad) _ _ ->
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
                                  (GHC2CoreEnv noSrcSpan emptyFamInstEnvs)
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
        go "Clash.Magic.clashSimulation" _
          = C.Data <$> coreToDataCon falseDataCon
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
    term' (Lit l@LitRubbish{}) = do
      ty <- coreToType (literalType l)
      return (C.Prim (C.PrimInfo (pack "_RUBBISH_")
                                 ty
                                 C.WorkNever
                                 C.SingleResult
                                 C.NoUnfolding))
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
      return (C.Let (C.NonRec x' e1') e2')

    term' (Let (Rec xes) e) = do
      xes' <- mapM go xes
      e'   <- term e
      return (C.Let (C.Rec xes') e')
     where
      go (x,b) = do
        (b',sp) <- termSP (getSrcSpan x) b
        x' <- coreToIdSP sp x
        return (x',b')

    term' (Case s _ ty [])  = do
      s'  <- term' s
      ty' <- coreToType ty
      case C.collectArgs s' of
        (C.Prim p, _) | C.primName p `elem` C.undefinedXPrims ->
          -- GHC translates things like:
          --
          --   xToBV (Index.pack# (errorX @TY "QQ"))
          --
          -- to
          --
          --   xToBV (case (errorX @TY "QQ") of {})
          --
          --
          -- Here we then translate
          --
          --   case (errorX @TY "QQ") of {}
          --
          -- to
          --
          --   undefinedX @TY
          --
          -- So that the evaluator rule for 'xToBV' can recognize things that
          -- would normally throw XException
          return (C.TyApp (C.Prim C.undefinedX) ty')
        _ ->
          return (C.TyApp (C.Prim C.undefined) ty')

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
        return (C.Let (C.NonRec b' e') ct)
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
#if MIN_VERSION_ghc(9,4,0)
      C.Tick (C.SrcSpan (RealSrcSpan rsp GHC.Nothing)) <$>
             addUsefull (RealSrcSpan rsp GHC.Nothing) (term e)
#elif MIN_VERSION_ghc(9,0,0)
      C.Tick (C.SrcSpan (RealSrcSpan rsp Nothing)) <$>
             addUsefull (RealSrcSpan rsp Nothing) (term e)
#else
      C.Tick (C.SrcSpan (RealSrcSpan rsp)) <$> addUsefull (RealSrcSpan rsp) (term e)
#endif
    term' (Tick _ e) = term e
    term' (Type t) =
      C.TyApp (C.Prim (C.PrimInfo (pack "_TY_") C.undefinedTy C.WorkNever C.SingleResult C.NoUnfolding))
        <$> coreToType t
    term' (Coercion co) =
      C.TyApp (C.Prim (C.PrimInfo (pack "_CO_") C.undefinedTy C.WorkNever C.SingleResult C.NoUnfolding))
        <$> coreToType (coercionType co)


    termSP sp = fmap (second unSrcSpanRB) . RWS.listen . addUsefullR sp . term
    coreToIdSP sp = RWS.local (\r@(GHC2CoreEnv _ e) ->
                                  if isGoodSrcSpan sp then
                                    GHC2CoreEnv sp e
                                  else
                                    r)
                  . coreToId


    lookupPrim :: Text -> Maybe (Maybe CompiledPrimitive)
    lookupPrim nm = extractPrim <$> HashMap.lookup nm primMap

    var x = do
        xPrim <- if isGlobalId x then coreToPrimVar x else coreToVar x
        let xNameS = C.nameOcc xPrim
        xType  <- coreToType (varType x)
        case isDataConId_maybe x of
          Just dc -> case lookupPrim xNameS of
            Just p  ->
              -- Primitive will be marked MultiResult in Transformations if it
              -- is a multi result primitive.
              return $ C.Prim (C.PrimInfo xNameS xType (maybe C.WorkVariable workInfo p) C.SingleResult C.NoUnfolding)
            Nothing -> if isDataConWrapId x && not (isNewTyCon (dataConTyCon dc))
              then let xInfo = idInfo x
                       unfolding = unfoldingInfo xInfo
                   in  case unfolding of
                          CoreUnfolding {} -> do
                            sp <- view srcSpan
                            RWS.censor (const (SrcSpanRB sp)) (term (unfoldingTemplate unfolding))
                          NoUnfolding -> error ("No unfolding for DC wrapper: " ++ showPprUnsafe x)
                          _ -> error ("Unexpected unfolding for DC wrapper: " ++ showPprUnsafe x)
              else C.Data <$> coreToDataCon dc
          Nothing -> case lookupPrim xNameS of
            Just (Just (Primitive f wi _))
              | Just n <- parseBundle "bundle" f        -> return (bundleUnbundleTerm (n+1) xType)
              | Just n <- parseBundle "unbundle" f      -> return (bundleUnbundleTerm (n+1) xType)
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
              | f == "GHC.Magic.nospec"                 -> return (idTerm xType)
              | f == "GHC.Magic.runRW#"                 -> return (runRWTerm xType)
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
              -> return (C.Prim (C.PrimInfo xNameS xType wi C.SingleResult C.NoUnfolding))
              | otherwise
              -> do bndr <- coreToId x
                    return (C.Prim (C.PrimInfo xNameS xType wi C.SingleResult (C.Unfolding bndr)))
            Just (Just (BlackBox {workInfo = wi}))
              | x `elem` unlocs
              -> return $ C.Prim (C.PrimInfo xNameS xType wi C.SingleResult C.NoUnfolding)
              | otherwise
              -> do bndr <- coreToId x
                    return (C.Prim (C.PrimInfo xNameS xType wi C.SingleResult (C.Unfolding bndr)))
            Just (Just (BlackBoxHaskell {workInfo = wi}))
              | x `elem` unlocs
              -> return $ C.Prim (C.PrimInfo xNameS xType wi C.SingleResult C.NoUnfolding)
              | otherwise
              -> do bndr <- coreToId x
                    return $ C.Prim (C.PrimInfo xNameS xType wi C.SingleResult (C.Unfolding bndr))
            Just Nothing ->
              -- Was guarded by "DontTranslate". We don't know yet if Clash will
              -- actually use it later on, so we don't err here.
              return $ C.Prim (C.PrimInfo xNameS xType C.WorkVariable C.SingleResult C.NoUnfolding)
            Nothing
              | x `elem` unlocs
              -> return (C.Prim (C.PrimInfo xNameS xType C.WorkVariable C.SingleResult C.NoUnfolding))
              | otherwise
              -> C.Var <$> coreToId x

#if MIN_VERSION_ghc(9,2,0)
    alt _   (Alt DEFAULT      _  e) = (C.DefaultPat,) <$> term e
    alt _   (Alt (LitAlt l)   _  e) = (C.LitPat (coreToLiteral l),) <$> term e
    alt sp0 (Alt (DataAlt dc) xs e) = case span isTyVar xs of
#else
    alt _   (DEFAULT   , _ , e) = (C.DefaultPat,) <$> term e
    alt _   (LitAlt l  , _ , e) = (C.LitPat (coreToLiteral l),) <$> term e
    alt sp0 (DataAlt dc, xs, e) = case span isTyVar xs of
#endif
      (tyvs,tmvs) -> do
        (e',sp1) <- termSP sp0 e
        (,) <$> (C.DataPat <$> coreToDataCon dc
                           <*> mapM coreToTyVar tyvs
                           <*> mapM (coreToIdSP sp1) tmvs)
            <*> pure e'

    coreToLiteral :: Literal
                  -> C.Literal
    coreToLiteral l = case l of
      LitString  fs  -> C.StringLiteral (Char8.unpack fs)
      LitChar    c   -> C.CharLiteral c
      LitRubbish{}   ->
        error $ "coreToTerm: Encountered LibRubbish. This is a bug in Clash. "
             ++ "Report on https://github.com/clash-lang/clash-compiler/issues."
#if MIN_VERSION_ghc(9,0,0)
      LitNumber lt i -> case lt of
#else
      LitNumber lt i _ -> case lt of
#endif
#if MIN_VERSION_ghc(9,4,0)
        LitNumBigNat  -> C.ByteArrayLiteral (ByteArray (integerToBigNatClamp# i))
#else
        LitNumInteger -> C.IntegerLiteral i
        LitNumNatural -> C.NaturalLiteral i
#endif
        LitNumInt     -> C.IntLiteral i
        LitNumInt64   -> C.Int64Literal i
        LitNumWord    -> C.WordLiteral i
        LitNumWord64  -> C.Word64Literal i
#if MIN_VERSION_ghc(9,2,0)
        LitNumInt8    -> C.Int8Literal i
        LitNumInt16   -> C.Int16Literal i
        LitNumInt32   -> C.Int32Literal i
        LitNumWord8   -> C.Word8Literal i
        LitNumWord16  -> C.Word16Literal i
        LitNumWord32  -> C.Word32Literal i
#endif
      LitFloat r    -> C.FloatLiteral . floatToWord $ fromRational r
      LitDouble r   -> C.DoubleLiteral . doubleToWord $ fromRational r
      LitNullAddr   -> C.StringLiteral []
#if MIN_VERSION_ghc(9,12,0)
      LitLabel fs _ -> C.StringLiteral (unpackFS fs)
#else
      LitLabel fs _ _ -> C.StringLiteral (unpackFS fs)
#endif

addUsefull :: SrcSpan
           -> C2C a
           -> C2C a
addUsefull x m =
  if isGoodSrcSpan x
  then do a <- RWS.local (srcSpan .~ x) m
          RWS.tell (SrcSpanRB x)
          return a
  else m

addUsefullR :: SrcSpan
            -> C2C a
            -> C2C a
addUsefullR x m =
  if isGoodSrcSpan x
  then RWS.local (srcSpan .~ x) m
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
#if MIN_VERSION_ghc(9,10,0)
hasPrimCo (ForAllCo {fco_body = co}) = hasPrimCo co
#else
hasPrimCo (ForAllCo _ _ co) = hasPrimCo co
#endif

#if !MIN_VERSION_ghc(9,12,0)
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
#endif

hasPrimCo (SymCo co) = hasPrimCo co

hasPrimCo (TransCo co1 co2) = do
  tc1M <- hasPrimCo co1
  case tc1M of
    Just _ -> return tc1M
    _ -> hasPrimCo co2

#if MIN_VERSION_ghc(9,12,0)
hasPrimCo (AxiomCo _ coers) = do
#else
hasPrimCo (AxiomRuleCo _ coers) = do
#endif
  tcs <- catMaybes <$> mapM hasPrimCo coers
  return (listToMaybe tcs)

#if MIN_VERSION_ghc(9,6,0)
hasPrimCo (SelCo _ co) = hasPrimCo co
#else
hasPrimCo (NthCo _ _ co)  = hasPrimCo co
#endif
hasPrimCo (LRCo _ co)   = hasPrimCo co
hasPrimCo (InstCo co _) = hasPrimCo co
hasPrimCo (SubCo co)    = hasPrimCo co

hasPrimCo (Refl {}) = return Nothing
hasPrimCo (GRefl {}) = return Nothing
hasPrimCo (FunCo {}) = return Nothing
hasPrimCo (CoVarCo {}) = return Nothing
hasPrimCo (UnivCo {}) = return Nothing
hasPrimCo (KindCo {}) = return Nothing
hasPrimCo (HoleCo {}) = return Nothing

coreToDataCon :: DataCon
              -> C2C C.DataCon
coreToDataCon dc = do
#if MIN_VERSION_ghc(9,0,0)
    repTys <- mapM (coreToType . scaledThing) (dataConRepArgTys dc)
#else
    repTys <- mapM coreToType (dataConRepArgTys dc)
#endif
    dcTy   <- coreToType (varType $ dataConWorkId dc)
    mkDc dcTy repTys
  where
    mkDc dcTy repTys = do
#if MIN_VERSION_ghc(9,6,0)
      let decLabel = decodeUtf8 . bytesFS . field_label . flLabel
#else
      let decLabel = decodeUtf8 . bytesFS . flLabel
#endif
      let repBangs = fmap hsImplBangToBool (dataConImplBangs dc)
      let fLabels  = map decLabel (dataConFieldLabels dc)

      nm   <- coreToName dataConName getUnique qualifiedNameString dc
      uTvs <- mapM coreToTyVar (dataConUnivTyVars dc)
      eTvs <- mapM coreToTyVar (dataConExTyCoVars dc)
      return $ C.MkData
             { C.dcName        = nm
             , C.dcUniq        = C.nameUniq nm
             , C.dcTag         = dataConTag dc
             , C.dcType        = dcTy
             , C.dcArgTys      = repTys
             , C.dcArgStrict   = repBangs
             , C.dcUnivTyVars  = uTvs
             , C.dcExtTyVars   = eTvs
             , C.dcFieldLabels = fLabels
             }

hsImplBangToBool :: HsImplBang -> C.DcStrictness
hsImplBangToBool HsLazy = C.Lazy
hsImplBangToBool HsStrict{} = C.Strict
hsImplBangToBool HsUnpack{} = C.Strict

typeConstructorToString
  :: TyCon
  -> C2C String
typeConstructorToString constructor =
   Text.unpack . C.nameOcc <$> coreToName tyConName tyConUnique qualifiedNameString constructor

-- | Flatten a list type structure to a list of types.
listTypeToListOfTypes :: Type -> [Type]
-- TyConApp ': [kind, head, tail]
listTypeToListOfTypes (TyConApp _ [_, a, as]) = a : listTypeToListOfTypes as
listTypeToListOfTypes ty                      =
  case coreView ty of
    Nothing -> []
    Just ty' -> listTypeToListOfTypes ty'

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
                  , showPprUnsafe s ]

-- | Returns string of (LitTy (StrTyLit s)) construction.
tyLitToString :: Type -> String
tyLitToString (LitTy (StrTyLit s)) = unpackFS s
tyLitToString s = error $ unwords [ "Could not unpack given type to string:"
                                  , showPprUnsafe s ]

-- | Returns string in Text form of (LitTy (StrTyLit s)) construction.
tyLitToText :: Type -> Text
tyLitToText = Text.pack . tyLitToString

-- | Returns integer of (LitTy (NumTyLit n)) construction.
tyLitToInteger :: Type -> Integer
tyLitToInteger (LitTy (NumTyLit n)) = n
tyLitToInteger s = error $ unwords [ "Could not unpack given type to integer:"
                                   , showPprUnsafe s ]

-- | Try to interpret a Type as an Attr
coreToAttr :: Type -> C2C (Attr Text)
coreToAttr t0@(TyConApp ty args) = do
  name <- typeConstructorToString ty
  envs <- view famInstEnvs
  let
    -- XXX: This relies on 'value' not being evaluated if the constructor
    --      doesn't have a second field.
    key = args !! 1
    value = args !! 2
#if MIN_VERSION_ghc(9,4,0)
  let Reduction _ key1 = normaliseType envs Nominal key
      Reduction _ value1 = normaliseType envs Nominal value
#else
  let (_,key1) = normaliseType envs Nominal key
      (_,value1) = normaliseType envs Nominal value
#endif
  if
    | name == show 'StringAttr ->
      return $ StringAttr (tyLitToText key1) (tyLitToText value1)
    | name == show 'IntegerAttr ->
      return $ IntegerAttr (tyLitToText key1) (tyLitToInteger value1)
    | name == show 'BoolAttr -> do
      bool <- boolTypeToBool value1
      return $ BoolAttr (tyLitToText key1) bool
    | name == show 'Attr ->
      return $ Attr (tyLitToText key1)
    | otherwise ->
      case coreView t0 of
        Just t1 -> coreToAttr t1
        Nothing -> error $ [__i|Expected constructor of Attr, got #{name}|]
coreToAttr t0 =
  case coreView t0 of
    Just t1 -> coreToAttr t1
    Nothing -> error $ [__i|Expected constructor of Attr, got #{showPprUnsafe t0}|]

coreToAttrs' :: [Type] -> C2C [Attr Text]
coreToAttrs' [k, a, attrs] = do
  -- We expect three type arguments:
  --
  --  k: either @Attr@ or @[Attr]@
  --  a: type being annotated
  --  attrs: attribute or list of attributes
  --
  attrs1 <- tryList
  attrs2 <- tryAttr
  case attrs1 <|> attrs2 of
    Just theseAttrs -> do
      subAttrs <- coreToAttrs a
      pure (theseAttrs <> subAttrs)
    Nothing ->
      error [__i|
        Expected either an attribute or a list of attributes, got:

          #{showPprUnsafe k}
      |]
 where

  isListTy = fmap (== show ''[]) . typeConstructorToString
  isAttrTy = fmap (== show ''Attr) . typeConstructorToString

  tryList = case k of
    TyConApp ty0 [TyConApp ty1 _] -> do
      ifM
        (andM [isListTy ty0, isAttrTy ty1])
        (Just <$> traverse coreToAttr (listTypeToListOfTypes attrs))
        (pure Nothing)
    _ -> pure Nothing

  tryAttr = case k of
    TyConApp ty _ -> do
      ifM
        (isAttrTy ty)
        (Just <$> sequence [coreToAttr attrs])
        (pure Nothing)
    _ -> pure Nothing

coreToAttrs' illegal =
  error $ "Unexpected type args to Annotate: " ++ show (map (showPprUnsafe) illegal)

-- | If this type has an annotate type synonym, return list of attributes.
coreToAttrs :: Type -> C2C [Attr Text]
coreToAttrs (TyConApp tycon kindsOrTypes) = do
  name' <- typeConstructorToString tycon

  if name' == show ''Annotate
  then coreToAttrs' kindsOrTypes
  else return []

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
#if MIN_VERSION_ghc(9,6,0)
  | Just (FunTy _ _ ty1 ty2) <- tyConAppFunTy_maybe tc args = C.mkFunTy <$> coreToType ty1 <*> coreToType ty2
#else
  | isFunTyCon tc = foldl C.AppTy (C.ConstTy C.Arrow) <$> mapM coreToType args
#endif
  | otherwise     = case expandSynTyCon_maybe tc args of
#if MIN_VERSION_ghc(9,6,0)
                      ExpandsSyn substs synTy remArgs -> do
#else
                      Just (substs,synTy,remArgs) -> do
#endif
                        let substs' = mkTvSubstPrs substs
                            synTy'  = substTy substs' synTy
                        foldl C.AppTy <$> coreToType synTy' <*> mapM coreToType remArgs
                      _ -> do
                        tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
                        tyConMap %= (C.insert tcName tc)
                        C.mkTyConApp <$> (pure tcName) <*> mapM coreToType args
coreToType' (ForAllTy (Bndr tv _) ty)   = C.ForAllTy <$> coreToTyVar tv <*> coreToType ty
-- TODO: save the distinction between => and ->
#if MIN_VERSION_ghc(9,0,0)
coreToType' (FunTy _ _ ty1 ty2)             = C.mkFunTy <$> coreToType ty1 <*> coreToType ty2
#else
coreToType' (FunTy _ ty1 ty2)             = C.mkFunTy <$> coreToType ty1 <*> coreToType ty2
#endif
coreToType' (LitTy tyLit)    = return $ C.LitTy (coreToTyLit tyLit)
coreToType' (AppTy ty1 ty2)  = C.AppTy <$> coreToType ty1 <*> coreToType' ty2
coreToType' (CastTy t (Refl{})) = coreToType' t
coreToType' t@(CastTy _ _)   = error ("Cannot handle CastTy " ++ showPprUnsafe t)
coreToType' t@(CoercionTy _) = error ("Cannot handle CoercionTy " ++ showPprUnsafe t)

coreToTyLit :: TyLit
            -> C.LitTy
coreToTyLit (NumTyLit i) = C.NumTy (fromInteger i)
coreToTyLit (StrTyLit s) = C.SymTy (unpackFS s)
#if MIN_VERSION_ghc(9,2,0)
coreToTyLit (CharTyLit c) = C.CharTy c
#endif

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
  let key  = fromGhcUnique (toUnique v)
      locI = getSrcSpan (toName v)
      -- Is it one of [ds,ds1,ds2,..]
      isDSX = maybe False (maybe True (isDigit . fst) . Text.uncons) . Text.stripPrefix "ds"
      sort | isDSX ns || Text.isPrefixOf "$" ns
           = C.System
           | otherwise
           = C.User
  locR <- view srcSpan
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
mapSignalTerm (C.ForAllTy aTV (C.ForAllTy bTV (C.ForAllTy clkTV funTy)))
  | (C.FunTy _ funTy'') <- C.tyView funTy
  , (C.FunTy aTy bTy)   <- C.tyView funTy''
  = let
      fName = C.mkUnsafeSystemName "f" 0
      xName = C.mkUnsafeSystemName "x" 1
      fTy   = C.mkFunTy aTy bTy
      fId   = C.mkLocalId fTy fName
      xId   = C.mkLocalId aTy xName
    in
      C.TyLam aTV (
      C.TyLam bTV (
      C.TyLam clkTV (
      C.Lam   fId (
      C.Lam   xId (
      C.App (C.Var fId) (C.Var xId))))))

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
signalTerm (C.ForAllTy aTV (C.ForAllTy domTV funTy))
  | (C.FunTy _ saTy) <- C.tyView funTy
  = let
      xName = C.mkUnsafeSystemName "x" 0
      xId   = C.mkLocalId saTy xName
    in
      C.TyLam aTV (
      C.TyLam domTV (
      C.Lam   xId (
      C.Var   xId)))

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
appSignalTerm (C.ForAllTy domTV (C.ForAllTy aTV (C.ForAllTy bTV funTy)))
  | (C.FunTy _ funTy'') <- C.tyView funTy
  , (C.FunTy saTy sbTy) <- C.tyView funTy''
  = let
      fName = C.mkUnsafeSystemName "f" 0
      xName = C.mkUnsafeSystemName "x" 1
      fTy   = C.mkFunTy saTy sbTy
      fId   = C.mkLocalId fTy fName
      xId   = C.mkLocalId saTy xName
    in
      C.TyLam domTV (
      C.TyLam aTV (
      C.TyLam bTV (
      C.Lam   fId (
      C.Lam   xId (
      C.App (C.Var fId) (C.Var xId))))))

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
vecUnwrapTerm (C.ForAllTy tTV (C.ForAllTy nTV (C.ForAllTy aTV funTy)))
  | (C.FunTy _ vsTy) <- C.tyView funTy
  = let
        vsName = C.mkUnsafeSystemName "vs" 0
        vsId   = C.mkLocalId vsTy vsName
    in
        C.TyLam tTV (
        C.TyLam nTV (
        C.TyLam aTV (
        C.Lam   vsId (
        C.Var vsId))))

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
traverseTerm (C.ForAllTy fTV (C.ForAllTy aTV (C.ForAllTy bTV (C.ForAllTy domTV funTy))))
    | (C.FunTy dictTy funTy1) <- C.tyView funTy
    , (C.FunTy gTy    funTy2) <- C.tyView funTy1
    , (C.FunTy xTy    _)      <- C.tyView funTy2
    = let
        dictName = C.mkUnsafeSystemName "dict" 0
        gName    = C.mkUnsafeSystemName "g" 1
        xName    = C.mkUnsafeSystemName "x" 2
        dictId   = C.mkLocalId dictTy dictName
        gId      = C.mkLocalId gTy gName
        xId      = C.mkLocalId xTy xName
      in
        C.TyLam fTV (
        C.TyLam aTV (
        C.TyLam bTV (
        C.TyLam domTV (
        C.Lam   dictId (
        C.Lam   gId (
        C.Lam   xId (
        C.App (C.Var gId) (C.Var xId))))))))

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
#if MIN_VERSION_ghc(9,8,0)
dollarTerm (C.ForAllTy raTV (C.ForAllTy rbTV (C.ForAllTy aTV (C.ForAllTy bTV funTy))))
  | (C.FunTy fTy funTy'') <- C.tyView funTy
  , (C.FunTy aTy _)       <- C.tyView funTy''
  = let
      fName = C.mkUnsafeSystemName "f" 0
      xName = C.mkUnsafeSystemName "x" 1
      fId   = C.mkLocalId fTy fName
      xId   = C.mkLocalId aTy xName
    in
      C.TyLam raTV (
      C.TyLam rbTV (
      C.TyLam aTV (
      C.TyLam bTV (
      C.Lam   fId (
      C.Lam   xId (
      C.App (C.Var fId) (C.Var xId)))))))
#else
dollarTerm (C.ForAllTy rTV (C.ForAllTy aTV (C.ForAllTy bTV funTy)))
  | (C.FunTy fTy funTy'') <- C.tyView funTy
  , (C.FunTy aTy _)       <- C.tyView funTy''
  = let
      fName = C.mkUnsafeSystemName "f" 0
      xName = C.mkUnsafeSystemName "x" 1
      fId   = C.mkLocalId fTy fName
      xId   = C.mkLocalId aTy xName
    in
      C.TyLam rTV (
      C.TyLam aTV (
      C.TyLam bTV (
      C.Lam   fId (
      C.Lam   xId (
      C.App (C.Var fId) (C.Var xId))))))
#endif

dollarTerm ty = error $ $(curLoc) ++ C.showPpr ty

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
withFrozenCallStackTerm (C.ForAllTy aTV funTy)
  | (C.FunTy callStackTy fTy) <- C.tyView funTy
  = let
      callStackName = C.mkUnsafeSystemName "callStack" 0
      fName         = C.mkUnsafeSystemName "f" 1
      callStackId   = C.mkLocalId callStackTy callStackName
      fId           = C.mkLocalId fTy fName
    in
      C.TyLam  aTV (
      C.Lam    callStackId (
      C.Lam    fId (
      C.App (C.Var fId) (C.Var callStackId))))

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
idTerm (C.ForAllTy aTV funTy)
  | (C.FunTy xTy _) <- C.tyView funTy
  = let
      xName           = C.mkUnsafeSystemName "x" 0
      xId             = C.mkLocalId xTy xName
    in
      C.TyLam aTV (
      C.Lam   xId (
      C.Var xId))

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
runRWTerm (C.ForAllTy rTV (C.ForAllTy oTV funTy))
  | (C.FunTy fTy _)  <- C.tyView funTy
  , (C.FunTy rwTy _) <- C.tyView fTy
  = let
      fName            = C.mkUnsafeSystemName "f" 0
      fId              = C.mkLocalId fTy fName
      rwNm             = pack "GHC.Prim.realWorld#"
    in
      C.TyLam rTV (
      C.TyLam oTV (
      C.Lam   fId (
      (C.App (C.Var fId)
             (C.Prim (C.PrimInfo rwNm rwTy C.WorkNever C.SingleResult C.NoUnfolding))))))

runRWTerm ty = error $ $(curLoc) ++ show ty

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
checkUnpackUndefTerm (C.ForAllTy nTV (C.ForAllTy aTV funTy))
  | C.FunTy knTy r0Ty <- C.tyView funTy
  , C.FunTy tpTy r1Ty <- C.tyView r0Ty
  , C.FunTy fTy _     <- C.tyView r1Ty
  = let
      knName            = C.mkUnsafeSystemName "kn" 0
      tpName            = C.mkUnsafeSystemName "tp" 1
      fName             = C.mkUnsafeSystemName "f" 2
      knId              = C.mkLocalId knTy knName
      tpId              = C.mkLocalId tpTy tpName
      fId               = C.mkLocalId fTy fName
    in
      C.TyLam nTV (
      C.TyLam aTV (
      C.Lam knId (
      C.Lam tpId (
      C.Lam fId (
      C.Var fId)))))

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
nameModTerm sa (C.ForAllTy nmTV (C.ForAllTy aTV funTy))
  | (C.FunTy xTy _) <- C.tyView funTy
  = let
      -- Safe to use `mkUnsafeSystemName` here, because we're building the
      -- identity \x.x, so any shadowing of 'x' would be the desired behavior.
      xName            = C.mkUnsafeSystemName "x" 0
      xId              = C.mkLocalId xTy xName
    in
      C.TyLam nmTV (
      C.TyLam aTV (
      C.Lam   xId (
      (C.Tick (C.NameMod sa (C.VarTy nmTV)) (C.Var xId)))))

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
xToErrorCtxTerm (C.ForAllTy aTV funTy)
  | (C.FunTy ctxTy rTy) <- C.tyView funTy
  , (C.FunTy xTy _)     <- C.tyView rTy
  = let
      -- Safe to use `mkUnsafeSystemName` here, because we're building the
      -- identity \_ x.x, so any shadowing of 'x' would be the desired behavior.
      ctxName = C.mkUnsafeSystemName "ctx" 0
      ctxId   = C.mkLocalId ctxTy ctxName
      xName   = C.mkUnsafeSystemName "x" 1
      xId     = C.mkLocalId xTy xName
    in
      C.TyLam aTV (
      C.Lam ctxId (
      C.Lam xId (
      C.Var xId)))

xToErrorCtxTerm ty = error $ $(curLoc) ++ show ty

isDataConWrapId :: Id -> Bool
isDataConWrapId v = case idDetails v of
  DataConWrapId {} -> True
  _                -> False
