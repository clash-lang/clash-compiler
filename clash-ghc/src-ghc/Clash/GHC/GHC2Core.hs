{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.GHC.GHC2Core
  ( C2C
  , GHC2CoreState
  , GHC2CoreEnv (..)
  , srcSpan
  , famInstEnvs
  , tyConMap
  , coreToName
  , modNameM
  , qualifiedNameString
  , qualifiedNameString'
  , makeAllTyCons
  , emptyGHC2CoreState
  , pendingWarnings
    -- * Types of scoped Core
  , varKey
  , coreToTypeB
  , coreToTyVarB
  , coreToVarTypeB
  , coreToGlobalIdB
  , convertedTyVars
  , withoutSrcSpan
    -- * For "Clash.GHC.Scoped.GHC2Core"
  , SrcSpanRB (..)
  , coreToVar
  , coreToPrimVar
  , coreToDataCon
  , coreToLiteral
  , addUsefull
  , addUsefullR
  , hasPrimCo
  , isSizedCast
  , isDataConWrapId
  )
where

-- External Modules
import           Control.Lens ((^.), (%~), (&), (%=), (.~), use, view, makeLenses)
import           Control.Applicative         ((<|>))
import           Control.Monad.Extra         (ifM, andM)
import           Control.Monad.RWS.Strict    (RWS)
import qualified Control.Monad.RWS.Strict    as RWS
import           GHC.Float                   (castDoubleToWord64, castFloatToWord32)
import qualified Data.ByteString.Char8       as Char8
import           Data.Char                   (isDigit)
import           Data.Hashable               (Hashable (..))
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
#if !MIN_VERSION_base(4,20,0)
import           Data.List                   (foldl')
#endif
import           Data.Maybe                  (catMaybes,fromMaybe,listToMaybe,mapMaybe)
import           Data.Sequence               (Seq, (|>))
import qualified Data.Sequence               as Seq
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Text.Extra             (showt)
import qualified Data.Traversable            as T
import           Data.String.Interpolate     (__i)
import           Data.Primitive.ByteArray    (ByteArray(ByteArray))
import           GHC.Num.Integer             (integerToBigNatClamp#)
import           Language.Haskell.Syntax.Basic (FieldLabelString (..))

-- GHC API
import GHC.Core.Reduction (Reduction(Reduction), HetReduction(..))
import GHC.Core.Coercion.Axiom
  (CoAxiom (co_ax_branches), CoAxBranch (cab_lhs,cab_rhs), fromBranches)
import GHC.Core.Coercion (Role (Nominal))
import GHC.Core.DataCon
  ( DataCon, dataConExTyCoVars, dataConName, dataConRepArgTys, dataConTag,
    dataConUnivTyVars, dataConWorkId, dataConFieldLabels, flLabel,
    HsImplBang(..), dataConImplBangs )
import GHC.Core.FamInstEnv
  ( FamInst (..), FamInstEnvs, familyInstances, normaliseType,
    topReduceTyFamApp_maybe )
import GHC.Data.FastString (FastString, mkFastString, unpackFS, bytesFS)
import GHC.Types.Id.Info (IdDetails (..))
import GHC.Types.Literal (Literal (..), LitNumType (..))
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Types.Name
  (Name, nameModule_maybe, nameOccName, nameUnique, getSrcSpan)
import GHC.Builtin.Names  (integerTyConKey, naturalTyConKey)
import GHC.Types.Name.Occurrence (occNameFS, occNameString)
import GHC.Types.SrcLoc (isGoodSrcSpan)
import GHC.Core.TyCon
  (AlgTyConRhs (..), TyCon, tyConName, algTyConRhs, isAlgTyCon, isFamilyTyCon,
   isPrimTyCon, isTupleTyCon,
   isClosedSynFamilyTyConWithAxiom_maybe, expandSynTyCon_maybe, tyConArity,
   tyConDataCons, tyConKind,
#if MIN_VERSION_ghc(9,14,0)
   tyConTyVars,
   isUnaryClassTyCon,
#endif
   tyConUnique, isClassTyCon,
   isPromotedDataCon_maybe)
import GHC.Core.TyCon (ExpandSynResult (..))
import GHC.Core.Type (tyConAppFunTy_maybe)
import GHC.Core.Type (mkTvSubstPrs, substTy, coreView)
import GHC.Core.TyCo.Rep (Coercion (..), TyLit (..), Type (..), scaledThing)
#if !MIN_VERSION_ghc(9,12,0)
import GHC.Core.Coercion (coercionKind)
import GHC.Data.Pair (Pair (..))
#endif
import GHC.Types.Unique (Uniquable (..), Unique, getKey, hasKey)
import GHC.Types.Var
  ( Id, TyVar, Var, VarBndr (..), idDetails, varName, varType, varUnique,
    isGlobalId )
import GHC.Types.Var.Env (lookupVarEnv, mkVarEnv)
import GHC.Types.Var.Set (elemVarSet, emptyVarSet, extendVarSet)

-- Local imports
import Clash.Core.Basic (ConstTy (..), LitTy (..))
import           Clash.Annotations.SynthesisAttributes (Annotate, Attr(..))
import qualified Clash.Core.DataCon          as C
import qualified Clash.Core.Literal          as C
import qualified Clash.Core.Name             as C
import qualified Clash.Core.TyCon            as C
import qualified Clash.Data.UniqMap          as C
import           Clash.Driver.Types          (ClashOpts, HasClashOpts(..))
import           Clash.Driver.Warning        (CanWarn(..), PendingWarning)
import           Clash.Unique                (fromGhcUnique)
import           Clash.Util
import           Clash.GHC.Util
import           Clash.Core.Scoped.Builder
  (Key, TyB, annTyB, appTyB, constTyB, forAllTyB, funTyB, litTyB, runClosedType,
   runDataConSig, runNewTyConSig, runTyFamEqn, uniqueKey, varTyB)
import Clash.Core.DataCon (mkDataCon)
import Clash.Core.Scoped.Syntax (mkGlobalId)
import           Clash.Core.Scoped.Sig       (TyFamEqn)
import qualified Clash.Core.Scoped.Syntax    as S

instance Hashable Name where
  hashWithSalt s = hashWithSalt s . getKey . nameUnique

-- | A GHC 'Type' keyed for cache lookups: 'Eq' and 'Hashable' compare types
-- structurally instead of up to alpha-equivalence or type synonym expansion.
-- See 'eqType'.
newtype StructuralType = StructuralType Type

instance Eq StructuralType where
  StructuralType l == StructuralType r = eqType l r

instance Hashable StructuralType where
  hashWithSalt salt (StructuralType ty) = hashType salt ty

-- | Structural equality on GHC types: type synonyms stay unexpanded,
-- alpha-equivalent types are distinct, and types containing casts or
-- coercions never compare equal.
eqType :: Type -> Type -> Bool
eqType = go
 where
  go (TyVarTy v1) (TyVarTy v2) = eqVar v1 v2
  go (AppTy l1 r1) (AppTy l2 r2) = go l1 l2 && go r1 r2
  go (TyConApp tc1 args1) (TyConApp tc2 args2) =
    tc1 == tc2 && goList args1 args2
  go (ForAllTy (Bndr v1 f1) t1) (ForAllTy (Bndr v2 f2) t2) =
    eqVar v1 v2 && f1 == f2 && go t1 t2
  go (FunTy f1 m1 a1 r1) (FunTy f2 m2 a2 r2) =
    f1 == f2 && go m1 m2 && go a1 a2 && go r1 r2
  go (LitTy l1) (LitTy l2) = l1 == l2
  -- XXX: Not sure how to handle casts/coercions, so for now just claim they're
  --      not equal. That's fair in the only context this is used: caching.
  go _ _ = False

  -- GHC's 'Eq' on 'Var' and on 'Name' only compares uniques, and a unique only
  -- identifies a variable within one scope. We want to use it for cache lookups,
  -- so we should also take name and type differences into account!
  eqVar v1 v2 =
    v1 == v2 && eqVarName (varName v1) (varName v2)
             && go (varType v1) (varType v2)

  -- 'coreToName' reads a name's occurrence name and module (as a qualified
  -- string, which also decides its 'C.NameSort') and its source span
  eqVarName n1 n2 =
    nameOccName n1 == nameOccName n2
      && nameModule_maybe n1 == nameModule_maybe n2
      && getSrcSpan n1 == getSrcSpan n2

  goList (t1:ts1) (t2:ts2) = go t1 t2 && goList ts1 ts2
  goList [] [] = True
  goList _ _ = False

-- | Hash a GHC type, matching 'eqType': types that compare equal hash equally.
hashType :: Int -> Type -> Int
hashType = go
 where
  go s ty = case ty of
    TyVarTy v -> hashVar (tag s 0) v
    AppTy l r -> go (go (tag s 1) l) r
    TyConApp tc args -> foldl go (hashWithSalt (tag s 2) (getKey (tyConUnique tc))) args
    ForAllTy (Bndr v _) t -> go (hashVar (tag s 3) v) t
    FunTy _ m a r -> go (go (go (tag s 4) m) a) r
    LitTy l -> case l of
      NumTyLit i -> hashWithSalt (tag s 5) i
      StrTyLit str -> hashWithSalt (tag s 6) (unpackFS str)
      CharTyLit c -> hashWithSalt (tag s 7) c
    CastTy t _ -> go (tag s 8) t
    CoercionTy _ -> tag s 9

  -- Matching 'eqType', see comment there. Deliberately coarser than 'eqVar':
  -- it skips the name, which only costs the odd collision. The hash law needs
  -- equal values to hash equally, not the other way around.
  hashVar s v = go (hashWithSalt s (getKey (varUnique v))) (varType v)

  tag :: Int -> Int -> Int
  tag = hashWithSalt

data GHC2CoreState
  = GHC2CoreState
  { _tyConMap :: C.UniqMap TyCon
  , _nameMap  :: HashMap Name Text
  , _scopedTypeCache :: HashMap StructuralType TyB
  -- ^ Cache for types converted to scoped Core, keyed on the GHC type. See
  -- 'coreToTypeB'.
  , _scopedVarTypeMap :: HashMap Name (S.Type 'S.VoidS)
  -- ^ Cache for the types of global variables, converted to scoped Core. See
  -- 'coreToVarTypeB'.
  , _dataConCache :: C.UniqMap C.DataCon
  -- ^ Cache for converted data constructors, by unique. See 'coreToDataCon'.
  , _pendingWarnings :: Seq PendingWarning
  -- ^ Warnings collected while converting, in the order they were reported.
  -- 'C2C' is pure, so it cannot print them itself; the driver reports these
  -- once the conversion is done. See the 'HasClashOpts' instance below.
  }

makeLenses ''GHC2CoreState

data GHC2CoreEnv
  = GHC2CoreEnv
  { _srcSpan :: SrcSpan
  , _famInstEnvs :: FamInstEnvs
  , _clashOpts :: ClashOpts
  }

makeLenses ''GHC2CoreEnv

instance HasClashOpts C2C where
  askClashOpts = view clashOpts

-- | 'C2C' is a pure monad, so it cannot print warnings itself: it collects
-- them in 'GHC2CoreState' for the driver to report.
instance CanWarn C2C where
  reportWarning pw = pendingWarnings %= (|> pw)

emptyGHC2CoreState :: GHC2CoreState
emptyGHC2CoreState =
  GHC2CoreState mempty HashMap.empty HashMap.empty HashMap.empty mempty Seq.empty

newtype SrcSpanRB = SrcSpanRB {unSrcSpanRB :: SrcSpan}

instance Semigroup SrcSpanRB where
  (SrcSpanRB l) <> (SrcSpanRB r) =
    if   isGoodSrcSpan r
    then SrcSpanRB r
    else SrcSpanRB l

instance Monoid SrcSpanRB where
  mempty = SrcSpanRB noSrcSpan

type C2C = RWS GHC2CoreEnv SrcSpanRB GHC2CoreState

-- | Convert the type constructors that the conversion of terms found, and
-- those that converting these finds, and so on. Their types are built from
-- GHC's types ('coreToTypeB').
makeAllTyCons
  :: ClashOpts
  -> GHC2CoreState
  -> FamInstEnvs
  -> C.UniqMap C.TyCon
makeAllTyCons opts hm fiEnvs = go hm hm
  where
    go old new
        | C.null (new ^. tyConMap) = mempty
        | otherwise                = tcm <> tcm'
      where
        (tcm,old', _) = RWS.runRWS (T.mapM makeTyCon (new ^. tyConMap))
                                   (GHC2CoreEnv noSrcSpan fiEnvs opts)
                                   old
        tcm'          = go old' (old' & tyConMap %~ (`C.difference` (old ^. tyConMap)))

-- | Convert a type constructor. Like 'makeTyConClash', which converts through
-- the types of Clash Core, and looks up the same names in the same order.
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
      | otherwise           = mkPrimTyCon
      where
        tcArity = tyConArity tc
        tcName = coreToName tyConName tyConUnique qualifiedNameString tc
        tcKind = runClosedType <$> coreToTypeB (tyConKind tc)

        mkAlgTyCon = do
          nm <- tcName
          k <- tcKind
          tcRhsM <- makeAlgTyConRhs tc $ algTyConRhs tc
          case tcRhsM of
            Just tcRhs -> return (C.mkAlgTyCon nm k tcArity tcRhs (isClassTyCon tc))
            Nothing -> return (C.mkPrimTyCon nm k tcArity)

        mkFunTyCon = do
          nm <- tcName
          k <- tcKind
          envs <- view famInstEnvs
          eqns <- case isClosedSynFamilyTyConWithAxiom_maybe tc of
            Nothing -> mapM (\fi -> tyFamEqnB envs (fi_tys fi) (fi_rhs fi))
                            (familyInstances envs tc)
            Just cx -> mapM (\b -> tyFamEqnB envs (cab_lhs b) (cab_rhs b))
                            (fromBranches (co_ax_branches cx))
          return (C.mkFunTyCon nm k tcArity eqns)

        mkTupleTyCon = do
          nm <- tcName
          k <- tcKind
          case tyConDataCons tc of
            dc:_ -> do
              tcDc <- coreToDataCon dc
              return (C.mkAlgTyCon nm k tcArity (C.DataTyCon [tcDc]) (isClassTyCon tc))
            _ -> error "impossible"

        mkPrimTyCon = do
          nm <- tcName
          k <- tcKind
          return (C.mkPrimTyCon nm k tcArity)

        mkPromotedDataCon dc = do
          nm <- tcName
          k <- tcKind
          tcData <- coreToDataCon dc
          return (C.mkPromotedDataCon nm k tcArity tcData)

-- | An equation of a type family, given its left-hand side and right-hand
-- side. Its pattern variables are the free type variables of the converted
-- types, ordered like 'C.mkFunTyConFromClash' orders them.
tyFamEqnB :: FamInstEnvs -> [Type] -> Type -> C2C TyFamEqn
tyFamEqnB envs lhs rhs = do
  lhs' <- mapM coreToTypeB lhs
  rhs' <- coreToTypeB rhs
  tvs <- withoutSrcSpan (mapM coreToTyVarB (dependencySort (nubVars allTvs)))
  pure (runTyFamEqn tvs lhs' rhs')
 where
  allTvs = concatMap (convertedTyVars envs) (lhs ++ [rhs])

  nubVars = go emptyVarSet
   where
    go _ [] = []
    go seen (v:vs)
      | v `elemVarSet` seen = go seen vs
      | otherwise = v : go (extendVarSet seen v) vs

  -- See 'C.dependencySortTyVars': every variable after those in its kind
  dependencySort tvs = reverse (snd (foldl' visit (emptyVarSet, []) tvs))
   where
    tvEnv = mkVarEnv [ (tv, tv) | tv <- tvs ]
    visit (seen, acc) tv
      | tv `elemVarSet` seen = (seen, acc)
      | otherwise =
          let kindTvs = mapMaybe (lookupVarEnv tvEnv) (convertedTyVars envs (varType tv))
              (seen1, acc1) = foldl' visit (extendVarSet seen tv, acc) kindTvs
          in  (seen1, tv : acc1)

-- | Convert the right-hand side of an algebraic data type. See
-- 'makeAlgTyConRhsClash'.
makeAlgTyConRhs :: TyCon
                -> AlgTyConRhs
                -> C2C (Maybe C.AlgTyConRhs)
makeAlgTyConRhs _tc algTcRhs = case algTcRhs of
#if MIN_VERSION_ghc(9,14,0)
  -- See 'makeAlgTyConRhsClash'
  UnaryClassTyCon {data_con = dc} -> do
    let tvs = tyConTyVars _tc
        rhsEtad = case dataConRepArgTys dc of
          [scaled] -> scaledThing scaled
          args -> error [__i|
            makeAlgTyConRhs: UnaryClassTyCon DataCon does not have one field
              Type constructor: #{showPprUnsafe _tc}
              Data constructor: #{showPprUnsafe dc}
              Representation argument types (#{length args}): #{showPprUnsafe args}
            |]
    Just <$> newTyConRhs dc tvs rhsEtad
#endif
  DataTyCon {data_cons = dcs} -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
  SumTyCon dcs _ -> Just <$> C.DataTyCon <$> mapM coreToDataCon dcs
  NewTyCon dc _ (rhsTvs,rhsEtad) _ _ -> Just <$> newTyConRhs dc rhsTvs rhsEtad
  AbstractTyCon {} -> return Nothing
  TupleTyCon {}    -> error "Cannot handle tuple tycons"
 where
  newTyConRhs dc tvs rhsEtad =
    C.mkNewTyConRhs <$> coreToDataCon dc
                    <*> (runNewTyConSig <$> mapM coreToTyVarB tvs <*> coreToTypeB rhsEtad)

coreToLiteral :: Literal
              -> C.Literal
coreToLiteral l = case l of
  LitString  fs  -> C.StringLiteral (Char8.unpack fs)
  LitChar    c   -> C.CharLiteral c
  LitRubbish{}   ->
    error $ "coreToTerm: Encountered LibRubbish. This is a bug in Clash. "
         ++ "Report on https://github.com/clash-lang/clash-compiler/issues."
  LitNumber lt i -> case lt of
    LitNumBigNat  -> C.ByteArrayLiteral (ByteArray (integerToBigNatClamp# i))
    LitNumInt     -> C.IntLiteral i
    LitNumInt64   -> C.Int64Literal i
    LitNumWord    -> C.WordLiteral i
    LitNumWord64  -> C.Word64Literal i
    LitNumInt8    -> C.Int8Literal i
    LitNumInt16   -> C.Int16Literal i
    LitNumInt32   -> C.Int32Literal i
    LitNumWord8   -> C.Word8Literal i
    LitNumWord16  -> C.Word16Literal i
    LitNumWord32  -> C.Word32Literal i
  LitFloat r    -> C.FloatLiteral . castFloatToWord32 $ fromRational r
  LitDouble r   -> C.DoubleLiteral . castDoubleToWord64 $ fromRational r
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

-- | Convert without an ambient source span: names GHC gives no location of
-- their own get 'noSrcSpan' instead of the location of the binder under
-- conversion.
withoutSrcSpan :: C2C a -> C2C a
withoutSrcSpan = RWS.local (srcSpan .~ noSrcSpan)

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

hasPrimCo (SelCo _ co) = hasPrimCo co
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

-- | Convert a data constructor, building its types from GHC's types
-- ('coreToTypeB').
--
-- Memoized on the data constructor, so that its occurrences share one
-- 'C.DataCon'. It converts without a source span (see 'withoutSrcSpan'), so
-- that the names in it don't depend on the occurrence that converts it, and
-- are those of its type constructor in the 'C.TyConMap' ('makeAllTyCons').
coreToDataCon :: DataCon
              -> C2C C.DataCon
coreToDataCon dc = makeCachedU (fromGhcUnique (getUnique dc)) dataConCache $ withoutSrcSpan $ do
    repTys <- mapM (coreToTypeB . scaledThing) (dataConRepArgTys dc)
    dcTy   <- coreToTypeB (varType $ dataConWorkId dc)
    nm   <- coreToName dataConName getUnique qualifiedNameString dc
    uTvs <- mapM coreToTyVarB (dataConUnivTyVars dc)
    eTvs <- mapM coreToTyVarB (dataConExTyCoVars dc)
    return $ mkDataCon
      nm
      (dataConTag dc)
      (runClosedType dcTy)
      (runDataConSig uTvs eTvs repTys)
      (dataConStrictness dc)
      (dataConLabels dc)

-- | The strictness of the fields of a data constructor
dataConStrictness :: DataCon -> [C.DcStrictness]
dataConStrictness = fmap hsImplBangToBool . dataConImplBangs

-- | The labels of the fields of a data constructor
dataConLabels :: DataCon -> [Text]
dataConLabels = map (decodeUtf8 . bytesFS . field_label . flLabel) . dataConFieldLabels

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
  constructorName <- Text.pack <$> typeConstructorToString constructor
  return $ case constructorName of
    _ | constructorName == showt 'True  -> True
      | constructorName == showt 'False -> False
      | otherwise -> error $ "Expected boolean constructor, got:" ++ Text.unpack constructorName
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
  let Reduction _ key1 = normaliseType envs Nominal key
      Reduction _ value1 = normaliseType envs Nominal value
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

annotateOccFS :: FastString
annotateOccFS = mkFastString "Annotate"
{-# NOINLINE annotateOccFS #-}

-- | If this type has an annotate type synonym, return list of attributes.
coreToAttrs :: Type -> C2C [Attr Text]
coreToAttrs (TyConApp tycon kindsOrTypes)
  -- Cheap pre-check on the interned occurrence name; only build the qualified
  -- name when it can possibly be Clash.Annotations.SynthesisAttributes.Annotate.
  | occNameFS (nameOccName (tyConName tycon)) == annotateOccFS = do
      name' <- typeConstructorToString tycon

      if name' == show ''Annotate
      then coreToAttrs' kindsOrTypes
      else return []

coreToAttrs _ =
    return []

-- * Types of scoped Core

-- | The key of a GHC variable in the builders of scoped Core, see
-- "Clash.Core.Scoped.Builder"
varKey :: Var -> Key
varKey = uniqueKey . fromGhcUnique . varUnique

-- | Converts a GHC type to a type of scoped Core, as a builder (see
-- "Clash.Core.Scoped.Builder"). It gives the type that converting the type
-- of Clash Core of 'coreToType' would give.
--
-- It looks up the same names as 'coreToType', in the same order. That
-- matters: the names are cached by unique (see 'qualifiedNameStringM'), and
-- different GHC names can share a unique, so the order in which names are
-- first looked up decides their text. So it also converts the kinds of the
-- type variables it meets, as 'coreToType' does, although the builder of an
-- occurrence of a type variable doesn't need its kind.
--
-- Memoized on the GHC type, like 'coreToType', which shares the closed types
-- without binders, see "Clash.Core.Scoped.Builder".
coreToTypeB :: Type -> C2C TyB
coreToTypeB ty = do
  cache <- use scopedTypeCache
  case HashMap.lookup (StructuralType ty) cache of
    Just ty1 -> pure ty1
    Nothing -> do
      ty1 <- withoutSrcSpan convert
      scopedTypeCache %= HashMap.insert (StructuralType ty) ty1
      pure ty1
  where
    convert = do
      ty1 <- ty''
      attrs <- coreToAttrs ty
      pure (if null attrs then ty1 else annTyB attrs ty1)

    ty'' | Just ty' <- coreView ty = coreToTypeB ty'
         | TyConApp tc xs <- ty = do
             envs <- view famInstEnvs
             case topReduceTyFamApp_maybe envs tc xs of
               Nothing -> coreToTypeB' ty
               Just (HetReduction (Reduction _ ty') _) -> coreToTypeB ty'
         | otherwise = coreToTypeB' ty

-- | See 'coreToType''
coreToTypeB' :: Type -> C2C TyB
coreToTypeB' (TyVarTy tv) = varTyB (varKey tv) <$ coreToTyVarB tv
coreToTypeB' (TyConApp tc args)
  | Just (FunTy _ _ ty1 ty2) <- tyConAppFunTy_maybe tc args = funTyB . pure <$> coreToTypeB ty1 <*> coreToTypeB ty2
  | otherwise     = case expandSynTyCon_maybe tc args of
                      ExpandsSyn substs synTy remArgs -> do
                        let substs' = mkTvSubstPrs substs
                            synTy'  = substTy substs' synTy
                        foldl appTyB <$> coreToTypeB synTy' <*> mapM coreToTypeB remArgs
                      _ -> do
                        tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
                        tyConMap %= C.insertIfAbsent tcName tc
                        foldl appTyB (constTyB (TyCon tcName)) <$> mapM coreToTypeB args
coreToTypeB' (ForAllTy (Bndr tv _) ty) = do
  (key, nm, k) <- coreToTyVarB tv
  forAllTyB key nm k <$> coreToTypeB ty
coreToTypeB' (FunTy _ _ ty1 ty2) = funTyB . pure <$> coreToTypeB ty1 <*> coreToTypeB ty2
coreToTypeB' (LitTy tyLit)    = return $ litTyB (coreToTyLit tyLit)
coreToTypeB' (AppTy ty1 ty2)  = appTyB <$> coreToTypeB ty1 <*> coreToTypeB' ty2
coreToTypeB' (CastTy t (Refl{})) = coreToTypeB' t
coreToTypeB' t@(CastTy _ _)   = error ("Cannot handle CastTy " ++ showPprUnsafe t)
coreToTypeB' t@(CoercionTy _) = error ("Cannot handle CoercionTy " ++ showPprUnsafe t)

-- | The closed type of a variable, see 'coreToVarType'. Memoized on the name
-- for global variables, like 'coreToVarType'.
coreToVarTypeB :: Var -> C2C (S.Type 'S.VoidS)
coreToVarTypeB v
  | isGlobalId v = makeCached (varName v) scopedVarTypeMap convert
  | otherwise = convert
 where
  convert = runClosedType <$> coreToTypeB (varType v)

-- | A global variable, see 'coreToId'
coreToGlobalIdB :: Id -> C2C S.GlobalId
coreToGlobalIdB i = do
  ty <- coreToVarTypeB i
  nm <- coreToVar i
  pure (mkGlobalId nm ty)

-- | The free type variables of the type of Clash Core that 'coreToType'
-- converts a GHC type to, in the order of 'C.typeFreeVars': closed over the
-- kinds of type variables, with duplicates. Mirrors 'coreToType'.
convertedTyVars :: FamInstEnvs -> Type -> [TyVar]
convertedTyVars envs = go emptyVarSet
 where
  go bound ty
    | Just ty' <- coreView ty = go bound ty'
    | TyConApp tc xs <- ty
    , Just (HetReduction (Reduction _ ty') _) <- topReduceTyFamApp_maybe envs tc xs
    = go bound ty'
    | otherwise = go' bound ty

  go' bound = \case
    TyVarTy tv
      -- See Note [Closing over kind variables] in "Clash.Core.FreeVars"
      | tv `elemVarSet` bound -> go bound (varType tv)
      | otherwise -> tv : go emptyVarSet (varType tv)
    TyConApp tc args
      | Just (FunTy _ _ ty1 ty2) <- tyConAppFunTy_maybe tc args -> go bound ty1 ++ go bound ty2
      | ExpandsSyn substs synTy remArgs <- expandSynTyCon_maybe tc args
      -> go bound (substTy (mkTvSubstPrs substs) synTy) ++ concatMap (go bound) remArgs
      | otherwise -> concatMap (go bound) args
    ForAllTy (Bndr tv _) ty -> go bound (varType tv) ++ go (extendVarSet bound tv) ty
    FunTy _ _ ty1 ty2 -> go bound ty1 ++ go bound ty2
    AppTy ty1 ty2 -> go bound ty1 ++ go' bound ty2
    CastTy t (Refl{}) -> go' bound t
    _ -> []

-- | Convert a type variable binder: its key, name and kind. Like
-- 'coreToTyVar', it converts the kind first.
coreToTyVarB :: TyVar -> C2C (Key, C.Name S.Type, TyB)
coreToTyVarB tv = do
  k <- coreToTypeB (varType tv)
  nm <- coreToVar tv
  pure (varKey tv, nm, k)

coreToTyLit :: TyLit
            -> LitTy
coreToTyLit (NumTyLit i) = NumTy (fromInteger i)
coreToTyLit (StrTyLit s) = SymTy (unpackFS s)
coreToTyLit (CharTyLit c) = CharTy c

coreToVar :: Var
          -> C2C (C.Name a)
coreToVar = coreToName varName varUnique qualifiedNameStringM

coreToPrimVar :: Var
              -> C2C (C.Name S.Term)
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

isDataConWrapId :: Id -> Bool
isDataConWrapId v = case idDetails v of
  DataConWrapId {} -> True
  _                -> False
