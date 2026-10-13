{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Conversion of GHC Core to closed terms of scoped Clash Core
  ("Clash.Core.Scoped.Syntax"). It converts in two phases, see
  "Clash.Core.Scoped.Builder".

  = Types

  The types in terms, and those of global variables, primitives and data
  constructors, are types of scoped Core, which
  'Clash.GHC.GHC2Core.coreToTypeB' builds from GHC's types directly.

  = Primitives

  The type of a primitive is closed in scoped Core. Primitives that stand in
  for a term of some type, @_RUBBISH_@, @_TY_@ and @_CO_@, have the shape of
  'Clash.Core.Scoped.Util.primAtType': if the type is open, they are a
  primitive of type @forall a. a@ applied to the type.

  = Recursive groups

  A recursive group of global binders (see
  @Clash.GHC.GenerateBindings.processBind@) refers to its own binders as local
  variables, which the caller binds: 'coreToTermB' gets the set of these
  binders.
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
    -- * Helpers of the conversion of terms
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
    -- * Terms of scoped Core
  , coreToTermB
  , coreToIdB
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
import Clash.Core.DataCon (mkDataCon)
import Clash.Core.Scoped.Syntax (mkGlobalId)
import           Clash.Core.Scoped.Sig       (TyFamEqn)
import qualified Clash.Core.Scoped.Syntax    as S

import           Control.Lens                ((<>=))
import           Data.Bifunctor              (second)
import qualified GHC.Magic
import qualified GHC.Prim
import qualified GHC.Stack
import qualified GHC.Stack.Types
import qualified Text.Read                   as Text
import qualified GHC.Data.Strict             as GHC

-- GHC API
import GHC.Builtin.Types (falseDataCon)
import GHC.Core.Coercion (coercionType, coercionKind)
import GHC.Core.FVs  (exprSomeFreeVars)
import GHC.Core
  (AltCon (..), Bind (..), CoreExpr, Expr (..), Unfolding (..),
   Alt(..),
#if MIN_VERSION_ghc(9,14,0)
   isTypeArg, isValArg,
#endif
   collectArgs, rhssOfAlts)
import GHC.Types.Tickish (GenTickish (..))
import GHC.Core.DataCon (dataConTyCon)
import GHC.Core.FamInstEnv (emptyFamInstEnvs)
import GHC.Types.Id (isDataConId_maybe)
import GHC.Types.Id.Info (unfoldingInfo)
import GHC.Types.Literal (literalType)
import GHC.Data.Pair (Pair (..))
import GHC.Types.SrcLoc (SrcSpan (..))
import GHC.Core.TyCon (isNewTyCon)
import GHC.Types.Var
  (isTyVar, idInfo)
import GHC.Types.Var.Set (VarSet, isEmptyVarSet)

-- Local imports
import Clash.Core.Basic (IsMultiPrim (..), NameMod (..), WorkInfo (..), undefinedXPrims)
import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Core.Scoped.Syntax    (mkPrimInfo)
import           Clash.Core.Scoped.TysPrim   (undefinedTy)
import           Clash.Core.Scoped.Builder
import qualified Clash.Normalize.Scoped.Primitives as NP
import           Clash.Primitives.Types      hiding (name)

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
  , _convertedTypeCache :: HashMap StructuralType TyB
  -- ^ Cache for types converted to scoped Core, keyed on the GHC type. See
  -- 'coreToTypeB'.
  , _varTypeMap :: HashMap Name (S.Type 'S.VoidS)
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

-- | Convert a type constructor
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
-- types, closed over kinds, in the order of their first occurrence (in the
-- left-hand side, then the right-hand side), except that every variable comes
-- after the variables in its kind.
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

  -- Every variable after those in its kind, keeping their order otherwise
  dependencySort tvs = reverse (snd (foldl' visit (emptyVarSet, []) tvs))
   where
    tvEnv = mkVarEnv [ (tv, tv) | tv <- tvs ]
    visit (seen, acc) tv
      | tv `elemVarSet` seen = (seen, acc)
      | otherwise =
          let kindTvs = mapMaybe (lookupVarEnv tvEnv) (convertedTyVars envs (varType tv))
              (seen1, acc1) = foldl' visit (extendVarSet seen tv, acc) kindTvs
          in  (seen1, tv : acc1)

-- | Convert the right-hand side of an algebraic data type
makeAlgTyConRhs :: TyCon
                -> AlgTyConRhs
                -> C2C (Maybe C.AlgTyConRhs)
makeAlgTyConRhs _tc algTcRhs = case algTcRhs of
#if MIN_VERSION_ghc(9,14,0)
  -- GHC 9.14 represents unary classes (a class with a single method or a single
  -- superclass) with their own AlgTyConRhs constructor instead of as a newtype.
  -- See Note [Unary class magic] in GHC.Core.TyCon. We map it back to Clash's
  -- 'NewTyCon' so that the wrapper data constructor (e.g. 'C:KnownNat') is
  -- treated as identity by clash's normalizer, which is what its semantics is.
  UnaryClassTyCon {data_con = dc} -> do
    let tvs = tyConTyVars _tc
        -- The DataCon for a unary class has exactly one field; that's the
        -- newtype's RHS (e.g. 'SNat n' for 'KnownNat n').
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
-- "Clash.Core.Scoped.Builder").
--
-- The order in which it looks up names matters: the names are cached by
-- unique (see 'qualifiedNameStringM'), and different GHC names can share a
-- unique, so the order in which names are first looked up decides their
-- text. So it also converts the kinds of the type variables it meets,
-- although the builder of an occurrence of a type variable doesn't need its
-- kind.
--
-- Memoized on the GHC type, which shares the closed types without binders,
-- see "Clash.Core.Scoped.Builder". The conversion runs 'withoutSrcSpan' so
-- that it depends on the GHC type alone.
coreToTypeB :: Type -> C2C TyB
coreToTypeB ty = do
  cache <- use convertedTypeCache
  case HashMap.lookup (StructuralType ty) cache of
    Just ty1 -> pure ty1
    Nothing -> do
      ty1 <- withoutSrcSpan convert
      convertedTypeCache %= HashMap.insert (StructuralType ty) ty1
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

-- | See 'coreToTypeB'
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

-- | The closed type of a variable, memoized on the variable's name.
--
-- Only global variables are memoized: their names come from GHC's name cache
-- and are globally unique, so the same name always refers to the same
-- variable, whose type is intrinsic. Names of local variables (e.g. binders
-- instantiated when loading unfoldings from interface files) can collide
-- between declarations and must not share cache entries.
coreToVarTypeB :: Var -> C2C (S.Type 'S.VoidS)
coreToVarTypeB v
  | isGlobalId v = makeCached (varName v) varTypeMap convert
  | otherwise = convert
 where
  convert = runClosedType <$> coreToTypeB (varType v)

-- | A global variable: its name and closed type
coreToGlobalIdB :: Id -> C2C S.GlobalId
coreToGlobalIdB i = do
  ty <- coreToVarTypeB i
  nm <- coreToVar i
  pure (mkGlobalId nm ty)

-- | The free type variables of the type that 'coreToTypeB' converts a GHC
-- type to, in the order of their occurrences: closed over the kinds of type
-- variables, a variable before the variables in its kind, with duplicates.
-- Mirrors 'coreToTypeB'.
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
      -- When a type variable is free, its kind variables are free too, even
      -- if a binder with the same name is in scope: in @forall k . b -> k@
      -- with @b :: k -> Type@, the @k@ in the kind of @b@ is not the bound
      -- @k@. So close over the kind of a free variable with nothing bound.
      -- See also: https://gitlab.haskell.org/ghc/ghc/-/commit/503514b94f8dc7bd9eab5392206649aee45f140b
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

-- | Convert a type variable binder: its key, name and kind. It converts the
-- kind first, see 'coreToTypeB' for why the order matters.
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

-- * Terms

-- | Convert a GHC term, see the module header, given the compiled primitives
-- and the unlocatable variables. The given global binders are local
-- variables of the term, see "Recursive groups" in the module header.
coreToTermB
  :: CompiledPrimMap
  -> [Var]
  -> VarSet
  -> CoreExpr
  -> C2C TmB
coreToTermB primMap unlocs recLocals = term
  where
    term :: CoreExpr -> C2C TmB
    term e
#if MIN_VERSION_ghc(9,14,0)
      -- See Note [Unary class magic] in GHC.Core.TyCon. The worker for a unary
      -- class data constructor (e.g. 'C:KnownNat') is semantically the identity
      -- on its single value field. Peel it off here so that downstream
      -- transformations (in particular the evaluator's literal extractors) see
      -- the underlying value directly, as they would have on GHC < 9.14 when
      -- unary classes were represented as newtypes.
      -- Match a fully-saturated worker application: all type args, then
      -- exactly one value arg. A naive '[Type _, val]' pattern would also
      -- match a partial application like 'C:IP @"callStack" @CallStack'
      -- (two 'Type' args, no value yet), binding 'val' to a 'Type' node
      -- and producing '_TY_'-wrapped junk when peeled.
      | (Var x, args) <- collectArgs e
      , Just dc <- isDataConId_maybe x
      , isUnaryClassTyCon (dataConTyCon dc)
      , (tyArgs, [val]) <- break isValArg args
      , all isTypeArg tyArgs
      , isValArg val
      = term val
#endif
      | (Var x,args) <- collectArgs e
      = do
        opts <- askClashOpts
        let (nm, st, _) = RWS.runRWS (qualifiedNameString (varName x))
                                     (GHC2CoreEnv noSrcSpan emptyFamInstEnvs opts)
                                     emptyGHC2CoreState
        pendingWarnings <>= st ^. pendingWarnings
        go nm args
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
        go nm args
          | nm == showt '($), length args == 5
          = term (App (args!!3) (args!!4))
        go nm args  -- noinline :: forall a. a -> a
          | nm == showt 'GHC.Magic.noinline, [_ty, x] <- args
          = term x
        -- Remove most CallStack logic
        go nm args
          | nm == showt 'GHC.Stack.Types.PushCallStack
          = term (last args)
        go nm args
          | nm == showt 'GHC.Stack.Types.FreezeCallStack
          = term (last args)
        go nm args
          | nm == showt 'GHC.Stack.withFrozenCallStack, length args == 3
          = term (App (args!!2) (args!!1))
        go "Clash.Sized.BitVector.Internal.checkUnpackUndef" args
          | [_nTy,_aTy,_kn,_typ,f] <- args
          = term f
        go "Clash.Magic.prefixName" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod PrefixName nmTy f
        go "Clash.Magic.suffixName" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod SuffixName nmTy f
        go "Clash.Magic.suffixNameFromNat" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod SuffixName nmTy f
        go "Clash.Magic.suffixNameP" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod SuffixNameP nmTy f
        go "Clash.Magic.suffixNameFromNatP" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod SuffixNameP nmTy f
        go "Clash.Magic.setName" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod SetName nmTy f
        go "Clash.Magic.deDup" args
          | [_aTy,f] <- args
          = tickB deDupTickB <$> term f
        go "Clash.Magic.noDeDup" args
          | [_aTy,f] <- args
          = tickB noDeDupTickB <$> term f
        go "Clash.Magic.clashSimulation" _
          = dataB <$> coreToDataCon falseDataCon
        go "Clash.XException.xToErrorCtx" args
          -- xToErrorCtx :: forall a. String -> a -> a
          | [_ty, _msg, x] <- args
          = term x
        go "Clash.Annotations.SynthesisAttributes.annotateReg" args
          | [ Type nTy, _domTy, _aTy, attrs, x] <- args
          = tickB <$> (attributesTickB <$> coreToTypeB nTy <*> term attrs) <*> term x
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

        nameMod m nmTy f = tickB <$> (nameModTickB m <$> coreToTypeB nmTy) <*> term f

    parseBundle :: Text -> Text -> Maybe Int
    parseBundle fNm nm0 = do
      nm1 <- Text.stripPrefix ("Clash.Signal.Bundle." <> fNm) nm0
      nm2 <- Text.stripSuffix "#" nm1
      Text.readMaybe (Text.unpack nm2)

    term' (Var x)                 = var x
    term' (Lit l@LitRubbish{}) = do
      let rubbish ty = mkPrimInfo (pack "_RUBBISH_") ty WorkNever SingleResult S.NoUnfolding
      envs <- view famInstEnvs
      ty <- coreToTypeB (literalType l)
      -- The shape of 'Clash.Core.Scoped.Util.primAtType', see the module header
      if null (convertedTyVars envs (literalType l))
        then pure (primB (rubbish (runClosedType ty)))
        else pure (tyAppB (primB (rubbish undefinedTy)) ty)
    term' (Lit l)                 = return $ literalB (coreToLiteral l)
    term' (App eFun (Type tyArg)) = tyAppB <$> term eFun <*> coreToTypeB tyArg
    term' (App eFun eArg)         = appB   <$> term eFun <*> term eArg
    term' (Lam x e)
      | isTyVar x
      = do
        (key, occ, k) <- coreToTyVarB x
        tyLamB key occ k <$> addUsefull (getSrcSpan x) (term e)
      | otherwise
      = do
        (e',sp) <- termSP (getSrcSpan x) e
        (ty, occ) <- coreToIdSP sp x
        return (lamB (varKey x) occ ty e')
    term' (Let (NonRec x e1) e2)  = do
      (e1',sp) <- termSP (getSrcSpan x) e1
      (ty, occ)  <- coreToIdSP sp x
      e2' <- term e2
      return (letB (varKey x) occ ty e1' e2')

    term' (Let (Rec xes) e) = do
      xes' <- mapM go xes
      e'   <- term e
      return (letrecB xes' e')
     where
      go (x,b) = do
        (b',sp) <- termSP (getSrcSpan x) b
        (ty, occ) <- coreToIdSP sp x
        return (varKey x, occ, ty, b')

    term' (Case s _ ty [])  = do
      s'  <- term' s
      ty' <- coreToTypeB ty
      case tmbHead s' of
        Just p | S.primName p `elem` undefinedXPrims ->
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
          return (tyAppB (primB NP.undefinedX) ty')
        _ ->
          return (tyAppB (primB NP.undefined) ty')

    term' (Case e b ty alts) = do
     let usesBndr = any ( not . isEmptyVarSet . exprSomeFreeVars (== b))
                  $ rhssOfAlts alts
     (e',sp) <- termSP (getSrcSpan b) e
     (bTy, bOcc) <- coreToIdSP sp b
     ty' <- coreToTypeB ty
     let caseTerm v =
             caseB v ty' <$> mapM (addUsefull sp . alt sp) alts
     if usesBndr
      then do
        ct <- caseTerm (varB (varKey b))
        return (letB (varKey b) bOcc bTy e' ct)
      else caseTerm e'

    term' (Cast e co) = do
      let (Pair ty1 ty2) = coercionKind co
      hasPrimCoM <- hasPrimCo co
      sizedCast <- isSizedCast ty1 ty2
      case hasPrimCoM of
        Just _ | sizedCast
          -> castB <$> term e <*> coreToTypeB ty1 <*> coreToTypeB ty2
        _ -> term e
    term' (Tick (SourceNote rsp _) e) =
      tickB (srcSpanTickB (RealSrcSpan rsp GHC.Nothing)) <$>
             addUsefull (RealSrcSpan rsp GHC.Nothing) (term e)
    term' (Tick _ e) = term e
    term' (Type t) = tyAppB (primB tyPrim) <$> coreToTypeB t
    term' (Coercion co) = tyAppB (primB coPrim) <$> coreToTypeB (coercionType co)

    termSP sp = fmap (second unSrcSpanRB) . RWS.listen . addUsefullR sp . term
    coreToIdSP sp = addUsefullR sp . coreToIdB

    lookupPrim :: Text -> Maybe (Maybe CompiledPrimitive)
    lookupPrim nm = extractPrim <$> HashMap.lookup nm primMap

    var x = do
        xPrim <- if isGlobalId x then coreToPrimVar x else coreToVar x
        -- Converted for every variable, although only primitives use it:
        -- converting it caches the names in it, see 'coreToTypeB'. Its
        -- closed type is only built when used.
        xType  <- coreToVarTypeB x
        let xNameS = C.nameOcc xPrim
            -- The builder terms split the GHC type
            xTy = varType x
            prim wi unf = primB (mkPrimInfo xNameS xType wi SingleResult unf)
            primVar wi =
              if x `elem` unlocs
                then return (prim wi S.NoUnfolding)
                else prim wi . S.Unfolding <$> coreToGlobalIdB x
        case isDataConId_maybe x of
          Just dc -> case lookupPrim xNameS of
            Just p  ->
              -- Primitive will be marked MultiResult in Transformations if it
              -- is a multi result primitive.
              return $ prim (maybe WorkVariable workInfo p) S.NoUnfolding
            Nothing -> if isDataConWrapId x && not (isNewTyCon (dataConTyCon dc))
              then let xInfo = idInfo x
                       unfolding = unfoldingInfo xInfo
                   in  case unfolding of
                          CoreUnfolding {uf_tmpl} -> do
                            sp <- view srcSpan
                            RWS.censor (const (SrcSpanRB sp)) (term uf_tmpl)
                          NoUnfolding -> error ("No unfolding for DC wrapper: " ++ showPprUnsafe x)
                          _ -> error ("Unexpected unfolding for DC wrapper: " ++ showPprUnsafe x)
              else dataB <$> coreToDataCon dc
          Nothing -> case lookupPrim xNameS of
            Just (Just (Primitive f wi _))
              | Just n <- parseBundle "bundle" f        -> bundleUnbundleB (n+1) xTy
              | Just n <- parseBundle "unbundle" f      -> bundleUnbundleB (n+1) xTy
              | f == "Clash.Signal.Internal.mapSignal#" -> mapSignalB xTy
              | f == "Clash.Signal.Internal.signal#"    -> signalB xTy
              | f == "Clash.Signal.Internal.appSignal#" -> appSignalB xTy
              | f == "Clash.Signal.Internal.traverse#"  -> traverseB xTy
              | f == "Clash.Signal.Internal.joinSignal#" -> joinB xTy
              | f == "Clash.Signal.Bundle.vecBundle#"   -> vecUnwrapB xTy
              | f == showt '($)                           -> dollarB xTy
              | f == showt 'GHC.Stack.withFrozenCallStack -> withFrozenCallStackB xTy
              | f == showt 'GHC.Magic.noinline            -> idB xTy
              | f == showt 'GHC.Magic.lazy                -> idB xTy
              -- 'nospec' is a wired-in magic Id in 'GHC.Magic' (resp.
              -- 'GHC.Internal.Magic' on GHC >= 9.14). It is not user-importable,
              -- so we can't quote it via TH. Both module names need a literal
              -- match here so that this clause fires across GHC versions.
              | f == "GHC.Magic.nospec"                   -> idB xTy
              | f == "GHC.Internal.Magic.nospec"          -> idB xTy
              | f == showt 'GHC.Magic.runRW#              -> runRWB xTy
              | f == "Clash.Sized.Internal.BitVector.checkUnpackUndef" -> checkUnpackUndefB xTy
              | f == "Clash.Magic.prefixName"
              -> nameModB PrefixName xTy
              | f == "Clash.Magic.postfixName"
              -> nameModB SuffixName xTy
              | f == "Clash.Magic.setName"
              -> nameModB SetName xTy
              | f == "Clash.XException.xToErrorCtx"
              -> xToErrorCtxB xTy
              | f == "Clash.Annotations.SynthesisAttributes.annotateReg"
              -> annotateRegB xTy
              | otherwise
              -> primVar wi
            Just (Just (BlackBox {workInfo = wi}))
              -> primVar wi
            Just (Just (BlackBoxHaskell {workInfo = wi}))
              -> primVar wi
            Just Nothing ->
              -- Was guarded by "DontTranslate". We don't know yet if Clash will
              -- actually use it later on, so we don't err here.
              return $ prim WorkVariable S.NoUnfolding
            Nothing
              | x `elem` unlocs ->
                  return (prim WorkVariable S.NoUnfolding)
              | isGlobalId x, not (x `elemVarSet` recLocals)
              -> globalB <$> coreToGlobalIdB x
              | otherwise
              -> return (varB (varKey x))

    alt _   (Alt DEFAULT      _  e) = defaultAltB <$> term e
    alt _   (Alt (LitAlt l)   _  e) = litAltB (coreToLiteral l) <$> term e
    alt sp0 (Alt (DataAlt dc) xs e) = case span isTyVar xs of
      (tyvs,tmvs) -> do
        (e',sp1) <- termSP sp0 e
        dc' <- coreToDataCon dc
        tyvs' <- mapM coreToTyVarB tyvs
        tmvs' <- mapM (\v -> (\(ty, occ) -> (varKey v, occ, ty)) <$> coreToIdSP sp1 v) tmvs
        return (dataAltB dc' tyvs' tmvs' e')

-- | Convert a term variable binder: its type and name
coreToIdB :: Id -> C2C (TyB, C.Name S.Term)
coreToIdB i = (,) <$> coreToTypeB (varType i) <*> coreToVar i

-- | The primitives that stand in for a type and a coercion, see the module
-- header
tyPrim, coPrim :: S.PrimInfo
tyPrim = mkPrimInfo (pack "_TY_") undefinedTy WorkNever SingleResult S.NoUnfolding
coPrim = mkPrimInfo (pack "_CO_") undefinedTy WorkNever SingleResult S.NoUnfolding

-- * Builder terms
--
-- Closed terms for primitives that GHC2Core replaces, given the GHC type of
-- the primitive. Their bodies only refer to their own binders, so the keys
-- of their term variables are arbitrary, see "Clash.Core.Scoped.Builder".
--
-- They split the GHC type as its conversion ('coreToTypeB') would be split,
-- and convert the parts. These are part of the conversion of the whole type,
-- which the caller converts first, so this looks up no new names.

-- | Split a forall-type, like matching 'S.ForAllTy' on the conversion of the
-- type
forAllView :: Type -> Maybe (TyVar, Type)
forAllView ty | Just ty' <- coreView ty = forAllView ty'
forAllView (ForAllTy (Bndr tv _) ty) = Just (tv, ty)
forAllView _ = Nothing

-- | Split a function type, like matching
-- 'Clash.Core.Scoped.Type.FunTy' of 'Clash.Core.Scoped.Type.tyView' on the
-- conversion of the type
funView :: Type -> Maybe (Type, Type)
funView ty | Just ty' <- coreView ty = funView ty'
funView (FunTy _ _ a r) = Just (a, r)
funView (TyConApp tc args)
  | Just (FunTy _ _ a r) <- tyConAppFunTy_maybe tc args = Just (a, r)
funView _ = Nothing

-- | Type lambdas binding the given type variables. Their names are those of
-- the conversion of the type they come from, which runs without a source
-- span, see 'coreToTypeB'.
tyLamsB :: [TyVar] -> TmB -> C2C TmB
tyLamsB tvs body = foldr tyLam (pure body) tvs
 where
  tyLam tv r = do
    (key, occ, k) <- withoutSrcSpan (coreToTyVarB tv)
    tyLamB key occ k <$> r

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
bundleUnbundleB :: Int -> Type -> C2C TmB
bundleUnbundleB nTyVarsExpected = go []
 where
  go tvs ty
    | Just (tv, ty') <- forAllView ty = go (tv:tvs) ty'
    | Just (argTy, _resTy) <- funView ty =
        if length tvs /= nTyVarsExpected then
          error $ $(curLoc) ++ show (length tvs) ++ " vs " ++ show nTyVarsExpected
        else do
          argTyB <- coreToTypeB argTy
          tyLamsB (reverse tvs) (lamB 0 (C.mkUnsafeSystemName "c$s" 0) argTyB (varB 0))
    | otherwise = error $ $(curLoc) ++ showPprUnsafe ty ++ " " ++ showPprUnsafe tvs

-- | @\\f x -> f x@, for 'mapSignalB', 'appSignalB' and 'dollarB'
applyB :: TyB -> TyB -> TmB
applyB fTy xTy =
  lamB 0 (C.mkUnsafeSystemName "f" 0) fTy $
  lamB 1 (C.mkUnsafeSystemName "x" 1) xTy $
  appB (varB 0) (varB 1)

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
mapSignalB :: Type -> C2C TmB
mapSignalB ty
  | Just (aTV, ty1) <- forAllView ty
  , Just (bTV, ty2) <- forAllView ty1
  , Just (clkTV, funTy) <- forAllView ty2
  , Just (_, funTy'') <- funView funTy
  , Just (aTy, bTy) <- funView funTy''
  = do
    aTyB <- coreToTypeB aTy
    bTyB <- coreToTypeB bTy
    tyLamsB [aTV, bTV, clkTV] (applyB (funTyB [aTyB] bTyB) aTyB)
mapSignalB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | Given the type:
--
-- @forall a. forall dom. a -> Signal dom a@
--
-- Generate the term
--
-- @/\(a:*)./\(dom:Domain).\(x:Signal dom a).x@
signalB :: Type -> C2C TmB
signalB ty
  | Just (aTV, ty1) <- forAllView ty
  , Just (domTV, funTy) <- forAllView ty1
  , Just (_, saTy) <- funView funTy
  = do
    saTyB <- coreToTypeB saTy
    tyLamsB [aTV, domTV] (lamB 0 (C.mkUnsafeSystemName "x" 0) saTyB (varB 0))
signalB ty = error $ $(curLoc) ++ showPprUnsafe ty

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
appSignalB :: Type -> C2C TmB
appSignalB ty
  | Just (domTV, ty1) <- forAllView ty
  , Just (aTV, ty2) <- forAllView ty1
  , Just (bTV, funTy) <- forAllView ty2
  , Just (_, funTy'') <- funView funTy
  , Just (saTy, sbTy) <- funView funTy''
  = do
    saTyB <- coreToTypeB saTy
    sbTyB <- coreToTypeB sbTy
    tyLamsB [domTV, aTV, bTV] (applyB (funTyB [saTyB] sbTyB) saTyB)
appSignalB ty = error $ $(curLoc) ++ showPprUnsafe ty

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
vecUnwrapB :: Type -> C2C TmB
vecUnwrapB ty
  | Just (tTV, ty1) <- forAllView ty
  , Just (nTV, ty2) <- forAllView ty1
  , Just (aTV, funTy) <- forAllView ty2
  , Just (_, vsTy) <- funView funTy
  = do
    vsTyB <- coreToTypeB vsTy
    tyLamsB [tTV, nTV, aTV] (lamB 0 (C.mkUnsafeSystemName "vs" 0) vsTyB (varB 0))
vecUnwrapB ty = error $ $(curLoc) ++ showPprUnsafe ty

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
traverseB :: Type -> C2C TmB
traverseB ty
  | Just (fTV, ty1) <- forAllView ty
  , Just (aTV, ty2) <- forAllView ty1
  , Just (bTV, ty3) <- forAllView ty2
  , Just (domTV, funTy) <- forAllView ty3
  , Just (dictTy, funTy1) <- funView funTy
  , Just (gTy, funTy2) <- funView funTy1
  , Just (xTy, _) <- funView funTy2
  = do
    dictTyB <- coreToTypeB dictTy
    gTyB <- coreToTypeB gTy
    xTyB <- coreToTypeB xTy
    tyLamsB [fTV, aTV, bTV, domTV] $
      lamB 0 (C.mkUnsafeSystemName "dict" 0) dictTyB $
      lamB 1 (C.mkUnsafeSystemName "g" 1) gTyB $
      lamB 2 (C.mkUnsafeSystemName "x" 2) xTyB $
      appB (varB 1) (varB 2)
traverseB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | Given the type:
--
-- @forall (r :: Rep) (a :: TYPE Lifted) (b :: TYPE r). (a -> b) -> a -> b@
--
-- Generate the term:
--
-- @/\(r:Rep)/\(a:TYPE Lifted)./\(b:TYPE r).\(f : (a -> b)).\(x : a).f x@
dollarB :: Type -> C2C TmB
#if MIN_VERSION_ghc(9,8,0)
dollarB ty
  | Just (raTV, ty1) <- forAllView ty
  , Just (rbTV, ty2) <- forAllView ty1
  , Just (aTV, ty3) <- forAllView ty2
  , Just (bTV, funTy) <- forAllView ty3
  , Just (fTy, funTy'') <- funView funTy
  , Just (aTy, _) <- funView funTy''
  = do
    fTyB <- coreToTypeB fTy
    aTyB <- coreToTypeB aTy
    tyLamsB [raTV, rbTV, aTV, bTV] (applyB fTyB aTyB)
#else
dollarB ty
  | Just (rTV, ty1) <- forAllView ty
  , Just (aTV, ty2) <- forAllView ty1
  , Just (bTV, funTy) <- forAllView ty2
  , Just (fTy, funTy'') <- funView funTy
  , Just (aTy, _) <- funView funTy''
  = do
    fTyB <- coreToTypeB fTy
    aTyB <- coreToTypeB aTy
    tyLamsB [rTV, aTV, bTV] (applyB fTyB aTyB)
#endif
dollarB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | Given the type:
--
-- @forall a. forall dom. Signal dom (Signal dom a) -> Signal dom a@
--
-- Generate the term
--
-- @/\(a:*)./\(dom:Domain).\(x:Signal dom a).x@
joinB :: Type -> C2C TmB
joinB ty
  | Just _ <- forAllView ty = signalB ty
  | otherwise = error $ $(curLoc) ++ showPprUnsafe ty

-- | Given the type:
--
-- @forall a. CallStack -> (HasCallStack => a) -> a@
--
-- Generate the term
--
-- @/\(a:*)./\(callStack:CallStack).\(f:HasCallStack => a).f callStack@
withFrozenCallStackB :: Type -> C2C TmB
withFrozenCallStackB ty
  | Just (aTV, funTy) <- forAllView ty
  , Just (callStackTy, fTy) <- funView funTy
  = do
    callStackTyB <- coreToTypeB callStackTy
    fTyB <- coreToTypeB fTy
    tyLamsB [aTV] $
      lamB 0 (C.mkUnsafeSystemName "callStack" 0) callStackTyB $
      lamB 1 (C.mkUnsafeSystemName "f" 1) fTyB $
      appB (varB 1) (varB 0)
withFrozenCallStackB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | Given the type:
--
-- @forall a. a -> a@
--
-- Generate the term
--
-- @/\(a:*).\(x:a).x@
idB :: Type -> C2C TmB
idB ty
  | Just (aTV, funTy) <- forAllView ty
  , Just (xTy, _) <- funView funTy
  = do
    xTyB <- coreToTypeB xTy
    tyLamsB [aTV] (lamB 0 (C.mkUnsafeSystemName "x" 0) xTyB (varB 0))
idB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | Given the type:
--
-- @forall (r :: RuntimeRep) (o :: TYPE r).(State# RealWorld -> o) -> o@
--
-- Generate the term:
--
-- @/\(r:RuntimeRep)./\(o:TYPE r).\(f:State# RealWord -> o) -> f realWorld#@
runRWB :: Type -> C2C TmB
runRWB ty
  | Just (rTV, ty1) <- forAllView ty
  , Just (oTV, funTy) <- forAllView ty1
  , Just (fTy, _) <- funView funTy
  , Just (rwTy, _) <- funView fTy
  = do
    fTyB <- coreToTypeB fTy
    rwTyB <- coreToTypeB rwTy
    let realWorld = mkPrimInfo (showt 'GHC.Prim.realWorld#) (runClosedType rwTyB)
                      WorkNever SingleResult S.NoUnfolding
    tyLamsB [rTV, oTV] $
      lamB 0 (C.mkUnsafeSystemName "f" 0) fTyB $
      appB (varB 0) (primB realWorld)
runRWB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | Given the type:
--
-- @forall (n :: Nat) (a :: Type) .Knownnat n => Typeable a => (BitVector n -> a) -> BitVector n -> a@
--
-- Generate the term:
--
-- @/\(n:Nat)./\(a:TYPE r).\(kn:KnownNat n).\(f:a -> BitVector n).f@
checkUnpackUndefB :: Type -> C2C TmB
checkUnpackUndefB ty
  | Just (nTV, ty1) <- forAllView ty
  , Just (aTV, funTy) <- forAllView ty1
  , Just (knTy, r0Ty) <- funView funTy
  , Just (tpTy, r1Ty) <- funView r0Ty
  , Just (fTy, _) <- funView r1Ty
  = do
    knTyB <- coreToTypeB knTy
    tpTyB <- coreToTypeB tpTy
    fTyB <- coreToTypeB fTy
    tyLamsB [nTV, aTV] $
      lamB 0 (C.mkUnsafeSystemName "kn" 0) knTyB $
      lamB 1 (C.mkUnsafeSystemName "tp" 1) tpTyB $
      lamB 2 (C.mkUnsafeSystemName "f" 2) fTyB $
      varB 2
checkUnpackUndefB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | Given the type:
--
-- @forall (name :: Symbol) (a :: Type) . a -> (name ::: a)@
--
-- Generate the term:
--
-- @/\(name:Symbol)./\(a:Type).\(x:a) -> <TICK>x@
nameModB :: NameMod -> Type -> C2C TmB
nameModB sa ty
  | Just (nmTV, ty1) <- forAllView ty
  , Just (aTV, funTy) <- forAllView ty1
  , Just (xTy, _) <- funView funTy
  = do
    xTyB <- coreToTypeB xTy
    tyLamsB [nmTV, aTV] $
      lamB 0 (C.mkUnsafeSystemName "x" 0) xTyB $
      tickB (nameModTickB sa (varTyB (varKey nmTV))) (varB 0)
nameModB _ ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | Given the type:
--
-- @forall (a :: Type) . String -> a -> a@
--
-- Generate the term:
--
-- @/\(a:Type).\(ctx:String).\(x:a) -> x@
xToErrorCtxB :: Type -> C2C TmB
xToErrorCtxB ty
  | Just (aTV, funTy) <- forAllView ty
  , Just (ctxTy, rTy) <- funView funTy
  , Just (xTy, _) <- funView rTy
  = do
    ctxTyB <- coreToTypeB ctxTy
    xTyB <- coreToTypeB xTy
    tyLamsB [aTV] $
      lamB 0 (C.mkUnsafeSystemName "ctx" 0) ctxTyB $
      lamB 1 (C.mkUnsafeSystemName "x" 1) xTyB $
      varB 1
xToErrorCtxB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | Given the type
--
-- > forall n dom a . Vec n (Attr String) -> Signal dom a -> Signal dom a
--
-- Generate the term:
--
-- > /\(n:Nat) (dom:Symbol) (a:Type).\(attrs:Vec n (Attr String)) (x:Signal dom a).<TICK attrs> x
annotateRegB :: Type -> C2C TmB
annotateRegB ty
  | Just (nTV, ty1) <- forAllView ty
  , Just (domTV, ty2) <- forAllView ty1
  , Just (aTV, funTy) <- forAllView ty2
  , Just (attrTy, rTy) <- funView funTy
  , Just (xTy, _) <- funView rTy
  = do
    attrTyB <- coreToTypeB attrTy
    xTyB <- coreToTypeB xTy
    tyLamsB [nTV, domTV, aTV] $
      lamB 1 (C.mkUnsafeSystemName "attrs" 1) attrTyB $
      lamB 0 (C.mkUnsafeSystemName "x" 0) xTyB $
      tickB (attributesTickB (varTyB (varKey nTV)) (varB 1)) (varB 0)
annotateRegB ty = error ($(curLoc) ++ showPprUnsafe ty)
