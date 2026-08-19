{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2021-2026, QBayLogic B.V.
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
import           Control.Lens                ((^.), (%~), (&), (%=), (.~), use, view, makeLenses)
import           Control.Applicative         ((<|>))
import           Control.Monad.Extra         (ifM, andM)
import           Control.Monad.RWS.Strict    (RWS)
import qualified Control.Monad.RWS.Strict    as RWS
import           Data.Bifunctor              (second)
import           GHC.Float                   (castDoubleToWord64, castFloatToWord32)
import qualified Data.ByteString.Char8       as Char8
import           Data.Char                   (isDigit)
import           Data.Hashable               (Hashable (..))
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8)
import qualified Data.Traversable            as T
import           Data.String.Interpolate     (__i)
import qualified Text.Read                   as Text
import           Data.Primitive.ByteArray    (ByteArray(ByteArray))
import qualified GHC.Data.Strict             as GHC
import           GHC.Num.Integer             (integerToBigNatClamp#)
import           Language.Haskell.Syntax.Basic (FieldLabelString (..))

-- GHC API
import GHC.Core.Reduction (Reduction(Reduction), HetReduction(..))
import GHC.Builtin.Types (falseDataCon)
import GHC.Builtin.Types.Prim (fUNTyCon)
import GHC.Core.Coercion.Axiom
  (CoAxiom (co_ax_branches), CoAxBranch (cab_lhs,cab_rhs), fromBranches)
import GHC.Core.Coercion (Role (Nominal), coercionType, coercionKind)
import GHC.Core.FVs  (exprSomeFreeVars)
import GHC.Core
  (AltCon (..), Bind (..), CoreExpr, Expr (..), Unfolding (..),
   Alt(..),
   collectArgs, rhssOfAlts, unfoldingTemplate)
import GHC.Types.Tickish (GenTickish (..))
import GHC.Core.DataCon
  (DataCon, dataConExTyCoVars, dataConName, dataConRepArgTys, dataConTag,
   dataConTyCon, dataConUnivTyVars, dataConWorkId, dataConFieldLabels, flLabel,
   HsImplBang(..), dataConImplBangs)
import GHC.Core.FamInstEnv
  ( FamInst (..), FamInstEnvs
  , familyInstances, normaliseType, emptyFamInstEnvs, topReduceTyFamApp_maybe
  )
import GHC.Data.FastString (FastString, mkFastString, unpackFS, bytesFS)
import GHC.Types.Id (isDataConId_maybe)
import GHC.Types.Id.Info (IdDetails (..), unfoldingInfo)
import GHC.Types.Literal (Literal (..), LitNumType (..), literalType)
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Types.Name
  (Name, nameModule_maybe, nameOccName, nameUnique, getSrcSpan)
import GHC.Types.Name.Occurrence (occNameFS, occNameString)
import GHC.Data.Pair (Pair (..))
import GHC.Types.SrcLoc (SrcSpan (..), isGoodSrcSpan)
import GHC.Core.TyCon
  (AlgTyConRhs (..), TyCon, tyConName, algTyConRhs, isAlgTyCon, isFamilyTyCon,
   isNewTyCon, isPrimTyCon, isTupleTyCon,
   isClosedSynFamilyTyConWithAxiom_maybe, expandSynTyCon_maybe, tyConArity,
   tyConDataCons, tyConKind, tyConName, tyConUnique, isClassTyCon, isPromotedDataCon_maybe)
import GHC.Core.TyCon (ExpandSynResult (..))
import GHC.Core.Type (tyConAppFunTy_maybe)
import GHC.Core.Type (mkTvSubstPrs, substTy, coreView)
import GHC.Core.Utils (exprType)
import GHC.Core.TyCo.Rep (Coercion (..), TyLit (..), Type (..), scaledThing)
import GHC.Types.Unique (Uniquable (..), Unique, getKey)
import GHC.Types.Var
  (Id, TyVar, Var, VarBndr (..), idDetails, isTyVar, varName, varType,
   varUnique, idInfo, isGlobalId)
import GHC.Types.Var.Set (isEmptyVarSet)

-- Local imports
import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Annotations.SynthesisAttributes (Annotate, Attr(..))
import qualified Clash.Core.DataCon          as C
import qualified Clash.Core.Literal          as C
import qualified Clash.Core.Name             as C
import qualified Clash.Core.Pretty           as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.TyCon            as C
import qualified Clash.Core.Subst            as C (aeqType)
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
  go (TyVarTy v1) (TyVarTy v2) = v1 == v2
  go (AppTy l1 r1) (AppTy l2 r2) = go l1 l2 && go r1 r2
  go (TyConApp tc1 args1) (TyConApp tc2 args2) =
    tc1 == tc2 && goList args1 args2
  go (ForAllTy (Bndr v1 f1) t1) (ForAllTy (Bndr v2 f2) t2) =
    v1 == v2 && f1 == f2 && go t1 t2
  go (FunTy f1 m1 a1 r1) (FunTy f2 m2 a2 r2) =
    f1 == f2 && go m1 m2 && go a1 a2 && go r1 r2
  go (LitTy l1) (LitTy l2) = l1 == l2
  -- XXX: Not sure how to handle casts/coercions, so for now just claim they're
  --      not equal. That's fair in the only context this is used: caching.
  go _ _ = False

  goList (t1:ts1) (t2:ts2) = go t1 t2 && goList ts1 ts2
  goList [] [] = True
  goList _ _ = False

-- | Hash a GHC type, matching 'eqType': types that compare equal hash equally.
hashType :: Int -> Type -> Int
hashType = go
 where
  go s ty = case ty of
    TyVarTy v -> hashWithSalt (tag s 0) (getKey (varUnique v))
    AppTy l r -> go (go (tag s 1) l) r
    TyConApp tc args -> foldl go (hashWithSalt (tag s 2) (getKey (tyConUnique tc))) args
    ForAllTy (Bndr v _) t -> go (hashWithSalt (tag s 3) (getKey (varUnique v))) t
    FunTy _ m a r -> go (go (go (tag s 4) m) a) r
    LitTy l -> case l of
      NumTyLit i -> hashWithSalt (tag s 5) i
      StrTyLit str -> hashWithSalt (tag s 6) (unpackFS str)
      CharTyLit c -> hashWithSalt (tag s 7) c
    CastTy t _ -> go (tag s 8) t
    CoercionTy _ -> tag s 9

  tag :: Int -> Int -> Int
  tag = hashWithSalt

data GHC2CoreState
  = GHC2CoreState
  { _tyConMap :: C.UniqMap TyCon
  , _nameMap  :: HashMap Name Text
  , _varTypeMap :: HashMap Name C.Type
  -- ^ Cache for converted types of global variables ('varType'). See
  -- 'coreToVarType'.
  , _convertedTypeCache :: HashMap StructuralType C.Type
  -- ^ Cache for converted types, keyed on the GHC type. See 'coreToType'.
  }

makeLenses ''GHC2CoreState

data GHC2CoreEnv
  = GHC2CoreEnv
  { _srcSpan :: SrcSpan
  , _famInstEnvs :: FamInstEnvs
  }

makeLenses ''GHC2CoreEnv

emptyGHC2CoreState :: GHC2CoreState
emptyGHC2CoreState =
  GHC2CoreState mempty HashMap.empty HashMap.empty HashMap.empty

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

-- Note [Casting signals]
--
-- This note explains why Clash implements some primitives, most notably
-- 'mapSignal#', 'appSignal#', and 'signal#', using Casts.
--
-- For user code, (de)constructing 'Signal's using Signal's constructor (:-)
-- is not synthesizable due to the ability to write non-causal properties.
-- Functions that need to (de)construct a Signal this way generally need a
-- blackbox implementation. This way, Clash will use the blackbox to produce
-- HDL instead of the Core generated by GHC. A number of primitives are
-- excluded from this rule however. These primitives are:
--
--   1. Clash.Signal.Internal.mapSignal#  (fmap)
--   2. Clash.Signal.Internal.appSignal#  (<*>)
--   3. Clash.Signal.Internal.signal#     (pure)
--
-- (and similarly joinSignal#, traverse#, vecBundle#, and the generated
-- bundle#/unbundle# functions).
--
-- 'mapSignal#' (1) and 'appSignal#' (2) are both implemented very similarly,
-- so we'll only go over the first. Given the following:
--
--    f  :: a -> b
--    xs :: Signal dom a
--
-- 'coreToTerm' will rewrite the following pseudocode:
--
--    fmap f xs
--
--  to
--
--      coerce @b @(Signal dom b)
--    $ f
--    $ coerce @(Signal dom a) @a xs
--
-- In other words, 'xs' will be cast from a 'Signal dom a' to 'a' and given
-- to 'f' as its first argument. The result of that expression, of type 'b',
-- is cast back to 'Signal dom b'. This seems rather silly; something of type
-- 'Signal dom a' and 'a' have very different representations! However, in
-- Clash's context this is perfectly safe to do: all functions except (1),
-- (2), and (3) (de)constructing signals must have blackbox implementations.
-- Hence, Clash will never see a Signal constructor. This in turn means that
-- the difference in representation does not matter, making it safe to do
-- the coercion.
--
-- The implementation of 'signal#' (pure) is a bit different, but allows us
-- to do constant elimination on signals producing a constant value. In
-- pseudocode:
--
--    pure (x :: a)
--
-- is rewritten to:
--
--    coerce @a @(Signal dom a) x
--
-- Therefore, the expression:
--
--    fmap f (pure x)
--
-- will turn into:
--
--      coerce @b @(Signal dom b)
--    $ f
--    $ coerce @(Signal dom a) @a
--    $ coerce @a @(Signal dom a)
--    $ x
--
-- 'elimCastCast' can eliminate the back-to-back coercions, which in turn
-- enables constant folding on 'x'.
--
-- N.B.: 'coerce @a @b x' is represented as 'Cast x a b' in Clash Core.

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
        -- The Signal transformers (mapSignal#, signal#, appSignal#,
        -- joinSignal#, vecBundle#, bundle#, unbundle#) are not handled here:
        -- their applications decompose through 'term'' and 'var', where they
        -- are translated to terms casting between @Signal dom a@ and @a@.
        -- See Note [Casting signals].

        -- Saturated applications of the cast-translated Signal combinators
        -- are beta-reduced at translation time, with 'mkCastS' cancelling
        -- the cast pairs of directly composed combinators (e.g.
        -- @f \<$\> x \<*\> y@) before normalization ever sees them. See
        -- Note [Casting signals]. Unsaturated occurrences fall through to
        -- 'var', which emits the lambda-with-casts form.
        go "Clash.Signal.Internal.mapSignal#" args
          | [Type _aTy, Type bTy, Type _domTy, fE, xE] <- args
          = do bC  <- coreToType bTy
               aC  <- coreToType (exprType fE) >>= \fTyC -> case C.tyView fTyC of
                        C.FunTy argC _ -> return argC
                        _ -> return bC -- unreachable for well-typed input
               saC <- coreToType (exprType xE)
               sbC <- coreToType (exprType e)
               fC  <- term fE
               xC  <- term xE
               return (mkCastS (C.App fC (mkCastS xC saC aC)) bC sbC)
        go "Clash.Signal.Internal.signal#" args
          | [Type aTy, Type _domTy, xE] <- args
          = do aC  <- coreToType aTy
               saC <- coreToType (exprType e)
               xC  <- term xE
               return (mkCastS xC aC saC)
        go "Clash.Signal.Internal.appSignal#" args
          | [Type _domTy, Type aTy, Type bTy, fE, xE] <- args
          = do aC  <- coreToType aTy
               bC  <- coreToType bTy
               sfC <- coreToType (exprType fE)
               saC <- coreToType (exprType xE)
               sbC <- coreToType (exprType e)
               fC  <- term fE
               xC  <- term xE
               return (mkCastS (C.App (mkCastS fC sfC (C.mkFunTy aC bC))
                                      (mkCastS xC saC aC))
                               bC sbC)
        go "Clash.Signal.Internal.traverse#" args
          | [Type _fTy, Type aTy, Type _bTy, Type _domTy, _dictE, gE, xE] <- args
          = do aC  <- coreToType aTy
               saC <- coreToType (exprType xE)
               gTyC <- coreToType (exprType gE)
               fsbC <- coreToType (exprType e)
               case C.tyView gTyC of
                 C.FunTy _ fbC -> do
                   gC <- term gE
                   xC <- term xE
                   return (mkCastS (C.App gC (mkCastS xC saC aC)) fbC fsbC)
                 _ -> term' e -- unreachable for well-typed input
        go "Clash.Signal.Internal.joinSignal#" args
          | [Type _domTy, Type _aTy, xE] <- args
          = castOnly xE
        go "Clash.Signal.Bundle.vecBundle#" args
          | [Type _, Type _, Type _, xE] <- args
          = castOnly xE
        -- The generated bundle#/unbundle# functions: any number of
        -- foralls, one value argument, a pure cast.
        go nm args
          | Just _ <- parseBundle "bundle" nm <|> parseBundle "unbundle" nm
          , (tyArgs, [xE]) <- span isTypeArg args
          , all isTypeArg tyArgs
          = castOnly xE
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
        go "Clash.Annotations.SynthesisAttributes.annotateReg" args
          | [ Type nTy, _domTy, _aTy, attrs, x] <- args
          = C.Tick <$> (C.Attributes <$> coreToType nTy <*> term attrs) <*> term x
        go _ _ = term' e

        -- A combinator whose whole meaning is a cast of its single value
        -- argument from its own type to the application's result type.
        castOnly xE = do
          fromC <- coreToType (exprType xE)
          toC   <- coreToType (exprType e)
          xC    <- term xE
          return (mkCastS xC fromC toC)

        isTypeArg (Type {}) = True
        isTypeArg _         = False

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

    -- Keep all casts. Since Clash's core language does not have
    -- evidence-carrying coercions, a cast is fully described by its source
    -- and target type. Casts whose source and target convert to the same
    -- Clash core type (e.g. multiplicity or levity coercions) are dropped:
    -- they carry no information in Clash's type language.
    term' (Cast e co) = do
      let (Pair ty1 ty2) = coercionKind co
      ty1C <- coreToType ty1
      ty2C <- coreToType ty2
      e1 <- term e
      if ty1C == ty2C
        then return e1
        else return (C.Cast e1 ty1C ty2C)
    term' (Tick (SourceNote rsp _) e) =
      C.Tick (C.SrcSpan (RealSrcSpan rsp GHC.Nothing)) <$>
             addUsefull (RealSrcSpan rsp GHC.Nothing) (term e)
    term' (Tick _ e) = term e
    term' (Type t) =
      C.TyApp (C.Prim (C.PrimInfo (pack "_TY_") C.undefinedTy C.WorkNever C.SingleResult C.NoUnfolding))
        <$> coreToType t
    term' (Coercion co) =
      C.TyApp (C.Prim (C.PrimInfo (pack "_CO_") C.undefinedTy C.WorkNever C.SingleResult C.NoUnfolding))
        <$> coreToType (coercionType co)


    termSP sp = fmap (second unSrcSpanRB) . RWS.listen . addUsefullR sp . term
    coreToIdSP sp = addUsefullR sp . coreToId


    lookupPrim :: Text -> Maybe (Maybe CompiledPrimitive)
    lookupPrim nm = extractPrim <$> HashMap.lookup nm primMap

    var x = do
        xPrim <- if isGlobalId x then coreToPrimVar x else coreToVar x
        let xNameS = C.nameOcc xPrim
        xType  <- coreToVarType x
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
              | f == "Clash.Annotations.SynthesisAttributes.annotateReg"
              -> return (annotateRegTerm xType)
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

    alt _   (Alt DEFAULT      _  e) = (C.DefaultPat,) <$> term e
    alt _   (Alt (LitAlt l)   _  e) = (C.LitPat (coreToLiteral l),) <$> term e
    alt sp0 (Alt (DataAlt dc) xs e) = case span isTyVar xs of
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

coreToDataCon :: DataCon
              -> C2C C.DataCon
coreToDataCon dc = do
    repTys <- mapM (coreToType . scaledThing) (dataConRepArgTys dc)
    dcTy   <- coreToType (varType $ dataConWorkId dc)
    mkDc dcTy repTys
  where
    mkDc dcTy repTys = do
      let decLabel = decodeUtf8 . bytesFS . field_label . flLabel
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
--
-- Conversions are memoized on the GHC type, which also shares the converted
-- types. The conversion runs 'withoutSrcSpan' so that it depends on the GHC
-- type alone; nothing ever reads back the source spans of names inside a
-- type, so no information is lost.
coreToType
  :: Type
  -> C2C C.Type
coreToType ty = do
  cache <- use convertedTypeCache
  case HashMap.lookup (StructuralType ty) cache of
    Just ty1 -> pure ty1
    Nothing -> do
      ty1 <- withoutSrcSpan convert
      convertedTypeCache %= HashMap.insert (StructuralType ty) ty1
      pure ty1
  where
    convert = ty'' >>= annotateType ty

    ty'' | Just ty' <- coreView ty = coreToType ty'
         | TyConApp tc xs <- ty = do
             envs <- view famInstEnvs
             case topReduceTyFamApp_maybe envs tc xs of
               Nothing -> coreToType' ty
               Just (HetReduction (Reduction _ ty') _) -> coreToType ty'
         | otherwise = coreToType' ty

coreToType'
  :: Type
  -> C2C C.Type
coreToType' (TyVarTy tv) = C.VarTy <$> coreToTyVar tv
coreToType' (TyConApp tc args)
  | Just (FunTy _ _ ty1 ty2) <- tyConAppFunTy_maybe tc args = C.mkFunTy <$> coreToType ty1 <*> coreToType ty2
  -- A partially applied FUN, e.g. resulting from the eta-reduced right-hand
  -- side of a newtype of a function type. Convert to Clash's Arrow, dropping
  -- the multiplicity argument, such that 'tyView' recognizes the type as a
  -- function type once it is fully applied.
  -- See Note [Arrow arguments] in Clash.Core.Type.
  | tc == fUNTyCon, (_mult:rest) <- args
  = foldl C.AppTy (C.ConstTy C.Arrow) <$> mapM coreToType rest
  | otherwise     = case expandSynTyCon_maybe tc args of
                      ExpandsSyn substs synTy remArgs -> do
                        let substs' = mkTvSubstPrs substs
                            synTy'  = substTy substs' synTy
                        foldl C.AppTy <$> coreToType synTy' <*> mapM coreToType remArgs
                      _ -> do
                        tcName <- coreToName tyConName tyConUnique qualifiedNameString tc
                        tyConMap %= C.insertIfAbsent tcName tc
                        C.mkTyConApp <$> (pure tcName) <*> mapM coreToType args
coreToType' (ForAllTy (Bndr tv _) ty)   = C.ForAllTy <$> coreToTyVar tv <*> coreToType ty
-- TODO: save the distinction between => and ->
coreToType' (FunTy _ _ ty1 ty2)             = C.mkFunTy <$> coreToType ty1 <*> coreToType ty2
coreToType' (LitTy tyLit)    = return $ C.LitTy (coreToTyLit tyLit)
coreToType' (AppTy ty1 ty2)  = C.AppTy <$> coreToType ty1 <*> coreToType' ty2
coreToType' (CastTy t (Refl{})) = coreToType' t
coreToType' t@(CastTy _ _)   = error ("Cannot handle CastTy " ++ showPprUnsafe t)
coreToType' t@(CoercionTy _) = error ("Cannot handle CoercionTy " ++ showPprUnsafe t)

coreToTyLit :: TyLit
            -> C.LitTy
coreToTyLit (NumTyLit i) = C.NumTy (fromInteger i)
coreToTyLit (StrTyLit s) = C.SymTy (unpackFS s)
coreToTyLit (CharTyLit c) = C.CharTy c

coreToTyVar :: TyVar
            -> C2C C.TyVar
coreToTyVar tv =
  C.mkTyVar <$> coreToType (varType tv) <*> coreToVar tv

coreToId :: Id
         -> C2C C.Id
coreToId i = do
  C.mkId <$> coreToVarType i <*> pure scope <*> coreToVar i
 where
  scope = if isGlobalId i then C.GlobalId else C.LocalId

coreToVar :: Var
          -> C2C (C.Name a)
coreToVar = coreToName varName varUnique qualifiedNameStringM

-- | Convert the type of a variable, memoized on the variable's name.
--
-- Only global variables are memoized: their names come from GHC's name cache
-- and are globally unique, so the same name always refers to the same
-- variable, whose type is intrinsic. Names of local variables (e.g. binders
-- instantiated when loading unfoldings from interface files) can collide
-- between declarations and must not share cache entries.
coreToVarType :: Var -> C2C C.Type
coreToVarType v
  | isGlobalId v = makeCached (varName v) varTypeMap (coreToType (varType v))
  | otherwise = coreToType (varType v)

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
  go tvs (C.tyView -> C.FunTy argTy resTy) =
    if length tvs /= nTyVarsExpected then
      -- Internal error: should never happen unless we change the type of
      -- bundle / unbundle.
      error $ $(curLoc) ++ show (length tvs) ++ " vs " ++ show nTyVarsExpected
    else
      let sigName = C.mkLocalId argTy (C.mkUnsafeSystemName "c$s" 0) in
      foldr C.TyLam (C.Lam sigName (C.Cast (C.Var sigName) argTy resTy)) (reverse tvs)
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
-- | Syntactic smart constructor for the casts of the Signal combinators:
-- drops casts between alpha-equivalent types and merges (cancels)
-- back-to-back casts. Only used for the pairwise-introduced Signal casts,
-- whose types match syntactically when they meet; the full cast-equality
-- oracle is not needed (nor available) during GHC-to-Clash translation.
mkCastS :: C.Term -> C.Type -> C.Type -> C.Term
mkCastS e from to
  | C.aeqType from to = e
mkCastS (C.Tick t e) from to = C.Tick t (mkCastS e from to)
mkCastS (C.Cast e from0 to0) from to
  | C.aeqType to0 from = mkCastS e from0 to
mkCastS e from to = C.Cast e from to

mapSignalTerm :: C.Type
              -> C.Term
mapSignalTerm (C.ForAllTy aTV (C.ForAllTy bTV (C.ForAllTy clkTV funTy)))
  | (C.FunTy fTy funTy'') <- C.tyView funTy
  , (C.FunTy saTy sbTy)   <- C.tyView funTy''
  , (C.FunTy aTy bTy)     <- C.tyView fTy
  = let
      fName = C.mkUnsafeSystemName "f" 0
      xName = C.mkUnsafeSystemName "x" 1
      fId   = C.mkLocalId fTy fName
      xId   = C.mkLocalId saTy xName
    in
      C.TyLam aTV (
      C.TyLam bTV (
      C.TyLam clkTV (
      C.Lam   fId (
      C.Lam   xId (
      C.Cast (C.App (C.Var fId) (C.Cast (C.Var xId) saTy aTy)) bTy sbTy)))))

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
  | (C.FunTy aTy saTy) <- C.tyView funTy
  = let
      xName = C.mkUnsafeSystemName "x" 0
      xId   = C.mkLocalId aTy xName
    in
      C.TyLam aTV (
      C.TyLam domTV (
      C.Lam   xId (
      C.Cast (C.Var xId) aTy saTy)))

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
  | (C.FunTy sfTy funTy'') <- C.tyView funTy
  , (C.FunTy saTy sbTy) <- C.tyView funTy''
  , (C.TyConApp _ [_,fTy]) <- C.tyView sfTy
  , (C.FunTy aTy bTy) <- C.tyView fTy
  = let
      fName = C.mkUnsafeSystemName "f" 0
      xName = C.mkUnsafeSystemName "x" 1
      fId   = C.mkLocalId sfTy fName
      xId   = C.mkLocalId saTy xName
    in
      C.TyLam domTV (
      C.TyLam aTV (
      C.TyLam bTV (
      C.Lam   fId (
      C.Lam   xId (
      C.Cast (C.App (C.Cast (C.Var fId) sfTy fTy)
                    (C.Cast (C.Var xId) saTy aTy))
             bTy
             sbTy)))))

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
  | (C.FunTy vecSigTy sigVecTy) <- C.tyView funTy
  = let
        vsName = C.mkUnsafeSystemName "vs" 0
        vsId   = C.mkLocalId vecSigTy vsName
    in
        C.TyLam tTV (
        C.TyLam nTV (
        C.TyLam aTV (
        C.Lam   vsId (
        C.Cast (C.Var vsId) vecSigTy sigVecTy))))

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
    , (C.FunTy saTy   fsbTy)  <- C.tyView funTy2
    , (C.FunTy aTy    fbTy)   <- C.tyView gTy
    = let
        dictName = C.mkUnsafeSystemName "dict" 0
        gName    = C.mkUnsafeSystemName "g" 1
        xName    = C.mkUnsafeSystemName "x" 2
        dictId   = C.mkLocalId dictTy dictName
        gId      = C.mkLocalId gTy gName
        xId      = C.mkLocalId saTy xName
      in
        C.TyLam fTV (
        C.TyLam aTV (
        C.TyLam bTV (
        C.TyLam domTV (
        C.Lam   dictId (
        C.Lam   gId (
        C.Lam   xId (
        C.Cast (C.App (C.Var gId) (C.Cast (C.Var xId) saTy aTy)) fbTy fsbTy)))))))

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

-- | Given the type
--
-- > forall n dom a . Vec n (Attr String) -> Signal dom a -> Signal dom a
--
-- Generate the term:
--
-- > /\(n:Nat) (dom:Symbol) (a:Type).\(attrs:Vec n (Attr String)) (x:Signal dom a).<TICK attrs> x
annotateRegTerm
  :: C.Type
  -> C.Term
annotateRegTerm (C.ForAllTy nTV (C.ForAllTy domTV (C.ForAllTy aTV funTy)))
  | C.FunTy attrTy rTy <- C.tyView funTy
  , C.FunTy xTy _ <- C.tyView rTy
  = let
      -- Safe to use `mkUnsafeSystemName` here, because we're building the
      -- identity \x.x, so any shadowing of 'x' would be the desired behavior.
      xName            = C.mkUnsafeSystemName "x" 0
      xId              = C.mkLocalId xTy xName
      attrName         = C.mkUnsafeSystemName "attrs" 1
      attrId           = C.mkLocalId attrTy attrName
    in
      C.TyLam nTV (
      C.TyLam domTV (
      C.TyLam aTV (
      C.Lam   attrId (
      C.Lam   xId (
      C.Tick (C.Attributes (C.VarTy nTV) (C.Var attrId)) (C.Var xId))))))

annotateRegTerm ty = error ($(curLoc) ++ show ty)

isDataConWrapId :: Id -> Bool
isDataConWrapId v = case idDetails v of
  DataConWrapId {} -> True
  _                -> False
