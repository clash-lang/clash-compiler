{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2017     , Google Inc.
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Views on, and utilities for, the types of scoped Clash Core.

  = Scopes

  The functions that look through newtypes and type families ('coreView1' and
  everything built on it) take the 'Scope' of the type: unwrapping a newtype or
  reducing a type family substitutes types for type variables, which may
  rename binders (see "Clash.Core.TyCon"). Closed types, such as the
  types of globals, are in the empty scope 'emptyScope'.

  Rather than a 'Clash.Core.TyCon.TyConMap', these functions take a
  t'TypeEnv', which caches the signatures of the type constructors as scoped
  types (see "Clash.Core.Scoped.TypeEnv").

  = Function types

  A polymorphic function type binds type variables, which scope over the rest
  of the type. So rather than a list @[Either TyVar Type]@, splitting it gives
  a t'PiTele': a telescope of quantified type variables and argument types.
  Pattern match on it as you would on the list: e.g. the second argument type
  of a type that starts with two argument types is @t@ in
  @'PiArg' _ ('PiArg' t _)@, in the scope of the type itself, whereas in
  @'PiForall' _ ('PiArg' t _)@ it is in the scope of the quantified type
  variable.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Type
  ( -- * Views
    TypeView (..)
  , tyView
  , coreView
  , coreView1
  , normalizeType
    -- * Constructing types
  , mkTyConTy
  , mkFunTy
  , mkTyConApp
  , splitTyConAppM
    -- * Function types
  , PiTele (..)
  , mkPolyFunTy
  , piTeleNameBinders
  , piTeleTyArity
  , piTeleTmArity
  , anyPiArg
  , SplitFunForall (..)
  , splitFunForallTy
  , SplitCoreFunForall (..)
  , splitCoreFunForallTy
  , splitFunTy
  , splitFunTys
  , applyFunTy
  , isFunTy
  , isPolyTy
  , isPolyFunTy
  , isPolyFunCoreTy
    -- * Type families
  , reduceTypeFamily
  , isTypeFamilyApplication
  , argView
    -- * Literal types
  , litView
  , symLitView
  , charLitView
  , tyNatSize
  , tyLitShow
    -- * Predicates
  , isClassTy
  , isIntegerTy
  , isSignalType
  , isEnable
  , isClockOrReset
    -- * Annotations
  , typeAttrs
  , stripAnnTypes
  , stripIP
  ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Except (Except, throwE)
import Data.Char (chr, ord)
import Data.Coerce (coerce)
import qualified Data.HashSet as HashSet
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text.Extra (showt)
import GHC.Base (isTrue#, (==#))
import GHC.Integer (smallInteger)
import GHC.Integer.Logarithms (integerLogBase#)
import GHC.Stack (HasCallStack)

import GHC.Builtin.Names
  ( integerTyConKey, ipClassKey, ordEQDataConKey, ordGTDataConKey
  , ordLTDataConKey, typeCharCmpTyFamNameKey, typeCharToNatTyFamNameKey
  , typeConsSymbolTyFamNameKey, typeNatAddTyFamNameKey, typeNatCmpTyFamNameKey
  , typeNatDivTyFamNameKey, typeNatExpTyFamNameKey, typeNatModTyFamNameKey
  , typeNatMulTyFamNameKey, typeNatSubTyFamNameKey, typeNatToCharTyFamNameKey
  , typeSymbolAppendFamNameKey, typeSymbolCmpTyFamNameKey
  , typeUnconsSymbolTyFamNameKey )
import GHC.Types.SrcLoc (wiredInSrcSpan)

import Clash.Core.Basic (ConstTy (..), LitTy (..))
import Clash.Annotations.SynthesisAttributes (Attr)
import Clash.Core.DataCon (DataCon (dcName))
import Clash.Core.Name (Name (..), NameSort (User))
import Clash.Core.Pretty (showPpr)
import Clash.Debug (traceIf)
import Clash.Unique (fromGhcUnique)
import Clash.Util (clogBase, curLoc)

import Clash.Core.Scoped.Alpha (aeqTy)
import Clash.Core.DataCon (dataConInstArgTys)
import Clash.Core.Scoped.FreeVars (HasNameBinder (..))
import Clash.Core.Scoped.NameMap (NameBinders, emptyNameBinders, snocNameBinders)
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.TyCon
import Clash.Core.Scoped.TypeEnv

-- * Views

-- | An easier view on types, see 'tyView'
data TypeView n
  = FunTy !(Type n) !(Type n)
  -- ^ Function type
  | TyConApp !TyConName [Type n]
  -- ^ Applied type constructor
  | OtherType !(Type n)
  -- ^ Neither of the above

deriving instance Show (TypeView n)

-- | An easier view on types
--
-- Note [Arrow arguments]
--
-- Clash' Arrow type can either have 2 or 4 arguments, depending on who created it.
-- By default it has two arguments: the argument type of a function, and the result
-- type of a function.
--
-- So when do we have 4 arguments? When in Haskell/GHC land the arrow was
-- unsaturated. This can happen in instance heads, or in the eta-reduced
-- representation of newtypes. So what are those additional 2 arguments compared to
-- the "normal" function type? They're the kinds of argument and result type.
tyView :: Type n -> TypeView n
-- Manually unrolled, because it is called so often
tyView tOrig = case tOrig of
  ConstTy c -> case c of
    TyCon tc -> TyConApp tc []
    _ -> OtherType tOrig
  AppTy l0 res -> case l0 of
    ConstTy (TyCon tc) -> TyConApp tc [res]
    AppTy l1 arg -> case l1 of
      ConstTy Arrow -> FunTy arg res
      ConstTy (TyCon tc) -> TyConApp tc [arg, res]
      AppTy l2 resK -> case l2 of
        ConstTy (TyCon tc) -> TyConApp tc [resK, arg, res]
        AppTy l3 argK -> case l3 of
          ConstTy (TyCon tc) -> TyConApp tc [argK, resK, arg, res]
          ConstTy Arrow -> FunTy arg res -- See Note [Arrow arguments]
          _ -> case go [argK, resK, arg, res] l3 of
            (ConstTy (TyCon tc), args) -> TyConApp tc args
            _ -> OtherType tOrig
        _ -> OtherType tOrig
      _ -> OtherType tOrig
    _ -> OtherType tOrig
  _ -> OtherType tOrig
 where
  go args (AppTy ty1 ty2) = go (ty2:args) ty1
  go args t1 = (t1, args)

-- | A view on types in which newtypes are transparent, the Signal type is
-- transparent, and type functions are evaluated to WHNF (when possible).
--
-- Strips away all layers. If no layers are found it returns the given type.
coreView :: Distinct n => TypeEnv -> Scope n -> Type n -> Type n
coreView tenv sc ty = case coreView1 tenv sc ty of
  Nothing -> ty
  Just ty' -> coreView tenv sc ty'

-- | A view on types in which newtypes are transparent, the Signal type is
-- transparent, and type functions are evaluated to WHNF (when possible).
--
-- Only strips away one layer.
coreView1 :: Distinct n => TypeEnv -> Scope n -> Type n -> Maybe (Type n)
coreView1 tenv sc ty = case tyView ty of
  TyConApp tcNm args
    | nameOcc tcNm == "Clash.Signal.BiSignal.BiSignalIn"
    , [_, _, _, elTy] <- args
    -> Just elTy
    | nameOcc tcNm == "Clash.Signal.BiSignal.BiSignalOut"
    , [_, _, _, elTy] <- args
    -> Just elTy
    | nameOcc tcNm == "Clash.Signal.Internal.Signal"
    , [_, elTy] <- args
    -> Just elTy
    | otherwise
    -> case newTyConSigOf tenv tcNm of
         Just sig -> newTyConInstRhs sc sig args
         Nothing -> reduceTyConApp tenv sc tcNm args
  OtherType (AnnType _ ty') -> coreView1 tenv sc ty'
  _ -> Nothing

-- | Normalize a type, looking through Signals and newtypes.
--
-- For example: @Signal a (Vec (6-1) (Unsigned (3+1)))@ normalizes to
-- @Vec 5 (Unsigned 4)@
normalizeType :: Distinct n => TypeEnv -> Scope n -> Type n -> Type n
normalizeType tenv = go
 where
  go :: Distinct x => Scope x -> Type x -> Type x
  go sc ty = case tyView ty of
    TyConApp tcNm args
      -- These Clash types are implemented with newtypes. We need to keep these
      -- newtypes because they define the width of the numbers.
      | nameOcc tcNm == "Clash.Sized.Internal.BitVector.Bit" ||
        nameOcc tcNm == "Clash.Sized.Internal.BitVector.BitVector" ||
        nameOcc tcNm == "Clash.Sized.Internal.Index.Index" ||
        nameOcc tcNm == "Clash.Sized.Internal.Signed.Signed" ||
        nameOcc tcNm == "Clash.Sized.Internal.Unsigned.Unsigned"
      -> mkTyConApp tcNm (map (go sc) args)
      | otherwise
      -> case newTyConSigOf tenv tcNm of
           Just sig -> case newTyConInstRhs sc sig args of
             Just ty' -> go sc ty'
             Nothing -> ty
           Nothing ->
             let args' = map (go sc) args
                 ty' = mkTyConApp tcNm args'
             in case reduceTypeFamily tenv sc ty' of
                  Just ty'' -> go sc ty''
                  Nothing -> ty'
    FunTy ty1 ty2 -> mkFunTy (go sc ty1) (go sc ty2)
    OtherType (ForAllTy b ty') ->
      openTyBinder sc b ty' $ \sc' b' ty'' -> ForAllTy b' (go sc' ty'')
    _ -> ty

-- * Constructing types

-- | Make a type out of a type constructor
mkTyConTy :: TyConName -> Type n
mkTyConTy tc = ConstTy (TyCon tc)

-- | Make a function type of an argument and result type
mkFunTy :: Type n -> Type n -> Type n
mkFunTy t1 = AppTy (AppTy (ConstTy Arrow) t1)

-- | Make a type constructor application out of a type constructor and a list
-- of argument types
mkTyConApp :: TyConName -> [Type n] -> Type n
mkTyConApp tc = List.foldl' AppTy (mkTyConTy tc)

-- | Split a type constructor application in a type constructor and its
-- arguments
splitTyConAppM :: Type n -> Maybe (TyConName, [Type n])
splitTyConAppM (tyView -> TyConApp tc args) = Just (tc, args)
splitTyConAppM _ = Nothing

-- * Function types

-- | The quantified type variables and argument types of a polymorphic function
-- type, extending scope @n@ to @l@: the type variables scope over the rest of
-- the telescope. The scoped counterpart of @[Either TyVar Type]@, see
-- 'splitFunForallTy'.
data PiTele (n :: S) (l :: S) where
  PiNil :: PiTele n n
  -- | A quantified type variable
  PiForall :: !(TyBinder n i) -> !(PiTele i l) -> PiTele n l
  -- | An argument type
  PiArg :: !(Type n) -> !(PiTele n l) -> PiTele n l

deriving instance Show (PiTele n l)

-- | Make a polymorphic function type out of its quantified type variables and
-- argument types, and its result type.
mkPolyFunTy :: PiTele n l -> Type l -> Type n
mkPolyFunTy tele res = case tele of
  PiNil -> res
  PiForall b rest -> ForAllTy b (mkPolyFunTy rest res)
  PiArg a rest -> mkFunTy a (mkPolyFunTy rest res)

-- | The type variables a telescope binds, e.g. to move its result type out of
-- their scope with 'unsinkType'
piTeleNameBinders :: PiTele n l -> NameBinders n l
piTeleNameBinders = go emptyNameBinders
 where
  go :: NameBinders n i -> PiTele i l -> NameBinders n l
  go !acc = \case
    PiNil -> acc
    PiForall b rest -> go (snocNameBinders acc (nameBinderOf b)) rest
    PiArg _ rest -> go acc rest

-- | The number of quantified type variables
piTeleTyArity :: PiTele n l -> Int
piTeleTyArity = go 0
 where
  go :: Int -> PiTele x y -> Int
  go !acc = \case
    PiNil -> acc
    PiForall _ rest -> go (acc + 1) rest
    PiArg _ rest -> go acc rest

-- | The number of argument types
piTeleTmArity :: PiTele n l -> Int
piTeleTmArity = go 0
 where
  go :: Int -> PiTele x y -> Int
  go !acc = \case
    PiNil -> acc
    PiForall _ rest -> go acc rest
    PiArg _ rest -> go (acc + 1) rest

-- | Does any argument type of a telescope satisfy the predicate?
anyPiArg :: (forall x. Type x -> Bool) -> PiTele n l -> Bool
anyPiArg f = \case
  PiNil -> False
  PiForall _ rest -> anyPiArg f rest
  PiArg a rest -> f a || anyPiArg f rest

-- | A polymorphic function type, split into its quantified type variables and
-- argument types, and its result type, see 'splitFunForallTy'
data SplitFunForall n where
  SplitFunForall :: PiTele n l -> Type l -> SplitFunForall n

deriving instance Show (SplitFunForall n)

-- | Split a polymorphic function type into its quantified type variables and
-- argument types, and its result type.
splitFunForallTy :: Type n -> SplitFunForall n
splitFunForallTy ty = case ty of
  ForAllTy b res -> case splitFunForallTy res of
    SplitFunForall tele r -> SplitFunForall (PiForall b tele) r
  (tyView -> FunTy arg res) -> case splitFunForallTy res of
    SplitFunForall tele r -> SplitFunForall (PiArg arg tele) r
  _ -> SplitFunForall PiNil ty

-- | A polymorphic function type, split into its quantified type variables and
-- argument types, and its result type, together with the scope of the result
-- type, see 'splitCoreFunForallTy'
data SplitCoreFunForall n where
  SplitCoreFunForall :: DExt n l => Scope l -> PiTele n l -> Type l -> SplitCoreFunForall n

-- | Split a polymorphic function type into its quantified type variables and
-- argument types, and its result type, looking through Signals, newtypes and
-- type families ('coreView1'). The result type is as it was before looking
-- through it.
--
-- Gives the scope of the result type: this goes under the quantified type
-- variables, renaming them if they clash with the scope.
splitCoreFunForallTy :: Distinct n => TypeEnv -> Scope n -> Type n -> SplitCoreFunForall n
splitCoreFunForallTy tenv sc0 ty0 = go sc0 ty0 ty0
 where
  go :: Distinct x => Scope x -> Type x -> Type x -> SplitCoreFunForall x
  go sc origTy ty
    | Just ty' <- coreView1 tenv sc ty = go sc origTy ty'
    | ForAllTy b res <- ty =
        openTyBinder sc b res $ \sc' b' res' -> case go sc' res' res' of
          SplitCoreFunForall scR tele r -> SplitCoreFunForall scR (PiForall b' tele) r
    | FunTy arg res <- tyView ty = case go sc res res of
        SplitCoreFunForall scR tele r -> SplitCoreFunForall scR (PiArg arg tele) r
    | otherwise = SplitCoreFunForall sc PiNil origTy

-- | Split a function type in an argument and result type.
splitFunTy :: Distinct n => TypeEnv -> Scope n -> Type n -> Maybe (Type n, Type n)
splitFunTy tenv sc ty
  | Just ty' <- coreView1 tenv sc ty = splitFunTy tenv sc ty'
  | FunTy arg res <- tyView ty = Just (arg, res)
  | otherwise = Nothing

-- | Split a (monomorphic) function type in its argument types and result type.
splitFunTys :: Distinct n => TypeEnv -> Scope n -> Type n -> ([Type n], Type n)
splitFunTys tenv sc ty0 = go [] ty0 ty0
 where
  go args origTy ty
    | Just ty' <- coreView1 tenv sc ty = go args origTy ty'
    | FunTy arg res <- tyView ty = go (arg:args) res res
    | otherwise = (reverse args, origTy)

-- | Apply a function type to an argument type and get the result type.
applyFunTy :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Type n -> Type n -> Type n
applyFunTy tenv sc ty arg
  | Just ty' <- coreView1 tenv sc ty = applyFunTy tenv sc ty' arg
  | FunTy _ resTy <- tyView ty = resTy
  | otherwise = error $ $(curLoc) ++ "Report as bug: not a FunTy"

-- | Is a type a function type?
isFunTy :: Distinct n => TypeEnv -> Scope n -> Type n -> Bool
isFunTy tenv sc = isJust . splitFunTy tenv sc

-- | Is a type polymorphic?
isPolyTy :: Type n -> Bool
isPolyTy = \case
  ForAllTy {} -> True
  (tyView -> FunTy _ res) -> isPolyTy res
  _ -> False

-- | Is a type a polymorphic or function type?
isPolyFunTy :: Type n -> Bool
isPolyFunTy = \case
  ForAllTy {} -> True
  (tyView -> FunTy {}) -> True
  _ -> False

-- | Is a type a polymorphic or function type under 'coreView1'?
isPolyFunCoreTy :: Distinct n => TypeEnv -> Scope n -> Type n -> Bool
isPolyFunCoreTy tenv sc ty
  | Just ty' <- coreView1 tenv sc ty = isPolyFunCoreTy tenv sc ty'
  | otherwise = case tyView ty of
      FunTy {} -> True
      OtherType (ForAllTy {}) -> True
      _ -> False

-- * Type families

{- [Note: Eager type families]

I don't know whether type families are evaluated strictly or lazily, but since
type families do not reduce on stuck argument, we assume strictly.
-}

-- | Reduce a type family application, by one step.
--
-- The arguments of an equation are reduced to WHNF before they are matched
-- against its patterns (see [Note: Eager type families]). This does not reduce
-- with an equation whose right-hand side mentions a pattern variable that its
-- left-hand side doesn't bind: that would give a type with a free type
-- variable.
reduceTypeFamily :: Distinct n => TypeEnv -> Scope n -> Type n -> Maybe (Type n)
reduceTypeFamily tenv sc (tyView -> TyConApp tc tys) = reduceTyConApp tenv sc tc tys
reduceTypeFamily _ _ _ = Nothing

-- | 'reduceTypeFamily' of an applied type constructor
reduceTyConApp
  :: Distinct n => TypeEnv -> Scope n -> TyConName -> [Type n] -> Maybe (Type n)
reduceTyConApp tenv sc tc tys
  | nameUniq tc == fromGhcUnique typeNatAddTyFamNameKey
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2] -> Just (LitTy (NumTy (i1 + i2)))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeNatMulTyFamNameKey
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2] -> Just (LitTy (NumTy (i1 * i2)))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeNatExpTyFamNameKey
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2] -> Just (LitTy (NumTy (i1 ^ i2)))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeNatSubTyFamNameKey
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2]
        | let z = i1 - i2
        , z >= 0
        -> Just (LitTy (NumTy z))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeNatCmpTyFamNameKey -- "GHC.TypeNats.CmpNat"
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2] -> Just (orderingTy (compare i1 i2))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeSymbolCmpTyFamNameKey -- "GHC.TypeNats.CmpSymbol"
  = case mapMaybe (symLitView tenv sc) tys of
      [s1, s2] -> Just (orderingTy (compare s1 s2))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeCharCmpTyFamNameKey -- "GHC.TypeNats.CmpChar"
  = case mapMaybe (charLitView tenv sc) tys of
      [s1, s2] -> Just (orderingTy (compare s1 s2))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeConsSymbolTyFamNameKey -- ConsSymbol
  , [c0, s0] <- tys
  , Just c1 <- charLitView tenv sc c0
  , Just s1 <- symLitView tenv sc s0
  = Just (LitTy (SymTy (c1:s1)))

  | nameUniq tc == fromGhcUnique typeUnconsSymbolTyFamNameKey -- UnconsSymbol
  , [s1] <- mapMaybe (symLitView tenv sc) tys
  = fromMaybe (error "reduceTypeFamily: cannot construct UnconsSymbol result") $ do
      FunTyCon {} <- lookupTyCon tenv tc
      TyConApp maybeTcNm [tupTcApp] <-
        pure (tyView (snd (splitFunTys tenv sc (tyConKindOf tenv tc))))
      maybeTc <- lookupTyCon tenv maybeTcNm
      [nothingTc, justTc] <- pure (map (coerce . dcName) (tyConDataCons maybeTc))
      TyConApp tupTcNm [charTy, symbolTy] <- pure (tyView tupTcApp)
      tupTc <- lookupTyCon tenv tupTcNm
      [tupDc] <- pure (map (coerce . dcName) (tyConDataCons tupTc))
      case s1 of
        [] ->
          pure (Just (mkTyConApp nothingTc [tupTcApp]))
        (c:cs) ->
          let tup = mkTyConApp tupDc
                      [charTy, symbolTy, LitTy (CharTy c), LitTy (SymTy cs)]
           in pure (Just (mkTyConApp justTc [tupTcApp, tup]))

  | nameUniq tc == fromGhcUnique typeCharToNatTyFamNameKey -- CharToNat
  , [c1] <- mapMaybe (charLitView tenv sc) tys
  = Just (LitTy (NumTy (fromIntegral (ord c1))))

  | nameUniq tc == fromGhcUnique typeNatToCharTyFamNameKey -- NatToChar
  , [n1] <- mapMaybe (litView tenv sc) tys
  = Just (LitTy (CharTy (chr (fromInteger n1))))

  | nameUniq tc == fromGhcUnique typeSymbolAppendFamNameKey -- "GHC.TypeLits.AppendSymbol"
  = case mapMaybe (symLitView tenv sc) tys of
      [s1, s2] -> Just (LitTy (SymTy (s1 ++ s2)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.FLog", "GHC.TypeNats.FLog"]
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2]
        | i1 > 1
        , i2 > 0
        -> Just (LitTy (NumTy (smallInteger (integerLogBase# i1 i2))))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.CLog", "GHC.TypeNats.CLog"]
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2]
        | Just k <- clogBase i1 i2
        -> Just (LitTy (NumTy (toInteger k)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.Log", "GHC.TypeNats.Log"]
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2]
        | i1 > 1
        , i2 > 0
        -> if i2 == 1
           then Just (LitTy (NumTy 0))
           else let z1 = integerLogBase# i1 i2
                    z2 = integerLogBase# i1 (i2-1)
                in  if isTrue# (z1 ==# z2)
                        then Nothing
                        else Just (LitTy (NumTy (smallInteger z1)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.GCD", "GHC.TypeNats.GCD"]
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2] -> Just (LitTy (NumTy (i1 `gcd` i2)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.LCM", "GHC.TypeNats.LCM"]
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2] -> Just (LitTy (NumTy (i1 `lcm` i2)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.Div"]
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2]
        | i2 > 0
        -> Just (LitTy (NumTy (i1 `div` i2)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.Mod"]
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2]
        | i2 > 0
        -> Just (LitTy (NumTy (i1 `mod` i2)))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeNatDivTyFamNameKey
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2]
        | i2 > 0
        -> Just (LitTy (NumTy (i1 `div` i2)))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeNatModTyFamNameKey
  = case mapMaybe (litView tenv sc) tys of
      [i1, i2]
        | i2 > 0
        -> Just (LitTy (NumTy (i1 `mod` i2)))
      _ -> Nothing

  | Just eqns <- tyFamEqnsOf tenv tc
  = let -- See [Note: Eager type families]
        tysR = map (argView tenv sc) tys
     in findFunSubst tenv sc eqns tysR

  | otherwise
  = Nothing

-- | A promoted constructor of 'Ordering'
orderingTy :: Ordering -> Type n
orderingTy o = ConstTy $ TyCon $ case o of
  LT -> Name User (showt 'LT) (fromGhcUnique ordLTDataConKey) wiredInSrcSpan
  EQ -> Name User (showt 'EQ) (fromGhcUnique ordEQDataConKey) wiredInSrcSpan
  GT -> Name User (showt 'GT) (fromGhcUnique ordGTDataConKey) wiredInSrcSpan

-- | The right-hand side of the first equation whose patterns match the given
-- types
findFunSubst :: Distinct n => TypeEnv -> Scope n -> [TyFamEqn] -> [Type n] -> Maybe (Type n)
findFunSubst _ _ [] _ = Nothing
findFunSubst tenv sc (eqn:rest) args = case funSubsts tenv sc eqn args of
  Just ty -> Just ty
  Nothing -> findFunSubst tenv sc rest args

-- | If the patterns of an equation match the given types, its right-hand side,
-- with the matched types substituted for its pattern variables, applied to the
-- types the patterns don't consume
funSubsts :: Distinct n => TypeEnv -> Scope n -> TyFamEqn -> [Type n] -> Maybe (Type n)
funSubsts tenv sc (TyFamEqn tvs lhs rhs) args = do
  let (funArgs, remainder) = zipAtLeast lhs args
  matched <- foldM (\m (p, t) -> funSubst tenv sc m p t) IntMap.empty funArgs
  s <- matchedSubst sc tvs matched
  -- Type functions can return higher-kinded types, so don't forget to apply
  -- the arguments not consumed by the type function application. Forgetting
  -- leads to: #232
  pure (List.foldl' AppTy (substTy s rhs) remainder)
 where
  zipAtLeast [] ys = ([], ys)
  zipAtLeast _ [] = error "Under-applied type family"
  zipAtLeast (x:xs) (y:ys) =
    let (zs, remainder) = zipAtLeast xs ys
     in ((x, y):zs, remainder)

-- | Substitute the matched types for the pattern variables. 'Nothing' if a
-- pattern variable wasn't matched.
matchedSubst
  :: forall n p. Scope n -> Tele TyBinder 'VoidS p -> IntMap (Type n) -> Maybe (Subst p n)
matchedSubst sc tvs matched = go (closedSubst sc) tvs
 where
  go :: Subst i n -> Tele TyBinder i p -> Maybe (Subst p n)
  go s TNil = Just s
  go s (TCons b bs) = case IntMap.lookup (tyNameRaw (tyNameOf b)) matched of
    Just ty -> go (extendTySubst s b ty) bs
    Nothing -> Nothing

-- | Match a pattern against a type, extending the matched types of the pattern
-- variables, by their raw names.
--
-- A pattern variable that occurs more than once must match alpha-equivalent
-- types. E.g. given
--
-- > type family Max x y where
-- >   Max 0 b = b
-- >   Max a 0 = a
-- >   Max n n = n
-- >   Max a b = If (a <=? b) b a
--
-- @Max 4 8@ matches the fourth equation.
--
-- The variables of the pattern are its pattern variables: a pattern under a
-- binder never matches.
funSubst
  :: Distinct n
  => TypeEnv
  -> Scope n
  -> IntMap (Type n)
  -> Type p
  -> Type n
  -> Maybe (IntMap (Type n))
funSubst tenv sc = go
 where
  go s pat ty = case (pat, ty) of
    -- AnnType cannot be matched in type families within regular GHC (as they
    -- are type synonyms) so it is fine to skip over them here.
    (AnnType _ p, _) -> go s p ty

    (VarTy _ _ nm, _) -> case IntMap.lookup k s of
      Nothing -> Just (IntMap.insert k ty s)
      Just ty' | aeqTy ty' ty -> Just s
      _ -> Nothing
     where
      k = tyNameRaw nm

    -- Only look through annotations in the type after the VarTy case, so the
    -- matched types keep their annotations
    (_, AnnType _ t) -> go s pat t

    -- [Note] funSubst FunTy
    --
    -- Whenever type classes have associated types whose instances 'map' to
    -- functions, we try to find substitutions in the LHS and RHS of these
    -- (type-level) functions. Because we use @funSubst@ recursively, we
    -- implicitly check if these functions are of the same arity and match
    -- in the first place. An example of such a construct:
    --
    --   class Example p where
    --     type AB p
    --
    --   instance Example (a -> a) where
    --     type AB (a -> a) = a
    --
    -- In the given example, we would find two substitutions. For example, when
    -- matching against `Char -> Char` we'd find a duplicate `a -> Char`. We
    -- can't think of any (type-checking) cases where these mappings would map
    -- to different types, so this is OK for our purposes.
    (AppTy p1 r1, AppTy t1 r2) -> do
      s1 <- go s p1 t1
      -- See [Note: Eager type families]
      go s1 r1 (argView tenv sc r2)

    (ConstTy c1, ConstTy c2) | c1 == c2 -> Just s
    (LitTy l1, LitTy l2) | l1 == l2 -> Just s
    _ -> Nothing

-- | Is the type an application of a type family?
isTypeFamilyApplication :: TypeEnv -> Type n -> Bool
isTypeFamilyApplication tenv (tyView -> TyConApp tcNm _args)
  | Just (FunTyCon {}) <- lookupTyCon tenv tcNm = True
isTypeFamilyApplication _ _ = False

-- | Reduce type families at the head of a type, until it no longer reduces
argView :: Distinct n => TypeEnv -> Scope n -> Type n -> Type n
argView tenv sc t = case reduceTypeFamily tenv sc t of
  Nothing -> t
  Just tR -> argView tenv sc tR

-- * Literal types

-- | The number of a type that reduces to a type-level number
litView :: Distinct n => TypeEnv -> Scope n -> Type n -> Maybe Integer
litView _ _ (LitTy (NumTy i)) = Just i
litView tenv sc (reduceTypeFamily tenv sc -> Just ty') = litView tenv sc ty'
litView _ _ _ = Nothing

-- | The string of a type that reduces to a type-level symbol
symLitView :: Distinct n => TypeEnv -> Scope n -> Type n -> Maybe String
symLitView _ _ (LitTy (SymTy s)) = Just s
symLitView tenv sc (reduceTypeFamily tenv sc -> Just ty') = symLitView tenv sc ty'
symLitView _ _ _ = Nothing

-- | The character of a type that reduces to a type-level character
charLitView :: Distinct n => TypeEnv -> Scope n -> Type n -> Maybe Char
charLitView _ _ (LitTy (CharTy c)) = Just c
charLitView tenv sc (reduceTypeFamily tenv sc -> Just ty') = charLitView tenv sc ty'
charLitView _ _ _ = Nothing

-- | The number a type reduces to under 'coreView1'
tyNatSize :: Distinct n => TypeEnv -> Scope n -> Type n -> Except String Integer
tyNatSize tenv sc (coreView1 tenv sc -> Just ty) = tyNatSize tenv sc ty
tyNatSize _ _ (LitTy (NumTy i)) = return i
tyNatSize _ _ ty = throwE $ $(curLoc) ++ "Cannot reduce to an integer:\n" ++ showPpr ty

-- | The string representation of the symbol or number a type reduces to under
-- 'coreView1'
tyLitShow :: Distinct n => TypeEnv -> Scope n -> Type n -> Except String String
tyLitShow tenv sc (coreView1 tenv sc -> Just ty) = tyLitShow tenv sc ty
tyLitShow _ _ (LitTy (SymTy s)) = return s
tyLitShow _ _ (LitTy (NumTy s)) = return (show s)
tyLitShow _ _ ty = throwE $ $(curLoc) ++ "Cannot reduce to a string:\n" ++ showPpr ty

-- * Predicates

-- | Is the type a class dictionary?
isClassTy :: TypeEnv -> Type n -> Bool
isClassTy tenv (tyView -> TyConApp tcNm _) =
  case lookupTyCon tenv tcNm of
    Just (AlgTyCon {isClassTc}) -> isClassTc
    _ -> False
isClassTy _ _ = False

-- | Is the type @Integer@?
isIntegerTy :: Type n -> Bool
isIntegerTy (ConstTy (TyCon nm)) = nameUniq nm == fromGhcUnique integerTyConKey
isIntegerTy _ = False

-- | Determine whether a type is isomorphic to "Clash.Signal.Internal.Signal",
-- or contains one.
isSignalType :: Distinct n => TypeEnv -> Scope n -> Type n -> Bool
isSignalType tenv sc = go HashSet.empty
 where
  go tcSeen (tyView -> TyConApp tcNm args) = case nameOcc tcNm of
    "Clash.Signal.Internal.Signal" -> True
    "Clash.Signal.BiSignal.BiSignalIn" -> True
    "Clash.Signal.BiSignal.BiSignalOut" -> True
    _ | tcNm `HashSet.member` tcSeen -> False -- Do not follow rec types
      | otherwise -> case lookupTyCon tenv tcNm of
          Just tc ->
            let dcs = tyConDataCons tc
                dcInsArgTys = concat (mapMaybe (\dc -> dataConInstArgTys tenv sc dc args) dcs)
                tcSeen' = HashSet.insert tcNm tcSeen
            in any (go tcSeen') dcInsArgTys
          Nothing -> traceIf True ($(curLoc) ++ "isSignalType: " ++ show tcNm
                                   ++ " not found.") False
  go _ _ = False

-- | Determines whether a type is (an alias of) an Enable line.
isEnable :: Distinct n => TypeEnv -> Scope n -> Type n -> Bool
isEnable tenv sc ty0
  | TyConApp (nameOcc -> "Clash.Signal.Internal.Enable") _ <- tyView ty0 = True
  | Just ty1 <- coreView1 tenv sc ty0 = isEnable tenv sc ty1
  | otherwise = False

-- | Determines whether a type is (an alias of) a Clock or Reset line.
isClockOrReset :: Distinct n => TypeEnv -> Scope n -> Type n -> Bool
isClockOrReset tenv sc (coreView1 tenv sc -> Just ty) = isClockOrReset tenv sc ty
isClockOrReset _ _ (tyView -> TyConApp tcNm _) = case nameOcc tcNm of
  "Clash.Signal.Internal.Clock" -> True
  "Clash.Signal.Internal.ClockN" -> True
  "Clash.Signal.Internal.Reset" -> True
  _ -> False
isClockOrReset _ _ _ = False

-- * Annotations

-- | The attributes of an annotated type. Empty if the type is not annotated.
typeAttrs :: Type n -> [Attr Text]
typeAttrs (AnnType attrs _) = attrs
typeAttrs _ = []

-- | Remove all 'AnnType' wrappers from a type.
stripAnnTypes :: Type n -> Type n
stripAnnTypes (AnnType _ ty) = stripAnnTypes ty
stripAnnTypes ty = ty

-- | Strip implicit parameter wrappers (IP).
stripIP :: Type n -> Type n
stripIP t@(tyView -> TyConApp tcNm [_a1, a2]) =
  if nameUniq tcNm == fromGhcUnique ipClassKey then a2 else t
stripIP t = t
