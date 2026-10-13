{-|
  Copyright   :  (C) 2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The types of Clash Core terms, and the kinds of their types.

  Local variables carry their type ('Var', and 'idBinderType'), and so do
  case-expressions and casts, and global variables ('globalIdType'). Data
  constructors and primitives have closed types ('Clash.Core.DataCon.dcType',
  'primType'), which 'sinkClosed' and 'primInfoType' move into any scope.

  = Inferring types under binders

  'inferCoreTypeOf' goes under the binders of a term, e.g. into the body of a
  lambda, to infer its type. It doesn't rename the term to do so. Binders that
  don't clash with the scope are used as they are (like 'openIdBinder' does).
  Below a binder that does clash, it only renames the binders, and
  substitutes the types it gives back. So inferring the type of a term whose
  binders clash with the scope, e.g. one that was sunk into it, doesn't copy
  the term.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.HasType
  ( -- * Types of terms
    literalType
  , primInfoType
  , inferCoreTypeOf
  , applyTypeToArgs
  , piResultTy
  , piResultTys
    -- * Kinds of types
  , inferCoreKindOf
  ) where

import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)

import Clash.Core.Basic (IsMultiPrim (..), LitTy (..))
import Clash.Core.DataCon (DataCon (dcType))
import Clash.Core.Literal (Literal (..))
import Clash.Core.Pretty (showPpr)
import Clash.Core.TyCon (isTupleTyConLike)
import Clash.Debug (debugIsOn)
import Clash.Util (curLoc)
import qualified Clash.Util.Interpolate as I

import Clash.Core.Alpha (aeqTy)
import Clash.Core.NameMap (tyBinderNameBinders)
import Clash.Core.Subst
import Clash.Core.Syntax
import Clash.Core.Term (collectArgs)
import Clash.Core.Type
import Clash.Core.TypeEnv (TypeEnv, tyConKindOf)
import Clash.Core.TysPrim

-- * Types of terms

-- | The type of a literal, in any scope
literalType :: Literal -> Type n
literalType = \case
  IntegerLiteral _ -> integerPrimTy
  IntLiteral _ -> intPrimTy
  WordLiteral _ -> wordPrimTy
  StringLiteral _ -> stringPrimTy
  FloatLiteral _ -> floatPrimTy
  DoubleLiteral _ -> doublePrimTy
  CharLiteral _ -> charPrimTy
  Int64Literal _ -> int64PrimTy
  Word64Literal _ -> word64PrimTy
  Int8Literal _ -> int8PrimTy
  Int16Literal _ -> int16PrimTy
  Int32Literal _ -> int32PrimTy
  Word8Literal _ -> word8PrimTy
  Word16Literal _ -> word16PrimTy
  Word32Literal _ -> word32PrimTy
  NaturalLiteral _ -> naturalPrimTy
  ByteArrayLiteral _ -> byteArrayPrimTy

-- | The type of a primitive, in any scope. The type of a multi-result
-- primitive returns its results as separate arguments, see Note [MultiResult
-- type] in "Clash.Normalize.Transformations.MultiPrim".
primInfoType :: (HasCallStack, Distinct n) => PrimInfo -> Type n
primInfoType p = case primMultiResult p of
  SingleResult -> sinkClosed (primType p)
  MultiResult -> sinkClosed (multiResultType (primType p))
{-# INLINE primInfoType #-}

-- | The type of a multi-result primitive, from the type of the primitive that
-- returns a tuple: its results become arguments, see 'primInfoType'
multiResultType :: HasCallStack => Type n -> Type n
multiResultType ty = case splitFunForallTy ty of
  SplitFunForall tele res
    | TyConApp tupTcNm tupArgs <- tyView res
    , isTupleTyConLike tupTcNm
    -> mkPolyFunTy (appendArgs tele tupArgs) res
  _ -> error "primInfoType: MultiResult primitive without tuple type"
 where
  appendArgs :: PiTele x l -> [Type l] -> PiTele x l
  appendArgs tele args = case tele of
    PiNil -> foldr PiArg PiNil args
    PiForall b rest -> PiForall b (appendArgs rest args)
    PiArg a rest -> PiArg a (appendArgs rest args)

-- | The type of a term. See the module header.
inferCoreTypeOf :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Term n -> Type n
inferCoreTypeOf = inferTypeOpen

-- | Goes under binders that don't clash with the scope as they are. At a
-- binder that does, continues with 'inferTypeSubst'.
inferTypeOpen :: Distinct n => TypeEnv -> Scope n -> Term n -> Type n
inferTypeOpen tenv sc tm = case tm of
  Lam b e -> openIdBinder sc b (Opened False e) $ \sc' b' (Opened renamed e') ->
    if renamed then inferTypeSubst tenv (idSubst sc) tm else
    mkFunTy (idBinderType b') (strengthenTyId b' (inferTypeOpen tenv sc' e'))
  TyLam b e -> openTyBinder sc b (Opened False e) $ \sc' b' (Opened renamed e') ->
    if renamed then inferTypeSubst tenv (idSubst sc) tm else
    ForAllTy b' (inferTypeOpen tenv sc' e')
  App {} -> inferTypeOpenApp tenv sc tm
  TyApp {} -> inferTypeOpenApp tenv sc tm
  Let bs e -> openBind sc bs (Opened False e) $ \sc' bs' (Opened renamed e') ->
    if renamed then inferTypeSubst tenv (idSubst sc) tm else
    case bs' of
      NonRec b _ -> strengthenTyId b (inferTypeOpen tenv sc' e')
      Rec bndrs _ -> strengthenTyIds bndrs (inferTypeOpen tenv sc' e')
  Tick _ e -> inferTypeOpen tenv sc e
  _ -> inferTypeSubst tenv (idSubst sc) tm

inferTypeOpenApp :: Distinct n => TypeEnv -> Scope n -> Term n -> Type n
inferTypeOpenApp tenv sc tm = case collectArgs tm of
  (fun, args) -> applyTypeToArgsWith tm tenv sc (inferTypeOpen tenv sc fun) args

-- | A term under a binder, and whether going under the binder renamed it. See
-- 'inferCoreTypeOf'. The term is lazy, so that it isn't renamed unless it is
-- used.
data Opened (n :: S) = Opened Bool (Term n)

instance HasSubst Opened where
  subst s (Opened _ e) = Opened True (subst s e)

-- | The type of a term in scope @i@, substituted into scope @o@. Goes under
-- binders by renaming only the binders, see the module header.
inferTypeSubst :: (HasCallStack, Distinct o) => TypeEnv -> Subst i o -> Term i -> Type o
inferTypeSubst tenv = go
 where
  go :: Distinct y => Subst x y -> Term x -> Type y
  go s = \case
    Var _ ty _ -> substTy s ty
    GlobalVar g -> sinkClosed (globalIdType g)
    Data dc -> sinkClosed (dcType dc)
    Literal l -> literalType l
    Prim p -> primInfoType p
    Lam b e -> substIdBinder s b $ \s' b' ->
      mkFunTy (idBinderType b') (strengthenTyId b' (go s' e))
    TyLam b e -> substTyBinder s b $ \s' b' ->
      ForAllTy b' (go s' e)
    e@App {} -> goApp s e
    e@TyApp {} -> goApp s e
    Let (NonRec b _) e -> substIdBinder s b $ \s' b' ->
      strengthenTyId b' (go s' e)
    Let (Rec bs _) e -> substTele substIdBinder s bs $ \s' bs' ->
      strengthenTyIds bs' (go s' e)
    Case _ ty _ -> substTy s ty
    Cast _ _ ty -> substTy s ty
    Tick _ e -> go s e

  goApp :: Distinct y => Subst x y -> Term x -> Type y
  goApp s e = case collectArgs e of
    (fun, args) ->
      applyTypeToArgsWith e tenv (substScope s) (go s fun) (map (fmap (substTy s)) args)

-- | The result type of a function, given its type and its arguments
applyTypeToArgs
  :: (HasCallStack, Distinct n)
  => Term n
  -- ^ The complete term, used for error messages
  -> TypeEnv
  -> Scope n
  -> Type n
  -> [Either (Term n) (Type n)]
  -> Type n
applyTypeToArgs = applyTypeToArgsWith

-- | 'applyTypeToArgs', where the terms are only used for error messages, and
-- may be in another scope
applyTypeToArgsWith
  :: (HasCallStack, Distinct n)
  => Term x
  -> TypeEnv
  -> Scope n
  -> Type n
  -> [Either (Term x) (Type n)]
  -> Type n
applyTypeToArgsWith e tenv sc = go
 where
  go opTy [] = opTy
  go opTy (Right ty:args) = goTyArgs opTy [ty] args
  go opTy (Left a:args) = case splitFunTy tenv sc opTy of
    Just (_, resTy) -> go resTy args
    _ -> error [I.i|
        Unexpected application. The term

          #{showPpr e}

        applied an argument

          #{showPpr a}

        to something with the non-function type

          #{showPpr opTy}
      |]

  goTyArgs opTy revTys (Right ty:args) = goTyArgs opTy (ty:revTys) args
  goTyArgs opTy revTys args = go (piResultTys tenv sc opTy (reverse revTys)) args

-- | Like 'piResultTys', but only applies a single type
piResultTy :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Type n -> Type n -> Type n
piResultTy tenv sc ty arg = piResultTys tenv sc ty [arg]

-- | @(piResultTys f_ty [ty1, ..., tyn])@ gives the type of @(f ty1 .. tyn)@
-- where @f :: f_ty@.
--
-- The function type may have more or fewer foralls than there are arguments:
-- e.g. @piResultTys (forall a. a) [forall b. b -> b, Int]@ is @Int -> Int@.
--
-- When there are no foralls, this simply drops arrows from a function type,
-- given the types of the arguments applied to it. With the @debug@ flag, these
-- must match the argument types of the function type.
piResultTys
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> Type n
  -> [Type n]
  -> Type n
piResultTys = piResultTysWith True

-- | Like 'piResultTys', but applies a kind to types. The arrows of the kind are
-- dropped without checking the kinds of the types against them.
piResultKinds
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> Kind n
  -> [Type n]
  -> Kind n
piResultKinds = piResultTysWith False

-- | See 'piResultTys' and 'piResultKinds'
piResultTysWith
  :: forall n
   . (HasCallStack, Distinct n)
  => Bool
  -- ^ Check (with the @debug@ flag) that the types dropping an arrow match its
  -- argument type
  -> TypeEnv
  -> Scope n
  -> Type n
  -> [Type n]
  -> Type n
piResultTysWith _ _ _ ty [] = ty
piResultTysWith checkArgs tenv sc ty origArgs@(arg:args)
  | Just ty' <- coreView1 tenv sc ty
  = piResultTysWith checkArgs tenv sc ty' origArgs
  | FunTy a res <- tyView ty
  -- TODO coreView is used here because the partial evaluator will sometimes
  -- encounter / not encounter a Signal as an argument unexpectedly. When PR
  -- #1064 is merged the coreView calls should be removed again.
  = if debugIsOn && checkArgs && not (aeqTy (coreView tenv sc a) (coreView tenv sc arg)) then error [I.i|
      Unexpected application. A function with type:

        #{showPpr ty}

      Got applied to an argument of type:

        #{showPpr arg}
    |]
    else
      piResultTysWith checkArgs tenv sc res args
  | ForAllTy b res <- ty
  = go (extendTySubst (idSubst sc) b arg) res args
  | otherwise
  = error ($(curLoc) ++ "piResultTys1: " ++ showPpr ty ++ "\n" ++ showPpr origArgs)
 where
  -- Substitutes for the foralls in one go
  go :: Subst i n -> Type i -> [Type n] -> Type n
  go s ty' [] = substTy s ty'
  go s ty' allArgs@(arg':args')
    | ForAllTy b res <- ty'
    = go (extendTySubst s b arg') res args'
    | FunTy _ res <- tyView ty'
    = go s res args'
    -- Look through the type, or deal with (piResultTys (forall a.a) [forall
    -- b.b, Int]), after substituting: the type is not in the scope of the
    -- arguments
    | otherwise
    = let ty'' = substTy s ty' in
      case coreView1 tenv sc ty'' of
        Just ty3 -> go (idSubst sc) ty3 allArgs
        Nothing -> piResultTysWith checkArgs tenv sc ty'' allArgs

-- * Kinds of types

-- | The kind of a type
inferCoreKindOf :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Type n -> Kind n
inferCoreKindOf tenv sc ty = case tyView ty of
  FunTy {} ->
    liftedTypeKind

  TyConApp tc args ->
    piResultKinds tenv sc (tyConKindOf tenv tc) args

  OtherType {} ->
    case ty of
      ConstTy c -> error $ $(curLoc) ++ "inferCoreKindOf: naked ConstTy: " ++ show c
      VarTy _ k _ -> k
      ForAllTy b a -> openTyBinder sc b a $ \sc' b' a' ->
        fromMaybe
          (error $ $(curLoc) ++ "inferCoreKindOf: kind mentions the type variable: "
                     ++ showPpr ty)
          (unsinkType (tyBinderNameBinders b') (inferCoreKindOf tenv sc' a'))
      LitTy NumTy {} -> typeNatKind
      LitTy SymTy {} -> typeSymbolKind
      LitTy CharTy {} -> charPrimTy
      AnnType _ a -> inferCoreKindOf tenv sc a
      AppTy a b -> goApp a [b]
 where
  goApp (AppTy c d) args = goApp c (d : args)
  goApp c args = piResultKinds tenv sc (inferCoreKindOf tenv sc c) args
