{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Utility class to extract type information from data which has a type.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.HasType
  ( HasType(..)
  , coreKindOf
  , InferType(..)
  , inferCoreKindOf
  , applyTypeToArgs
  , piResultTy
  , piResultTys
  ) where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (line)
#else
import Data.Text.Prettyprint.Doc (line)
#endif

import GHC.Stack (HasCallStack)

import Clash.Core.DataCon (DataCon(dcType))
import Clash.Core.HasFreeVars
import Clash.Core.Literal (Literal(..))
import Clash.Core.Pretty
import Clash.Core.Subst
import Clash.Core.Term (Term(..), IsMultiPrim(..), PrimInfo(..), collectArgs)
import Clash.Core.TyCon (TyCon(tyConKind), TyConMap, isTupleTyConLike)
import Clash.Core.Type
import Clash.Core.TysPrim
import Clash.Core.Var (Var(varType))
import Clash.Core.VarEnv
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Debug (debugIsOn)
import Clash.Util (curLoc, pprPanic)
import qualified Clash.Util.Interpolate as I

class HasType a where
  coreTypeOf :: a -> Type

coreKindOf :: (HasType a) => a -> Kind
coreKindOf = coreTypeOf
{-# INLINE coreKindOf #-}

instance HasType DataCon where
  coreTypeOf = dcType

instance HasType Literal where
  coreTypeOf = \case
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

instance HasType PrimInfo where
  coreTypeOf pr =
    case primMultiResult pr of
      SingleResult -> primType pr

      -- See Note [MultiResult type] in Clash.Normalize.Transformations.MultiPrim
      MultiResult
        | let (primArgs, primResTy) = splitFunForallTy (primType pr)
        , TyConApp tupTcNm tupArgs <- tyView primResTy
        , isTupleTyConLike tupTcNm
        -> mkPolyFunTy primResTy (primArgs <> fmap Right tupArgs)

        | otherwise
        -> error "PrimInfo.coreTypeOf: MultiResult primitive without tuple type"

instance HasType TyCon where
  coreTypeOf = tyConKind

instance HasType Type where
  coreTypeOf = id

instance HasType (Var a) where
  coreTypeOf = varType

class InferType a where
  inferCoreTypeOf :: TyConMap -> a -> Type

inferCoreKindOf :: (InferType a) => TyConMap -> a -> Kind
inferCoreKindOf = inferCoreTypeOf
{-# INLINE inferCoreKindOf #-}

instance InferType Type where
  inferCoreTypeOf tcm ty =
    case tyView ty of
      FunTy{} ->
        liftedTypeKind

      TyConApp tc args ->
        piResultTys tcm (tyConKind (UniqMap.find tc tcm)) args

      OtherType{} ->
        case ty of
          ConstTy c -> error $ $(curLoc) ++ "inferCoreTypeOf: naked ConstTy: " ++ show c
          VarTy k -> varType k
          ForAllTy _ a -> inferCoreTypeOf tcm a
          LitTy NumTy{} -> typeNatKind
          LitTy SymTy{} -> typeSymbolKind
          LitTy CharTy{} -> charPrimTy
          AnnType _ a -> inferCoreTypeOf tcm a
          AppTy a b -> go a [b]
           where
            go (AppTy c d) args = go c (d : args)
            go c args = piResultTys tcm (inferCoreTypeOf tcm c) args

instance InferType Term where
  inferCoreTypeOf tcm = go
   where
    go = \case
      Var i -> coreTypeOf i
      Data dc -> coreTypeOf dc
      Literal l -> coreTypeOf l
      Prim pr -> coreTypeOf pr
      Lam i x -> mkFunTy (coreTypeOf i) (go x)
      TyLam i x -> ForAllTy i (go x)

      x@App{} ->
        case collectArgs x of
          (fun, args) -> applyTypeToArgs x tcm (go fun) args

      x@TyApp{} ->
        case collectArgs x of
          (fun, args) -> applyTypeToArgs x tcm (go fun) args

      Let _ x -> go x
      Case _ ty _ -> ty
      Cast _ _ a -> a
      Tick _ x -> go x

-- | Get the result type of a polymorphic function given a list of arguments
applyTypeToArgs
  :: Term
  -- ^ The complete term, used for error messages.
  -> TyConMap
  -> Type
  -> [Either Term Type]
  -> Type
applyTypeToArgs e m opTy args = go opTy args
 where
  go opTy' []               = opTy'
  go opTy' (Right ty:args') = goTyArgs opTy' [ty] args'
  go opTy' (Left a:args')   = case splitFunTy m opTy' of
    Just (_,resTy) -> go resTy args'
    _ -> error [I.i|
        Unexpected application. The term

          #{showPpr e}

        applied an argument

          #{showPpr a}

        to something with the non-function type

          #{showPpr opTy'}
      |]

  goTyArgs opTy' revTys (Right ty:args') = goTyArgs opTy' (ty:revTys) args'
  goTyArgs opTy' revTys args'            = go (piResultTys m opTy' (reverse revTys)) args'

-- | Like 'piResultTys', but only applies a single type. If multiple types are
-- being applied use 'piResultTys', as it is more efficient to only substitute
-- once with many types.
piResultTy
  :: HasCallStack
  => TyConMap
  -> Type
  -> Type
  -> Type
piResultTy m ty arg =
  piResultTys m ty [arg]

-- | @(piResultTys f_ty [ty1, ..., tyn])@ gives the type of @(f ty1 .. tyn)@
-- where @f :: f_ty@
--
-- 'piResultTys' is interesting because:
--
--    1. 'f_ty' may have more foralls than there are args
--    2. Less obviously, it may have fewer foralls
--
-- Fore case 2. think of:
--
--   piResultTys (forall a . a) [forall b.b, Int]
--
-- This really can happen, such as situations involving 'undefined's type:
--
--   undefined :: forall a. a
--
--   undefined (forall b. b -> b) Int
--
-- This term should have the type @(Int -> Int)@, but notice that there are
-- more type args than foralls in 'undefined's type.
--
-- For efficiency reasons, when there are no foralls, we simply drop arrows from
-- a function type/kind.
piResultTys
  :: HasCallStack
  => TyConMap
  -> Type
  -> [Type]
  -> Type
piResultTys _ ty [] = ty
piResultTys m ty origArgs@(arg:args)
  | Just ty' <- coreView1 m ty
  = piResultTys m ty' origArgs
  | FunTy a res <- tyView ty
  -- TODO coreView is used here because the partial evaluator will sometimes
  -- encounter / not encounter a Signal as an argument unexpectedly. When PR
  -- #1064 is merged the coreView calls should be removed again.
  = if debugIsOn && not (aeqType (coreView m a) (coreView m arg)) then error [I.i|
      Unexpected application. A function with type:

        #{showPpr ty}

      Got applied to an argument of type:

        #{showPpr arg}
    |]
    else
      piResultTys m res args
  | ForAllTy tv res <- ty
  = go (extendVarEnv tv arg emptyVarEnv) res args
  | otherwise
  = pprPanic "piResultTys1" (ppr ty <> line <> ppr origArgs)
 where
  inScope = mkInScopeSet (freeVarsOf (ty:origArgs))

  go env ty' [] = substTy (mkTvSubst inScope env) ty'
  go env ty' allArgs@(arg':args')
    | Just ty'' <- coreView1 m ty'
    = go env ty'' allArgs
    | FunTy _ res <- tyView ty'
    = go env res args'
    | ForAllTy tv res <- ty'
    = go (extendVarEnv tv arg' env) res args'
    | VarTy tv <- ty'
    , Just ty'' <- lookupVarEnv tv env
      -- Deals with (piResultTys  (forall a.a) [forall b.b, Int])
    = piResultTys m ty'' allArgs
    | otherwise
    = pprPanic "piResultTys2" (ppr ty' <> line <> ppr origArgs <> line <> ppr allArgs)
