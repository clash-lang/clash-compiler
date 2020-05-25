{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Core.TermInfo where

import Data.Text.Prettyprint.Doc (line)

import Clash.Core.DataCon
import Clash.Core.FreeVars
import Clash.Core.Literal
import Clash.Core.Pretty
import Clash.Core.Subst
import Clash.Core.Term
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Debug (debugIsOn)
import Clash.Util
import Clash.Util.Interpolate as I

termSize :: Term -> Word
termSize (Var {})     = 1
termSize (Data {})    = 1
termSize (Literal {}) = 1
termSize (Prim {})    = 1
termSize (Lam _ e)    = termSize e + 1
termSize (TyLam _ e)  = termSize e
termSize (App e1 e2)  = termSize e1 + termSize e2
termSize (TyApp e _)  = termSize e
termSize (Cast e _ _) = termSize e
termSize (Tick _ e)   = termSize e
termSize (Letrec bndrs e) = sum (bodySz:bndrSzs)
 where
  bndrSzs = map (termSize . snd) bndrs
  bodySz  = termSize e
termSize (Case subj _ alts) = sum (subjSz:altSzs)
 where
  subjSz = termSize subj
  altSzs = map (termSize . snd) alts

-- | Determine the type of a term
termType :: TyConMap -> Term -> Type
termType m e = case e of
  Var t          -> varType t
  Data dc        -> dcType dc
  Literal l      -> literalType l
  Prim t         -> primType t
  Lam v e'       -> mkFunTy (varType v) (termType m e')
  TyLam tv e'    -> ForAllTy tv (termType m e')
  App _ _        -> case collectArgs e of
                      (fun, args) -> applyTypeToArgs e m (termType m fun) args
  TyApp _ _      -> case collectArgs e of
                      (fun, args) -> applyTypeToArgs e m (termType m fun) args
  Letrec _ e'    -> termType m e'
  Case _ ty _    -> ty
  Cast _ _ ty2   -> ty2
  Tick _ e'      -> termType m e'

-- | Get the result type of a polymorphic function given a list of arguments
applyTypeToArgs
  :: Term
  -> TyConMap
  -> Type
  -> [Either Term Type]
  -> Type
applyTypeToArgs e m opTy args = go opTy args
 where
  go opTy' []               = opTy'
  go opTy' (Right ty:args') = goTyArgs opTy' [ty] args'
  go opTy' (Left _:args')   = case splitFunTy m opTy' of
    Just (_,resTy) -> go resTy args'
    _ -> error $ unlines ["applyTypeToArgs:"
                         ,"Expression: " ++ showPpr e
                         ,"Type: " ++ showPpr opTy
                         ,"Args: " ++ unlines (map (either showPpr showPpr) args)
                         ]

  goTyArgs opTy' revTys (Right ty:args') = goTyArgs opTy' (ty:revTys) args'
  goTyArgs opTy' revTys args'            = go (piResultTys m opTy' (reverse revTys)) args'

-- | Like 'piResultTyMaybe', but errors out when a type application is not
-- valid.
--
-- Do not iterate 'piResultTy', because it's inefficient to substitute one
-- variable at a time; instead use 'piResultTys'
piResultTy
  :: HasCallStack
  => TyConMap
  -> Type
  -> Type
  -> Type
piResultTy m ty arg = case piResultTyMaybe m ty arg of
  Just res -> res
  Nothing  -> pprPanic "piResultTy" (ppr ty <> line <> ppr arg)

-- | Like 'piResultTys' but for a single argument.
--
-- Do not iterate 'piResultTyMaybe', because it's inefficient to substitute one
-- variable at a time; instead use 'piResultTys'
piResultTyMaybe
  :: HasCallStack
  => TyConMap
  -> Type
  -> Type
  -> Maybe Type
piResultTyMaybe m ty arg
  | Just ty' <- coreView1 m ty
  = piResultTyMaybe m ty' arg
  | FunTy a res <- tyView ty
  = if debugIsOn && not (aeqType a arg) then error [I.i|
      Unexpected application. A function with type:

        #{showPpr ty}

      Got applied to an argument of type:

        #{showPpr arg}
    |]
    else
      Just res
  | ForAllTy tv res <- ty
  = let emptySubst = mkSubst (mkInScopeSet (tyFVsOfTypes [arg,res]))
    in  Just (substTy (extendTvSubst emptySubst tv arg) res)
  | otherwise
  = Nothing

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
  = if debugIsOn && not (aeqType a arg) then error [I.i|
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
  inScope = mkInScopeSet (tyFVsOfTypes (ty:origArgs))

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

-- | Does a term have a function type?
isFun :: TyConMap -> Term -> Bool
isFun m t = isFunTy m (termType m t)

-- | Does a term have a function or polymorphic type?
isPolyFun :: TyConMap -> Term -> Bool
isPolyFun m t = isPolyFunCoreTy m (termType m t)

-- | Is a term a term-abstraction?
isLam :: Term -> Bool
isLam (Lam {}) = True
isLam _        = False

-- | Is a term a recursive let-binding?
isLet :: Term -> Bool
isLet (Letrec {}) = True
isLet _           = False

-- | Is a term a variable reference?
isVar :: Term -> Bool
isVar (Var {}) = True
isVar _        = False

isLocalVar :: Term -> Bool
isLocalVar (Var v) = isLocalId v
isLocalVar _ = False

-- | Is a term a datatype constructor?
isCon :: Term -> Bool
isCon (Data {}) = True
isCon _         = False

-- | Is a term a primitive?
isPrim :: Term -> Bool
isPrim (Prim {}) = True
isPrim _         = False

