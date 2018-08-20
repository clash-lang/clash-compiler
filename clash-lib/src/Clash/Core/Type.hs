{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2017     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Types in CoreHW
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

module Clash.Core.Type
  ( Type (..)
  , TypeView (..)
  , ConstTy (..)
  , LitTy (..)
  , Kind
  , KindOrType
  , KiName
  , KiOccName
  , TyName
  , TyOccName
  , TyVar
  , tyView
  , coreView
  , typeKind
  , mkTyConTy
  , mkFunTy
  , mkTyConApp
  , splitFunTy
  , splitFunTys
  , splitFunForallTy
  , splitCoreFunForallTy
  , splitTyConAppM
  , isPolyFunTy
  , isPolyFunCoreTy
  , isPolyTy
  , isFunTy
  , applyFunTy
  , applyTy
  , findFunSubst
  , reduceTypeFamily
  , undefinedTy
  , isIntegerTy
  , normalizeType
  , varAttrs
  , getFunResult
  , typeAttrs
  )
where

-- External import
import           Control.DeepSeq                         as DS
import           Data.Binary                             (Binary)
import           Data.Hashable                           (Hashable)
import           Data.HashMap.Strict                     (HashMap)
import qualified Data.HashMap.Strict                     as HashMap
import           Data.List                               (foldl',isPrefixOf)
import           Data.Maybe                              (isJust, mapMaybe)
import           GHC.Base                                (isTrue#,(==#),(<|>))
import           GHC.Generics                            (Generic(..))
import           GHC.Integer                             (smallInteger)
import           GHC.Integer.Logarithms                  (integerLogBase#)
import           Unbound.Generics.LocallyNameless        (Alpha(..),Bind,Fresh,
                                                          Subst(..),SubstName(..),
                                                          acompare,aeq,bind,embed,
                                                          gacompare,gaeq,gfvAny,
                                                          runFreshM,unbind,unembed)
import           Unbound.Generics.LocallyNameless.Extra  ()
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

-- Local imports
import           Clash.Core.DataCon
import           Clash.Core.Name
import           Clash.Core.Subst
import {-# SOURCE #-} Clash.Core.Term
import           Clash.Core.TyCon
import           Clash.Core.TysPrim
import           Clash.Core.Var
import           Clash.Util

varAttrs :: Var a -> [Attr']
varAttrs t@(TyVar _ _) =
  error $ $(curLoc) ++ "Unexpected argument: " ++ show t

varAttrs (Id _ ty) =
  case unembed ty of
    AnnType attrs _typ -> attrs
    _                  -> []


-- | Types in CoreHW: function and polymorphic types
data Type
  = VarTy    !Kind !TyName      -- ^ Type variable
  | ConstTy  !ConstTy           -- ^ Type constant
  | ForAllTy !(Bind TyVar Type) -- ^ Polymorphic Type
  | AppTy    !Type !Type        -- ^ Type Application
  | LitTy    !LitTy             -- ^ Type literal
  | AnnType  [Attr'] !Type      -- ^ Annotated type, see Clash.Annotations.SynthesisAttributes
  deriving (Show,Generic,NFData,Hashable,Binary)

-- | An easier view on types
data TypeView
  = FunTy    !Type  !Type      -- ^ Function type
  | TyConApp !TyConName [Type] -- ^ Applied TyCon
  | OtherType !Type            -- ^ Neither of the above
  deriving Show

-- | Type Constants
data ConstTy
  = TyCon !TyConName -- ^ TyCon type
  | Arrow            -- ^ Function type
  deriving (Show,Generic,NFData,Alpha,Hashable,Binary)

-- | Literal Types
data LitTy
  = NumTy !Integer
  | SymTy !String
  deriving (Show,Generic,NFData,Alpha,Hashable,Binary)

-- | The level above types
type Kind       = Type
-- | Either a Kind or a Type
type KindOrType = Type

-- | Reference to a Type
type TyName     = Name Type
type TyOccName  = OccName Type
-- | Reference to a Kind
type KiName     = Name Kind
type KiOccName  = OccName Kind

instance Alpha Type where
  fvAny' c nfn (VarTy t n) = fmap (VarTy t) $ fvAny' c nfn n
  fvAny' c nfn t           = fmap to . gfvAny c nfn $ from t

  aeq' c (VarTy _ n) (VarTy _ m) = aeq' c n m
  aeq' c t1          t2          = gaeq c (from t1) (from t2)

  acompare' c (VarTy _ n) (VarTy _ m) = acompare' c n m
  acompare' c t1          t2          = gacompare c (from t1) (from t2)

instance Subst a LitTy where
  subst _ _ lt = lt
  substs _ lt  = lt

instance Subst a ConstTy where
  subst _ _ ct = ct
  substs _ ct  = ct

instance Subst Term Type
instance Subst Type Type where
  isvar (VarTy _ v) = Just (SubstName (nameOcc v))
  isvar _           = Nothing

instance Eq Type where
  (==) = aeq

instance Ord Type where
  compare = acompare

-- | An easier view on types
tyView :: Type -> TypeView
tyView ty@(AppTy _ _) = case splitTyAppM ty of
  Just (ConstTy Arrow, [ty1,ty2]) -> FunTy ty1 ty2
  Just (ConstTy (TyCon tc), args) -> TyConApp tc args
  _ -> OtherType ty
tyView (ConstTy (TyCon tc)) = TyConApp tc []
tyView t = OtherType t

-- | A view on types in which newtypes are transparent, the Signal type is
-- transparent, and type functions are evaluated to WHNF (when possible).
--
-- Only strips away one "layer".
coreView :: HashMap TyConOccName TyCon -> Type -> Maybe Type
coreView tcMap ty = case tyView ty of
  TyConApp tcNm args
    | name2String tcNm == "Clash.Signal.BiSignal.BiSignalIn"
    , [_,_,_,elTy] <- args
    -> Just elTy
    | name2String tcNm == "Clash.Signal.BiSignal.BiSignalOut"
    , [_,_,_,elTy] <- args
    -> Just elTy
    | name2String tcNm == "Clash.Signal.Internal.Signal"
    , [_,elTy] <- args
    -> Just elTy
    | otherwise
    -> case tcMap HashMap.! nameOcc tcNm of
         AlgTyCon {algTcRhs = (NewTyCon _ nt)}
           -> newTyConInstRhs nt args
         _ -> reduceTypeFamily tcMap ty
  _ -> Nothing

-- | Instantiate and Apply the RHS/Original of a NewType with the given
-- list of argument types
--
-- Returns /Nothing/ when under-applied
newTyConInstRhs :: ([TyName],Type) -> [Type] -> Maybe Type
newTyConInstRhs (tvs,ty) tys
    | length tvs <= length tys
    = Just (foldl AppTy (substTys (zip tvs' tys1) ty) tys2)
    | otherwise
    = Nothing
  where
    (tys1, tys2) = splitAtList tvs tys
    tvs'         = map nameOcc tvs

-- | Make a function type of an argument and result type
mkFunTy :: Type -> Type -> Type
mkFunTy t1 = AppTy (AppTy (ConstTy Arrow) t1)

-- | Make a TyCon Application out of a TyCon and a list of argument types
mkTyConApp :: TyConName -> [Type] -> Type
mkTyConApp tc = foldl AppTy (ConstTy $ TyCon tc)

-- | Make a Type out of a TyCon
mkTyConTy :: TyConName -> Type
mkTyConTy ty = ConstTy $ TyCon ty

-- | Split a TyCon Application in a TyCon and its arguments
splitTyConAppM :: Type
               -> Maybe (TyConName,[Type])
splitTyConAppM (tyView -> TyConApp tc args) = Just (tc,args)
splitTyConAppM _                            = Nothing

-- | Is a type a Superkind?
isSuperKind :: HashMap TyConOccName TyCon -> Type -> Bool
isSuperKind tcMap (ConstTy (TyCon (((tcMap HashMap.!) . nameOcc) -> SuperKindTyCon {}))) = True
isSuperKind _ _ = False

-- | Determine the kind of a type
typeKind :: HashMap TyConOccName TyCon -> Type -> Kind
typeKind _ (VarTy k _)          = k
typeKind m (ForAllTy b)         = let (_,ty) = runFreshM $ unbind b
                                  in typeKind m ty
typeKind _ (LitTy (NumTy _))    = typeNatKind
typeKind _ (LitTy (SymTy _))    = typeSymbolKind
typeKind m (AnnType _ann typ)   = typeKind m typ
typeKind m (tyView -> FunTy _arg res)
  | isSuperKind m k = k
  | otherwise       = liftedTypeKind
  where k = typeKind m res

typeKind m (tyView -> TyConApp tc args) = foldl' kindFunResult (tyConKind (m HashMap.! nameOcc tc)) args

typeKind m (AppTy fun arg)      = kindFunResult (typeKind m fun) arg
typeKind _ (ConstTy ct)         = error $ $(curLoc) ++ "typeKind: naked ConstTy: " ++ show ct

kindFunResult :: Kind -> KindOrType -> Kind
kindFunResult (tyView -> FunTy _ res) _ = res

kindFunResult (ForAllTy b) arg =
  let (kv,ki) = runFreshM . unbind $ b
  in  substKindWith (zip [nameOcc (varName kv)] [arg]) ki

kindFunResult k tys =
  error $ $(curLoc) ++ "kindFunResult: " ++ show (k,tys)

-- | Is a type polymorphic?
isPolyTy :: Type -> Bool
isPolyTy (ForAllTy _)            = True
isPolyTy (tyView -> FunTy _ res) = isPolyTy res
isPolyTy _                       = False

-- | Split a function type in an argument and result type
splitFunTy :: HashMap TyConOccName TyCon
           -> Type
           -> Maybe (Type, Type)
splitFunTy m (coreView m -> Just ty)   = splitFunTy m ty
splitFunTy _ (tyView -> FunTy arg res) = Just (arg,res)
splitFunTy _ _ = Nothing

splitFunTys :: HashMap TyConOccName TyCon
            -> Type
            -> ([Type],Type)
splitFunTys m ty = go [] ty ty
  where
    go args orig_ty (coreView m -> Just ty')  = go args orig_ty ty'
    go args _       (tyView -> FunTy arg res) = go (arg:args) res res
    go args orig_ty _                         = (reverse args, orig_ty)

-- | Split a poly-function type in a: list of type-binders and argument types,
-- and the result type
splitFunForallTy :: Type
                 -> ([Either TyVar Type],Type)
splitFunForallTy = go []
  where
    go args (ForAllTy b) = let (tv,ty) = runFreshM $ unbind b
                           in  go (Left tv:args) ty
    go args (tyView -> FunTy arg res) = go (Right arg:args) res
    go args ty                        = (reverse args,ty)

-- | Split a poly-function type in a: list of type-binders and argument types,
-- and the result type. Looks through 'Signal' and type functions.
splitCoreFunForallTy :: HashMap TyConOccName TyCon
                     -> Type
                     -> ([Either TyVar Type], Type)
splitCoreFunForallTy tcm ty = go [] ty ty
  where
    go args orig_ty (coreView tcm -> Just ty') = go args orig_ty ty'
    go args _       (ForAllTy b)               = let (tv,res) = runFreshM $ unbind b
                                                 in  go (Left tv:args) res res
    go args _       (tyView -> FunTy arg res)  = go (Right arg:args) res res
    go args orig_ty _                          = (reverse args,orig_ty)

-- | Is a type a polymorphic or function type?
isPolyFunTy :: Type
            -> Bool
isPolyFunTy = not . null . fst . splitFunForallTy

-- | Is a type a polymorphic or function type under 'coreView'?
isPolyFunCoreTy :: HashMap TyConOccName TyCon
                -> Type
                -> Bool
isPolyFunCoreTy m (coreView m -> Just ty) = isPolyFunCoreTy m ty
isPolyFunCoreTy _ ty = case tyView ty of
  FunTy _ _ -> True
  OtherType (ForAllTy _) -> True
  _ -> False

-- | Extract rightmost type of types concatenated using an arrow (i.e., the
-- result of a function). Fails (returns Nothing) if this is not a function
-- type.
getFunResult
  :: Type
  -> Maybe Type
getFunResult typ =
  case tyView typ of
    FunTy _arg result ->
      getFunResult result <|> Just result
    _ ->
      Nothing

-- | Extract attributes from type. Will return an empty list if this is an
-- AnnType with an empty list AND if this is not an AnnType at all.
typeAttrs
  :: Type
  -> [Attr']
typeAttrs (AnnType attrs _typ) = attrs
typeAttrs _                    = []

-- | Is a type a function type?
isFunTy :: HashMap TyConOccName TyCon
        -> Type
        -> Bool
isFunTy m = isJust . splitFunTy m

-- | Apply a function type to an argument type and get the result type
applyFunTy :: HashMap TyConOccName TyCon
           -> Type
           -> Type
           -> Type
applyFunTy m (coreView m -> Just ty)   arg = applyFunTy m ty arg
applyFunTy _ (tyView -> FunTy _ resTy) _   = resTy
applyFunTy _ _ _ = error $ $(curLoc) ++ "Report as bug: not a FunTy"

-- | Substitute the type variable of a type ('ForAllTy') with another type
applyTy :: Fresh m
        => HashMap TyConOccName TyCon
        -> Type
        -> KindOrType
        -> m Type
applyTy tcm (coreView tcm -> Just ty) arg = applyTy tcm ty arg
applyTy _   (ForAllTy b) arg = do
  (tv,ty) <- unbind b
  return (substTy (nameOcc (varName tv)) arg ty)
applyTy _ ty arg = error ($(curLoc) ++ "applyTy: not a forall type:\n" ++ show ty ++ "\nArg:\n" ++ show arg)

-- | Split a type application in the applied type and the argument types
splitTyAppM :: Type
            -> Maybe (Type, [Type])
splitTyAppM = fmap (second reverse) . go []
  where
    go args (AppTy ty1 ty2) =
      case go args ty1 of
        Nothing             -> Just (ty1,ty2:args)
        Just (ty1',ty1args) -> Just (ty1',ty2:ty1args )
    go _ _ = Nothing

-- Type function substitutions

-- Given a set of type functions, and list of argument types, get the first
-- type function that matches, and return its substituted RHS type.
findFunSubst :: HashMap TyConOccName TyCon -> [([Type],Type)] -> [Type] -> Maybe Type
findFunSubst _   [] _ = Nothing
findFunSubst tcm (tcSubst:rest) args = case funSubsts tcm tcSubst args of
  Just ty -> Just ty
  Nothing -> findFunSubst tcm rest args

-- Given a ([LHS match type], RHS type) representing a type function, and
-- a set of applied types. Match LHS with args, and when successful, return
-- a substituted RHS
funSubsts :: HashMap TyConOccName TyCon -> ([Type],Type) -> [Type] -> Maybe Type
funSubsts tcm (tcSubstLhs,tcSubstRhs) args = do
  tySubts <- foldl' (funSubst tcm) (Just []) (zip tcSubstLhs args)
  let tyRhs = substTys tySubts tcSubstRhs
  -- Type functions can return higher-kinded types
  case drop (length tcSubstLhs) args of
    []    -> return tyRhs
    -- So don't forget to apply the arguments not consumed by the type
    -- function application!
    --
    -- Forgetting leads to: #232
    args' -> return (foldl' AppTy tyRhs args')

-- Given a LHS matching type, and a RHS to-match type, check if LHS and RHS
-- are a match. If they do match, and the LHS is a variable, return a
-- substitution
funSubst
  :: HashMap TyConOccName TyCon
  -> Maybe [(TyOccName,Type)]
  -> (Type,Type)
  -> Maybe [(TyOccName,Type)]
funSubst _   Nothing  = const Nothing
funSubst tcm (Just s) = uncurry go
  where
    go (VarTy _ (nameOcc -> nmF)) ty = case lookup nmF s of
      Nothing -> Just ((nmF,ty):s)
      -- Given, for example, the type family definition:
      --
      -- > type family Max x y where
      -- >   Max 0 b = b
      -- >   Max a 0 = a
      -- >   Max n n = n
      -- >   Max a b = If (a <=? b) b a
      --
      -- Then `Max 4 8` matches against the 4th clause.
      --
      -- So this is why, whenever we match against a type variable, we first
      -- check if there is already a substitution defined for this type variable,
      -- and if so, the applied type, and the type in the substitution should match.
      Just ty' | ty' == ty -> Just s
      _ -> Nothing
    go ty1 (reduceTypeFamily tcm -> Just ty2) = go ty1 ty2 -- See [Note: lazy type families]
    go ty1@(LitTy _) ty2 = if ty1 == ty2 then Just s else Nothing
    go (tyView -> TyConApp tc argTys) (tyView -> TyConApp tc' argTys')
      | tc == tc'
      = foldl' (funSubst tcm) (Just s) (zip argTys argTys')
    go _ _ = Nothing

{- [Note: lazy type families]

I don't know whether type families are evaluated strictly or lazily, but this
being Haskell, I assume type families are evaluated lazily.

Clash hence follows the Haskell way, and only evaluates type family arguments
to (WH)NF when the formal parameter is _not_ a type variable.
-}

reduceTypeFamily :: HashMap TyConOccName TyCon -> Type -> Maybe Type
reduceTypeFamily tcm (tyView -> TyConApp tc tys)
#if MIN_VERSION_ghc(8,2,0)
  | name2String tc == "GHC.TypeNats.+"
#else
  | name2String tc == "GHC.TypeLits.+"
#endif
  , [i1, i2] <- mapMaybe (litView tcm) tys
  = Just (LitTy (NumTy (i1 + i2)))

#if MIN_VERSION_ghc(8,2,0)
  | name2String tc == "GHC.TypeNats.*"
#else
  | name2String tc == "GHC.TypeLits.*"
#endif
  , [i1, i2] <- mapMaybe (litView tcm) tys
  = Just (LitTy (NumTy (i1 * i2)))

#if MIN_VERSION_ghc(8,2,0)
  | name2String tc == "GHC.TypeNats.^"
#else
  | name2String tc == "GHC.TypeLits.^"
#endif
  , [i1, i2] <- mapMaybe (litView tcm) tys
  = Just (LitTy (NumTy (i1 ^ i2)))

#if MIN_VERSION_ghc(8,2,0)
  | name2String tc == "GHC.TypeNats.-"
#else
  | name2String tc == "GHC.TypeLits.-"
#endif
  , [i1, i2] <- mapMaybe (litView tcm) tys
  = Just (LitTy (NumTy (i1 - i2)))

#if MIN_VERSION_ghc(8,2,0)
  | name2String tc == "GHC.TypeNats.<=?"
#else
  | name2String tc == "GHC.TypeLits.<=?"
#endif
  , [i1, i2] <- mapMaybe (litView tcm) tys
  , Just (FunTyCon {tyConKind = tck}) <- HashMap.lookup (nameOcc tc) tcm
  , (_,tyView -> TyConApp boolTcNm []) <- splitFunTys tcm tck
  , Just boolTc <- HashMap.lookup (nameOcc boolTcNm) tcm
  = let [falseTc,trueTc] = map (coerceName . dcName) (tyConDataCons boolTc)
    in  if i1 <= i2 then Just (mkTyConApp trueTc [] )
                    else Just (mkTyConApp falseTc [])

  | name2String tc == "GHC.TypeLits.Extra.FLog"
  , [i1, i2] <- mapMaybe (litView tcm) tys
  , i1 > 1
  , i2 > 0
  = Just (LitTy (NumTy (smallInteger (integerLogBase# i1 i2))))

  | name2String tc == "GHC.TypeLits.Extra.CLog"
  , [i1, i2] <- mapMaybe (litView tcm) tys
  , Just k <- clogBase i1 i2
  = Just (LitTy (NumTy (toInteger k)))

  | name2String tc == "GHC.TypeLits.Extra.Log"
  , [i1, i2] <- mapMaybe (litView tcm) tys
  , i1 > 1
  , i2 > 0
  = if i2 == 1
       then Just (LitTy (NumTy 0))
       else let z1 = integerLogBase# i1 i2
                z2 = integerLogBase# i1 (i2-1)
            in  if isTrue# (z1 ==# z2)
                   then Nothing
                   else Just (LitTy (NumTy (smallInteger z1)))


  | name2String tc == "GHC.TypeLits.Extra.GCD"
  , [i1, i2] <- mapMaybe (litView tcm) tys
  = Just (LitTy (NumTy (i1 `gcd` i2)))

  | name2String tc == "GHC.TypeLits.Extra.LCM"
  , [i1, i2] <- mapMaybe (litView tcm) tys
  = Just (LitTy (NumTy (i1 `lcm` i2)))

  | name2String tc == "GHC.TypeLits.Extra.Div"
  , [i1, i2] <- mapMaybe (litView tcm) tys
  , i2 > 0
  = Just (LitTy (NumTy (i1 `div` i2)))

  | name2String tc == "GHC.TypeLits.Extra.Mod"
  , [i1, i2] <- mapMaybe (litView tcm) tys
  , i2 > 0
  = Just (LitTy (NumTy (i1 `mod` i2)))

  | Just (FunTyCon {tyConSubst = tcSubst}) <- HashMap.lookup (nameOcc tc) tcm
  = findFunSubst tcm tcSubst tys

reduceTypeFamily _ _ = Nothing

litView :: HashMap TyConOccName TyCon -> Type -> Maybe Integer
litView _ (LitTy (NumTy i))                = Just i
litView m (reduceTypeFamily m -> Just ty') = litView m ty'
litView _ _ = Nothing

-- | The type of GHC.Err.undefined :: forall a . a
undefinedTy :: Type
undefinedTy =
  let aNm = string2SystemName "a"
  in  ForAllTy (bind (TyVar aNm (embed liftedTypeKind)) (VarTy liftedTypeKind aNm))

isIntegerTy :: Type -> Bool
isIntegerTy (ConstTy (TyCon (nm)))
  | "GHC.Integer.Type.Integer" `isPrefixOf` (name2String nm) = True
isIntegerTy _ = False

-- Normalize a type, looking through Signals and newtypes
--
-- For example: Vec (6-1) (Unsigned (3+1)) normalizes to Vec 5 (Unsigned 4)
normalizeType :: HashMap TyConOccName TyCon -> Type -> Type
normalizeType tcMap = go
  where
  go ty = case tyView ty of
    TyConApp tcNm args
      | name2String tcNm == "Clash.Signal.Internal.Signal"
      , [_,elTy] <- args
      -> go elTy
      -- These Clash types are implemented with newtypes.
      -- We need to keep these newtypes because they define the width of the numbers.
      | name2String tcNm == "Clash.Sized.Internal.BitVector.Bit" ||
        name2String tcNm == "Clash.Sized.Internal.BitVector.BitVector" ||
        name2String tcNm == "Clash.Sized.Internal.Index.Index"         ||
        name2String tcNm == "Clash.Sized.Internal.Signed.Signed"       ||
        name2String tcNm == "Clash.Sized.Internal.Unsigned.Unsigned"
      -> mkTyConApp tcNm (map go args)
      | otherwise
      -> case tcMap HashMap.! nameOcc tcNm of
           AlgTyCon {algTcRhs = (NewTyCon _ nt)}
             -> case newTyConInstRhs nt args of
                  Just ty' -> go ty'
                  Nothing  -> ty
           _ -> let args' = map go args
                    ty' = mkTyConApp tcNm args'
                in case reduceTypeFamily tcMap ty' of
                  Just ty'' -> ty''
                  Nothing  -> ty'
    FunTy ty1 ty2 -> mkFunTy (go ty1) (go ty2)
    (OtherType (ForAllTy (unsafeUnbind -> (tyvar,ty'))))
      -> ForAllTy (bind tyvar (go ty'))
    _ -> ty
