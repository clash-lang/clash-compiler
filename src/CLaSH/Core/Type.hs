{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
{-# OPTIONS_GHC -fno-warn-duplicate-constraints #-}
#endif

-- | Types in CoreHW
module CLaSH.Core.Type
  ( Type (..)
  , TypeView (..)
  , ConstTy (..)
  , LitTy (..)
  , Kind
  , KindOrType
  , KiName
  , TyName
  , TyVar
  , tyView
  , coreView
  , transparentTy
  , typeKind
  , mkTyConTy
  , mkFunTy
  , mkTyConApp
  , splitFunTy
  , splitFunForallTy
  , splitTyConAppM
  , isPolyTy
  , isFunTy
  , applyFunTy
  , applyTy
  )
where

-- External import
import                Data.Maybe                    (isJust)
import                Unbound.LocallyNameless       as Unbound hiding (Arrow)
import                Unbound.LocallyNameless.Alpha (aeqR1,fvR1)
import                Unbound.LocallyNameless.Ops   (unsafeUnbind)

-- Local imports
import                CLaSH.Core.Subst
import {-# SOURCE #-} CLaSH.Core.Term
import                CLaSH.Core.TyCon
import                CLaSH.Core.TysPrim
import                CLaSH.Core.Var
import                CLaSH.Util

-- | Types in CoreHW: function and polymorphic types
data Type
  = VarTy    Kind TyName       -- ^ Type variable
  | ConstTy  ConstTy           -- ^ Type constant
  | ForAllTy (Bind TyVar Type) -- ^ Polymorphic Type
  | AppTy    Type Type         -- ^ Type Application
  | LitTy    LitTy             -- ^ Type literal
  deriving Show

-- | An easier view on types
data TypeView
  = FunTy    Type  Type   -- ^ Function type
  | TyConApp TyCon [Type] -- ^ Applied TyCon
  | OtherType Type        -- ^ Neither of the above
  deriving Show

-- | Type Constants
data ConstTy
  = TyCon TyCon -- ^ TyCon type
  | Arrow       -- ^ Function type
  deriving Show

-- | Literal Types
data LitTy
  = NumTy Int
  | SymTy String
  deriving Show

-- | The level above types
type Kind       = Type
-- | Either a Kind or a Type
type KindOrType = Type

-- | Reference to a Type
type TyName     = Name Type
-- | Reference to a Kind
type KiName     = Name Kind

Unbound.derive [''Type,''LitTy,''ConstTy]

instance Alpha Type where
  fv' c (VarTy _ n) = fv' c n
  fv' c t           = fvR1 rep1 c t

  aeq' c (VarTy _ n) (VarTy _ m) = aeq' c n m
  aeq' c t1          t2          = aeqR1 rep1 c t1 t2

instance Alpha ConstTy
instance Alpha LitTy

instance Subst Type LitTy
instance Subst Term LitTy
instance Subst Type ConstTy
instance Subst Term ConstTy
instance Subst Term Type
instance Subst Type Type where
  isvar (VarTy _ v) = Just (SubstName v)
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

-- | A transformation that renders 'Signal' types transparent
transparentTy :: Type -> Type
transparentTy (AppTy (ConstTy (TyCon tc)) ty)
  = case name2String (tyConName tc) of
      "CLaSH.Signal.Signal"  -> transparentTy ty
      "CLaSH.Signal.SignalP" -> transparentTy ty
      _ -> AppTy (ConstTy (TyCon tc)) (transparentTy ty)
transparentTy (AppTy ty1 ty2) = AppTy (transparentTy ty1) (transparentTy ty2)
transparentTy (ForAllTy b)    = ForAllTy (uncurry bind $ second transparentTy $ unsafeUnbind b)
transparentTy ty              = ty

-- | A view on types in which 'Signal' types and newtypes are transparent
coreView :: Type -> TypeView
coreView ty =
  let tView = tyView ty
  in case tView of
       TyConApp (AlgTyCon {algTcRhs = (NewTyCon _ nt)}) args
         | length (fst nt) == length args -> coreView (newTyConInstRhs nt args)
         | otherwise  -> tView
       TyConApp tc args -> case name2String (tyConName tc) of
         "CLaSH.Signal.Signal"  -> coreView (head args)
         "CLaSH.Signal.SignalP" -> coreView (head args)
         _ -> tView
       _ -> tView

-- | Instantiate and Apply the RHS/Original of a NewType with the given
-- list of argument types
newTyConInstRhs :: ([TyName],Type) -> [Type] -> Type
newTyConInstRhs (tvs,ty) tys = foldl AppTy (substTys (zip tvs tys1) ty) tys2
  where
    (tys1, tys2) = splitAtList tvs tys

-- | Make a function type of an argument and result type
mkFunTy :: Type -> Type -> Type
mkFunTy t1 = AppTy (AppTy (ConstTy Arrow) t1)

-- | Make a TyCon Application out of a TyCon and a list of argument types
mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp tc = foldl AppTy (ConstTy $ TyCon tc)

-- | Make a Type out of a TyCon
mkTyConTy :: TyCon -> Type
mkTyConTy ty = ConstTy $ TyCon ty

-- | Split a TyCon Application in a TyCon and its arguments
splitTyConAppM :: Type
               -> Maybe (TyCon,[Type])
splitTyConAppM (tyView -> TyConApp tc args) = Just (tc,args)
splitTyConAppM _                            = Nothing

-- | Is a type a Superkind?
isSuperKind :: Type -> Bool
isSuperKind (ConstTy (TyCon (SuperKindTyCon {}))) = True
isSuperKind _                                     = False

-- | Determine the kind of a type
typeKind :: Type -> Kind
typeKind (VarTy k _)          = k
typeKind (ForAllTy b)         = let (_,ty) = runFreshM $ unbind b
                                in typeKind ty
typeKind (LitTy (NumTy _))    = typeNatKind
typeKind (LitTy (SymTy _))    = typeSymbolKind
typeKind (tyView -> FunTy _arg res)
  | isSuperKind k = k
  | otherwise     = liftedTypeKind
  where k = typeKind res

typeKind (tyView -> TyConApp tc args) = foldl kindFunResult (tyConKind tc) args

typeKind (AppTy fun arg)      = kindFunResult (typeKind fun) arg
typeKind (ConstTy ct)         = error $ $(curLoc) ++ "typeKind: naked ConstTy: " ++ show ct

kindFunResult :: Kind -> KindOrType -> Kind
kindFunResult (tyView -> FunTy _ res) _ = res

kindFunResult (ForAllTy b) arg =
  let (kv,ki) = runFreshM . unbind $ b
  in  substKindWith (zip [varName kv] [arg]) ki

kindFunResult k tys =
  error $ $(curLoc) ++ "kindFunResult: " ++ show (k,tys)

-- | Is a type polymorphic?
isPolyTy :: Type -> Bool
isPolyTy (ForAllTy _)            = True
isPolyTy (tyView -> FunTy _ res) = isPolyTy res
isPolyTy _                       = False

-- | Split a function type in an argument and result type
splitFunTy :: Type
           -> Maybe (Type, Type)
splitFunTy (coreView -> FunTy arg res) = Just (arg,res)
splitFunTy _                           = Nothing

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

-- | Is a type a function type?
isFunTy :: Type
        -> Bool
isFunTy = isJust . splitFunTy

-- | Apply a function type to an argument type and get the result type
applyFunTy :: Type
           -> Type
           -> Type
applyFunTy (coreView -> FunTy _ resTy) _ = resTy
applyFunTy _ _ = error $ $(curLoc) ++ "Report as bug: not a FunTy"

-- | Substitute the type variable of a type ('ForAllTy') with another type
applyTy :: Fresh m
        => Type
        -> KindOrType
        -> m Type
applyTy (ForAllTy b) arg = do
  (tv,ty) <- unbind b
  return $ substTy (varName tv) arg ty
applyTy _ _ = error $ $(curLoc) ++ "applyTy: not a forall type"

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
