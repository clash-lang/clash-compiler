{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2017     , Google Inc.
                     2021-2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Types in CoreHW
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.Core.Type
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
  , coreView1
  , mkTyConTy
  , mkFunTy
  , mkPolyFunTy
  , mkTyConApp
  , splitFunTy
  , splitFunTys
  , splitFunForallTy
  , splitCoreFunForallTy
  , splitTyConAppM
  , isPolyFunTy
  , isPolyFunCoreTy
  , isPolyTy
  , isTypeFamilyApplication
  , isFunTy
  , isClassTy
  , applyFunTy
  , findFunSubst
  , reduceTypeFamily
  , isIntegerTy
  , normalizeType
  , varAttrs
  , typeAttrs
  )
where

-- External import
import           Control.DeepSeq        as DS
import           Data.Binary            (Binary)
import           Data.Coerce            (coerce)
import           Data.Hashable          (Hashable (hashWithSalt))
#if !MIN_VERSION_base(4,20,0)
import           Data.List              (foldl')
#endif
import           Data.List.Extra        (splitAtList)
import           Data.Maybe             (isJust, mapMaybe)
import           Data.Text              (Text)
import           GHC.Base               (isTrue#,(==#))
import           GHC.Generics           (Generic(..))
import           GHC.Integer            (smallInteger)
import           GHC.Integer.Logarithms (integerLogBase#)
import           GHC.TypeLits           (type TypeError, ErrorMessage(Text, (:<>:)))
#if MIN_VERSION_base(4,16,0)
import           GHC.Base               (ord)
import           Data.Char              (chr)
import           Data.Maybe             (fromMaybe)
import           Data.Text.Extra        (showt)
#endif

-- GHC API
#if MIN_VERSION_ghc(9,0,0)
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Builtin.Names
  (typeCharCmpTyFamNameKey, typeConsSymbolTyFamNameKey, typeUnconsSymbolTyFamNameKey,
   typeCharToNatTyFamNameKey, typeNatToCharTyFamNameKey)
#else
import           GHC.Builtin.Names      (typeNatLeqTyFamNameKey)
#endif
import           GHC.Builtin.Names
  (integerTyConKey, typeNatAddTyFamNameKey, typeNatExpTyFamNameKey,
   typeNatMulTyFamNameKey, typeNatSubTyFamNameKey,
   typeNatCmpTyFamNameKey, ordLTDataConKey, ordEQDataConKey, ordGTDataConKey,
   typeSymbolAppendFamNameKey, typeSymbolCmpTyFamNameKey)
import           GHC.Types.SrcLoc       (wiredInSrcSpan)
#else
import           PrelNames
  (ordLTDataConKey, ordEQDataConKey, ordGTDataConKey)
import           PrelNames
  (integerTyConKey, typeNatAddTyFamNameKey, typeNatExpTyFamNameKey,
   typeNatLeqTyFamNameKey, typeNatMulTyFamNameKey, typeNatSubTyFamNameKey,
   typeNatCmpTyFamNameKey,
   typeSymbolAppendFamNameKey, typeSymbolCmpTyFamNameKey)
import           SrcLoc                 (wiredInSrcSpan)
#endif

-- Local imports
import           Clash.Annotations.SynthesisAttributes
import           Clash.Core.DataCon
import           Clash.Core.Name
import {-# SOURCE #-} Clash.Core.Subst
import           Clash.Core.TyCon
import           Clash.Core.Var
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Unique (fromGhcUnique)
import           Clash.Util

varAttrs :: Var a -> [Attr Text]
varAttrs t@(TyVar {}) =
  error $ $(curLoc) ++ "Unexpected argument: " ++ show t

varAttrs (Id _ _ ty _) =
  case ty of
    AnnType attrs _typ -> attrs
    _                  -> []


-- | Types in CoreHW: function and polymorphic types
data Type
  = VarTy    !TyVar             -- ^ Type variable
  | ConstTy  !ConstTy           -- ^ Type constant
  | ForAllTy !TyVar !Type       -- ^ Polymorphic Type
  | AppTy    !Type !Type        -- ^ Type Application
  | LitTy    !LitTy             -- ^ Type literal
  | AnnType  [Attr Text] !Type  -- ^ Annotated type, see Clash.Annotations.SynthesisAttributes
  deriving (Show, Generic, NFData, Binary)

instance TypeError (
        'Text "A broken implementation of Hashable Type has been "
  ':<>: 'Text "removed in Clash 1.4.7. If this is an issue for you, please submit "
  ':<>: 'Text "an issue report at https://github.com/clash-lang/clash-compiler/issues."
  ) => Hashable Type where
    hashWithSalt = error "Type.hashWithSalt: unreachable"

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
  deriving (Eq,Ord,Show,Generic,NFData,Hashable,Binary)

-- | Literal Types
data LitTy
  = NumTy !Integer
  | SymTy !String
  | CharTy !Char
  deriving (Eq,Ord,Show,Generic,NFData,Hashable,Binary)

-- | The level above types
type Kind       = Type
-- | Either a Kind or a Type
type KindOrType = Type

-- | Reference to a Type
type TyName     = Name Type
-- | Reference to a Kind
type KiName     = Name Kind

-- TODO
--
-- tyView could be smarter about what it gives back. Since it traverses the
-- arguments to make a `TyConApp`, if the leftmost innermost type isn't a
-- TyCon it could still return a list of applied types to save a later call to
-- something like splitFunForallTy.
--
-- It could / should also look through annotations instead of just returning
-- the original type wrapped in OtherType.

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
tyView :: Type -> TypeView
-- XXX: this is a manually unrolled version of:
--
-- tyView tOrig = go [] tOrig
--  where
--   go args t = case t of
--     ConstTy c -> case c of
--       TyCon tc -> TyConApp tc args
--       Arrow -> case args of
--         (arg:res:rest) -> case rest of
--           [] -> FunTy arg res
--           [arg1,res1] -> FunTy arg1 res1
--           _ -> OtherType tOrig
--     AppTy l r -> go (r:args) l
--     _ -> OtherType tOrig
--
-- To get a FunTy without recursive calls. Because it is called so often this
-- saves us 5-10% runtime.
tyView tOrig = case tOrig of
  ConstTy c -> case c of
    TyCon tc -> TyConApp tc []
    _ -> OtherType tOrig
  AppTy l0 res -> case l0 of
    ConstTy (TyCon tc) -> TyConApp tc [res]
    AppTy l1 arg -> case l1 of
      ConstTy Arrow -> FunTy arg res
      ConstTy (TyCon tc) -> TyConApp tc [arg,res]
      AppTy l2 resK -> case l2 of
        ConstTy (TyCon tc) -> TyConApp tc [resK,arg,res]
        AppTy l3 argK -> case l3 of
          ConstTy (TyCon tc) -> TyConApp tc [argK,resK,arg,res]
          ConstTy Arrow -> FunTy arg res -- See Note [Arrow arguments]
          _ -> case go [argK,resK,arg,res] l3 of
            (ConstTy (TyCon tc),args)
              -> TyConApp tc args
            _ -> OtherType tOrig
        _ -> OtherType tOrig
      _ -> OtherType tOrig
    _ -> OtherType tOrig
  _ -> OtherType tOrig
 where
  go args (AppTy ty1 ty2) = go (ty2:args) ty1
  go args t1              = (t1,args)

-- | A view on types in which newtypes are transparent, the Signal type is
-- transparent, and type functions are evaluated to WHNF (when possible).
--
-- Strips away ALL layers. If no layers are found it returns the given type.
coreView :: TyConMap -> Type -> Type
coreView tcm ty =
  case coreView1 tcm ty of
    Nothing  -> ty
    Just ty' -> coreView tcm ty'

-- | A view on types in which newtypes are transparent, the Signal type is
-- transparent, and type functions are evaluated to WHNF (when possible).
--
-- Only strips away one "layer".
coreView1 :: TyConMap -> Type -> Maybe Type
coreView1 tcMap ty = case tyView ty of
  TyConApp tcNm args
    | nameOcc tcNm == "Clash.Signal.BiSignal.BiSignalIn"
    , [_,_,_,elTy] <- args
    -> Just elTy
    | nameOcc tcNm == "Clash.Signal.BiSignal.BiSignalOut"
    , [_,_,_,elTy] <- args
    -> Just elTy
    | nameOcc tcNm == "Clash.Signal.Internal.Signal"
    , [_,elTy] <- args
    -> Just elTy
    | otherwise
    -> case UniqMap.find tcNm tcMap of
         AlgTyCon {algTcRhs = (NewTyCon _ nt)}
           -> newTyConInstRhs nt args
         _ -> reduceTypeFamily tcMap ty
  OtherType (AnnType _ ty') -> coreView1 tcMap ty'
  _ -> Nothing

-- | Instantiate and Apply the RHS/Original of a NewType with the given
-- list of argument types
--
-- Returns /Nothing/ when under-applied
newTyConInstRhs :: ([TyVar],Type) -> [Type] -> Maybe Type
newTyConInstRhs (tvs,ty) tys
    | length tvs <= length tys
    = Just (foldl' AppTy (substTyWith tvs tys1 ty) tys2)
    | otherwise
    = Nothing
  where
    (tys1, tys2) = splitAtList tvs tys

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

-- | Is a type polymorphic?
isPolyTy :: Type -> Bool
isPolyTy (ForAllTy _ _)          = True
isPolyTy (tyView -> FunTy _ res) = isPolyTy res
isPolyTy _                       = False

-- | Split a function type in an argument and result type
splitFunTy :: TyConMap
           -> Type
           -> Maybe (Type, Type)
splitFunTy m (coreView1 m -> Just ty)  = splitFunTy m ty
splitFunTy _ (tyView -> FunTy arg res) = Just (arg,res)
splitFunTy _ _ = Nothing

splitFunTys :: TyConMap
            -> Type
            -> ([Type],Type)
splitFunTys m ty = go [] ty ty
  where
    go args orig_ty (coreView1 m -> Just ty') = go args orig_ty ty'
    go args _       (tyView -> FunTy arg res) = go (arg:args) res res
    go args orig_ty _                         = (reverse args, orig_ty)

-- | Split a poly-function type in a: list of type-binders and argument types,
-- and the result type
splitFunForallTy :: Type
                 -> ([Either TyVar Type],Type)
splitFunForallTy = go []
  where
    go args (ForAllTy tv ty)          = go (Left tv:args) ty
    go args (tyView -> FunTy arg res) = go (Right arg:args) res
    go args ty                        = (reverse args,ty)

-- | Make a polymorphic function type out of a result type and a list of
-- quantifiers and function argument types
mkPolyFunTy
  :: Type
  -- ^ Result type
  -> [Either TyVar Type]
  -- ^ List of quantifiers and function argument types
  -> Type
mkPolyFunTy = foldr (either ForAllTy mkFunTy)

-- | Split a poly-function type in a: list of type-binders and argument types,
-- and the result type. Looks through 'Signal' and type functions.
splitCoreFunForallTy :: TyConMap
                     -> Type
                     -> ([Either TyVar Type], Type)
splitCoreFunForallTy tcm ty = go [] ty ty
  where
    go args orig_ty (coreView1 tcm -> Just ty') = go args orig_ty ty'
    go args _       (ForAllTy tv res)           = go (Left tv:args) res res
    go args _       (tyView -> FunTy arg res)   = go (Right arg:args) res res
    go args orig_ty _                           = (reverse args,orig_ty)

-- | Is a type a polymorphic or function type?
isPolyFunTy :: Type
            -> Bool
isPolyFunTy = not . null . fst . splitFunForallTy

-- | Is a type a polymorphic or function type under 'coreView1'?
isPolyFunCoreTy :: TyConMap
                -> Type
                -> Bool
isPolyFunCoreTy m (coreView1 m -> Just ty) = isPolyFunCoreTy m ty
isPolyFunCoreTy _ ty = case tyView ty of
  FunTy _ _ -> True
  OtherType (ForAllTy _ _) -> True
  _ -> False

-- | Extract attributes from type. Will return an empty list if this is an
-- AnnType with an empty list AND if this is not an AnnType at all.
typeAttrs :: Type -> [Attr Text]
typeAttrs (AnnType attrs _typ) = attrs
typeAttrs _                    = []

-- | Is a type a function type?
isFunTy :: TyConMap
        -> Type
        -> Bool
isFunTy m = isJust . splitFunTy m

-- | Apply a function type to an argument type and get the result type
applyFunTy :: TyConMap
           -> Type
           -> Type
           -> Type
applyFunTy m (coreView1 m -> Just ty)   arg = applyFunTy m ty arg
applyFunTy _ (tyView -> FunTy _ resTy) _    = resTy
applyFunTy _ _ _ = error $ $(curLoc) ++ "Report as bug: not a FunTy"

-- Type function substitutions

-- Given a set of type functions, and list of argument types, get the first
-- type function that matches, and return its substituted RHS type.
findFunSubst :: TyConMap -> [([Type],Type)] -> [Type] -> Maybe Type
findFunSubst _   [] _ = Nothing
findFunSubst tcm (tcSubst:rest) args = case funSubsts tcm tcSubst args of
  Just ty -> Just ty
  Nothing -> findFunSubst tcm rest args

-- Given a ([LHS match type], RHS type) representing a type function, and
-- a set of applied types. Match LHS with args, and when successful, return
-- a substituted RHS
funSubsts :: TyConMap -> ([Type],Type) -> [Type] -> Maybe Type
funSubsts tcm (tcSubstLhs,tcSubstRhs) args = do
  let (funArgs,remainder) = zipAtLeast tcSubstLhs args
  tySubts <- foldl' (funSubst tcm) (Just []) funArgs
  let tyRhs = uncurry substTyWith (unzip tySubts) tcSubstRhs
  -- Type functions can return higher-kinded types
  case remainder of
    []    -> return tyRhs
    -- So don't forget to apply the arguments not consumed by the type
    -- function application!
    --
    -- Forgetting leads to: #232
    args' -> return (foldl' AppTy tyRhs args')
  where
    zipAtLeast [] ys = ([],ys)
    zipAtLeast _  [] = error "Under-applied type family"
    zipAtLeast (x:xs) (y:ys) =
      let (zs,remainder) = zipAtLeast xs ys
       in ((x,y):zs,remainder)

-- Given a LHS matching type, and a RHS to-match type, check if LHS and RHS
-- are a match. If they do match, and the LHS is a variable, return a
-- substitution
funSubst
  :: TyConMap
  -> Maybe [(TyVar,Type)]
  -> (Type,Type)
  -> Maybe [(TyVar,Type)]
funSubst _   Nothing  = const Nothing
funSubst tcm (Just s) = uncurry go
  where
    -- AnnType cannot be matched in type-families within regular GHC (as they
    -- are type synonyms) so it is fine to skip over them here.
    go (AnnType _ t1) t2 = go t1 t2

    go (VarTy nmF) ty = case lookup nmF s of
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
      Just ty' | ty' `aeqType` ty -> Just s
      _ -> Nothing

    -- Only look through annotations in RHS after the VarTy case, so we can
    -- preserve annotations in the substitution created by the VarTy case above
    go t1 (AnnType _ t2) = go t1 t2

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
    go (AppTy a1 r1) (AppTy a2 r2) = do
      s1 <- funSubst tcm (Just s) (a1, a2)
      funSubst tcm (Just s1)
                   ( r1
                   , argView tcm r2 -- See [Note: Eager type families]
                   )

    go (ConstTy c1) (ConstTy c2)
      | c1 == c2 = Just s

    go (LitTy l1) (LitTy l2)
      | l1 == l2 = Just s

    go _ _ = Nothing

{- [Note: Eager type families]

I don't know whether type families are evaluated strictly or lazily, but since
type families do not reduce on stuck argument, we assume strictly.
-}

reduceTypeFamily :: TyConMap -> Type -> Maybe Type
reduceTypeFamily tcm (tyView -> TyConApp tc tys)
  | nameUniq tc == fromGhcUnique typeNatAddTyFamNameKey
  = case mapMaybe (litView tcm) tys of
      [i1,i2] -> Just (LitTy (NumTy (i1 + i2)))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeNatMulTyFamNameKey
  = case mapMaybe (litView tcm) tys of
      [i1, i2] -> Just (LitTy (NumTy (i1 * i2)))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeNatExpTyFamNameKey
  = case mapMaybe (litView tcm) tys of
      [i1, i2] -> Just (LitTy (NumTy (i1 ^ i2)))
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeNatSubTyFamNameKey
  = case mapMaybe (litView tcm) tys of
      [i1, i2]
        | let z = i1 - i2
        , z >= 0
        -> Just (LitTy (NumTy z))
      _ -> Nothing

#if !MIN_VERSION_ghc(9,2,0)
  | nameUniq tc == fromGhcUnique typeNatLeqTyFamNameKey
  = case mapMaybe (litView tcm) tys of
      [i1, i2]
        | Just (FunTyCon {tyConKind = tck}) <- UniqMap.lookup tc tcm
        , (_,tyView -> TyConApp boolTcNm []) <- splitFunTys tcm tck
        , Just boolTc <- UniqMap.lookup boolTcNm tcm
        -> let [falseTc,trueTc] = map (coerce . dcName) (tyConDataCons boolTc)
            in  if i1 <= i2 then Just (mkTyConApp trueTc [])
                            else Just (mkTyConApp falseTc [])
      _ -> Nothing
#endif

  | nameUniq tc == fromGhcUnique typeNatCmpTyFamNameKey -- "GHC.TypeNats.CmpNat"
  = case mapMaybe (litView tcm) tys of
      [i1, i2] ->
        Just $ ConstTy $ TyCon $
          case compare i1 i2 of
            LT -> Name User "GHC.Types.LT"
                    (fromGhcUnique ordLTDataConKey) wiredInSrcSpan
            EQ -> Name User "GHC.Types.EQ"
                    (fromGhcUnique ordEQDataConKey) wiredInSrcSpan
            GT -> Name User "GHC.Types.GT"
                    (fromGhcUnique ordGTDataConKey) wiredInSrcSpan
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeSymbolCmpTyFamNameKey -- "GHC.TypeNats.CmpSymbol"
  = case mapMaybe (symLitView tcm) tys of
      [s1, s2] ->
        Just $ ConstTy $ TyCon $
          case compare s1 s2 of
            LT -> Name User "GHC.Types.LT"
                    (fromGhcUnique ordLTDataConKey) wiredInSrcSpan
            EQ -> Name User "GHC.Types.EQ"
                    (fromGhcUnique ordEQDataConKey) wiredInSrcSpan
            GT -> Name User "GHC.Types.GT"
                    (fromGhcUnique ordGTDataConKey) wiredInSrcSpan
      _ -> Nothing

#if MIN_VERSION_base(4,16,0)
  | nameUniq tc == fromGhcUnique typeCharCmpTyFamNameKey -- "GHC.TypeNats.CmpSymbol"
  = case mapMaybe (charLitView tcm) tys of
      [s1, s2] ->
        Just $ ConstTy $ TyCon $
          case compare s1 s2 of
            LT -> Name User (showt 'LT)
                    (fromGhcUnique ordLTDataConKey) wiredInSrcSpan
            EQ -> Name User (showt 'EQ)
                    (fromGhcUnique ordEQDataConKey) wiredInSrcSpan
            GT -> Name User (showt 'GT)
                    (fromGhcUnique ordGTDataConKey) wiredInSrcSpan
      _ -> Nothing

  | nameUniq tc == fromGhcUnique typeConsSymbolTyFamNameKey -- ConsSymbol
  , [c0, s0] <- tys
  , Just c1 <- charLitView tcm c0
  , Just s1 <- symLitView tcm s0
  = Just (LitTy (SymTy (c1:s1)))

  | nameUniq tc == fromGhcUnique typeUnconsSymbolTyFamNameKey -- UnconsSymbol
  , [s1] <- mapMaybe (symLitView tcm) tys
  = fromMaybe (error "reduceTypeFamily: cannot construct UnconsSymbol result") $ do
      FunTyCon {tyConKind = tck} <- UniqMap.lookup tc tcm
      TyConApp maybeTcNm [tupTcApp] <- pure (tyView (snd (splitFunTys tcm tck)))
      maybeTc <- UniqMap.lookup maybeTcNm tcm
      [nothingTc,justTc] <- pure (map (coerce . dcName) (tyConDataCons maybeTc))
      TyConApp tupTcNm [charTy,symbolTy] <- pure (tyView tupTcApp)
      tupTc <- UniqMap.lookup tupTcNm tcm
      [tupDc] <- pure (map (coerce . dcName) (tyConDataCons tupTc))
      case s1 of
        [] ->
          pure (Just (mkTyConApp nothingTc [tupTcApp]))
        (c:cs) ->
          let tup = mkTyConApp tupDc
                      [charTy,symbolTy,LitTy (CharTy c),LitTy (SymTy cs)]
           in pure (Just (mkTyConApp justTc [tupTcApp,tup]))

  | nameUniq tc == fromGhcUnique typeCharToNatTyFamNameKey -- CharToNat
  , [c1] <- mapMaybe (charLitView tcm) tys
  = Just (LitTy (NumTy (fromIntegral (ord c1))))

  | nameUniq tc == fromGhcUnique typeNatToCharTyFamNameKey -- NatToChar
  , [n1] <- mapMaybe (litView tcm) tys
  = Just (LitTy (CharTy (chr (fromInteger n1))))
#endif

  | nameUniq tc == fromGhcUnique typeSymbolAppendFamNameKey  -- GHC.TypeLits.AppendSymbol"
  = case mapMaybe (symLitView tcm) tys of
      [s1, s2] ->
        Just (LitTy (SymTy (s1 ++ s2)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.FLog", "GHC.TypeNats.FLog"]
  = case mapMaybe (litView tcm) tys of
      [i1, i2]
        | i1 > 1
        , i2 > 0
        -> Just (LitTy (NumTy (smallInteger (integerLogBase# i1 i2))))
      _ -> Nothing


  | nameOcc tc `elem` ["GHC.TypeLits.Extra.CLog", "GHC.TypeNats.CLog"]
  = case mapMaybe (litView tcm) tys of
      [i1, i2]
        | Just k <- clogBase i1 i2
        -> Just (LitTy (NumTy (toInteger k)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.Log", "GHC.TypeNats.Log"]
  = case mapMaybe (litView tcm) tys of
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
  = case mapMaybe (litView tcm) tys of
      [i1, i2] -> Just (LitTy (NumTy (i1 `gcd` i2)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.LCM", "GHC.TypeNats.LCM"]
  = case  mapMaybe (litView tcm) tys of
      [i1, i2] -> Just (LitTy (NumTy (i1 `lcm` i2)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.Div", "GHC.TypeNats.Div"]
  = case mapMaybe (litView tcm) tys of
      [i1, i2]
        | i2 > 0
        -> Just (LitTy (NumTy (i1 `div` i2)))
      _ -> Nothing

  | nameOcc tc `elem` ["GHC.TypeLits.Extra.Mod", "GHC.TypeNats.Mod"]
  = case mapMaybe (litView tcm) tys of
      [i1, i2]
        | i2 > 0
        -> Just (LitTy (NumTy (i1 `mod` i2)))
      _ -> Nothing

  | Just (FunTyCon {tyConSubst = tcSubst}) <- UniqMap.lookup tc tcm
  = let -- See [Note: Eager type families]
        tysR = map (argView tcm) tys
     in findFunSubst tcm tcSubst tysR

reduceTypeFamily _ _ = Nothing

-- |
isTypeFamilyApplication ::  TyConMap -> Type -> Bool
isTypeFamilyApplication tcm (tyView -> TyConApp tcNm _args)
  | Just (FunTyCon {}) <- UniqMap.lookup tcNm tcm = True
isTypeFamilyApplication _tcm _type = False

argView :: TyConMap -> Type -> Type
argView m t = case reduceTypeFamily m t of
  Nothing -> t
  Just tR -> argView m tR

litView :: TyConMap -> Type -> Maybe Integer
litView _ (LitTy (NumTy i))                = Just i
litView m (reduceTypeFamily m -> Just ty') = litView m ty'
litView _ _ = Nothing

symLitView :: TyConMap -> Type -> Maybe String
symLitView _ (LitTy (SymTy s))                = Just s
symLitView m (reduceTypeFamily m -> Just ty') = symLitView m ty'
symLitView _ _ = Nothing

#if MIN_VERSION_base(4,16,0)
charLitView :: TyConMap -> Type -> Maybe Char
charLitView _ (LitTy (CharTy c)) = Just c
charLitView m (reduceTypeFamily m -> Just t) = charLitView m t
charLitView _ _ = Nothing
#endif

isIntegerTy :: Type -> Bool
isIntegerTy (ConstTy (TyCon nm)) = nameUniq nm == fromGhcUnique integerTyConKey
isIntegerTy _ = False

-- | Normalize a type, looking through Signals and newtypes
--
-- For example: @Signal a (Vec (6-1) (Unsigned (3+1)))@ normalizes to @Vec 5 (Unsigned 4)@
normalizeType :: TyConMap -> Type -> Type
normalizeType tcMap = go
  where
  go ty = case tyView ty of
    TyConApp tcNm args
      -- These Clash types are implemented with newtypes.
      -- We need to keep these newtypes because they define the width of the numbers.
      | nameOcc tcNm == "Clash.Sized.Internal.BitVector.Bit" ||
        nameOcc tcNm == "Clash.Sized.Internal.BitVector.BitVector" ||
        nameOcc tcNm == "Clash.Sized.Internal.Index.Index"         ||
        nameOcc tcNm == "Clash.Sized.Internal.Signed.Signed"       ||
        nameOcc tcNm == "Clash.Sized.Internal.Unsigned.Unsigned"
      -> mkTyConApp tcNm (map go args)
      | otherwise
      -> case UniqMap.find tcNm tcMap of
          AlgTyCon {algTcRhs = (NewTyCon _ nt)}
             -> case newTyConInstRhs nt args of
                  Just ty' -> go ty'
                  Nothing  -> ty
          _ ->
             let args' = map go args
                 ty' = mkTyConApp tcNm args'
             in case reduceTypeFamily tcMap ty' of
               -- TODO Instead of recursing here, reduceTypeFamily should
               -- ensure that if the result is a reducible type family it is
               -- also reduced. This would reduce traversals over a type.
               --
               -- It may be a good idea to keep reduceTypeFamily only reducing
               -- one family, and definiing reduceTypeFamilies to reduce all
               -- it encounters in one traversal.
               Just ty'' -> go ty''
               Nothing  -> ty'
    FunTy ty1 ty2 -> mkFunTy (go ty1) (go ty2)
    OtherType (ForAllTy tyvar ty')
      -> ForAllTy tyvar (go ty')
    _ -> ty

isClassTy
  :: TyConMap
  -> Type
  -> Bool
isClassTy tcm (tyView -> TyConApp tcNm _) =
  case UniqMap.lookup tcNm tcm of
    Just (AlgTyCon {isClassTc}) -> isClassTc
    _ -> False
isClassTy _ _ = False
