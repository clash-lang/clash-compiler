{-|
Copyright   : (C) 2021-2026, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random kind-directed generation of Kind and Type.

Kinds and types are generated in an environment ('Env'): the scope they live
in, and the type variables in it that they may mention. Quantifiers get fresh
binders in that scope, so generated kinds and types are well-scoped by
construction. The closed generators ('genClosedKindFrom', 'genClosedPolyType',
'genClosedMonoType') generate in the empty environment.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Hedgehog.Core.Type
  ( genKindFrom
  , genClosedKindFrom
  , genPolyTypeFrom
  , genClosedPolyType
  , genMonoTypeFrom
  , genClosedMonoType
  , genWithCodomain
  ) where

import Data.Coerce (coerce)
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Monoid (Any(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Core.Basic (ConstTy(TyCon), LitTy(..))
import Clash.Core.DataCon (DataCon(dcName))
import Clash.Core.Pretty (showPpr)
import Clash.Core.TyCon
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Alpha (aeqTy)
import Clash.Core.DataCon (dataConInstArgTys)
import Clash.Core.Scoped.Subst (openTyBinder)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Type
  (TypeView(..), isPolyTy, mkFunTy, mkTyConApp, mkTyConTy, tyView)
import Clash.Core.Scoped.TypeEnv (mkTypeEnv)
import Clash.Core.Scoped.TysPrim

import Clash.Hedgehog.Core.Monad
import Clash.Hedgehog.Core.Name
import Clash.Hedgehog.Core.Var
import Clash.Hedgehog.Unique

-- | Classify a type or kind according to some criteria. The classification
-- of a type or kind is used to determine the pre-defined types / kinds which
-- can be used in hole fills. See @classify@ and @useTyCon@.
--
data Class = Class
  { cData :: !Any -- ^ Uses -XDataKinds
  , cPoly :: !Any -- ^ Uses -XPolyKinds (if classifying a kind)
  , cRankN :: !Any -- ^ Uses -XRankNTypes
  , cFamily :: !Any -- ^ Uses -XTypeFamilies
  } deriving (Show)

instance Semigroup Class where
  x <> y = Class
    { cData = cData x   <> cData y
    , cPoly = cPoly x   <> cPoly y
    , cRankN = cRankN x  <> cRankN y
    , cFamily = cFamily x <> cFamily y
    }

instance Monoid Class where
  mempty = Class mempty mempty mempty mempty

-- | Classify the groups that a type / kind belongs to, in order to filter out
-- kinds which are not compatible with the chosen 'CoreGenConfig'. This
-- combines multiple checks into one for efficiency, to prevent multiple passes
-- over each kind which can potentially be used.
--
classify :: Bool -> TyConMap -> Type n -> Class
classify isKind tcm = go
 where
  go :: Type x -> Class
  go ty =
    case tyView ty of
      FunTy a b ->
        -- If the domain is polymorphic then we have -XRankNTypes.
        mempty { cRankN = Any (isPolyTy a) } <> go a <> go b

      TyConApp tcn args ->
        let tc = UniqMap.find tcn tcm
            isPoly = isPolyResult (length args) (tyConKind tc)
            argsClass = mconcat (fmap go args)
         in case tc of
              AlgTyCon{} ->
                -- If the constructor is algebraic then we have -XDataKinds if
                -- we are classifying a Kind instead of a Type.
                mempty { cData = Any isKind, cPoly = Any isPoly } <> argsClass

              PromotedDataCon{} ->
                -- If the constructor is a promoted data constructor then we
                -- have -XDataKinds.
                mempty { cData = Any True, cPoly = Any isPoly } <> argsClass

              FunTyCon{} ->
                -- If the constructor is a function then we have -XTypeFamilies.
                mempty { cPoly = Any isPoly, cFamily = Any True } <> argsClass

              PrimTyCon{}
                -- There's nothing special about Type.
                | aeqTy ty liftedTypeKind -> mempty

                -- If the constructor is Nat or Symbol, we have -XDataKinds.
                | aeqTy ty typeNatKind -> mempty { cData = Any True }
                | aeqTy ty typeSymbolKind -> mempty { cData = Any True }

                -- If the constructor is ~# then we have -XTypeFamilies.
                | aeqTy ty eqPrimTy ->
                    mempty { cPoly = Any isPoly, cFamily = Any True } <> argsClass

                -- If the constructor is anything else we have -XDataKinds if
                -- we are classifying a Kind instead of a Type.
                | otherwise ->
                    mempty { cData = Any isKind, cPoly = Any isPoly } <> argsClass

      OtherType{} ->
        case ty of
          ForAllTy _ a ->
            -- If there are quantifiers then we have polymorphism.
            mempty { cPoly = Any True } <> go a

          LitTy{} ->
            -- If there are literals then we have -XDataKinds.
            mempty { cData = Any True }

          VarTy{} -> mempty
          AppTy a b -> go a <> go b
          AnnType _ a -> go a
          ConstTy _ -> error ("classify: Naked ConstTy: " <> showPpr ty)

-- | Whether the kind of a type constructor, applied to the given number of
-- arguments, is polymorphic
isPolyResult :: Int -> Kind n -> Bool
isPolyResult 0 k = isPolyTy k
isPolyResult n (ForAllTy _ k) = isPolyResult (n - 1) k
isPolyResult n (tyView -> FunTy _ k) = isPolyResult (n - 1) k
isPolyResult _ _ = False

-- | Decide whether to use a type constructor based on the configuration and
-- the result of @classifyKind@. A type constructor is not usable if it uses
-- any features which are not included in the current configuration.
--
useTyCon :: Bool -> CoreGenConfig -> TyConMap -> TyCon -> Bool
useTyCon isKind config tcm tc
  | isKind
  = and
      -- We don't generate equalities at the kind level, because the only
      -- witness we have for equality is a term-level primitive (_CO_).
      [ isPrimKind ty
      , getAny (cData c) --> allowDataKinds config
      , getAny (cPoly c) --> allowPolyKinds config
      , getAny (cRankN c) --> allowRankNTypes config
      , getAny (cFamily c) --> allowTypeFamilies config
      ]

  | otherwise
  = and
      -- We don't generate Type, Nat or Symbol at the type level, because they
      -- have no term-level inhabitants. We also don't generate constraints
      -- like ~# because constraints are generated separately.
      [ not (isPrimKind ty)
      , not (aeqTy ty eqPrimTy)
      , getAny (cData c) --> allowDataKinds config
      , getAny (cRankN c) --> allowRankNTypes config
      , getAny (cFamily c) --> allowTypeFamilies config
      ]
 where
  a --> b = not a || b
  isPrimKind a = any (aeqTy a) [liftedTypeKind, typeNatKind, typeSymbolKind]
  ty = mkTyConTy (tyConName tc) :: Type 'VoidS
  c = classify isKind tcm ty

-- | Generate a function where the codomain is the given type / kind. Any other
-- restrictions are enforced by the given generator. This can be used with
-- generators for kinds and types.
--
genWithCodomain
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => Env n
  -> Kind n
  -> CoreGenT m (Type n)
  -> CoreGenT m (Type n)
genWithCodomain env cod gen =
  withCodomain (envScope env) cod <$> gen

-- | Add the given codomain to the end of a (polymorphic) function type, going
-- under its quantifiers
withCodomain :: Distinct n => Scope n -> Kind n -> Type n -> Type n
withCodomain sc cod = \case
  ForAllTy b t ->
    openTyBinder sc b t $ \sc' b' t' -> ForAllTy b' (withCodomain sc' (sink cod) t')
  (tyView -> FunTy a r) -> mkFunTy a (withCodomain sc cod r)
  ty -> mkFunTy ty cod

-- TODO
-- genConstraints
-- genConstrained

-- | Generate a closed kind (one without any free variables). If you want to
-- be able to use free variables in a kind, see 'genKindFrom'.
--
genClosedKindFrom
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> Kind 'VoidS
  -> CoreGenT m (Kind 'VoidS)
genClosedKindFrom tcm =
  genKindFrom tcm emptyEnv

-- | Generate a kind which is valid for the given 'TyConMap'. The kind may
-- contain the type variables of the given environment, and is a valid fit
-- for a hole with the given kind.
--
-- __N.B.__ Although the kind generated is a fit for the given hole, calling
-- a function like 'Clash.Core.Scoped.HasType.inferCoreKindOf' may return a
-- different kind. This is because quantifiers are both the introduction rule
-- for kind arrows and a kind former of their own right, so for the hole
--
--   Type -> Type
--
-- a generated fit might be
--
--   forall a. a -> a
--
-- but this is then inferred to have the kind
--
--   Type
--
genKindFrom
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Kind n
  -> CoreGenT m (Kind n)
genKindFrom tcm env hole =
  let genSub = genKindFrom tcm env
      -- A special case for holes of kind Type: we do not attempt to generate
      -- a fresh hole fit as this will produce an endless stream of (->) when
      -- not using -XPolyKinds.
      genOr = if aeqTy hole liftedTypeKind
                then empty
                else genFreshKind tcm env hole
   in Gen.choice
        [ sampleTyConOr True tcm hole genSub
            (sampleTyVarOr env hole genSub genOr)
        , sampleTyVarOr env hole genSub
            (sampleTyConOr True tcm hole genSub genOr)
        ]

-- | Generate a polymorphic type which is valid for the given environment.
-- The generated type should have the specified kind, and no free variables.
--
genClosedPolyType
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> Kind 'VoidS
  -> CoreGenT m (Type 'VoidS)
genClosedPolyType tcm =
  genPolyTypeFrom tcm emptyEnv

-- | Generate a polymorphic type which is valid for the given environment.
-- The generated type should have the specified kind, and may contain the
-- type variables of the environment.
--
genPolyTypeFrom
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Kind n
  -> CoreGenT m (Type n)
genPolyTypeFrom tcm env hole =
  let genSub = genPolyTypeFrom tcm env
      genOr  = genFreshPolyType tcm env hole
   in Gen.choice
        [ sampleTyConOr False tcm hole genSub
            (sampleTyVarOr env hole genSub genOr)
        , sampleTyVarOr env hole genSub
            (sampleTyConOr False tcm hole genSub genOr)
        , genFreshPolyType tcm env hole
        ]

-- | Generate a monomorphic type which is valid for the given environment.
-- The generated type should have the specified kind, and no free variables.
--
genClosedMonoType
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> Kind 'VoidS
  -> CoreGenT m (Type 'VoidS)
genClosedMonoType tcm =
  genMonoTypeFrom tcm emptyEnv

-- | Generate a monomorphic type which is valid for the given environment.
-- The generated type should have the specified kind, and may contain the
-- type variables of the environment.
--
genMonoTypeFrom
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Kind n
  -> CoreGenT m (Type n)
genMonoTypeFrom tcm env hole =
  let genSub = genMonoTypeFrom tcm env
      -- TODO Maybe this can be changed for types, because it is always free
      -- to generate a forall, and the new binder may help break loops.
      genOr = if aeqTy hole liftedTypeKind
                then empty
                else genFreshMonoType tcm env hole
   in Gen.choice
        [ sampleTyConOr False tcm hole genSub
            (sampleTyVarOr env hole genSub genOr)
        , sampleTyVarOr env hole genSub
            (sampleTyConOr False tcm hole genSub genOr)
        , genFreshMonoType tcm env hole
        ]

-- | For the given hole, attempt to use a variable in the environment to fill
-- the hole, potentially solving subgoals if the variable is function kinded
-- and the hole is the codomain.
--
sampleTyVarOr
  :: forall m n
   . (Alternative m, MonadGen m)
  => Env n
  -> Kind n
  -> (Kind n -> CoreGenT m (Type n))
  -> CoreGenT m (Type n)
  -> CoreGenT m (Type n)
sampleTyVarOr env hole genSub genOr =
  sampleTyVar <|> genOr
 where
  sampleTyVar = do
    (tv, holes) <- sampleList kindOfTyVar (const True) hole (envTyVars env)
    holeFills <- traverse genSub holes

    pure (List.foldl' AppTy tv holeFills)

  kindOfTyVar = \case
    VarTy _ k _ -> k
    ty -> error ("sampleTyVarOr: not a type variable: " <> showPpr ty)

-- | For the given hole, attempt to use a type constructor in the 'TyConMap' to
-- fill the hole, potentially solving subgoals if the constructor is function
-- kinded and the hole is the codomain.
--
sampleTyConOr
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => Bool
  -> TyConMap
  -> Kind n
  -> (Kind n -> CoreGenT m (Type n))
  -> CoreGenT m (Type n)
  -> CoreGenT m (Type n)
sampleTyConOr isKind tcm hole genSub genOr =
  sampleTyCon <|> genOr
 where
  sampleTyCon = do
    config <- ask
    (tc, holes) <-
      sampleUniqMapBiased (sinkClosed . tyConKind) (useTyCon isKind config tcm) hole tcm
    holeFills <- traverse genSub holes

    pure (mkTyConApp (tyConName tc) holeFills)

-- | Generate a quantifier over a fresh type variable of the first kind, whose
-- body fits the second kind
genForAll
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => Env n
  -> Kind n
  -> Kind n
  -> (forall l. Distinct l => Env l -> Kind l -> CoreGenT m (Type l))
  -> CoreGenT m (Type n)
genForAll env k1 k2 genSub =
  genTyVar env k1 genVarName $ \env' v ->
    ForAllTy v <$> genSub env' (sink k2)

-- | Generate a "fresh" kind. This involves using the shape of the hole to
-- generate a layer of the result kind, then solving any subgoal with either
-- a variable, type constructor or another "fresh" kind.
--
genFreshKind
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Kind n
  -> CoreGenT m (Kind n)
genFreshKind tcm env hole =
  canGenPolyKinds >>= \case
    True -> genPolyKind tcm env hole
    False -> genMonoKind tcm env hole

-- | Generate a potentially polymorphic kind to fill a hole. This should not be
-- exported as it can be used to circumvent constraints on generation which are
-- given by the 'CoreGenConfig'.
--
genPolyKind
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Kind n
  -> CoreGenT m (Kind n)
genPolyKind tcm env hole
  -- Hole: forall v. a
  | ForAllTy v a <- hole
  = openTyBinder (envScope env) v a $ \sc v' a' ->
      ForAllTy v' <$> genKindFrom tcm (extendEnv sc [tyBinderVar v'] [] env) a'

  -- Hole: a -> b
  | FunTy a b <- tyView hole
  = genForAll env a b (genKindFrom tcm)

  -- Hole: Type
  --
  -- If -XRankNTypes is not enabled, then we make sure the LHS of a generated
  -- arrow kind is a monomorphic kind.
  --
  -- As we shrink, it becomes more likely we just return Type for this hole.
  -- This rule is needed to prevent the generator recursing infinitely.
  | aeqTy hole liftedTypeKind
  = canGenRankNTypes >>= \case
      True ->
        let polyGen = genKindFrom tcm env liftedTypeKind
         in Gen.choice
              [ Gen.subterm2 polyGen polyGen mkFunTy
              , genForAll env liftedTypeKind liftedTypeKind (genKindFrom tcm)
              ]

      False ->
        let polyGen = genKindFrom tcm env liftedTypeKind
            monoGen = local (\r -> r { allowPolyKinds = False }) polyGen
         in Gen.choice
              [ Gen.subterm2 monoGen polyGen mkFunTy
              , genForAll env liftedTypeKind liftedTypeKind (genKindFrom tcm)
              ]

  -- The hole is not anything which may result in a quantifier being generated,
  -- so we can fallback to genMonoKind for these cases.
  | otherwise
  = genMonoKind tcm env hole

genMonoKind
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Kind n
  -> CoreGenT m (Kind n)
genMonoKind tcm env hole
  -- Hole: Nat
  --
  -- This comes before algebraic data types, as Nat is Natural.
  | aeqTy hole typeNatKind
  = canGenDataKinds >>= \case
      True -> genNatLit
      False -> error "genMonoKind: Cannot generate Nat without -XDataKinds"

  -- Hole: Symbol
  | aeqTy hole typeSymbolKind
  = canGenDataKinds >>= \case
      True -> genSymLit
      False -> error "genMonoKind: Cannot generate Symbol without -XDataKinds"

  -- Hole: C
  | Just dcs <- promotedFits tcm env hole
  = do (dc, argKns) <- Gen.element dcs
       args <- traverse (genKindFrom tcm env) argKns
       pure (mkTyConApp (coerce (dcName dc)) args)

  -- Hole: Type
  --
  -- As we shrink, it becomes more likely we just return Type for this hole.
  -- This rule is needed to prevent the generator recursing infinitely.
  | aeqTy hole liftedTypeKind
  = let gen = genKindFrom tcm env liftedTypeKind
     in Gen.subterm2 gen gen mkFunTy

  | otherwise
  = error ("genMonoKind: Cannot generate fit for hole: " <> showPpr hole)

genFreshPolyType
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Kind n
  -> CoreGenT m (Type n)
genFreshPolyType tcm env hole
  | ForAllTy tv kn <- hole
  = openTyBinder (envScope env) tv kn $ \sc tv' kn' ->
      ForAllTy tv' <$> genPolyTypeFrom tcm (extendEnv sc [tyBinderVar tv'] [] env) kn'

  | FunTy a b <- tyView hole
  = genForAll env a b (genPolyTypeFrom tcm)

  | aeqTy hole liftedTypeKind
  = canGenRankNTypes >>= \case
      True ->
        let polyGen = genPolyTypeFrom tcm env liftedTypeKind
         in Gen.choice
              [ genFreshMonoType tcm env liftedTypeKind
              , Gen.subterm2 polyGen polyGen mkFunTy
              , genForAll env liftedTypeKind liftedTypeKind (genPolyTypeFrom tcm)
              ]

      False ->
        let polyGen = genPolyTypeFrom tcm env liftedTypeKind
            monoGen = genMonoTypeFrom tcm env liftedTypeKind
         in Gen.choice
              [ genFreshMonoType tcm env hole
              , Gen.subterm2 monoGen polyGen mkFunTy
              , genForAll env liftedTypeKind liftedTypeKind (genPolyTypeFrom tcm)
              ]

  | otherwise
  = genFreshMonoType tcm env hole

genFreshMonoType
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Kind n
  -> CoreGenT m (Type n)
genFreshMonoType tcm env hole
  | aeqTy hole typeNatKind
  = canGenDataKinds >>= \case
      True -> genNatLit
      False -> error "genFreshMonoType: Cannot generate Nat without -XDataKinds"

  | aeqTy hole typeSymbolKind
  = canGenDataKinds >>= \case
      True -> genSymLit
      False -> error "genFreshMonoType: Cannot generate Symbol without -XDataKinds"

  | Just dcs <- promotedFits tcm env hole
  = do (dc, argKns) <- Gen.element dcs
       args <- traverse (genMonoTypeFrom tcm env) argKns
       pure (mkTyConApp (coerce (dcName dc)) args)

  | aeqTy hole liftedTypeKind
  = let gen = genMonoTypeFrom tcm env liftedTypeKind
     in Gen.subterm2 gen gen mkFunTy

  | otherwise
  = error ("genFreshMonoType: Cannot generate fit for hole: " <> showPpr hole)

-- | The data constructors of the algebraic data type that a hole is, with the
-- kinds of their fields, to fill the hole with a promoted data constructor.
-- 'Nothing' if the hole is no algebraic data type without type arguments, or
-- if it has no such data constructors.
promotedFits
  :: Distinct n
  => TyConMap
  -> Env n
  -> Kind n
  -> Maybe [(DataCon, [Kind n])]
promotedFits tcm env hole
  | ConstTy (TyCon tcn) <- hole
  , Just tc <- UniqMap.lookup tcn tcm
  , dcs@(_:_) <- mapMaybe instDc (tyConDataCons tc)
  = Just dcs

  | otherwise
  = Nothing
 where
  instDc dc = (dc,) <$> dataConInstArgTys (mkTypeEnv tcm) (envScope env) dc []

genNatLit :: forall m n. MonadGen m => m (Type n)
genNatLit = LitTy . NumTy . toInteger <$> Gen.word Range.linearBounded

genSymLit :: forall m n. MonadGen m => m (Type n)
genSymLit = LitTy . SymTy <$> Gen.string (Range.linear 5 10) Gen.alphaNum
