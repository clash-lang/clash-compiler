{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random kind-directed generation of Kind and Type.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Monoid (Any(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Core.DataCon
import Clash.Core.HasType (piResultTys)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Subst (aeqType)
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.TysPrim
import Clash.Data.UniqMap (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap

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
classify :: Bool -> TyConMap -> KindOrType -> Class
classify isKind tcm = go
 where
  go ty =
    case tyView ty of
      FunTy a b ->
        -- If the domain is polymorphic then we have -XRankNTypes.
        mempty { cRankN = Any (isPolyTy a) } <> go a <> go b

      TyConApp tcn args ->
        let tc = UniqMap.find tcn tcm
            isPoly = isPolyTy (piResultTys tcm (tyConKind tc) args)
         in case UniqMap.find tcn tcm of
              AlgTyCon{} ->
                -- If the constructor is algebraic then we have -XDataKinds if
                -- we are classifying a Kind instead of a Type.
                mempty { cData = Any isKind, cPoly = Any isPoly }
                  <> mconcat (fmap go args)

              PromotedDataCon{} ->
                -- If the constructor is a promoted data constructor then we
                -- have -XDataKinds.
                mempty { cData = Any True, cPoly = Any isPoly }
                  <> mconcat (fmap go args)

              FunTyCon{} ->
                -- If the constructor is a function then we have -XTypeFamilies.
                mempty { cPoly = Any isPoly, cFamily = Any True }
                  <> mconcat (fmap go args)

              PrimTyCon{}
                -- There's nothing special about Type.
                | aeqType ty liftedTypeKind -> mempty

                -- If the constructor is Nat or Symbol, we have -XDataKinds.
                | aeqType ty typeNatKind -> mempty { cData = Any True }
                | aeqType ty typeSymbolKind -> mempty { cData = Any True }

                -- If the constructor is ~# then we have -XTypeFamilies.
                | aeqType ty eqPrimTy ->
                    mempty { cPoly = Any isPoly, cFamily = Any True }
                      <> mconcat (fmap go args)

                -- If the constructor is anything else we have -XDataKinds if
                -- we are classifying a Kind instead of a Type.
                | otherwise ->
                    mempty { cData = Any isKind, cPoly = Any isPoly }
                      <> mconcat (fmap go args)

      OtherType{} ->
        case ty of
          ForAllTy _ a ->
            -- If there are quantifiers then we have polymorphism.
            mempty { cPoly = Any True } <> go a

          LitTy{} ->
            -- If there are literals then we have -XDataKinds.
            mempty { cData = Any True }

          VarTy _ -> mempty
          AppTy a b -> go a <> go b
          AnnType _ a -> go a
          ConstTy _ -> error ("classify: Naked ConstTy: " <> showPpr ty)

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
      , not (aeqType ty eqPrimTy)
      , getAny (cData c) --> allowDataKinds config
      , getAny (cRankN c) --> allowRankNTypes config
      , getAny (cFamily c) --> allowTypeFamilies config
      ]
 where
  a --> b = not a || b
  isPrimKind a = any (aeqType a) [liftedTypeKind, typeNatKind, typeSymbolKind]
  ty = mkTyConTy (tyConName tc)
  c = classify isKind tcm ty

-- | Generate a function where the codomain is the given type / kind. Any other
-- restrictions are enforced by the given generator. This can be used with
-- generators for kinds and types.
--
genWithCodomain
  :: forall m
   . (Alternative m, MonadGen m)
  => Kind
  -> CoreGenT m KindOrType
  -> CoreGenT m KindOrType
genWithCodomain cod gen = do
  (args, res) <- fmap splitFunForallTy gen
  pure (mkPolyFunTy cod (args <> [Right res]))

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
  -> Kind
  -> CoreGenT m Kind
genClosedKindFrom tcm =
  genKindFrom tcm mempty

-- | Generate a kind which is valid for the given 'TyConMap'. The kind may
-- contain free variables which are given in a 'UniqMap', and is a valid fit
-- for a hole with the given kind.
--
-- __N.B.__ Although the kind generated is a fit for the given hole, calling
-- a function like 'Clash.Core.HasType.inferCoreKindOf' may return a different
-- kind. This is because quantifiers are both the introduction rule for kind
-- arrows and a kind former of their own right, so for the hole
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
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap TyVar
  -> Kind
  -> CoreGenT m Kind
genKindFrom tcm env hole =
  let genSub = genKindFrom tcm env
      -- A special case for holes of kind Type: we do not attempt to generate
      -- a fresh hole fit as this will produce an endless stream of (->) when
      -- not using -XPolyKinds.
      genOr = if aeqType hole liftedTypeKind
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
  -> Kind
  -> CoreGenT m Type
genClosedPolyType tcm =
  genPolyTypeFrom tcm mempty

-- | Generate a polymorphic type which is valid for the given environment.
-- The generated type should have the specified kind, and may contain the
-- specified free variables.
--
genPolyTypeFrom
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap TyVar
  -> Kind
  -> CoreGenT m Type
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
  -> Kind
  -> CoreGenT m Type
genClosedMonoType tcm =
  genMonoTypeFrom tcm mempty

-- | Generate a monomorphic type which is valid for the given environment.
-- The generated type should have the specified kind, and may contain the
-- specified free variables.
--
genMonoTypeFrom
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap TyVar
  -> Kind
  -> CoreGenT m Type
genMonoTypeFrom tcm env hole =
  let genSub = genMonoTypeFrom tcm env
      -- TODO Maybe this can be changed for types, because it is always free
      -- to generate a forall, and the new binder may help break loops.
      genOr = if aeqType hole liftedTypeKind
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
  :: forall m
   . (Alternative m, MonadGen m)
  => UniqMap TyVar
  -> Kind
  -> (Kind -> CoreGenT m KindOrType)
  -> CoreGenT m KindOrType
  -> CoreGenT m KindOrType
sampleTyVarOr env hole genSub genOr =
  sampleTyVar <|> genOr
 where
  sampleTyVar = do
    (tv, holes) <- sampleUniqMap (const True) hole env
    holeFills <- traverse genSub holes

    pure (foldr AppTy (VarTy tv) holeFills)

-- | For the given hole, attempt to use a type constructor in the 'TyConMap' to
-- fill the hole, potentially solving subgoals if the constructor is function
-- kinded and the hole is the codomain.
--
sampleTyConOr
  :: forall m
   . (Alternative m, MonadGen m)
  => Bool
  -> TyConMap
  -> Kind
  -> (Kind -> CoreGenT m KindOrType)
  -> CoreGenT m KindOrType
  -> CoreGenT m KindOrType
sampleTyConOr isKind tcm hole genSub genOr =
  sampleTyCon <|> genOr
 where
  sampleTyCon = do
    config <- ask
    (tc, holes) <- sampleUniqMapBiased (useTyCon isKind config tcm) hole tcm
    holeFills <- traverse genSub holes

    pure (mkTyConApp (tyConName tc) holeFills)

genForAll
  :: forall m
   . (Alternative m, MonadGen m)
  => UniqMap TyVar
  -> Kind
  -> Kind
  -> (UniqMap TyVar -> Kind -> CoreGenT m KindOrType)
  -> CoreGenT m KindOrType
genForAll env k1 k2 genSub = do
  v <- genTyVar k1 (genFreshName env genVarName)
  Gen.subterm (genSub (UniqMap.insertUnique v env) k2) (ForAllTy v)

-- | Generate a "fresh" kind. This involves using the shape of the hole to
-- generate a layer of the result kind, then solving any subgoal with either
-- a variable, type constructor or another "fresh" kind.
--
genFreshKind
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap TyVar
  -> Kind
  -> CoreGenT m Kind
genFreshKind tcm env hole =
  canGenPolyKinds >>= \case
    True -> genPolyKind tcm env hole
    False -> genMonoKind tcm env hole

-- | Generate a potentially polymorphic kind to fill a hole. This should not be
-- exported as it can be used to circumvent constraints on generation which are
-- given by the 'CoreGenConfig'.
--
genPolyKind
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap TyVar
  -> Kind
  -> CoreGenT m Kind
genPolyKind tcm env hole
  -- Hole: forall v. a
  | ForAllTy v a <- hole
  = let env' = UniqMap.insertUnique v env
     in Gen.subterm (genKindFrom tcm env' a) (ForAllTy v)

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
  | aeqType hole liftedTypeKind
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
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap TyVar
  -> Kind
  -> CoreGenT m Kind
genMonoKind tcm env hole
  -- Hole: C
  | ConstTy (TyCon tcn) <- hole
  , Just tc <- UniqMap.lookup tcn tcm
  , let dcs = tyConDataCons tc
  , not (null dcs)
  = do dc <- Gen.element dcs
       args <- traverse (genKindFrom tcm env) (dcArgTys dc)
       pure (mkTyConApp (coerce (dcName dc)) args)

  -- Hole: Nat
  | aeqType hole typeNatKind
  = canGenDataKinds >>= \case
      True -> LitTy . NumTy . toInteger <$> Gen.word Range.linearBounded
      False -> error "genMonoKind: Cannot generate Nat without -XDataKinds"

  -- Hole: Symbol
  | aeqType hole typeSymbolKind
  = canGenDataKinds >>= \case
      True -> LitTy . SymTy <$> Gen.string (Range.linear 5 10) Gen.alphaNum
      False -> error "genMonoKind: Cannot generate Symbol without -XDataKinds"

  -- Hole: Type
  --
  -- As we shrink, it becomes more likely we just return Type for this hole.
  -- This rule is needed to prevent the generator recursing infinitely.
  | aeqType hole liftedTypeKind
  = let gen = genKindFrom tcm env liftedTypeKind
     in Gen.subterm2 gen gen mkFunTy

  | otherwise
  = error ("genMonoKind: Cannot generate fit for hole: " <> showPpr hole)

genFreshPolyType
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap TyVar
  -> Kind
  -> CoreGenT m Type
genFreshPolyType tcm env hole
  | ForAllTy tv kn <- hole
  = let env' = UniqMap.insertUnique tv env
     in Gen.subterm (genPolyTypeFrom tcm env' kn) (ForAllTy tv)

  | FunTy a b <- tyView hole
  = genForAll env a b (genPolyTypeFrom tcm)

  | aeqType hole liftedTypeKind
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
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap TyVar
  -> Kind
  -> CoreGenT m Type
genFreshMonoType tcm env hole
  | ConstTy (TyCon tcn) <- hole
  , Just tc <- UniqMap.lookup tcn tcm
  , let dcs = tyConDataCons tc
  , not (null dcs)
  = do dc <- Gen.element dcs
       args <- traverse (genMonoTypeFrom tcm env) (dcArgTys dc)
       pure (mkTyConApp (coerce (dcName dc)) args)

  | aeqType hole typeNatKind
  = canGenDataKinds >>= \case
      True -> LitTy . NumTy . toInteger <$> Gen.word Range.linearBounded
      False -> error "genFreshMonoType: Cannot generate Nat without -XDataKinds"

  | aeqType hole typeSymbolKind
  = canGenDataKinds >>= \case
      True -> LitTy . SymTy <$> Gen.string (Range.linear 5 10) Gen.alphaNum
      False -> error "genFreshMonoType: Cannot generate Symbol without -XDataKinds"

  | aeqType hole liftedTypeKind
  = let gen = genMonoTypeFrom tcm env liftedTypeKind
     in Gen.subterm2 gen gen mkFunTy

  | otherwise
  = error ("genFreshMonoType: Cannot generate fit for hole: " <> showPpr hole)
