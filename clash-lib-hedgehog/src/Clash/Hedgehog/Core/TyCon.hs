{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of type constructors.
-}

{-# LANGUAGE LambdaCase #-}

module Clash.Hedgehog.Core.TyCon
  ( genTyConMap
  ) where

import Control.Monad (forM)
import Data.Coerce (coerce)
import Data.Either (rights)
import Hedgehog (Range)
import qualified Hedgehog.Gen as Gen

import Clash.Core.DataCon
import Clash.Core.HasType
import Clash.Core.Name (nameUniq)
import Clash.Core.Subst
import Clash.Core.TyCon
import Clash.Core.Type (Kind, Type(VarTy), mkTyConApp, splitFunForallTy)
import Clash.Core.TysPrim (liftedTypeKind, tysPrimMap)
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Data.UniqMap (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Hedgehog.Core.DataCon
import Clash.Hedgehog.Core.Monad
import Clash.Hedgehog.Core.Name
import Clash.Hedgehog.Core.Type
import Clash.Hedgehog.Core.Var
import Clash.Hedgehog.Unique

{-
Note [order of generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~
In Clash core (as with GHC core), there is a degree of circularity:

  * A type can be a type constructor (referenced by name)
  * A type constructor has a type (kind)
  * A data constructor has a type
  * A type constructor may contain data constructors

This makes it impossible to naively write a generator for these parts of the
core IR, as such a generator may never terminate. However not everything
generated is completely random, e.g. the codomain of a data constructor is
always the type constructor the data constructor belongs to.

A reasonable approximation of a real program can be made by first generating
an environment, then generating things which exist within that environment. To
that end, the first thing (typically) to generate is a TyConMap, which contains
information about all type constructors that exist. This leads to a sensible
order of generation, e.g.

  * generate a TyConMap, `tcm`
  * generate a kind that exists in `tcm`, `k`
  * generate a type of kind `k` that exists in `tcm`, `a`
  * generate a term of type `a` that exists in `tcm`

By generating the TyConMap first, the generation of complete programs becomes
a more manageable process of running increasingly more constrained generators.
This helps ensure that generated data is well-formed, as a constrained
generator produces random hole-fits instead of completely arbitrary values.
-}

arityOf :: Kind -> Int
arityOf = length . fst . splitFunForallTy

-- | A TyConMap contains all the algebraic data types and type families that
-- are used in a program. This is typically the first thing that should be
-- generated, as calls to other generators like @genKind@ or @genTypeFrom@ will
-- likely want to use the type constructors added to the TyConMap.
--
-- TODO It would be nice if this also included types from @clash-prelude@ like
-- Signal and the sized number types. Maybe we want to hook into @clash-ghc@
-- to load type constructors and primitives from @Clash.Prelude@.
--
genTyConMap
  :: forall m
   . (Alternative m, MonadGen m)
  => Range Int
  -> CoreGenT m TyConMap
genTyConMap numDcs = go tysPrimMap
 where
  -- Either stop adding new items to the TyConMap, or generate a new item.
  -- 'Gen.recursive' is necessary to ensure termination.
  go tcm =
    Gen.recursive Gen.choice
      [Gen.constant tcm]
      [Gen.subtermM (extendTyConMap tcm) go]

  extendTyConMap tcm = do
    -- We return new UniqMap instead of individual TyCon, because for AlgTyCon
    -- we may also generate PromotedDataCon for -XDataKinds.
    new <- canGenTypeFamilies >>= \case
      True -> Gen.choice
        [ genAlgTyConFrom numDcs tcm
        , genFunTyConFrom tcm <|> genAlgTyConFrom numDcs tcm
        ]
      False -> Gen.choice [genAlgTyConFrom numDcs tcm]

    pure (tcm <> new)

-- | Generate a new algebraic type constructor using the types that are already
-- in scope. This will also promote data constructors if the configuration
-- supports @-XDataKinds@.
--
genAlgTyConFrom
  :: forall m
   . (Alternative m, MonadGen m)
  => Range Int
  -> TyConMap
  -> CoreGenT m TyConMap
genAlgTyConFrom range tcm = do
  let used = fmap tyConUniq tcm
  name <- genFreshName used genTyConName

  -- TODO We want to use this, but we cannot sample polymorphic constructors
  -- when making Term / Type, so we avoid generating polymorphic data now.
  --
  -- See NOTE [finding more complex fits] in Clash.Hedgehog.Unique.
  --
  -- let argGen = genClosedKindFrom tcm liftedTypeKind
  -- kn  <- genWithCodomain liftedTypeKind argGen

  -- All ADTs live in the kind Type
  let kn = liftedTypeKind
  let arity = arityOf kn

  rhs <- Gen.choice
           [ DataTyCon <$> genDataConsFrom range tcm name kn
             -- TODO Generate NewTyCon
           ]

  let tc = AlgTyCon (nameUniq name) name kn arity rhs False

  canGenDataKinds >>= \case
    True ->
      -- Promote all the data constructors in the TyCon.
      let dcs = tyConDataCons tc
       in pure (UniqMap.fromList ((name, tc) : fmap promoteDataCon dcs))

    False ->
      pure (UniqMap.singleton name tc)
 where
   promoteDataCon dc =
     let tcn = coerce (dcName dc)
         arity = arityOf (dcType dc)
      in (tcn, PromotedDataCon (dcUniq dc) tcn (dcType dc) arity dc)

-- TODO In the future we may want to also generate indirectly recursive type
-- families. For example:
--
--   Even 0 = 'True         Odd 0 = 'False
--   Even n = Odd (n - 1)   Odd n = Even (n - 1)

-- | Generate a new type family, using the types that are already in scope.
--
genFunTyConFrom
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> CoreGenT m TyConMap
genFunTyConFrom tcm = do
  let used = fmap tyConUniq tcm
  name <- genFreshName used genTyConName

  kn <- genClosedKindFrom tcm liftedTypeKind
  let arity = arityOf kn

  let (argKns, resKn) = splitFunForallTy kn
  substs <- genSubsts name (rights argKns) resKn

  let tc = FunTyCon (nameUniq name) name kn arity substs
  pure (UniqMap.singleton name tc)
 where
  genSubsts :: TyConName -> [Kind] -> Kind -> CoreGenT m [([Type], Type)]
  genSubsts _ [] rhsKn = do
    -- Nullary type family, we only need to generate the RHS type
    let tcm' = UniqMap.filter (not . isPrimTc) tcm
    rhs <- genMonoTypeFrom tcm' mempty rhsKn

    pure [([], rhs)]

  genSubsts name argKns rhsKn = do
    let tcm' = UniqMap.filter (not . isPrimTc) tcm

    tvs <- genVars genTyVar argKns genVarName
    let acc = fmap (\x -> (UniqMap.singletonUnique x, VarTy x)) tvs

    lhss <- refineArgs tcm acc

    forM lhss $ \args -> do
      -- The RHS of each equation can use free vars in the types, since types
      -- in the LHS of a type family are treated more like patterns.
      let free = mconcat (fmap fst args)

      -- Direct recursion in type families requires -XUndecidableInstances.
      rhs <- canGenUndecidableInstances >>= \case
        True -> Gen.choice
                   [ genMonoTypeFrom tcm' free rhsKn
                   , mkTyConApp name
                       <$> traverse (genMonoTypeFrom tcm' free) argKns
                   ]
        False -> genMonoTypeFrom tcm' free rhsKn

      pure (fmap snd args, rhs)

refineArgs
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> [(UniqMap TyVar, Type)]
  -> m [[(UniqMap TyVar, Type)]]
refineArgs tcm args = go [args]
 where
  go acc =
    Gen.recursive Gen.choice
      [Gen.constant acc]
      [Gen.subtermM (refineAgain acc) go]

  refineAgain acc@(xs:_) = do
    -- Every arg can be refined or left alone.
    let gen x = Gen.choice [uncurry (refineArg tcm) x, Gen.constant x]
    refined <- traverse gen xs
    pure (refined : acc)

  refineAgain [] =
    error "refineArgs: No types to refine."

-- | Refine a type, selecting one of the free variables and substituting it
-- for a type constructor of the desired kind (filling in any holes with new
-- type variables). For example, successive calls may give
--
--   a ~> A b c ~> A (B b) c ~> A (B b) C ~> A (B D) C
--
refineArg
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap TyVar
  -> Type
  -> m (UniqMap TyVar, Type)
refineArg tcm free ty
  | UniqMap.null free
  = pure (free, ty)

  | otherwise
  = do -- Pick a free variable and remove it from free vars
       fv <- fst <$> sampleAnyUniqMap free
       let free' = UniqMap.delete fv free

       -- Pick a type constructor that fits that free variable. This cannot be
       -- an unboxed primitive type, so for now all primitive types are excluded.
       -- This is slightly too strict, as Integer and Natural can be used.
       (tc, holes) <- sampleUniqMapBiased (not . isPrimTc) (coreTypeOf fv) tcm

       -- Take any holes for that constructor and make them new free variables.
       holeVars <- genVars genTyVar holes genVarName
       let free'' = UniqMap.insertMany (zip holeVars holeVars) free'

       -- Substitute the removed free variable for the type constructor with
       -- any new free variables applied to it.
       let inScope = extendInScopeSetList emptyInScopeSet (UniqMap.elems free'')
       let substTv = unitVarEnv fv (mkTyConApp (tyConName tc) (fmap VarTy holeVars))
       let subst = mkTvSubst inScope substTv

       -- Return the refined type and free variable environment.
       pure (free'', substTy subst ty)

{-
Note [generating substs]
~~~~~~~~~~~~~~~~~~~~~~~~
When generating a FunTyCon, we generate a sequence of alternatives like so:

  type family F t_1 t_2 ... t_n :: k where
    F a_1 a_2 ... a_n = k_1
    F b_1 b_2 ... b_n = k_2
    ...

In "real" code, these would typically be written in a way where no alternatives
become dead, i.e. if you start with

  F v_1 v_2 ... v_n

where all arguments v_1 are type variables, this pattern will always be used
and the other alternatives never considered. To ensure FunTyCon are more
realistic, alternatives should be ordered from more specific to more general.

We can achieve this by starting from the most general solution and working
towards a more specific solution. At each step we can either return the list
of alternatives, or make a more specific alternative based on the previous
most specific alternative and put this at the head of the list. The returned
list is then guaranteed to not have any dead alternatives. Multiple of these
lists can be merged provided the merge operation preserves the ordering of
more to less specific.

If there is need to test type families where some alternatives may be dead, we
can use Gen.shuffle to rearrange the substs before taking a subsequence.
-}
