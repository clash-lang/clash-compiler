{-|
Copyright   : (C) 2021-2026, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of type constructors.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Clash.Hedgehog.Core.TyCon
  ( genTyConMap
  ) where

import Control.Monad (forM)
import Data.Coerce (coerce)
import qualified Data.IntMap.Strict as IntMap
import Hedgehog (Range)
import qualified Hedgehog.Gen as Gen

import Clash.Core.DataCon
import Clash.Core.TyCon
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Syntax hiding (Name)
import Clash.Core.Type
  (SplitFunForall(..), mkTyConApp, piTeleTmArity, piTeleTyArity, splitFunForallTy)
import Clash.Core.TysPrim (liftedTypeKind, tysPrimMap)

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

arityOf :: Kind n -> Int
arityOf kn = case splitFunForallTy kn of
  SplitFunForall tele _ -> piTeleTyArity tele + piTeleTmArity tele

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
  -- kn  <- genWithCodomain emptyEnv liftedTypeKind argGen

  -- All ADTs live in the kind Type
  let kn = liftedTypeKind
  let arity = arityOf kn

  rhs <- Gen.choice
           [ DataTyCon <$> genDataConsFrom range tcm name kn
             -- TODO Generate NewTyCon
           ]

  let tc = mkAlgTyCon name kn arity rhs False

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
      in (tcn, mkPromotedDataCon tcn (dcType dc) (arityOf (dcType dc)) dc)

-- TODO In the future we may want to also generate indirectly recursive type
-- families. For example:
--
--   Even 0 = 'True         Odd 0 = 'False
--   Even n = Odd (n - 1)   Odd n = Even (n - 1)

-- | Generate a new type family, using the types that are already in scope.
--
-- The kind of the type family is monomorphic, even with @-XPolyKinds@, so
-- that the pattern variables of its equations have closed kinds.
--
genFunTyConFrom
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> CoreGenT m TyConMap
genFunTyConFrom tcm = do
  let used = fmap tyConUniq tcm
  name <- genFreshName used genTyConName

  kn <- local (\r -> r { allowPolyKinds = False })
          (genClosedKindFrom tcm liftedTypeKind)
  let (argKns, resKn) = splitMonoFunTy kn

  eqns <- genEqns name argKns resKn

  let tc = mkFunTyCon name kn (length argKns) eqns
  pure (UniqMap.singleton name tc)
 where
  tcm' = UniqMap.filter (not . isPrimTc) tcm

  genEqns :: TyConName -> [Kind 'VoidS] -> Kind 'VoidS -> CoreGenT m [TyFamEqn]
  genEqns _ [] rhsKn = do
    -- Nullary type family, we only need to generate the RHS type
    rhs <- genMonoTypeFrom tcm' emptyEnv rhsKn

    pure [TyFamEqn TNil [] rhs]

  genEqns name argKns rhsKn = do
    let args = zipWith (\i k -> ([(i, k)], PVar i)) [0..] argKns
    lhss <- refineArgs tcm (length argKns) args

    forM lhss $ \lhs ->
      -- The RHS of each equation can use the pattern variables, since types
      -- in the LHS of a type family are treated more like patterns.
      let free = concatMap fst lhs
       in genTyVars emptyEnv (map (sinkClosed . snd) free) genVarName $ \env tele vs -> do
            let vars = IntMap.fromList (zip (map fst free) vs)
                pats = fmap (patType vars . snd) lhs

            -- Direct recursion in type families requires -XUndecidableInstances.
            rhs <- canGenUndecidableInstances >>= \case
              True -> Gen.choice
                         [ genMonoTypeFrom tcm' env (sinkClosed rhsKn)
                         , mkTyConApp name
                             <$> traverse (genMonoTypeFrom tcm' env . sinkClosed) argKns
                         ]
              False -> genMonoTypeFrom tcm' env (sinkClosed rhsKn)

            pure (TyFamEqn tele pats rhs)

-- | A pattern of the left-hand side of a type family equation, whose variables
-- are numbered
data PatTy
  = PVar !Int
  | PCon !TyConName [PatTy]

-- | Substitute a pattern for a pattern variable
substPat :: Int -> PatTy -> PatTy -> PatTy
substPat v p = go
 where
  go = \case
    PVar w | v == w -> p
    PCon tc ps -> PCon tc (fmap go ps)
    q -> q

-- | A pattern as a type, given the pattern variables as types
patType :: IntMap.IntMap (Type n) -> PatTy -> Type n
patType vars = \case
  PVar v -> IntMap.findWithDefault (error ("patType: unbound pattern variable " <> show v)) v vars
  PCon tc ps -> mkTyConApp tc (fmap (patType vars) ps)

-- | A pattern of a type family equation, with its pattern variables and their
-- closed kinds
type PatArg = ([(Int, Kind 'VoidS)], PatTy)

refineArgs
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> Int
  -- ^ The next number of a pattern variable
  -> [PatArg]
  -> m [[PatArg]]
refineArgs tcm next0 args = snd <$> go (next0, [args])
 where
  go acc =
    Gen.recursive Gen.choice
      [Gen.constant acc]
      [Gen.subtermM (refineAgain acc) go]

  refineAgain (next, acc@(xs:_)) = do
    -- Every arg can be refined or left alone.
    let gen nxt x = Gen.choice [refineArg tcm nxt x, Gen.constant (nxt, x)]
    (next', refined) <- mapAccumLM gen next xs
    pure (next', refined : acc)

  refineAgain (_, []) =
    error "refineArgs: No types to refine."

mapAccumLM
  :: forall m acc x y
   . Monad m
  => (acc -> x -> m (acc, y))
  -> acc
  -> [x]
  -> m (acc, [y])
mapAccumLM _ acc [] = return (acc, [])
mapAccumLM f acc (x:xs) = do
  (acc', y) <- f acc x
  (acc'', ys) <- mapAccumLM f acc' xs
  return (acc'', y:ys)

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
  -> Int
  -- ^ The next number of a pattern variable
  -> PatArg
  -> m (Int, PatArg)
refineArg tcm next (free, ty)
  | null free
  = pure (next, (free, ty))

  | otherwise
  = do -- Pick a free variable and remove it from free vars
       (fv, fvKind) <- Gen.element free
       let free' = filter ((/= fv) . fst) free

       -- Pick a type constructor that fits that free variable. This cannot be
       -- an unboxed primitive type, so for now all primitive types are excluded.
       -- This is slightly too strict, as Integer and Natural can be used.
       (tc, holes) <- sampleUniqMapBiased tyConKind (not . isPrimTc) fvKind tcm

       -- Take any holes for that constructor and make them new free variables.
       let holeVars = zip [next..] holes
       let free'' = free' <> holeVars

       -- Substitute the removed free variable for the type constructor with
       -- any new free variables applied to it.
       let ty' = substPat fv (PCon (tyConName tc) (fmap (PVar . fst) holeVars)) ty

       -- Return the refined type and free variable environment.
       pure (next + length holes, (free'', ty'))

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
