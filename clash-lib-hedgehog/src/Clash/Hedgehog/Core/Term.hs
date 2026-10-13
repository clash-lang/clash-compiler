{-|
Copyright   : (C) 2021-2026, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random, type-directed generation of Term.

Terms are generated in an environment ('Env'): the scope they live in, and the
type variables and term variables in it that they may mention. Binders get
fresh names in that scope, so generated terms are well-scoped by construction.
'genClosedTerm' generates closed terms.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Hedgehog.Core.Term
  ( genTermFrom
  , genClosedTerm
  ) where

import Control.Monad (forM, replicateM)
import Data.Maybe (catMaybes)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Core.Basic (NameMod(..))
import Clash.Core.DataCon
import Clash.Core.Pretty (showPpr)
import Clash.Core.TyCon
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Subst (openTyBinder, patVars)
import Clash.Core.Scoped.Syntax hiding (Name)
import Clash.Core.Scoped.Term (mkTmApps)
import Clash.Core.Scoped.Type (TypeView(..), normalizeType, splitTyConAppM, tyView)
import Clash.Core.Scoped.TypeEnv (mkTypeEnv)
import Clash.Core.Scoped.TysPrim (liftedTypeKind, typeSymbolKind)
import Clash.Core.Scoped.Util (listToLets)

import Clash.Hedgehog.Core.Literal
import Clash.Hedgehog.Core.Monad
import Clash.Hedgehog.Core.Name
import Clash.Hedgehog.Core.Type
import Clash.Hedgehog.Core.Var
import Clash.Hedgehog.Unique

-- | Sample a data constructor from the environment, potentially partially
-- applying it so that the type fits the hole. If there are no possible fits
-- for the hole in the environment, an alternative generator is used instead.
sampleDataConOr
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -- ^ The types in scope while generating
  -> Type n
  -- ^ The hole to generate a fit for
  -> (Type n -> CoreGenT m (Term n))
  -- ^ A generator for sub-holes (used when partially applying a fit)
  -> CoreGenT m (Term n)
  -- ^ A generator to use if there are no hole fits
  -> CoreGenT m (Term n)
sampleDataConOr tcm hole genSub genOr =
  sampleDataCon <|> genOr
 where
  sampleDataCon = do
    -- TODO We cannot fill in any datacon where the type is polymorphic. This
    -- is because sampleList will never pick one because it cannot see the
    -- fit is valid without unification. See the TODO by sampleUniqMap.
    --
    -- We mitigate this by not generating algebraic type constructors with
    -- kinds other than Type, i.e. no type params / poly kinds. See the
    -- 'genAlgTyCon' function and NOTE [finding more complex fits].
    let dcs = concatMap tyConDataCons (UniqMap.elems tcm)
    (dc, holes) <- sampleList (sinkClosed . dcType) (const True) hole dcs
    holeFills <- traverse genSub holes

    pure (mkTmApps (Data dc) holeFills)

-- | Attempt to sample an identifier which can be made to fit a hole of the
-- desired type. If this is not possible (due to nothing in the environment
-- matching) then the given alternative generator is used instead.
--
sampleIdOr
  :: forall m n
   . (Alternative m, MonadGen m)
  => Env n
  -- ^ The currently bound type and term variables
  -> Type n
  -- ^ The hole to generate a fit for
  -> (Type n -> CoreGenT m (Term n))
  -- ^ A generator for sub-holes (used when partially applying a fit)
  -> CoreGenT m (Term n)
  -- ^ A generator to use if there are no hole fits
  -> CoreGenT m (Term n)
sampleIdOr env hole genSub genOr =
  sampleId <|> genOr
 where
  sampleId = do
    (i, holes) <- sampleList typeOfId (const True) hole (envTmVars env)
    holeFills <- traverse genSub holes

    pure (mkTmApps i holeFills)

-- | The type of a term variable
typeOfId :: Term n -> Type n
typeOfId = \case
  Var _ ty _ -> ty
  tm -> error ("typeOfId: not a variable: " <> showPpr tm)

-- | Generate a closed term that is valid for the given type constructor map.
-- The term generated must have the specified type.
--
genClosedTerm
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -- ^ The types in scope while generating
  -> Type 'VoidS
  -- ^ The type of the term being generated
  -> CoreGenT m (Term 'VoidS)
genClosedTerm tcm =
  genTermFrom tcm emptyEnv

-- | Generate a term that is valid for the given type constructor map and
-- environment of free type and term variables. The term generated must have
-- the specified type.
--
genTermFrom
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -- ^ The types in scope while generating
  -> Env n
  -- ^ The currently bound type and term variables
  -> Type n
  -- ^ The type of the term being generated
  -> CoreGenT m (Term n)
genTermFrom tcm env hole =
  let genSub = genTermFrom tcm env
      genOr = genFreshTerm tcm env hole
   in Gen.choice
        [ sampleDataConOr tcm hole genSub genOr
        , sampleIdOr env hole genSub genOr
        ]

{-
NOTE [generated terms]
~~~~~~~~~~~~~~~~~~~~~~
Term generation is currently limited in some ways. This is for no particular
reason other than to make the generator easier to understand. For example

  * when the hole is a forall or a function, a lambda (or tick) is inserted
    instead of allowing expressions like letrec or case which could still have
    the desired type

  * primitives are not currently generated, as we likely want to build a
    collection of known primitives when we build the environments before making
    types and terms for tests

  * casts are currently not generated, as the majority are discarded by Clash
    during the GHC2Core stage. See PR #1064.
-}

-- | Generate a "fresh" term, i.e. one which is randomly created according to
-- the type of the hole, rather than sampling from the known variables or data
-- constructors.
--
-- This generator will fail if there are no values for the given hole.
--
genFreshTerm
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Type n
  -> CoreGenT m (Term n)
genFreshTerm tcm env hole =
  -- We need to normalize in case the hole is a type family.
  --
  -- TODO When casts are supported, this will need a cast between the
  -- normalized type and the given type.
  case normalizeType (mkTypeEnv tcm) (envScope env) hole of
    -- Hole: forall i. a
    normHole@(ForAllTy i a) ->
      Gen.recursive Gen.choice
        [ openTyBinder (envScope env) i a $ \sc i' a' ->
            TyLam i' <$> genTermFrom tcm (extendEnv sc [tyBinderVar i'] [] env) a'
        ]
        [Tick <$> genTickInfo tcm <*> genFreshTerm tcm env normHole]

    AnnType _ a ->
      genTermFrom tcm env a

    normHole ->
      case tyView normHole of
        -- Hole: a -> b
        FunTy a b ->
          Gen.recursive Gen.choice
            [ genLocalId env a genVarName $ \env' i ->
                Lam i <$> genTermFrom tcm env' (sink b)
            ]
            [Tick <$> genTickInfo tcm <*> genFreshTerm tcm env normHole]

        -- Hole: Primitive type constructor.
        TyConApp tcn []
          |  Just PrimTyCon{} <- UniqMap.lookup tcn tcm
          -> Gen.recursive Gen.choice
               [Literal <$> genLiteralFrom normHole]
               -- We may fail to generate a case expression if there is nothing
               -- in scope to use as a subject. If this happens, let bindings
               -- are introduced so next time genCase is called it does not fail.
               [ genCase tcm env normHole <|> genLet tcm env normHole
               , genLet tcm env normHole
               ]

        -- Hole: Algebraic type constructor.
        TyConApp tcn _
          |  Just AlgTyCon{} <- UniqMap.lookup tcn tcm
          -- We may have got here by trying to fill the hole with an identifier, so
          -- it makes sense to try again. If we got here by sampleDataConOr, the
          -- data constructor is isomorphic to Void, and we will hit the error.
          -> Gen.recursive Gen.choice
               [sampleDataConOr tcm hole (genTermFrom tcm env)
                 (error ("No term level value for hole: " <> showPpr hole))]
               -- We may fail to generate a case expression if there is nothing
               -- in scope to use as a subject. If this happens, let bindings
               -- are introduced so next time genCase is called it does not fail.
               [ genCase tcm env normHole <|> genLet tcm env normHole
               , genLet tcm env normHole
               ]

        _ ->
          error ("No term level value for hole: " <> showPpr normHole)

-- TODO
-- genIsMultiPrim
-- genPrimInfo
-- genPrimUnfolding
-- genMultiPrimInfo
-- genWorkInfo

-- | Generate recursive let-bindings, split into strongly connected
-- components, around a term of the given type
genLet
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Type n
  -> CoreGenT m (Term n)
genLet tcm env hole = do
  -- Limit the number of new bindings to 8 to prevent an explosion in the
  -- number of sub-holes to generate.
  types <- Gen.list (Range.linear 1 8) (genMonoTypeFrom tcm env liftedTypeKind)

  genLocalIds env types genVarName $ \env' bs vars -> do
    rhss <- forM (zip vars (fmap sink types)) $ \(v, ty) ->
      -- Bindings can be indirectly recursive, but not directly recursive. This
      -- stops the generator from generating let x = x in ...
      let vars' = filter (not . sameVar v) vars
       in genTermFrom tcm (extendEnv (envScope env') [] vars' env) ty

    body <- genTermFrom tcm env' (sink hole)

    pure (listToLets (envScope env) bs rhss body)
 where
  sameVar (Var _ _ x) (Var _ _ y) = x == y
  sameVar _ _ = False

{-
NOTE [generating useful case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When generating case expressions, it is perfectly valid to generate something
like the following:

  case C x_1 ... x_n of
    C p_1 ... p_n -> ...

However, we want to favour generating the more interesting case expressions
where the subject is an application on variables, like so

  case v x_1 ... x_n of
    C p_1 ... p_i -> ...
    D p_1 ... p_j -> ...
    E p_1 ... p_k -> ...

Likewise, we want to avoid generating case expressions with literals as the
subject, or function types (where the only viable pattern is DefaultPat). While
these are valid terms to generate, the generator will naturally bias towards
them, making too many of the tests too "artificial" in nature.

A downside of this approach is that generating a case expression is not
guaranteed to work, as if there is nothing in the environment that can be used
as the subject then the generator will return 'empty'. However, since letrec
always passes and introduces new bindings, we fallback to this if case fails.
This means the failure will happen at most once when generating.
-}

genCase
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> Env n
  -> Type n
  -> CoreGenT m (Term n)
genCase tcm env altTy = do
  -- I need to select something as the subject. It can be any type, but should
  -- bias towards something from the environment where possible. It may not be
  -- possible though, so I should be able to fallback to just building a term.
  (subj, subjTy) <- sampleSubj

  case splitTyConAppM subjTy of
    Just (tcn, tyArgs) ->
      case UniqMap.lookup tcn tcm of
        Just tc@AlgTyCon{} -> do
          dcs <- Gen.subsequence (tyConDataCons tc)
          dcAlts <- catMaybes <$> traverse (genDataAltFrom tyArgs) dcs
          defAlt <- genAltFrom DefaultPat

          pure (Case subj altTy (defAlt : dcAlts))

        Just PrimTyCon{} -> do
          -- Upper bound is 8 to prevent explosion in number of sub-holes.
          lits <- Gen.list (Range.linear 0 8) (genLiteralFrom subjTy)
          alts <- traverse genAltFrom (DefaultPat : fmap LitPat lits)

          pure (Case subj altTy alts)

        _ -> do
          alt <- genAltFrom DefaultPat
          pure (Case subj altTy [alt])

    _ -> do
      alt <- genAltFrom DefaultPat
      pure (Case subj altTy [alt])
 where
  sc = envScope env

  -- Subjects are applications on variables in the environment.
  -- See NOTE [generating useful case expressions].
  sampleSubj :: CoreGenT m (Term n, Type n)
  sampleSubj = do
    (v, holes) <- sampleAnyList typeOfId (envTmVars env)
    holeFills <- traverse (genTermFrom tcm env) holes

    pure (mkTmApps v holeFills, snd (splitMonoFunTy (typeOfId v)))

  -- An alternative for a data constructor, instantiated at the type arguments
  -- of the subject. 'Nothing' if the data constructor can't be instantiated
  -- at them.
  genDataAltFrom :: [Type n] -> DataCon -> CoreGenT m (Maybe (Alt n))
  genDataAltFrom tyArgs dc = do
    names <- case dcSig dc of
      DataConSig _ _ argTys -> replicateM (length argTys) genVarName
    sequenceA $ withDataConPat (mkTypeEnv tcm) sc dc tyArgs names $ \sc' pat _ ->
      let (tvs, ids) = patVars pat
       in Alt pat <$> genTermFrom tcm (extendEnv sc' tvs ids env) (sink altTy)

  -- Generate the terms in alternatives with the newly bound vars in scope.
  genAltFrom :: Pat n n -> CoreGenT m (Alt n)
  genAltFrom pat = Alt pat <$> genTermFrom tcm env altTy

-- TODO genCast

genTickInfo
  :: forall m n
   . (Alternative m, MonadGen m, Distinct n)
  => TyConMap
  -> CoreGenT m (TickInfo n)
genTickInfo tcm =
  Gen.choice
    [ NameMod <$> genNameMod <*> (sinkClosed <$> genClosedKindFrom tcm typeSymbolKind)
    , Gen.constant DeDup
    , Gen.constant NoDeDup
    ]

genNameMod :: forall m. MonadGen m => m NameMod
genNameMod = Gen.element [PrefixName, SuffixName, SuffixNameP, SetName]
