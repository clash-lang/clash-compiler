{-|
  Copyright  :  (C) 2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Solving the type equations of the fields of data constructors, for their
  existentially quantified type variables, in scoped Clash Core.

  A solution gives a type variable a closed type: a type constructor or a
  number (see 'solveEq' and 'solveAdd'). Substituting a solution for an
  existentially quantified type variable keeps its binder, as a pattern has to
  bind all of them, but replaces its occurrences ('solvePatExistentials',
  'dataConInstArgTysE').
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Scoped.EqSolver
  ( -- * Solving equations
    TypeEqSolution (..)
  , catSolutions
  , solveNonAbsurds
  , solveEq
  , solveAdd
  , normalizeAdd
  , typeEq
  , isAbsurdEq
    -- * Patterns
  , withPatEqs
  , isAbsurdPat
  , solvePatExistentials
    -- * Data constructors
  , dataConInstArgTysE
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.List as List
import Data.List.Extra (zipEqual)
import Data.Maybe (mapMaybe)
import GHC.Stack (HasCallStack)

import GHC.Builtin.Names
  (eqPrimTyConKey, eqTyConKey, typeNatAddTyFamNameKey, unsafeReflDataConKey)

import Clash.Core.Basic (LitTy (..))
import Clash.Core.DataCon (DataCon (dcUniq), dcSig)
import Clash.Core.Name (Name (nameUniq))
import Clash.Unique (fromGhcUnique)

import Clash.Core.DataCon (DataConSig (..))
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Type
import Clash.Core.Scoped.TypeEnv (TypeEnv)

-- * Solving equations

-- | What kind of solution (if any) was found
data TypeEqSolution n
  = Solution !(TyName n) !(Type 'VoidS)
  -- ^ A solution was found: the variable equals a closed type
  | AbsurdSolution
  -- ^ A solution was found, but it involved negative naturals, or the
  -- equation equates different types
  | NoSolution
  -- ^ The type wasn't an equation, or it was unsolvable
  deriving (Show)

-- | The solutions
catSolutions :: [TypeEqSolution n] -> [(TyName n, Type 'VoidS)]
catSolutions = mapMaybe getSol
 where
  getSol (Solution tv ty) = Just (tv, ty)
  getSol _ = Nothing

-- | Solve the given equations for the given type variables, and return all
-- non-absurd solutions.
solveNonAbsurds
  :: Distinct n
  => TypeEnv
  -> Scope n
  -> NameSet TyName n
  -- ^ The type variables to solve for
  -> [(Type n, Type n)]
  -> [(TyName n, Type 'VoidS)]
solveNonAbsurds tenv sc solveSet = concatMap $ \eq ->
  catSolutions (solveAdd solveSet eq : solveEq tenv sc solveSet eq)

-- | Solve simple equalities such as:
--
--   * a ~ Int
--   * Int ~ a
--   * SomeType a b ~ SomeType Int Bool
--   * SomeType Int Bool ~ SomeType a b
--   * SomeType a Bool ~ SomeType Int b
solveEq
  :: Distinct n
  => TypeEnv
  -> Scope n
  -> NameSet TyName n
  -- ^ The type variables to solve for
  -> (Type n, Type n)
  -> [TypeEqSolution n]
solveEq tenv sc solveSet (coreView tenv sc -> left, coreView tenv sc -> right) =
  case (left, right) of
    (VarTy _ _ tv, ConstTy c) | memberNameSet tv solveSet ->
      -- a ~ Int
      [Solution tv (ConstTy c)]
    (ConstTy c, VarTy _ _ tv) | memberNameSet tv solveSet ->
      -- Int ~ a
      [Solution tv (ConstTy c)]
    (ConstTy c1, ConstTy c2) ->
      -- Int /= Char
      [AbsurdSolution | c1 /= c2]
    (LitTy l1, LitTy l2) ->
      -- 3 /= 5
      [AbsurdSolution | l1 /= l2]
    _ ->
      -- The call to 'coreView' at the start of 'solveEq' should have reduced
      -- all solvable type families. If we encounter one here that means the
      -- type family is stuck (and that we shouldn't compare it to anything!).
      if any (isTypeFamilyApplication tenv) [left, right] then
        []
      else
        case (tyView left, tyView right) of
          (TyConApp leftNm leftTys, TyConApp rightNm rightTys) ->
            -- SomeType a b ~ SomeType 3 5 (or other way around)
            if leftNm == rightNm then
              concatMap (solveEq tenv sc solveSet) (zipEqual leftTys rightTys)
            else
              [AbsurdSolution]
          _ ->
            []

-- | Solve equations supported by 'normalizeAdd'.
solveAdd
  :: NameSet TyName n
  -- ^ The type variables to solve for
  -> (Type n, Type n)
  -> TypeEqSolution n
solveAdd solveSet ab =
  case normalizeAdd ab of
    Just (n, m, VarTy _ _ tv) | memberNameSet tv solveSet ->
      if n >= 0 && m >= 0 && n - m >= 0 then
        Solution tv (LitTy (NumTy (n - m)))
      else
        AbsurdSolution
    _ ->
      NoSolution

-- | Given the left and right side of an equation, normalize it such that
-- equations of the following forms:
--
--     * 5     ~ n + 2
--     * 5     ~ 2 + n
--     * n + 2 ~ 5
--     * 2 + n ~ 5
--
-- are returned as (5, 2, n).
normalizeAdd :: (Type n, Type n) -> Maybe (Integer, Integer, Type n)
normalizeAdd (a, b) = do
  (n, rhs) <- lhsLit a b
  case tyView rhs of
    TyConApp tc [left, right]
      | nameUniq tc == fromGhcUnique typeNatAddTyFamNameKey -> do
      (m, o) <- lhsLit left right
      return (n, m, o)
    _ ->
      Nothing
 where
  lhsLit x (LitTy (NumTy n)) = Just (n, x)
  lhsLit (LitTy (NumTy n)) y = Just (n, y)
  lhsLit _ _ = Nothing

-- | If the type is an equation, its left-hand and right-hand side.
typeEq :: Distinct n => TypeEnv -> Scope n -> Type n -> Maybe (Type n, Type n)
typeEq tenv sc ty =
  case tyView (coreView tenv sc ty) of
    TyConApp tc [_, _, left, right]
      | nameUniq tc == fromGhcUnique eqPrimTyConKey ->
      Just (coreView tenv sc left, coreView tenv sc right)
    TyConApp tc [_, left, right]
      -- Lifted equality (~): @(~) :: forall k. k -> k -> Constraint@
      --
      -- Note that lifted equality exists to house "bogus" types, i.e. a
      -- deferred type error. Clash doesn't support that, so we can treat lifted
      -- equality (~) the same as unlifted equality (~#).
      | nameUniq tc == fromGhcUnique eqTyConKey ->
      Just (coreView tenv sc left, coreView tenv sc right)
    _ ->
      Nothing

-- | Whether an equation is absurd: it equates types that are definitely not
-- equal, or the solution involves an absurd (intermediate) result, such as
-- -1.
isAbsurdEq
  :: Distinct n
  => TypeEnv
  -> Scope n
  -> NameSet TyName n
  -- ^ The existentially quantified type variables
  -> (Type n, Type n)
  -> Bool
isAbsurdEq tenv sc exts (left0, right0) =
  case solveAdd exts lr of
    AbsurdSolution -> True
    _ -> any isAbsurd (solveEq tenv sc exts lr)
 where
  lr = (coreView tenv sc left0, coreView tenv sc right0)

  isAbsurd AbsurdSolution = True
  isAbsurd _ = False

-- * Patterns

-- | The equations of the field types of a pattern, in the scope of its
-- existentially quantified type variables, which are also given.
--
-- Goes under the existentially quantified type variables, renaming them if they
-- clash with the scope.
withPatEqs
  :: Distinct n
  => TypeEnv
  -> Scope n
  -> Pat n l
  -> (forall i. DExt n i => Scope i -> NameSet TyName i -> [(Type i, Type i)] -> r)
  -> r
withPatEqs tenv sc pat k = case pat of
  DataPat _ tvs ids ->
    openTyVars IntMap.empty (idSubst sc) tvs $ \s _ names ->
      let sc' = substScope s
      in k sc' (nameSetFromList names)
           (mapMaybe (typeEq tenv sc' . substTy s) (teleIdBinderTypes ids))
  LitPat _ -> k sc emptyNameSet []
  DefaultPat -> k sc emptyNameSet []

-- | Whether a pattern can never match, because its equations are absurd.
isAbsurdPat :: Distinct n => TypeEnv -> Scope n -> Pat n l -> Bool
isAbsurdPat _ _ (DataPat dc _ _)
  -- unsafeCoerce is not absurd in the way intended by 'isAbsurdPat'
  | dcUniq dc == fromGhcUnique unsafeReflDataConKey
  = False
isAbsurdPat tenv sc pat =
  withPatEqs tenv sc pat $ \sc' exts eqs -> any (isAbsurdEq tenv sc' exts) eqs

-- | Solve the equations of the field types of a pattern for its existentially
-- quantified type variables, and substitute the solutions. Gives the pattern,
-- whose binders still bind the solved type variables, and the substitution to
-- apply to the right-hand side of the alternative. 'Nothing' if nothing was
-- solved.
--
-- This solves the equations once: a solution may make other equations
-- solvable. See
-- 'Clash.Normalize.Scoped.Transformations.Case.elimExistentials'.
solvePatExistentials
  :: Distinct n
  => TypeEnv
  -> Scope n
  -> Pat n l
  -> (forall l'. DExt n l' => Subst l l' -> Pat n l' -> r)
  -> Maybe r
solvePatExistentials tenv sc pat k = case pat of
  DataPat dc tvs ids ->
    openTyVars IntMap.empty (idSubst sc) tvs $ \s0 _ names ->
      let sc0 = substScope s0
          eqs = mapMaybe (typeEq tenv sc0 . substTy s0) (teleIdBinderTypes ids)
      in case solveNonAbsurds tenv sc0 (nameSetFromList names) eqs of
           [] -> Nothing
           sols -> Just $
             openTyVars (solutionPositions names sols) (idSubst sc) tvs $ \s1 tvs' _ ->
               substTele substIdBinder s1 ids $ \s2 ids' ->
                 k s2 (DataPat dc tvs' ids')
  LitPat _ -> Nothing
  DefaultPat -> Nothing

-- * Data constructors

-- | The types of the fields of a data constructor, given types for its
-- universally quantified type variables, in the scope of fresh binders of its
-- existentially quantified type variables. Solves the equations of the field
-- types for the existentially quantified type variables (see
-- 'solveNonAbsurds'), until no more solutions are found, and substitutes the
-- solutions. 'Nothing' if there are more or fewer types than universally
-- quantified type variables.
dataConInstArgTysE
  :: forall n r
   . (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> DataCon
  -> [Type n]
  -- ^ Types for the universally quantified type variables
  -> (forall l. DExt n l => Scope l -> Tele TyBinder n l -> [Type l] -> r)
  -> Maybe r
dataConInstArgTysE tenv sc dc univTys k = case dcSig dc of
  DataConSig univ ext argTys -> do
    s1 <- extendTySubstTele (closedSubst sc) univ univTys
    pure $ openTyVars IntMap.empty s1 ext $ \s2 ext' names ->
      solveLoop (substScope s2) ext' names (map (substTy s2) argTys)
 where
  solveLoop
    :: DExt n o => Scope o -> Tele TyBinder n o -> [TyName o] -> [Type o] -> r
  solveLoop sc1 ext names tys =
    let eqs = mapMaybe (typeEq tenv sc1) tys in
    case solveNonAbsurds tenv sc1 (nameSetFromList names) eqs of
      [] -> k sc1 ext tys
      sols ->
        openTyVars (solutionPositions names sols) (idSubst sc) ext $ \s ext' names' ->
          solveLoop (substScope s) ext' names' (map (substTy s) tys)

-- * Substituting solutions

-- | The solutions, by the position of their type variable in the given list.
-- Later solutions win.
solutionPositions :: [TyName n] -> [(TyName n, Type 'VoidS)] -> IntMap (Type 'VoidS)
solutionPositions names sols = IntMap.fromList
  [ (pos, ty)
  | (tv, ty) <- sols
  , Just pos <- [List.elemIndex (tyNameRaw tv) raws] ]
 where
  raws = map tyNameRaw names

-- | Go under a telescope of type variable binders, renaming them if they clash
-- with the scope, and substituting the given solutions, by position, for the
-- type variables they bind. A binder whose type variable is solved is kept,
-- but nothing refers to it. Also gives the type variables the new binders
-- bind, in order.
openTyVars
  :: Distinct o
  => IntMap (Type 'VoidS)
  -> Subst x o
  -> Tele TyBinder x i
  -> (forall o'. DExt o o' => Subst i o' -> Tele TyBinder o o' -> [TyName o'] -> r)
  -> r
openTyVars sols = go 0 []
 where
  go
    :: Distinct o1
    => Int
    -> [TyName o1]
    -> Subst x1 o1
    -> Tele TyBinder x1 i1
    -> (forall o'. DExt o1 o' => Subst i1 o' -> Tele TyBinder o1 o' -> [TyName o'] -> r1)
    -> r1
  go !_ acc s TNil k = k s TNil (reverse acc)
  go !pos acc s (TCons b bs) k =
    substTyBinder s b $ \s1 b' ->
      let s2 = case IntMap.lookup pos sols of
            Nothing -> s1
            Just sol -> extendTySubst (extendSubstScope (substScope s1) s) b (sinkClosed sol)
      in go (pos + 1) (tyNameOf b' : sink1 acc) s2 bs $ \s' bs' names ->
           k s' (TCons b' bs') names
