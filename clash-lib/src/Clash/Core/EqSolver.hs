{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.EqSolver where

import Data.List.Extra (zipEqual)
import Data.Maybe (catMaybes, mapMaybe)

import Clash.Core.Name (Name(nameOcc))
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Var

-- | Data type that indicates what kind of solution (if any) was found
data TypeEqSolution
  = Solution (TyVar, Type)
  -- ^ Solution was found. Variable equals some integer.
  | AbsurdSolution
  -- ^ A solution was found, but it involved negative naturals.
  | NoSolution
  -- ^ Given type wasn't an equation, or it was unsolvable.
    deriving (Show, Eq)

catSolutions :: [TypeEqSolution] -> [(TyVar, Type)]
catSolutions = mapMaybe getSol
 where
  getSol (Solution s) = Just s
  getSol _ = Nothing

-- | Solve given equations and return all non-absurd solutions
solveNonAbsurds :: TyConMap -> [(Type, Type)] -> [(TyVar, Type)]
solveNonAbsurds _tcm [] = []
solveNonAbsurds tcm (eq:eqs) =
  solved ++ solveNonAbsurds tcm eqs
 where
  solvers = [pure . solveAdd, solveEq tcm]
  solved = catSolutions (concat [s eq | s <- solvers])

-- | Solve simple equalities such as:
--
--   * a ~ 3
--   * 3 ~ a
--   * SomeType a b ~ SomeType 3 5
--   * SomeType 3 5 ~ SomeType a b
--   * SomeType a 5 ~ SomeType 3 b
--
solveEq :: TyConMap -> (Type, Type) -> [TypeEqSolution]
solveEq tcm (coreView tcm -> left, coreView tcm -> right) =
  case (left, right) of
    (VarTy tyVar, ConstTy {}) ->
      -- a ~ 3
      [Solution (tyVar, right)]
    (ConstTy {}, VarTy tyVar) ->
      -- 3 ~ a
      [Solution (tyVar, left)]
    (ConstTy {}, ConstTy {}) ->
      -- Int /= Char
      if left /= right then [AbsurdSolution] else []
    (LitTy {}, LitTy {}) ->
      -- 3 /= 5
      if left /= right then [AbsurdSolution] else []
    _ ->
      -- The call to 'coreView' at the start of 'solveEq' should have reduced
      -- all solvable type families. If we encounter one here that means the
      -- type family is stuck (and that we shouldn't compare it to anything!).
      if any (isTypeFamilyApplication tcm) [left, right] then
        []
      else
        case (tyView left, tyView right) of
          (TyConApp leftNm leftTys, TyConApp rightNm rightTys) ->
            -- SomeType a b ~ SomeType 3 5 (or other way around)
            if leftNm == rightNm then
              concat (map (solveEq tcm) (zipEqual leftTys rightTys))
            else
              [AbsurdSolution]
          _ ->
            []

-- | Solve equations supported by @normalizeAdd@. See documentation of
-- @TypeEqSolution@ to understand the return value.
solveAdd
  :: (Type, Type)
  -> TypeEqSolution
solveAdd ab =
  case normalizeAdd ab of
    Just (n, m, VarTy tyVar) ->
      if n >= 0 && m >= 0 && n - m >= 0 then
        Solution (tyVar, (LitTy (NumTy (n - m))))
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
-- are returned as (5, 2, n)
normalizeAdd
  :: (Type, Type)
  -> Maybe (Integer, Integer, Type)
normalizeAdd (a, b) = do
  (n, rhs) <- lhsLit a b
  case tyView rhs of
    TyConApp (nameOcc -> "GHC.TypeNats.+") [left, right] -> do
      (m, o) <- lhsLit left right
      return (n, m, o)
    _ ->
      Nothing
 where
  lhsLit x                 (LitTy (NumTy n)) = Just (n, x)
  lhsLit (LitTy (NumTy n)) y                 = Just (n, y)
  lhsLit _                 _                 = Nothing

-- | Tests for unreachable alternative due to types being "absurd". See
-- @isAbsurdEq@ for more info.
isAbsurdAlt
  :: TyConMap
  -> Alt
  -> Bool
isAbsurdAlt tcm alt =
  any (isAbsurdEq tcm) (altEqs tcm alt)

-- | Determines if an "equation" obtained through @altEqs@ or @typeEq@ is
-- absurd. That is, it tests if two types that are definitely not equal are
-- asserted to be equal OR if the computation of the types yield some absurd
-- (intermediate) result such as -1.
isAbsurdEq
  :: TyConMap
  -> (Type, Type)
  -> Bool
isAbsurdEq tcm ((left0, right0)) =
  case (coreView tcm left0, coreView tcm right0) of
    (solveAdd -> AbsurdSolution) -> True
    lr -> any (==AbsurdSolution) (solveEq tcm lr)

-- | Get constraint equations
altEqs
  :: TyConMap
  -> Alt
  -> [(Type, Type)]
altEqs tcm (pat, _term) =
 catMaybes (map (typeEq tcm . varType) (snd (patIds pat)))

-- | If type is an equation, return LHS and RHS.
typeEq
  :: TyConMap
  -> Type
  -> Maybe (Type, Type)
typeEq tcm ty =
 case tyView (coreView tcm ty) of
  TyConApp (nameOcc -> "GHC.Prim.~#") [_, _, left, right] ->
    Just (coreView tcm left, coreView tcm right)
  _ ->
    Nothing
