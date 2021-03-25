{-|
  Copyright  :  (C) 2021 QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.EqSolver where

import Data.List.Extra (zipEqual)
import Data.Maybe (catMaybes, mapMaybe)

import Clash.Core.Name (Name(nameOcc))
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Var
import Clash.Core.VarEnv (VarSet, elemVarSet, emptyVarSet, mkVarSet)
#if MIN_VERSION_ghc(9,0,0)
import Clash.Core.DataCon (dcUniq)
import GHC.Builtin.Names (unsafeReflDataConKey)
import GHC.Types.Unique (getKey)
#endif

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
solveNonAbsurds :: TyConMap -> VarSet -> [(Type, Type)] -> [(TyVar, Type)]
solveNonAbsurds _tcm _ [] = []
solveNonAbsurds tcm solveSet (eq:eqs) =
  solved ++ solveNonAbsurds tcm solveSet eqs
 where
  solvers = [pure . solveAdd solveSet, solveEq tcm solveSet]
  solved = catSolutions (concat [s eq | s <- solvers])

-- | Solve simple equalities such as:
--
--   * a ~ 3
--   * 3 ~ a
--   * SomeType a b ~ SomeType 3 5
--   * SomeType 3 5 ~ SomeType a b
--   * SomeType a 5 ~ SomeType 3 b
--
solveEq :: TyConMap -> VarSet -> (Type, Type) -> [TypeEqSolution]
solveEq tcm solveSet (coreView tcm -> left, coreView tcm -> right) =
  case (left, right) of
    (VarTy tyVar, ConstTy {}) | elemVarSet tyVar solveSet ->
      -- a ~ 3
      [Solution (tyVar, right)]
    (ConstTy {}, VarTy tyVar) | elemVarSet tyVar solveSet ->
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
              concat (map (solveEq tcm solveSet) (zipEqual leftTys rightTys))
            else
              [AbsurdSolution]
          _ ->
            []

-- | Solve equations supported by @normalizeAdd@. See documentation of
-- @TypeEqSolution@ to understand the return value.
solveAdd
  :: VarSet
  -> (Type, Type)
  -> TypeEqSolution
solveAdd solveSet ab =
  case normalizeAdd ab of
    Just (n, m, VarTy tyVar) | elemVarSet tyVar solveSet ->
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

-- | Tests for nonsencical patterns due to types being "absurd". See
-- @isAbsurdEq@ for more info.
isAbsurdPat
  :: TyConMap
  -> Pat
  -> Bool
#if MIN_VERSION_base(4,15,0)
isAbsurdPat _tcm (DataPat dc _ _)
  -- unsafeCoerce is not absurd in the way intended by /isAbsurdPat/
  | dcUniq dc == getKey unsafeReflDataConKey
  = False
#endif
isAbsurdPat tcm pat =
  any (isAbsurdEq tcm exts) (patEqs tcm pat)
 where
  exts = case pat of
    DataPat _dc extNms _ids -> mkVarSet extNms
    _ -> emptyVarSet

-- | Determines if an "equation" obtained through @patEqs@ or @typeEq@ is
-- absurd. That is, it tests if two types that are definitely not equal are
-- asserted to be equal OR if the computation of the types yield some absurd
-- (intermediate) result such as -1.
isAbsurdEq
  :: TyConMap
  -> VarSet -- ^ existential tvs
  -> (Type, Type)
  -> Bool
isAbsurdEq tcm exts ((left0, right0)) =
  case (coreView tcm left0, coreView tcm right0) of
    (solveAdd exts -> AbsurdSolution) -> True
    lr -> any (==AbsurdSolution) (solveEq tcm exts lr)

-- | Get constraint equations
patEqs
  :: TyConMap
  -> Pat
  -> [(Type, Type)]
patEqs tcm pat =
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
