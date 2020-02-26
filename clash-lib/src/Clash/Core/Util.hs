{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Smart constructor and destructor functions for CoreHW
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Util where

import           Control.Concurrent.Supply     (Supply, freshId)
import qualified Control.Lens                  as Lens
import Control.Monad.Trans.Except              (Except, throwE)
import           Data.Coerce                   (coerce)
import qualified Data.HashSet                  as HashSet
import qualified Data.Graph                    as Graph
import Data.List
  (foldl', mapAccumR, elemIndices, nub)
import Data.Maybe
  (fromJust, isJust, mapMaybe, catMaybes)
import qualified Data.Set                      as Set
import qualified Data.Set.Lens                 as Lens
import qualified Data.String.Interpolate       as I
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (line)
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

import           PrelNames               (ipClassKey)
import           Unique                  (getKey)

import Clash.Core.DataCon
  (DataCon (MkData), dcType, dcUnivTyVars, dcExtTyVars, dcArgTys)
import Clash.Core.FreeVars
  (tyFVsOfTypes, typeFreeVars, freeLocalIds)
import Clash.Core.Literal                      (literalType)
import Clash.Core.Name
  (Name (..), OccName, mkUnsafeInternalName, mkUnsafeSystemName)
import Clash.Core.Pretty                       (ppr, showPpr)
import Clash.Core.Subst
  (extendTvSubst, mkSubst, mkTvSubst, substTy, substTyWith,
   substTyInVar, extendTvSubstList, aeqType)
import Clash.Core.Term
  (LetBinding, Pat (..), PrimInfo (..), Term (..), Alt, WorkInfo (..),
   TickInfo (..), collectArgs)
import Clash.Core.Type
  (Kind, LitTy (..), Type (..), TypeView (..),
   coreView, coreView1, isFunTy, isPolyFunCoreTy, mkFunTy, splitFunTy, tyView,
   undefinedTy, isTypeFamilyApplication)
import Clash.Core.TyCon
  (TyConMap, tyConDataCons)
import Clash.Core.TysPrim                      (typeNatKind)
import Clash.Core.Var
  (Id, TyVar, Var (..), isLocalId, mkLocalId, mkTyVar)
import Clash.Core.VarEnv
  (InScopeSet, VarEnv, emptyVarEnv, extendInScopeSet, extendVarEnv,
   lookupVarEnv, mkInScopeSet, uniqAway, extendInScopeSetList, unionInScope,
   mkVarSet, unionVarSet, unitVarSet, emptyVarSet)
import Clash.Unique
import Clash.Util

-- | Type environment/context
type Gamma = VarEnv Type
-- | Kind environment/context
type Delta = VarEnv Kind

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
              concat (map (solveEq tcm) (zip leftTys rightTys))
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

-- | Get constraint equations
altEqs
  :: TyConMap
  -> Alt
  -> [(Type, Type)]
altEqs tcm (pat, _term) =
 catMaybes (map (typeEq tcm . varType) (snd (patIds pat)))

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

-- Safely substitute global type variables in a list of potentially
-- shadowing type variables.
substGlobalsInExistentials
  :: HasCallStack
  => InScopeSet
  -- ^ Variables in scope
  -> [TyVar]
  -- ^ List of existentials to apply the substitution for
  -> [(TyVar, Type)]
  -- ^ Substitutions
  -> [TyVar]
substGlobalsInExistentials is exts substs0 = result
  -- TODO: Is is actually possible that existentials shadow each other? If they
  -- TODO: can't, we can remove this function
  where
    iss     = scanl extendInScopeSet is exts
    substs1 = map (\is_ -> extendTvSubstList (mkSubst is_) substs0) iss
    result  = zipWith substTyInVar substs1 exts

-- | Safely substitute type variables in a list of existentials. This function
-- will account for cases where existentials shadow each other.
substInExistentialsList
  :: HasCallStack
  => InScopeSet
  -- ^ Variables in scope
  -> [TyVar]
  -- ^ List of existentials to apply the substitution for
  -> [(TyVar, Type)]
  -- ^ Substitutions
  -> [TyVar]
substInExistentialsList is exts substs =
  foldl (substInExistentials is) exts substs

-- | Safely substitute a type variable in a list of existentials. This function
-- will account for cases where existentials shadow each other.
substInExistentials
  :: HasCallStack
  => InScopeSet
  -- ^ Variables in scope
  -> [TyVar]
  -- ^ List of existentials to apply the substitution for
  -> (TyVar, Type)
  -- ^ Substitution
  -> [TyVar]
substInExistentials is exts subst@(typeVar, _type) =
  -- TODO: Is is actually possible that existentials shadow each other? If they
  -- TODO: can't, we can remove this function
  case elemIndices typeVar exts of
    [] ->
      -- We're not replacing any of the existentials, but a global variable
      substGlobalsInExistentials is exts [subst]
    (last -> i) ->
      -- We're replacing an existential. That means we're not touching any
      -- variables that were introduced before it. For all variables after it,
      -- it is as we would replace global variables in them.
      take (i+1) exts ++ substGlobalsInExistentials is (drop (i+1) exts) [subst]

-- | Determine the type of a term
termType
  :: TyConMap
  -> Term
  -> Type
termType m e = case e of
  Var t          -> varType t
  Data dc        -> dcType dc
  Literal l      -> literalType l
  Prim t         -> primType t
  Lam v e'       -> mkFunTy (varType v) (termType m e')
  TyLam tv e'    -> ForAllTy tv (termType m e')
  App _ _        -> case collectArgs e of
                      (fun, args) -> applyTypeToArgs e m (termType m fun) args
  TyApp _ _      -> case collectArgs e of
                      (fun, args) -> applyTypeToArgs e m (termType m fun) args
  Letrec _ e'    -> termType m e'
  Case _ ty _    -> ty
  Cast _ _ ty2   -> ty2
  Tick _ e'      -> termType m e'

-- | Split a (Type)Abstraction in the bound variables and the abstracted term
collectBndrs :: Term
             -> ([Either Id TyVar], Term)
collectBndrs = go []
  where
    go bs (Lam v e')    = go (Left v:bs) e'
    go bs (TyLam tv e') = go (Right tv:bs) e'
    go bs e'            = (reverse bs,e')

-- | Get the result type of a polymorphic function given a list of arguments
applyTypeToArgs
  :: Term
  -> TyConMap
  -> Type
  -> [Either Term Type]
  -> Type
applyTypeToArgs e m opTy args = go opTy args
 where
  go opTy' []               = opTy'
  go opTy' (Right ty:args') = goTyArgs opTy' [ty] args'
  go opTy' (Left _:args')   = case splitFunTy m opTy' of
    Just (_,resTy) -> go resTy args'
    _ -> error $ unlines ["applyTypeToArgs:"
                         ,"Expression: " ++ showPpr e
                         ,"Type: " ++ showPpr opTy
                         ,"Args: " ++ unlines (map (either showPpr showPpr) args)
                         ]

  goTyArgs opTy' revTys (Right ty:args') = goTyArgs opTy' (ty:revTys) args'
  goTyArgs opTy' revTys args'            = go (piResultTys m opTy' (reverse revTys)) args'

-- | Like 'piResultTyMaybe', but errors out when a type application is not
-- valid.
--
-- Do not iterate 'piResultTy', because it's inefficient to substitute one
-- variable at a time; instead use 'piResultTys'
piResultTy
  :: HasCallStack
  => TyConMap
  -> Type
  -> Type
  -> Type
piResultTy m ty arg = case piResultTyMaybe m ty arg of
  Just res -> res
  Nothing  -> pprPanic "piResultTy" (ppr ty <> line <> ppr arg)

-- | Like 'piResultTys' but for a single argument.
--
-- Do not iterate 'piResultTyMaybe', because it's inefficient to substitute one
-- variable at a time; instead use 'piResultTys'
piResultTyMaybe
  :: HasCallStack
  => TyConMap
  -> Type
  -> Type
  -> Maybe Type
piResultTyMaybe m ty arg
  | Just ty' <- coreView1 m ty
  = piResultTyMaybe m ty' arg
  | FunTy a res <- tyView ty
  = if debugIsOn && not (aeqType a arg) then error [I.i|
      Unexpected application. A function with type:

        #{showPpr ty}

      Got applied to an argument of type:

        #{showPpr arg}
    |]
    else
      Just res
  | ForAllTy tv res <- ty
  = let emptySubst = mkSubst (mkInScopeSet (tyFVsOfTypes [arg,res]))
    in  Just (substTy (extendTvSubst emptySubst tv arg) res)
  | otherwise
  = Nothing

-- | @(piResultTys f_ty [ty1, ..., tyn])@ give sthe type of @(f ty1 .. tyn)@
-- where @f :: f_ty@
--
-- 'piResultTys' is interesting because:
--
--    1. 'f_ty' may have more foralls than there are args
--    2. Less obviously, it may have fewer foralls
--
-- Fore case 2. think of:
--
--   piResultTys (forall a . a) [forall b.b, Int]
--
-- This really can happen, such as situations involving 'undefined's type:
--
--   undefined :: forall a. a
--
--   undefined (forall b. b -> b) Int
--
-- This term should have the type @(Int -> Int)@, but notice that there are
-- more type args than foralls in 'undefined's type.
--
-- For efficiency reasons, when there are no foralls, we simply drop arrows from
-- a function type/kind.
piResultTys
  :: HasCallStack
  => TyConMap
  -> Type
  -> [Type]
  -> Type
piResultTys _ ty [] = ty
piResultTys m ty origArgs@(arg:args)
  | Just ty' <- coreView1 m ty
  = piResultTys m ty' origArgs
  | FunTy a res <- tyView ty
  = if debugIsOn && not (aeqType a arg) then error [I.i|
      Unexpected application. A function with type:

        #{showPpr ty}

      Got applied to an argument of type:

        #{showPpr arg}
    |]
    else
      piResultTys m res args
  | ForAllTy tv res <- ty
  = go (extendVarEnv tv arg emptyVarEnv) res args
  | otherwise
  = pprPanic "piResultTys1" (ppr ty <> line <> ppr origArgs)
 where
  inScope = mkInScopeSet (tyFVsOfTypes (ty:origArgs))

  go env ty' [] = substTy (mkTvSubst inScope env) ty'
  go env ty' allArgs@(arg':args')
    | Just ty'' <- coreView1 m ty'
    = go env ty'' allArgs
    | FunTy _ res <- tyView ty'
    = go env res args'
    | ForAllTy tv res <- ty'
    = go (extendVarEnv tv arg' env) res args'
    | VarTy tv <- ty'
    , Just ty'' <- lookupVarEnv tv env
      -- Deals with (piResultTys  (forall a.a) [forall b.b, Int])
    = piResultTys m ty'' allArgs
    | otherwise
    = pprPanic "piResultTys2" (ppr ty' <> line <> ppr origArgs <> line <> ppr allArgs)

-- | Get the list of term-binders out of a DataType pattern
patIds :: Pat -> ([TyVar],[Id])
patIds (DataPat _  tvs ids) = (tvs,ids)
patIds _                    = ([],[])

patVars :: Pat -> [Var a]
patVars (DataPat _ tvs ids) = coerce tvs ++ coerce ids
patVars _ = []

-- | Abstract a term over a list of term and type variables
mkAbstraction :: Term
              -> [Either Id TyVar]
              -> Term
mkAbstraction = foldr (either Lam TyLam)

-- | Abstract a term over a list of term variables
mkTyLams :: Term
         -> [TyVar]
         -> Term
mkTyLams tm = mkAbstraction tm . map Right

-- | Abstract a term over a list of type variables
mkLams :: Term
       -> [Id]
       -> Term
mkLams tm = mkAbstraction tm . map Left

-- | Apply a list of types and terms to a term
mkApps :: Term
       -> [Either Term Type]
       -> Term
mkApps = foldl' (\e a -> either (App e) (TyApp e) a)

-- | Apply a list of terms to a term
mkTmApps :: Term
         -> [Term]
         -> Term
mkTmApps = foldl' App

-- | Apply a list of types to a term
mkTyApps :: Term
         -> [Type]
         -> Term
mkTyApps = foldl' TyApp

mkTicks
  :: Term
  -> [TickInfo]
  -> Term
mkTicks tm ticks = foldl' (\e s -> Tick s e) tm (nub ticks)

-- | Does a term have a function type?
isFun :: TyConMap
      -> Term
      -> Bool
isFun m t = isFunTy m (termType m t)

-- | Does a term have a function or polymorphic type?
isPolyFun :: TyConMap
          -> Term
          -> Bool
isPolyFun m t = isPolyFunCoreTy m (termType m t)

-- | Is a term a term-abstraction?
isLam :: Term
      -> Bool
isLam (Lam {}) = True
isLam _        = False

-- | Is a term a recursive let-binding?
isLet :: Term
      -> Bool
isLet (Letrec {}) = True
isLet _           = False

-- | Is a term a variable reference?
isVar :: Term
      -> Bool
isVar (Var {}) = True
isVar _        = False

isLocalVar
  :: Term
  -> Bool
isLocalVar (Var v) = isLocalId v
isLocalVar _ = False

-- | Is a term a datatype constructor?
isCon :: Term
      -> Bool
isCon (Data {}) = True
isCon _         = False

-- | Is a term a primitive?
isPrim :: Term
       -> Bool
isPrim (Prim {}) = True
isPrim _         = False

-- | Make variable reference out of term variable
idToVar :: Id
        -> Term
idToVar i@(Id {}) = Var i
idToVar tv        = error $ $(curLoc) ++ "idToVar: tyVar: " ++ showPpr tv

-- | Make a term variable out of a variable reference
varToId :: Term
        -> Id
varToId (Var i) = i
varToId e       = error $ $(curLoc) ++ "varToId: not a var: " ++ showPpr e

termSize :: Term
         -> Word
termSize (Var {})     = 1
termSize (Data {})    = 1
termSize (Literal {}) = 1
termSize (Prim {})    = 1
termSize (Lam _ e)    = termSize e + 1
termSize (TyLam _ e)  = termSize e
termSize (App e1 e2)  = termSize e1 + termSize e2
termSize (TyApp e _)  = termSize e
termSize (Cast e _ _) = termSize e
termSize (Tick _ e)   = termSize e
termSize (Letrec bndrs e) = sum (bodySz:bndrSzs)
 where
  bndrSzs = map (termSize . snd) bndrs
  bodySz  = termSize e
termSize (Case subj _ alts) = sum (subjSz:altSzs)
 where
  subjSz = termSize subj
  altSzs = map (termSize . snd) alts

-- | Create a vector of supplied elements
mkVec :: DataCon -- ^ The Nil constructor
      -> DataCon -- ^ The Cons (:>) constructor
      -> Type    -- ^ Element type
      -> Integer -- ^ Length of the vector
      -> [Term]  -- ^ Elements to put in the vector
      -> Term
mkVec nilCon consCon resTy = go
  where
    go _ [] = mkApps (Data nilCon) [Right (LitTy (NumTy 0))
                                   ,Right resTy
                                   ,Left  (primCo nilCoTy)
                                   ]

    go n (x:xs) = mkApps (Data consCon) [Right (LitTy (NumTy n))
                                        ,Right resTy
                                        ,Right (LitTy (NumTy (n-1)))
                                        ,Left (primCo (consCoTy n))
                                        ,Left x
                                        ,Left (go (n-1) xs)]

    nilCoTy    = head (fromJust $! dataConInstArgTys nilCon  [(LitTy (NumTy 0))
                                                             ,resTy])
    consCoTy n = head (fromJust $! dataConInstArgTys consCon
                                                     [(LitTy (NumTy n))
                                                     ,resTy
                                                     ,(LitTy (NumTy (n-1)))])

-- | Append elements to the supplied vector
appendToVec :: DataCon -- ^ The Cons (:>) constructor
            -> Type    -- ^ Element type
            -> Term    -- ^ The vector to append the elements to
            -> Integer -- ^ Length of the vector
            -> [Term]  -- ^ Elements to append
            -> Term
appendToVec consCon resTy vec = go
  where
    go _ []     = vec
    go n (x:xs) = mkApps (Data consCon) [Right (LitTy (NumTy n))
                                        ,Right resTy
                                        ,Right (LitTy (NumTy (n-1)))
                                        ,Left (primCo (consCoTy n))
                                        ,Left x
                                        ,Left (go (n-1) xs)]

    consCoTy n = head (fromJust $! dataConInstArgTys consCon
                                                   [(LitTy (NumTy n))
                                                   ,resTy
                                                   ,(LitTy (NumTy (n-1)))])

-- | Create let-bindings with case-statements that select elements out of a
-- vector. Returns both the variables to which element-selections are bound
-- and the let-bindings
extractElems
  :: Supply
  -- ^ Unique supply
  -> InScopeSet
  -- ^ (Superset of) in scope variables
  -> DataCon
  -- ^ The Cons (:>) constructor
  -> Type
  -- ^ The element type
  -> Char
  -- ^ Char to append to the bound variable names
  -> Integer
  -- ^ Length of the vector
  -> Term
  -- ^ The vector
  -> (Supply, [(Term,[LetBinding])])
extractElems supply inScope consCon resTy s maxN vec =
  first fst (go maxN (supply,inScope) vec)
 where
  go :: Integer -> (Supply,InScopeSet) -> Term
     -> ((Supply,InScopeSet),[(Term,[LetBinding])])
  go 0 uniqs _ = (uniqs,[])
  go n uniqs0 e =
    (uniqs3,(elNVar,[(elNId, lhs),(restNId, rhs)]):restVs)
   where
    tys = [(LitTy (NumTy n)),resTy,(LitTy (NumTy (n-1)))]
    (Just idTys) = dataConInstArgTys consCon tys
    restTy       = last idTys

    (uniqs1,mTV) = mkUniqSystemTyVar uniqs0 ("m",typeNatKind)
    (uniqs2,[elNId,restNId,co,el,rest]) =
      mapAccumR mkUniqSystemId uniqs1 $ zip
        ["el" `T.append` (s `T.cons` T.pack (show (maxN-n)))
        ,"rest" `T.append` (s `T.cons` T.pack (show (maxN-n)))
        ,"_co_"
        ,"el"
        ,"rest"
        ]
        (resTy:restTy:idTys)

    elNVar    = Var elNId
    pat       = DataPat consCon [mTV] [co,el,rest]
    lhs       = Case e resTy  [(pat,Var el)]
    rhs       = Case e restTy [(pat,Var rest)]

    (uniqs3,restVs) = go (n-1) uniqs2 (Var restNId)

-- | Create let-bindings with case-statements that select elements out of a
-- tree. Returns both the variables to which element-selections are bound
-- and the let-bindings
extractTElems
  :: Supply
  -- ^ Unique supply
  -> InScopeSet
  -- ^ (Superset of) in scope variables
  -> DataCon
  -- ^ The 'LR' constructor
  -> DataCon
  -- ^ The 'BR' constructor
  -> Type
  -- ^ The element type
  -> Char
  -- ^ Char to append to the bound variable names
  -> Integer
  -- ^ Depth of the tree
  -> Term
  -- ^ The tree
  -> (Supply,([Term],[LetBinding]))
extractTElems supply inScope lrCon brCon resTy s maxN tree =
  first fst (go maxN [0..(2^(maxN+1))-2] [0..(2^maxN - 1)] (supply,inScope) tree)
 where
  go :: Integer
     -> [Int]
     -> [Int]
     -> (Supply,InScopeSet)
     -> Term
     -> ((Supply,InScopeSet),([Term],[LetBinding]))
  go 0 _ ks uniqs0 e = (uniqs1,([elNVar],[(elNId, rhs)]))
   where
    tys          = [LitTy (NumTy 0),resTy]
    (Just idTys) = dataConInstArgTys lrCon tys

    (uniqs1,[elNId,co,el]) =
      mapAccumR mkUniqSystemId uniqs0 $ zip
        [ "el" `T.append` (s `T.cons` T.pack (show (head ks)))
        , "_co_"
        , "el"
        ]
        (resTy:idTys)
    elNVar = Var elNId
    pat    = DataPat lrCon [] [co,el]
    rhs    = Case e resTy [(pat,Var el)]

  go n bs ks uniqs0 e =
    (uniqs4
    ,(lVars ++ rVars,(ltNId, ltRhs):
                     (rtNId, rtRhs):
                     (lBinds ++ rBinds)))
   where
    tys = [LitTy (NumTy n),resTy,LitTy (NumTy (n-1))]
    (Just idTys) = dataConInstArgTys brCon tys

    (uniqs1,mTV) = mkUniqSystemTyVar uniqs0 ("m",typeNatKind)
    (b0:bL,b1:bR) = splitAt (length bs `div` 2) bs
    brTy = last idTys
    (uniqs2,[ltNId,rtNId,co,lt,rt]) =
      mapAccumR mkUniqSystemId uniqs1 $ zip
        ["lt" `T.append` (s `T.cons` T.pack (show b0))
        ,"rt" `T.append` (s `T.cons` T.pack (show b1))
        ,"_co_"
        ,"lt"
        ,"rt"
        ]
        (brTy:brTy:idTys)
    ltVar = Var ltNId
    rtVar = Var rtNId
    pat   = DataPat brCon [mTV] [co,lt,rt]
    ltRhs = Case e brTy [(pat,Var lt)]
    rtRhs = Case e brTy [(pat,Var rt)]

    (kL,kR) = splitAt (length ks `div` 2) ks
    (uniqs3,(lVars,lBinds)) = go (n-1) bL kL uniqs2 ltVar
    (uniqs4,(rVars,rBinds)) = go (n-1) bR kR uniqs3 rtVar

-- | Create a vector of supplied elements
mkRTree :: DataCon -- ^ The LR constructor
        -> DataCon -- ^ The BR constructor
        -> Type    -- ^ Element type
        -> Integer -- ^ Depth of the tree
        -> [Term]  -- ^ Elements to put in the tree
        -> Term
mkRTree lrCon brCon resTy = go
  where
    go _ [x] = mkApps (Data lrCon) [Right (LitTy (NumTy 0))
                                    ,Right resTy
                                    ,Left  (primCo lrCoTy)
                                    ,Left  x
                                    ]

    go n xs =
      let (xsL,xsR) = splitAt (length xs `div` 2) xs
      in  mkApps (Data brCon) [Right (LitTy (NumTy n))
                              ,Right resTy
                              ,Right (LitTy (NumTy (n-1)))
                              ,Left (primCo (brCoTy n))
                              ,Left (go (n-1) xsL)
                              ,Left (go (n-1) xsR)]

    lrCoTy   = head (fromJust $! dataConInstArgTys lrCon  [(LitTy (NumTy 0))
                                                         ,resTy])
    brCoTy n = head (fromJust $! dataConInstArgTys brCon
                                                   [(LitTy (NumTy n))
                                                   ,resTy
                                                   ,(LitTy (NumTy (n-1)))])

-- | Determine whether a type is isomorphic to "Clash.Signal.Internal.Signal"
--
-- It is i.e.:
--
--   * Signal clk a
--   * (Signal clk a, Signal clk b)
--   * Vec n (Signal clk a)
--   * data Wrap = W (Signal clk' Int)
--   * etc.
--
-- This also includes BiSignals, i.e.:
--
--   * BiSignalIn High System Int
--   * etc.
--
isSignalType :: TyConMap -> Type -> Bool
isSignalType tcm ty = go HashSet.empty ty
  where
    go tcSeen (tyView -> TyConApp tcNm args) = case nameOcc tcNm of
      "Clash.Signal.Internal.Signal"      -> True
      "Clash.Signal.BiSignal.BiSignalIn"  -> True
      "Clash.Signal.Internal.BiSignalOut" -> True
      _ | tcNm `HashSet.member` tcSeen    -> False -- Do not follow rec types
        | otherwise -> case lookupUniqMap tcNm tcm of
            Just tc -> let dcs         = tyConDataCons tc
                           dcInsArgTys = concat
                                       $ mapMaybe (`dataConInstArgTys` args) dcs
                           tcSeen'     = HashSet.insert tcNm tcSeen
                       in  any (go tcSeen') dcInsArgTys
            Nothing -> traceIf True ($(curLoc) ++ "isSignalType: " ++ show tcNm
                                     ++ " not found.") False

    go _ _ = False

-- | Determines whether given type is an (alias of en) Enable line.
isEnable
  :: TyConMap
  -> Type
  -> Bool
isEnable m ty0
  | TyConApp (nameOcc -> "Clash.Signal.Internal.Enable") _ <- tyView ty0 = True
  | Just ty1 <- coreView1 m ty0 = isEnable m ty1
isEnable _ _ = False

-- | Determines whether given type is an (alias of en) Clock or Reset line
isClockOrReset
  :: TyConMap
  -> Type
  -> Bool
isClockOrReset m (coreView1 m -> Just ty)    = isClockOrReset m ty
isClockOrReset _ (tyView -> TyConApp tcNm _) = case nameOcc tcNm of
  "Clash.Signal.Internal.Clock" -> True
  "Clash.Signal.Internal.Reset" -> True
  _ -> False
isClockOrReset _ _ = False

tyNatSize :: TyConMap
          -> Type
          -> Except String Integer
tyNatSize m (coreView1 m -> Just ty) = tyNatSize m ty
tyNatSize _ (LitTy (NumTy i))        = return i
tyNatSize _ ty = throwE $ $(curLoc) ++ "Cannot reduce to an integer:\n" ++ showPpr ty


mkUniqSystemTyVar
  :: (Supply, InScopeSet)
  -> (OccName, Kind)
  -> ((Supply, InScopeSet), TyVar)
mkUniqSystemTyVar (supply,inScope) (nm, ki) =
  ((supply',extendInScopeSet inScope v'), v')
 where
  (u,supply') = freshId supply
  v           = mkTyVar ki (mkUnsafeSystemName nm u)
  v'          = uniqAway inScope v

mkUniqSystemId
  :: (Supply, InScopeSet)
  -> (OccName, Type)
  -> ((Supply,InScopeSet), Id)
mkUniqSystemId (supply,inScope) (nm, ty) =
  ((supply',extendInScopeSet inScope v'), v')
 where
  (u,supply') = freshId supply
  v           = mkLocalId ty (mkUnsafeSystemName nm u)
  v'          = uniqAway inScope v

mkUniqInternalId
  :: (Supply, InScopeSet)
  -> (OccName, Type)
  -> ((Supply,InScopeSet), Id)
mkUniqInternalId (supply,inScope) (nm, ty) =
  ((supply',extendInScopeSet inScope v'), v')
 where
  (u,supply') = freshId supply
  v           = mkLocalId ty (mkUnsafeInternalName nm u)
  v'          = uniqAway inScope v


-- | Same as @dataConInstArgTys@, but it tries to compute existentials too,
-- hence the extra argument @TyConMap@. WARNING: It will return the types
-- of non-existentials only
dataConInstArgTysE
  :: HasCallStack
  => InScopeSet
  -> TyConMap
  -> DataCon
  -> [Type]
  -> Maybe [Type]
dataConInstArgTysE is0 tcm (MkData { dcArgTys, dcExtTyVars, dcUnivTyVars }) inst_tys = do
  -- TODO: Check if all existentials were solved (they should be, or the wouldn't have
  -- TODO: been solved in the caseElemExistentials transformation)
  let is1   = extendInScopeSetList is0 dcExtTyVars
      is2   = unionInScope is1 (mkInScopeSet (tyFVsOfTypes inst_tys))
      subst = extendTvSubstList (mkSubst is2) (zip dcUnivTyVars inst_tys)
  go
    (substGlobalsInExistentials is0 dcExtTyVars (zip dcUnivTyVars inst_tys))
    (map (substTy subst) dcArgTys)

 where
  go
    :: [TyVar]
    -- ^ Existentials
    -> [Type]
    -- ^ Type arguments
    -> Maybe [Type]
    -- ^ Maybe ([type of non-existential])
  go exts0 args0 =
    let eqs = catMaybes (map (typeEq tcm) args0) in
    case solveNonAbsurds tcm eqs of
      [] ->
        Just args0
      sols ->
        go exts1 args1
        where
          exts1 = substInExistentialsList is0 exts0 sols
          is2   = extendInScopeSetList is0 exts1
          subst = extendTvSubstList (mkSubst is2) sols
          args1 = map (substTy subst) args0


-- | Given a DataCon and a list of types, the type variables of the DataCon
-- type are substituted for the list of types. The argument types are returned.
--
-- The list of types should be equal to the number of type variables, otherwise
-- @Nothing@ is returned.
dataConInstArgTys :: DataCon -> [Type] -> Maybe [Type]
dataConInstArgTys (MkData { dcArgTys, dcUnivTyVars, dcExtTyVars }) inst_tys =
  -- TODO: Check if inst_tys do not contain any free variables on call sites. If
  -- TODO: they do, this function is unsafe to use.
  let tyvars = dcUnivTyVars ++ dcExtTyVars in
  if length tyvars == length inst_tys then
    Just (map (substTyWith tyvars inst_tys) dcArgTys)
  else
    Nothing

-- | Make a coercion
primCo
  :: Type
  -> Term
primCo ty = Prim (PrimInfo "_CO_" ty WorkNever)

-- | Make an undefined term
undefinedTm
  :: Type
  -> Term
undefinedTm = TyApp (Prim (PrimInfo "Clash.Transformations.undefined" undefinedTy WorkNever))

substArgTys
  :: DataCon
  -> [Type]
  -> [Type]
substArgTys dc args =
  let univTVs = dcUnivTyVars dc
      extTVs  = dcExtTyVars dc
      argsFVs = foldl' unionVarSet emptyVarSet
                  (map (Lens.foldMapOf typeFreeVars unitVarSet) args)
      is      = mkInScopeSet (argsFVs `unionVarSet` mkVarSet extTVs)
      -- See Note [The substitution invariant]
      subst   = extendTvSubstList (mkSubst is) (univTVs `zipEqual` args)
  in  map (substTy subst) (dcArgTys dc)

stripTicks :: Term -> Term
stripTicks (Tick _ e) = stripTicks e
stripTicks e = e

-- | Try to reduce an arbitrary type to a literal type (Symbol or Nat),
-- and subsequently extract its String representation
tyLitShow
  :: TyConMap
  -> Type
  -> Except String String
tyLitShow m (coreView1 m -> Just ty) = tyLitShow m ty
tyLitShow _ (LitTy (SymTy s))        = return s
tyLitShow _ (LitTy (NumTy s))        = return (show s)
tyLitShow _ ty = throwE $ $(curLoc) ++ "Cannot reduce to a string:\n" ++ showPpr ty

-- | Determine whether we should split away types from a product type, i.e.
-- clocks should always be separate arguments, and not part of a product.
shouldSplit
  :: TyConMap
  -> Type
  -- ^ Type to examine
  -> Maybe (Term,[Type])
  -- ^ If we want to split values of the given type then we have /Just/:
  --
  -- 1. The (type-applied) data-constructor which, when applied to values of
  --    the types in 2., creates a value of the examined type
  --
  -- 2. The arguments types of the product we are trying to split.
  --
  -- Note that we only split one level at a time (although we check all the way
  -- down), e.g. given /(Int, (Clock, Bool))/ we return:
  --
  -- > Just ((,) @Int @(Clock, Bool), [Int, (Clock, Bool)])
  --
  -- An outer loop is required to subsequently split the /(Clock, Bool)/ tuple.
shouldSplit tcm (tyView ->  TyConApp (nameOcc -> "Clash.Explicit.SimIO.SimIO") [tyArg]) =
  -- We also look through `SimIO` to find things like Files
  shouldSplit tcm tyArg
shouldSplit tcm ty = shouldSplit0 tcm (tyView (coreView tcm ty))

-- | Worker of 'shouldSplit', works on 'TypeView' instead of 'Type'
shouldSplit0
  :: TyConMap
  -> TypeView
  -> Maybe (Term,[Type])
shouldSplit0 tcm (TyConApp tcNm tyArgs)
  | Just tc <- lookupUniqMap tcNm tcm
  , [dc] <- tyConDataCons tc
  , let dcArgs  = substArgTys dc tyArgs
  , let dcArgVs = map (tyView . coreView tcm) dcArgs
  = if any shouldSplitTy dcArgVs && not (isHidden tcNm tyArgs) then
      Just (mkApps (Data dc) (map Right tyArgs), dcArgs)
    else
      Nothing
 where
  shouldSplitTy :: TypeView -> Bool
  shouldSplitTy ty = isJust (shouldSplit0 tcm ty) || splitTy ty

  -- Hidden constructs (HiddenClock, HiddenReset, ..) don't need to be split
  -- because KnownDomain will be filtered anyway during netlist generation due
  -- to it being a zero-width type
  --
  -- TODO: This currently only handles (IP $x, KnownDomain) given that $x is any
  -- TODO: of the constructs handled in 'splitTy'. In practise this means only
  -- TODO: HiddenClock, HiddenReset, and HiddenEnable are handled. If a user were
  -- TODO: to define their own versions with -for example- the elements of the
  -- TODO: tuple swapped, 'isHidden' wouldn't recognize it. We could generalize
  -- TODO: this in the future.
  --
  isHidden :: Name a -> [Type] -> Bool
  isHidden nm [a1, a2] | TyConApp a2Nm _ <- tyView a2 =
       nameOcc nm == "GHC.Classes.(%,%)"
    && splitTy (tyView (stripIP a1))
    && nameOcc a2Nm == "Clash.Signal.Internal.KnownDomain"
  isHidden _ _ = False

  -- Currently we're only interested in splitting of Clock, Reset, and Enable
  splitTy (TyConApp tcNm0 _)
    = nameOcc tcNm0 `elem` [ "Clash.Signal.Internal.Clock"
                           , "Clash.Signal.Internal.Reset"
                           , "Clash.Signal.Internal.Enable"
                           -- iverilog doesn't like it when we put file handles
                           -- in a bitvector, so we need to make sure Clash
                           -- splits them off
                           , "Clash.Explicit.SimIO.File"
                           , "GHC.IO.Handle.Types.Handle"
                           ]
  splitTy _ = False

shouldSplit0 _ _ = Nothing

-- | Potentially split apart a list of function argument types. e.g. given:
--
-- > [Int,(Clock,(Reset,Bool)),Char]
--
-- we return
--
-- > [Int,Clock,Reset,Bool,Char]
--
-- But we would leave
--
-- > [Int, (Bool,Int), Char]
--
-- unchanged.
splitShouldSplit
  :: TyConMap
  -> [Type]
  -> [Type]
splitShouldSplit tcm = foldr go []
 where
  go ty rest = case shouldSplit tcm ty of
    Just (_,tys) -> splitShouldSplit tcm tys ++ rest
    Nothing      -> ty : rest

-- | Strip implicit parameter wrappers (IP)
stripIP :: Type -> Type
stripIP t@(tyView -> TyConApp tcNm [_a1, a2]) =
  if nameUniq tcNm == getKey ipClassKey then a2 else t
stripIP t = t

-- | Do an inverse topological sorting of the let-bindings in a let-expression
inverseTopSortLetBindings
  :: HasCallStack
  => Term
  -> Term
inverseTopSortLetBindings (Letrec bndrs0 res) =
  let (graph,nodeMap,_) =
        Graph.graphFromEdges
          (map (\(i,e) -> let fvs = fmap varUniq
                                    (Set.elems (Lens.setOf freeLocalIds e) )
                          in  ((i,e),varUniq i,fvs)) bndrs0)
      nodes  = postOrd graph
      bndrs1 = map ((\(x,_,_) -> x) . nodeMap) nodes
  in  Letrec bndrs1 res
 where
  postOrd :: Graph.Graph -> [Graph.Vertex]
  postOrd g = postorderF (Graph.dff g) []

  postorderF :: Graph.Forest a -> [a] -> [a]
  postorderF ts = foldr (.) id (map postorder ts)

  postorder :: Graph.Tree a -> [a] -> [a]
  postorder (Graph.Node a ts) = postorderF ts . (a :)

inverseTopSortLetBindings e = e
{-# SCC inverseTopSortLetBindings #-}
