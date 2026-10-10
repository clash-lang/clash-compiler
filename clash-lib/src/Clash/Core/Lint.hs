{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  A scope lint for Clash Core, in the spirit of GHC's Core Lint. It checks that
  a term is well-scoped in the context it appears in. See 'LintError' for the
  checks it performs.

  Term variables and type variables live in separate scopes: a type variable
  never binds a term variable with the same unique, nor the other way around.
  Note that this is stricter than Clash's 'Clash.Core.VarEnv.InScopeSet', which
  keys variables of both namespaces by their unique alone.
-}

{-# LANGUAGE LambdaCase #-}

module Clash.Core.Lint
  ( LintError (..)
  , LintCheck (..)
  , LintEnv
  , mkLintEnv
  , lintTerm
  , newLintErrors
  , showLintError
  ) where

import           Data.Coerce                 (coerce)
import qualified Data.List                   as List
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text
import           GHC.Exts                    (Any)

import           Clash.Core.Name             (Name (nameOcc))
import           Clash.Core.Term
  (Bind (..), Context, CoreContext (..), Pat (..), Term (..), TickInfo (..))
import           Clash.Core.Type             (Type (..))
import           Clash.Core.Var
  (Id, TyVar, Var (..), isGlobalId, isLocalId)
import           Clash.Core.VarEnv           (InScopeSet, lookupInScope)
import           Clash.Data.UniqMap          (UniqMap)
import qualified Clash.Data.UniqMap          as UniqMap
import           Clash.Unique                (Unique)

-- | A violation of the scoping rules of Clash Core
data LintError
  = UnboundId Id
  -- ^ An occurrence of a local term variable that is neither bound by an
  -- enclosing binder, nor by the context of the term
  | UnboundTyVar TyVar String
  -- ^ A type variable that is neither bound by an enclosing binder (a type
  -- lambda, a pattern, or a @forall@), nor by the context of the term. The
  -- string describes where it occurs, e.g. in the type of a binder.
  | UnknownGlobal Id
  -- ^ An occurrence of a global variable that is not in the binding map
  | Shadows (Var Any) (Var Any) Bool
  -- ^ A binder that shadows a variable in scope: the binder, the variable it
  -- shadows, and whether that variable is bound by the context of the term
  -- (as opposed to an enclosing binder in the term itself). Binders in
  -- different branches may share a unique, binders on the same path may not.
  | DuplicateBinder (Var Any)
  -- ^ A variable bound more than once by the same let-expression or pattern
  | LocalShadowsGlobal Id
  -- ^ A local binder with the unique of a global binder in the binding map.
  -- Clash distinguishes local and global variables with the same unique, but
  -- not everywhere: for example, an 'InScopeSet' does not. See issues #405
  -- and #3479.
  deriving (Eq, Show)

-- | The classes of checks 'lintTerm' performs. They can be enabled
-- separately, see 'mkLintEnv'.
data LintCheck
  = ScopeCheck
  -- ^ 'UnboundId', 'UnboundTyVar', 'UnknownGlobal', and 'DuplicateBinder'
  | ShadowCheck
  -- ^ 'Shadows'
  | LocalGlobalCheck
  -- ^ 'LocalShadowsGlobal'
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Where a variable in scope is bound
data Origin = InContext | InTerm

-- | Variables in scope, and how to recognize globals
data LintEnv = LintEnv
  { leIds :: UniqMap (Id, Origin)
  -- ^ Term variables bound by the context, or an enclosing binder
  , leTyVars :: UniqMap (TyVar, Origin)
  -- ^ Type variables bound by the context, or an enclosing binder
  , leInScope :: InScopeSet
  -- ^ The in-scope set of the context. Variables in it are considered bound
  -- by the context too, provided they are of the right namespace.
  , leIsGlobal :: Unique -> Bool
  -- ^ Whether a unique belongs to a global binder in the binding map
  , leChecks :: LintCheck -> Bool
  -- ^ Which checks to perform
  }

-- | Create the environment to lint a term that appears in the given context
mkLintEnv
  :: (LintCheck -> Bool)
  -- ^ Which checks to perform
  -> (Unique -> Bool)
  -- ^ Whether a unique belongs to a global binder in the binding map
  -> InScopeSet
  -- ^ The in-scope set of the context
  -> Context
  -- ^ The binders of the context
  -> LintEnv
mkLintEnv checks isGlobal is ctx =
  List.foldl' addCtx (LintEnv UniqMap.empty UniqMap.empty is isGlobal checks) ctx
 where
  addCtx env = \case
    LetBinding _ bs -> addIds env bs
    LetBody bs -> addIds env (map fst bs)
    LamBody i -> addIds env [i]
    TyLamBody tv -> addTyVars env [tv]
    CaseAlt (DataPat _ tvs ids) -> addIds (addTyVars env tvs) ids
    _ -> env

  addIds env is0 =
    env{leIds = List.foldl' (\m i -> UniqMap.insert i (i, InContext) m) (leIds env) is0}
  addTyVars env tvs =
    env{leTyVars = List.foldl' (\m tv -> UniqMap.insert tv (tv, InContext) m) (leTyVars env) tvs}

-- | Find all violations of the scoping rules in a term, see 'LintError'
lintTerm :: LintEnv -> Term -> [LintError]
lintTerm env0 tm0 = goTerm env0 tm0 []
 where
  scope = leChecks env0 ScopeCheck
  shadow = leChecks env0 ShadowCheck
  localGlobal = leChecks env0 LocalGlobalCheck

  goTerm :: LintEnv -> Term -> [LintError] -> [LintError]
  goTerm env tm = case tm of
    Var v
      | isGlobalId v ->
          (if not scope || leIsGlobal env (varUniq v) then id else (UnknownGlobal v :))
            . goType env ("the type of occurrence of " <> pprU v) (varType v)
      | otherwise ->
          (if not scope || idBound env v then id else (UnboundId v :))
            . goType env ("the type of occurrence of " <> pprU v) (varType v)
    Data{} -> id
    Literal{} -> id
    Prim{} -> id
    Lam i e ->
      goIdBndrs env [i] . goTerm (bindIds env [i]) e
    TyLam tv e ->
      goTyBndrs env [tv] . goTerm (bindTyVars env [tv]) e
    App e1 e2 -> goTerm env e1 . goTerm env e2
    TyApp e ty -> goTerm env e . goType env "a type application" ty
    Let (NonRec i x) e ->
      goIdBndrs env [i] . goTerm env x . goTerm (bindIds env [i]) e
    Let (Rec bs) e ->
      let is = map fst bs
          env1 = bindIds env is
       in goIdBndrs env is
            . foldr (\(_, x) k -> goTerm env1 x . k) id bs
            . goTerm env1 e
    Case scrut ty alts ->
      goTerm env scrut
        . goType env "the type of a case-expression" ty
        . foldr (\alt k -> goAlt env alt . k) id alts
    Cast e ty1 ty2 ->
      goTerm env e
        . goType env "a cast" ty1
        . goType env "a cast" ty2
    Tick t e -> goTick env t . goTerm env e

  goAlt env (pat, e) = case pat of
    DataPat _ tvs ids ->
      let env1 = bindTyVars env tvs
       in goTyBndrs env tvs
            -- The types of the term binders may refer to the existentials
            . goIdBndrs env1 ids
            . goTerm (bindIds env1 ids) e
    _ -> goTerm env e

  goTick env = \case
    NameMod _ ty -> goType env "a name modifier" ty
    Attributes ty tm -> goType env "a synthesis attribute" ty . goTerm env tm
    _ -> id

  -- Check binders of a single binding group, binding nothing yet
  goIdBndrs :: LintEnv -> [Id] -> [LintError] -> [LintError]
  goIdBndrs env is =
    foldr (\i k -> goIdBndr env i . k) id is
      . dups (coerce is)

  goIdBndr env i =
    goType env ("the type of binder " <> pprU i) (varType i)
      . (if localGlobal && isLocalId i && leIsGlobal env (varUniq i)
           then (LocalShadowsGlobal i :)
           else id)
      . if not shadow then id else case UniqMap.lookup i (leIds env) of
          Just (v, o) -> (Shadows (coerce i) (coerce v) (isContext o) :)
          Nothing
            | Just v <- lookupInScope (leInScope env) i
            , isLocalId v
            -> (Shadows (coerce i) v True :)
            | otherwise -> id

  goTyBndrs :: LintEnv -> [TyVar] -> [LintError] -> [LintError]
  goTyBndrs env tvs =
    foldr (\tv k -> goTyBndr env tv . k) id tvs
      . dups (coerce tvs)

  goTyBndr env tv =
    goType env ("the kind of binder " <> pprU tv) (varType tv)
      . if not shadow then id else case UniqMap.lookup tv (leTyVars env) of
          Just (v, o) -> (Shadows (coerce tv) (coerce v) (isContext o) :)
          Nothing
            | Just v@TyVar{} <- lookupInScope (leInScope env) tv
            -> (Shadows (coerce tv) v True :)
            | otherwise -> id

  dups :: [Var Any] -> [LintError] -> [LintError]
  dups vs = if not scope then id else case vs of
    [] -> id
    [_] -> id
    _ -> (map DuplicateBinder (go Set.empty vs) <>)
   where
    go _ [] = []
    go seen (v:rest)
      | varUniq v `Set.member` seen = v : go seen rest
      | otherwise = go (Set.insert (varUniq v) seen) rest

  -- Check that the free type variables of a type are in scope, reporting each
  -- once. Note that this does not look into the kinds of type variable
  -- occurrences, only into the kinds of type variable binders, see 'goTyBndr'.
  goType :: LintEnv -> String -> Type -> [LintError] -> [LintError]
  goType env what ty
    | scope = case go Set.empty ty [] of
        [] -> id
        tvs -> (map (`UnboundTyVar` what) (List.nubBy sameUniq tvs) <>)
    | otherwise = id
   where
    sameUniq tv1 tv2 = varUniq tv1 == varUniq tv2

    go bound = \case
      VarTy tv
        | varUniq tv `Set.member` bound -> id
        | tyVarBound env tv -> id
        | otherwise -> (tv :)
      ConstTy{} -> id
      ForAllTy tv ty1 -> go (Set.insert (varUniq tv) bound) ty1
      AppTy ty1 ty2 -> go bound ty1 . go bound ty2
      LitTy{} -> id
      AnnType _ ty1 -> go bound ty1

  isContext = \case
    InContext -> True
    InTerm -> False

  bindIds env is =
    env{leIds = List.foldl' (\m i -> UniqMap.insert i (i, InTerm) m) (leIds env) is}
  bindTyVars env tvs =
    env{leTyVars = List.foldl' (\m tv -> UniqMap.insert tv (tv, InTerm) m) (leTyVars env) tvs}

  idBound env i
    | UniqMap.elem i (leIds env) = True
    | Just Id{} <- lookupInScope (leInScope env) i = True
    | otherwise = False

  tyVarBound env tv
    | UniqMap.elem tv (leTyVars env) = True
    | Just TyVar{} <- lookupInScope (leInScope env) tv = True
    | otherwise = False

-- | The violations in the result of a transformation that were not already
-- present in its input. A transformation that is given an ill-scoped term is
-- not to blame for it; the transformation that produced it is.
--
-- Violations are identified by their kind and the unique of the offending
-- variable.
newLintErrors
  :: [LintError]
  -- ^ Violations in the input of the transformation
  -> [LintError]
  -- ^ Violations in the result of the transformation
  -> [LintError]
newLintErrors old new = filter ((`Set.notMember` oldKeys) . key) new
 where
  oldKeys = Set.fromList (map key old)
  key :: LintError -> (Int, Unique)
  key = \case
    UnboundId v -> (0, varUniq v)
    UnboundTyVar v _ -> (1, varUniq v)
    UnknownGlobal v -> (2, varUniq v)
    Shadows v _ _ -> (3, varUniq v)
    DuplicateBinder v -> (4, varUniq v)
    LocalShadowsGlobal v -> (5, varUniq v)

-- | Describe a violation, naming the offending variable and its binder
showLintError :: LintError -> String
showLintError = \case
  UnboundId v ->
    "Local variable " <> pprU v <> " is not in scope."
  UnboundTyVar tv what ->
    "Type variable " <> pprU tv <> " in " <> what <> " is not in scope."
  UnknownGlobal v ->
    "Global variable " <> pprU v <> " is not in the binding map."
  Shadows b v inCtx ->
    "Binder " <> pprU b <> " shadows " <> pprU v <> ", bound "
      <> (if inCtx then "in the context of the term." else "by an enclosing binder.")
  DuplicateBinder v ->
    "Variable " <> pprU v <> " is bound more than once by the same let-expression or pattern."
  LocalShadowsGlobal v ->
    "Local binder " <> pprU v <> " has the unique of a global binder."

-- | Show a variable by its name and unique: scoping is about uniques, so a
-- message without them is useless
pprU :: Var a -> String
pprU v = Text.unpack (nameOcc (varName v)) <> "[" <> show (varUniq v) <> "]"
