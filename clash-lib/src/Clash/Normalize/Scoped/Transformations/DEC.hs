{-|
  Copyright  :  (C) 2015-2016, University of Twente,
                    2021-2024,2026, QBayLogic B.V.
                    2022,      LumiGuide Fietsdetectie B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Disjoint expression consolidation for scoped Clash Core: the ported
  counterpart of "Clash.Normalize.Transformations.DEC". See there for what the
  transformation does.

  = Moving applications out of the alternatives

  DEC collects the applications in the alternatives of a case-expression in
  scope @n@ in a 'CaseTree': the case-expressions and let-expressions on the
  path from the case-expression to every application, with the arguments of
  the application at the leaves. Each level of the tree lives in the scope of
  the binders above it, so the arguments at a leaf live in the scope of the
  patterns and let-binders on its path.

  The function, the type arguments and the shared term arguments of an
  application that is lifted out of the alternatives have to be values of
  scope @n@. DEC only gets them there by unsinking them out of the scope of
  the binders on the path ('Clash.Core.Scoped.Subst.unsinkTerm'), which checks
  that none of those binders occur in them. So an application whose function
  or type arguments refer to a variable bound inside the alternatives can't be
  lifted, and an argument that refers to one can't be shared: the types
  enforce what the current code checks with the set of variables bound in the
  alternatives (see the regression tests in
  "Clash.Tests.Normalize.Transformations.DEC"). The arguments that aren't
  shared stay where they are, in the case-trees that select them.

  = Heads

  The applications are grouped by their function, their 'HeadKey', which keeps
  apart global variables, local variables (of scope @n@) and primitives. A
  local variable with the same unique as a global one is a different head, and
  a local variable bound inside the alternatives is no head at all.

  = Differences with "Clash.Normalize.Transformations.DEC"

  * Let-expressions keep their bindings in their order, and non-recursive ones
    stay non-recursive. The current code reorders them into strongly connected
    components, and makes them recursive.

  * The lifted applications are let-bound in the order of their heads: the
    globals by unique, then the locals, then the primitives by name. The
    current code orders them by the alpha-comparison of their functions.

  * The applications in the right-hand side of a non-recursive let-binding are
    not put under that let-binding in the 'CaseTree'. The current code does,
    where the binding is dead, and removed by the dead-code pass at the end.

  * The bindings of a recursive let-expression are visited in dependency
    order, and bindings that don't depend on each other in their order. The
    current code visits them in the order of the strongly connected components
    that "Data.Graph" gives for their uniques.

  * An argument that isn't shared is let-bound with its type moved out of the
    alternatives, or with the type of the parameter of the function if that
    mentions a variable bound in the alternatives. The current code uses the
    type of the argument, which may mention one.

  * Applications of the same function to different numbers of arguments
    aren't lifted, where the current code would crash, or drop arguments. They
    don't occur: with the same type arguments, they have different types, so
    one of them has a function type, which isn't representable, and DEC
    doesn't collect applications of a type that isn't representable.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Normalize.Scoped.Transformations.DEC
  ( disjointExpressionConsolidation
  , disjointExpressionConsolidationWorker
  ) where

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Data.Bits ((.&.), complement)
import qualified Data.Either as Either
import qualified Data.Graph as Graph
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Kind as K
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Extra (showt)
import GHC.Stack (HasCallStack)
import qualified Language.Haskell.TH as TH

import GHC.Core.Make (chunkify, mkChunkified)
import GHC.Settings.Constants (mAX_TUPLE_SIZE)

import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import Clash.Core.Term (PrimInfo (..))
import Clash.Core.TyCon (TyConName, tyConDataCons)
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (Binding)
import qualified Clash.Normalize.Primitives as NP
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL (Transformation, onCase, toScopedTransformation)
import Clash.Rewrite.StrategyDSL.TH (asScopedRewriteQ)
import Clash.Rewrite.Types
  (globalHeap, scopedBindings, scopedTypeEnv, tupleTcCache)
import Clash.Util (curLoc)

import Clash.Core.Scoped.Alpha (aeqTm, aeqTy)
import Clash.Core.Scoped.DataCon (DataConSig (..), dataConInstArgTys)
import Clash.Core.Scoped.EqSolver (typeEq)
import Clash.Core.Scoped.Evaluator (whnf)
import Clash.Core.Scoped.Evaluator.Types (Env, Machine (..), PrimHeap, initMachine)
import Clash.Core.Scoped.FreeVars
  (bindNameBinders, freeTmVars, patNameBinders, teleNameBinders)
import Clash.Core.Scoped.HasType (inferCoreTypeOf, piResultTy)
import Clash.Core.Scoped.NameMap
  ( GlobalEnv, NameBinders, appendNameBinders, emptyNameBinders, emptyNameMap
  , memberNameSet )
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term
  (collectArgs, collectArgsTicks, mkApps, mkTicks, stripTicks)
import Clash.Core.Scoped.Type
  (TypeView (..), coreView, mkTyConApp, splitFunTy, tyView)
import Clash.Core.Scoped.TypeEnv (TypeEnv, dataConSigOf, typeEnvTyConMap)
import Clash.Core.Scoped.Util (internalName, mkSelectorCase)
import qualified Clash.Normalize.Scoped.Transformations.Letrec as Letrec
import Clash.Rewrite.Scoped
  (ScopedTransform (..), TransformContext, bottomupR, changed, tfScope)
import Clash.Rewrite.Scoped.Util (isFromInt, isUntranslatableType)
import Clash.Rewrite.Scoped.Whnf (evaluatorEnv)
import Clash.Rewrite.Scoped.WorkFree (isConstant)

-- primitives
import qualified Clash.Sized.Internal.BitVector
import qualified Clash.Sized.Internal.Index
import qualified Clash.Sized.Internal.Signed
import qualified Clash.Sized.Internal.Unsigned
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Num.Integer
import qualified GHC.Prim

-- | Lift applications of global binders out of alternatives of
-- case-statements, see
-- 'Clash.Normalize.Transformations.DEC.disjointExpressionConsolidation'
disjointExpressionConsolidation :: Transformation
disjointExpressionConsolidation =
  toScopedTransformation "DEC" (onCase 'disjointExpressionConsolidationWorker)

-- | The 'Case' handler of 'disjointExpressionConsolidation'
disjointExpressionConsolidationWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
disjointExpressionConsolidationWorker ctx e _scrut _ty (_:_:_) = do
  env <- decEnv
  let sc = tfScope ctx
      te = deTypeEnv env
  -- Collect all (the applications of) global binders (and certain primitives)
  -- that would be interesting to share out of the case-alternatives.
  (_, collected) <- collectTm env emptyNameBinders Map.empty sc [] e
  -- Those that are used at most once in every (nested) branch, and whose
  -- function and type arguments don't refer to variables bound inside the
  -- alternatives.
  let disjoint = Maybe.mapMaybe (disjointGroup sc) (Map.toList collected)
  -- For every to-lift expression create (the generalization of):
  --
  -- let f_tupIn = case x of {A -> (3,y); B -> (x,x)}
  --     f_arg0  = case f_tupIn of (l,_) -> l
  --     f_arg1  = case f_tupIn of (_,r) -> r
  -- in  f f_arg0 f_arg0
  --
  -- if an argument is non-representable, the case-expression is inlined,
  -- and no let-binding will be created for it.
  lifted <- Maybe.catMaybes <$> mapM (mkDisjointGroup env sc) disjoint
  if null lifted then
    return e
  else
    -- Let-bind all the lifted expressions, and substitute references to them
    -- for the applications they replace, in the case-expression and in each
    -- other.
    withFreshIdBinders sc
      [ (internalName (decFunName (gHead g) <> "_out"), inferCoreTypeOf te sc l)
      | (g, l) <- lifted ]
      $ \scOut outs outVars -> do
        let nbsOut = teleNameBinders outs
            keys = map (gKey . fst) lifted
            replacements = Map.fromList (zip keys outVars)
        -- See 'Clash.Normalize.Transformations.DEC.disjointExpressionConsolidationWorker'
        -- for why the substitution of a lifted expression leaves out the
        -- lifted expression itself.
        lifted1 <- Monad.forM lifted $ \(g, l) ->
          fst <$> collectTm env nbsOut (Map.delete (gKey g) replacements) scOut
                    (gSeen g) (sink l)
        (e1, _) <- collectTm env nbsOut replacements scOut [] (sink e)
        -- `mkDisjointGroup` doesn't clean up unused let-binders.
        lb <- runScopedTransform (bottomupR deadCodeR) ctx (Let (Rec outs lifted1) e1)
        changed lb

disjointExpressionConsolidationWorker _ e _ _ _ = return e

-- | 'Letrec.deadCode'
deadCodeR :: ScopedTransform NormalizeSession
deadCodeR = $(asScopedRewriteQ Letrec.deadCode)

-- * The environment

-- | What DEC needs from the rewrite environment
data DecEnv = DecEnv
  { deTypeEnv :: TypeEnv
  , deTupleTcs :: IntMap TyConName
  -- ^ The tuple type constructors, by arity
  , deEvalEnv :: Env
  , deBindings :: GlobalEnv (Binding (Term 'VoidS))
  , dePrimHeap :: PrimHeap
  }

decEnv :: NormalizeSession DecEnv
decEnv = do
  te <- Lens.view scopedTypeEnv
  tupTcs <- Lens.view tupleTcCache
  bm <- Lens.use scopedBindings
  ph <- Lens.use globalHeap
  pure (DecEnv te tupTcs (evaluatorEnv te) bm ph)

-- * Heads

-- | The function of an application, as a key that keeps globals, locals, and
-- primitives apart (decision D5 of the port). A local variable is one of the
-- scope of the case-expression: it can't be bound inside the alternatives.
data HeadKey (n :: S)
  = HGlobal !GlobalId
  | HLocal !(TmName n)
  | HPrim !Text
  -- ^ By the name of the primitive
  deriving (Eq, Ord)

-- | The head of an application that's interesting to lift out of the
-- alternatives of a case-expression in scope @n@
data Head (n :: S) = Head
  { headKey :: !(HeadKey n)
  , headTerm :: !(Term n)
  -- ^ The function
  }

-- | The name for the let-binders of a lifted application, see
-- 'Clash.Normalize.Transformations.DEC.decFunName'
decFunName :: Term n -> C.OccName
decFunName fun = last . Text.splitOn "." $ case fun of
  GlobalVar g -> C.nameOcc (C.varName (globalIdVar g))
  Var occ _ _ -> C.nameOcc occ
  Prim p -> primName p
  _ -> "complex_expression"

-- * Case-trees

-- | The arguments of an application
newtype Args (n :: S) = Args [Either (Term n) (Type n)]

-- | Selected terms
newtype Terms (n :: S) = Terms [Term n]

-- | A case-tree of values in the scope of the binders on their path, see
-- 'Clash.Normalize.Transformations.DEC.CaseTree'. The constructors that go
-- under binders keep the scope they extend scope @n@ to.
data CaseTree (a :: S -> K.Type) (n :: S) where
  Leaf :: !(a n) -> CaseTree a n
  -- | The let-bindings of a let-expression, and the tree under them
  LB :: DExt n l => !(Scope l) -> !(Bind n l) -> !(CaseTree a l) -> CaseTree a n
  -- | The subject of a case-expression, and the alternatives in which the
  -- application occurs
  Branch :: !(Term n) -> [CaseAlt a n] -> CaseTree a n

-- | An alternative of a 'Branch'
data CaseAlt (a :: S -> K.Type) (n :: S) where
  CaseAlt :: DExt n l => !(Scope l) -> !(Pat n l) -> !(CaseTree a l) -> CaseAlt a n

-- | Map the leaves of a case-tree
mapLeaves :: (forall l. a l -> b l) -> CaseTree a n -> CaseTree b n
mapLeaves f = \case
  Leaf a -> Leaf (f a)
  LB sc b ct -> LB sc b (mapLeaves f ct)
  Branch s alts -> Branch s [CaseAlt sc p (mapLeaves f ct) | CaseAlt sc p ct <- alts]

-- | A leaf of a case-tree rooted in scope @n@, in its own scope @l@, with the
-- names bound between the two
data SomeLeaf (a :: S -> K.Type) (n :: S) where
  SomeLeaf :: DExt n l => !(Scope l) -> !(NameBinders n l) -> !(a l) -> SomeLeaf a n

-- | The leaves of a case-tree, from left to right
leavesOf :: forall a n. Distinct n => Scope n -> CaseTree a n -> [SomeLeaf a n]
leavesOf sc0 = go sc0 emptyNameBinders
 where
  go :: forall l. DExt n l => Scope l -> NameBinders n l -> CaseTree a l -> [SomeLeaf a n]
  go sc nbs = \case
    Leaf a -> [SomeLeaf sc nbs a]
    LB (sc' :: Scope l') b ct ->
      withExtTrans @n @l @l' $ go sc' (appendNameBinders nbs (bindNameBinders b)) ct
    Branch _ alts -> concatMap (goAlt nbs) alts

  goAlt :: forall l. DExt n l => NameBinders n l -> CaseAlt a l -> [SomeLeaf a n]
  goAlt nbs (CaseAlt (sc' :: Scope l') p ct) =
    withExtTrans @n @l @l' $ go sc' (appendNameBinders nbs (patNameBinders p)) ct

-- | Test if a 'CaseTree' collected from an expression indicates that
-- application of a global binder is disjoint: occur in separate branches of a
-- case-expression. See 'Clash.Normalize.Transformations.DEC.isDisjoint'; the
-- equality of the type arguments is checked by 'disjointGroup'.
isDisjoint :: CaseTree a n -> Bool
isDisjoint (Branch _ [_]) = False
isDisjoint ct0 = go ct0
 where
  go :: CaseTree a l -> Bool
  go = \case
    Leaf _ -> False
    LB _ _ ct -> go ct
    Branch _ [] -> False
    Branch _ [CaseAlt _ _ ct] -> go ct
    Branch _ (_:_:_) -> True

-- * Collecting applications

-- | The applications of a head collected from a term in scope @l@, inside a
-- case-expression in scope @n@
data Collected (n :: S) (l :: S) = Collected
  { cHead :: Term n
  -- ^ The function
  , cSeen :: [HeadKey n]
  -- ^ The heads seen before, see
  -- 'Clash.Normalize.Transformations.DEC.collectGlobals'
  , cTree :: CaseTree Args l
  }

-- | Collect 'CaseTree's for (potentially) disjoint applications out of an
-- expression in scope @l@, inside the case-expression in scope @n@. Also
-- substitute truly disjoint applications by a reference to a lifted out
-- application. See 'Clash.Normalize.Transformations.DEC.collectGlobals'.
collectTm
  :: forall n l
   . Distinct l
  => DecEnv
  -> NameBinders n l
  -- ^ The names bound between the case-expression and the expression
  -> Map (HeadKey n) (Term l)
  -- ^ References to lifted applications, by their heads
  -> Scope l
  -> [HeadKey n]
  -- ^ Already seen heads
  -> Term l
  -> NormalizeSession (Term l, Map (HeadKey n) (Collected n l))
collectTm env nbs repl sc seen e = case e of
  Case scrut ty alts -> do
    (alts1, collectedAlts) <- collectAlts env nbs repl sc seen alts
    (scrut1, collectedScrut) <-
      collectTm env nbs repl sc (Map.keys collectedAlts ++ seen) scrut
    let branches =
          fmap (\(h, s, cas) -> Collected h s (Branch scrut1 cas)) collectedAlts
    return (Case scrut1 ty alts1, Map.union branches collectedScrut)

  (collectArgsTicks -> (fun, args@(_:_), ticks))
    | not (isConstant e) -> do
      untran <- isUntranslatableType False (inferCoreTypeOf te sc e)
      if untran then
        -- Don't lift out non-representable values, because they cannot be
        -- let-bound in our desired normal form.
        return (e, Map.empty)
      else do
        -- Look for, and substitute by, disjoint applications in the arguments
        -- first, see 'Clash.Normalize.Transformations.DEC.collectGlobals'
        (args1, collectedArgs) <- collectArgsC env nbs repl sc seen args
        let seenInArgs = Map.keys collectedArgs ++ seen
        case interestingHead env sc nbs fun args ticks of
          Just h | headKey h `notElem` seenInArgs -> do
            let e1 = Maybe.fromMaybe (mkApps (mkTicks fun ticks) args1)
                                     (Map.lookup (headKey h) repl)
            return ( e1
                   , Map.insert (headKey h) (Collected (headTerm h) seen (Leaf (Args args1)))
                                collectedArgs )
          _ -> return (mkApps (mkTicks fun ticks) args1, collectedArgs)

  Let bind body -> openBind sc bind body $ \sc1 bind1 body1 -> do
    let nbs1 = appendNameBinders nbs (bindNameBinders bind1)
        repl1 = sink1 repl
    (body2, collectedBody) <- collectTm env nbs1 repl1 sc1 seen body1
    let seen1 = Map.keys collectedBody ++ seen
    case bind1 of
      NonRec b x -> do
        -- The applications in the right-hand side are not put under the
        -- binding, which isn't in scope of it
        (x1, collectedX) <- collectTm env nbs repl sc seen1 x
        let bind2 = NonRec b x1
        return ( Let bind2 body2
               , Map.union (fmap (underLB sc1 bind2) collectedBody) collectedX )
      Rec bs xs -> do
        (xs1, collectedXs) <- collectRec env nbs1 repl1 sc1 seen1 bs xs
        let bind2 = Rec bs xs1
        return ( Let bind2 body2
               , fmap (underLB sc1 bind2) (Map.union collectedBody collectedXs) )

  Tick t e1 -> do
    (e2, collected) <- collectTm env nbs repl sc seen e1
    return (Tick t e2, collected)

  _ -> return (e, Map.empty)
 where
  te = deTypeEnv env

  underLB :: DExt l l1 => Scope l1 -> Bind l l1 -> Collected n l1 -> Collected n l
  underLB sc1 bind1 c = c {cTree = LB sc1 bind1 (cTree c)}

-- | Collect 'CaseTree's out of the arguments of an application, see
-- 'Clash.Normalize.Transformations.DEC.collectGlobalsArgs'
collectArgsC
  :: Distinct l
  => DecEnv
  -> NameBinders n l
  -> Map (HeadKey n) (Term l)
  -> Scope l
  -> [HeadKey n]
  -> [Either (Term l) (Type l)]
  -> NormalizeSession ([Either (Term l) (Type l)], Map (HeadKey n) (Collected n l))
collectArgsC env nbs repl sc = go
 where
  go _ [] = return ([], Map.empty)
  go seen (Left tm : args) = do
    (tm1, collected) <- collectTm env nbs repl sc seen tm
    (args1, collectedArgs) <- go (Map.keys collected ++ seen) args
    return (Left tm1 : args1, Map.union collected collectedArgs)
  go seen (Right ty : args) = do
    (args1, collectedArgs) <- go seen args
    return (Right ty : args1, collectedArgs)

-- | Collect 'CaseTree's out of the alternatives of a case-expression, grouped
-- by head, see 'Clash.Normalize.Transformations.DEC.collectGlobalsAlts'
collectAlts
  :: forall n l
   . Distinct l
  => DecEnv
  -> NameBinders n l
  -> Map (HeadKey n) (Term l)
  -> Scope l
  -> [HeadKey n]
  -> [Alt l]
  -> NormalizeSession ([Alt l], Map (HeadKey n) (Term n, [HeadKey n], [CaseAlt Args l]))
collectAlts env nbs repl sc seen alts = do
  (alts1, collected) <- unzip <$> mapM goAlt alts
  let merge (h, s1, c1) (_, s2, c2) = (h, List.nub (s1 ++ s2), c1 ++ c2)
  return (alts1, Map.unionsWith merge collected)
 where
  goAlt
    :: Alt l
    -> NormalizeSession (Alt l, Map (HeadKey n) (Term n, [HeadKey n], [CaseAlt Args l]))
  goAlt alt = openAlt sc alt $ \sc1 p e -> do
    (e1, collected) <-
      collectTm env (appendNameBinders nbs (patNameBinders p)) (sink1 repl) sc1 seen e
    return ( Alt p e1
           , fmap (\c -> (cHead c, cSeen c, [CaseAlt sc1 p (cTree c)])) collected )

-- | Collect 'CaseTree's out of the right-hand sides of a recursive
-- let-binding, in dependency order. Skips the bindings that are part of a
-- cycle, see 'Clash.Normalize.Transformations.DEC.collectGlobalsLbs'.
collectRec
  :: forall n l k
   . Distinct l
  => DecEnv
  -> NameBinders n l
  -> Map (HeadKey n) (Term l)
  -> Scope l
  -> [HeadKey n]
  -> Tele IdBinder k l
  -> [Term l]
  -> NormalizeSession ([Term l], Map (HeadKey n) (Collected n l))
collectRec env nbs repl sc seen0 bs xs = do
  (_, done) <- Monad.foldM go (seen0, IntMap.empty) sccs
  let xs1 = zipWith (\i x -> maybe x fst (IntMap.lookup i done)) [0..] xs
  return (xs1, Map.unions (map snd (IntMap.elems done)))
 where
  binders = [nm | Var _ _ nm <- teleIdVars' bs]
  deps x = let fvs = freeTmVars x in [i | (i, nm) <- zip [0 :: Int ..] binders, memberNameSet nm fvs]
  sccs = Graph.stronglyConnComp [(i, i, deps x) | (i, x) <- zip [0..] xs]

  go (seen, done) = \case
    Graph.AcyclicSCC i -> do
      (x1, collected) <- collectTm env nbs repl sc seen (xs !! i)
      return (Map.keys collected ++ seen, IntMap.insert i (x1, collected) done)
    -- See 'Clash.Normalize.Transformations.DEC.collectGlobalsLbs' for why
    -- bindings in a cycle are skipped
    Graph.CyclicSCC _ -> return (seen, done)

  -- The variables of the binders, in their scope
  teleIdVars' :: Tele IdBinder k l -> [Term l]
  teleIdVars' = teleIdVars

-- * Interesting heads

-- | The head of an application that is interesting to lift out of a
-- case-expression in scope @n@: all global and local functions, and certain
-- primitives, see 'Clash.Normalize.Transformations.DEC.interestingToLift'.
-- 'Nothing' for a local function bound inside the alternatives: it can't be
-- lifted out of them.
interestingHead
  :: Distinct l
  => DecEnv
  -> Scope l
  -> NameBinders n l
  -> Term l
  -> [Either (Term l) (Type l)]
  -> [TickInfo l]
  -> Maybe (Head n)
interestingHead env sc nbs fun args ticks
  | any isNoDeDup ticks = Nothing
  | otherwise = case fun of
      GlobalVar g -> Just (Head (HGlobal g) (GlobalVar g))
      Var {} -> case unsinkTerm nbs fun of
        Just fun1@(Var _ _ nm) -> Just (Head (HLocal nm) fun1)
        _ -> Nothing
      Prim p
        | interestingPrim env sc p args ticks -> Just (Head (HPrim (primName p)) (Prim p))
      _ -> Nothing

isNoDeDup :: TickInfo n -> Bool
isNoDeDup = \case
  NoDeDup -> True
  _ -> False

isDeDup :: TickInfo n -> Bool
isDeDup = \case
  DeDup -> True
  _ -> False

-- | Whether an application in function position is interesting, like the
-- arguments of a higher-order primitive, see 'interestingPrim'
isInterestingApp :: Distinct l => DecEnv -> Scope l -> Term l -> Bool
isInterestingApp env sc tm = case collectArgsTicks tm of
  (fun, args, ticks)
    | any isNoDeDup ticks -> False
    | otherwise -> case fun of
        GlobalVar _ -> True
        Var {} -> True
        Prim p -> interestingPrim env sc p args ticks
        _ -> False

-- | Whether an application of a primitive is interesting to lift out of a
-- case-expression: all non-cheap multiplications, and all division-like
-- operations with a non-cheap divisor, see
-- 'Clash.Normalize.Transformations.DEC.interestingToLift'. Multiplying or
-- dividing by zero or a power of two is cheap.
interestingPrim
  :: Distinct l
  => DecEnv
  -> Scope l
  -> PrimInfo
  -> [Either (Term l) (Type l)]
  -> [TickInfo l]
  -> Bool
interestingPrim env sc pInfo args ticks =
  case List.lookup (primName pInfo) interestingPrims of
    Just t | t || anyArgNotConstant -> True
    _ | any isDeDup ticks -> True
    _ -> isHOTy (primType pInfo) && any (isInterestingApp env sc) lArgs
 where
  lArgs = Either.lefts args
  anyArgNotConstant = any (not . isConstant) lArgs

  interestingPrims = map (\(nm, t) -> (fromTHName nm, t))
    [('(Clash.Sized.Internal.BitVector.*#),bothNotPow2)
    ,('Clash.Sized.Internal.BitVector.times#,bothNotPow2)
    ,('Clash.Sized.Internal.BitVector.quot#,lastNotPow2)
    ,('Clash.Sized.Internal.BitVector.rem#,lastNotPow2)
    ,('(Clash.Sized.Internal.Index.*#),bothNotPow2)
    ,('Clash.Sized.Internal.Index.times#,bothNotPow2)
    ,('Clash.Sized.Internal.Index.quot#,lastNotPow2)
    ,('Clash.Sized.Internal.Index.rem#,lastNotPow2)
    ,('(Clash.Sized.Internal.Signed.*#),bothNotPow2)
    ,('Clash.Sized.Internal.Signed.times#,bothNotPow2)
    ,('Clash.Sized.Internal.Signed.rem#,lastNotPow2)
    ,('Clash.Sized.Internal.Signed.quot#,lastNotPow2)
    ,('Clash.Sized.Internal.Signed.div#,lastNotPow2)
    ,('Clash.Sized.Internal.Signed.mod#,lastNotPow2)
    ,('(Clash.Sized.Internal.Unsigned.*#),bothNotPow2)
    ,('Clash.Sized.Internal.Unsigned.times#,bothNotPow2)
    ,('Clash.Sized.Internal.Unsigned.quot#,lastNotPow2)
    ,('Clash.Sized.Internal.Unsigned.rem#,lastNotPow2)
    ,('GHC.Base.quotInt,lastNotPow2)
    ,('GHC.Base.remInt,lastNotPow2)
    ,('GHC.Base.divInt,lastNotPow2)
    ,('GHC.Base.modInt,lastNotPow2)
    ,('GHC.Classes.divInt#,lastNotPow2)
    ,('GHC.Classes.modInt#,lastNotPow2)
    ,('GHC.Num.Integer.integerMul,bothNotPow2)
    ,('GHC.Num.Integer.integerDiv,lastNotPow2)
    ,('GHC.Num.Integer.integerMod,lastNotPow2)
    ,('GHC.Num.Integer.integerQuot,lastNotPow2)
    ,('GHC.Num.Integer.integerRem,lastNotPow2)
    ,('(GHC.Prim.*#),bothNotPow2)
    ,('GHC.Prim.quotInt#,lastNotPow2)
    ,('GHC.Prim.remInt#,lastNotPow2)
    ]

  lastNotPow2 = case lArgs of
    [] -> True
    _ -> not (termIsPow2 env sc (last lArgs))

  -- This only looks at the last two arguments, skipping over any constraints
  -- if they exist
  bothNotPow2 = go lArgs
   where
    go xs = case xs of
      [] -> True
      [a] -> not (termIsPow2 env sc a)
      [a,b] -> not (termIsPow2 env sc a) && not (termIsPow2 env sc b)
      (_:rest) -> go rest

  isHOTy t = case C.splitFunForallTy t of
    (args', _) -> any C.isPolyFunTy (Either.rights args')

-- | Whether a term evaluates to a power of two (or zero), see
-- 'Clash.Normalize.Transformations.DEC.interestingToLift'. Evaluates without
-- the let-bindings of the context, like the current code.
termIsPow2 :: Distinct l => DecEnv -> Scope l -> Term l -> Bool
termIsPow2 env sc e =
  whnf (deEvalEnv env) False
    (initMachine sc emptyNameMap (deBindings env) (dePrimHeap env) e)
    (\m -> isPow2Term (mTerm m))
 where
  isPow2Term :: Term x -> Bool
  isPow2Term = \case
    Literal (IntegerLiteral n) -> isPow2 n
    a -> case collectArgs a of
      (Prim p, [Right _, Left _, Left (Literal (IntegerLiteral n))])
        | isFromInt (primName p) -> isPow2 n
      (Prim p, [Right _, Left _, Left _, Left (Literal (IntegerLiteral n))])
        | primName p == fromTHName 'Clash.Sized.Internal.BitVector.fromInteger# -> isPow2 n
      _ -> False

  -- This used to contain (x /= 0), but multiplying by zero is free
  isPow2 x = (x .&. (complement x + 1)) == x

fromTHName :: TH.Name -> Text
fromTHName = Text.pack . show

-- * Lifting

-- | An application that can be lifted out of the alternatives of a
-- case-expression in scope @n@
data Group (n :: S) = Group
  { gKey :: HeadKey n
  , gHead :: Term n
  -- ^ The function
  , gSeen :: [HeadKey n]
  , gTree :: CaseTree Args n
  , gTyArgs :: [Maybe (Type n)]
  -- ^ For every argument: the type argument, which all applications share,
  -- or 'Nothing' for a term argument
  }

-- | Whether the applications of a head are disjoint, and can be lifted out
-- of the alternatives: their function is one of scope @n@ (which
-- 'interestingHead' ensures), and their type arguments are the same values
-- of scope @n@. See 'Clash.Normalize.Transformations.DEC.isDisjoint' and
-- 'Clash.Normalize.Transformations.DEC.isLiftable'.
--
-- Also checks that every application has the same number of arguments, in
-- the same order of types and terms, which
-- 'Clash.Normalize.Transformations.DEC.mkDisjointGroup' assumes.
disjointGroup
  :: forall n. Distinct n => Scope n -> (HeadKey n, Collected n n) -> Maybe (Group n)
disjointGroup sc (key, Collected fun seen ct)
  | not (isDisjoint ct) = Nothing
  | otherwise = do
      shapes <- traverse leafShape leaves
      shape <- case shapes of
        [] -> Nothing
        s : rest
          | all (sameShape s) rest -> Just s
          | otherwise -> Nothing
      -- The type arguments of every application, at each position
      let positions = List.transpose shapes
      tyArgs <- Monad.zipWithM tyArgAt shape positions
      return (Group key fun seen ct tyArgs)
 where
  leaves = leavesOf sc ct

  -- The type arguments of an application, moved out of the alternatives,
  -- and 'Nothing' for its term arguments
  leafShape :: SomeLeaf Args n -> Maybe [Either () (Type n)]
  leafShape (SomeLeaf _ nbs (Args args)) =
    traverse (either (const (Just (Left ()))) (fmap Right . unsinkType nbs)) args

  sameShape s1 s2 =
    length s1 == length s2 && and (zipWith (\a b -> Either.isLeft a == Either.isLeft b) s1 s2)

  tyArgAt (Left ()) _ = Just Nothing
  tyArgAt (Right ty) col
    | all (either (const False) (aeqTy ty)) col = Just (Just ty)
    | otherwise = Nothing

-- | An argument of the lifted application
data ArgSlot (n :: S)
  = Shared !(Either (Term n) (Type n))
  -- ^ Shared by all applications
  | Selected !Int
  -- ^ The variable of the let-binding with this index

-- | Given a group of disjoint applications, give the lifted application: the
-- function applied to the shared arguments, and to let-bound case-expressions
-- that select between the other arguments. See
-- 'Clash.Normalize.Transformations.DEC.mkDisjointGroup'.
mkDisjointGroup
  :: forall n
   . (HasCallStack, Distinct n)
  => DecEnv -> Scope n -> Group n -> NormalizeSession (Maybe (Group n, Term n))
mkDisjointGroup env sc g = do
  let leaves = leavesOf sc (gTree g)
      funName = decFunName (gHead g)
  slots <- Monad.forM (zip3 [0 :: Int ..] (gTyArgs g) paramTys) $ \(i, tyArg, paramTy) ->
    case tyArg of
      Just ty -> return (Just (Left (Shared (Right ty))))
      Nothing -> case sharedArg i leaves of
        Just tm -> return (Just (Left (Shared (Left tm))))
        Nothing -> case argType i leaves paramTy of
          Nothing -> return Nothing
          Just ty -> do
            let sel = mapLeaves (\(Args args) -> Terms [termArg i args]) (gTree g)
            untran <- isUntranslatableType False ty
            return $ Just $ if untran then
              -- Non-representable arguments are not let-bound
              Left (Shared (Left (genCase te tupTcs sc ty [ty] sel)))
            else
              Right (internalName (funName <> "_arg" <> showt i), ty, i)
  case sequence slots of
    Nothing -> return Nothing
    Just slots1 -> do
      let -- The let-bound arguments, with the index of their variable
          bound = [(nm, ty, i) | Right (nm, ty, i) <- slots1]
          argSlots = snd (List.mapAccumL toSlot 0 slots1)
          toSlot k = \case
            Left s -> (k, s)
            Right _ -> (k + 1, Selected k)
          -- The selected arguments, as a case-tree of lists
          selected = mapLeaves (\(Args args) -> Terms [termArg i args | (_, _, i) <- bound]) (gTree g)
          tys = [ty | (_, ty, _) <- bound]
          apply :: DExt n o => [Term o] -> Term o
          apply vs = mkApps (sink (gHead g)) (map (slotArg vs) argSlots)
      return $ Just $ (g,) $ case bound of
        [] -> apply []
        [(nm, ty, _)] ->
          withFreshIdBinders sc [(nm, ty)] $ \_ bs vs ->
            Let (Rec bs [sink (genCase te tupTcs sc ty tys selected)]) (apply vs)
        _ ->
          let tupTy = mkBigTupTy te tupTcs tys
          in withFreshIdBinders sc ((internalName (funName <> "_tupIn"), tupTy) : [(nm, ty) | (nm, ty, _) <- bound]) $
               \so bs vs -> case vs of
                 tupIn : vs1 ->
                   Let (Rec bs (sink (genCase te tupTcs sc tupTy tys selected)
                                : [mkBigTupSelector so te tupTcs tupIn (map sink tys) k | k <- [0 .. length vs1 - 1]]))
                       (apply vs1)
                 [] -> error ($(curLoc) ++ "no binder for the tuple")
 where
  te = deTypeEnv env
  tupTcs = deTupleTcs env

  slotArg :: DExt n o => [Term o] -> ArgSlot n -> Either (Term o) (Type o)
  slotArg vs = \case
    Shared (Left tm) -> Left (sink tm)
    Shared (Right ty) -> Right (sink ty)
    Selected k -> Left (vs !! k)

  termArg :: Int -> [Either (Term l) (Type l)] -> Term l
  termArg i args = case args !! i of
    Left tm -> tm
    Right _ -> error ($(curLoc) ++ "type argument where a term argument was expected")

  -- The types of the parameters of the function, for its term arguments,
  -- instantiated with the type arguments
  paramTys :: [Maybe (Type n)]
  paramTys = go (Just (inferCoreTypeOf te sc (gHead g))) (gTyArgs g)
   where
    go _ [] = []
    go fTy (Just ty : rest) = Nothing : go (fmap (\t -> piResultTy te sc t ty) fTy) rest
    go fTy (Nothing : rest) = case fTy >>= splitFunTy te sc of
      Just (argTy, resTy) -> Just argTy : go (Just resTy) rest
      Nothing -> Nothing : go Nothing rest

  -- The type of the term argument at position @i@: that of the argument of
  -- the first application, if it doesn't mention variables bound inside the
  -- alternatives, and otherwise that of the parameter of the function
  argType :: Int -> [SomeLeaf Args n] -> Maybe (Type n) -> Maybe (Type n)
  argType i ls paramTy = case ls of
    SomeLeaf scL nbs (Args args) : _ ->
      case unsinkType nbs (inferCoreTypeOf te scL (termArg i args)) of
        Just ty -> Just ty
        Nothing -> paramTy
    [] -> Nothing

  -- The term argument at position @i@, if all applications share it: it
  -- doesn't mention variables bound inside the alternatives, and is either
  -- the same for all, or a proof of an equality, see
  -- 'Clash.Normalize.Transformations.DEC.areShared'
  sharedArg :: Int -> [SomeLeaf Args n] -> Maybe (Term n)
  sharedArg i ls = case map unsunk ls of
    Just x : rest
      | isProof (stripTicks x) -> Just x
      | all (maybe False (aeqTm (stripTicks x) . stripTicks)) rest -> Just x
    _ -> Nothing
   where
    unsunk (SomeLeaf _ nbs (Args args)) = unsinkTerm nbs (termArg i args)

  isProof co = case tyView (inferCoreTypeOf te sc co) of
    TyConApp nm _ -> C.nameOcc nm == fromTHName ''(~)
    _ -> False

-- | Create a case-expression that selects between the distinct arguments given
-- a case-tree, see 'Clash.Normalize.Transformations.DEC.genCase'
genCase
  :: forall n
   . Distinct n
  => TypeEnv
  -> IntMap TyConName
  -> Scope n
  -> Type n
  -- ^ Type of the alternatives
  -> [Type n]
  -- ^ Types of the arguments
  -> CaseTree Terms n
  -- ^ Case-tree of arguments
  -> Term n
genCase te tupTcs sc0 ty argTys = go sc0
 where
  go :: forall l. DExt n l => Scope l -> CaseTree Terms l -> Term l
  go sc = \case
    Leaf (Terms tms) -> mkBigTupTm te tupTcs (zip (map sink argTys) tms)
    LB (sc1 :: Scope l1) b ct -> withExtTrans @n @l @l1 $ Let b (go sc1 ct)
    Branch scrut [CaseAlt (sc1 :: Scope l1) p ct] -> withExtTrans @n @l @l1 $
      let ct1 = go sc1 ct
      in case unsinkAlt p ct1 of
           Just ct2 -> ct2
           Nothing -> Case scrut (sink ty) (addDefault te sc scrut (sink ty) [Alt p ct1])
    Branch scrut alts ->
      Case scrut (sink ty) (addDefault te sc scrut (sink ty) (map goAlt alts))

  goAlt :: forall l. DExt n l => CaseAlt Terms l -> Alt l
  goAlt (CaseAlt (sc1 :: Scope l1) p ct) = withExtTrans @n @l @l1 $ Alt p (go sc1 ct)

-- | Add a default alternative to the alternatives of a case-tree, if they're
-- not exhaustive, see 'Clash.Normalize.Transformations.DEC.genCase' (#2770)
addDefault :: forall n. Distinct n => TypeEnv -> Scope n -> Term n -> Type n -> [Alt n] -> [Alt n]
addDefault te sc scrut ty alts
  | any isDefaultAlt alts
  = alts
  -- Literal patterns can never cover all values
  | any isLitAlt alts
  = defaultAlt : alts
  | TyConApp tcNm tyArgs <- tyView (coreView te sc (inferCoreTypeOf te sc scrut))
  , Just tc <- UniqMap.lookup tcNm (typeEnvTyConMap te)
  , let dcs = tyConDataCons tc
  , all (isVanillaDc tyArgs) dcs
  , length dcs > length alts
  = -- In Core, default patterns always come first
    defaultAlt : alts
  | otherwise
  = alts
 where
  defaultAlt = Alt DefaultPat (TyApp (Prim NP.undefined) ty)

  isDefaultAlt (Alt DefaultPat _) = True
  isDefaultAlt _ = False

  isLitAlt (Alt LitPat{} _) = True
  isLitAlt _ = False

  -- XXX: For GADT-like constructors (e.g. Vec) counting alternatives cannot
  -- tell whether missing constructors can match. We therefore leave those
  -- alone.
  isVanillaDc :: [Type n] -> DataCon -> Bool
  isVanillaDc tyArgs dc = case dataConSigOf te dc of
    DataConSig _ TNil _ ->
      maybe False (all (Maybe.isNothing . typeEq te sc)) (dataConInstArgTys te sc dc tyArgs)
    _ -> False

-- * Tuples

-- | Lookup the TyConName and DataCon for a tuple of size n
findTup :: TypeEnv -> IntMap TyConName -> Int -> (TyConName, DataCon)
findTup te tupTcs n =
  Maybe.fromMaybe (error ("Cannot build " <> show n <> "-tuple")) $ do
    tupTcNm <- IntMap.lookup n tupTcs
    tupTc <- UniqMap.lookup tupTcNm (typeEnvTyConMap te)
    tupDc <- Maybe.listToMaybe (tyConDataCons tupTc)
    return (tupTcNm, tupDc)

mkBigTupTm :: TypeEnv -> IntMap TyConName -> [(Type n, Term n)] -> Term n
mkBigTupTm te tupTcs args = snd (mkChunkified (mkSmallTup te tupTcs) args)

mkSmallTup :: TypeEnv -> IntMap TyConName -> [(Type n, Term n)] -> (Type n, Term n)
mkSmallTup _ _ [] = error ($(curLoc) ++ "mkSmallTup: Can't create 0-tuple")
mkSmallTup _ _ [(ty, tm)] = (ty, tm)
mkSmallTup te tupTcs args = (ty, tm)
 where
  (argTys, tms) = unzip args
  (tupTcNm, tupDc) = findTup te tupTcs (length args)
  tm = mkApps (Data tupDc) (map Right argTys ++ map Left tms)
  ty = mkTyConApp tupTcNm argTys

mkSmallTupTy, mkBigTupTy :: TypeEnv -> IntMap TyConName -> [Type n] -> Type n
mkSmallTupTy _ _ [] = error ($(curLoc) ++ "mkSmallTupTy: Can't create 0-tuple")
mkSmallTupTy _ _ [ty] = ty
mkSmallTupTy te tupTcs tys = mkTyConApp (fst (findTup te tupTcs (length tys))) tys

mkBigTupTy te tupTcs = mkChunkified (mkSmallTupTy te tupTcs)

-- | Select the @n@-th element of a big tuple, see
-- 'Clash.Normalize.Transformations.DEC.mkBigTupSelector'
mkSmallTupSelector, mkBigTupSelector
  :: Distinct n => Scope n -> TypeEnv -> IntMap TyConName -> Term n -> [Type n] -> Int -> Term n
mkSmallTupSelector _ _ _ scrut [_] 0 = scrut
mkSmallTupSelector _ _ _ _ [_] n =
  error ($(curLoc) ++ "mkSmallTupSelector called with one type, but to select " ++ show n)
mkSmallTupSelector sc te _ scrut _ n =
  mkSelectorCase ($(curLoc) ++ "mkSmallTupSelector") sc te scrut 1 n

mkBigTupSelector sc te tupTcs scrut tys n = go (chunkify tys)
 where
  go [_] = mkSmallTupSelector sc te tupTcs scrut tys n
  go tyss =
    let (nOuter, nInner) = divMod n mAX_TUPLE_SIZE
        tyss' = map (mkSmallTupTy te tupTcs) tyss
        outer = mkSmallTupSelector sc te tupTcs scrut tyss' nOuter
    in mkSmallTupSelector sc te tupTcs outer (tyss List.!! nOuter) nInner
