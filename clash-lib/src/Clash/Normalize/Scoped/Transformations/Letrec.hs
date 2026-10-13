{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on let-expressions of scoped Clash Core.

  = Merging let-bindings

  'flattenLet', 'topLet' and 'recToLetRec' merge let-bindings into a single
  recursive let-binding. In scoped Core, the right-hand side of a 'NonRec'
  binding lives outside the scope of its binder, so it has to be moved into
  that scope ('sink') before it can join a 'Rec' telescope, which needs the
  binder to be opened first ('openLetBindings'). Opening renames the binder if
  it clashes with the scope. So a binder that shadows a variable its
  right-hand side refers to, as in @\\x -> let x = g x in x@, can't be merged
  into the loop @letrec x = g x@: that would be a type error.

  = Order of let-bindings

  'simpleCSE' keeps the bindings in the reverse of the inverse topological
  order in which it visits them, which is computed from the positions of the
  bindings ('inverseTopSortOrder'). In the result of 'flattenLet', the
  bindings of a let-expression in a right-hand side come right before the
  binding whose right-hand side it was.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Scoped.Transformations.Letrec
  ( deadCode
  , deadCodeWorker
  , flattenLet
  , flattenLetWorker
  , recToLetRec
  , removeUnusedExpr
  , removeUnusedExprCase
  , removeUnusedExprSpine
  , simpleCSE
  , simpleCSEWorker
  , topLet
  , topLetWorker
  ) where

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Writer (listen)
import Data.Bifunctor (second)
import qualified Data.Either as Either
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.Maybe (fromMaybe)
import qualified Data.Monoid as Monoid (Any (..))
import qualified Data.Text as Text
import qualified Data.Text.Extra as Text
import GHC.Stack (HasCallStack)

import Clash.Annotations.BitRepresentation.Deriving (dontApplyInHDL)
import Clash.Sized.Vector as Vec (Vec (Cons), splitAt)

import Clash.Annotations.Primitive (extractPrim)
import Clash.Core.DataCon (DataCon (..))
import qualified Clash.Core.Name as C
import Clash.Core.TyCon (tyConDataCons)
import Clash.Netlist.BlackBox.Types ()
import Clash.Netlist.BlackBox.Util (getUsedArguments)
import Clash.Normalize.Scoped.Primitives (removedArg)
import Clash.Normalize.Scoped.Transformations.Reduce (reduceBinders)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Primitives.Types (Primitive (..), UsedArguments (..))
import Clash.Rewrite.StrategyDSL
  ( Transformation, onAppNode, onCase, onLet, onLetNode, onPrimNode, onTickNode
  , onTyAppNode, anyConstructor, toTransformation )
import Clash.Rewrite.Types (curFun, primitives, typeEnv)

import Clash.Core.Scoped.Alpha (aeqTm, aeqTy)
import Clash.Core.Scoped.FreeVars
  (freeTmVarOccurrences, teleNameBinders, tmNameOccursIn)
import Clash.Core.Scoped.HasType (inferCoreTypeOf, primInfoType)
import Clash.Core.Scoped.NameMap
  (emptyNameMap, lookupNameMap, nameMapFromList, singletonNameMap)
import Clash.Core.Scoped.Subst
  (openAlt, teleIdVars, unsinkAlt, unsinkIdBinder, unsinkTerm)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term
  ( AbsBinder (..), appendTele, collectArgs, collectArgsTicks, collectTicks
  , mkApps, mkLams, mkTicks, openBndrs, partitionTicks, stripAllTicks, stripTicks )
import Clash.Core.Scoped.TermInfo (isCon, isLocalVar)
import Clash.Core.Scoped.Type
  (SplitFunForall (..), TypeView (..), isClassTy, normalizeType, piTeleTmArity, splitFunForallTy, tyNatSize, tyView)
import Clash.Core.Scoped.TypeEnv (TypeEnv, lookupTyCon)
import Clash.Core.Scoped.Util
  (LetGroup (..), inverseTopSortOrder, mkLets, mkVec, regroupLetBindings)
import Clash.Rewrite.Scoped
  ( FrameKind (..), TransformContext, changed, ctxFrames, isLambdaBodyKind
  , isTickKind, setChanged, tfScope )
import Clash.Rewrite.Scoped.Globals (isWorkFreeTerm)
import Clash.Rewrite.Scoped.Util
  ( LetBinding (..), isFromInt, isUntranslatable, keepBindings, mkTmBinderFor
  , openLetBindings, removeUnusedBinders, substVars )

-- | Remove unused let-bindings
deadCode :: Transformation
deadCode = toTransformation "deadCode" (onLet 'deadCodeWorker)

-- | The 'Let' handler of 'deadCode'
deadCodeWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Bind n l
  -> Term l
  -> NormalizeSession (Term n)
deadCodeWorker ctx e bind body =
  case removeUnusedBinders (tfScope ctx) bind body of
    Just t -> changed t
    Nothing -> return e
{-# SCC deadCodeWorker #-}

-- * removeUnusedExpr

-- | Replace the unused arguments of primitives by 'removedArg'
removeUnusedExpr :: Transformation
removeUnusedExpr = toTransformation "removeUnusedExpr"
  (  onPrimNode 'removeUnusedExprSpine
  <> onAppNode 'removeUnusedExprSpine
  <> onTyAppNode 'removeUnusedExprSpine
  <> onTickNode 'removeUnusedExprSpine
  <> onCase 'removeUnusedExprCase )

-- | The 'Case' handler of 'removeUnusedExpr': a single-alternative case whose
-- pattern binds no used variables is replaced by the alternative.
removeUnusedExprCase
  :: TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
removeUnusedExprCase _ e _subj _ty [Alt p@(DataPat _ TNil _) altExpr] =
  case unsinkAlt p altExpr of
    Just altExpr1 -> changed altExpr1
    Nothing -> return e
removeUnusedExprCase _ e _subj _ty _alts = return e

-- | The application-spine handlers of 'removeUnusedExpr': replace unused
-- arguments of primitives (and 'Vec's 'Vec.Cons') with 'removedArg'.
--
-- This collects the whole application spine with 'collectArgsTicks'. Say we
-- have an application:
--
--     f a b c
--
-- Then we're only interested in the root (@f a b c@), not in the inner nodes
-- (@f a@, @f a b@). At the root, 'collectArgsTicks' collects the full argument
-- list @[a, b, c]@; an inner node would only ever see a prefix of that (@[a]@,
-- @[a, b]@) at the same argument indices. So any argument an inner node could
-- remove, the root removes too, which makes the inner attempts wasted work.
-- This holds through ticks and type applications as well, since
-- 'collectArgsTicks' looks through both. We therefore skip any node whose
-- parent continues the spine (a 'KAppFun', 'KTyAppC', or 'KTickC' frame);
-- 'removeUnusedExpr' only routes spine nodes (@App@, @TyApp@, @Prim@, @Tick@)
-- here, so no explicit node-constructor check is needed. The
-- single-alternative-Case handling lives in 'removeUnusedExprCase' and is
-- unaffected: the spine only threads through applications, type applications,
-- and ticks, so a Case is never an inner node of a spine.
removeUnusedExprSpine
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
removeUnusedExprSpine ctx e = case ctxFrames ctx of
  KAppFun : _ -> return e
  KTyAppC : _ -> return e
  KTickC : _ -> return e
  _ -> case collectArgsTicks e of
    (p@(Prim pInfo), args, ticks) -> removeUnusedPrimArgs sc e p pInfo args ticks
    -- Replace any expression that creates a Vector of size 0 within the
    -- application of the Cons constructor, by the Nil constructor.
    (Data dc, [_, Right aTy, Right nTy, _, Left a, Left nil], ticks)
      | C.nameOcc (dcName dc) == Text.showt 'Vec.Cons -> do
          te <- Lens.view typeEnv
          case runExcept (tyNatSize te sc nTy) of
            Right 0
              | (con, _) <- collectArgs nil
              , not (isCon con)
              -> let eTy = inferCoreTypeOf te sc e
                     v = fromMaybe (error "removeUnusedExpr: failed to create Vec DCs") $ do
                       TyConApp vecTcNm _ <- pure (tyView eTy)
                       vecTc <- lookupTyCon te vecTcNm
                       [nilCon, consCon] <- pure (tyConDataCons vecTc)
                       return (mkTicks (mkVec te sc nilCon consCon aTy 1 [a]) ticks)
                 in  changed v
            _ -> return e
    _ -> return e
 where
  sc = tfScope ctx
{-# SCC removeUnusedExprSpine #-}

-- | Replace the unused arguments of an applied primitive by 'removedArg'
removeUnusedPrimArgs
  :: forall n
   . (HasCallStack, Distinct n)
  => Scope n
  -> Term n
  -- ^ The application
  -> Term n
  -- ^ The primitive
  -> PrimInfo
  -> [Either (Term n) (Type n)]
  -> [TickInfo n]
  -> NormalizeSession (Term n)
removeUnusedPrimArgs sc e p pInfo args ticks = do
  bbM <- HashMap.lookup (primName pInfo) <$> Lens.view primitives
  let
    usedArgs0 =
      case Monad.join (extractPrim <$> bbM) of
        Just (BlackBoxHaskell{usedArguments}) ->
          case usedArguments of
            UsedArguments used -> Just used
            IgnoredArguments ignored -> Just ([0..length args - 1] \\ ignored)
        Just (BlackBox pNm _ _ _ _ _ _ _ _ _ inc r ri templ) -> Just $
          if | isFromInt pNm -> [0,1,2]
             | primName pInfo `elem` [ Text.showt 'dontApplyInHDL
                                       , Text.showt 'Vec.splitAt
                                       ] -> [0,1]
             | otherwise -> concat [ concatMap getUsedArguments r
                                   , concatMap getUsedArguments ri
                                   , getUsedArguments templ
                                   , concatMap (getUsedArguments . snd) inc ]
        _ ->
          Nothing

  case usedArgs0 of
    Nothing ->
      return e
    Just usedArgs1 -> do
      te <- Lens.view typeEnv
      (args1, Monoid.getAny -> hasChanged) <- listen (go te 0 usedArgs1 args)
      if hasChanged then
        return (mkApps (mkTicks p ticks) args1)
      else
        return e
 where
  arity = case splitFunForallTy (primInfoType pInfo :: Type 'VoidS) of
    SplitFunForall tele _ -> piTeleTmArity tele

  go :: TypeEnv -> Int -> [Int] -> [Either (Term n) (Type n)]
     -> NormalizeSession [Either (Term n) (Type n)]
  go _ _ _ [] = return []
  go te !n used (Right ty:args') = do
    args'' <- go te n used args'
    return (Right ty : args'')
  go te !n used (Left tm : args') = do
    args'' <- go te (n+1) used args'
    case tm of
      TyApp (Prim p0) _
        | primName p0 == primName removedArg
        -> return (Left tm : args'')
      _ -> do
        let ty = inferCoreTypeOf te sc tm
            p' = TyApp (Prim removedArg) ty
        if n < arity && n `notElem` used
           then changed (Left p' : args'')
           else return  (Left tm : args'')

-- * Merging let-bindings

-- | Merge directly nested let-expressions, @let bs1 in let bs2 in ... in e@,
-- into a single telescope of binders, @bs1 ++ bs2 ++ ...@, and their
-- right-hand sides, in the scope of all of them. Also gives the number of
-- let-expressions merged.
collectLets
  :: Distinct n
  => Scope n
  -> Bind n l
  -> Term l
  -> (forall o. DExt n o => Scope o -> Tele IdBinder n o -> [Term o] -> Term o -> Int -> r)
  -> r
collectLets sc bind body k = openLetBindings sc bind body $ \sc1 bs1 lbs1 body1 ->
  case body1 of
    Let bind2 body2 -> collectLets sc1 bind2 body2 $ \sc2 bs2 xs2 body3 levels ->
      k sc2 (appendTele bs1 bs2) (map (sink . lbRhs) lbs1 ++ xs2) body3 (levels + 1)
    _ -> k sc1 bs1 (map lbRhs lbs1) body1 1

-- | The number of free occurrences of a term variable in a term
occurrences :: TmName n -> Term n -> Int
occurrences x e = fromMaybe 0 (lookupNameMap x (freeTmVarOccurrences e))

-- * flattenLet

-- | Flatten's letrecs after `inlineCleanup`
--
-- `inlineCleanup` sometimes exposes additional possibilities for `caseCon`,
-- which then introduces let-bindings in what should be ANF. This transformation
-- flattens those nested let-bindings again.
--
-- NB: must only be called in the cleaning up phase.
--
-- It never makes a 'NonRec' binding recursive: its right-hand side doesn't
-- refer to the binder by construction. So a let-expression @let x = g x in e@
-- in a right-hand side, where @g x@ refers to a variable @x@ that the binder
-- shadows, is inlined (if the other conditions hold).
flattenLet :: Transformation
flattenLet = toTransformation "flattenLet" (onLetNode 'flattenLetWorker)

-- | The 'Let' handler of 'flattenLet'
flattenLetWorker
  :: forall n
   . (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
flattenLetWorker ctx e = case e of
  Let bind body -> collectLets sc bind body $ \scO bs xs body1 levels -> do
    -- Merging a let-expression in the body is a change too, see #1837
    Monad.when (levels > 1) setChanged
    let nonRec = levels == 1 && case bind of { NonRec {} -> True; Rec {} -> False }
    mergeLetRhss scO xs body1 $ \scY ext out bodyY merged ->
      let anyChange = levels > 1 || merged in
      case (bs, ext, out) of
        (TCons b TNil, TNil, [(_, e1)]) -> flattenSingle scY b e1 bodyY nonRec anyChange
        _ | not anyChange -> return e
          | otherwise ->
              -- The bindings in their new order: the bindings of a merged
              -- let-expression right before the binding it was the right-hand
              -- side of
              let tele = appendTele bs ext
                  xsY = IntMap.elems (IntMap.fromList out)
              in  regroupLetBindings sc tele xsY bodyY [RecGroup (map fst out)] $
                    \_ binds body' -> return (mkLets binds body')
  _ -> return e
 where
  sc = tfScope ctx

  -- Inline a single binding into the body when it doesn't perform any work or
  -- is used only once, unless it's recursive
  flattenSingle
    :: (Distinct o, DExt n o)
    => Scope o -> IdBinder n o -> Term o -> Term o -> Bool -> Bool
    -> NormalizeSession (Term n)
  flattenSingle scO b e1 body nonRec anyChange = do
    let x = tmNameOf b
        occ = occurrences x body
    inline <- if | occ == 0 -> pure False
                 | occ < 2 -> pure True
                 | otherwise -> isWorkFreeTerm e1
    if inline && not (x `tmNameOccursIn` e1)
      then case unsinkIdBinder b (substVars scO (singletonNameMap x e1) body) of
        Just body1 -> changed body1
        Nothing -> error "flattenLet: an inlined binder remains"
      else if | not anyChange -> return e
              | nonRec, Just e1' <- unsinkIdBinder b e1 -> return (Let (NonRec b e1') body)
              | otherwise -> return (Let (Rec (TCons b TNil) [e1]) body)
{-# SCC flattenLetWorker #-}

-- | Merge the let-expressions in the right-hand sides of let-bindings into
-- the bindings, see 'flattenLet'. Gives the binders of the merged
-- let-expressions, as an extension of the given scope, and the right-hand
-- sides in their new order, by their positions in the binders: first the
-- given bindings, then those of the telescope. Also says whether anything was
-- merged.
mergeLetRhss
  :: forall o r
   . Distinct o
  => Scope o
  -> [Term o]
  -- ^ The right-hand sides of the bindings
  -> Term o
  -- ^ The body
  -> (forall y. DExt o y => Scope y -> Tele IdBinder o y -> [(Int, Term y)] -> Term y -> Bool -> NormalizeSession r)
  -> NormalizeSession r
mergeLetRhss sc0 xs0 body0 k = go sc0 TNil (length xs0) [] (zip [0..] xs0) body0 False
 where
  -- The scope, the merged binders, the next position, the right-hand sides
  -- done (in reverse order), the right-hand sides to do, the body, and whether
  -- anything was merged
  go
    :: DExt o x
    => Scope x
    -> Tele IdBinder o x
    -> Int
    -> [(Int, Term x)]
    -> [(Int, Term x)]
    -> Term x
    -> Bool
    -> NormalizeSession r
  go sc ext _ done [] body merged = k sc ext (reverse done) body merged
  go sc ext next done ((pos, rhs) : todo) body merged = case collectTicks rhs of
    (Let ibind ibody, ticks) ->
      let (srcTicks, nmTicks) = partitionTicks ticks in
      openLetBindings sc ibind ibody $ \sc1 ibs ilbs ibody1 -> do
        let nm t = mkTicks t (map sink nmTicks)
            -- Merge the bindings, followed by the binding of the right-hand
            -- side, distributing the name ticks of the let-expression over
            -- all of them
            mergeAll parentRhs = do
              setChanged
              let inner = zip [next ..] [nm (lbRhs lb) | lb <- ilbs]
                  done1 = (pos, nm parentRhs) : reverse inner ++ map (second sink) done
              go sc1 (appendTele ext ibs) (next + length ilbs) done1
                 (map (second sink) todo) (sink body) True
        case ilbs of
          -- Inline a single binding into the body of the let-expression when
          -- it doesn't perform any work or is used only once
          [lb2] -> do
            let x2 = lbName lb2
                e2 = lbRhs lb2
                occ = occurrences x2 ibody1
            inline <- if | occ == 0 -> pure False
                         | occ < 2 -> pure True
                         | otherwise -> isWorkFreeTerm e2
            if | not inline -> mergeAll (mkTicks ibody1 (map sink srcTicks))
                 -- Except when the binding is recursive!
               | x2 `tmNameOccursIn` e2 -> mergeAll ibody1
               | otherwise ->
                   case unsinkTerm (teleNameBinders ibs) (substVars sc1 (singletonNameMap x2 e2) ibody1) of
                     Just ibody2 -> do
                       setChanged
                       -- Only apply srcTicks to the body
                       let rhs1 = mkTicks (mkTicks ibody2 srcTicks) nmTicks
                       go sc ext next ((pos, rhs1) : done) todo body True
                     Nothing -> error "flattenLet: an inlined binder remains"
          _ -> mergeAll (mkTicks ibody1 (map sink srcTicks))
    _ -> go sc ext next ((pos, rhs) : done) todo body merged

-- * recToLetRec

-- | Turn a  normalized recursive function, where the recursive calls only pass
-- along the unchanged original arguments, into let-recursive function. This
-- means that all recursive calls are replaced by the same variable reference as
-- found in the body of the top-level let-expression.
--
-- It only applies to a term in normal form
-- ('Clash.Netlist.Util.splitNormalized'): lambdas around let-expressions whose
-- body is a variable. It doesn't fire if the variable is one of the bindings
-- that it would replace by the variable.
recToLetRec
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
recToLetRec ctx e
  | not (null (ctxFrames ctx)) = return e
  | otherwise = do
      (fn, _) <- Lens.use curFun
      te <- Lens.view typeEnv
      openBndrs (tfScope ctx) e $ \scA absBs body0 -> case idTele absBs of
        -- Not in normal form: type arguments
        Nothing -> return e
        Just idBs -> case collectTicks body0 of
          (Let bind body1, ticks) -> collectLets scA bind body1 $ \scL bs xs0 res _ ->
            let args = map sink (teleIdVars idBs)
                -- The ticks of the let-expression go to its bindings, see
                -- 'Clash.Netlist.Util.splitNormalized'
                xs = map (`mkTicks` map sink ticks) xs0
                lbs = zipWith letBinding (teleIdVars bs) xs
                (toInline, others) = List.partition (eqApp te scL fn args . lbRhs) lbs
                resV = stripTicks res
                resInlined = case resV of
                  Var _ _ r -> any ((== r) . lbName) toInline
                  _ -> False
            in  case (toInline, others) of
                  (_:_, _:_)
                    | isVarTerm resV && not resInlined ->
                        let m = nameMapFromList [(lbName lb, resV) | lb <- toInline]
                            others1 = [lb {lbRhs = substVars scL m (lbRhs lb)} | lb <- others]
                        in  case keepBindings scA bs others1 resV of
                              Just body2 -> changed (mkLams body2 idBs)
                              Nothing -> error "recToLetRec: an inlined binder remains"
                  _ -> return e
          _ -> return e
 where
  letBinding v x = case v of
    Var occ ty nm -> LetBinding occ ty nm x
    _ -> error "recToLetRec: not a variable"

  isVarTerm = \case
    Var {} -> True
    GlobalVar {} -> True
    _ -> False

  -- This checks whether things are semantically equal. For example, say we
  -- have:
  --
  --   x :: (a, (b, c))
  --
  -- and
  --
  --   y :: (a, (b, c))
  --
  -- If we can determine that 'y' is constructed solely using the
  -- corresponding fields in 'x', then we can say they are semantically
  -- equal. The algorithm below keeps track of what (sub)field it is
  -- constructing, and checks if the field-expression projects the
  -- corresponding (sub)field from the target variable.
  --
  -- TODO: See Note [Breaks on constants and predetermined equality]
  --
  -- Since 'aeqTm' looks at ticks when determining equality, all ticks are
  -- removed with 'stripAllTicks'.
  eqApp :: Distinct l => TypeEnv -> Scope l -> GlobalId -> [Term l] -> Term l -> Bool
  eqApp te sc fn args (collectArgs . stripAllTicks -> (GlobalVar g, args'))
    | g == fn
    , let args2 = Either.lefts args'
    , length args == length args2
    = and (zipWith (eqArg te sc) args args2)
  eqApp _ _ _ _ _ = False

  eqArg :: Distinct l => TypeEnv -> Scope l -> Term l -> Term l -> Bool
  eqArg _ _ v1 v2@Var{}
    = aeqTm v1 v2
  eqArg te sc v1 v2@(collectArgs -> (Data _, args'))
    | let t1 = normalizeType te sc (inferCoreTypeOf te sc v1)
    , let t2 = normalizeType te sc (inferCoreTypeOf te sc v2)
    , aeqTy t1 t2
    = if isClassConstraint t1 then
        -- Class constraints are equal if their types are equal, so we can
        -- take a shortcut here.
        True
      else
        -- Check whether all arguments to the data constructor are projections
        and (zipWith (eqDat te sc v1) (map pure [0..]) (Either.lefts args'))
  eqArg _ _ _ _
    = False

  -- Recursively check whether a term /e/ is semantically equal to some
  -- variable /v/. Currently it can only assert equality when /e/:
  --
  --   * is syntactically equal to /v/; or
  --   * is constructed out of projections of /v/; or
  --   * is constructed out of type-equal class dictionaries
  --
  -- or a mix of these.
  --
  -- Note [Breaks on constants and predetermined equality]
  -- This function currently breaks if:
  --
  --   * One or more subfields are constants. Constants might have been
  --     inlined for the construction, instead of being a projection of the
  --     target variable.
  --
  --   * One or more subfields are determined to be equal and one is simply
  --     swapped / replaced by the other. For example, say we have
  --     `x :: (a, a)`. If GHC determines that both elements of the tuple will
  --     always be the same, it might replace the (semantically equal to 'x')
  --     construction of `y` with `(fst x, fst x)`.
  eqDat :: Distinct l => TypeEnv -> Scope l -> Term l -> [Int] -> Term l -> Bool
  eqDat te sc v fTrace (collectArgs -> (Data _, args)) =
    and (zipWith (eqDat te sc v) (map (:fTrace) [0..]) (Either.lefts args))
  eqDat te sc v1 fTrace v2 =
    case stripProjection sc (reverse fTrace) v1 v2 of
      Just [] -> True
      -- A class dictionary subfield is uniquely determined by its type, so we
      -- don't require it to be projected from the exact corresponding field.
      -- GHC routinely shares such dictionaries (e.g. the @KnownDomain@ inside
      -- a @HiddenClockResetEnable@) via CSE, projecting them from a different
      -- but type-equal field of the target than the one being reconstructed.
      _ -> isClassTy te (inferCoreTypeOf te sc v2)

  stripProjection :: Distinct l => Scope l -> [Int] -> Term l -> Term l -> Maybe [Int]
  stripProjection sc fTrace0 vTarget0 (Case v _ [alt]) = do
    -- Get projection made in subject of case:
    fTrace1 <- stripProjection sc fTrace0 vTarget0 v
    -- Extract projection of this case statement. Subsequent calls to
    -- 'stripProjection' will check if new target is actually used.
    (n, fTrace2) <- List.uncons fTrace1
    openAlt sc alt $ \sc1 p r -> case p of
      DataPat _ _ ids -> do
        vTarget1 <- List.indexMaybe (teleIdVars ids) n
        stripProjection sc1 fTrace2 vTarget1 r
      _ -> Nothing
  stripProjection _ fTrace vTarget@Var{} v@Var{} =
    if aeqTm vTarget v then Just fTrace else Nothing
  stripProjection _ _fTrace _vTarget _v =
    Nothing
{-# SCC recToLetRec #-}

-- | The telescope of term binders of a telescope of abstractions, if it binds
-- no type variables
idTele :: Tele AbsBinder n l -> Maybe (Tele IdBinder n l)
idTele = \case
  TNil -> Just TNil
  TCons (AbsId b) bs -> TCons b <$> idTele bs
  TCons (AbsTy _) _ -> Nothing

-- | Whether a type is a class constraint: a constraint tuple, or the type of
-- the dictionary of a class
isClassConstraint :: Type n -> Bool
isClassConstraint (tyView -> TyConApp nm0 _) =
  if -- Constraint tuple. The host module is 'GHC.Classes' on GHC <= 9.12 and
     -- 'GHC.Internal.Classes' on GHC >= 9.14; both end in '.Classes.(%'.
     | ".Classes.(%" `Text.isInfixOf` nm1 -> True
     -- Constraint class:
     | "C:" `Text.isInfixOf` nm2 -> True
     | otherwise -> False
 where
  nm1 = C.nameOcc nm0
  nm2 = snd (Text.breakOnEnd "." nm1)
isClassConstraint _ = False

-- * simpleCSE

-- | Simplified CSE, only works on let-bindings, does an inverse topological
-- sort of the let-bindings and then works from top to bottom
--
-- XXX: Check whether inverse top-sort followed by single traversal removes as
-- many binders as the previous "apply-until-fixpoint" approach in the presence
-- of recursive groups in the let-bindings. If not but just for checking whether
-- changes to transformation affect the eventual size of the circuit, it would
-- be really helpful if we tracked circuit size in the regression/test suite.
-- On the two examples that were tested, Reducer and PipelinesViaFolds, this new
-- version of CSE removed the same amount of let-binders.
simpleCSE :: Transformation
simpleCSE = toTransformation "CSE" (onLet 'simpleCSEWorker)

-- | The 'Let' handler of 'simpleCSE'
simpleCSEWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Bind n l
  -> Term l
  -> NormalizeSession (Term n)
simpleCSEWorker ctx term bind body = case bind of
  -- A single binding has nothing to be shared with
  NonRec {} -> return term
  Rec {} -> openLetBindings sc bind body $ \scL bs lbs body1 ->
    let lbsArr = IntMap.fromList (zip [0..] lbs)
        order = inverseTopSortOrder bs (map lbRhs lbs)
    in  do
      ((m, kept), Monoid.getAny -> replaced) <-
        listen (reduceBinders scL emptyNameMap [] [lbsArr IntMap.! i | i <- order])
      if replaced
        then do
          -- The substitution is applied to the kept bindings too: the
          -- bindings of a recursive group can refer to a binding that is
          -- only replaced later in the order.
          let substM = substVars scL m
              kept1 = [lb {lbRhs = substM (lbRhs lb)} | lb <- kept]
          case keepBindings sc bs kept1 (substM body1) of
            Just t -> changed t
            Nothing -> error "simpleCSE: a replaced binder remains"
        else return term
 where
  sc = tfScope ctx
{-# SCC simpleCSEWorker #-}

-- * topLet

-- | Ensure that top-level lambda's eventually bind a let-expression of which
-- the body is a variable-reference.
topLet :: Transformation
topLet = anyConstructor "topLet" 'topLetWorker

-- | The worker of 'topLet': a plain rewrite, offered at every node
topLetWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
topLetWorker ctx e
  | all (\c -> isLambdaBodyKind c || isTickKind c) (ctxFrames ctx) = case e of
      Tick {} -> return e
      Let bind body
        | isLocalVar body -> return e
        | otherwise -> openLetBindings sc bind body $ \scL bs lbs body1 -> do
            untranslatable <- isUntranslatable False scL body1
            if untranslatable
              then return e
              else do
                te <- Lens.view typeEnv
                -- The new binder joins the bindings in a recursive group,
                -- which needs the right-hand side of a 'NonRec' binding to
                -- be moved into the scope of its binder: opening it renamed
                -- the binder if it could capture a variable.
                --
                -- NB: We cannot nest the new binding instead:
                --
                -- Let bind (Let (NonRec result body) (Var result))
                --
                -- 'flattenLet' would later inline the single use of its
                -- binder, e.g. after this let-expression was inlined into an
                -- argument of 'bindSimIO#', leaving a let-expression whose
                -- body is not a variable. That makes
                -- tests/shouldwork/SimIO/Test00.hs fail.
                mkTmBinderFor te scL (C.mkUnsafeSystemName "result" 0) body1 $ \_ b ->
                  changed (Let (Rec (appendTele bs (TCons b TNil))
                                    (map (sink . lbRhs) lbs ++ [sink body1]))
                               (idBinderVar b))
      _ -> do
        untranslatable <- isUntranslatable False sc e
        if untranslatable
          then return e
          else do
            te <- Lens.view typeEnv
            mkTmBinderFor te sc (C.mkUnsafeSystemName "result" 0) e $ \_ b ->
              changed (Let (NonRec b e) (idBinderVar b))
  | otherwise = return e
 where
  sc = tfScope ctx
{-# SCC topLetWorker #-}
