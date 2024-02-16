{-|
  Copyright  :  (C) 2015-2016, University of Twente,
                    2021-2024, QBayLogic B.V.
                    2022,      LumiGuide Fietsdetectie B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The 'disjointExpressionConsolidation' transformation lifts applications of
  global binders out of alternatives of case-statements.

  e.g. It converts:

  > case x of
  >   A -> f 3 y
  >   B -> f x x
  >   C -> h x

  into:

  > let f_arg0 = case x of {A -> 3; B -> x}
  >     f_arg1 = case x of {A -> y; B -> x}
  >     f_out  = f f_arg0 f_arg1
  > in  case x of
  >       A -> f_out
  >       B -> f_out
  >       C -> h x
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations.DEC
  ( disjointExpressionConsolidation
  ) where

import Control.Concurrent.Supply (splitSupply)
import Control.Lens ((^.), _1)
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Data.Bifunctor (first, second)
import Data.Bits ((.&.), complement)
import Data.Coerce (coerce)
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.Graph as Graph
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (All(..))
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)

#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.Make (chunkify, mkChunkified)
#elif MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Utils (chunkify, mkChunkified)
#else
import HsUtils (chunkify, mkChunkified)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Settings.Constants (mAX_TUPLE_SIZE)
#else
import Constants (mAX_TUPLE_SIZE)
#endif

-- internal
import Clash.Core.DataCon    (DataCon)
import Clash.Core.Evaluator.Types  (whnf')
import Clash.Core.FreeVars
  (termFreeVars', typeFreeVars', localVarsDoNotOccurIn)
import Clash.Core.HasType
import Clash.Core.Literal (Literal(..))
import Clash.Core.Name (nameOcc)
import Clash.Core.Term
  ( Alt, LetBinding, Pat(..), PrimInfo(..), Term(..), TickInfo(..)
  , collectArgs, collectArgsTicks, mkApps, mkTicks, patIds, stripTicks)
import Clash.Core.TyCon (TyConMap, TyConName, tyConDataCons)
import Clash.Core.Type
  (Type, TypeView (..), isPolyFunTy, mkTyConApp, splitFunForallTy, tyView)
import Clash.Core.Util (mkInternalVar, mkSelectorCase, sccLetBindings)
import Clash.Core.Var (isGlobalId, isLocalId, varName)
import Clash.Core.VarEnv
  ( InScopeSet, elemInScopeSet, extendInScopeSet, extendInScopeSetList
  , notElemInScopeSet, unionInScope)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Normalize.Transformations.Letrec (deadCode)
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Rewrite.Combinators (bottomupR)
import Clash.Rewrite.Types
import Clash.Rewrite.Util (changed, isFromInt, isUntranslatableType)
import Clash.Rewrite.WorkFree (isConstant)
import Clash.Util (MonadUnique, curLoc)

-- | This transformation lifts applications of global binders out of
-- alternatives of case-statements.
--
-- e.g. It converts:
--
-- @
-- case x of
--   A -> f 3 y
--   B -> f x x
--   C -> h x
-- @
--
-- into:
--
-- @
-- let f_arg0 = case x of {A -> 3; B -> x}
--     f_arg1 = case x of {A -> y; B -> x}
--     f_out  = f f_arg0 f_arg1
-- in  case x of
--       A -> f_out
--       B -> f_out
--       C -> h x
-- @
disjointExpressionConsolidation :: HasCallStack => NormRewrite
disjointExpressionConsolidation ctx@(TransformContext isCtx _) e@(Case _scrut _ty _alts@(_:_:_)) = do
    -- Collect all (the applications of) global binders (and certain primitives)
    -- that would be interesting to share out of the case-alternatives.
    (_,isCollected,collected) <- collectGlobals isCtx [] [] e
    -- Filter those that are used at most once in every (nested) branch.
    let disJoint = filter (isDisjoint . snd . snd) collected
    if null disJoint
       then return e
       else do
         -- For every to-lift expression create (the generalization of):
         --
         -- let fargs = case x of {A -> (3,y); B -> (x,x)}
         -- in  f (fst fargs) (snd fargs)
         --
         -- the let-expression is not created when `f` has only one (selectable)
         -- argument
         --
         -- NB: mkDisJointGroup needs the context InScopeSet, isCtx, to determine
         -- whether expressions reference variables from the context, or
         -- variables inside a let-expression inside one of the alternatives.
         lifted <- mapM (mkDisjointGroup isCtx) disJoint
         tcm    <- Lens.view tcCache
         -- Create let-binders for all of the lifted expressions
         --
         -- NB: Because we will be substituting under binders we use the collected
         -- inScopeSet, isCollected, which also contains all the binders
         -- created inside all of the alternatives. With this inScopeSet, we
         -- ensure that the let-bindings we create here won't be accidentally
         -- captured by binders inside the case-alternatives.
         (_,funOutIds) <- List.mapAccumLM (mkFunOut tcm)
                                          isCollected
                                          (zip disJoint lifted)
         -- Create "substitutions" of the form [f X Y := f_out]
         let substitution = zip (map fst disJoint) (map Var funOutIds)
         -- For all of the lifted expression: substitute occurrences of the
         -- disjoint expressions (f X Y) by a variable reference to the lifted
         -- expression (f_out)
         let isCtx1 = extendInScopeSetList isCtx funOutIds
         lifted1 <- substLifted isCtx1 substitution lifted
         -- Do the same for the actual case expression
         (e1,_,_) <- collectGlobals isCtx1 substitution [] e
         -- Let-bind all the lifted function
         let lb = Letrec (zip funOutIds lifted1) e1
         -- Do an initial dead-code elimination pass, as `mkDisJoint` doesn't
         -- clean-up unused let-binders.
         lb1 <- bottomupR deadCode ctx lb
         changed lb1
  where
    -- Make the let-binder for the lifted expressions
    mkFunOut tcm isN ((fun,_),(eLifted,_)) = do
      let ty  = inferCoreTypeOf tcm eLifted
          nm  = case collectArgs fun of
                   (Var v,_)  -> nameOcc (varName v)
                   (Prim p,_) -> primName p
                   _          -> "complex_expression_"
          nm1 = last (Text.splitOn "." nm) `Text.append` "Out"
      nm2 <- mkInternalVar isN nm1 ty
      return (extendInScopeSet isN nm2,nm2)

    -- Substitute inside the lifted expressions
    --
    -- In case you are wondering why this function isn't simply
    --
    -- > mapM (\s (eL,seen) -> collectGlobal isN s seen eL) substitution lifted
    --
    -- then that's because we have e.g. the list of "substitutions":
    --
    -- [foo _ _ := foo_out; bar _ _ := bar_out]
    --
    -- and if we were to apply that to a lifted expression, which is going
    -- to be of the form `foo (case ...) (case ...)` then we would end up
    -- with let-bindings that are simply:
    --
    -- > let foo_out = foo_out ; bar_out = bar_out
    --
    -- instead of the desired
    --
    -- > let foo_out = foo ((case ...)[foo _ _ := foo_out; bar _ _ := bar_out])
    -- >                   ((case ...)[foo _ _ := foo_out; bar _ _ := bar_out])
    -- >     bar_out = bar ((case ...)[foo _ _ := foo_out; bar _ _ := bar_out])
    -- >                   ((case ...)[foo _ _ := foo_out; bar _ _ := bar_out])
    --
    -- So what we do is that for every lifted-expression we make sure that the
    -- 'substitution' never contains the self-substitution, so we end up with:
    --
    -- > let foo_out = (foo (case ...) (case ...))[bar _ _ := bar_out]
    --       bar_out = (bar (case ...) (case ...))[foo _ _ := foo_out]
    --
    -- We used to have a different approach, see commit
    -- 73d237017c4a5fff0c49bb72c9c4d5f6c68faf69
    --
    -- But that lead to the generation of combinational loops. Now that we no
    -- longer traverse into recursive groups of let-bindings, the issue #1316
    -- that the above commit tried to solve, no longer shows up.
    substLifted isN substitution lifted = do
      -- remove the self-substitutions for the respective lifted expressions
      let subsMatrix = l2m substitution
      lifted1 <- Monad.zipWithM (\s (eL,seen) -> collectGlobals isN s seen eL)
                                 subsMatrix
                                 lifted
      return (map (^. _1) lifted1)

    l2m = go []
     where
      go _  []     = []
      go xs (y:ys) = (xs ++ ys) : go (xs ++ [y]) ys

disjointExpressionConsolidation _ e = return e
{-# SCC disjointExpressionConsolidation #-}

data CaseTree a
  = Leaf a
  | LB [LetBinding] (CaseTree a)
  | Branch Term [(Pat,CaseTree a)]
  deriving (Eq,Show,Functor,Foldable)

-- | Test if a 'CaseTree' collected from an expression indicates that
-- application of a global binder is disjoint: occur in separate branches of a
-- case-expression.
isDisjoint :: CaseTree ([Either Term Type])
           -> Bool
isDisjoint (Branch _ [_]) = False
isDisjoint ct = go ct
  where
    go (Leaf _)             = False
    go (LB _ ct')           = go ct'
    go (Branch _ [])        = False
    go (Branch _ [(_,x)])   = go x
    go b@(Branch _ (_:_:_)) = allEqual (map Either.rights (Foldable.toList b))

-- Remove empty branches from a 'CaseTree'
removeEmpty :: Eq a => CaseTree [a] -> CaseTree [a]
removeEmpty l@(Leaf _) = l
removeEmpty (LB lb ct) =
  case removeEmpty ct of
    Leaf [] -> Leaf []
    ct'     -> LB lb ct'
removeEmpty (Branch s bs) =
  case filter ((/= (Leaf [])) . snd) (map (second removeEmpty) bs) of
    []  -> Leaf []
    bs' -> Branch s bs'

-- | Test if all elements in a list are equal to each other.
allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of an expression. Also substitute truly disjoint applications of globals by a
-- reference to a lifted out application.
collectGlobals
  :: InScopeSet
  -> [(Term,Term)]
  -- ^ Substitution of (applications of) a global binder by a reference to a
  -- lifted term.
  -> [Term]
  -- ^ List of already seen global binders
  -> Term
  -- ^ The expression
  -> NormalizeSession (Term, InScopeSet, [(Term, ([Term], CaseTree [Either Term Type]))])
collectGlobals is0 substitution seen (Case scrut ty alts) = do
  rec (alts1, isAlts, collectedAlts) <-
        collectGlobalsAlts is0 substitution seen scrut1 alts
      (scrut1, isScrut, collectedScrut) <-
        collectGlobals is0 substitution (map fst collectedAlts ++ seen) scrut
  return ( Case scrut1 ty alts1
         , unionInScope isAlts isScrut
         , collectedAlts ++ collectedScrut )

collectGlobals is0 substitution seen e@(collectArgsTicks -> (fun, args@(_:_), ticks))
  | not (isConstant e) = do
    tcm <- Lens.view tcCache
    bndrs <- Lens.use bindings
    evaluate <- Lens.view evaluator
    ids <- Lens.use uniqSupply
    let (ids1,ids2) = splitSupply ids
    uniqSupply Lens..= ids2
    gh <- Lens.use globalHeap
    let eval = (Lens.view Lens._3) . whnf' evaluate bndrs mempty tcm gh ids1 is0 False
    let eTy  = inferCoreTypeOf tcm e
    untran <- isUntranslatableType False eTy
    case untran of
      -- Don't lift out non-representable values, because they cannot be let-bound
      -- in our desired normal form.
      False -> do
        -- Look for, and substitute by, disjoint applications of globals in
        -- the arguments first before considering the current term in function
        -- position. Doing it in the other order (this term in function position
        -- first, followed by arguments) resulted in issue #1322
        (args1,isArgs,collectedArgs) <-
          collectGlobalsArgs is0 substitution seen args
        let seenInArgs = map fst collectedArgs ++ seen
            isInteresting = interestingToLift is0 eval fun args ticks
        case isInteresting of
          Just fun1 | fun1 `notElem` seenInArgs -> do
            let e1 = Maybe.fromMaybe (mkApps fun1 args1) (List.lookup fun1 substitution)
            -- This function is lifted out an environment with the currently 'seen'
            -- binders. When we later apply substitution, we need to start with this
            -- environment, otherwise we perform incorrect substitutions in the
            -- arguments.
            return (e1,isArgs,(fun1,(seen,Leaf args1)):collectedArgs)
          _ -> return (mkApps (mkTicks fun ticks) args1, isArgs, collectedArgs)
      _ -> return (e,is0,[])

-- FIXME: This duplicates A LOT of let-bindings, where I just pray that after
-- the ANF, CSE, and DeadCodeRemoval pass all duplicates are removed.
--
-- I think we should be able to do better, but perhaps we cannot fix it here.
collectGlobals is0 substitution seen (Letrec lbs body) = do
  let is1 = extendInScopeSetList is0 (map fst lbs)
  (body1,isBody,collectedBody) <-
    collectGlobals is1 substitution seen body
  (lbs1,isBndrs,collectedBndrs) <-
    collectGlobalsLbs is1 substitution (map fst collectedBody ++ seen) lbs
  return ( Letrec lbs1 body1
         , unionInScope isBody isBndrs
         , map (second (second (LB lbs1))) (collectedBody ++ collectedBndrs)
         )

collectGlobals is0 substitution seen (Tick t e) = do
  (e1,is1,collected) <- collectGlobals is0 substitution seen e
  return (Tick t e1, is1, collected)

collectGlobals is0 _ _ e = return (e,is0,[])

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of application arguments. Also substitute truly disjoint
-- applications of globals by a reference to a lifted out application.
collectGlobalsArgs
  :: InScopeSet
  -> [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> [Either Term Type] -- ^ The list of arguments
  -> NormalizeSession
       ( [Either Term Type]
       , InScopeSet
       , [(Term, ([Term], CaseTree [(Either Term Type)]))]
       )
collectGlobalsArgs is0 substitution seen args = do
    ((is1,_),(args',collected)) <- second unzip <$> List.mapAccumLM go (is0,seen) args
    return (args',is1,concat collected)
  where
    go (isN0,s) (Left tm) = do
      (tm',isN1,collected) <- collectGlobals isN0 substitution s tm
      return ((isN1,map fst collected ++ s),(Left tm',collected))
    go (isN,s) (Right ty) = return ((isN,s),(Right ty,[]))

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of alternatives. Also substitute truly disjoint applications of
-- globals by a reference to a lifted out application.
collectGlobalsAlts ::
     InScopeSet
  -> [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> Term -- ^ The subject term
  -> [Alt] -- ^ The list of alternatives
  -> NormalizeSession
       ( [Alt]
       , InScopeSet
       , [(Term, ([Term], CaseTree [(Either Term Type)]))]
       )
collectGlobalsAlts is0 substitution seen scrut alts = do
    (is1,(alts',collected)) <- second unzip <$> List.mapAccumLM go is0 alts
    let collectedM  = map (Map.fromList . map (second (second (:[])))) collected
        collectedUN = Map.unionsWith (\(l1,r1) (l2,r2) -> (List.nub (l1 ++ l2),r1 ++ r2)) collectedM
        collected'  = map (second (second (Branch scrut))) (Map.toList collectedUN)
    return (alts',is1,collected')
  where
    go isN0 (p,e) = do
      let isN1 = extendInScopeSetList isN0 (snd (patIds p))
      (e',isN2,collected) <- collectGlobals isN1 substitution seen e
      return (isN2,((p,e'),map (second (second (p,))) collected))

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of let-bindings. Also substitute truly disjoint applications of
-- globals by a reference to a lifted out application.
collectGlobalsLbs ::
     InScopeSet
  -> [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> [LetBinding] -- ^ The list let-bindings
  -> NormalizeSession
       ( [LetBinding]
       , InScopeSet
       , [(Term, ([Term], CaseTree [(Either Term Type)]))]
       )
collectGlobalsLbs is0 substitution seen lbs = do
    let lbsSCCs = sccLetBindings lbs
    ((is1,_),(lbsSCCs1,collected)) <-
      second unzip <$> List.mapAccumLM go (is0,seen) lbsSCCs
    return (Graph.flattenSCCs lbsSCCs1,is1,concat collected)
  where
    go :: (InScopeSet,[Term]) -> Graph.SCC LetBinding
       -> NormalizeSession
            ( (InScopeSet, [Term])
            , ( Graph.SCC LetBinding
              , [(Term, ([Term], CaseTree [(Either Term Type)]))]
              )
            )
    go (isN0,s) (Graph.AcyclicSCC (id_, e)) = do
      (e',isN1,collected) <- collectGlobals isN0 substitution s e
      return ((isN1,map fst collected ++ s),(Graph.AcyclicSCC (id_,e'),collected))
    -- TODO: This completely skips recursive let-bindings in the collection of
    -- potentially disjoint applications of globals; and skips substituting truly
    -- disjoint applications of globals by a reference to a lifted out application.
    --
    -- This is to prevent the creation of combinational loops that have occurred
    -- "in the wild", but for which we have not been able to a create small
    -- unit test that triggers this creation-of-combinational-loops bug.
    -- Completely skipping recursive let-bindings is taking the hammer to
    -- solving this bug, without knowing whether a scalpel even existed and what
    -- it might look like. We should at some point think hard how traversing
    -- recursive let-bindings can introduce combinational loops, and whether
    -- there exists a solution that can traverse recursive let-bindings,
    -- finding more opportunities for DEC, while not introducing combinational
    -- loops.
    go acc scc@(Graph.CyclicSCC {}) = return (acc,(scc,[]))

-- | Given a case-tree corresponding to a disjoint interesting \"term-in-a-
-- function-position\", return a let-expression: where the let-binding holds
-- a case-expression selecting between the distinct arguments of the case-tree,
-- and the body is an application of the term applied to the shared arguments of
-- the case tree, and projections of let-binding corresponding to the distinct
-- argument positions.
mkDisjointGroup
  :: InScopeSet
  -- ^ Variables in scope at the very top of the case-tree, i.e., the original
  -- expression
  -> (Term,([Term],CaseTree [(Either Term Type)]))
  -- ^ Case-tree of arguments belonging to the applied term.
  -> NormalizeSession (Term,[Term])
mkDisjointGroup inScope (fun,(seen,cs)) = do
    tcm <- Lens.view tcCache
    let argss    = Foldable.toList cs
        argssT   = zip [0..] (List.transpose argss)
        (sharedT,distinctT) = List.partition (areShared tcm inScope . fmap (first stripTicks) . snd) argssT
        -- TODO: find a better solution than "maybe undefined fst . uncons"
        shared   = map (second (maybe (error "impossible") fst . List.uncons)) sharedT
        distinct = map (Either.lefts) (List.transpose (map snd distinctT))
        cs'      = fmap (zip [0..]) cs
        cs''     = removeEmpty
                 $ fmap (Either.lefts . map snd)
                        (if null shared
                           then cs'
                           else fmap (filter (`notElem` shared)) cs')
    (distinctCaseM,distinctProjections) <- case distinct of
      -- only shared arguments: do nothing.
      [] -> return (Nothing,[])
      -- Create selectors and projections
      (uc:_) -> do
        let argTys = map (inferCoreTypeOf tcm) uc
        disJointSelProj inScope argTys cs''
    let newArgs = mkDJArgs 0 shared distinctProjections
    case distinctCaseM of
      Just lb -> return (Letrec [lb] (mkApps fun newArgs), seen)
      Nothing -> return (mkApps fun newArgs, seen)

-- | Create a single selector for all the representable distinct arguments by
-- selecting between tuples. This selector is only ('Just') created when the
-- number of representable uncommmon arguments is larger than one, otherwise it
-- is not ('Nothing').
--
-- It also returns:
--
-- * For all the non-representable distinct arguments: a selector
-- * For all the representable distinct arguments: a projection out of the tuple
--   created by the larger selector. If this larger selector does not exist, a
--   single selector is created for the single representable distinct argument.
disJointSelProj
  :: InScopeSet
  -> [Type]
  -- ^ Types of the arguments
  -> CaseTree [Term]
  -- The case-tree of arguments
  -> NormalizeSession (Maybe LetBinding,[Term])
disJointSelProj _ _ (Leaf []) = return (Nothing,[])
disJointSelProj inScope argTys cs = do
    tcm    <- Lens.view tcCache
    tupTcm <- Lens.view tupleTcCache
    let maxIndex = length argTys - 1
        css = map (\i -> fmap ((:[]) . (!!i)) cs) [0..maxIndex]
    (untran,tran) <- List.partitionM (isUntranslatableType False . snd) (zip [0..] argTys)
    let untranCs   = map (css!!) (map fst untran)
        untranSels = zipWith (\(_,ty) cs' -> genCase tcm tupTcm ty [ty]  cs')
                             untran untranCs
    (lbM,projs) <- case tran of
      []       -> return (Nothing,[])
      [(i,ty)] -> return (Nothing,[genCase tcm tupTcm ty [ty] (css!!i)])
      tys      -> do
        let m            = length tys
            (tyIxs,tys') = unzip tys
            tupTy        = mkBigTupTy tcm tupTcm tys'
            cs'          = fmap (\es -> map (es !!) tyIxs) cs
            djCase       = genCase tcm tupTcm tupTy tys' cs'
        scrutId <- mkInternalVar inScope "tupIn" tupTy
        projections <- mapM (mkBigTupSelector inScope tcm tupTcm (Var scrutId) tys') [0..m-1]
        return (Just (scrutId,djCase),projections)
    let selProjs = tranOrUnTran 0 (zip (map fst untran) untranSels) projs

    return (lbM,selProjs)
  where
    tranOrUnTran _ []       projs     = projs
    tranOrUnTran _ sels     []        = map snd sels
    tranOrUnTran n ((ut,s):uts) (p:projs)
      | n == ut   = s : tranOrUnTran (n+1) uts          (p:projs)
      | otherwise = p : tranOrUnTran (n+1) ((ut,s):uts) projs

-- | Arguments are shared between invocations if:
--
-- * They contain _no_ references to locally-bound variables
-- * Are either:
--     1. All equal
--     2. A proof of an equality: we don't care about the shape of a proof.
--
--        Whether we have `Refl : True ~ True` or `SomeAxiom : (1 <=? 2) ~ True`
--        it doesn't matter, since when we normalize both sides we always end
--        up with a proof of `True ~ True`.
--        Since DEC only fires for applications where all the type arguments
--        are equal, we can deduce that all equality arguments witness the same
--        equality, hence we don't have to care about the shape of the proof.
areShared :: TyConMap -> InScopeSet -> [Either Term Type] -> Bool
areShared _   _       []       = True
areShared tcm inScope xs@(x:_) = noFV1 && (isProof x || allEqual xs)
 where
  noFV1 = case x of
    Right ty -> getAll (Lens.foldMapOf (typeFreeVars' isLocallyBound IntSet.empty)
                                       (const (All False)) ty)
    Left tm  -> getAll (Lens.foldMapOf (termFreeVars' isLocallyBound)
                                       (const (All False)) tm)

  isLocallyBound v = isLocalId v && v `notElemInScopeSet` inScope

  isProof (Left co) = case tyView (inferCoreTypeOf tcm co) of
    TyConApp (nameOcc -> "GHC.Types.~") _ -> True
    _ -> False
  isProof _ = False

-- | Create a list of arguments given a map of positions to common arguments,
-- and a list of arguments
mkDJArgs :: Int -- ^ Current position
         -> [(Int,Either Term Type)] -- ^ map from position to common argument
         -> [Term] -- ^ (projections for) distinct arguments
         -> [Either Term Type]
mkDJArgs _ cms []   = map snd cms
mkDJArgs _ [] uncms = map Left uncms
mkDJArgs n ((m,x):cms) (y:uncms)
  | n == m    = x       : mkDJArgs (n+1) cms (y:uncms)
  | otherwise = Left y  : mkDJArgs (n+1) ((m,x):cms) uncms

-- | Create a case-expression that selects between the distinct arguments given
-- a case-tree
genCase :: TyConMap
        -> IntMap TyConName
        -> Type -- ^ Type of the alternatives
        -> [Type] -- ^ Types of the arguments
        -> CaseTree [Term] -- ^ CaseTree of arguments
        -> Term
genCase tcm tupTcm ty argTys = go
  where
    go (Leaf tms) =
      mkBigTupTm tcm tupTcm (List.zipEqual argTys tms)

    go (LB lb ct) =
      Letrec lb (go ct)

    go (Branch scrut [(p,ct)]) =
      let ct' = go ct
          (ptvs,pids) = patIds p
      in  if (coerce ptvs ++ coerce pids) `localVarsDoNotOccurIn` ct'
             then ct'
             else Case scrut ty [(p,ct')]

    go (Branch scrut pats) =
      Case scrut ty (map (second go) pats)

-- | Lookup the TyConName and DataCon for a tuple of size n
findTup :: TyConMap -> IntMap TyConName -> Int -> (TyConName,DataCon)
findTup tcm tupTcm n =
  Maybe.fromMaybe (error ("Cannot build " <> show n <> "-tuble")) $ do
    tupTcNm <- IntMap.lookup n tupTcm
    tupTc <- UniqMap.lookup tupTcNm tcm
    tupDc <- Maybe.listToMaybe (tyConDataCons tupTc)
    return (tupTcNm,tupDc)

mkBigTupTm :: TyConMap -> IntMap TyConName -> [(Type,Term)] -> Term
mkBigTupTm tcm tupTcm args = snd $ mkBigTup tcm tupTcm args

mkSmallTup,mkBigTup :: TyConMap -> IntMap TyConName -> [(Type,Term)] -> (Type,Term)
mkSmallTup _ _ [] = error $ $curLoc ++ "mkSmallTup: Can't create 0-tuple"
mkSmallTup _ _ [(ty,tm)] = (ty,tm)
mkSmallTup tcm tupTcm args = (ty,tm)
  where
    (argTys,tms) = unzip args
    (tupTcNm,tupDc) = findTup tcm tupTcm (length args)
    tm = mkApps (Data tupDc) (map Right argTys ++ map Left tms)
    ty = mkTyConApp tupTcNm argTys

mkBigTup tcm tupTcm = mkChunkified (mkSmallTup tcm tupTcm)

mkSmallTupTy,mkBigTupTy
  :: TyConMap
  -> IntMap TyConName
  -> [Type]
  -> Type
mkSmallTupTy _ _ [] = error $ $curLoc ++ "mkSmallTupTy: Can't create 0-tuple"
mkSmallTupTy _ _ [ty] = ty
mkSmallTupTy tcm tupTcm tys = mkTyConApp tupTcNm tys
  where
    m = length tys
    (tupTcNm,_) = findTup tcm tupTcm m

mkBigTupTy tcm tupTcm = mkChunkified (mkSmallTupTy tcm tupTcm)

mkSmallTupSelector,mkBigTupSelector
  :: MonadUnique m
  => InScopeSet
  -> TyConMap
  -> IntMap TyConName
  -> Term
  -> [Type]
  -> Int
  -> m Term
mkSmallTupSelector _ _ _ scrut [_] 0 = return scrut
mkSmallTupSelector _ _ _ _     [_] n = error $ $curLoc ++ "mkSmallTupSelector called with one type, but to select " ++ show n
mkSmallTupSelector inScope tcm _ scrut _ n = mkSelectorCase ($curLoc ++ "mkSmallTupSelector") inScope tcm scrut 1 n

mkBigTupSelector inScope tcm tupTcm scrut tys n = go (chunkify tys)
  where
    go [_] = mkSmallTupSelector inScope tcm tupTcm scrut tys n
    go tyss = do
      let (nOuter,nInner) = divMod n mAX_TUPLE_SIZE
          tyss' = map (mkSmallTupTy tcm tupTcm) tyss
      outer <- mkSmallTupSelector inScope tcm tupTcm scrut tyss' nOuter
      inner <- mkSmallTupSelector inScope tcm tupTcm outer (tyss List.!! nOuter) nInner
      return inner


-- | Determine if a term in a function position is interesting to lift out of
-- of a case-expression.
--
-- This holds for all global functions, and certain primitives. Currently those
-- primitives are:
--
-- * All non-cheap multiplications
-- * All division-like operations with a non-cheap divisor
--
-- Multiplying/dividing by zero or powers of two are considered cheap and
-- isn't lifted out.
interestingToLift
  :: InScopeSet
  -- ^ in scope
  -> (Term -> Term)
  -- ^ Evaluator
  -> Term
  -- ^ Term in function position
  -> [Either Term Type]
  -- ^ Arguments
  -> [TickInfo]
  -- ^ Tick annoations
  -> Maybe Term
interestingToLift inScope _ e@(Var v) _ ticks =
  if NoDeDup `notElem` ticks && (isGlobalId v ||  v `elemInScopeSet` inScope)
     then (Just e)
     else Nothing
interestingToLift inScope eval e@(Prim pInfo) args ticks
  | NoDeDup `notElem` ticks = do
  let anyArgNotConstant = any (not . isConstant) lArgs
  case List.lookup (primName pInfo) interestingPrims of
    Just t | t || anyArgNotConstant -> (Just e)
    _ | DeDup `elem` ticks -> (Just e)
    _ ->
      if isHOTy (coreTypeOf pInfo) then do
        let anyInteresting = List.any (Maybe.isJust . isInteresting) lArgs
        if anyInteresting then Just e else Nothing
      else
        Nothing

  where
    isInteresting = (\(x, y, z) -> interestingToLift inScope eval x y z) . collectArgsTicks
    interestingPrims =
      [("Clash.Sized.Internal.BitVector.*#",bothNotPow2)
      ,("Clash.Sized.Internal.BitVector.times#",bothNotPow2)
      ,("Clash.Sized.Internal.BitVector.quot#",lastNotPow2)
      ,("Clash.Sized.Internal.BitVector.rem#",lastNotPow2)
      ,("Clash.Sized.Internal.Index.*#",bothNotPow2)
      ,("Clash.Sized.Internal.Index.times#",bothNotPow2)
      ,("Clash.Sized.Internal.Index.quot#",lastNotPow2)
      ,("Clash.Sized.Internal.Index.rem#",lastNotPow2)
      ,("Clash.Sized.Internal.Signed.*#",bothNotPow2)
      ,("Clash.Sized.Internal.Signed.times#",bothNotPow2)
      ,("Clash.Sized.Internal.Signed.rem#",lastNotPow2)
      ,("Clash.Sized.Internal.Signed.quot#",lastNotPow2)
      ,("Clash.Sized.Internal.Signed.div#",lastNotPow2)
      ,("Clash.Sized.Internal.Signed.mod#",lastNotPow2)
      ,("Clash.Sized.Internal.Unsigned.*#",bothNotPow2)
      ,("Clash.Sized.Internal.Unsigned.times#",bothNotPow2)
      ,("Clash.Sized.Internal.Unsigned.quot#",lastNotPow2)
      ,("Clash.Sized.Internal.Unsigned.rem#",lastNotPow2)
      ,("GHC.Base.quotInt",lastNotPow2)
      ,("GHC.Base.remInt",lastNotPow2)
      ,("GHC.Base.divInt",lastNotPow2)
      ,("GHC.Base.modInt",lastNotPow2)
      ,("GHC.Classes.divInt#",lastNotPow2)
      ,("GHC.Classes.modInt#",lastNotPow2)
#if MIN_VERSION_base(4,15,0)
      ,("GHC.Num.Integer.integerMul",bothNotPow2)
      ,("GHC.Num.Integer.integerDiv",lastNotPow2)
      ,("GHC.Num.Integer.integerMod",lastNotPow2)
      ,("GHC.Num.Integer.integerQuot",lastNotPow2)
      ,("GHC.Num.Integer.integerRem",lastNotPow2)
#else
      ,("GHC.Integer.Type.timesInteger",bothNotPow2)
      ,("GHC.Integer.Type.divInteger",lastNotPow2)
      ,("GHC.Integer.Type.modInteger",lastNotPow2)
      ,("GHC.Integer.Type.quotInteger",lastNotPow2)
      ,("GHC.Integer.Type.remInteger",lastNotPow2)
#endif
      ,("GHC.Prim.*#",bothNotPow2)
      ,("GHC.Prim.quotInt#",lastNotPow2)
      ,("GHC.Prim.remInt#",lastNotPow2)
      ]

    lArgs       = Either.lefts args

    lastNotPow2 = case lArgs of
                    [] -> True
                    _  -> not (termIsPow2 (last lArgs))
    -- | This only looks at the last two arguments, skipping over any constraints if they exist
    bothNotPow2 = go lArgs
     where
      go xs = case xs of
                [] -> True
                [a] -> not (termIsPow2 a)
                [a,b] -> not (termIsPow2 a) && not (termIsPow2 b)
                (_:rest) -> go rest

    termIsPow2 e' = case eval e' of
      Literal (IntegerLiteral n) -> isPow2 n
      a -> case collectArgs a of
        (Prim p,[Right _,Left _,       Left (Literal (IntegerLiteral n))])
          | isFromInt (primName p) -> isPow2 n
        (Prim p,[Right _,Left _,Left _,Left (Literal (IntegerLiteral n))])
          | primName p == "Clash.Sized.Internal.BitVector.fromInteger#"  -> isPow2 n
        _ -> False

    -- This used to contain (x /= 0), but multiplying by zero is free
    isPow2 x = (x .&. (complement x + 1)) == x

    isHOTy t = case splitFunForallTy t of
      (args',_) -> any isPolyFunTy (Either.rights args')

interestingToLift _ _ _ _ _ = Nothing
