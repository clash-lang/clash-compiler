{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2017     , Google Inc.,
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Turn CoreHW terms into normalized CoreHW Terms
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Normalize where

import           Control.Exception                (throw)
import qualified Control.Lens                     as Lens
import           Control.Monad                    ((>=>), when)
import qualified Control.Monad.Writer             as Writer
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.State.Strict       (State)
import           Data.Default                     (def)
import           Data.Either                      (lefts,partitionEithers)
import qualified Data.IntMap                      as IntMap
import qualified Data.IORef                       as IORef
import           Clash.Data.UniqMap               (UniqMap)
import qualified Clash.Data.UniqMap               as UniqMap
import           Data.List
  (intercalate, intersect, mapAccumL)
import qualified Data.Map                         as Map
import qualified Data.Maybe                       as Maybe
import qualified Data.Monoid                      as Monoid
import qualified Data.Set                         as Set
import qualified Data.Set.Lens                    as Lens

#if MIN_VERSION_prettyprinter(1,7,0)
import           Prettyprinter                    (vcat)
#else
import           Data.Text.Prettyprint.Doc        (vcat)
#endif

import           GHC.BasicTypes.Extra             (isNoInline)

import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs)
import           Clash.Core.Evaluator.Types as WHNF (Evaluator)
import           Clash.Core.FreeVars
  (freeLocalIds, globalIdOccursIn, globalIds)
import           Clash.Core.HasFreeVars           (notElemFreeVars)
import           Clash.Core.HasType
import           Clash.Core.PartialEval as PE     (Evaluator)
import           Clash.Core.Pretty                (PrettyOptions(..), showPpr, showPpr', ppr)
import           Clash.Core.Subst
  (eqTerm, extendGblSubstList, mkSubst, substTm)
import           Clash.Core.Term
  (CoreContext (..), Term (..), collectArgsTicks, mkApps, mkTicks)
import           Clash.Core.Type                  (Type, splitCoreFunForallTy)
import           Clash.Core.TyCon (TyConMap)
import           Clash.Core.Type                  (isPolyTy)
import           Clash.Core.Var                   (Id, varName, varType)
import           Clash.Core.VarEnv
  (VarEnv, elemVarSet, eltsVarEnv, emptyInScopeSet, emptyVarEnv,
   delVarEnv, extendInScopeSetList, extendVarEnv, lookupVarEnv, mapVarEnv,
   mapMaybeVarEnv, mkVarEnv, mkVarSet, notElemVarEnv, notElemVarSet,
   nullVarEnv, unionVarEnv)
import           Clash.Debug                      (traceIf)
import           Clash.Driver.Types
  (BindingMap, Binding(..), DebugOpts(..), ClashEnv(..))
import           Clash.Netlist.Types
  (HWMap, FilteredHWType(..))
import           Clash.Netlist.Util
  (splitNormalized)
import           Clash.Normalize.Strategy
import           Clash.Normalize.Transformations
import           Clash.Normalize.Types
import           Clash.Normalize.Util
import           Clash.Rewrite.Combinators
  ((>->), (>-!), (!->), allR, bottomupWithR, repeatR, topdownFixWithR)
import           Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), TransformContext (..), bindings,
   curFun, debugOpts, extra, tcCache, topEntities, newInlineStrategy)
import           Clash.Rewrite.Util
  (apply, isUntranslatableType, runRewriteSession)
import           Clash.Util
import           Clash.Util.Eq                    (fastEqBy)
import           Clash.Util.Interpolate           (i)
import           Clash.Util.Supply                (Supply)

import           Data.Binary                      (encode)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL

import           Clash.Rewrite.Types (RewriteStep(..))


-- | Run a NormalizeSession in a given environment
runNormalization
  :: ClashEnv
  -> Supply
  -- ^ UniqueSupply
  -> BindingMap
  -- ^ Global Binders
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcoded Type -> HWType translator
  -> PE.Evaluator
  -- ^ Hardcoded evaluator for partial evaluation
  -> WHNF.Evaluator
  -- ^ Hardcoded evaluator for WHNF (old evaluator)
  -> VarEnv Bool
  -- ^ Map telling whether a components is part of a recursive group
  -> [Id]
  -- ^ topEntities
  -> NormalizeSession a
  -- ^ NormalizeSession to run
  -> IO a
runNormalization env supply globals typeTrans peEval eval rcsMap topEnts =
  runRewriteSession rwEnv rwState
  where
    -- TODO The RewriteEnv should just take ClashOpts.
    rwEnv     = RewriteEnv
                  env
                  typeTrans
                  peEval
                  eval
                  (mkVarSet topEnts)

    rwState   = RewriteState
                  0
                  mempty       -- transformAppliedCounters Map
                  mempty       -- transformTriedCounters Map
                  globals
                  supply
                  (error $ $(curLoc) ++ "Report as bug: no curFun",noSrcSpan)
                  0
                  (IntMap.empty, 0)
                  emptyVarEnv
                  Map.empty    -- hwTypeCache
                  normState

    normState = NormalizeState
                  emptyVarEnv
                  Map.empty
                  emptyVarEnv
                  emptyVarEnv
                  Map.empty
                  rcsMap

normalize
  :: [Id]
  -> NormalizeSession BindingMap
normalize = go >=> unionWithCache
 where
  go []  = return emptyVarEnv
  go top = do
    (new,topNormalized) <- unzip <$> mapM normalize' top
    newNormalized <- normalize (concat new)
    return (unionVarEnv (mkVarEnv topNormalized) newNormalized)

  unionWithCache :: BindingMap -> NormalizeSession BindingMap
  unionWithCache env = do
    cache <- Lens.use (extra.normalized)
    -- We need to include the cache in our final result, forgetting to do so
    -- leads to https://github.com/clash-lang/clash-compiler/issues/3109
    --
    -- On the other hand, just returning the cache as our final result could
    -- not be enough, because normalize' might return a non-normalized binder
    -- that is later picked up and cleaned up by flattenCallTree.
    return (unionVarEnv cache env)

normalize' :: Id -> NormalizeSession ([Id], (Id, Binding Term))
normalize' nm = do
  exprM <- lookupVarEnv nm <$> Lens.use bindings
  let nmS = showPpr (varName nm)
  case exprM of
    Just (Binding nm' sp inl pr tm r) -> do
      tcm <- Lens.view tcCache
      topEnts <- Lens.view topEntities
      let isTop = nm `elemVarSet` topEnts
          ty0 = coreTypeOf nm'
          ty1 = if isTop then tvSubstWithTyEq ty0 else ty0

      -- check for polymorphic types
      when (isPolyTy ty1) $
        let msg = $curLoc ++ [i|
              Clash can only normalize monomorphic functions, but this is polymorphic:
              #{showPpr' def{displayUniques=False\} nm'}
              |]
            msgExtra | ty0 == ty1 = Nothing
                     | otherwise = Just $ [i|
              Even after applying type equality constraints it remained polymorphic:
              #{showPpr' def{displayUniques=False\} nm'{varType=ty1\}}
                         |]
        in throw (ClashException sp msg msgExtra)

      -- check for unrepresentable result type
      let (args,resTy) = splitCoreFunForallTy tcm ty1
          isTopEnt = nm `elemVarSet` topEnts
          isFunction = not $ null $ lefts args
      resTyRep <- not <$> isUntranslatableType False resTy
      if resTyRep
         then do
            tmNorm <- normalizeTopLvlBndr isTopEnt nm (Binding nm' sp inl pr tm r)
            let usedBndrs = Lens.toListOf globalIds (bindingTerm tmNorm)
            traceIf (bindingRecursive tmNorm)
                    (concat [ $(curLoc),"Expr belonging to bndr: ",nmS ," (:: "
                            , showPpr (coreTypeOf (bindingId tmNorm))
                            , ") remains recursive after normalization:\n"
                            , showPpr (bindingTerm tmNorm) ])
                    (return ())
            prevNorm <- mapVarEnv bindingId <$> Lens.use (extra.normalized)
            let toNormalize = filter (`notElemVarSet` topEnts)
                            $ filter (`notElemVarEnv` (extendVarEnv nm nm prevNorm)) usedBndrs
            return (toNormalize,(nm,tmNorm))
         else
           do
            -- Throw an error for unrepresentable topEntities and functions
            when (isTopEnt || isFunction) $
              let msg = $(curLoc) ++ [i|
                    This bndr has a non-representable return type and can't be normalized:
                    #{showPpr' def{displayUniques=False\} nm'}
                    |]
              in throw (ClashException sp msg Nothing)

            -- But allow the compilation to proceed for nonrepresentable values.
            -- This can happen for example when GHC decides to create a toplevel binder
            -- for the ByteArray# inside of a Natural constant.
            -- (GHC-8.4 does this with tests/shouldwork/Numbers/Exp.hs)
            -- It will later be inlined by flattenCallTree.
            opts <- Lens.view debugOpts
            -- Which already-normalized binders reference this one? Without that,
            -- the trace below says nothing about where the offending binder came
            -- from.
            referers <-
              if dbg_invariants opts
                 then do
                   prevNorm <- Lens.use (extra.normalized)
                   pure [ showPpr (varName (bindingId b))
                        | b <- eltsVarEnv prevNorm
                        , nm `elem` Lens.toListOf globalIds (bindingTerm b)
                        ]
                 else pure []
            traceIf (dbg_invariants opts)
                    (concat [$(curLoc), "Expr belonging to bndr: ", nmS, " (:: "
                            , showPpr (coreTypeOf nm')
                            , ") has a non-representable return type."
                            , " Referenced by: "
                            , if null referers
                                 then "nothing normalized so far"
                                 else intercalate ", " referers
                            , ". Not normalizing:\n", showPpr tm] )
                    (return ([],(nm,(Binding nm' sp inl pr tm r))))


    Nothing -> error $ $(curLoc) ++ "Expr belonging to bndr: " ++ nmS ++ " not found"

-- | Check whether the normalized bindings are non-recursive. Errors when one
-- of the components is recursive.
checkNonRecursive
  :: BindingMap
  -- ^ List of normalized binders
  -> BindingMap
checkNonRecursive norm = case mapMaybeVarEnv go norm of
  rcs | nullVarEnv rcs  -> norm
  rcs -> error $ $(curLoc) ++ "Callgraph after normalization contains following recursive components: "
                   ++ show (vcat [ ppr a <> ppr b
                                 | (a,b) <- eltsVarEnv rcs
                                 ])
 where
  go (Binding nm _ _ _ tm r) =
    if r then Just (nm,tm) else Nothing

-- | Perform general \"clean up\" of the normalized (non-recursive) function
-- hierarchy. This includes:
--
--   * Inlining functions that simply \"wrap\" another function
cleanupGraph
  :: Id
  -> BindingMap
  -> NormalizeSession BindingMap
cleanupGraph topEntity norm
  | Just ct <- mkCallTree [] norm topEntity
  = do memo <- liftIO newFlattenMemo
       ctFlat <- flattenCallTree memo ct
       return (mkVarEnv $ snd $ callTreeToList [] ctFlat)
cleanupGraph _ norm = return norm

-- | A tree of identifiers and their bindings, with branches containing
-- additional bindings which are used. See "Clash.Driver.Types.Binding".
--
data CallTree
  = CLeaf   (Id, Binding Term)
  | CBranch (Id, Binding Term) [CallTree]

mkCallTree
  :: [Id]
  -- ^ Visited
  -> BindingMap
  -- ^ Global binders
  -> Id
  -- ^ Root of the call graph
  -> Maybe CallTree
mkCallTree visited bindingMap root
  | Just rootTm <- lookupVarEnv root bindingMap
  = let used   = Set.toList $ Lens.setOf globalIds $ (bindingTerm rootTm)
        other  = Maybe.mapMaybe (mkCallTree (root:visited) bindingMap) (filter (`notElem` visited) used)
    in  case used of
          [] -> Just (CLeaf   (root,rootTm))
          _  -> Just (CBranch (root,rootTm) other)
mkCallTree _ _ _ = Nothing

stripArgs
  :: [Id]
  -> [Id]
  -> [Either Term Type]
  -> Maybe [Either Term Type]
stripArgs _      (_:_) []   = Nothing
stripArgs allIds []    args = if any mentionsId args
                                then Nothing
                                else Just args
  where
    mentionsId t = not $ null (either (Lens.toListOf freeLocalIds) (const []) t
                              `intersect`
                              allIds)

stripArgs allIds (id_:ids) (Left (Var nm):args)
      | id_ == nm = stripArgs allIds ids args
      | otherwise = Nothing
stripArgs _ _ _ = Nothing

flattenNode
  :: CallTree
  -> NormalizeSession (Either CallTree ((Id,Term),[CallTree]))
flattenNode c@(CLeaf (_,(Binding _ _ spec _ _ _))) | isNoInline spec = return (Left c)
flattenNode c@(CLeaf (nm,(Binding _ _ _ _ e _))) = do
  isTopEntity <- elemVarSet nm <$> Lens.view topEntities
  if isTopEntity then return (Left c) else do
    tcm  <- Lens.view tcCache
    let norm = splitNormalized tcm e
    case norm of
      Right (ids,[(bId,bExpr)],_) -> do
        let (fun,args,ticks) = collectArgsTicks bExpr
        case stripArgs ids (reverse ids) (reverse args) of
          Just remainder | bId `notElemFreeVars` bExpr ->
               return (Right ((nm,mkApps (mkTicks fun ticks) (reverse remainder)),[]))
          _ -> return (Right ((nm,e),[]))
      _ -> return (Right ((nm,e),[]))
flattenNode b@(CBranch (_,(Binding _ _ spec _ _ _)) _) | isNoInline spec =
  return (Left b)
flattenNode b@(CBranch (nm,(Binding _ _ _ _ e _)) us) = do
  isTopEntity <- elemVarSet nm <$> Lens.view topEntities
  if isTopEntity then return (Left b) else do
    tcm  <- Lens.view tcCache
    let norm = splitNormalized tcm e
    case norm of
      Right (ids,[(bId,bExpr)],_) -> do
        let (fun,args,ticks) = collectArgsTicks bExpr
        case stripArgs ids (reverse ids) (reverse args) of
          Just remainder | bId `notElemFreeVars` bExpr ->
               return (Right ((nm,mkApps (mkTicks fun ticks) (reverse remainder)),us))
          _ -> return (Right ((nm,e),us))
      _ -> do
        newInlineStrat <- Lens.view newInlineStrategy
        if newInlineStrat || isCheapFunction e
           then return (Right ((nm,e),us))
           else return (Left b)

{-
Note [flatten pass structure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Through experimentation we've learned the following:

1. The evaluator-backed rewrites ('reduceConst', 'reducePrim') must not
   sit inside the top-down propagation bundle. 'topdownFixR' settles that bundle
   at each node with 'repeatR', so a bundled 'reduceConst' is re-attempted -
   evaluator call and all - for every 'appProp' or 'caseCon' that fires there.
   On large designs we've measured that dominates; hoisting them out was worth
   ~30%. See #3338.

2. There should be exactly two traversals per round, and the bottom-up one has
   to come first.

   'allR' rebuilds every node it walks, so each extra traversal costs a full
   term rebuild per round even when nothing fires. Running 'flattenLet' and the
   evaluator-backed rewrites as one fused bottom-up pass instead of two
   consecutive ones is therefore free of charge (it does not change which
   rewrites fire, or in which order they fire relative to each other).

   Running that bottom-up pass _before_ the top-down one lets the constants it
   folds be consumed by 'caseCon' in the same round. With the passes the other
   way round the folded constants sit unused until the next round, so
   constant-heavy designs pay for an extra iteration of the whole loop:
   @tests/shouldwork/Basic/AES.hs@ did ~36k extra node visits per transformation
   that way.

3. Traversals skip let-bindings that a pass has already seen without anything
   firing, see Note [flatten memo]. Passes that can only fire in a few places,
   such as 'topLet' and 'collapseRHSNoops', don't traverse the whole term.

If you touch code related to this, please make sure to run benchmarks.
-}

-- | Memo tables of one 'cleanupGraph' call
data FlattenMemo = FlattenMemo
  { fmCallTree :: IORef.IORef (UniqMap CallTree)
  -- ^ Flattened call trees, keyed by binder Id. See 'flattenCallTree'.
  , fmBottomUp :: CleanBinders
  -- ^ Let-bindings the bottom-up pass of the 'flatten' loop does not change
  , fmTopDown :: CleanBinders
  -- ^ Let-bindings the top-down pass of the 'flatten' loop does not change
  , fmDeadCode :: CleanBinders
  -- ^ Let-bindings 'deadCode' does not change
  }

newFlattenMemo :: IO FlattenMemo
newFlattenMemo =
  FlattenMemo
    <$> IORef.newIORef UniqMap.empty
    <*> IORef.newIORef emptyVarEnv
    <*> IORef.newIORef emptyVarEnv
    <*> IORef.newIORef emptyVarEnv

{-
Note [flatten memo]
~~~~~~~~~~~~~~~~~~~
'flattenCallTree' flattens a function after inlining the flattened bodies of
its callees, so the passes of 'flatten' mostly visit let-bindings they have
visited before: in an earlier round of the fixpoint loop, or while flattening
the callee. Thin wrappers make this very visible: each level of a chain like
@top -> wrapper -> body@ used to traverse all of @body@ more than ten times,
while rewrites only fired where @body@ got inlined.

The passes of 'flatten' therefore remember the let-bindings they visited
without anything firing, by mapping the binder to the right-hand side it had
('CleanBinders'). They skip a right-hand side that is (structurally) equal to
the remembered one. The tables live for one 'cleanupGraph' call, so they carry
over from callees to their callers.

This only skips work that would not have changed anything. Whether a rewrite
fires at a node depends on the subterm at that node, its context, and global
state that doesn't change during 'cleanupGraph' (global binders are only
added). The rewrites in 'flatten' only look at these parts of a context:

  * Its head, e.g. 'reduceConst' skips 'AppFun' positions. For the nodes in
    a right-hand side, that is either an entry within the right-hand side or
    the 'LetBinding' of the binding itself.

  * 'LetBody' entries, whose bindings 'whnfRW' hands to the evaluator, and
    'AppArg' entries of primitive arguments, see 'shouldReduce'. 'allCleanR'
    only skips bindings whose context has neither.

  * Whether it consists of lambda bodies and ticks only ('topLet'), which is
    never the case below a 'LetBinding'.

The one exception is 'bindConstantVar': it never inlines a binding whose
right-hand side is a reference to the function being rewritten ('curFun'). We
therefore don't remember right-hand sides that mention that function.

A table also tells where the individual rewrites of its pass won't fire: the
top-down pass applies 'caseCon' and 'bindConstantVar' at every node, and the
bottom-up pass applies 'flattenLet' at every node. The passes after the loop
make use of that.

Skipping work changes which uniques fresh binders get, but not which rewrites
fire.
-}

-- | Let-bindings in which a pass found nothing to rewrite: the binder and the
-- right-hand side it had then. See Note [flatten memo].
type CleanBinders = IORef.IORef (VarEnv Term)

-- | Like 'allR', but skips the right-hand sides of let-bindings that the given
-- table has as clean. See Note [flatten memo].
allCleanR
  :: CleanBinders
  -> Bool
  -- ^ Whether to add the right-hand sides in which nothing fired to the table
  -> NormRewrite
  -> NormRewrite
allCleanR cleanRef record trans (TransformContext is0 ctx) (Letrec xes e)
  | all plainCtx ctx = do
      clean <- liftIO (IORef.readIORef cleanRef)
      xes1 <- traverse (rewriteBind clean) xes
      e1 <- trans (TransformContext is1 (LetBody xes:ctx)) e
      return (Letrec xes1 e1)
 where
  bndrs = map fst xes
  is1 = extendInScopeSetList is0 bndrs

  rewriteBind clean (b,rhs0)
    | Just rhsClean <- lookupVarEnv b clean
    , fastEqBy eqTerm rhsClean rhs0
    = return (b,rhs0)
    | otherwise = do
      (rhs1, Monoid.getAny -> rhsChanged) <-
        Writer.listen (trans (TransformContext is1 (LetBinding b bndrs:ctx)) rhs0)
      when record $ do
        (fn,_) <- Lens.use curFun
        liftIO . IORef.modifyIORef' cleanRef $
          if rhsChanged || fn `globalIdOccursIn` rhs1
            then (`delVarEnv` b)
            else extendVarEnv b rhs1
      return (b,rhs1)

  plainCtx LetBody{} = False
  plainCtx (AppArg (Just _)) = False
  plainCtx _ = True

allCleanR _ _ trans ctx e = allR trans ctx e

-- | 'topdownSucR' for 'topLet': it only fires along the spine of lambdas (and
-- ticks) of a function, so there is no need to look further.
topLetR :: NormRewrite
topLetR = apply "topLet" topLet >-! spine
 where
  spine ctx e@Lam{} = allR topLetR ctx e
  spine ctx e@Tick{} = allR topLetR ctx e
  spine _ e = return e

-- | Flatten a 'CallTree', memoizing results by binder Id within one cleanup
-- pass. Without the cache, every binder reachable from the root is flattened
-- as many times as it appears in the (un-deduplicated) call tree.
flattenCallTree
  :: FlattenMemo
  -- ^ Memo tables. Local to one 'cleanupGraph' call.
  -> CallTree
  -> NormalizeSession CallTree
flattenCallTree _ c@(CLeaf _) = return c
flattenCallTree memo (CBranch (nm,(Binding nm' sp inl pr tm r)) used) = do
  -- XXX: Careful! If you ever add concurrency, this will have to be changed to
  --      account for multiple workers.
  let cache = fmCallTree memo
  cached <- liftIO (UniqMap.lookup nm <$> IORef.readIORef cache)
  case cached of
    Just ct -> pure ct
    Nothing -> do
      ct <- doFlatten
      liftIO (IORef.modifyIORef' cache (UniqMap.insert nm ct))
      pure ct
 where
  doFlatten = do
   flattenedUsed   <- mapM (flattenCallTree memo) used
   (newUsed,il_ct) <- partitionEithers <$> mapM flattenNode flattenedUsed
   let (toInline,il_used) = unzip il_ct
       subst = extendGblSubstList (mkSubst emptyInScopeSet) toInline
   newExpr <- case toInline of
     [] -> return tm
     _  -> do
       let tm1 = substTm "flattenCallTree.flattenExpr" subst tm

       -- NB: When -fclash-debug-history is on, emit binary data holding the recorded rewrite steps
       opts <- Lens.view debugOpts
       let rewriteHistFile = dbg_historyFile opts
       when (Maybe.isJust rewriteHistFile) $
         liftIO
           $ BS.appendFile (Maybe.fromJust rewriteHistFile)
           $ BL.toStrict
           $ encode RewriteStep
               { t_ctx    = []
               , t_name   = "INLINE"
               , t_bndrS  = showPpr (varName nm')
               , t_before = tm
               , t_after  = tm1
               }
       rewriteExpr ("flattenExpr",flatten) (showPpr nm, tm1) (nm', sp)
   let allUsed = newUsed ++ concat il_used
   -- inline all components when the resulting expression after flattening
   -- is still considered "cheap". This happens often at the topEntity which
   -- wraps another functions and has some selectors and data-constructors.
   if not (isNoInline inl) && isCheapFunction newExpr
      then do
         let (toInline',allUsed') = unzip (map goCheap allUsed)
             subst' = extendGblSubstList (mkSubst emptyInScopeSet)
                                         (Maybe.catMaybes toInline')
         let tm1 = substTm "flattenCallTree.flattenCheap" subst' newExpr
         newExpr' <- rewriteExpr ("flattenCheap",flatten) (showPpr nm, tm1) (nm', sp)
         return (CBranch (nm,(Binding nm' sp inl pr newExpr' r)) (concat allUsed'))
      else return (CBranch (nm,(Binding nm' sp inl pr newExpr r)) allUsed)

  flatten =
    -- See Note [flatten pass structure] and Note [flatten memo].
    repeatR (bottomupWithR (allCleanR (fmBottomUp memo) True)
                       (apply "flattenLet" flattenLet >->
                        (apply "reduceConst" reduceConst !->
                           apply "deadCode" deadCode) >->
                        apply "reducePrim" reducePrim >->
                        apply "removeUnusedExpr" removeUnusedExpr) >->
             topdownFixWithR (allCleanR (fmTopDown memo) True)
              (apply "appProp" appProp >->
               apply "bindConstantVar" bindConstantVar >->
               apply "caseCon" caseCon)) !->
    bottomupWithR (allCleanR (fmDeadCode memo) True)
      (apply "deadCode" deadCode) >-> -- See #3407
    topLetR >->
    -- See [Note] relation `collapseRHSNoops` and `inlineCleanup`
    -- Note that we do this as the very last step, after all constant propagation
    -- has been done to avoid #3036.
    onlyNoInline (topdownSucR (apply "collapseRHSNoops" collapseRHSNoops)) >->
    topdownSucR (apply "inlineCleanup" inlineCleanup) >->
    -- The next three passes only revisit let-bindings that changed since the
    -- loop above, see Note [flatten memo].
    bottomupWithR (allCleanR (fmTopDown memo) False)
      (apply "caseCon" caseCon) >-> -- https://github.com/clash-lang/clash-compiler/issues/3159 / #3204
    bottomupWithR (allCleanR (fmBottomUp memo) False)
      (apply "flattenLet" flattenLet) >-> -- https://github.com/clash-lang/clash-compiler/issues/3185
    bottomupWithR (allCleanR (fmTopDown memo) False)
      (apply "bindConstantVar" bindConstantVar) >-> -- https://github.com/clash-lang/clash-compiler/issues/3041
    topLetR

  -- 'collapseRHSNoops' only fires in synthesis boundaries, don't traverse the
  -- term for nothing.
  onlyNoInline rw ctx e = do
    noInline <- curFunIsNoInline
    if noInline then rw ctx e else return e

  goCheap c@(CLeaf   (nm2,(Binding _ _ inl2 _ e _)))
    | isNoInline inl2  = (Nothing     ,[c])
    | otherwise        = (Just (nm2,e),[])
  goCheap c@(CBranch (nm2,(Binding _ _ inl2 _ e _)) us)
    | isNoInline inl2  = (Nothing, [c])
    | otherwise        = (Just (nm2,e),us)

callTreeToList :: [Id] -> CallTree -> ([Id], [(Id, Binding Term)])
callTreeToList visited (CLeaf (nm,bndr))
  | nm `elem` visited = (visited,[])
  | otherwise         = (nm:visited,[(nm,bndr)])
callTreeToList visited (CBranch (nm,bndr) used)
  | nm `elem` visited = (visited,[])
  | otherwise         = (visited',(nm,bndr):(concat others))
  where
    (visited',others) = mapAccumL callTreeToList (nm:visited) used
