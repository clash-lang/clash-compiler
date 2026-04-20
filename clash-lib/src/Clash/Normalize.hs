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

module Clash.Normalize where

import           Control.Exception                (throw)
import qualified Control.Lens                     as Lens
import           Control.Monad                    ((>=>), foldM, when)
import           Control.Monad.State.Strict       (State)
import           Data.Default                     (def)
import           Data.Either                      (lefts,partitionEithers)
import qualified Data.IntMap                      as IntMap
import           Data.List
  (foldl', intersect)
import qualified Data.Map                         as Map
import qualified Data.Maybe                       as Maybe
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
  (freeLocalIds, globalIds)
import           Clash.Core.HasFreeVars           (notElemFreeVars)
import           Clash.Core.HasType
import           Clash.Core.PartialEval as PE     (Evaluator)
import           Clash.Core.Pretty                (PrettyOptions(..), showPpr, showPpr', ppr)
import           Clash.Core.Subst
  (extendGblSubstList, mkSubst, substTm)
import           Clash.Core.Term                  (Term (..), collectArgsTicks
                                                  ,mkApps, mkTicks, stripTicks)
import           Clash.Core.Type                  (Type, splitCoreFunForallTy)
import           Clash.Core.TyCon (TyConMap)
import           Clash.Core.Type                  (isPolyTy)
import           Clash.Core.Var                   (Id, varName, varType, varUniq)
import           Clash.Core.VarEnv
  (VarEnv, elemVarEnv, elemVarSet, eltsVarEnv, emptyInScopeSet, emptyVarEnv,
   extendVarEnv, lookupVarEnv, mapVarEnv, mapMaybeVarEnv,
   mkVarEnv, mkVarSet, notElemVarEnv, notElemVarSet, nullVarEnv, unionVarEnv)
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
  ((>->), (!->), bottomupR, repeatR, topdownFixR)
import           Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), bindings, debugOpts, extra,
   tcCache, topEntities, newInlineStrategy)
import           Clash.Rewrite.Util
  (apply, isUntranslatableType, runRewriteSession)
import           Clash.Util
import           Clash.Util.Graph                 (reverseTopSort)
import           Clash.Util.Interpolate           (i)
import           Clash.Util.Supply                (Supply)

import           Data.Binary                      (encode)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL

import           System.IO.Unsafe                 (unsafePerformIO)
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
                  mempty       -- transformCounters Map
                  mempty       -- transformStats Map
                  globals
                  supply
                  (error $ $(curLoc) ++ "Report as bug: no curFun",noSrcSpan)
                  0
                  (IntMap.empty, 0)
                  emptyVarEnv
                  normState

    normState = NormalizeState
                  emptyVarEnv
                  Map.empty
                  emptyVarEnv
                  emptyVarEnv
                  Map.empty
                  rcsMap
                  Map.empty
                  Map.empty
                  Map.empty

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
            traceIf (dbg_invariants opts)
                    (concat [$(curLoc), "Expr belonging to bndr: ", nmS, " (:: "
                            , showPpr (coreTypeOf nm')
                            , ") has a non-representable return type."
                            , " Not normalising:\n", showPpr tm] )
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
  | not (topEntity `elemVarEnv` norm) = pure norm
  | otherwise =
      case reverseTopSort nodes edges of
        Left msg -> error msg
        Right order -> do
          flatNodes <- foldM (flattenGraphNode norm cg) emptyVarEnv order
          pure (collectFlatBindings flatNodes topEntity)
 where
  cg = callGraph norm topEntity
  reachable =
    filter (\bndr -> bindingId bndr `elemVarEnv` cg) (eltsVarEnv norm)

  nodes =
    [ (varUniq (bindingId bndr), bindingId bndr)
    | bndr <- reachable
    ]

  edges =
    concatMap graphEdges reachable

  graphEdges bndr =
    let nm = bindingId bndr
    in [ (varUniq nm, varUniq dep)
       | dep <- directDeps bndr
       , dep `elemVarEnv` cg
       ]

data FlatNode =
  FlatNode
    { fnId :: !Id
    , fnBinding :: !(Binding Term)
    , fnUsed :: [Id]
    , fnHadUses :: !Bool
    }

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

directDeps :: Binding Term -> [Id]
directDeps = Set.toList . Lens.setOf globalIds . bindingTerm

lookupFlatNodes :: VarEnv FlatNode -> [Id] -> [FlatNode]
lookupFlatNodes flatNodes =
  Maybe.mapMaybe (`lookupVarEnv` flatNodes)

flattenInlineable
  :: FlatNode
  -> NormalizeSession (Either FlatNode ((Id,Term),[Id]))
flattenInlineable node@(FlatNode _ (Binding _ _ spec _ _ _) _ _)
  | isNoInline spec
  = pure (Left node)
flattenInlineable node@(FlatNode nm (Binding _ _ _ _ e _) us hadUses) = do
  isTopEntity <- elemVarSet nm <$> Lens.view topEntities
  if isTopEntity then pure (Left node) else do
    tcm  <- Lens.view tcCache
    case splitNormalized tcm e of
      Right (ids,[(bId,bExpr)],_) -> do
        let (fun,args,ticks) = collectArgsTicks bExpr
        case stripArgs ids (reverse ids) (reverse args) of
          Just remainder | bId `notElemFreeVars` bExpr ->
               pure (Right ((nm,mkApps (mkTicks fun ticks) (reverse remainder)),us))
          _ -> pure (Right ((nm,e),us))
      _ -> do
        if hadUses
           then do
             newInlineStrat <- Lens.view newInlineStrategy
             if newInlineStrat || isCheapFunction e
                then pure (Right ((nm,e),us))
                else pure (Left node)
           else pure (Right ((nm,e),[]))

flattenGraphNode
  :: BindingMap
  -> VarEnv (VarEnv Word)
  -> VarEnv FlatNode
  -> Id
  -> NormalizeSession (VarEnv FlatNode)
flattenGraphNode norm cg flatNodes nm =
  case lookupVarEnv nm norm of
    Nothing -> pure flatNodes
    Just bndr
      | null allDeps ->
          pure (extendVarEnv nm (FlatNode nm bndr [] False) flatNodes)
      | otherwise -> do
          node <- flattenBranch flatNodes nm bndr reachableDeps
          pure (extendVarEnv nm node flatNodes)
     where
      allDeps = directDeps bndr
      reachableDeps = filter (`elemVarEnv` cg) allDeps

flattenBranch
  :: VarEnv FlatNode
  -> Id
  -> Binding Term
  -> [Id]
  -> NormalizeSession FlatNode
flattenBranch flatNodes nm (Binding nm' sp inl pr tm r) usedIds = do
  let used = lookupFlatNodes flatNodes usedIds

  (newUsed,il_ct) <- partitionEithers <$> mapM flattenInlineable used
  let (toInline,il_used) = unzip il_ct
      subst = extendGblSubstList (mkSubst emptyInScopeSet) toInline
  newExpr <- case toInline of
    [] -> pure tm
    _  -> do
      let tm1 = substTm "flattenCallTree.flattenExpr" subst tm

      -- NB: When -fclash-debug-history is on, emit binary data holding the recorded rewrite steps
      opts <- Lens.view debugOpts
      let rewriteHistFile = dbg_historyFile opts
      when (Maybe.isJust rewriteHistFile) $
        let !_ = unsafePerformIO
             $ BS.appendFile (Maybe.fromJust rewriteHistFile)
             $ BL.toStrict
             $ encode RewriteStep
                 { t_ctx    = []
                 , t_name   = "INLINE"
                 , t_bndrS  = showPpr (varName nm')
                 , t_before = tm
                 , t_after  = tm1
                 }
        in pure ()
      rewriteExpr ("flattenExpr",flatten) (showPpr nm, tm1) (nm', sp)
  let allUsed = newUsed ++ lookupFlatNodes flatNodes (concat il_used)
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
        pure (FlatNode nm (Binding nm' sp inl pr newExpr' r) (concat allUsed') True)
     else pure (FlatNode nm (Binding nm' sp inl pr newExpr r) (map fnId allUsed) True)
  where
    flattenPropagate :: NormRewrite
    flattenPropagate ctx term =
      case term of
        App {}   -> appLike ctx term
        TyApp {} -> appLike ctx term
        Case {}  -> caseLike ctx term
        Letrec {} -> letLike ctx term
        Tick _ _ -> case stripTicks term of
          App {}   -> appLike ctx term
          TyApp {} -> appLike ctx term
          _        -> pure term
        _ -> pure term

    appLike :: NormRewrite
    appLike =
      apply "appProp" appProp >->
      (apply "reduceConst" reduceConst !-> apply "deadcode" deadCode) >->
      apply "reduceNonRepPrim" reduceNonRepPrim >->
      apply "removeUnusedExpr" removeUnusedExpr

    caseLike :: NormRewrite
    caseLike =
      apply "caseCon" caseCon >->
      apply "removeUnusedExpr" removeUnusedExpr

    letLike :: NormRewrite
    letLike =
      apply "bindConstantVar" bindConstantVar

    flatten =
      -- topdownFixR integrates the local fixpoint: when a child change
      -- (e.g. caseCon on a scrutinee) exposes a new redex at the parent
      -- (e.g. caseLet), only the path to that parent is re-examined rather
      -- than restarting the entire traversal from the root. The outer
      -- repeatR is retained because the bottomupR flattenLet pass can
      -- expose new top-down opportunities.
      repeatR (topdownFixR flattenPropagate >->
               bottomupR (apply "flattenLet" flattenLet)) !->
      topdownSucR (apply "topLet" topLet) >->
      -- See [Note] relation `collapseRHSNoops` and `inlineCleanup`
      -- Note that we do this as the very last step, after all constant propagation
      -- has been done to avoid #3036.
      topdownSucR (apply "collapseRHSNoops" collapseRHSNoops) >->
      topdownSucR (apply "inlineCleanup" inlineCleanup) >->
      topdownSucR (apply "caseCon" caseCon) >-> -- https://github.com/clash-lang/clash-compiler/issues/3159
      bottomupR (apply "flattenLet" flattenLet) >-> -- https://github.com/clash-lang/clash-compiler/issues/3185
      topdownSucR (apply "topLet" topLet)

    goCheap (FlatNode nm2 (Binding _ _ inl2 _ e _) us _)
      | isNoInline inl2 = (Nothing, [nm2])
      | otherwise       = (Just (nm2,e), us)

collectFlatBindings :: VarEnv FlatNode -> Id -> BindingMap
collectFlatBindings flatNodes =
  go emptyVarEnv
 where
  go acc nm
    | nm `elemVarEnv` acc
    = acc
    | otherwise
    = case lookupVarEnv nm flatNodes of
        Nothing -> acc
        Just (FlatNode _ bndr used _) ->
          foldl' go (extendVarEnv nm bndr acc) used
