{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2017     , Google Inc.,
                     2021-2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Turn CoreHW terms into normalized CoreHW Terms
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize where

import           Control.Concurrent.Lifted        (getNumCapabilities, myThreadId)
import qualified Control.Concurrent.Async.Lifted as Async
import           Control.Concurrent.MVar.Lifted (MVar)
import qualified Control.Concurrent.MVar.Lifted as MVar
import           Control.Exception                (throw)
import qualified Control.Lens                     as Lens
import           Control.Monad                    (when)
import qualified Control.Monad.IO.Class as Monad  (liftIO)
import           Control.Monad.State.Strict       (State)
import           Data.Bifunctor                   (second)
import           Data.Default                     (def)
import           Data.Either                      (lefts,partitionEithers)
import           Data.Foldable                    (traverse_)
import qualified Data.HashMap.Strict              as HashMap
import           Data.List
  (intersect, mapAccumL)
import qualified Data.Map                         as Map
import qualified Data.Maybe                       as Maybe
import qualified Data.Set                         as Set
import qualified Data.Set.Lens                    as Lens
import qualified Data.Concurrent.Queue.MichaelScott as MS

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
                                                  ,mkApps, mkTicks)
import           Clash.Core.Type                  (Type, splitCoreFunForallTy)
import           Clash.Core.TyCon (TyConMap)
import           Clash.Core.Type                  (isPolyTy)
import           Clash.Core.Var                   (Id, varName, varType)
import           Clash.Core.VarEnv
  (VarEnv, VarSet, elemVarSet, eltsVarEnv, emptyInScopeSet, emptyVarEnv, emptyVarSet,
   extendVarEnv, extendVarSet, lookupVarEnv, mapMaybeVarEnv,
   mkVarEnv, mkVarSet, notElemVarEnv, notElemVarSet, nullVarEnv,
   listToVarEnv, toListVarEnv)
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
  ((>->), (!->), bottomupR, repeatR, topdownR)
import           Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), bindings, debugOpts, extra, uniqSupply,
   tcCache, topEntities, newInlineStrategy, ioLock)
import           Clash.Rewrite.Util
  (apply, isUntranslatableType, runRewriteSession)
import           Clash.Util
import           Clash.Util.Interpolate           (i)
import           Clash.Util.Supply                (Supply, splitSupply)

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
  -> MVar ()
  -- ^ Synchronization on stdout
  -> [Id]
  -- ^ topEntities
  -> NormalizeSession a
  -- ^ NormalizeSession to run
  -> IO a
runNormalization env supply globals typeTrans peEval eval rcsMap lock entities session = do
  normState <- NormalizeState
    <$> MVar.newMVar emptyVarEnv
    <*> MVar.newMVar Map.empty
    <*> MVar.newMVar emptyVarEnv
    <*> MVar.newMVar emptyVarEnv
    <*> MVar.newMVar Map.empty
    <*> MVar.newMVar rcsMap

  rwState <- RewriteState
    <$> MVar.newMVar mempty
    <*> MVar.newMVar globals
    <*> pure supply
    <*> MVar.newMVar HashMap.empty
    <*> MVar.newMVar 0
    <*> MVar.newMVar (mempty, 0)
    <*> MVar.newMVar emptyVarEnv
    <*> pure lock
    <*> pure normState

  runRewriteSession rwEnv rwState session
 where
  rwEnv = RewriteEnv
    { _clashEnv = env
    , _typeTranslator = typeTrans
    , _peEvaluator = peEval
    , _evaluator = eval
    , _topEntities = mkVarSet entities
    }

supplies :: Int -> Supply -> [Supply]
supplies 0 _ = []
supplies n s = let (s0', s1') = splitSupply s in s0' : supplies (n-1) s1'

normalize :: [Id] -> NormalizeSession BindingMap
normalize tops = do
  q <- Monad.liftIO MS.newQ
  traverse_ (Monad.liftIO . MS.pushL q) tops
  binds <- MVar.newMVar (emptyVarSet, [])
  uniq0 <- Lens.use uniqSupply
  nWorkers <- Monad.liftIO getNumCapabilities
  Monad.liftIO $ print $ ("nWorkers" :: String, nWorkers)
  let ss = supplies nWorkers uniq0
  Async.mapConcurrently_ (normalizeStep q binds) ss
  mkVarEnv . snd <$> MVar.readMVar binds

normalizeStep
    :: MS.LinkedQueue Id
    -> MVar (VarSet, [(Id, Binding Term)])
    -> Supply
    -> NormalizeSession ()
normalizeStep q binds s = do
  uniqSupply Lens..= s
  res <- Monad.liftIO $ MS.tryPopR q
  threadId <- myThreadId
  Monad.liftIO $ print$ ("Pop!" :: String, threadId)
  case res of
      Just id' -> do
        (bound, pairs) <- MVar.takeMVar binds
        if not (id' `elemVarSet` bound)
          then do
            -- mark that we are attempting to normalize id'
            MVar.putMVar binds (bound `extendVarSet` id', pairs)
            Monad.liftIO $ print  $ ("normalize'" :: String, threadId, id')
            pair <- normalize' id' q
            MVar.modifyMVar_ binds (pure . second (pair:))
          else
            MVar.putMVar binds (bound, pairs)
        nextS <- Lens.use uniqSupply
        normalizeStep q binds nextS
      Nothing  -> pure ()

normalize' :: Id -> MS.LinkedQueue Id -> NormalizeSession (Id, Binding Term)
normalize' nm q = do
  bndrsV <- Lens.use bindings
  exprM <- MVar.withMVar bndrsV (pure . lookupVarEnv nm)
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

            normV <- Lens.use (extra.normalized)

            toNormalize <-
              MVar.withMVar normV $ \norm -> do
                prevNorm <- listToVarEnv <$> traverse (\(k, v) -> (k,) . bindingId <$> MVar.readMVar v) (toListVarEnv norm)
                let toNormalize = filter (`notElemVarSet` topEnts)
                                $ filter (`notElemVarEnv` extendVarEnv nm nm prevNorm) usedBndrs
                  in pure toNormalize

            traverse_ (Monad.liftIO . MS.pushL q) toNormalize
            pure (nm, tmNorm)
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
                    (return (nm,(Binding nm' sp inl pr tm r)))


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
  = do ctFlat <- flattenCallTree ct
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

flattenCallTree
  :: CallTree
  -> NormalizeSession CallTree
flattenCallTree c@(CLeaf _) = return c
flattenCallTree (CBranch (nm,(Binding nm' sp inl pr tm r)) used) = do
  flattenedUsed   <- mapM flattenCallTree used
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

      when (Maybe.isJust rewriteHistFile) $ do
        lock <- Lens.use ioLock

        MVar.withMVar lock $ \() ->
          Monad.liftIO
            . BS.appendFile (Maybe.fromJust rewriteHistFile)
            . BL.toStrict
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
  where
    flatten =
      repeatR (topdownR (apply "appProp" appProp >->
                 apply "bindConstantVar" bindConstantVar >->
                 apply "caseCon" caseCon >->
                 (apply "reduceConst" reduceConst !-> apply "deadcode" deadCode) >->
                 apply "reduceNonRepPrim" reduceNonRepPrim >->
                 apply "removeUnusedExpr" removeUnusedExpr) >->
               bottomupR (apply "flattenLet" flattenLet)) !->
      topdownSucR (apply "topLet" topLet) >->
      -- See [Note] relation `collapseRHSNoops` and `inlineCleanup`
      -- Note that we do this as the very last step, after all constant propagation
      -- has been done to avoid #3036.
      topdownSucR (apply "collapseRHSNoops" collapseRHSNoops) >->
      topdownSucR (apply "inlineCleanup" inlineCleanup)

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
