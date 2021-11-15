{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2017     , Google Inc.,
                     2021     , QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Turn CoreHW terms into normalized CoreHW Terms
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize where

import           Control.Concurrent.Supply        (Supply)
import           Control.Exception                (throw)
import qualified Control.Lens                     as Lens
import           Control.Monad                    (when)
import           Control.Monad.State.Strict       (State)
import           Data.Default                     (def)
import           Data.Either                      (lefts,partitionEithers)
import qualified Data.IntMap                      as IntMap
import           Data.IntMap.Strict               (IntMap)
import           Data.List
  (intersect, mapAccumL)
import qualified Data.Map                         as Map
import qualified Data.Maybe                       as Maybe
import qualified Data.Set                         as Set
import qualified Data.Set.Lens                    as Lens

#if MIN_VERSION_prettyprinter(1,7,0)
import           Prettyprinter                    (vcat)
#else
import           Data.Text.Prettyprint.Doc        (vcat)
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.Basic                  (InlineSpec (..))
#else
import           BasicTypes                       (InlineSpec (..))
#endif

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
import           Clash.Core.TyCon
  (TyConMap, TyConName)
import           Clash.Core.Type                  (isPolyTy)
import           Clash.Core.Var                   (Id, varName, varType)
import           Clash.Core.VarEnv
  (VarEnv, elemVarSet, eltsVarEnv, emptyInScopeSet, emptyVarEnv,
   extendVarEnv, lookupVarEnv, mapVarEnv, mapMaybeVarEnv,
   mkVarEnv, mkVarSet, notElemVarEnv, notElemVarSet, nullVarEnv, unionVarEnv)
import           Clash.Debug                      (traceIf)
import           Clash.Driver.Types
  (BindingMap, Binding(..), ClashOpts (..), DebugOpts(..))
import           Clash.Netlist.Types
  (HWMap, FilteredHWType(..))
import           Clash.Netlist.Util
  (splitNormalized)
import           Clash.Normalize.Strategy
import           Clash.Normalize.Transformations
import           Clash.Normalize.Types
import           Clash.Normalize.Util
import           Clash.Primitives.Types           (CompiledPrimMap)
import           Clash.Rewrite.Combinators        ((>->),(!->),repeatR,topdownR)
import           Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), bindings, debugOpts, extra,
   tcCache, topEntities)
import           Clash.Rewrite.Util
  (apply, isUntranslatableType, runRewriteSession)
import           Clash.Util
import           Clash.Util.Interpolate           (i)

import           Data.Binary                      (encode)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL

import           System.IO.Unsafe                 (unsafePerformIO)
import           Clash.Rewrite.Types (RewriteStep(..))


-- | Run a NormalizeSession in a given environment
runNormalization
  :: ClashOpts
  -- ^ Level of debug messages to print
  -> Supply
  -- ^ UniqueSupply
  -> BindingMap
  -- ^ Global Binders
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcoded Type -> HWType translator
  -> CustomReprs
  -> TyConMap
  -- ^ TyCon cache
  -> IntMap TyConName
  -- ^ Tuple TyCon cache
  -> PE.Evaluator
  -- ^ Hardcoded evaluator for partial evaluation
  -> WHNF.Evaluator
  -- ^ Hardcoded evaluator for WHNF (old evaluator)
  -> CompiledPrimMap
  -- ^ Primitive Definitions
  -> VarEnv Bool
  -- ^ Map telling whether a components is part of a recursive group
  -> [Id]
  -- ^ topEntities
  -> NormalizeSession a
  -- ^ NormalizeSession to run
  -> a
runNormalization opts supply globals typeTrans reprs tcm tupTcm peEval eval primMap rcsMap topEnts
  = runRewriteSession rwEnv rwState
  where
    rwEnv     = RewriteEnv
                  (opt_debug opts)
                  (opt_aggressiveXOpt opts)
                  typeTrans
                  tcm
                  tupTcm
                  peEval
                  eval
                  (mkVarSet topEnts)
                  reprs
                  (opt_evaluatorFuelLimit opts)

    rwState   = RewriteState
                  0
                  mempty       -- transformCounters Map
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
                  (opt_specLimit opts)
                  emptyVarEnv
                  (opt_inlineLimit opts)
                  (opt_inlineFunctionLimit opts)
                  (opt_inlineConstantLimit opts)
                  primMap
                  Map.empty
                  rcsMap
                  (opt_newInlineStrat opts)
                  (opt_ultra opts)
                  (opt_inlineWFCacheLimit opts)


normalize
  :: [Id]
  -> NormalizeSession BindingMap
normalize []  = return emptyVarEnv
normalize top = do
  (new,topNormalized) <- unzip <$> mapM normalize' top
  newNormalized <- normalize (concat new)
  return (unionVarEnv (mkVarEnv topNormalized) newNormalized)

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
flattenNode c@(CLeaf (_,(Binding _ _ NoInline _ _ _))) = return (Left c)
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
flattenNode b@(CBranch (_,(Binding _ _ NoInline _ _ _)) _) =
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
        newInlineStrat <- Lens.use (extra.newInlineStrategy)
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
  let allUsed = newUsed ++ concat il_used
  -- inline all components when the resulting expression after flattening
  -- is still considered "cheap". This happens often at the topEntity which
  -- wraps another functions and has some selectors and data-constructors.
  if inl /= NoInline && isCheapFunction newExpr
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
                 apply "removeUnusedExpr" removeUnusedExpr >->
                 apply "flattenLet" flattenLet)) !->
      topdownSucR (apply "topLet" topLet)

    goCheap c@(CLeaf   (nm2,(Binding _ _ inl2 _ e _)))
      | inl2 == NoInline = (Nothing     ,[c])
      | otherwise        = (Just (nm2,e),[])
    goCheap c@(CBranch (nm2,(Binding _ _ inl2 _ e _)) us)
      | inl2 == NoInline = (Nothing, [c])
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
