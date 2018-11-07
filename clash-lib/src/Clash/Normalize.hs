{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2017     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Turn CoreHW terms into normalized CoreHW Terms
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Normalize where

import           Control.Concurrent.Supply        (Supply)
import           Control.Lens                     ((.=),(^.),_1,_4)
import qualified Control.Lens                     as Lens
import           Data.Either                      (partitionEithers)
import           Data.Coerce                      (coerce)
import qualified Data.IntMap                      as IntMap
import           Data.IntMap.Strict               (IntMap)
import           Data.List
  (groupBy, intersect, mapAccumL, sortBy)
import qualified Data.Map                         as Map
import qualified Data.Maybe                       as Maybe
import qualified Data.Set                         as Set
import qualified Data.Set.Lens                    as Lens
import           Data.Semigroup                   ((<>))
import           Data.Text.Prettyprint.Doc        (vcat)

import           BasicTypes                       (InlineSpec (..))
import           SrcLoc                           (SrcSpan,noSrcSpan)

import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs)
import           Clash.Core.Evaluator             (PrimEvaluator)
import           Clash.Core.FreeVars              (termFreeIds, idOccursIn)
import           Clash.Core.Name                  (Name (..), NameSort (..))
import           Clash.Core.Pretty                (showPpr, ppr)
import           Clash.Core.Subst                 (deShadowTerm, extendIdSubstList, mkSubst, substTm)
import           Clash.Core.Term                  (Term (..))
import           Clash.Core.Type                  (Type, splitCoreFunForallTy)
import           Clash.Core.TyCon
  (TyConMap, TyConName)
import           Clash.Core.Util                  (collectArgs, mkApps, termType)
import           Clash.Core.Var                   (Id, varName, varType)
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, eltsVarEnv, emptyInScopeSet, emptyVarEnv,
   extendVarEnv, lookupVarEnv, mapVarEnv, mapMaybeVarEnv, mkInScopeSet, mkVarEnv, mkVarSet, notElemVarEnv, notElemVarSet, nullVarEnv, unionVarEnv)
import           Clash.Driver.Types
  (BindingMap, ClashOpts (..), DebugLevel (..))
import           Clash.Netlist.Types              (HWType (..))
import           Clash.Netlist.Util
  (splitNormalized, unsafeCoreTypeToHWType)
import           Clash.Normalize.Strategy
import           Clash.Normalize.Transformations
  (appProp, bindConstantVar, caseCon, flattenLet, reduceConst, topLet)
import           Clash.Normalize.Types
import           Clash.Normalize.Util
import           Clash.Primitives.Types           (CompiledPrimMap)
import           Clash.Rewrite.Combinators        ((>->),(!->))
import           Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), bindings, curFun, dbgLevel, extra,
   tcCache, topEntities, typeTranslator, customReprs, globalInScope)
import           Clash.Rewrite.Util               (isUntranslatableType,
                                                   runRewrite,
                                                   runRewriteSession)
import Clash.Signal.Internal                      (ResetKind (..))
import           Clash.Unique                     (uniqMapToUniqSet)
import           Clash.Util

-- | Run a NormalizeSession in a given environment
runNormalization
  :: ClashOpts
  -- ^ Level of debug messages to print
  -> Supply
  -- ^ UniqueSupply
  -> BindingMap
  -- ^ Global Binders
  -> (CustomReprs -> TyConMap -> Bool -> Type -> Maybe (Either String HWType))
  -- ^ Hardcoded Type -> HWType translator
  -> CustomReprs
  -> TyConMap
  -- ^ TyCon cache
  -> IntMap TyConName
  -- ^ Tuple TyCon cache
  -> PrimEvaluator
  -- ^ Hardcoded evaluator (delta-reduction)
  -> CompiledPrimMap
  -- ^ Primitive Definitions
  -> VarEnv Bool
  -- ^ Map telling whether a components is part of a recursive group
  -> [Id]
  -- ^ topEntities
  -> NormalizeSession a
  -- ^ NormalizeSession to run
  -> a
runNormalization opts supply globals typeTrans reprs tcm tupTcm eval primMap rcsMap topEnts
  = runRewriteSession rwEnv rwState
  where
    globalsInScope = mkInScopeSet . mkVarSet . map (Lens.view _1) $ eltsVarEnv globals

    rwEnv     = RewriteEnv
                  (opt_dbgLevel opts)
                  typeTrans
                  tcm
                  tupTcm
                  eval
                  (mkVarSet topEnts)
                  reprs

    rwState   = RewriteState
                  0
                  globals
                  supply
                  (error $ $(curLoc) ++ "Report as bug: no curFun",noSrcSpan)
                  0
                  (IntMap.empty, 0)
                  globalsInScope
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
                  rcsMap


normalize
  :: [Id]
  -> NormalizeSession BindingMap
normalize []  = return emptyVarEnv
normalize top = do
  (new,topNormalized) <- unzip <$> mapM normalize' top
  newNormalized <- normalize (concat new)
  return (unionVarEnv (mkVarEnv topNormalized) newNormalized)

normalize'
  :: Id
  -> NormalizeSession ([Id],(Id,(Id,SrcSpan,InlineSpec,Term)))
normalize' nm = do
  exprM <- lookupVarEnv nm <$> Lens.use bindings
  let nmS = showPpr (varName nm)
  case exprM of
    Just (nm',sp,inl,tm) -> do
      tcm <- Lens.view tcCache
      reprs <- Lens.view customReprs
      let (_,resTy) = splitCoreFunForallTy tcm (varType nm')
      resTyRep <- not <$> isUntranslatableType False resTy
      if resTyRep
         then do
            tmNorm <- makeCachedU nm (extra.normalized) $ do
                        -- We deshadow the term because sometimes GHC gives us
                        -- code where a local binder has the same unique as a
                        -- global binder, sometimes causing the inliner to go
                        -- into a loop. Deshadowing freshens all the bindings
                        -- to avoid this.
                        --
                        -- TODO: See if we can remove this, now that the
                        -- TODO: inlining functions account for global vars
                        is0 <- Lens.use globalInScope
                        let tm1 = deShadowTerm is0 tm
                        curFun .= (nm',sp)
                        tm2 <- rewriteExpr ("normalization",normalization) (nmS,tm1)
                        let ty' = termType tcm tm2
                        return (nm' {varType = ty'},sp,inl,tm2)
            let usedBndrs = Lens.toListOf termFreeIds (tmNorm ^. _4)
            traceIf (nm `elem` usedBndrs)
                    (concat [ $(curLoc),"Expr belonging to bndr: ",nmS ," (:: "
                            , showPpr (varType (tmNorm ^. _1))
                            , ") remains recursive after normalization:\n"
                            , showPpr (tmNorm ^. _4) ])
                    (return ())
            tyTrans <- Lens.view typeTranslator
            case clockResetErrors sp reprs tyTrans tcm (varType nm') of
              msgs@(_:_) -> traceIf True (concat (nmS:" (:: ":showPpr (varType (tmNorm ^. _1))
                              :")\nhas potentially dangerous meta-stability issues:\n\n"
                              :msgs))
                              (return ())
              _ -> return ()
            prevNorm <- mapVarEnv (Lens.view _1) <$> Lens.use (extra.normalized)
            topEnts  <- Lens.view topEntities
            let toNormalize = filter (`notElemVarSet` topEnts)
                            $ filter (`notElemVarEnv` (extendVarEnv nm nm prevNorm)) usedBndrs
            return (toNormalize,(nm,tmNorm))
         else do
            let usedBndrs = Lens.toListOf termFreeIds tm
            prevNorm <- mapVarEnv (Lens.view _1) <$> Lens.use (extra.normalized)
            topEnts  <- Lens.view topEntities
            let toNormalize = filter (`notElemVarSet` topEnts)
                            $ filter (`notElemVarEnv` (extendVarEnv nm nm prevNorm)) usedBndrs
            lvl <- Lens.view dbgLevel
            traceIf (lvl >= DebugFinal)
                    (concat [$(curLoc), "Expr belonging to bndr: ", nmS, " (:: "
                            , showPpr (varType nm')
                            , ") has a non-representable return type."
                            , " Not normalising:\n", showPpr tm] )
                    (return (toNormalize,(nm,(nm',sp,inl,tm))))
    Nothing -> error $ $(curLoc) ++ "Expr belonging to bndr: " ++ nmS ++ " not found"

-- | Rewrite a term according to the provided transformation
rewriteExpr :: (String,NormRewrite) -- ^ Transformation to apply
            -> (String,Term) -- ^ Term to transform
            -> NormalizeSession Term
rewriteExpr (nrwS,nrw) (bndrS,expr) = do
  lvl <- Lens.view dbgLevel
  let before = showPpr expr
  let expr' = traceIf (lvl >= DebugFinal)
                (bndrS ++ " before " ++ nrwS ++ ":\n\n" ++ before ++ "\n")
                expr
  rewritten <- runRewrite nrwS emptyInScopeSet nrw expr'
  let after = showPpr rewritten
  traceIf (lvl >= DebugFinal)
    (bndrS ++ " after " ++ nrwS ++ ":\n\n" ++ after ++ "\n") $
    return rewritten

-- | Check whether the normalized bindings are non-recursive. Errors when one
-- of the components is recursive.
checkNonRecursive
  :: BindingMap
  -- ^ List of normalized binders
  -> BindingMap
checkNonRecursive norm = case mapMaybeVarEnv go norm of
  rcs | nullVarEnv rcs  -> norm
  rcs -> error $ $(curLoc) ++ "Callgraph after normalisation contains following recursive components: "
                   ++ show (vcat [ ppr a <> ppr b
                                 | (a,b) <- eltsVarEnv rcs
                                 ])
 where
  go (nm,_,_,tm) =
    if nm `idOccursIn` tm
       then Just (nm,tm)
       else Nothing


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
  = do let inScope = mkInScopeSet (uniqMapToUniqSet (mapVarEnv (coerce . Lens.view _1) norm))
       ctFlat <- flattenCallTree inScope ct
       return (mkVarEnv $ snd $ callTreeToList [] ctFlat)
cleanupGraph _ norm = return norm

data CallTree = CLeaf   (Id,(Id,SrcSpan,InlineSpec,Term))
              | CBranch (Id,(Id,SrcSpan,InlineSpec,Term)) [CallTree]

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
  = let used   = Set.toList $ Lens.setOf termFreeIds $ (rootTm ^. _4)
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
    mentionsId t = not $ null (either (Lens.toListOf termFreeIds) (const []) t
                              `intersect`
                              allIds)

stripArgs allIds (id_:ids) (Left (Var nm):args)
      | id_ == nm = stripArgs allIds ids args
      | otherwise = Nothing
stripArgs _ _ _ = Nothing

flattenNode
  :: CallTree
  -> NormalizeSession (Either CallTree ((Id,Term),[CallTree]))
flattenNode (CLeaf (nm,(nameSort . varName -> Internal,_,_,e))) =
  return (Right ((nm,e),[]))
flattenNode c@(CLeaf (nm,(_,_,_,e))) = do
  tcm  <- Lens.view tcCache
  let norm = splitNormalized tcm e
  case norm of
    Right (ids,[(_,bExpr)],_) -> do
      let (fun,args) = collectArgs bExpr
      case stripArgs ids (reverse ids) (reverse args) of
        Just remainder -> return (Right ((nm,mkApps fun (reverse remainder)),[]))
        Nothing        -> return (Right ((nm,e),[]))
    _ | isCheapFunction e -> return (Right ((nm,e),[]))
      | otherwise         -> return (Left c)
flattenNode (CBranch (nm,(nameSort . varName -> Internal,_,_,e)) us) =
  return (Right ((nm,e),us))
flattenNode b@(CBranch (nm,(_,_,_,e)) us) = do
  tcm  <- Lens.view tcCache
  let norm = splitNormalized tcm e
  case norm of
    Right (ids,[(_,bExpr)],_) -> do
      let (fun,args) = collectArgs bExpr
      case stripArgs ids (reverse ids) (reverse args) of
        Just remainder -> return (Right ((nm,mkApps fun (reverse remainder)),us))
        Nothing        -> return (Right ((nm,e),us))
    _ | isCheapFunction e -> return (Right ((nm,e),us))
      | otherwise         -> return (Left b)

flattenCallTree
  :: InScopeSet
  -> CallTree
  -> NormalizeSession CallTree
flattenCallTree _ c@(CLeaf _) = return c
flattenCallTree is (CBranch (nm,(nm',sp,inl,tm)) used) = do
  flattenedUsed   <- mapM (flattenCallTree is) used
  (newUsed,il_ct) <- partitionEithers <$> mapM flattenNode flattenedUsed
  let (toInline,il_used) = unzip il_ct
      subst = extendIdSubstList (mkSubst is) toInline
  newExpr <- case toInline of
               [] -> return tm
               _  -> rewriteExpr ("flattenExpr",flatten) (showPpr nm, substTm "flattenCallTree.flattenExpr" subst tm)
  let allUsed = newUsed ++ concat il_used
  -- inline all components when the resulting expression after flattening
  -- is still considered "cheap". This happens often at the topEntity which
  -- wraps another functions and has some selectors and data-constructors.
  if isCheapFunction newExpr
     then do
        let (toInline',allUsed') = unzip (map goCheap allUsed)
            subst' = extendIdSubstList (mkSubst is) toInline'
        newExpr' <- rewriteExpr ("flattenCheap",flatten) (showPpr nm, substTm "flattenCallTree.flattenCheap" subst' newExpr)
        return (CBranch (nm,(nm',sp,inl,newExpr')) (concat allUsed'))
     else return (CBranch (nm,(nm',sp,inl,newExpr)) allUsed)
  where
    flatten =
      innerMost (appProp >-> bindConstantVar >-> caseCon >-> reduceConst >-> flattenLet) !->
      topdownSucR topLet

    goCheap (CLeaf   (nm2,(_,_,_,e)))    = ((nm2,e),[])
    goCheap (CBranch (nm2,(_,_,_,e)) us) = ((nm2,e),us)

callTreeToList
  :: [Id]
  -> CallTree
  -> ([Id],[(Id,(Id,SrcSpan,InlineSpec,Term))])
callTreeToList visited (CLeaf (nm,bndr))
  | nm `elem` visited = (visited,[])
  | otherwise         = (nm:visited,[(nm,bndr)])
callTreeToList visited (CBranch (nm,bndr) used)
  | nm `elem` visited = (visited,[])
  | otherwise         = (visited',(nm,bndr):(concat others))
  where
    (visited',others) = mapAccumL callTreeToList (nm:visited) used

-- | Clash's clock and reset domain annotations prevent most accidental
-- meta-stability situations. That is, unless the developer uses the
-- functions marked "unsafe", the type system will prevent things like
-- illegal clock domain crossing, or improper use of asynchronous resets.
--
-- However, this all depends on clock and resets being unique. With explicit
-- clocks and resets, it is possible to have multiple clock and reset arguments
-- that are annotated with the same domain. If these arguments aren't connected
-- to the same source, we can still get metastability due to either illegal
-- clock domain crossing, or improper use of asynchronous resets.
--
-- The following situations are reported:
-- * There are 2 or more clock arguments in scope that have the same clock
--   domain annotation.
-- * There are 2 or more reset arguments in scope that have the same reset
--   domain annotation, and at least one of them is an asynchronous reset.
clockResetErrors
  :: SrcSpan
  -> CustomReprs
  -> (CustomReprs -> TyConMap -> Bool -> Type -> Maybe (Either String HWType))
  -> TyConMap
  -> Type
  -> [String]
clockResetErrors sp reprs tyTran tcm ty =
   (Maybe.mapMaybe reportClock clks ++ Maybe.mapMaybe reportResets rsts)
  where
    (args,_)  = splitCoreFunForallTy tcm ty
    (_,args') = partitionEithers args
    hwArgs    = zip (map (unsafeCoreTypeToHWType sp $(curLoc) tyTran reprs tcm False) args') args'
    clks      = groupBy ((==) `on` fst) . sortBy (compare `on` fst)
              $ [ ((nm,i),ty') | (Clock nm i _,ty') <- hwArgs]
    rsts      = groupBy ((==) `on` (fst.fst)) . sortBy (compare `on` (fst.fst))
              $ [ (((nm,i),s),ty') | (Reset nm i s,ty') <- hwArgs]

    reportClock clks'
      | length clks' >= 2
      = Just
      $ concat ["The following clocks:\n"
               ,concatMap (\c -> "* " ++ showPpr (snd c) ++ "\n") clks'
               ,"belong to the same clock domain and should be connected to "
               ,"the same clock source in order to prevent meta-stability "
               ,"issues."
               ]
      | otherwise
      = Nothing

    reportResets rsts'
      | length rsts' >= 2
      , any (\((_,sync),_) -> sync == Asynchronous) rsts'
      = Just
      $ concat ["The following resets:\n"
               ,concatMap (\c -> "* " ++ showPpr (snd c) ++ "\n") rsts'
               ,"belong to the same reset domain, and one or more of these "
               ,"resets is Asynchronous. Ensure that these resets are "
               ,"synchronized in order to prevent meta-stability issues."
               ]
    reportResets _ = Nothing
