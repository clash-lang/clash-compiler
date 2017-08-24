{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Turn CoreHW terms into normalized CoreHW Terms
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module CLaSH.Normalize where

import           Control.Concurrent.Supply        (Supply)
import           Control.Lens                     ((.=),(^.),_2,_5)
import qualified Control.Lens                     as Lens
import           Data.Either                      (partitionEithers)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import qualified Data.HashSet                     as HashSet
import           Data.IntMap.Strict               (IntMap)
import           Data.List
  (groupBy, intersect, mapAccumL, sortBy)
import qualified Data.Map                         as Map
import qualified Data.Maybe                       as Maybe
import qualified Data.Set                         as Set
import qualified Data.Set.Lens                    as Lens
import           Unbound.Generics.LocallyNameless (unembed)

import           BasicTypes                       (InlineSpec (..))
import           SrcLoc                           (SrcSpan,noSrcSpan)

import           CLaSH.Core.FreeVars              (termFreeIds)
import           CLaSH.Core.Name                  (Name (..), NameSort (..))
import           CLaSH.Core.Pretty                (showDoc)
import           CLaSH.Core.Subst                 (substTms)
import           CLaSH.Core.Term                  (Term (..), TmName, TmOccName)
import           CLaSH.Core.Type                  (Type, splitCoreFunForallTy)
import           CLaSH.Core.TyCon
  (TyCon, TyConName, TyConOccName)
import           CLaSH.Core.Util                  (collectArgs, mkApps, termType)
import           CLaSH.Core.Var                   (Id,varName)
import           CLaSH.Driver.Types
  (BindingMap, CLaSHOpts (..), DebugLevel (..))
import           CLaSH.Netlist.BlackBox.Types     (BlackBoxTemplate)
import           CLaSH.Netlist.Types              (HWType (..))
import           CLaSH.Netlist.Util
  (splitNormalized, unsafeCoreTypeToHWType)
import           CLaSH.Normalize.Strategy
import           CLaSH.Normalize.Transformations
  (appProp, bindConstantVar, caseCon, flattenLet, reduceConst, topLet)
import           CLaSH.Normalize.Types
import           CLaSH.Normalize.Util
import           CLaSH.Primitives.Types           (PrimMap)
import           CLaSH.Rewrite.Combinators        ((>->),(!->))
import           CLaSH.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), bindings, curFun, dbgLevel, extra,
   tcCache, topEntities, typeTranslator)
import           CLaSH.Rewrite.Util               (isUntranslatableType,
                                                   runRewrite,
                                                   runRewriteSession)
import CLaSH.Signal.Internal                      (ResetKind (..))
import           CLaSH.Util

-- | Run a NormalizeSession in a given environment
runNormalization
  :: CLaSHOpts
  -- ^ Level of debug messages to print
  -> Supply
  -- ^ UniqueSupply
  -> BindingMap
  -- ^ Global Binders
  -> (HashMap TyConOccName TyCon -> Type -> Maybe (Either String HWType))
  -- ^ Hardcoded Type -> HWType translator
  -> HashMap TyConOccName TyCon
  -- ^ TyCon cache
  -> IntMap TyConName
  -- ^ Tuple TyCon cache
  -> (HashMap TyConOccName TyCon -> Bool -> Term -> Term)
  -- ^ Hardcoded evaluator (delta-reduction)
  -> PrimMap BlackBoxTemplate
  -- ^ Primitive Definitions
  -> HashMap TmOccName Bool
  -- ^ Map telling whether a components is part of a recursive group
  -> [TmOccName]
  -- ^ topEntities
  -> NormalizeSession a
  -- ^ NormalizeSession to run
  -> a
runNormalization opts supply globals typeTrans tcm tupTcm eval primMap rcsMap topEnts
  = runRewriteSession rwEnv rwState
  where
    rwEnv     = RewriteEnv
                  (opt_dbgLevel opts)
                  typeTrans
                  tcm
                  tupTcm
                  eval
                  (opt_allowZero opts)
                  (HashSet.fromList topEnts)

    rwState   = RewriteState
                  0
                  globals
                  supply
                  (error $ $(curLoc) ++ "Report as bug: no curFun",noSrcSpan)
                  0
                  normState

    normState = NormalizeState
                  HashMap.empty
                  Map.empty
                  HashMap.empty
                  (opt_specLimit opts)
                  HashMap.empty
                  (opt_inlineLimit opts)
                  (opt_inlineBelow opts)
                  primMap
                  rcsMap


normalize
  :: [TmOccName]
  -> NormalizeSession BindingMap
normalize []  = return HashMap.empty
normalize top = do
  (new,topNormalized) <- unzip <$> mapM normalize' top
  newNormalized <- normalize (concat new)
  return (HashMap.union (HashMap.fromList topNormalized) newNormalized)

normalize'
  :: TmOccName
  -> NormalizeSession ([TmOccName],(TmOccName,(TmName,Type,SrcSpan,InlineSpec,Term)))
normalize' nm = do
  exprM <- HashMap.lookup nm <$> Lens.use bindings
  let nmS = showDoc nm
  case exprM of
    Just (nm',ty,sp,inl,tm) -> do
      tcm <- Lens.view tcCache
      let (_,resTy) = splitCoreFunForallTy tcm ty
      resTyRep <- not <$> isUntranslatableType resTy
      if resTyRep
         then do
            tmNorm <- makeCached nm (extra.normalized) $ do
                        curFun .= (nm',sp)
                        tm' <- rewriteExpr ("normalization",normalization) (nmS,tm)
                        ty' <- termType tcm tm'
                        return (nm',ty',sp,inl,tm')
            let usedBndrs = Lens.toListOf termFreeIds (tmNorm ^. _5)
            traceIf (nm `elem` usedBndrs)
                    (concat [ $(curLoc),"Expr belonging to bndr: ",nmS ," (:: "
                            , showDoc (tmNorm ^. _2)
                            , ") remains recursive after normalization:\n"
                            , showDoc (tmNorm ^. _5) ])
                    (return ())
            tyTrans <- Lens.view typeTranslator
            case clockResetErrors tyTrans tcm ty of
              msgs@(_:_) -> traceIf True (concat (nmS:" (:: ":showDoc (tmNorm ^. _2)
                              :")\nhas potentially dangerous meta-stability issues:\n\n"
                              :msgs))
                              (return ())
              _ -> return ()
            prevNorm <- fmap HashMap.keys $ Lens.use (extra.normalized)
            topEnts  <- Lens.view topEntities
            let toNormalize = filter (not . (`HashSet.member` topEnts))
                            $ filter (`notElem` (nm:prevNorm)) usedBndrs
            return (toNormalize,(nm,tmNorm))
         else do
            let usedBndrs = Lens.toListOf termFreeIds tm
            prevNorm <- fmap HashMap.keys $ Lens.use (extra.normalized)
            topEnts  <- Lens.view topEntities
            let toNormalize = filter (not . (`HashSet.member` topEnts))
                            $ filter (`notElem` (nm:prevNorm)) usedBndrs
            lvl <- Lens.view dbgLevel
            traceIf (lvl >= DebugFinal)
                    (concat [$(curLoc), "Expr belonging to bndr: ", nmS, " (:: "
                            , showDoc ty
                            , ") has a non-representable return type."
                            , " Not normalising:\n", showDoc tm] )
                    (return (toNormalize,(nm,(nm',ty,sp,inl,tm))))
    Nothing -> error $ $(curLoc) ++ "Expr belonging to bndr: " ++ nmS ++ " not found"

-- | Rewrite a term according to the provided transformation
rewriteExpr :: (String,NormRewrite) -- ^ Transformation to apply
            -> (String,Term) -- ^ Term to transform
            -> NormalizeSession Term
rewriteExpr (nrwS,nrw) (bndrS,expr) = do
  lvl <- Lens.view dbgLevel
  let before = showDoc expr
  let expr' = traceIf (lvl >= DebugFinal)
                (bndrS ++ " before " ++ nrwS ++ ":\n\n" ++ before ++ "\n")
                expr
  rewritten <- runRewrite nrwS nrw expr'
  let after = showDoc rewritten
  traceIf (lvl >= DebugFinal)
    (bndrS ++ " after " ++ nrwS ++ ":\n\n" ++ after ++ "\n") $
    return rewritten

-- | Check whether the normalized bindings are non-recursive. Errors when one
-- of the components is recursive.
checkNonRecursive
  :: BindingMap
  -- ^ List of normalized binders
  -> BindingMap
checkNonRecursive norm = case Maybe.mapMaybe go (HashMap.toList norm) of
    []  -> norm
    rcs -> error $ $(curLoc) ++ "Callgraph after normalisation contains following recursive components: " ++ show rcs
  where
    go (nm,(_,_,_,_,tm)) =
      let used = Lens.toListOf termFreeIds tm
      in  if nm `elem` used
             then Just (nm,tm)
             else Nothing

-- | Perform general \"clean up\" of the normalized (non-recursive) function
-- hierarchy. This includes:
--
--   * Inlining functions that simply \"wrap\" another function
cleanupGraph
  :: TmOccName
  -> BindingMap
  -> NormalizeSession BindingMap
cleanupGraph topEntity norm
  | Just ct <- mkCallTree [] norm topEntity
  = do ctFlat <- flattenCallTree ct
       return (HashMap.fromList $ snd $ callTreeToList [] ctFlat)
cleanupGraph _ norm = return norm

data CallTree = CLeaf   (TmOccName,(TmName,Type,SrcSpan,InlineSpec,Term))
              | CBranch (TmOccName,(TmName,Type,SrcSpan,InlineSpec,Term)) [CallTree]

mkCallTree
  :: [TmOccName]
  -- ^ Visited
  -> BindingMap
  -- ^ Global binders
  -> TmOccName
  -- ^ Root of the call graph
  -> Maybe CallTree
mkCallTree visited bindingMap root
  | Just rootTm <- HashMap.lookup root bindingMap
  = let used   = Set.toList $ Lens.setOf termFreeIds $ (rootTm ^. _5)
        other  = Maybe.mapMaybe (mkCallTree (root:visited) bindingMap) (filter (`notElem` visited) used)
    in  case used of
          [] -> Just (CLeaf   (root,rootTm))
          _  -> Just (CBranch (root,rootTm) other)
mkCallTree _ _ _ = Nothing

stripArgs
  :: [TmOccName]
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

stripArgs allIds (id_:ids) (Left (Var _ nm):args)
      | varName id_ == nm = stripArgs allIds ids args
      | otherwise         = Nothing
stripArgs _ _ _ = Nothing

flattenNode
  :: CallTree
  -> NormalizeSession (Either CallTree ((TmOccName,Term),[CallTree]))
flattenNode (CLeaf (nm,(nameSort -> Internal,_,_,_,e))) =
  return (Right ((nm,e),[]))
flattenNode c@(CLeaf (nm,(_,_,_,_,e))) = do
  tcm  <- Lens.view tcCache
  norm <- splitNormalized tcm e
  case norm of
    Right (ids,[(_,bExpr)],_) -> do
      let (fun,args) = collectArgs (unembed bExpr)
      case stripArgs (map (nameOcc.varName) ids) (reverse ids) (reverse args) of
        Just remainder -> return (Right ((nm,mkApps fun (reverse remainder)),[]))
        Nothing        -> return (Right ((nm,e),[]))
    _ | isCheapFunction e -> return (Right ((nm,e),[]))
      | otherwise         -> return (Left c)
flattenNode (CBranch (nm,(nameSort -> Internal,_,_,_,e)) us) =
  return (Right ((nm,e),us))
flattenNode b@(CBranch (nm,(_,_,_,_,e)) us) = do
  tcm  <- Lens.view tcCache
  norm <- splitNormalized tcm e
  case norm of
    Right (ids,[(_,bExpr)],_) -> do
      let (fun,args) = collectArgs (unembed bExpr)
      case stripArgs (map (nameOcc.varName) ids) (reverse ids) (reverse args) of
        Just remainder -> return (Right ((nm,mkApps fun (reverse remainder)),us))
        Nothing        -> return (Right ((nm,e),us))
    _ | isCheapFunction e -> return (Right ((nm,e),us))
      | otherwise         -> return (Left b)

flattenCallTree
  :: CallTree
  -> NormalizeSession CallTree
flattenCallTree c@(CLeaf _) = return c
flattenCallTree (CBranch (nm,(nm',ty,sp,inl,tm)) used) = do
  flattenedUsed   <- mapM flattenCallTree used
  (newUsed,il_ct) <- partitionEithers <$> mapM flattenNode flattenedUsed
  let (toInline,il_used) = unzip il_ct
  newExpr <- case toInline of
               [] -> return tm
               _  -> rewriteExpr ("flattenExpr",flatten) (showDoc nm, substTms toInline tm)
  let allUsed = newUsed ++ concat il_used
  -- inline all components when the resulting expression after flattening
  -- is still considered "cheap". This happens often at the topEntity which
  -- wraps another functions and has some selectors and data-constructors.
  if isCheapFunction newExpr
     then do
        let (toInline',allUsed') = unzip (map goCheap allUsed)
        newExpr' <- rewriteExpr ("flattenCheap",flatten) (showDoc nm, substTms toInline' newExpr)
        return (CBranch (nm,(nm',ty,sp,inl,newExpr')) (concat allUsed'))
     else return (CBranch (nm,(nm',ty,sp,inl,newExpr)) allUsed)
  where
    flatten =
      innerMost (appProp >-> bindConstantVar >-> caseCon >-> reduceConst >-> flattenLet) !->
      topdownSucR topLet

    goCheap (CLeaf   (nm2,(_,_,_,_,e)))    = ((nm2,e),[])
    goCheap (CBranch (nm2,(_,_,_,_,e)) us) = ((nm2,e),us)

callTreeToList
  :: [TmOccName]
  -> CallTree
  -> ([TmOccName],[(TmOccName,(TmName,Type,SrcSpan,InlineSpec,Term))])
callTreeToList visited (CLeaf (nm,bndr))
  | nm `elem` visited = (visited,[])
  | otherwise         = (nm:visited,[(nm,bndr)])
callTreeToList visited (CBranch (nm,bndr) used)
  | nm `elem` visited = (visited,[])
  | otherwise         = (visited',(nm,bndr):(concat others))
  where
    (visited',others) = mapAccumL callTreeToList (nm:visited) used

-- | CLaSH's clock and reset domain annotations prevent most accidental
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
  :: (HashMap TyConOccName TyCon -> Type -> Maybe (Either String HWType))
  -> HashMap TyConOccName TyCon
  -> Type
  -> [String]
clockResetErrors tyTran tcm ty =
   (Maybe.mapMaybe reportClock clks ++ Maybe.mapMaybe reportResets rsts)
  where
    (args,_)  = splitCoreFunForallTy tcm ty
    (_,args') = partitionEithers args
    hwArgs    = zip (map (unsafeCoreTypeToHWType $(curLoc) tyTran tcm) args') args'
    clks      = groupBy ((==) `on` fst) . sortBy (compare `on` fst)
              $ [ ((nm,i),ty') | (Clock nm i _,ty') <- hwArgs]
    rsts      = groupBy ((==) `on` (fst.fst)) . sortBy (compare `on` (fst.fst))
              $ [ (((nm,i),s),ty') | (Reset nm i s,ty') <- hwArgs]

    reportClock clks'
      | length clks' >= 2
      = Just
      $ concat ["The following clocks:\n"
               ,concatMap (\c -> "* " ++ showDoc (snd c) ++ "\n") clks'
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
               ,concatMap (\c -> "* " ++ showDoc (snd c) ++ "\n") rsts'
               ,"belong to the same reset domain, and one or more of these "
               ,"resets is Asynchronous. Ensure that these resets are "
               ,"synchronized in order to prevent meta-stability issues."
               ]
    reportResets _ = Nothing
