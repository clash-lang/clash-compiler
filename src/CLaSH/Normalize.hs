{-# LANGUAGE TemplateHaskell #-}

-- | Turn CoreHW terms into normalized CoreHW Terms
module CLaSH.Normalize where

import           Control.Concurrent.Supply (Supply)
import           Control.Lens              ((.=))
import qualified Control.Lens              as Lens
import qualified Control.Monad.State       as State
import           Data.Either               (partitionEithers)
import           Data.HashMap.Lazy         (HashMap)
import qualified Data.HashMap.Lazy         as HashMap
import           Data.List                 (mapAccumL)
import qualified Data.Map                  as Map
import qualified Data.Maybe                as Maybe
import qualified Data.Set                  as Set
import           Unbound.LocallyNameless   (unembed)

import           CLaSH.Core.FreeVars       (termFreeIds)
import           CLaSH.Core.Pretty         (showDoc)
import           CLaSH.Core.Subst          (substTms)
import           CLaSH.Core.Term           (Term (..), TmName)
import           CLaSH.Core.Type           (Type)
import           CLaSH.Core.Util           (collectArgs, mkApps)
import           CLaSH.Core.Var            (Id,varName)
import           CLaSH.Netlist.Types       (HWType)
import           CLaSH.Netlist.Util        (splitNormalized)
import           CLaSH.Normalize.Strategy
import           CLaSH.Normalize.Transformations ( bindConstantVar, topLet )
import           CLaSH.Normalize.Types
import           CLaSH.Normalize.Util
import           CLaSH.Rewrite.Combinators ((!->),topdownR)
import           CLaSH.Rewrite.Types       (DebugLevel (..), RewriteState (..),
                                            bindings, dbgLevel)
import           CLaSH.Rewrite.Util        (liftRS, runRewrite,
                                            runRewriteSession)
import           CLaSH.Util

-- | Run a NormalizeSession in a given environment
runNormalization :: DebugLevel
                 -- ^ Level of debug messages to print
                 -> Supply
                 -- ^ UniqueSupply
                 -> HashMap TmName (Type,Term)
                 -- ^ Global Binders
                 -> (Type -> Maybe (Either String HWType))
                 -- ^ Hardcoded Type -> HWType translator
                 -> NormalizeSession a
                 -- ^ NormalizeSession to run
                 -> a
runNormalization lvl supply globals typeTrans
  = flip State.evalState normState
  . runRewriteSession lvl rwState
  where
    rwState   = RewriteState 0 globals supply typeTrans
    normState = NormalizeState
                  HashMap.empty
                  Map.empty
                  HashMap.empty
                  100
                  HashMap.empty
                  20
                  (error "Report as bug: no curFun")

-- | Normalize a list of global binders
normalize :: [TmName]
          -> NormalizeSession [(TmName,(Type,Term))]
normalize (bndr:bndrs) = do
  let bndrS = showDoc bndr
  exprM <- fmap (HashMap.lookup bndr) $ Lens.use bindings
  case exprM of
    Just (ty,expr) -> do
      liftRS $ curFun .= bndr
      normalizedExpr <- makeCachedT3' bndr normalized $
                         rewriteExpr ("normalization",normalization) (bndrS,expr)
      let usedBndrs = Set.toList $ termFreeIds normalizedExpr
      if bndr `elem` usedBndrs
        then error $ $(curLoc) ++ "Expr belonging to bndr: " ++ bndrS ++ " remains recursive after normalization."
        else do
          prevNorm <- fmap HashMap.keys $ liftRS $ Lens.use normalized
          let toNormalize = filter (`notElem` prevNorm) usedBndrs
          normalizedOthers <- normalize (toNormalize ++ bndrs)
          return ((bndr,(ty,normalizedExpr)):normalizedOthers)
    Nothing -> error $ $(curLoc) ++ "Expr belonging to bndr: " ++ bndrS ++ " not found"

normalize [] = return []

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

-- | Check if the call graph (second argument), starting at the @topEnity@
-- (first argument) is non-recursive. Returns the list of normalized terms if
-- call graph is indeed non-recursive, errors otherwise.
checkNonRecursive :: TmName -- ^ @topEntity@
                  -> [(TmName,(Type,Term))] -- ^ List of normalized binders
                  -> [(TmName,(Type,Term))]
checkNonRecursive topEntity norm =
  let cg = callGraph [] (HashMap.fromList $ map (second snd) norm) topEntity
  in  case recursiveComponents cg of
       []  -> norm
       rcs -> error $ "Callgraph after normalisation contains following recursive cycles: " ++ show rcs

-- | Perform general \"clean up\" of the normalized (non-recursive) function
-- hierarchy. This includes:
--
--   * Inlining functions that simply \"wrap\" another function
cleanupGraph :: TmName
             -> [(TmName,(Type,Term))]
             -> NormalizeSession [(TmName,(Type,Term))]
cleanupGraph topEntity norm = do
  let ct = mkCallTree [] (HashMap.fromList norm) topEntity
  ctFlat <- flattenCallTree ct
  return (snd $ callTreeToList [] ctFlat)


data CallTree = CLeaf   (TmName,(Type,Term))
              | CBranch (TmName,(Type,Term)) [CallTree]

mkCallTree :: [TmName] -- ^ Visited
           -> HashMap TmName (Type,Term) -- ^ Global binders
           -> TmName -- ^ Root of the call graph
           -> CallTree
mkCallTree visited bindingMap root = case used of
                            [] -> CLeaf   (root,rootTm)
                            _  -> CBranch (root,rootTm) other
  where
    rootTm = Maybe.fromMaybe (error $ show root ++ " is not a global binder") $ HashMap.lookup root bindingMap
    used   = Set.toList $ termFreeIds $ snd rootTm
    other  = map (mkCallTree (root:visited) bindingMap) (filter (`notElem` visited) used)

stripArgs :: [Id]
          -> [Either Term Type]
          -> Maybe [Either Term Type]
stripArgs (_:_) []   = Nothing
stripArgs []    args = Just args
stripArgs (id_:ids) (Left (Var _ nm):args)
      | varName id_ == nm = stripArgs ids args
      | otherwise         = Nothing
stripArgs _ _ = Nothing

flattenNode :: CallTree
            -> NormalizeSession (Either CallTree ((TmName,Term),[CallTree]))
flattenNode c@(CLeaf (nm,(_,e))) = do
  norm <- splitNormalized e
  case norm of
    Right (ids,[(_,bExpr)],_) -> do
      let (fun,args) = collectArgs (unembed bExpr)
      case stripArgs (reverse ids) (reverse args) of
        Just remainder -> return (Right ((nm,mkApps fun (reverse remainder)),[]))
        Nothing        -> return (Left c)
    _ -> return (Left c)
flattenNode b@(CBranch (nm,(_,e)) us) = do
  norm <- splitNormalized e
  case norm of
    Right (ids,[(_,bExpr)],_) -> do
      let (fun,args) = collectArgs (unembed bExpr)
      case stripArgs (reverse ids) (reverse args) of
        Just remainder -> return (Right ((nm,mkApps fun (reverse remainder)),us))
        Nothing        -> return (Left b)
    _ -> return (Left b)

flattenCallTree :: CallTree
                -> NormalizeSession CallTree
flattenCallTree c@(CLeaf _) = return c
flattenCallTree (CBranch (nm,(ty,tm)) used) = do
  flattenedUsed   <- mapM flattenCallTree used
  (newUsed,il_ct) <- partitionEithers <$> mapM flattenNode flattenedUsed
  let (toInline,il_used) = unzip il_ct
  newExpr <- case toInline of
               [] -> return tm
               _  -> rewriteExpr ("bindConstants",(topdownR bindConstantVar) !-> topLet) (showDoc nm, substTms toInline tm)
  return (CBranch (nm,(ty,newExpr)) (newUsed ++ (concat il_used)))

callTreeToList :: [TmName]
               -> CallTree
               -> ([TmName],[(TmName,(Type,Term))])
callTreeToList visited (CLeaf (nm,(ty,tm)))
  | nm `elem` visited = (visited,[])
  | otherwise         = (nm:visited,[(nm,(ty,tm))])
callTreeToList visited (CBranch (nm,(ty,tm)) used)
  | nm `elem` visited = (visited,[])
  | otherwise         = (visited',(nm,(ty,tm)):(concat others))
  where
    (visited',others) = mapAccumL callTreeToList (nm:visited) used
