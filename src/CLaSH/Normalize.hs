{-# LANGUAGE TemplateHaskell #-}
module CLaSH.Normalize where

import           Control.Concurrent.Supply (Supply)
import           Control.Lens              ((.=))
import qualified Control.Lens              as Lens
import qualified Control.Monad.State       as State
import           Data.HashMap.Lazy         (HashMap)
import qualified Data.HashMap.Lazy         as HashMap
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set

import           CLaSH.Core.FreeVars       (termFreeIds)
import           CLaSH.Core.Pretty         (showDoc)
import           CLaSH.Core.Term           (Term, TmName)
import           CLaSH.Core.Type           (Type)
import           CLaSH.Netlist.Types       (HWType)
import           CLaSH.Normalize.Strategy
import           CLaSH.Normalize.Types
import           CLaSH.Normalize.Util
import           CLaSH.Rewrite.Types       (DebugLevel (..), RewriteState (..),
                                            bindings, dbgLevel)
import           CLaSH.Rewrite.Util        (liftRS, runRewrite,
                                            runRewriteSession)
import           CLaSH.Util

runNormalization ::
  DebugLevel
  -> Supply
  -> HashMap TmName (Type,Term)
  -> (Type -> Maybe (Either String HWType))
  -> NormalizeSession a
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
                  []
                  (error "Report as bug: no curFun")

normalize ::
  [TmName]
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

rewriteExpr ::
  (String,NormRewrite)
  -> (String,Term)
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

cleanupGraph :: [TmName] -> [(TmName,(Type,Term))] -> NormalizeSession [(TmName,(Type,Term))]
cleanupGraph bndrs norm = do
    bindings .= HashMap.fromList norm
    cleanupGraph' ("cleanup",cleanup) bndrs
  where
    cleanupGraph' :: (String,NormRewrite) -> [TmName] -> NormalizeSession [(TmName,(Type,Term))]
    cleanupGraph' rw (bndr:bndrs') = do
      let bndrS = showDoc bndr
      exprM <- fmap (HashMap.lookup bndr) $ Lens.use bindings
      case exprM of
        Just (ty,expr) -> do
          liftRS $ curFun .= bndr
          cleaned <- rewriteExpr rw (bndrS,expr)
          let usedBndrs = Set.toList $ termFreeIds cleaned
          cleanedOthers <- cleanupGraph' rw (usedBndrs ++ bndrs')
          return $! (bndr,(ty,cleaned)):cleanedOthers
        Nothing -> error $ $(curLoc) ++ "Expr belonging to bndr: " ++ bndrS ++ " not found"
    cleanupGraph' _ [] = return []

checkNonRecursive :: TmName
                  -> [(TmName,(Type,Term))]
                  -> [(TmName,(Type,Term))]
checkNonRecursive topEntity norm =
  let cg = callGraph [] (HashMap.fromList $ map (second snd) norm) topEntity
  in  case recursiveComponents cg of
       []  -> norm
       rcs -> error $ "Callgraph after normalisation contains following recursive cycles: " ++ show rcs
