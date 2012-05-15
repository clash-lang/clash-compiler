module CLaSH.Normalize where

import Control.Concurrent.Supply        (Supply)
import qualified Control.Monad.State as State
import Data.HashMap.Lazy                (HashMap)
import qualified Data.HashMap.Lazy   as HashMap
import qualified Data.Label.PureM    as LabelM
import qualified Data.Map            as Map

import CLaSH.Core.FreeVars      (termFreeIds)
import CLaSH.Core.Pretty        (showDoc)
import CLaSH.Core.Term          (TmName,Term)
import CLaSH.Core.Type          (Type)
import CLaSH.Normalize.Strategy
import CLaSH.Normalize.Types
import CLaSH.Rewrite.Types      (DebugLevel(..),RewriteState(..),dbgLevel,
  bindings)
import CLaSH.Rewrite.Util       (runRewrite,runRewriteSession)
import CLaSH.Util

runNormalization ::
  DebugLevel
  -> Supply
  -> HashMap TmName (Type,Term)
  -> NormalizeSession a
  -> a
runNormalization lvl supply globals
  = flip State.evalState normState
  . runRewriteSession lvl rwState
  where
    rwState   = RewriteState 0 globals supply
    normState = NormalizeState HashMap.empty Map.empty

normalize ::
  [TmName]
  -> NormalizeSession [(TmName,(Type,Term))]
normalize (bndr:bndrs) = do
  let bndrS = showDoc HashMap.empty bndr
  exprM <- fmap (HashMap.lookup bndr) $ LabelM.gets bindings
  case exprM of
    Just (ty,expr) -> do
      normalizedExpr <- makeCachedT3 bndr normalized $
                         normalizeExpr bndrS expr
      let usedBndrs = termFreeIds normalizedExpr
      normalizedOthers <- normalize (usedBndrs ++ bndrs)
      return ((bndr,(ty,normalizedExpr)):normalizedOthers)
    Nothing -> error $ $(curLoc) ++ "Expr belonging to bndr: " ++ bndrS ++ " not found"

normalize [] = return []

normalizeExpr ::
  String
  -> Term
  -> NormalizeSession Term
normalizeExpr bndrS expr = do
  let emptyHM = HashMap.empty
  lvl <- LabelM.asks dbgLevel
  let before = showDoc emptyHM expr
  let expr' = traceIf (lvl >= DebugFinal)
                (bndrS ++ " before normalization:\n\n" ++ before ++ "\n")
                expr
  rewritten <- runRewrite "monomorphization" normalization expr'
  let after = showDoc emptyHM rewritten
  traceIf (lvl >= DebugFinal)
    (bndrS ++ " after normalization:\n\n" ++ after ++ "\n") $
    return rewritten
