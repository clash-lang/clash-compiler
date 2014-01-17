{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Utility functions used by the normalisation transformations
module CLaSH.Normalize.Util where

import           Control.Lens            ((%=))
import qualified Control.Lens            as Lens
import qualified Data.Graph              as Graph
import           Data.HashMap.Lazy       (HashMap)
import qualified Data.HashMap.Lazy       as HashMap
import qualified Data.Maybe              as Maybe
import qualified Data.Set                as Set
import           Unbound.LocallyNameless (Fresh)

import           CLaSH.Core.FreeVars     (termFreeIds)
import           CLaSH.Core.Term         (Term (..), TmName)
import           CLaSH.Core.Type         (splitFunForallTy)
import           CLaSH.Core.Util         (collectArgs, termType)
import           CLaSH.Normalize.Types
import           CLaSH.Rewrite.Util      (specialise)

-- | Determine if a function is already inlined in the context of the 'NetlistMonad'
alreadyInlined :: TmName
               -> NormalizeMonad (Maybe Int)
alreadyInlined f = do
  cf <- Lens.use curFun
  inlinedHM <- Lens.use inlineHistory
  limit     <- Lens.use inlineLimit
  case HashMap.lookup cf inlinedHM of
    Nothing       -> return Nothing
    Just inlined' -> case HashMap.lookup f inlined'
                      of Just n
                           | n < limit -> return Nothing
                           | otherwise -> return (Just n)
                         Nothing -> return Nothing

addNewInline :: TmName
             -> NormalizeMonad ()
addNewInline f = do
  cf <- Lens.use curFun
  inlineHistory %= HashMap.insertWith
                     (\_ hm -> HashMap.insertWith (+) f 1 hm)
                     cf
                     (HashMap.singleton f 1)

-- | Specialize under the Normalization Monad
specializeNorm :: NormRewrite
specializeNorm = specialise specialisationCache specialisationHistory specialisationLimit

-- | Determine if a term is closed
isClosed :: (Functor m, Fresh m)
         => Term
         -> m Bool
isClosed = fmap (not . isPolyFunTy) . termType
  where
    -- Is a type a (polymorphic) function type?
    isPolyFunTy = not . null . fst . splitFunForallTy

-- | Determine if a term represents a constant
isConstant :: Term -> Bool
isConstant e = case collectArgs e of
  (Data _, args)   -> all (either isConstant (const True)) args
  (Prim _ _, args) -> all (either isConstant (const True)) args
  (Literal _,_)    -> True
  _                -> False

-- | Create a call graph for a set of global binders, given a root
callGraph :: [TmName] -- ^ List of functions that should not be inspected
          -> HashMap TmName Term -- ^ Global binders
          -> TmName -- ^ Root of the call graph
          -> [(TmName,[TmName])]
callGraph visited bindingMap root = node:other
  where
    rootTm = Maybe.fromMaybe (error $ show root ++ " is not a global binder") $ HashMap.lookup root bindingMap
    used   = Set.toList $ termFreeIds rootTm
    node   = (root,used)
    other  = concatMap (callGraph (root:visited) bindingMap) (filter (`notElem` visited) used)

-- | Determine the sets of recursive components given the edges of a callgraph
recursiveComponents :: [(TmName,[TmName])] -- ^ [(calling function,[called function])]
                    -> [[TmName]]
recursiveComponents = Maybe.catMaybes
                    . map (\case {Graph.CyclicSCC vs -> Just vs; _ -> Nothing})
                    . Graph.stronglyConnComp
                    . map (\(n,es) -> (n,n,es))
