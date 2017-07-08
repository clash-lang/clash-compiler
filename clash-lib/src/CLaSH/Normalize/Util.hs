{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utility functions used by the normalisation transformations
-}

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module CLaSH.Normalize.Util where

import           Control.Lens            ((%=),(^.),_3)
import qualified Control.Lens            as Lens
import           Data.Function           (on)
import qualified Data.Graph              as Graph
import           Data.HashMap.Lazy       (HashMap)
import qualified Data.HashMap.Lazy       as HashMap
import qualified Data.List               as List
import qualified Data.Maybe              as Maybe
import qualified Data.Set                as Set
import qualified Data.Set.Lens           as Lens
import           Unbound.Generics.LocallyNameless (Fresh, bind, embed, rec)

import           SrcLoc                  (SrcSpan)

import           CLaSH.Core.FreeVars     (termFreeIds)
import           CLaSH.Core.Var          (Var (Id))
import           CLaSH.Core.Term         (Term (..), TmName)
import           CLaSH.Core.Type         (Type)
import           CLaSH.Core.TyCon        (TyCon, TyConName)
import           CLaSH.Core.Util
  (collectArgs, isClockOrReset, isPolyFun, termType)
import           CLaSH.Normalize.Types
import           CLaSH.Rewrite.Types     (bindings,extra,tcCache)
import           CLaSH.Rewrite.Util      (specialise)

-- | Determine if a function is already inlined in the context of the 'NetlistMonad'
alreadyInlined :: TmName -- ^ Function we want to inline
               -> TmName -- ^ Function in which we want to perform the inlining
               -> NormalizeMonad (Maybe Int)
alreadyInlined f cf = do
  inlinedHM <- Lens.use inlineHistory
  case HashMap.lookup cf inlinedHM of
    Nothing       -> return Nothing
    Just inlined' -> return (HashMap.lookup f inlined')

addNewInline :: TmName -- ^ Function we want to inline
             -> TmName -- ^ Function in which we want to perform the inlining
             -> NormalizeMonad ()
addNewInline f cf =
  inlineHistory %= HashMap.insertWith
                     (\_ hm -> HashMap.insertWith (+) f 1 hm)
                     cf
                     (HashMap.singleton f 1)

-- | Specialize under the Normalization Monad
specializeNorm :: NormRewrite
specializeNorm = specialise specialisationCache specialisationHistory specialisationLimit

-- | Determine if a term is closed
isClosed :: Fresh m
         => HashMap TyConName TyCon
         -> Term
         -> m Bool
isClosed tcm = fmap not . isPolyFun tcm

-- | Determine if a term represents a constant
isConstant :: Term -> Bool
isConstant e = case collectArgs e of
  (Data _, args)   -> all (either isConstant (const True)) args
  (Prim _ _, args) -> all (either isConstant (const True)) args
  (Literal _,_)    -> True
  _                -> False

isConstantNotClockReset :: Term -> NormalizeSession Bool
isConstantNotClockReset e = do
  tcm <- Lens.view tcCache
  eTy <- termType tcm e
  if isClockOrReset tcm eTy
     then return False
     else return (isConstant e)

isRecursiveBndr :: TmName -> NormalizeSession Bool
isRecursiveBndr f = do
  cg <- Lens.use (extra.recursiveComponents)
  case HashMap.lookup f cg of
    Just isR -> return isR
    Nothing -> do
      bndrs <- Lens.use bindings
      let cg'  = callGraph [] bndrs f
          rcs  = concat $ mkRecursiveComponents cg'
          isR  = f `elem` rcs
          cg'' = HashMap.fromList
               $ map (\(t,_) -> (t,t `elem` rcs)) cg'
      (extra.recursiveComponents) %= HashMap.union cg''
      return isR

-- | Create a call graph for a set of global binders, given a root
callGraph :: [TmName] -- ^ List of functions that should not be inspected
          -> HashMap TmName (Type,SrcSpan,Term) -- ^ Global binders
          -> TmName -- ^ Root of the call graph
          -> [(TmName,[TmName])]
callGraph visited bindingMap root
  | Just rootTm <- HashMap.lookup root bindingMap
  = let  used   = Set.toList $ Lens.setOf termFreeIds (rootTm ^. _3)
         node   = (root,used)
         other  = concatMap (callGraph (root:visited) bindingMap) (filter (`notElem` visited) used)
    in   node : other
callGraph _ _ _ = []

-- | Determine the sets of recursive components given the edges of a callgraph
mkRecursiveComponents :: [(TmName,[TmName])] -- ^ [(calling function,[called function])]
                    -> [[TmName]]
mkRecursiveComponents cg = map (List.sortBy (compare `on` (`List.elemIndex` fs)))
                         . Maybe.catMaybes
                         . map (\case {Graph.CyclicSCC vs -> Just vs; _ -> Nothing})
                         . Graph.stronglyConnComp
                         $ map (\(n,es) -> (n,n,es)) cg
  where
    fs = map fst cg

-- | Let-binds mutually recursive functions at their call-sites. That is,
-- given a group of recursive top-level binders @rc@, and a function @f@ where
-- @any (`elem` FreeVars(f)) rc@, replace the body of @f@, @fBody@, by
-- @letrec rc in fBody@.
--
-- This transformation is performed to, where possible, transform global
-- mutually recursive bindings into: local let-recursive non-function-type
-- binders. Where the let-recursive non-function-type binders can be turned
-- into feedback loops.
--
-- We need this transformation because GHC has a transformation which does the
-- exact opposite. This is a problem because global (mutual) recursive functions
-- describe an infinite structure when viewed with a structural lens, which
-- cannot be realised as a circuit.
lambdaDrop :: HashMap TmName (Type,SrcSpan,Term)
           -> TmName
           -> HashMap TmName (Type,SrcSpan,Term)
lambdaDrop bndrs topEntity = bndrs''
  where
    depGraph = callGraph [] bndrs topEntity
    -- We only care about mutually recursive bindings, there is no point
    -- lambda-dropping self-recursive bindings as they would be lifted again
    -- in a later stage.
    rcs     = filter ((>1).length) (mkRecursiveComponents depGraph)
    -- Add the bodies to the recursive components.
    rcsTms  = zipWith (,) rcs (map (map (bndrs HashMap.!)) rcs)
    -- Add the recursive binders at call-sites
    bndrs'  = HashMap.map addRC bndrs
    -- Delete any of the remaining mutually recursive top-level binders, they
    -- should no longer be needed because they are all let-bound at their
    -- call-sites.
    --
    -- Also, if there's a bug in lambdaDrop, Clash will complain loudly because
    -- it will look for a function which has been deleted.
    --
    -- Bug 1 discovered and fixed: topEntity itself can be part of a recursive
    -- group, but it should not be deleted.
    bndrs'' = List.foldl' (flip HashMap.delete) bndrs'
                          (filter (/= topEntity) (concat rcs))

    addRC (ty,sp,tm) =
      let fv      = Lens.toListOf termFreeIds tm
          -- Only interested in the recursive components which are used in this
          -- function
          rcsTms' = filter (any (`elem` fv) . fst) rcsTms
          -- We create a single list of all the recursive bindings
          bnds    = map mkBind (concat (map (uncurry zip) rcsTms'))
          newTm   = Letrec (bind (rec bnds) tm)
      in  case bnds of
            [] -> (ty,sp,tm)
            _  -> (ty,sp,newTm)

    mkBind (nm,(ty,_,tm)) = (Id nm (embed ty),embed tm)
