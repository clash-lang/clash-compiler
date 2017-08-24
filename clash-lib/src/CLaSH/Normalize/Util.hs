{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utility functions used by the normalisation transformations
-}

{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module CLaSH.Normalize.Util where

import           Control.Lens            ((&),(+~),(%=),(^.),_5)
import qualified Control.Lens            as Lens
import           Data.HashMap.Lazy       (HashMap)
import qualified Data.HashMap.Lazy       as HashMap
import qualified Data.List               as List
import qualified Data.Set                as Set
import qualified Data.Set.Lens           as Lens
import           Unbound.Generics.LocallyNameless        (Fresh, unembed ,unrec)
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           CLaSH.Core.FreeVars     (termFreeIds)
import           CLaSH.Core.Term         (Term (..), TmOccName)
import           CLaSH.Core.TyCon        (TyCon, TyConOccName)
import           CLaSH.Core.Util
  (collectArgs, isClockOrReset, isPolyFun, termType)
import           CLaSH.Driver.Types      (BindingMap)
import           CLaSH.Normalize.Types
import           CLaSH.Rewrite.Types     (bindings,extra,tcCache)
import           CLaSH.Rewrite.Util      (specialise)

-- | Determine if a function is already inlined in the context of the 'NetlistMonad'
alreadyInlined
  :: TmOccName
  -- ^ Function we want to inline
  -> TmOccName
  -- ^ Function in which we want to perform the inlining
  -> NormalizeMonad (Maybe Int)
alreadyInlined f cf = do
  inlinedHM <- Lens.use inlineHistory
  case HashMap.lookup cf inlinedHM of
    Nothing       -> return Nothing
    Just inlined' -> return (HashMap.lookup f inlined')

addNewInline
  :: TmOccName
  -- ^ Function we want to inline
  -> TmOccName
  -- ^ Function in which we want to perform the inlining
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
         => HashMap TyConOccName TyCon
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

-- | Assert whether a name is a reference to a recursive binder.
isRecursiveBndr
  :: TmOccName
  -> NormalizeSession Bool
isRecursiveBndr f = do
  cg <- Lens.use (extra.recursiveComponents)
  case HashMap.lookup f cg of
    Just isR -> return isR
    Nothing -> do
      fBodyM <- HashMap.lookup f <$> Lens.use bindings
      case fBodyM of
        Nothing -> return False
        Just (_,_,_,_,fBody) -> do
          -- There are no global mutually-recursive functions, only self-recursive
          -- ones, so checking whether 'f' is part of the free variables of the
          -- body of 'f' is sufficient.
          let used = Lens.toListOf termFreeIds fBody
              isR  = f `elem` used
          (extra.recursiveComponents) %= HashMap.insert f isR
          return isR

-- | Create a call graph for a set of global binders, given a root
callGraph
  :: [TmOccName]
  -- ^ List of functions that should not be inspected
  -> BindingMap
  -- ^ Global binders
  -> TmOccName
  -- ^ Root of the call graph
  -> [(TmOccName,[TmOccName])]
callGraph visited bindingMap root
  | Just rootTm <- HashMap.lookup root bindingMap
  = let  used   = Set.toList $ Lens.setOf termFreeIds (rootTm ^. _5)
         node   = (root,used)
         other  = concatMap (callGraph (root:visited) bindingMap) (filter (`notElem` visited) used)
    in   node : other
callGraph _ _ _ = []

-- | Give a "performance/size" classification of a function in normal form.
classifyFunction
  :: Term
  -> TermClassification
classifyFunction = go (TermClassification 0 0 0)
  where
    go !c (Lam b)    = let (_,e) = unsafeUnbind b in go c e
    go !c (TyLam b)  = let (_,e) = unsafeUnbind b in go c e
    go !c (Letrec b) =
      let (bndsR,_) = unsafeUnbind b
          es        = map (unembed . snd) (unrec bndsR)
      in  List.foldl' go c es
    go !c e@(App _ _) = case fst (collectArgs e) of
      Prim _ _ -> c & primitive +~ 1
      Var _ _  -> c & function +~ 1
      _ -> c
    go !c (Case _ _ alts) = case alts of
      (_:_:_) -> c & selection  +~ 1
      _ -> c
    go c _ = c

-- | Determine whether a function adds a lot of hardware or not.
--
-- It is considered expensive when it has 2 or more of the following components:
--
-- * functions
-- * primitives
-- * selections (multiplexers)
isCheapFunction
  :: Term
  -> Bool
isCheapFunction tm = case classifyFunction tm of
  TermClassification {..}
    | _function  <= 1 -> _primitive <= 0 && _selection <= 0
    | _primitive <= 1 -> _function  <= 0 && _selection <= 0
    | _selection <= 1 -> _function  <= 0 && _primitive <= 0
    | otherwise       -> False
