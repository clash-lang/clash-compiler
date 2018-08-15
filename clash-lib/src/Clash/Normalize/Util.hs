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

module Clash.Normalize.Util where

import           Control.Lens            ((&),(+~),(%=),(^.),_4)
import qualified Control.Lens            as Lens
import qualified Data.List               as List

import           Clash.Core.FreeVars     (idOccursIn, termFreeIds)
import           Clash.Core.Term         (Term (..))
import           Clash.Core.TyCon        (TyConMap)
import           Clash.Core.Var          (Id, Var (..))
import           Clash.Core.VarEnv
import           Clash.Core.Util
  (collectArgs, isClockOrReset, isPolyFun, termType)
import           Clash.Driver.Types      (BindingMap)
import           Clash.Normalize.Types
import           Clash.Rewrite.Types     (bindings,extra,tcCache)
import           Clash.Rewrite.Util      (specialise)
import           Clash.Unique

-- | Determine if a function is already inlined in the context of the 'NetlistMonad'
alreadyInlined
  :: Id
  -- ^ Function we want to inline
  -> Id
  -- ^ Function in which we want to perform the inlining
  -> NormalizeMonad (Maybe Int)
alreadyInlined f cf = do
  inlinedHM <- Lens.use inlineHistory
  case lookupVarEnv cf inlinedHM of
    Nothing       -> return Nothing
    Just inlined' -> return (lookupVarEnv f inlined')

addNewInline
  :: Id
  -- ^ Function we want to inline
  -> Id
  -- ^ Function in which we want to perform the inlining
  -> NormalizeMonad ()
addNewInline f cf =
  inlineHistory %= extendVarEnvWith
                     cf
                     (unitVarEnv f 1)
                     (\_ hm -> extendVarEnvWith f 1 (+) hm)

-- | Specialize under the Normalization Monad
specializeNorm :: NormRewrite
specializeNorm = specialise specialisationCache specialisationHistory specialisationLimit

-- | Determine if a term is closed
isClosed :: TyConMap
         -> Term
         -> Bool
isClosed tcm = not . isPolyFun tcm

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
  let eTy = termType tcm e
  if isClockOrReset tcm eTy
     then return False
     else return (isConstant e)

-- | Assert whether a name is a reference to a recursive binder.
isRecursiveBndr
  :: Id
  -> NormalizeSession Bool
isRecursiveBndr f = do
  cg <- Lens.use (extra.recursiveComponents)
  case lookupVarEnv f cg of
    Just isR -> return isR
    Nothing -> do
      fBodyM <- lookupVarEnv f <$> Lens.use bindings
      case fBodyM of
        Nothing -> return False
        Just (_,_,_,fBody) -> do
          -- There are no global mutually-recursive functions, only self-recursive
          -- ones, so checking whether 'f' is part of the free variables of the
          -- body of 'f' is sufficient.
          let isR = f `idOccursIn` fBody
          (extra.recursiveComponents) %= extendVarEnv f isR
          return isR

-- | A call graph counts the number of occurrences that a functions 'g' is used
-- in 'f'.
type CallGraph = VarEnv (VarEnv Word)

-- | Create a call graph for a set of global binders, given a root
callGraph
  :: BindingMap
  -> Id
  -> CallGraph
callGraph bndrs rt = go emptyVarEnv (varUniq rt)
  where
    go cg root
      | Nothing     <- lookupUniqMap root cg
      , Just rootTm <- lookupUniqMap root bndrs =
      let used = Lens.foldMapByOf termFreeIds (unionVarEnvWith (+))
                  emptyVarEnv (`unitUniqMap` 1) (rootTm ^. _4)
          cg'  = extendUniqMap root used cg
      in  List.foldl' go cg' (keysUniqMap used)
    go cg _ = cg

-- | Give a "performance/size" classification of a function in normal form.
classifyFunction
  :: Term
  -> TermClassification
classifyFunction = go (TermClassification 0 0 0)
  where
    go !c (Lam _ e)     = go c e
    go !c (TyLam _ e)   = go c e
    go !c (Letrec bs _) = List.foldl' go c (map snd bs)
    go !c e@(App {}) = case fst (collectArgs e) of
      Prim {} -> c & primitive +~ 1
      Var {}  -> c & function +~ 1
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
