{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utility functions of the normalization transformations on scoped Clash Core
  ("Clash.Core.Scoped.Syntax"): the ported counterparts of
  "Clash.Normalize.Util".

  = Globals

  Tables of the normalizer that are keyed by global binders (the recursive
  components and the inline history) are looked up by 'GlobalId', so that a
  local variable can never hit an entry of a global with the same unique.
  The tables themselves are still those of Clash Core, in 'NormalizeState'.

  = The boundary with Clash Core

  During the port, the global bindings ('Clash.Rewrite.Types.bindings') are
  those of Clash Core. Phase 4 of the port, which runs the normalizer on
  scoped Core, changes that.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Normalize.Scoped.Util
  ( -- * Constant arguments of primitives
    isConstantArg
  , shouldReduce
    -- * Inlining
  , alreadyInlined
  , addNewInline
  , mkInlineTick
    -- * Recursive globals
  , isRecursiveBndr
  , isNonRecursiveGlobalVar
    -- * Call graphs
  , CallGraph
  , callGraph
  , callGraphGlobals
    -- * Classifying functions
  , classifyFunction
  , isCheapFunction
  ) where

import           Control.Lens                ((&), (+~), (%=))
import qualified Control.Lens                as Lens
import           Control.Monad.Extra         (anyM)
import qualified Data.List                   as List
import qualified Data.Text                   as Text

import qualified Clash.Core.FreeVars         as C (globalIdOccursIn)
import qualified Clash.Core.Name             as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Var              as C
import           Clash.Core.VarEnv
  (extendVarEnv, extendVarEnvWith, lookupVarEnv, unitVarEnv)
import           Clash.Driver.Types          (Binding (..))
import           Clash.Normalize.Types
import           Clash.Normalize.Util        (isConstantArg)
import           Clash.Rewrite.Types         (bindings, extra)

import           Clash.Core.Scoped.NameMap
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Term
import           Clash.Rewrite.Scoped.Types  (FrameKind (..), TransformContext, ctxFrames)

-- * Constant arguments of primitives

-- | Whether the context of a subterm indicates that it is to be reduced to a
-- constant: whether it is an argument of a primitive that wants that argument
-- to be constant (see 'isConstantArg'). See 'Clash.Normalize.Util.shouldReduce'.
shouldReduce :: TransformContext n -> NormalizeSession Bool
shouldReduce = anyM isConstantArg' . ctxFrames
 where
  isConstantArg' (KAppArg (Just (nm, _, i))) = isConstantArg nm i
  isConstantArg' _ = pure False

-- * Inlining

-- | How often a global was inlined into another one. See
-- 'Clash.Normalize.Util.alreadyInlined'.
alreadyInlined
  :: GlobalId
  -- ^ Function we want to inline
  -> GlobalId
  -- ^ Function in which we want to perform the inlining
  -> NormalizeMonad (Maybe Int)
alreadyInlined f cf = do
  inlinedHM <- Lens.use inlineHistory
  pure (lookupVarEnv (globalIdVar f) =<< lookupVarEnv (globalIdVar cf) inlinedHM)

-- | Record a new inlining in the 'inlineHistory'. See
-- 'Clash.Normalize.Util.addNewInline'.
addNewInline
  :: GlobalId
  -- ^ Function we're inlining
  -> GlobalId
  -- ^ Function in which we're inlining it
  -> NormalizeMonad ()
addNewInline (globalIdVar -> f) (globalIdVar -> cf) =
  inlineHistory %= extendVarEnvWith
                     cf
                     (unitVarEnv f 1)
                     (\_ hm -> extendVarEnvWith f 1 (+) hm)

-- | A tick to prefix an inlined expression with the name of the global it
-- came from, see 'Clash.Normalize.Util.mkInlineTick'
mkInlineTick :: GlobalId -> TickInfo n
mkInlineTick n = NameMod C.PrefixName (LitTy (C.SymTy (toStr n)))
 where
  toStr = Text.unpack . snd . Text.breakOnEnd "." . C.nameOcc . C.varName . globalIdVar

-- * Recursive globals

-- | Whether a term is an application of a non-recursive global. See
-- 'Clash.Normalize.Util.isNonRecursiveGlobalVar'. A local variable is not a
-- global: unlike in Clash Core, they can't be confused.
isNonRecursiveGlobalVar :: Term n -> NormalizeSession Bool
isNonRecursiveGlobalVar (collectArgs -> (GlobalVar g, _)) = not <$> isRecursiveBndr g
isNonRecursiveGlobalVar _ = pure False

-- | Whether a global binder is recursive. Caches its answer in
-- 'recursiveComponents'. See 'Clash.Normalize.Util.isRecursiveBndr'.
isRecursiveBndr :: GlobalId -> NormalizeSession Bool
isRecursiveBndr g = do
  cg <- Lens.use (extra.recursiveComponents)
  case lookupVarEnv f cg of
    Just isR -> pure isR
    Nothing -> do
      fBodyM <- lookupVarEnv f <$> Lens.use bindings
      case fBodyM of
        Nothing -> pure False
        Just b -> do
          -- There are no global mutually-recursive functions, only
          -- self-recursive ones, so checking whether 'f' occurs in its body is
          -- sufficient. The bindings are still those of Clash Core, see the
          -- module header.
          let isR = f `C.globalIdOccursIn` bindingTerm b
          (extra.recursiveComponents) %= extendVarEnv f isR
          pure isR
 where
  f = globalIdVar g

-- * Call graphs

-- | A call graph counts the number of occurrences of every global @g@ in the
-- body of a global @f@. See 'Clash.Normalize.Util.CallGraph'.
type CallGraph = GlobalEnv (GlobalEnv Word)

-- | All globals mentioned in a call graph
callGraphGlobals :: CallGraph -> GlobalSet
callGraphGlobals cg =
  unionsGlobalSet (globalEnvKeysSet cg : map globalEnvKeysSet (globalEnvElems cg))

-- | The call graph of the globals reachable from a root. See
-- 'Clash.Normalize.Util.callGraph'.
callGraph :: GlobalEnv (Binding (Term 'VoidS)) -> GlobalId -> CallGraph
callGraph bndrs = go emptyGlobalEnv
 where
  go cg root
    | not (memberGlobalEnv root cg)
    , Just rootTm <- lookupGlobalEnv root bndrs =
    let used = globalOccurrences (bindingTerm rootTm)
        cg' = insertGlobalEnv root used cg
    in  List.foldl' go cg' (globalEnvKeys used)
  go cg _ = cg

-- | The number of occurrences of every global in a term, including in the
-- terms of ticks, like 'Clash.Core.FreeVars.globalIds'
globalOccurrences :: Term n -> GlobalEnv Word
globalOccurrences = go emptyGlobalEnv
 where
  go :: GlobalEnv Word -> Term x -> GlobalEnv Word
  go !acc = \case
    Var {} -> acc
    GlobalVar g -> insertWithGlobalEnv (+) g 1 acc
    Data {} -> acc
    Literal {} -> acc
    Prim {} -> acc
    Lam _ e -> go acc e
    TyLam _ e -> go acc e
    App e1 e2 -> go (go acc e1) e2
    TyApp e _ -> go acc e
    Let (NonRec _ x) e -> go (go acc x) e
    Let (Rec _ xs) e -> List.foldl' go (go acc e) xs
    Case e _ alts -> List.foldl' (\a (Alt _ e') -> go a e') (go acc e) alts
    Cast e _ _ -> go acc e
    Tick (Attributes _ e') e -> go (go acc e') e
    Tick _ e -> go acc e

-- * Classifying functions

-- | Give a "performance/size" classification of a function in normal form.
-- See 'Clash.Normalize.Util.classifyFunction'.
classifyFunction :: Term n -> TermClassification
classifyFunction = go (TermClassification 0 0 0)
 where
  go :: TermClassification -> Term x -> TermClassification
  go !c = \case
    Lam _ e -> go c e
    TyLam _ e -> go c e
    -- Only the right-hand sides, like 'Clash.Normalize.Util.classifyFunction'
    Let (NonRec _ x) _ -> go c x
    Let (Rec _ xs) _ -> List.foldl' go c xs
    e@App {} -> case fst (collectArgs e) of
      Prim {} -> c & primitive +~ 1
      Var {} -> c & function +~ 1
      GlobalVar {} -> c & function +~ 1
      _ -> c
    Case _ _ alts -> case alts of
      (_:_:_) -> c & selection +~ 1
      _ -> c
    Tick _ e -> go c e
    _ -> c

-- | Whether a function adds little hardware: it has at most one function, at
-- most one primitive or at most one selection (multiplexer), and none of the
-- other two. See 'Clash.Normalize.Util.isCheapFunction'.
isCheapFunction :: Term n -> Bool
isCheapFunction tm = case classifyFunction tm of
  TermClassification {..}
    | _function  <= 1 -> _primitive <= 0 && _selection <= 0
    | _primitive <= 1 -> _function  <= 0 && _selection <= 0
    | _selection <= 1 -> _function  <= 0 && _primitive <= 0
    | otherwise       -> False
