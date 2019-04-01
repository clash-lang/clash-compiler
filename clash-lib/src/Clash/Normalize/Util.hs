{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utility functions used by the normalisation transformations
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Normalize.Util
 ( isConstantArg
 , shouldReduce
 , alreadyInlined
 , addNewInline
 , specializeNorm
 , isRecursiveBndr
 , isClosed
 , callGraph
 , classifyFunction
 , isCheapFunction
 , isNonRecursiveGlobalVar
 , canConstantSpec
 , normalizeTopLvlBndr
 , rewriteExpr
 )
 where

import           Control.Lens            ((&),(+~),(%=),(^.),_4,(.=))
import qualified Control.Lens            as Lens
import qualified Data.List               as List
import qualified Data.Map                as Map
import qualified Data.HashMap.Strict     as HashMapS
import           Data.Text               (Text)

import           BasicTypes              (InlineSpec)

import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Core.FreeVars     (idOccursIn, termFreeIds)
import           Clash.Core.Pretty       (showPpr)
import           Clash.Core.Subst        (deShadowTerm)
import           Clash.Core.Term         (Term (..))
import           Clash.Core.TyCon        (TyConMap)
import           Clash.Core.Var          (Id, Var (..))
import           Clash.Core.VarEnv
import           Clash.Core.Util
  (collectArgs, isPolyFun, termType, isClockOrReset)
import           Clash.Driver.Types      (BindingMap, DebugLevel (..))
import {-# SOURCE #-} Clash.Normalize.Strategy (normalization)
import           Clash.Normalize.Types
import           Clash.Primitives.Util   (constantArgs)
import           Clash.Rewrite.Types
  (bindings,extra,RewriteMonad,CoreContext(AppArg), tcCache, curFun,
   globalInScope, dbgLevel)
import           Clash.Rewrite.Util
  (specialise, hasLocalFreeVars, runRewrite)
import           Clash.Unique
import           Clash.Util              (SrcSpan, anyM, makeCachedU, traceIf)

-- | Determine if argument should reduce to a constant given a primitive and
-- an argument number. Caches results.
isConstantArg
  :: Text
  -- ^ Primitive name
  -> Int
  -- ^ Argument number
  -> RewriteMonad NormalizeState Bool
  -- ^ Yields @DontCare@ for if given primitive name is not found, if the
  -- argument does not exist, or if the argument was not mentioned by the
  -- blackbox.
isConstantArg nm i = do
  argMap <- Lens.use (extra.primitiveArgs)
  case Map.lookup nm argMap of
    Nothing -> do
      -- Constant args not yet calculated, or primitive does not exist
      prims <- Lens.use (extra.primitives)
      case extractPrim =<< HashMapS.lookup nm prims of
        Nothing ->
          -- Primitive does not exist:
          pure False
        Just p -> do
          -- Calculate constant arguments:
          let m = constantArgs nm p
          (extra.primitiveArgs) Lens.%= Map.insert nm m
          pure (i `elem` m)
    Just m ->
      -- Cached version found
      pure (i `elem` m)

-- | Given a list of transformation contexts, determine if any of the contexts
-- indicates that the current arg is to be reduced to a constant / literal.
shouldReduce
  :: [CoreContext]
  -- ^ ..in the current transformcontext
  -> RewriteMonad NormalizeState Bool
shouldReduce = anyM isConstantArg'
  where
    isConstantArg' (AppArg (Just (nm, _, i))) = isConstantArg nm i
    isConstantArg' _ = pure False

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

-- | Test whether binder is a non-shadowed global variable
isGlobalBndr
  :: InScopeSet
  -- ^ Local scope
  -> Id
  -- ^ Term to check for global variness
  -> RewriteMonad extra Bool
isGlobalBndr localScope x = do
  bndrs <- Lens.use bindings
  let inLocalScope = x `elemInScopeSet` localScope
  let inGlobalScope = elemUniqMap (varName x) bndrs
  return (not inLocalScope && inGlobalScope)

-- | Test whether a given term represents a non-recursive global variable
isNonRecursiveGlobalVar
  :: InScopeSet
  -> Term
  -> NormalizeSession Bool
isNonRecursiveGlobalVar is (collectArgs -> (Var i, _args)) = do
  eIsGlobal <- isGlobalBndr is i
  eIsRec    <- isRecursiveBndr i
  return (eIsGlobal && not eIsRec)
isNonRecursiveGlobalVar _ _ = return False

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

-- | Test if we can constant specialize current term in current function. The
-- rules are, we can constant fold if:
--
--   * Term does not carry a clock or reset
--   * Term is constant is @isConstant@ sense, and additionally when term is a
--     global, non-recursive variable
--
canConstantSpec
  :: InScopeSet
  -> Term
  -> RewriteMonad NormalizeState Bool
canConstantSpec is0 e = do
  tcm <- Lens.view tcCache
  if isClockOrReset tcm (termType tcm e) then
    case collectArgs e of
      (Prim nm _, _) -> return (nm == "Clash.Transformations.removedArg")
      _              -> return False
  else
    case collectArgs e of
      (Data _, args)   -> and <$> mapM (either (canConstantSpec is0) (const (pure True))) args
      (Prim _ _, args) -> and <$> mapM (either (canConstantSpec is0) (const (pure True))) args
      (Lam _ _, _)     -> not <$> (hasLocalFreeVars <*> pure e)
      (Var f, args)    -> do
        (curF, _) <- Lens.use curFun

        argsConst <- and <$> mapM (either (canConstantSpec is0) (const (pure True))) args
        isNonRecGlobVar <- isNonRecursiveGlobalVar is0 e
        return (argsConst && isNonRecGlobVar && f /= curF)

      (Literal _,_)    -> pure True
      _                -> pure False

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

normalizeTopLvlBndr
  :: Id
  -> (Id, SrcSpan, InlineSpec, Term)
  -> NormalizeSession (Id, SrcSpan, InlineSpec, Term)
normalizeTopLvlBndr nm (nm',sp,inl,tm) = makeCachedU nm (extra.normalized) $ do
  tcm <- Lens.view tcCache
  let nmS = showPpr (varName nm)
  -- We deshadow the term because sometimes GHC gives us
  -- code where a local binder has the same unique as a
  -- global binder, sometimes causing the inliner to go
  -- into a loop. Deshadowing freshens all the bindings
  -- to avoid this.
  --
  -- Additionally, it allows for a much cheaper `appProp`
  -- transformation, see Note [AppProp no-shadow invariant]
  is0 <- Lens.use globalInScope
  let tm1 = deShadowTerm is0 tm
  old <- Lens.use curFun
  tm2 <- rewriteExpr ("normalization",normalization) (nmS,tm1) (nm',sp)
  curFun .= old
  let ty' = termType tcm tm2
  return (nm' {varType = ty'},sp,inl,tm2)

-- | Rewrite a term according to the provided transformation
rewriteExpr :: (String,NormRewrite) -- ^ Transformation to apply
            -> (String,Term)        -- ^ Term to transform
            -> (Id, SrcSpan)        -- ^ Renew current function being rewritten
            -> NormalizeSession Term
rewriteExpr (nrwS,nrw) (bndrS,expr) (nm, sp) = do
  curFun .= (nm, sp)
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
