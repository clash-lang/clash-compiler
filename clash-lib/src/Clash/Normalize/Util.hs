{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utility functions used by the normalisation transformations
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Normalize.Util
 ( ConstantSpecInfo(..)
 , isConstantArg
 , shouldReduce
 , alreadyInlined
 , addNewInline
 , specializeNorm
 , isRecursiveBndr
 , isClosed
 , callGraph
 , collectCallGraphUniques
 , classifyFunction
 , isCheapFunction
 , isNonRecursiveGlobalVar
 , constantSpecInfo
 , normalizeTopLvlBndr
 , rewriteExpr
 , removedTm
 , mkInlineTick
 , substWithTyEq
 , tvSubstWithTyEq
 )
 where

import           Control.Lens            ((&),(+~),(%=),(.=))
import qualified Control.Lens            as Lens
import           Data.Bifunctor          (bimap)
import           Data.Either             (lefts)
import qualified Data.List               as List
import qualified Data.List.Extra         as List
import qualified Data.Map                as Map
import qualified Data.HashMap.Strict     as HashMapS
import qualified Data.HashSet            as HashSet
import           Data.Text               (Text)
import qualified Data.Text as Text

import           PrelNames               (eqTyConKey)
import           Unique                  (getKey)

import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Core.FreeVars
  (globalIds, hasLocalFreeVars, globalIdOccursIn)
import           Clash.Core.Name         (Name(nameOcc,nameUniq))
import           Clash.Core.Pretty       (showPpr)
import           Clash.Core.Subst
  (deShadowTerm, extendTvSubst, extendTvSubstList, mkSubst, substTm, substTy,
   substId, extendIdSubst)
import           Clash.Core.Term
import           Clash.Core.TermInfo     (isPolyFun, termType)
import           Clash.Core.TyCon        (TyConMap)
import           Clash.Core.Type
  (Type(LitTy, VarTy), LitTy(SymTy), TypeView (..), tyView, undefinedTy,
   splitFunForallTy, splitTyConAppM, mkPolyFunTy)
import           Clash.Core.Util
  (isClockOrReset)
import           Clash.Core.Var          (Id, TyVar, Var (..), isGlobalId)
import           Clash.Core.VarEnv
  (VarEnv, emptyInScopeSet, emptyVarEnv, extendVarEnv, extendVarEnvWith,
   lookupVarEnv, unionVarEnvWith, unitVarEnv, extendInScopeSetList)
import           Clash.Debug             (traceIf)
import           Clash.Driver.Types      (BindingMap, Binding(..), DebugLevel (..))
import {-# SOURCE #-} Clash.Normalize.Strategy (normalization)
import           Clash.Normalize.Types
import           Clash.Primitives.Util   (constantArgs)
import           Clash.Rewrite.Types
  (RewriteMonad, TransformContext(..), bindings, curFun, dbgLevel, extra,
   tcCache)
import           Clash.Rewrite.Util
  (runRewrite, specialise, mkTmBinderFor, mkDerivedName)
import           Clash.Unique
import           Clash.Util              (SrcSpan, makeCachedU)

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
isConstantArg "Clash.Explicit.SimIO.mealyIO" i = pure (i == 2 || i == 3)
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
  :: Context
  -- ^ ..in the current transformcontext
  -> RewriteMonad NormalizeState Bool
shouldReduce = List.anyM isConstantArg'
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

-- | Test whether a given term represents a non-recursive global variable
isNonRecursiveGlobalVar
  :: Term
  -> NormalizeSession Bool
isNonRecursiveGlobalVar (collectArgs -> (Var i, _args)) = do
  let eIsGlobal = isGlobalId i
  eIsRec    <- isRecursiveBndr i
  return (eIsGlobal && not eIsRec)
isNonRecursiveGlobalVar _ = return False

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
        Just b -> do
          -- There are no global mutually-recursive functions, only self-recursive
          -- ones, so checking whether 'f' is part of the free variables of the
          -- body of 'f' is sufficient.
          let isR = f `globalIdOccursIn` bindingTerm b
          (extra.recursiveComponents) %= extendVarEnv f isR
          return isR

data ConstantSpecInfo =
  ConstantSpecInfo
    { csrNewBindings :: [(Id, Term)]
    -- ^ New let-bindings to be created for all the non-constants found
    , csrNewTerm :: !Term
    -- ^ A term where all the non-constant constructs are replaced by variable
    -- references (found in 'csrNewBindings')
    , csrFoundConstant :: !Bool
    -- ^ Whether the algorithm found a constant at all. (If it didn't, it's no
    -- use creating any new let-bindings!)
    } deriving (Show)

-- | Indicate term is fully constant (don't bind anything)
constantCsr :: Term -> ConstantSpecInfo
constantCsr t = ConstantSpecInfo [] t True

-- | Bind given term to a new variable and indicate that it's fully non-constant
bindCsr
  :: TransformContext
  -> Term
  -> RewriteMonad NormalizeState ConstantSpecInfo
bindCsr ctx@(TransformContext is0 _) oldTerm = do
  -- TODO: Seems like the need to put global ids in scope has been made obsolete
  -- TODO: by a recent change in Clash. Investigate whether this is true.
  tcm <- Lens.view tcCache
  newId <- mkTmBinderFor is0 tcm (mkDerivedName ctx "bindCsr") oldTerm
  pure (ConstantSpecInfo
    { csrNewBindings = [(newId, oldTerm)]
    , csrNewTerm = Var newId
    , csrFoundConstant = False
    })

mergeCsrs
  :: TransformContext
  -> [TickInfo]
  -- ^ Ticks to wrap around proposed new term
  -> Term
  -- ^ "Old" term
  -> ([Either Term Type] -> Term)
  -- ^ Proposed new term in case any constants were found
  -> [Either Term Type]
  -- ^ Subterms
  -> RewriteMonad NormalizeState ConstantSpecInfo
mergeCsrs ctx ticks oldTerm proposedTerm subTerms = do
  subCsrs <- snd <$> List.mapAccumLM constantSpecInfoFolder ctx subTerms

  -- If any arguments are constant (and hence can be constant specced), a new
  -- term is created with these constants left in, but variable parts let-bound.
  -- There's one edge case: whenever a term has _no_ arguments. This happens for
  -- constructors without fields, or -depending on their WorkInfo- primitives
  -- without args. We still set 'csrFoundConstant', because we know the newly
  -- proposed term will be fully constant.
  let
    anyArgsOrResultConstant =
      null (lefts subCsrs) || any csrFoundConstant (lefts subCsrs)

  if anyArgsOrResultConstant then
    let newTerm = proposedTerm (bimap csrNewTerm id <$> subCsrs)  in
    pure (ConstantSpecInfo
      { csrNewBindings = concatMap csrNewBindings (lefts subCsrs)
      , csrNewTerm = mkTicks newTerm ticks
      , csrFoundConstant = True
      })
  else do
    -- No constructs were found to be constant, so we might as well refer to the
    -- whole thing with a new let-binding (instead of creating a number of
    -- "smaller" let-bindings)
    bindCsr ctx oldTerm

 where
  constantSpecInfoFolder
    :: TransformContext
    -> Either Term Type
    -> RewriteMonad NormalizeState (TransformContext, Either ConstantSpecInfo Type)
  constantSpecInfoFolder localCtx (Right typ) =
    pure (localCtx, Right typ)
  constantSpecInfoFolder localCtx@(TransformContext is0 tfCtx) (Left term) = do
    specInfo <- constantSpecInfo localCtx term
    let newIds = map fst (csrNewBindings specInfo)
    let is1 = extendInScopeSetList is0 newIds
    pure (TransformContext is1 tfCtx, Left specInfo)


-- | Calculate constant spec info. The goal of this function is to analyze a
-- given term and yield a new term that:
--
--  * Leaves all the constant parts as they were.
--  * Has all _variable_ parts replaced by a newly generated identifier.
--
-- The result structure will additionally contain:
--
--  * Whether the function found any constant parts at all
--  * A list of let-bindings binding the aforementioned identifiers with
--    the term they replaced.
--
-- This can be used in functions wanting to constant specialize over
-- partially constant data structures.
constantSpecInfo
  :: TransformContext
  -> Term
  -> RewriteMonad NormalizeState ConstantSpecInfo
constantSpecInfo ctx e = do
  tcm <- Lens.view tcCache
  -- Don't constant spec clocks or resets, they're either:
  --
  --  * A simple wire (Var), therefore not interesting to spec
  --  * A clock/reset generator, and speccing a generator weirds out HDL simulators.
  --
  -- I believe we can remove this special case in the future by looking at the
  -- primitive's workinfo.
  if isClockOrReset tcm (termType tcm e) then
    case collectArgs e of
      (Prim p, _)
        | primName p == "Clash.Transformations.removedArg" ->
          pure (constantCsr e)
      _ -> bindCsr ctx e
  else
    case collectArgsTicks e of
      (dc@(Data _), args, ticks) ->
        mergeCsrs ctx ticks e (mkApps dc) args

      -- TODO: Work with prim's WorkInfo?
      (prim@(Prim _), args, ticks) -> do
        csr <- mergeCsrs ctx ticks e (mkApps prim) args
        if null (csrNewBindings csr) then
          pure csr
        else
          bindCsr ctx e

      (Lam _ _, _, _ticks) ->
        if hasLocalFreeVars e then
          bindCsr ctx e
        else
          pure (constantCsr e)

      (var@(Var f), args, ticks) -> do
        (curF, _) <- Lens.use curFun
        isNonRecGlobVar <- isNonRecursiveGlobalVar e
        if isNonRecGlobVar && f /= curF then do
          csr <- mergeCsrs ctx ticks e (mkApps var) args
          if null (csrNewBindings csr) then
            pure csr
          else
            bindCsr ctx e
        else
          bindCsr ctx e

      (Literal _,_, _ticks) ->
        pure (constantCsr e)

      _ ->
        bindCsr ctx e

-- | A call graph counts the number of occurrences that a functions 'g' is used
-- in 'f'.
type CallGraph = VarEnv (VarEnv Word)

-- | Collect all binders mentioned in CallGraph into a HashSet
collectCallGraphUniques :: CallGraph -> HashSet.HashSet Unique
collectCallGraphUniques cg = HashSet.fromList (us0 ++ us1)
 where
  us0 = keysUniqMap cg
  us1 = concatMap keysUniqMap (eltsUniqMap cg)

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
      let used = Lens.foldMapByOf globalIds (unionVarEnvWith (+))
                  emptyVarEnv (`unitUniqMap` 1) (bindingTerm rootTm)
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
    go !c (Tick _ e) = go c e
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
  :: Bool
  -> Id
  -> Binding Term
  -> NormalizeSession (Binding Term)
normalizeTopLvlBndr isTop nm (Binding nm' sp inl tm) = makeCachedU nm (extra.normalized) $ do
  tcm <- Lens.view tcCache
  let nmS = showPpr (varName nm)
  -- We deshadow the term because sometimes GHC gives us
  -- code where a local binder has the same unique as a
  -- global binder, sometimes causing the inliner to go
  -- into a loop. Deshadowing freshens all the bindings
  -- to avoid this.
  let tm1 = deShadowTerm emptyInScopeSet tm
      tm2 = if isTop then substWithTyEq tm1 else tm1
  old <- Lens.use curFun
  tm3 <- rewriteExpr ("normalization",normalization) (nmS,tm2) (nm',sp)
  curFun .= old
  let ty' = termType tcm tm3
  return (Binding nm'{varType = ty'} sp inl tm3)

-- | Turn type equality constraints into substitutions and apply them.
--
-- So given:
--
-- > /\dom . \(eq : dom ~ "System") . \(eta : Signal dom Bool) . eta
--
-- we create the substitution [dom := "System"] and apply it to create:
--
-- > \(eq : "System" ~ "System") . \(eta : Signal "System" Bool) . eta
--
-- __NB:__ Users of this function should ensure it's only applied to TopEntities
substWithTyEq
  :: Term
  -> Term
substWithTyEq e0 = go [] False [] e0
 where
  go
    :: [TyVar]
    -> Bool
    -> [Id]
    -> Term
    -> Term
  go tvs changed ids_ (TyLam tv e) = go (tv:tvs) changed ids_ e
  go tvs changed ids_ (Lam v e)
    | TyConApp (nameUniq -> tcUniq) (tvFirst -> Just (tv, ty)) <- tyView (varType v)
    , tcUniq == getKey eqTyConKey
    , tv `elem` tvs
    = let
        subst0 = extendTvSubst (mkSubst emptyInScopeSet) tv ty
        subst1 = extendIdSubst subst0 v (removedTm (varType v))
      in go (tvs List.\\ [tv]) True (substId subst0 v : ids_) (substTm "substWithTyEq e" subst1 e)
    | otherwise = go tvs changed (v:ids_) e
  go tvs True ids_ e =
    let
      e1 = List.foldl' (flip TyLam) e tvs
      e2 = List.foldl' (flip Lam) e1 ids_
    in e2
  go _ False _ _ = e0

-- Type equality (~) is symmetrical, so users could write: (dom ~ System) or (System ~ dom)
tvFirst :: [Type] -> Maybe (TyVar, Type)
tvFirst [_, VarTy tv, ty] = Just (tv, ty)
tvFirst [_, ty, VarTy tv] = Just (tv, ty)
tvFirst _ = Nothing

-- | The type equivalent of 'substWithTyEq'
tvSubstWithTyEq
  :: Type
  -> Type
tvSubstWithTyEq ty0 = go [] args0
 where
  (args0,tyRes) = splitFunForallTy ty0

  go :: [(TyVar,Type)] -> [Either TyVar Type] -> Type
  go eqs (Right arg : args)
    | Just (tc,tcArgs) <- splitTyConAppM arg
    , nameUniq tc == getKey eqTyConKey
    , Just eq <- tvFirst tcArgs
    = go (eq:eqs) args
    | otherwise = go eqs args
  go eqs (Left _tv : args)
    = go eqs args -- drop (ForAll) tv
  go []  [] = ty0 -- no eq constraints, returning original type
  go eqs [] = substTy subst ty2
   where
     subst = extendTvSubstList (mkSubst emptyInScopeSet) eqs
     args2 = args0 List.\\ (map (Left . fst) eqs)
     ty2 = mkPolyFunTy tyRes args2

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

removedTm
  :: Type
  -> Term
removedTm =
  TyApp (Prim (PrimInfo "Clash.Transformations.removedArg" undefinedTy WorkNever Nothing))

-- | A tick to prefix an inlined expression with it's original name.
-- For example, given
--
--     foo = bar  -- ...
--     bar = baz  -- ...
--     baz = quuz -- ...
--
-- if bar is inlined into foo, then the name of the component should contain
-- the name of the inlined component. This tick ensures that the component in
-- foo is called bar_baz instead of just baz.
--
mkInlineTick :: Id -> TickInfo
mkInlineTick n = NameMod PrefixName (LitTy . SymTy $ toStr n)
 where
  toStr = Text.unpack . snd . Text.breakOnEnd "." . nameOcc . varName
