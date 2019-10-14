{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utility functions used by the normalisation transformations
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MagicHash         #-}

module Clash.Normalize.Util
 ( ConstantSpecInfo(..)
 , isConstantArg
 , shouldReduce
 , alreadyInlined
 , addNewInline
 , specializeNorm
 , isRecursiveBndr
 , isClosed
 , isWorkFree
 , callGraph
 , classifyFunction
 , isCheapFunction
 , isNonRecursiveGlobalVar
 , constantSpecInfo
 , normalizeTopLvlBndr
 , rewriteExpr
 , removedTm
 , numLiteral
 )
 where

import           Control.Applicative     (liftA2)
import           Control.Lens            ((&),(+~),(%=),(^.),_4,(.=))
import qualified Control.Lens            as Lens
import           Data.Bifunctor          (bimap)
import           Data.Either             (lefts)
import qualified Data.List               as List
import qualified Data.Map                as Map
import qualified Data.HashMap.Strict     as HashMapS
import           Data.Text               (Text)
import           GHC.Integer.GMP.Internals
  (Integer (..), BigNat (..))
import           GHC.Stack               (HasCallStack)

import           BasicTypes              (InlineSpec)

import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Core.FreeVars
  (globalIds, hasLocalFreeVars, globalIdOccursIn)
import           Clash.Core.DataCon      (dcTag)
import qualified Clash.Core.Literal      as L
import           Clash.Core.Pretty       (showPpr)
import           Clash.Core.Subst        (deShadowTerm)
import           Clash.Core.Term
  (Context, CoreContext(AppArg), PrimInfo (..), Term (..), WorkInfo (..),
   TickInfo, collectArgs, collectArgsTicks)
import           Clash.Core.TyCon        (TyConMap)
import           Clash.Core.Type         (Type, isPolyFunTy, undefinedTy)
import           Clash.Core.Util
  (isClockOrReset, isPolyFun, termType, mkApps, mkTicks)
import           Clash.Core.Var          (Id, Var (..), isGlobalId, isLocalId)
import           Clash.Core.VarEnv
  (VarEnv, emptyInScopeSet, emptyVarEnv, extendVarEnv, extendVarEnvWith,
   lookupVarEnv, unionVarEnvWith, unitVarEnv, extendInScopeSetList)
import           Clash.Driver.Types      (BindingMap, DebugLevel (..))
import {-# SOURCE #-} Clash.Normalize.Strategy (normalization)
import           Clash.Normalize.Types
import           Clash.Primitives.Util   (constantArgs)
import           Clash.Rewrite.Types
  (RewriteMonad, TransformContext(..), bindings, curFun, dbgLevel, extra,
   tcCache)
import qualified Data.Primitive.ByteArray    as BA
import qualified Data.Vector.Primitive       as PV
import           Clash.Rewrite.Util
  (runRewrite, specialise, mkTmBinderFor, mkDerivedName, isConstant)
import           Clash.Unique
import           Clash.Util
  (SrcSpan, allM, anyM, makeCachedU, traceIf, mapAccumLM)

-- | Determines whether a global binder is work free. Errors if binder does
-- not exist.
isWorkFreeBinder :: HasCallStack => Id -> NormalizeSession Bool
isWorkFreeBinder bndr =
  makeCachedU bndr (extra.workFreeComponents) $ do
    bExprM <- lookupVarEnv bndr <$> Lens.use bindings
    case bExprM of
      Nothing ->
        error ("isWorkFreeBinder: couldn't find binder: " ++ showPpr bndr)
      Just (_, _, _, bExpr) ->
        isWorkFree bExpr

numLiteral :: Term -> NormalizeSession (Maybe Integer)
numLiteral v =
  pure (go v)
 where
  go :: Term -> Maybe Integer
  go (Literal (L.IntegerLiteral i)) = Just i
  go (Literal (L.IntLiteral i)) = Just i
  go (Literal (L.NaturalLiteral i)) = Just i
  go (Literal (L.Int64Literal i)) = Just i
  go t =
    case collectArgs t of
      (Data dc, [Left (Literal (L.IntLiteral i))]) | dcTag dc == 1 -> Just i
      (Data dc, [Left (Literal (L.ByteArrayLiteral (PV.Vector _ _ (BA.ByteArray ba))))])
        | dcTag dc == 2 -> Just (Jp# (BN# ba))
        | dcTag dc == 3 -> Just (Jn# (BN# ba))
      _ -> Nothing

-- | Determine whether a term does any work, i.e. adds to the size of the circuit
isWorkFree
  :: Term
  -> NormalizeSession Bool
isWorkFree (collectArgs -> (fun,args)) = case fun of
  Var i -> do
    isRec <- isRecursiveBndr i
    let isPoly = isPolyFunTy (varType i)
    if isLocalId i && not isPoly then
      pure True
    else if isGlobalId i && not isRec then do
      isWorkFreeBinder i
    else
      pure False
  Data {}          -> allM isWorkFreeArg args
  Literal {}       -> pure True
  Prim _ pInfo -> case primWorkInfo pInfo of
    WorkConstant   -> pure True -- We can ignore the arguments, because this
                                -- primitive outputs a constant regardless of its
                                -- arguments
    WorkNever      -> allM isWorkFreeArg args
    WorkVariable   -> pure (all isConstantArg' args)
    WorkAlways     -> pure False -- Things like clock or reset generator always
                                 -- perform work
  Lam _ e          -> liftA2 (&&) (isWorkFree e) (allM isWorkFreeArg args)
  TyLam _ e        -> liftA2 (&&) (isWorkFree e) (allM isWorkFreeArg args)
  Letrec bs e ->
    and <$> sequence [ isWorkFree e
                     , allM (isWorkFree . snd) bs
                     , allM isWorkFreeArg args ]
  Case s _ [(_,a)] ->
    and <$> sequence [ isWorkFree s
                     , isWorkFree a
                     , allM isWorkFreeArg args ]
  Cast e _ _       -> liftA2 (&&) (isWorkFree e) (allM isWorkFreeArg args)
  _                -> pure False
 where
  isWorkFreeArg = either isWorkFree (pure . const True)
  isConstantArg' = either isConstant (const True)

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
  :: Context
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
        Just (_,_,_,fBody) -> do
          -- There are no global mutually-recursive functions, only self-recursive
          -- ones, so checking whether 'f' is part of the free variables of the
          -- body of 'f' is sufficient.
          let isR = f `globalIdOccursIn` fBody
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
  subCsrs <- snd <$> mapAccumLM constantSpecInfoFolder ctx subTerms

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
      (Prim "Clash.Transformations.removedArg" _, _) ->
        pure (constantCsr e)
      _ -> do
        bindCsr ctx e
  else
    case collectArgsTicks e of
      (dc@(Data _), args, ticks) ->
        mergeCsrs ctx ticks e (mkApps dc) args

      -- TODO: Work with prim's WorkInfo?
      (prim@(Prim _ _), args, ticks) -> do
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
  let tm1 = deShadowTerm emptyInScopeSet tm
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

removedTm
  :: Type
  -> Term
removedTm =
  TyApp (Prim "Clash.Transformations.removedArg" (PrimInfo undefinedTy WorkNever))
