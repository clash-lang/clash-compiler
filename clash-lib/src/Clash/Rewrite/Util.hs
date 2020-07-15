{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilities for rewriting: e.g. inlining, specialisation, etc.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Rewrite.Util where

import           Control.Monad.Extra         (andM, eitherM)
import           Control.Concurrent.Supply   (splitSupply)
import           Control.DeepSeq
import           Control.Exception           (throw)
import           Control.Lens
  (Lens', (%=), (+=), (^.), _Left)
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail          (MonadFail)
#endif
import qualified Control.Monad.State.Strict  as State
import qualified Control.Monad.Writer        as Writer
import           Data.Bool                   (bool)
import           Data.Bifunctor              (bimap)
import           Data.Coerce                 (coerce)
import           Data.Functor.Const          (Const (..))
import           Data.List                   (group, partition, sort)
import qualified Data.List                   as List
import qualified Data.List.Extra             as List
import           Data.List.Extra             (allM, partitionM)
import qualified Data.Map                    as Map
import           Data.Maybe
import qualified Data.Monoid                 as Monoid
import qualified Data.Set                    as Set
import qualified Data.Set.Lens               as Lens
import qualified Data.Set.Ordered            as OSet
import qualified Data.Set.Ordered.Extra      as OSet
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

#ifdef HISTORY
import           Data.Binary                 (encode)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           System.IO.Unsafe            (unsafePerformIO)
#endif

import           BasicTypes                  (InlineSpec (..))

import           Clash.Core.DataCon          (dcExtTyVars)

#if EXPERIMENTAL_EVALUATOR
import           Clash.Core.Evaluator.Models
#else
import           Clash.Core.Evaluator.Types  (PureHeap, whnf')
#endif

import           Clash.Core.FreeVars
  (freeLocalVars, hasLocalFreeVars, localIdDoesNotOccurIn, localIdOccursIn,
   typeFreeVars, termFreeVars', freeLocalIds, globalIdOccursIn)
import           Clash.Core.Name
import           Clash.Core.Pretty           (showPpr)
import           Clash.Core.Subst
  (substTmEnv, aeqTerm, aeqType, extendIdSubst, mkSubst, substTm)
import           Clash.Core.Term
import           Clash.Core.TermInfo
import           Clash.Core.TyCon
  (TyConMap, tyConDataCons)
import           Clash.Core.Type             (KindOrType, Type (..),
                                              TypeView (..), coreView1,
                                              normalizeType,
                                              typeKind, tyView, isPolyFunTy)
import           Clash.Core.Util
  (dataConInstArgTysE, isClockOrReset, isEnable)
import           Clash.Core.Var
  (Id, IdScope (..), TyVar, Var (..), isLocalId, mkGlobalId, mkLocalId, mkTyVar)
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, elemVarSet, extendInScopeSetList, mkInScopeSet,
   uniqAway, uniqAway', mapVarEnv, eltsVarEnv, unitVarSet, emptyVarEnv,
   mkVarEnv, eltsVarSet, elemVarEnv, lookupVarEnv, extendVarEnv)
import           Clash.Debug
import           Clash.Driver.Types
  (DebugLevel (..), BindingMap, Binding(..))
import           Clash.Netlist.Util          (representableType)
import           Clash.Pretty                (clashPretty, showDoc)
import           Clash.Rewrite.Types
import           Clash.Unique
import           Clash.Util
import qualified Clash.Util.Interpolate as I

-- | Lift an action working in the '_extra' state to the 'RewriteMonad'
zoomExtra :: State.State extra a
          -> RewriteMonad extra a
zoomExtra m = R (\_ s w -> case State.runState m (s ^. extra) of
                            (a,s') -> (a,s {_extra = s'},w))

-- | Some transformations might erroneously introduce shadowing. For example,
-- a transformation might result in:
--
--   let a = ...
--       b = ...
--       a = ...
--
-- where the last 'a', shadows the first, while Clash assumes that this can't
-- happen. This function finds those constructs and a list of found duplicates.
--
findAccidentialShadows :: Term -> [[Id]]
findAccidentialShadows =
  \case
    Var {}      -> []
    Data {}     -> []
    Literal {}  -> []
    Prim {}     -> []
    Lam _ t     -> findAccidentialShadows t
    TyLam _ t   -> findAccidentialShadows t
    App t1 t2   -> concatMap findAccidentialShadows [t1, t2]
    TyApp t _   -> findAccidentialShadows t
    Cast t _ _  -> findAccidentialShadows t
    Tick _ t    -> findAccidentialShadows t
    Case t _ as ->
      concatMap (findInPat . fst) as ++
        concatMap findAccidentialShadows (t : map snd as)
    Letrec bs t ->
      findDups (map fst bs) ++ findAccidentialShadows t

 where
  findInPat :: Pat -> [[Id]]
  findInPat (LitPat _)        = []
  findInPat (DefaultPat)      = []
  findInPat (DataPat _ _ ids) = findDups ids

  findDups :: [Id] -> [[Id]]
  findDups ids = filter ((1 <) . length) (group (sort ids))


-- | Record if a transformation is successfully applied
apply
  :: String
  -- ^ Name of the transformation
  -> Rewrite extra
  -- ^ Transformation to be applied
  -> Rewrite extra
apply = \s rewrite ctx expr0 -> do
  lvl <- Lens.view dbgLevel
  dbgTranss <- Lens.view dbgTransformations
  let isTryLvl = lvl == DebugTry || lvl >= DebugAll
      isRelevantTrans = s `Set.member` dbgTranss || Set.null dbgTranss
  traceIf (isTryLvl && isRelevantTrans) ("Trying: " ++ s) (pure ())

  (expr1,anyChanged) <- Writer.listen (rewrite ctx expr0)
  let hasChanged = Monoid.getAny anyChanged
      !expr2     = if hasChanged then expr1 else expr0
  Monad.when hasChanged (transformCounter += 1)
#ifdef HISTORY
  -- NB: When HISTORY is on, emit binary data holding the recorded rewrite steps
  Monad.when hasChanged $ do
    (curBndr, _) <- Lens.use curFun
    let !_ = unsafePerformIO
             $ BS.appendFile "history.dat"
             $ BL.toStrict
             $ encode RewriteStep
                 { t_ctx    = tfContext ctx
                 , t_name   = s
                 , t_bndrS  = showPpr (varName curBndr)
                 , t_before = expr0
                 , t_after  = expr1
                 }
    return ()
#endif

  dbgFrom <- Lens.view dbgTransformationsFrom
  dbgLimit <- Lens.view dbgTransformationsLimit
  let fromLimit =
        if (dbgFrom, dbgLimit) == (0, maxBound)
        then Nothing
        else Just (dbgFrom, dbgLimit)

  if lvl == DebugNone
    then return expr2
    else applyDebug lvl dbgTranss fromLimit s expr0 hasChanged expr2
{-# INLINE apply #-}

applyDebug
  :: DebugLevel
  -- ^ The current debugging level
  -> Set.Set String
  -- ^ Transformations to debug
  -> Maybe (Int, Int)
  -- ^ Only print debug information for transformations [n, n+limit]. See flag
  -- documentation of "-fclash-debug-transformations-from" and
  -- "-fclash-debug-transformations-limit"
  -> String
  -- ^ Name of the transformation
  -> Term
  -- ^ Original expression
  -> Bool
  -- ^ Whether the rewrite indicated change
  -> Term
  -- ^ New expression
  -> RewriteMonad extra Term
applyDebug lvl transformations fromLimit name exprOld hasChanged exprNew
  | Just (from, limit) <- fromLimit = do
    nTrans <- Lens.use transformCounter
    if | nTrans - from > limit ->
          error "-fclash-debug-transformations-limit exceeded"
       | nTrans > from ->
          applyDebug lvl transformations Nothing name exprOld hasChanged exprNew
       | otherwise ->
          pure exprNew

applyDebug lvl transformations fromLimit name exprOld hasChanged exprNew
  | not (Set.null transformations) =
    let newLvl = bool DebugNone lvl (name `Set.member` transformations) in
    applyDebug newLvl Set.empty fromLimit name exprOld hasChanged exprNew

applyDebug lvl _transformations _fromLimit name exprOld hasChanged exprNew =
 traceIf (lvl >= DebugAll) ("Tried: " ++ name ++ " on:\n" ++ before) $ do
  nTrans <- pred <$> Lens.use transformCounter
  Monad.when (lvl > DebugNone && hasChanged) $ do
    tcm                  <- Lens.view tcCache
    let beforeTy          = termType tcm exprOld
        beforeFV          = Lens.setOf freeLocalVars exprOld
        afterTy           = termType tcm exprNew
        afterFV           = Lens.setOf freeLocalVars exprNew
        newFV             = not (afterFV `Set.isSubsetOf` beforeFV)
        accidentalShadows = findAccidentialShadows exprNew

    Monad.when newFV $
            error ( concat [ $(curLoc)
                           , "Error when applying rewrite ", name
                           , " to:\n" , before
                           , "\nResult:\n" ++ after ++ "\n"
                           , "It introduces free variables."
                           , "\nBefore: " ++ showPpr (Set.toList beforeFV)
                           , "\nAfter: " ++ showPpr (Set.toList afterFV)
                           ]
                  )
    Monad.when (not (null accidentalShadows)) $
      error ( concat [ $(curLoc)
                     , "Error when applying rewrite ", name
                     , " to:\n" , before
                     , "\nResult:\n" ++ after ++ "\n"
                     , "It accidentally creates shadowing let/case-bindings:\n"
                     , " ", showPpr accidentalShadows, "\n"
                     , "This usually means that a transformation did not extend "
                     , "or incorrectly extended its InScopeSet before applying a "
                     , "substitution."
                     ])

    traceIf (lvl >= DebugApplied && (not (normalizeType tcm beforeTy `aeqType` normalizeType tcm afterTy)))
            ( concat [ $(curLoc)
                     , "Error when applying rewrite ", name
                     , " to:\n" , before
                     , "\nResult:\n" ++ after ++ "\n"
                     , "Changes type from:\n", showPpr beforeTy
                     , "\nto:\n", showPpr afterTy
                     ]
            ) (return ())

  Monad.when (lvl >= DebugApplied && not hasChanged && not (exprOld `aeqTerm` exprNew)) $
    error $ $(curLoc) ++ "Expression changed without notice(" ++ name ++  "): before"
                      ++ before ++ "\nafter:\n" ++ after

  traceIf (lvl >= DebugName && hasChanged) (name <> " {" <> show nTrans <> "}") $
    traceIf (lvl >= DebugApplied && hasChanged) ("Changes when applying rewrite to:\n"
                      ++ before ++ "\nResult:\n" ++ after ++ "\n") $
      traceIf (lvl >= DebugAll && not hasChanged) ("No changes when applying rewrite "
                        ++ name ++ " to:\n" ++ after ++ "\n") $
        return exprNew
 where
  before = showPpr exprOld
  after  = showPpr exprNew

-- | Perform a transformation on a Term
runRewrite
  :: String
  -- ^ Name of the transformation
  -> InScopeSet
  -> Rewrite extra
  -- ^ Transformation to perform
  -> Term
  -- ^ Term to transform
  -> RewriteMonad extra Term
runRewrite name is rewrite expr = apply name rewrite (TransformContext is []) expr

-- | Evaluate a RewriteSession to its inner monad.
runRewriteSession :: RewriteEnv
                  -> RewriteState extra
                  -> RewriteMonad extra a
                  -> a
runRewriteSession r s m =
  traceIf (_dbgLevel r > DebugSilent)
    ("Clash: Applied " ++ show (s' ^. transformCounter) ++ " transformations")
    a
  where
    (a,s',_) = runR m r s

-- | Notify that a transformation has changed the expression
setChanged :: RewriteMonad extra ()
setChanged = Writer.tell (Monoid.Any True)

-- | Identity function that additionally notifies that a transformation has
-- changed the expression
changed :: a -> RewriteMonad extra a
changed val = do
  Writer.tell (Monoid.Any True)
  return val

closestLetBinder :: Context -> Maybe Id
closestLetBinder [] = Nothing
closestLetBinder (LetBinding id_ _:_) = Just id_
closestLetBinder (_:ctx)              = closestLetBinder ctx

mkDerivedName :: TransformContext -> OccName -> TmName
mkDerivedName (TransformContext _ ctx) sf = case closestLetBinder ctx of
  Just id_ -> appendToName (varName id_) ('_' `Text.cons` sf)
  _ -> mkUnsafeInternalName sf 0

-- | Make a new binder and variable reference for a term
mkTmBinderFor
  :: (MonadUnique m, MonadFail m)
  => InScopeSet
  -> TyConMap -- ^ TyCon cache
  -> Name a -- ^ Name of the new binder
  -> Term -- ^ Term to bind
  -> m Id
mkTmBinderFor is tcm name e = do
  Left r <- mkBinderFor is tcm name (Left e)
  return r

-- | Make a new binder and variable reference for either a term or a type
mkBinderFor
  :: (MonadUnique m, MonadFail m)
  => InScopeSet
  -> TyConMap -- ^ TyCon cache
  -> Name a -- ^ Name of the new binder
  -> Either Term Type -- ^ Type or Term to bind
  -> m (Either Id TyVar)
mkBinderFor is tcm name (Left term) = do
  name' <- cloneNameWithInScopeSet is name
  let ty = termType tcm term
  return (Left (mkLocalId ty (coerce name')))

mkBinderFor is tcm name (Right ty) = do
  name' <- cloneNameWithInScopeSet is name
  let ki = typeKind tcm ty
  return (Right (mkTyVar ki (coerce name')))

-- | Make a new, unique, identifier
mkInternalVar
  :: (MonadUnique m)
  => InScopeSet
  -> OccName
  -- ^ Name of the identifier
  -> KindOrType
  -> m Id
mkInternalVar inScope name ty = do
  i <- getUniqueM
  let nm = mkUnsafeInternalName name i
  return (uniqAway inScope (mkLocalId ty nm))

-- | Inline the binders in a let-binding that have a certain property
inlineBinders
  :: (Term -> LetBinding -> RewriteMonad extra Bool)
  -- ^ Property test
  -> Rewrite extra
inlineBinders condition (TransformContext inScope0 _) expr@(Letrec xes res) = do
  (toInline,toKeep) <- partitionM (condition expr) xes
  case toInline of
    [] -> return expr
    _  -> do
      let inScope1 = extendInScopeSetList inScope0 (map fst xes)
          (toInlRec,(toKeep1,res1)) =
            substituteBinders inScope1 toInline toKeep res
      case toInlRec ++ toKeep1 of
        []   -> changed res1
        xes1 -> changed (Letrec xes1 res1)

inlineBinders _ _ e = return e

-- | Determine whether a binder is a join-point created for a complex case
-- expression.
--
-- A join-point is when a local function only occurs in tail-call positions,
-- and when it does, more than once.
isJoinPointIn :: Id   -- ^ 'Id' of the local binder
              -> Term -- ^ Expression in which the binder is bound
              -> Bool
isJoinPointIn id_ e = case tailCalls id_ e of
                      Just n | n > 1 -> True
                      _              -> False

-- | Count the number of (only) tail calls of a function in an expression.
-- 'Nothing' indicates that the function was used in a non-tail call position.
tailCalls :: Id   -- ^ Function to check
          -> Term -- ^ Expression to check it in
          -> Maybe Int
tailCalls id_ = \case
  Var nm | id_ == nm -> Just 1
         | otherwise -> Just 0
  Lam _ e -> tailCalls id_ e
  TyLam _ e -> tailCalls id_ e
  App l r  -> case tailCalls id_ r of
                Just 0 -> tailCalls id_ l
                _      -> Nothing
  TyApp l _ -> tailCalls id_ l
  Letrec bs e ->
    let (bsIds,bsExprs) = unzip bs
        bsTls           = map (tailCalls id_) bsExprs
        bsIdsUsed       = mapMaybe (\(l,r) -> pure l <* r) (zip bsIds bsTls)
        bsIdsTls        = map (`tailCalls` e) bsIdsUsed
        bsCount         = pure . sum $ catMaybes bsTls
    in  case (all isJust bsTls) of
          False -> Nothing
          True  -> case (all (==0) $ catMaybes bsTls) of
            False  -> case all isJust bsIdsTls of
              False -> Nothing
              True  -> (+) <$> bsCount <*> tailCalls id_ e
            True -> tailCalls id_ e
  Case scrut _ alts ->
    let scrutTl = tailCalls id_ scrut
        altsTl  = map (tailCalls id_ . snd) alts
    in  case scrutTl of
          Just 0 | all (/= Nothing) altsTl -> Just (sum (catMaybes altsTl))
          _ -> Nothing
  _ -> Just 0

-- | Determines whether a function has the following shape:
--
-- > \(w :: Void) -> f a b c
--
-- i.e. is a wrapper around a (partially) applied function 'f', where the
-- introduced argument 'w' is not used by 'f'
isVoidWrapper :: Term -> Bool
isVoidWrapper (Lam bndr e@(collectArgs -> (Var _,_))) =
  bndr `localIdDoesNotOccurIn` e
isVoidWrapper _ = False

-- | Inline the first set of binder into the second set of binders and into the
-- body of the original let expression.
substituteBinders
  :: InScopeSet
  -> [LetBinding]
  -- ^ Let-binders to substitute
  -> [LetBinding]
  -- ^ Let-binders where substitution takes place
  -> Term
  -- ^ Body where substitution takes place
  -> ([LetBinding],([LetBinding],Term))
  -- ^
  -- 1. Let-bindings that we wanted to substitute, but turned out to be recursive
  -- 2.1 Let-binders where substitution took place
  -- 2.2 Body where substitution took place
substituteBinders inScope toInline toKeep body =
  let (subst,toInlRec) = go (mkSubst inScope) [] toInline
  in  ( map (second (substTm "substToInlRec" subst)) toInlRec
      , ( map (second (substTm "substToKeep" subst)) toKeep
        , substTm "substBody" subst body) )
 where
  go subst inlRec [] = (subst,inlRec)
  go !subst !inlRec ((x,e):toInl) =
    let e1      = substTm "substInl" subst e
        substE  = extendIdSubst (mkSubst inScope) x e1
        subst1  = subst { substTmEnv = mapVarEnv (substTm "substSubst" substE)
                                                 (substTmEnv subst)}
        subst2  = extendIdSubst subst1 x e1
    in  if x `localIdOccursIn` e1 then
          go subst ((x,e1):inlRec) toInl
        else
          go subst2 inlRec toInl

-- | Lift the first set of binders to the level of global bindings, and substitute
-- these lifted bindings into the second set of binders and the body of the
-- original let expression.
liftAndSubsituteBinders
  :: InScopeSet
  -> [LetBinding]
  -- ^ Let-binders to lift, and substitute the lifted result
  -> [LetBinding]
  -- ^ Lef-binders where substitution takes place
  -> Term
  -- ^ Body where substitution takes place
  -> RewriteMonad extra ([LetBinding],Term)
liftAndSubsituteBinders inScope toLift toKeep body = do
  subst <- go (mkSubst inScope) toLift
  pure ( map (second (substTm "liftToKeep" subst)) toKeep
       , substTm "keepBody" subst body
       )
 where
  go subst [] = pure subst
  go !subst ((x,e):inl) = do
    let e1 = substTm "liftInl" subst e
    (_,e2) <- liftBinding (x,e1)
    let substE = extendIdSubst (mkSubst inScope) x e2
        subst1 = subst { substTmEnv = mapVarEnv (substTm "liftSubst" substE)
                                                (substTmEnv subst) }
        subst2 = extendIdSubst subst1 x e2
    if x `localIdOccursIn` e2 then do
      (_,sp) <- Lens.use curFun
      throw (ClashException sp [I.i|
        Internal error: inlineOrLiftBInders failed on:

        #{showPpr (x,e)}

        creating a self-recursive let-binding:

        #{showPpr (x,e2)}

        given the already built subtitution:

        #{showDoc (clashPretty (substTmEnv subst))}
      |] Nothing)
    else
      go subst2 inl

-- | Determines whether a global binder is work free. Errors if binder does
-- not exist.
isWorkFreeBinder :: HasCallStack => Id -> RewriteMonad extra Bool
isWorkFreeBinder bndr =
  makeCachedU bndr workFreeBinders $ do
    bExprM <- lookupVarEnv bndr <$> Lens.use bindings
    case bExprM of
      Nothing -> error ("isWorkFreeBinder: couldn't find binder: " ++ showPpr bndr)
      Just (bindingTerm -> t) ->
        if bndr `globalIdOccursIn` t
        then pure False
        else isWorkFree t

-- | Determine whether a term does any work, i.e. adds to the size of the circuit
isWorkFree
  :: Term
  -> RewriteMonad extra Bool
isWorkFree (collectArgs -> (fun,args)) = case fun of
  Var i ->
    if | isPolyFunTy (varType i) -> pure False
       | isLocalId i -> pure True
       | otherwise -> andM [isWorkFreeBinder i, allM isWorkFreeArg args]
  Data {} -> allM isWorkFreeArg args
  Literal {} -> pure True
  Prim pInfo -> case primWorkInfo pInfo of
    -- We can ignore the arguments, because this primitive outputs a constant
    -- regardless of its arguments
    WorkConstant -> pure True
    WorkNever -> allM isWorkFreeArg args
    WorkVariable -> pure (all isConstantArg args)
    -- Things like clock or reset generator always perform work
    WorkAlways -> pure False
  Lam _ e -> andM [isWorkFree e, allM isWorkFreeArg args]
  TyLam _ e -> andM [isWorkFree e, allM isWorkFreeArg args]
  Letrec bs e ->
    andM [isWorkFree e, allM (isWorkFree . snd) bs, allM isWorkFreeArg args]
  Case s _ [(_,a)] ->
    andM [isWorkFree s, isWorkFree a, allM isWorkFreeArg args]
  Cast e _ _ ->
    andM [isWorkFree e, allM isWorkFreeArg args]
  _ ->
    pure False
 where
  isWorkFreeArg e = eitherM isWorkFree (pure . const True) (pure e)
  isConstantArg = either isConstant (const True)

isFromInt :: Text -> Bool
isFromInt nm = nm == "Clash.Sized.Internal.BitVector.fromInteger##" ||
               nm == "Clash.Sized.Internal.BitVector.fromInteger#" ||
               nm == "Clash.Sized.Internal.Index.fromInteger#" ||
               nm == "Clash.Sized.Internal.Signed.fromInteger#" ||
               nm == "Clash.Sized.Internal.Unsigned.fromInteger#"

-- | Determine if a term represents a constant
isConstant :: Term -> Bool
isConstant e = case collectArgs e of
  (Data _, args)   -> all (either isConstant (const True)) args
  (Prim _, args) -> all (either isConstant (const True)) args
  (Lam _ _, _)     -> not (hasLocalFreeVars e)
  (Literal _,_)    -> True
  _                -> False

isConstantNotClockReset
  :: Term
  -> RewriteMonad extra Bool
isConstantNotClockReset e = do
  tcm <- Lens.view tcCache
  let eTy = termType tcm e
  if isClockOrReset tcm eTy
     then case collectArgs e of
        (Prim p,_) -> return (primName p == "Clash.Transformations.removedArg")
        _ -> return False
     else pure (isConstant e)

-- TODO: Remove function after using WorkInfo in 'isWorkFreeIsh'
isWorkFreeClockOrResetOrEnable
  :: TyConMap
  -> Term
  -> Maybe Bool
isWorkFreeClockOrResetOrEnable tcm e =
  let eTy = termType tcm e in
  if isClockOrReset tcm eTy || isEnable tcm eTy then
    case collectArgs e of
      (Prim p,_) -> Just (primName p == "Clash.Transformations.removedArg")
      (Var _, []) -> Just True
      (Data _, []) -> Just True -- For Enable True/False
      (Literal _,_) -> Just True
      _ -> Just False
  else
    Nothing

-- | A conservative version of 'isWorkFree'. Is used to determine in 'bindConstantVar'
-- to determine whether an expression can be "bound" (locally inlined). While
-- binding workfree expressions won't result in extra work for the circuit, it
-- might very well cause extra work for Clash. In fact, using 'isWorkFree' in
-- 'bindConstantVar' makes Clash two orders of magnitude slower for some of our
-- test cases.
--
-- In effect, this function is a version of 'isConstant' that also considers
-- references to clocks and resets constant. This allows us to bind
-- HiddenClock(ResetEnable) constructs, allowing Clash to constant spec
-- subconstants - most notably KnownDomain. Doing that enables Clash to
-- eliminate any case-constructs on it.
isWorkFreeIsh
  :: Term
  -> RewriteMonad extra Bool
isWorkFreeIsh e = do
  tcm <- Lens.view tcCache
  case isWorkFreeClockOrResetOrEnable tcm e of
    Just b -> pure b
    Nothing ->
      case collectArgs e of
        (Data _, args)   -> allM isWorkFreeIshArg args
        (Prim pInfo, args) -> case primWorkInfo pInfo of
          WorkAlways     -> pure False -- Things like clock or reset generator always
                                       -- perform work
          WorkVariable   -> pure (all isConstantArg args)
          _              -> allM isWorkFreeIshArg args
        (Lam _ _, _)     -> pure (not (hasLocalFreeVars e))
        (Literal _,_)    -> pure True
        _                -> pure False
 where
  isWorkFreeIshArg = either isWorkFreeIsh (pure . const True)
  isConstantArg    = either isConstant (const True)

inlineOrLiftBinders
  :: (LetBinding -> RewriteMonad extra Bool)
  -- ^ Property test
  -> (Term -> LetBinding -> Bool)
  -- ^ Test whether to lift or inline
  --
  -- * True: inline
  -- * False: lift
  -> Rewrite extra
inlineOrLiftBinders condition inlineOrLift (TransformContext inScope0 _) e@(Letrec bndrs body) = do
  (toReplace,toKeep) <- partitionM condition bndrs
  case toReplace of
    [] -> return e
    _  -> do
      let inScope1 = extendInScopeSetList inScope0 (map fst bndrs)
      let (toInline,toLift) = partition (inlineOrLift e) toReplace
      -- We first substitute the binders that we can inline both the binders
      -- that we intend to lift, the other binders, and the body
      let (toLiftExtra,(toReplace1,body1)) =
            substituteBinders inScope1 toInline (toLift ++ toKeep) body
          (toLift1,toKeep1) = splitAt (length toLift) toReplace1
      -- We then substitute the lifted binders in the other binders and the body
      (toKeep2,body2) <- liftAndSubsituteBinders inScope1
                           (toLiftExtra ++ toLift1)
                           toKeep1 body1
      case toKeep2 of
        [] -> changed body2
        _  -> changed (Letrec toKeep2 body2)

inlineOrLiftBinders _ _ _ e = return e

-- | Create a global function for a Let-binding and return a Let-binding where
-- the RHS is a reference to the new global function applied to the free
-- variables of the original RHS
liftBinding :: LetBinding
            -> RewriteMonad extra LetBinding
liftBinding (var@Id {varName = idName} ,e) = do
  -- Get all local FVs, excluding the 'idName' from the let-binding
  let unitFV :: Var a -> Const (UniqSet TyVar,UniqSet Id) (Var a)
      unitFV v@(Id {})    = Const (emptyUniqSet,unitUniqSet (coerce v))
      unitFV v@(TyVar {}) = Const (unitUniqSet (coerce v),emptyUniqSet)

      interesting :: Var a -> Bool
      interesting Id {idScope = GlobalId} = False
      interesting v@(Id {idScope = LocalId}) = varUniq v /= varUniq var
      interesting _ = True

      (boundFTVsSet,boundFVsSet) =
        getConst (Lens.foldMapOf (termFreeVars' interesting) unitFV e)
      boundFTVs = eltsUniqSet boundFTVsSet
      boundFVs  = eltsUniqSet boundFVsSet

  -- Make a new global ID
  tcm       <- Lens.view tcCache
  let newBodyTy = termType tcm $ mkTyLams (mkLams e boundFVs) boundFTVs
  (cf,sp)   <- Lens.use curFun
  binders <- Lens.use bindings
  newBodyNm <-
    cloneNameWithBindingMap
      binders
      (appendToName (varName cf) ("_" `Text.append` nameOcc idName))
  let newBodyId = mkGlobalId newBodyTy newBodyNm {nameSort = Internal}

  -- Make a new expression, consisting of the the lifted function applied to
  -- its free variables
  let newExpr = mkTmApps
                  (mkTyApps (Var newBodyId)
                            (map VarTy boundFTVs))
                  (map Var boundFVs)
      inScope0 = mkInScopeSet (coerce boundFVsSet)
      inScope1 = extendInScopeSetList inScope0 [var,newBodyId]
  let subst    = extendIdSubst (mkSubst inScope1) var newExpr
      -- Substitute the recursive calls by the new expression
      e' = substTm "liftBinding" subst e
      -- Create a new body that abstracts over the free variables
      newBody = mkTyLams (mkLams e' boundFVs) boundFTVs

  -- Check if an alpha-equivalent global binder already exists
  aeqExisting <- (eltsUniqMap . filterUniqMap ((`aeqTerm` newBody) . bindingTerm)) <$> Lens.use bindings
  case aeqExisting of
    -- If it doesn't, create a new binder
    [] -> do -- Add the created function to the list of global bindings
             bindings %= extendUniqMap newBodyNm
                                    -- We mark this function as internal so that
                                    -- it can be inlined at the very end of
                                    -- the normalisation pipeline as part of the
                                    -- flattening pass. We don't inline
                                    -- right away because we are lifting this
                                    -- function at this moment for a reason!
                                    -- (termination, CSE and DEC oppertunities,
                                    -- ,etc.)
                                    (Binding
                                      newBodyId
                                      sp
#if MIN_VERSION_ghc(8,4,1)
                                      NoUserInline
#else
                                      EmptyInlineSpec
#endif
                                      newBody)
             -- Return the new binder
             return (var, newExpr)
    -- If it does, use the existing binder
    (b:_) ->
      let newExpr' = mkTmApps
                      (mkTyApps (Var $ bindingId b)
                                (map VarTy boundFTVs))
                      (map Var boundFVs)
      in  return (var, newExpr')

liftBinding _ = error $ $(curLoc) ++ "liftBinding: invalid core, expr bound to tyvar"

-- | Ensure that the 'Unique' of a variable does not occur in the 'BindingMap'
uniqAwayBinder
  :: BindingMap
  -> Name a
  -> Name a
uniqAwayBinder binders nm =
  uniqAway' (`elemUniqMapDirectly` binders) (nameUniq nm) nm

-- | Make a global function for a name-term tuple
mkFunction
  :: TmName
  -- ^ Name of the function
  -> SrcSpan
  -> InlineSpec
  -> Term
  -- ^ Term bound to the function
  -> RewriteMonad extra Id
  -- ^ Name with a proper unique and the type of the function
mkFunction bndrNm sp inl body = do
  tcm <- Lens.view tcCache
  let bodyTy = termType tcm body
  binders <- Lens.use bindings
  bodyNm <- cloneNameWithBindingMap binders bndrNm
  addGlobalBind bodyNm bodyTy sp inl body
  return (mkGlobalId bodyTy bodyNm)

-- | Add a function to the set of global binders
addGlobalBind
  :: TmName
  -> Type
  -> SrcSpan
  -> InlineSpec
  -> Term
  -> RewriteMonad extra ()
addGlobalBind vNm ty sp inl body = do
  let vId = mkGlobalId ty vNm
  (ty,body) `deepseq` bindings %= extendUniqMap vNm (Binding vId sp inl body)

-- | Create a new name out of the given name, but with another unique. Resulting
-- unique is guaranteed to not be in the given InScopeSet.
cloneNameWithInScopeSet
  :: (MonadUnique m)
  => InScopeSet
  -> Name a
  -> m (Name a)
cloneNameWithInScopeSet is nm = do
  i <- getUniqueM
  return (uniqAway is (setUnique nm i))

-- | Create a new name out of the given name, but with another unique. Resulting
-- unique is guaranteed to not be in the given BindingMap.
cloneNameWithBindingMap
  :: (MonadUnique m)
  => BindingMap
  -> Name a
  -> m (Name a)
cloneNameWithBindingMap binders nm = do
  i <- getUniqueM
  return (uniqAway' (`elemUniqMapDirectly` binders) i (setUnique nm i))

{-# INLINE isUntranslatable #-}
-- | Determine if a term cannot be represented in hardware
isUntranslatable
  :: Bool
  -- ^ String representable
  -> Term
  -> RewriteMonad extra Bool
isUntranslatable stringRepresentable tm = do
  tcm <- Lens.view tcCache
  not <$> (representableType <$> Lens.view typeTranslator
                             <*> Lens.view customReprs
                             <*> pure stringRepresentable
                             <*> pure tcm
                             <*> pure (termType tcm tm))

{-# INLINE isUntranslatableType #-}
-- | Determine if a type cannot be represented in hardware
isUntranslatableType
  :: Bool
  -- ^ String representable
  -> Type
  -> RewriteMonad extra Bool
isUntranslatableType stringRepresentable ty =
  not <$> (representableType <$> Lens.view typeTranslator
                             <*> Lens.view customReprs
                             <*> pure stringRepresentable
                             <*> Lens.view tcCache
                             <*> pure ty)

-- | Make a binder that should not be referenced
mkWildValBinder
  :: (MonadUnique m)
  => InScopeSet
  -> Type
  -> m Id
mkWildValBinder is = mkInternalVar is "wild"

-- | Make a case-decomposition that extracts a field out of a (Sum-of-)Product type
mkSelectorCase
  :: HasCallStack
  => (Functor m, MonadUnique m)
  => String -- ^ Name of the caller of this function
  -> InScopeSet
  -> TyConMap -- ^ TyCon cache
  -> Term -- ^ Subject of the case-composition
  -> Int -- n'th DataCon
  -> Int -- n'th field
  -> m Term
mkSelectorCase caller inScope tcm scrut dcI fieldI = go (termType tcm scrut)
  where
    go (coreView1 tcm -> Just ty') = go ty'
    go scrutTy@(tyView -> TyConApp tc args) =
      case tyConDataCons (lookupUniqMap' tcm tc) of
        [] -> cantCreate $(curLoc) ("TyCon has no DataCons: " ++ show tc ++ " " ++ showPpr tc) scrutTy
        dcs | dcI > length dcs -> cantCreate $(curLoc) "DC index exceeds max" scrutTy
            | otherwise -> do
          let dc = indexNote ($(curLoc) ++ "No DC with tag: " ++ show (dcI-1)) dcs (dcI-1)
          let (Just fieldTys) = dataConInstArgTysE inScope tcm dc args
          if fieldI >= length fieldTys
            then cantCreate $(curLoc) "Field index exceed max" scrutTy
            else do
              wildBndrs <- mapM (mkWildValBinder inScope) fieldTys
              let ty = indexNote ($(curLoc) ++ "No DC field#: " ++ show fieldI) fieldTys fieldI
              selBndr <- mkInternalVar inScope "sel" ty
              let bndrs  = take fieldI wildBndrs ++ [selBndr] ++ drop (fieldI+1) wildBndrs
                  pat    = DataPat dc (dcExtTyVars dc) bndrs
                  retVal = Case scrut ty [ (pat, Var selBndr) ]
              return retVal
    go scrutTy = cantCreate $(curLoc) ("Type of subject is not a datatype: " ++ showPpr scrutTy) scrutTy

    cantCreate loc info scrutTy = error $ loc ++ "Can't create selector " ++ show (caller,dcI,fieldI) ++ " for: (" ++ showPpr scrut ++ " :: " ++ showPpr scrutTy ++ ")\nAdditional info: " ++ info

-- | Specialise an application on its argument
specialise :: Lens' extra (Map.Map (Id, Int, Either Term Type) Id) -- ^ Lens into previous specialisations
           -> Lens' extra (VarEnv Int) -- ^ Lens into the specialisation history
           -> Lens' extra Int -- ^ Lens into the specialisation limit
           -> Rewrite extra
specialise specMapLbl specHistLbl specLimitLbl ctx e = case e of
  (TyApp e1 ty) -> specialise' specMapLbl specHistLbl specLimitLbl ctx e (collectArgsTicks e1) (Right ty)
  (App e1 e2)   -> specialise' specMapLbl specHistLbl specLimitLbl ctx e (collectArgsTicks e1) (Left  e2)
  _             -> return e

-- | Specialise an application on its argument
specialise' :: Lens' extra (Map.Map (Id, Int, Either Term Type) Id) -- ^ Lens into previous specialisations
            -> Lens' extra (VarEnv Int) -- ^ Lens into specialisation history
            -> Lens' extra Int -- ^ Lens into the specialisation limit
            -> TransformContext -- Transformation context
            -> Term -- ^ Original term
            -> (Term, [Either Term Type], [TickInfo]) -- ^ Function part of the term, split into root and applied arguments
            -> Either Term Type -- ^ Argument to specialize on
            -> RewriteMonad extra Term
specialise' specMapLbl specHistLbl specLimitLbl (TransformContext is0 _) e (Var f, args, ticks) specArgIn = do
  lvl <- Lens.view dbgLevel
  tcm <- Lens.view tcCache

  -- Don't specialise TopEntities
  topEnts <- Lens.view topEntities
  if f `elemVarSet` topEnts
  then do
    case specArgIn of
      Left _ -> traceIf (lvl >= DebugNone) ("Not specializing TopEntity: " ++ showPpr (varName f)) (return e)
      Right tyArg -> traceIf (lvl >= DebugApplied) ("Dropping type application on TopEntity: " ++ showPpr (varName f) ++ "\ntype:\n" ++ showPpr tyArg) $
        -- TopEntities aren't allowed to be semantically polymorphic.
        -- But using type equality constraints they may be syntactically polymorphic.
        -- > topEntity :: forall dom . (dom ~ "System") => Signal dom Bool -> Signal dom Bool
        -- The TyLam's in the body will have been removed by 'Clash.Normalize.Util.substWithTyEq'.
        -- So we drop the TyApp ("specialising" on it) and change the varType to match.
        let newVarTy = piResultTy tcm (varType f) tyArg
        in  changed (mkApps (mkTicks (Var f{varType = newVarTy}) ticks) args)
  else do -- NondecreasingIndentation

  let specArg = bimap (normalizeTermTypes tcm) (normalizeType tcm) specArgIn
      -- Create binders and variable references for free variables in 'specArg'
      -- (specBndrsIn,specVars) :: ([Either Id TyVar], [Either Term Type])
      (specBndrsIn,specVars) = specArgBndrsAndVars specArg
      argLen  = length args
      specBndrs :: [Either Id TyVar]
      specBndrs = map (Lens.over _Left (normalizeId tcm)) specBndrsIn
      specAbs :: Either Term Type
      specAbs = either (Left . (`mkAbstraction` specBndrs)) (Right . id) specArg
  -- Determine if 'f' has already been specialized on (a type-normalized) 'specArg'
  specM <- Map.lookup (f,argLen,specAbs) <$> Lens.use (extra.specMapLbl)
  case specM of
    -- Use previously specialized function
    Just f' ->
      traceIf (lvl >= DebugApplied)
        ("Using previous specialization of " ++ showPpr (varName f) ++ " on " ++
          (either showPpr showPpr) specAbs ++ ": " ++ showPpr (varName f')) $
        changed $ mkApps (mkTicks (Var f') ticks) (args ++ specVars)
    -- Create new specialized function
    Nothing -> do
      -- Determine if we can specialize f
      bodyMaybe <- fmap (lookupUniqMap (varName f)) $ Lens.use bindings
      case bodyMaybe of
        Just (Binding _ sp inl bodyTm) -> do
          -- Determine if we see a sequence of specialisations on a growing argument
          specHistM <- lookupUniqMap f <$> Lens.use (extra.specHistLbl)
          specLim   <- Lens.use (extra . specLimitLbl)
          if maybe False (> specLim) specHistM
            then throw (ClashException
                        sp
                        (unlines [ "Hit specialisation limit " ++ show specLim ++ " on function `" ++ showPpr (varName f) ++ "'.\n"
                                 , "The function `" ++ showPpr f ++ "' is most likely recursive, and looks like it is being indefinitely specialized on a growing argument.\n"
                                 , "Body of `" ++ showPpr f ++ "':\n" ++ showPpr bodyTm ++ "\n"
                                 , "Argument (in position: " ++ show argLen ++ ") that triggered termination:\n" ++ (either showPpr showPpr) specArg
                                 , "Run with '-fclash-spec-limit=N' to increase the specialisation limit to N."
                                 ])
                        Nothing)
            else do
              let existingNames = collectBndrsMinusApps bodyTm
                  newNames      = [ mkUnsafeInternalName ("pTS" `Text.append` Text.pack (show n)) n
                                  | n <- [(0::Int)..]
                                  ]
              -- Make new binders for existing arguments
              (boundArgs,argVars) <- fmap (unzip . map (either (Left &&& Left . Var) (Right &&& Right . VarTy))) $
                                     Monad.zipWithM
                                       (mkBinderFor is0 tcm)
                                       (existingNames ++ newNames)
                                       args
              -- Determine name the resulting specialized function, and the
              -- form of the specialized-on argument
              (fId,inl',specArg') <- case specArg of
                Left a@(collectArgsTicks -> (Var g,gArgs,_gTicks)) -> if isPolyFun tcm a
                    then do
                      -- In case we are specialising on an argument that is a
                      -- global function then we use that function's name as the
                      -- name of the specialized higher-order function.
                      -- Additionally, we will return the body of the global
                      -- function, instead of a variable reference to the
                      -- global function.
                      --
                      -- This will turn things like @mealy g k@ into a new
                      -- binding @g'@ where both the body of @mealy@ and @g@
                      -- are inlined, meaning the state-transition-function
                      -- and the memory element will be in a single function.
                      gTmM <- fmap (lookupUniqMap (varName g)) $ Lens.use bindings
                      return (g,maybe inl bindingSpec gTmM, maybe specArg (Left . (`mkApps` gArgs) . bindingTerm) gTmM)
                    else return (f,inl,specArg)
                _ -> return (f,inl,specArg)
              -- Create specialized functions
              let newBody = mkAbstraction (mkApps bodyTm (argVars ++ [specArg'])) (boundArgs ++ specBndrs)
              newf <- mkFunction (varName fId) sp inl' newBody
              -- Remember specialization
              (extra.specHistLbl) %= extendUniqMapWith f 1 (+)
              (extra.specMapLbl)  %= Map.insert (f,argLen,specAbs) newf
              -- use specialized function
              let newExpr = mkApps (mkTicks (Var newf) ticks) (args ++ specVars)
              newf `deepseq` changed newExpr
        Nothing -> return e
  where
    collectBndrsMinusApps :: Term -> [Name a]
    collectBndrsMinusApps = reverse . go []
      where
        go bs (Lam v e')    = go (coerce (varName v):bs)  e'
        go bs (TyLam tv e') = go (coerce (varName tv):bs) e'
        go bs (App e' _) = case go [] e' of
          []  -> bs
          bs' -> init bs' ++ bs
        go bs (TyApp e' _) = case go [] e' of
          []  -> bs
          bs' -> init bs' ++ bs
        go bs _ = bs

specialise' _ _ _ _ctx _ (appE,args,ticks) (Left specArg) = do
  -- Create binders and variable references for free variables in 'specArg'
  let (specBndrs,specVars) = specArgBndrsAndVars (Left specArg)
  -- Create specialized function
      newBody = mkAbstraction specArg specBndrs
  -- See if there's an existing binder that's alpha-equivalent to the
  -- specialized function
  existing <- filterUniqMap ((`aeqTerm` newBody) . bindingTerm) <$> Lens.use bindings
  -- Create a new function if an alpha-equivalent binder doesn't exist
  newf <- case eltsUniqMap existing of
    [] -> do (cf,sp) <- Lens.use curFun
             mkFunction (appendToName (varName cf) "_specF")
                        sp
#if MIN_VERSION_ghc(8,4,1)
                        NoUserInline
#else
                        EmptyInlineSpec
#endif
                        newBody
    (b:_) -> return (bindingId b)
  -- Create specialized argument
  let newArg  = Left $ mkApps (Var newf) specVars
  -- Use specialized argument
  let newExpr = mkApps (mkTicks appE ticks) (args ++ [newArg])
  changed newExpr

specialise' _ _ _ _ e _ _ = return e

normalizeTermTypes :: TyConMap -> Term -> Term
normalizeTermTypes tcm e = case e of
  Cast e' ty1 ty2 -> Cast (normalizeTermTypes tcm e') (normalizeType tcm ty1) (normalizeType tcm ty2)
  Var v -> Var (normalizeId tcm v)
  -- TODO other terms?
  _ -> e

normalizeId :: TyConMap -> Id -> Id
normalizeId tcm v@(Id {}) = v {varType = normalizeType tcm (varType v)}
normalizeId _   tyvar     = tyvar

-- Note [Collect free-variables in an insertion-ordered set]
--
-- In order for the specialization cache to work, 'specArgBndrsAndVars' should
-- yield (alpha equivalent) results for the same specialization. While collecting
-- free variables in a given term or type it should therefore keep a stable
-- ordering based on the order in which it finds free vars. To see why,
-- consider the following two pseudo-code calls to 'specialise':
--
--     specialise {f ('a', x[123], y[456])}
--     specialise {f ('b', x[456], y[123])}
--
-- Collecting the binders in a VarSet would yield the following (unique ordered)
-- sets:
--
--     {x[123], y[456]}
--     {y[123], x[456]}
--
-- ..and therefore breaking specializing caching. We now track them in insert-
-- ordered sets, yielding:
--
--     {x[123], y[456]}
--     {x[456], y[123]}
--

-- | Create binders and variable references for free variables in 'specArg'
specArgBndrsAndVars
  :: Either Term Type
  -> ([Either Id TyVar], [Either Term Type])
specArgBndrsAndVars specArg =
  -- See Note [Collect free-variables in an insertion-ordered set]
  let unitFV :: Var a -> Const (OSet.OLSet TyVar, OSet.OLSet Id) (Var a)
      unitFV v@(Id {}) = Const (mempty, coerce (OSet.singleton (coerce v)))
      unitFV v@(TyVar {}) = Const (coerce (OSet.singleton (coerce v)), mempty)

      (specFTVs,specFVs) = case specArg of
        Left tm  -> (OSet.toListL *** OSet.toListL) . getConst $
                    Lens.foldMapOf freeLocalVars unitFV tm
        Right ty -> (eltsUniqSet (Lens.foldMapOf typeFreeVars unitUniqSet ty),[] :: [Id])

      specTyBndrs = map Right specFTVs
      specTmBndrs = map Left  specFVs

      specTyVars  = map (Right . VarTy) specFTVs
      specTmVars  = map (Left . Var) specFVs

  in  (specTyBndrs ++ specTmBndrs,specTyVars ++ specTmVars)

-- | Evaluate an expression to weak-head normal form (WHNF), and apply a
-- transformation on the expression in WHNF.
whnfRW
  :: Bool
  -- ^ Whether the expression we're reducing to WHNF is the subject of a
  -- case expression.
  -> TransformContext
  -> Term
  -> Rewrite extra
  -> RewriteMonad extra Term
whnfRW _isSubj ctx@(TransformContext is0 _) e rw = do
  tcm <- Lens.view tcCache
  bndrs <- Lens.use bindings
  eval <- Lens.view evaluator
  ids <- Lens.use uniqSupply
  let (ids1,ids2) = splitSupply ids
  uniqSupply Lens..= ids2
  gh <- Lens.use globalHeap

#if EXPERIMENTAL_EVALUATOR
  ri <- Lens.view recInfo
  fuel <- Lens.view fuelLimit
  (fn, _) <- Lens.use curFun

  case runEval (mkGlobalEnv fn bndrs ri fuel gh tcm is0 ids1) (evaluateNf eval e) of
    (!e', !env') -> do
      globalHeap Lens..= genvPrimsIO env'
      rw ctx (asTerm e')
#else
  case whnf' eval bndrs tcm gh ids1 is0 _isSubj e of
    (!gh1,ph,v) -> do
      globalHeap Lens..= gh1
      bindPureHeap tcm ph rw ctx v
#endif
{-# SCC whnfRW #-}

#if !EXPERIMENTAL_EVALUATOR
-- | Binds variables on the PureHeap over the result of the rewrite
--
-- To prevent unnecessary rewrites only do this when rewrite changed something.
bindPureHeap
  :: TyConMap
  -> PureHeap
  -> Rewrite extra
  -> Rewrite extra
bindPureHeap tcm heap rw ctx0@(TransformContext is0 hist) e = do
  (e1, Monoid.getAny -> hasChanged) <- Writer.listen $ rw ctx e
  if hasChanged && not (null bndrs) then do
    -- The evaluator results are post-processed with two operations:
    --
    --   1. Inline work free binders. We've seen cases in the wild† where the
    --      evaluator (or rather, 'bindPureHeap') would let-bind work-free
    --      binders that were crucial for eliminating case constructs. If these
    --      case constructs were used in a self-referential (but terminating)
    --      manner, Clash would get stuck in an infinite loop. The proper
    --      solution would be to use 'isWorkFree', instead of 'isWorkFreeIsh',
    --      in 'bindConstantVar' such that these work free constructs would get
    --      inlined again. However, this incurs a great performance penalty so
    --      we opt to prevent the evaluator from introducing this situation in
    --      the first place.
    --
    --      I'd like to stress that this is not a proper solution though, as GHC
    --      might produce a similar situation. We plan on properly solving this
    --      by eliminating the current lift/bind/eval strategy, instead replacing
    --      it by a partial evaluator‡.
    --
    --   2. Remove any unused let-bindings. Similar to (1), we risk Clash getting
    --      stuck in an infinite loop if we don't remove unused (eliminated by
    --      evaluation!) binders.
    --
    -- † https://github.com/clash-lang/clash-compiler/pull/1354#issuecomment-635430374
    -- ‡ https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/supercomp-by-eval.pdf
    inlineBinders inlineTest ctx0 (Letrec bndrs e1) >>= \case
      e2@(Letrec bnders1 e3) ->
        pure (fromMaybe e2 (removeUnusedBinders bnders1 e3))
      e2 ->
        pure e2
  else
    return e1
  where
    heapIds = map fst bndrs
    is1 = extendInScopeSetList is0 heapIds
    ctx = TransformContext is1 (LetBody heapIds : hist)

    bndrs = map toLetBinding $ toListUniqMap heap

    toLetBinding :: (Unique,Term) -> LetBinding
    toLetBinding (uniq,term) = (nm, term)
      where
        ty = termType tcm term
        nm = mkLocalId ty (mkUnsafeSystemName "x" uniq) -- See [Note: Name re-creation]

    inlineTest _ (_, stripTicks -> e_) = isWorkFree e_
#endif

-- | Remove unused binders in given let-binding. Returns /Nothing/ if no unused
-- binders were found.
removeUnusedBinders
  :: [LetBinding]
  -> Term
  -> Maybe Term
removeUnusedBinders binds body =
  case eltsVarEnv used of
    [] -> Just body
    qqL | not (List.equalLength qqL binds)
        -> Just (Letrec qqL body)
        | otherwise
        -> Nothing
 where
  bodyFVs = Lens.foldMapOf freeLocalIds unitVarSet body
  used = List.foldl' collectUsed emptyVarEnv (eltsVarSet bodyFVs)
  bindsEnv = mkVarEnv (map (\(x,e0) -> (x,(x,e0))) binds)

  collectUsed env v =
    if v `elemVarEnv` env then
      env
    else
      case lookupVarEnv v bindsEnv of
        Just (x,e0) ->
          let eFVs = Lens.foldMapOf freeLocalIds unitVarSet e0
          in  List.foldl' collectUsed
                          (extendVarEnv x (x,e0) env)
                          (eltsVarSet eFVs)
        Nothing -> env

