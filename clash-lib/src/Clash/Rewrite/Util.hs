{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2023, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

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

module Clash.Rewrite.Util
  ( module Clash.Rewrite.Util
  , module Clash.Rewrite.WorkFree
  ) where

import           Control.Concurrent.Lifted (myThreadId)
import qualified Control.Concurrent.MVar.Lifted as MVar
import           Control.DeepSeq
import           Control.Exception           (throw)
import           Control.Lens ((^.))
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import qualified Control.Monad.IO.Class      as Monad
import qualified Control.Monad.State.Strict  as State
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS as RWS
#else
import qualified Control.Monad.Trans.RWS.Strict as RWS
#endif
import qualified Control.Monad.Writer        as Writer
import           Data.Bifunctor              (second)
import           Data.Coerce                 (coerce)
import           Data.Functor.Const          (Const (..))
import qualified Data.HashMap.Strict         as HashMap
import           Data.List                   (group, partition, sort, sortOn)
import qualified Data.List                   as List
import qualified Data.List.Extra             as List
import           Data.List.Extra             (partitionM)
import           Data.Maybe
import qualified Data.Monoid                 as Monoid
import qualified Data.Set                    as Set
import qualified Data.Set.Lens               as Lens
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Binary                 (encode)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.Basic             (InlineSpec (..))
#else
import           BasicTypes                  (InlineSpec (..))
#endif

import           Clash.Core.Evaluator.Types  (PureHeap, whnf')
import           Clash.Core.FreeVars
  (freeLocalVars, termFreeVars', freeLocalIds, globalIdOccursIn)
import           Clash.Core.HasFreeVars      (elemFreeVars, notElemFreeVars)
import           Clash.Core.HasType
import           Clash.Core.Name
import           Clash.Core.Pretty           (showPpr)
import           Clash.Core.Subst
  (substTmEnv, aeqTerm, aeqType, extendIdSubst, mkSubst, substTm, eqTerm)
import           Clash.Core.Term
import           Clash.Core.TyCon            (TyConMap)
import           Clash.Core.Type             (Type (..), normalizeType)
import           Clash.Core.Var
  (Id, IdScope (..), TyVar, Var (..), mkGlobalId, mkLocalId, mkTyVar)
import           Clash.Core.VarEnv
  (InScopeSet, extendInScopeSet, extendInScopeSetList, mkInScopeSet, notElemInScopeSet,
   uniqAway, uniqAway', mapVarEnv, eltsVarEnv, unitVarSet, emptyVarEnv,
   mkVarEnv, eltsVarSet, elemVarEnv, lookupVarEnv, extendVarEnv, elemVarSet,
   differenceVarEnv)
import           Clash.Data.UniqMap (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Debug
import           Clash.Driver.Types
  (TransformationInfo(..), DebugOpts(..), BindingMap, Binding(..), IsPrim(..),
  ClashEnv(..), ClashOpts(..), hasDebugInfo, isDebugging)
import           Clash.Netlist.Util          (representableType)
import           Clash.Pretty                (clashPretty, showDoc)
import           Clash.Rewrite.Types
import           Clash.Rewrite.WorkFree
import           Clash.Unique
import           Clash.Util
import           Clash.Util.Eq               (fastEqBy)
import qualified Clash.Util.Interpolate as I
import           Clash.Util.Supply           (splitSupply)

-- | Lift an action working in the '_extra' state to the 'RewriteMonad'
zoomExtra :: State.StateT extra IO a -> RewriteMonad extra a
zoomExtra m = R . RWS.rwsT $ \_ s -> do
  (a, st') <- State.runStateT m (_extra s)
  pure (a, s { _extra = st' }, mempty)

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
    Let NonRec{} t -> findAccidentialShadows t
    Let (Rec bs) t -> findDups (map fst bs) ++ findAccidentialShadows t

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
  opts <- Lens.view debugOpts
  ioLockV <- Lens.use ioLock

  MVar.withMVar ioLockV $ \() ->
    traceWhen (hasDebugInfo TryName s opts) ("Trying: " <> s)

  (!expr1,anyChanged) <- Writer.listen (rewrite ctx expr0)
  let hasChanged = Monoid.getAny anyChanged

  Monad.when hasChanged $ do
    countersV <- Lens.use transformCounters
    MVar.modifyMVar_ countersV (pure . force . HashMap.insertWith (const succ) (Text.pack s) 1)

  -- NB: When -fclash-debug-history is on, emit binary data holding the recorded rewrite steps
  let rewriteHistFile = dbg_historyFile opts
  Monad.when (isJust rewriteHistFile && hasChanged) $ do
    thread <- myThreadId
    curFunsV <- Lens.use curFun

    MVar.withMVar curFunsV $ \curFuns ->
      case fst <$> HashMap.lookup thread curFuns of
        Just curBndr ->
          -- TODO Although we're locking access to the history file, entries
          -- may still be written to it interleaved by entity. I'm not sure if
          -- clash-term can handle this correctly...
          MVar.withMVar ioLockV $ \() ->
            Monad.liftIO
              . BS.appendFile (fromJust rewriteHistFile)
              . BL.toStrict
              $ encode RewriteStep
                         { t_ctx    = tfContext ctx
                         , t_name   = s
                         , t_bndrS  = showPpr (varName curBndr)
                         , t_before = expr0
                         , t_after  = expr1
                         }

        Nothing ->
          error "apply: Normalizing from an unknown thread"

  if isDebugging opts
    then do
      countersV <- Lens.use transformCounters
      nTrans <- sum <$> MVar.readMVar countersV
      applyDebug ctx s expr0 hasChanged expr1 nTrans
    else return expr1
{-# INLINE apply #-}

applyDebug
  :: TransformContext
  -> String
  -- ^ Name of the transformation
  -> Term
  -- ^ Original expression
  -> Bool
  -- ^ Whether the rewrite indicated change
  -> Term
  -- ^ New expression
  -> Word
  -> RewriteMonad extra Term
applyDebug ctx name exprOld hasChanged exprNew nTrans = do
  opts <- Lens.view debugOpts

  let from = fromMaybe 0 (dbg_transformationsFrom opts)
  let limit = fromMaybe maxBound (dbg_transformationsLimit opts)

  if | nTrans - from > limit -> do
         error "-fclash-debug-transformations-limit exceeded"
     | nTrans <= from -> do
         pure exprNew
     | otherwise ->
         go (pred nTrans) opts
 where
  go nTrans' opts = do
    ioLockV <- Lens.use ioLock

    MVar.withMVar ioLockV $ \() ->
      traceWhen (hasDebugInfo TryTerm name opts) ("Tried: " ++ name ++ " on:\n" ++ before)

    Monad.when (dbg_invariants opts && hasChanged) $ do
      tcm                  <- Lens.view tcCache
      let beforeTy          = inferCoreTypeOf tcm exprOld
          beforeFV          = Lens.setOf freeLocalVars exprOld
          afterTy           = inferCoreTypeOf tcm exprNew
          afterFV           = filterFVs (Lens.setOf freeLocalVars exprNew)
          newFV             = not (afterFV `Set.isSubsetOf` beforeFV)
          accidentalShadows = findAccidentialShadows exprNew
          -- see NOTE [Filter free variables]
          allowNewFVsFromCtx = name == "caseCon"
          filterFVs | allowNewFVsFromCtx = Set.filter notInCtx
                    | otherwise = id
          notInCtx v = notElemInScopeSet v (tfInScope ctx)

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

      -- TODO This check should be an error instead of a trace, however this is
      -- currently very fragile as Clash doesn't keep casts in core. This should
      -- be changed when #1064 is merged.
      Monad.when (hasDebugInfo AppliedTerm name opts && not (normalizeType tcm beforeTy `aeqType` normalizeType tcm afterTy)) $
        traceM ( concat [ $(curLoc)
                       , "Error when applying rewrite ", name
                       , " to:\n" , before
                       , "\nResult:\n" ++ after ++ "\n"
                       , "Changes type from:\n", showPpr beforeTy
                       , "\nto:\n", showPpr afterTy
                       ]
              )

    let exprNotEqual = not (fastEqBy eqTerm exprOld exprNew)
    Monad.when (dbg_invariants opts && not hasChanged && exprNotEqual) $
      error $ $(curLoc) ++ "Expression changed without notice(" ++ name ++  "): before"
                        ++ before ++ "\nafter:\n" ++ after

    MVar.withMVar ioLockV $ \() -> do
      traceWhen (hasDebugInfo AppliedName name opts && hasChanged) (name <> " {" <> show nTrans' <> "}")
      traceWhen (hasDebugInfo AppliedTerm name opts && hasChanged)
        ("Changes when applying rewrite to:\n" ++ before ++ "\nResult:\n" ++ after ++ "\n")
      traceWhen (hasDebugInfo TryTerm name opts && not hasChanged)
        ("No changes when applying rewrite " ++ name ++ " to:\n" ++ after ++ "\n")

    return exprNew
   where
    before = showPpr exprOld
    after  = showPpr exprNew

-- NOTE [Filter free variables]
-- Since [Give evaluator acces to inscope let-bindings #2571](https://github.com/clash-lang/clash-compiler/pull/2571)
-- the evaluator can rewrite expressions using let bindings from the 'TransformContext',
-- these bindings may reference other things bound in the context which weren't
-- in the expression before, and in doing so introduces new free variables and
-- fails this check for new free variables.
-- To prevent this we filter out all variables from bound in the context.
-- But only during a caseCon transformation, to not weaken this check unnecessarily.



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
                  -> IO a
runRewriteSession r s m = do
  (a, s', _) <- runR m r s
  MVar.withMVar (s' ^. transformCounters) $ \counters -> do
    MVar.withMVar (s' ^. ioLock) $ \() -> do
      traceWhen (dbg_countTransformations (opt_debug (envOpts (_clashEnv r))))
        ("Clash: Transformations:\n" ++ Text.unpack (showCounters counters))
      traceWhen (None < dbg_transformationInfo (opt_debug (envOpts (_clashEnv r))))
        ("Clash: Applied " ++ show (sum counters) ++ " transformations")

    pure a
  where
    showCounters =
      Text.unlines
        . map (\(nm,cnt) -> nm <> ": " <> Text.pack (show cnt))
        . sortOn snd
        . HashMap.toList

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
  :: (MonadUnique m)
  => InScopeSet
  -> TyConMap -- ^ TyCon cache
  -> Name a -- ^ Name of the new binder
  -> Term -- ^ Term to bind
  -> m Id
mkTmBinderFor is tcm name e =
  either id (error "mkTmBinderFor: Result is a TyVar")
    <$> mkBinderFor is tcm name (Left e)

-- | Make a new binder and variable reference for either a term or a type
mkBinderFor
  :: (MonadUnique m)
  => InScopeSet
  -> TyConMap -- ^ TyCon cache
  -> Name a -- ^ Name of the new binder
  -> Either Term Type -- ^ Type or Term to bind
  -> m (Either Id TyVar)
mkBinderFor is tcm name (Left term) = do
  name' <- cloneNameWithInScopeSet is name
  let ty = inferCoreTypeOf tcm term
  return (Left (mkLocalId ty (coerce name')))

mkBinderFor is tcm name (Right ty) = do
  name' <- cloneNameWithInScopeSet is name
  let ki = inferCoreKindOf tcm ty
  return (Right (mkTyVar ki (coerce name')))

-- | Inline the binders in a let-binding that have a certain property
inlineBinders
  :: (Term -> LetBinding -> RewriteMonad extra Bool)
  -- ^ Property test
  -> Rewrite extra
inlineBinders condition (TransformContext inScope0 _) expr@(Let (NonRec i x) res) = do
  inline <- condition expr (i, x)

  if inline && elemFreeVars i res then
    let inScope1 = extendInScopeSet inScope0 i
        subst = extendIdSubst (mkSubst inScope1) i x
     in changed (substTm "inlineBinders" subst res)
  else
    return expr

inlineBinders condition (TransformContext inScope0 _) expr@(Let (Rec xes) res) = do
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
  bndr `notElemFreeVars` e
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
    in  if x `elemFreeVars` e1 then
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
    if x `elemFreeVars` e2 then do
      curFunsV <- Lens.use curFun
      thread <- myThreadId
      Just (_,sp) <- MVar.withMVar curFunsV (pure . HashMap.lookup thread)
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

isFromInt :: Text -> Bool
isFromInt nm = nm == "Clash.Sized.Internal.BitVector.fromInteger##" ||
               nm == "Clash.Sized.Internal.BitVector.fromInteger#" ||
               nm == "Clash.Sized.Internal.Index.fromInteger#" ||
               nm == "Clash.Sized.Internal.Signed.fromInteger#" ||
               nm == "Clash.Sized.Internal.Unsigned.fromInteger#"

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
  let unitFV :: Var a -> Const (UniqMap TyVar,UniqMap Id) (Var a)
      unitFV v@(Id {})    = Const (UniqMap.empty,UniqMap.singletonUnique (coerce v))
      unitFV v@(TyVar {}) = Const (UniqMap.singletonUnique (coerce v),UniqMap.empty)

      interesting :: Var a -> Bool
      interesting Id {idScope = GlobalId} = False
      interesting v@(Id {idScope = LocalId}) = varUniq v /= varUniq var
      interesting _ = True

      (boundFTVsSet,boundFVsSet) =
        getConst (Lens.foldMapOf (termFreeVars' interesting) unitFV e)
      boundFTVs = UniqMap.elems boundFTVsSet
      boundFVs  = UniqMap.elems boundFVsSet

  -- Make a new global ID
  tcm       <- Lens.view tcCache
  let newBodyTy = inferCoreTypeOf tcm $ mkTyLams (mkLams e boundFVs) boundFTVs
  curFunsV <- Lens.use curFun
  thread <- myThreadId
  Just (cf,sp) <- MVar.withMVar curFunsV (pure . HashMap.lookup thread)
  bindersV <- Lens.use bindings
  binders <- MVar.takeMVar bindersV
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
  let aeqExisting = UniqMap.elems . UniqMap.filter ((`aeqTerm` newBody) . bindingTerm) $ binders
  case aeqExisting of
    -- If it doesn't, create a new binder
    [] -> do -- Add the created function to the list of global bindings
             let r = newBodyId `globalIdOccursIn` newBody
             MVar.putMVar bindersV $
               UniqMap.insert
                 newBodyNm
                 -- We mark this function as internal so that it can be inlined
                 -- at the very end of the normalization pipeline as part of the
                 -- flattening pass. We don't inline right away because we are
                 -- lifting this function at this moment for a reason!
                 -- (termination, CSE and DEC opportunities, etc.)
#if MIN_VERSION_ghc(9,2,0)
                 (Binding newBodyId sp NoUserInlinePrag IsFun newBody r)
#else
                 (Binding newBodyId sp NoUserInline IsFun newBody r)
#endif
                 binders

             return (var, newExpr)
    -- If it does, use the existing binder
    (b:_) -> do
      let newExpr' = mkTmApps
                      (mkTyApps (Var $ bindingId b)
                                (map VarTy boundFTVs))
                      (map Var boundFVs)
      MVar.putMVar bindersV binders
      return (var, newExpr')

liftBinding _ = error $ $(curLoc) ++ "liftBinding: invalid core, expr bound to tyvar"

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
  let bodyTy = inferCoreTypeOf tcm body
  bindersV <- Lens.use bindings

  MVar.modifyMVar bindersV $ \binders -> do
    bodyNm <- cloneNameWithBindingMap binders bndrNm
    let vId = mkGlobalId bodyTy bodyNm
        r = vId `globalIdOccursIn` body
        bind = Binding vId sp inl IsFun body r
        binders' = UniqMap.insert vId bind binders

    bodyTy `deepseq` body `deepseq` binders' `seq` pure (binders', vId)

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
  return (uniqAway' (`UniqMap.elem` binders) i (setUnique nm i))

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
                             <*> pure (inferCoreTypeOf tcm tm))

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

normalizeTermTypes :: TyConMap -> Term -> Term
normalizeTermTypes tcm e = case e of
  Cast e' ty1 ty2 -> Cast (normalizeTermTypes tcm e') (normalizeType tcm ty1) (normalizeType tcm ty2)
  Var v -> Var (normalizeId tcm v)
  -- TODO other terms?
  _ -> e

normalizeId :: TyConMap -> Id -> Id
normalizeId tcm v@(Id {}) = v {varType = normalizeType tcm (varType v)}
normalizeId _   tyvar     = tyvar

-- | Evaluate an expression to weak-head normal form (WHNF), and apply a
-- transformation on the expression in WHNF.
whnfRW
  :: forall extra
   . Bool
  -- ^ Whether the expression we're reducing to WHNF is the subject of a
  -- case expression.
  -> TransformContext
  -> Term
  -> Rewrite extra
  -> RewriteMonad extra Term
whnfRW isSubj (TransformContext is0 hist) e0 rw = do
  tcm <- Lens.view tcCache
  eval <- Lens.view evaluator

  bndrsV <- Lens.use bindings
  ids <- Lens.use uniqSupply
  ghV <- Lens.use globalHeap

  bndrs <- MVar.takeMVar bndrsV
  gh <- MVar.takeMVar ghV

  let (ids1,ids2) = splitSupply ids
  uniqSupply Lens..= ids2
  let lh = localBinders mempty hist

  case whnf' eval bndrs lh tcm gh ids1 is0 isSubj e0 of
    (!gh1,ph,v) -> do
      let result = bindPureHeap tcm bndrs (ph `differenceVarEnv` lh) v
      MVar.putMVar bndrsV bndrs
      MVar.putMVar ghV gh1
      result
 where
  localBinders acc [] = acc
  localBinders !acc (h:hs) = case h of
    LetBody ls -> localBinders (acc <> mkVarEnv ls) hs
    _ -> localBinders acc hs

  -- | Binds variables on the PureHeap over the result of the rewrite
  -- To prevent unnecessary rewrites only do this when rewrite changed something.
  bindPureHeap
    :: TyConMap
    -> BindingMap
    -> PureHeap
    -> Term
    -> RewriteMonad extra Term
  bindPureHeap tcm bs heap e1 = do
    (e2, Monoid.getAny -> hasChanged) <- Writer.listen $ rw ctx e1
    if hasChanged && not (null letBndrs) then do
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
      inlineBinders inlineTest ctx (Letrec letBndrs e2) >>= \case
        e3@(Let bnders1 e4) ->
          pure (fromMaybe e3 (removeUnusedBinders bnders1 e4))
        e3 ->
          pure e3
    else
      return e2
   where
    heapIds = map fst letBndrs
    is1 = extendInScopeSetList is0 heapIds
    ctx = TransformContext is1 (LetBody letBndrs : hist)

    letBndrs = map toLetBinding $ UniqMap.toList heap

    toLetBinding :: (Unique,Term) -> LetBinding
    toLetBinding (uniq,term) = (nm, term)
      where
        ty = inferCoreTypeOf tcm term
        nm = mkLocalId ty (mkUnsafeSystemName "x" uniq) -- See [Note: Name re-creation]

    inlineTest _ (_, stripTicks -> e_) = isWorkFree workFreeBinders bs e_
{-# SCC whnfRW #-}

-- | Remove unused binders in given let-binding. Returns /Nothing/ if no unused
-- binders were found.
removeUnusedBinders
  :: Bind Term
  -> Term
  -> Maybe Term
removeUnusedBinders (NonRec i _) body =
  let bodyFVs = Lens.foldMapOf freeLocalIds unitVarSet body
   in if i `elemVarSet` bodyFVs then Nothing else Just body

removeUnusedBinders (Rec binds) body =
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
