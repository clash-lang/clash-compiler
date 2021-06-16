{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021     , QBayLogic B.V.
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

import           Control.Concurrent.Supply   (splitSupply)
import           Control.DeepSeq
import           Control.Exception           (throw)
import           Control.Lens ((%=), (+=), (^.))
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail          (MonadFail)
#endif
import qualified Control.Monad.State.Strict  as State
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

import           System.IO.Unsafe            (unsafePerformIO)

import           Data.Binary                 (encode)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.Basic             (InlineSpec (..))
#else
import           BasicTypes                  (InlineSpec (..))
#endif

#if EXPERIMENTAL_EVALUATOR
import           Clash.Core.PartialEval
import           Clash.Core.PartialEval.NormalForm
#else
import           Clash.Core.Evaluator.Types  (PureHeap, whnf')
#endif

import           Clash.Core.FreeVars
  (freeLocalVars, localIdDoesNotOccurIn, localIdOccursIn,
   termFreeVars', freeLocalIds)
import           Clash.Core.Name
import           Clash.Core.Pretty           (showPpr)
import           Clash.Core.Subst
  (substTmEnv, aeqTerm, aeqType, extendIdSubst, mkSubst, substTm)
import           Clash.Core.Term
import           Clash.Core.TermInfo
import           Clash.Core.TyCon            (TyConMap)
import           Clash.Core.Type             (Type (..), normalizeType, typeKind)
import           Clash.Core.Var
  (Id, IdScope (..), TyVar, Var (..), mkGlobalId, mkLocalId, mkTyVar)
import           Clash.Core.VarEnv
  (InScopeSet, extendInScopeSetList, mkInScopeSet,
   uniqAway, uniqAway', mapVarEnv, eltsVarEnv, unitVarSet, emptyVarEnv,
   mkVarEnv, eltsVarSet, elemVarEnv, lookupVarEnv, extendVarEnv)
import           Clash.Debug
import           Clash.Driver.Types
  (TransformationInfo(..), DebugOpts(..), BindingMap, Binding(..), IsPrim(..),
  hasDebugInfo, isDebugging)
import           Clash.Netlist.Util          (representableType)
import           Clash.Pretty                (clashPretty, showDoc)
import           Clash.Rewrite.Types
import           Clash.Rewrite.WorkFree
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
  opts <- Lens.view debugOpts
  traceIf (hasDebugInfo TryName s opts) ("Trying: " <> s) (pure ())

  (!expr1,anyChanged) <- Writer.listen (rewrite ctx expr0)
  let hasChanged = Monoid.getAny anyChanged
  Monad.when hasChanged (transformCounter += 1)

  -- NB: When -fclash-debug-history is on, emit binary data holding the recorded rewrite steps
  let rewriteHistFile = dbg_historyFile opts
  Monad.when (isJust rewriteHistFile && hasChanged) $ do
    (curBndr, _) <- Lens.use curFun
    let !_ = unsafePerformIO
             $ BS.appendFile (fromJust rewriteHistFile)
             $ BL.toStrict
             $ encode RewriteStep
                 { t_ctx    = tfContext ctx
                 , t_name   = s
                 , t_bndrS  = showPpr (varName curBndr)
                 , t_before = expr0
                 , t_after  = expr1
                 }
    return ()

  if isDebugging opts
    then applyDebug s expr0 hasChanged expr1
    else return expr1
{-# INLINE apply #-}

applyDebug
  :: String
  -- ^ Name of the transformation
  -> Term
  -- ^ Original expression
  -> Bool
  -- ^ Whether the rewrite indicated change
  -> Term
  -- ^ New expression
  -> RewriteMonad extra Term
applyDebug name exprOld hasChanged exprNew = do
  nTrans <- Lens.use transformCounter
  opts <- Lens.view debugOpts

  let from = fromMaybe 0 (dbg_transformationsFrom opts)
  let limit = fromMaybe maxBound (dbg_transformationsLimit opts)

  if | nTrans - from > limit ->
         error "-fclash-debug-transformations-limit exceeded"
     | nTrans <= from ->
         pure exprNew
     | otherwise ->
         go opts
 where
  go opts = traceIf (hasDebugInfo TryTerm name opts) ("Tried: " ++ name ++ " on:\n" ++ before) $ do
    nTrans <- pred <$> Lens.use transformCounter

    Monad.when (dbg_countTransformations opts && hasChanged) $ do
      transformCounters %= HashMap.insertWith (const succ) (Text.pack name) 1

    Monad.when (dbg_invariants opts && hasChanged) $ do
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

      -- TODO This check should not have the `hasDebugInfo` call in it, as
      -- setting dbg_invariants should be all that is necessary to check this.
      -- However, currently this error is very fragile, as Clash currently does
      -- not keep casts, so "illegally" changing between `Signal dom a` and `a`
      -- will trigger this error for many designs.
      --
      -- This should be changed when #1064 (PR to keep casts in core) is merged.
      Monad.when (hasDebugInfo AppliedTerm name opts && not (normalizeType tcm beforeTy `aeqType` normalizeType tcm afterTy)) $
        error ( concat [ $(curLoc)
                       , "Error when applying rewrite ", name
                       , " to:\n" , before
                       , "\nResult:\n" ++ after ++ "\n"
                       , "Changes type from:\n", showPpr beforeTy
                       , "\nto:\n", showPpr afterTy
                       ]
              )

    Monad.when (dbg_invariants opts && not hasChanged && not (exprOld `aeqTerm` exprNew)) $
      error $ $(curLoc) ++ "Expression changed without notice(" ++ name ++  "): before"
                        ++ before ++ "\nafter:\n" ++ after

    traceIf (hasDebugInfo AppliedName name opts && hasChanged) (name <> " {" <> show nTrans <> "}") $
      traceIf (hasDebugInfo AppliedTerm name opts && hasChanged) ("Changes when applying rewrite to:\n"
                        ++ before ++ "\nResult:\n" ++ after ++ "\n") $
        traceIf (hasDebugInfo TryTerm name opts && not hasChanged) ("No changes when applying rewrite "
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
  traceIf (dbg_countTransformations (_debugOpts r))
    ("Clash: Transformations:\n" ++ Text.unpack (showCounters (s' ^. transformCounters))) $
    traceIf (None < dbg_transformationInfo (_debugOpts r))
      ("Clash: Applied " ++ show (s' ^. transformCounter) ++ " transformations")
      a
  where
    (a,s',_) = runR m r s
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
                                    (Binding newBodyId sp NoUserInline IsFun newBody)
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
  (ty,body) `deepseq` bindings %= extendUniqMap vNm (Binding vId sp inl IsFun body)

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
  :: Bool
  -- ^ Whether the expression we're reducing to WHNF is the subject of a
  -- case expression.
  -> TransformContext
  -> Term
  -> Rewrite extra
  -> RewriteMonad extra Term
whnfRW isSubj ctx@(TransformContext is0 _) e rw = do
  tcm <- Lens.view tcCache
  bndrs <- Lens.use bindings
  eval <- Lens.view evaluator
  ids <- Lens.use uniqSupply
  let (ids1,ids2) = splitSupply ids
  uniqSupply Lens..= ids2

#if EXPERIMENTAL_EVALUATOR
  (i, _) <- Lens.use curFun
  heap <- Lens.use ioHeap
  addr <- Lens.use ioAddr
  fuel <- Lens.view fuelLimit
  let genv = mkGlobalEnv bndrs tcm is0 ids1 fuel heap addr

  case unsafePerformIO (nf eval genv isSubj i e) of
    (!e', !genv') -> do
      ioHeap Lens..= genvHeap genv'
      ioAddr Lens..= genvAddr genv'

      -- TODO I remain unsure about this. Do I want to use bindPureHeap?
      rw (ctx { tfInScope = genvInScope genv' }) e'
#else
  gh <- Lens.use globalHeap

  case whnf' eval bndrs tcm gh ids1 is0 isSubj e of
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
    bs <- Lens.use bindings
    inlineBinders (inlineTest bs) ctx0 (Letrec bndrs e1) >>= \case
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

    inlineTest bs _ (_, stripTicks -> e_) = isWorkFree workFreeBinders bs e_
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
