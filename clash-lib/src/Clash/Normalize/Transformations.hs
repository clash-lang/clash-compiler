{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Transformations of the Normalization process
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Normalize.Transformations
  ( appProp
  , caseLet
  , caseCon
  , caseCase
  , inlineNonRep
  , inlineOrLiftNonRep
  , typeSpec
  , nonRepSpec
  , etaExpansionTL
  , nonRepANF
  , bindConstantVar
  , constantSpec
  , makeANF
  , deadCode
  , topLet
  , recToLetRec
  , inlineWorkFree
  , inlineHO
  , inlineSmall
  , simpleCSE
  , reduceConst
  , reduceNonRepPrim
  , caseFlat
  , disjointExpressionConsolidation
  , removeUnusedExpr
  , inlineCleanup
  , flattenLet
  , splitCastWork
  , inlineCast
  , caseCast
  , letCast
  , eliminateCastCast
  , argCastSpec
  )
where

import           Control.Concurrent.Supply   (splitSupply)
import           Control.Exception           (throw)
import           Control.Lens                (_2)
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.State         (StateT (..), modify)
import           Control.Monad.Writer        (censor, lift, listen)
import           Control.Monad.Trans.Except  (runExcept)
import           Data.Bits                   ((.&.), complement)
import           Data.Coerce                 (coerce)
import qualified Data.Either                 as Either
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.List                   as List
import qualified Data.Maybe                  as Maybe
import qualified Data.Monoid                 as Monoid
import qualified Data.Primitive.ByteArray    as BA
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Vector.Primitive       as PV
import           Debug.Trace                 (trace)
import           GHC.Integer.GMP.Internals   (Integer (..), BigNat (..))

import           BasicTypes                  (InlineSpec (..))

import           Clash.Core.DataCon          (DataCon (..))
import           Clash.Core.Evaluator        (PureHeap, whnf')
import           Clash.Core.Name
  (Name (..), NameSort (..), mkUnsafeSystemName)
import           Clash.Core.FreeVars
  (idOccursIn, idsDoNotOccurIn, termFreeIds, termFreeTyVars, typeFreeVars, varsDoNotOccurIn)
import           Clash.Core.Literal          (Literal (..))
import           Clash.Core.Pretty           (showPpr)
import           Clash.Core.Subst
  (substTm, mkSubst, extendIdSubst, extendIdSubstList, extendTvSubst, extendTvSubstList, freshenTm)
import           Clash.Core.Term             (LetBinding, Pat (..), Term (..))
import           Clash.Core.Type             (TypeView (..), applyFunTy,
                                              isPolyFunCoreTy,
                                              normalizeType,
                                              splitFunTy, typeKind,
                                              tyView, undefinedTy)
import           Clash.Core.TyCon            (TyConMap, tyConDataCons)
import           Clash.Core.Util
  (collectArgs, isClockOrReset, isCon, isFun, isLet, isPolyFun, isPrim,
   isSignalType, isVar, mkApps, mkLams, mkVec, piResultTy, termSize, termType,
   tyNatSize, patVars)
import           Clash.Core.Var              (Id, Var (..), mkId)
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, VarSet, elemInScopeSet, notElemInScopeSet, elemVarSet,
   emptyVarEnv, emptyVarSet, extendInScopeSet, extendInScopeSetList, lookupVarEnv,
   notElemVarSet, unionVarEnvWith, unionVarSet, unionInScope, unitVarEnv,
   unitVarSet, mkVarSet, mkInScopeSet, uniqAway)
import           Clash.Driver.Types          (DebugLevel (..))
import           Clash.Netlist.BlackBox.Util (usedArguments)
import           Clash.Netlist.Types         (HWType (..), FilteredHWType(..))
import           Clash.Netlist.Util
  (coreTypeToHWType, representableType, splitNormalized)
import           Clash.Normalize.DEC
import           Clash.Normalize.PrimitiveReductions
import           Clash.Normalize.Types
import           Clash.Normalize.Util
import           Clash.Primitives.Types
  (Primitive(..), PrimMap, TemplateKind(TExpr))
import           Clash.Rewrite.Combinators
import           Clash.Rewrite.Types
import           Clash.Rewrite.Util
import           Clash.Unique
  (Unique, lookupUniqMap, toListUniqMap)
import           Clash.Util

inlineOrLiftNonRep :: NormRewrite
inlineOrLiftNonRep = inlineOrLiftBinders nonRepTest inlineTest
  where
    nonRepTest :: (Id, Term) -> RewriteMonad extra Bool
    nonRepTest (Id {varType = ty}, _)
      = not <$> (representableType <$> Lens.view typeTranslator
                                   <*> Lens.view customReprs
                                   <*> pure False
                                   <*> Lens.view tcCache
                                   <*> pure ty)
    nonRepTest _ = return False

    inlineTest :: Term -> (Id, Term) -> RewriteMonad extra Bool
    inlineTest e (id_, e')
      = not . or <$> sequence -- We do __NOT__ inline:
              [ -- 1. recursive let-binders
                pure (id_ `idOccursIn` e')
                -- 2. join points (which are not void-wrappers)
              , pure (isJoinPointIn id_ e && not (isVoidWrapper e'))
                -- 3. binders that are used more than once in the body, because
                --    it makes CSE a whole lot more difficult.
              , (>1) <$> freeOccurances
              ]
      where
        -- The number of free occurrences of the binder in the entire
        -- let-expression
        freeOccurances :: RewriteMonad extra Int
        freeOccurances = case e of
          Letrec _ res -> do
            Monoid.getSum <$>
              (Lens.foldMapOf <$> localFreeIds
                              <*> pure (\i -> if i == id_
                                                 then Monoid.Sum 1
                                                 else Monoid.Sum 0)
                              <*> pure res)
          _ -> return 0

{- [Note] join points and void wrappers
Join points are functions that only occur in tail-call positions within an
expression, and only when they occur in a tail-call position more than once.

Normally bindNonRep binds/inlines all non-recursive local functions. However,
doing so for join points would significantly increase compilation time, so we
avoid it. The only exception to this rule are so-called void wrappers. Void
wrappers are functions of the form:

> \(w :: Void) -> f a b c

i.e. a wrapper around the function 'f' where the argument 'w' is not used. We
do bind/line these join-points because these void-wrappers interfere with the
'disjoint expression consolidation' (DEC) and 'common sub-expression elimination'
(CSE) transformation, sometimes resulting in circuits that are twice as big
as they'd need to be.
-}

-- | Specialize functions on their type
typeSpec :: NormRewrite
typeSpec ctx e@(TyApp e1 ty)
  | (Var {},  args) <- collectArgs e1
  , null $ Lens.toListOf typeFreeVars ty
  , (_, []) <- Either.partitionEithers args
  = specializeNorm ctx e

typeSpec _ e = return e

-- | Specialize functions on their non-representable argument
nonRepSpec :: NormRewrite
nonRepSpec ctx e@(App e1 e2)
  | (Var {}, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  = do tcm <- Lens.view tcCache
       let e2Ty = termType tcm e2
       localVar <- isNonGlobalVar e2
       nonRepE2 <- not <$> (representableType <$> Lens.view typeTranslator
                                              <*> Lens.view customReprs
                                              <*> pure False
                                              <*> Lens.view tcCache
                                              <*> pure e2Ty)
       if nonRepE2 && not localVar
         then do
           e2' <- inlineInternalSpecialisationArgument e2
           specializeNorm ctx (App e1 e2')
         else return e
  where
    -- | If the argument on which we're specialising ia an internal function,
    -- one created by the compiler, then inline that function before we
    -- specialise.
    --
    -- We need to do this because otherwise the specialisation history won't
    -- recognize the new specialisation argument as something the function has
    -- already been specialised on
    inlineInternalSpecialisationArgument
      :: Term
      -> NormalizeSession Term
    inlineInternalSpecialisationArgument app
      | (Var f,fArgs) <- collectArgs app
      = do
        fTmM <- lookupVarEnv f <$> Lens.use bindings
        case fTmM of
          Just (fNm,_,_,tm)
            | nameSort (varName fNm) == Internal
            -> do
              tm' <- censor (const mempty) (bottomupR appProp ctx (mkApps tm fArgs))
              return tm'
          _ -> return app
      | otherwise = return app

nonRepSpec _ e = return e

-- | Lift the let-bindings out of the subject of a Case-decomposition
caseLet :: NormRewrite
caseLet _ (Case (Letrec xes e) ty alts) =
  changed (Letrec xes (Case e ty alts))

caseLet _ e = return e

-- | Move a Case-decomposition from the subject of a Case-decomposition to the alternatives
caseCase :: NormRewrite
caseCase _ e@(Case (Case scrut alts1Ty alts1) alts2Ty alts2)
  = do
    ty1Rep <- representableType <$> Lens.view typeTranslator
                                <*> Lens.view customReprs
                                <*> pure False
                                <*> Lens.view tcCache
                                <*> pure alts1Ty
    if not ty1Rep
      then let newAlts = map (second (\altE -> Case altE alts2Ty alts2)) alts1
           in  changed $ Case scrut alts2Ty newAlts
      else return e

caseCase _ e = return e

-- | Inline function with a non-representable result if it's the subject
-- of a Case-decomposition
inlineNonRep :: NormRewrite
inlineNonRep (TransformContext localScope _) e@(Case scrut altsTy alts)
  | (Var f, args) <- collectArgs scrut
  , f `notElemInScopeSet` localScope
  = do
    (cf,_)    <- Lens.use curFun
    isInlined <- zoomExtra (alreadyInlined f cf)
    limit     <- Lens.use (extra.inlineLimit)
    tcm       <- Lens.view tcCache
    let scrutTy = termType tcm scrut
        noException = not (exception tcm scrutTy)
    if noException && (Maybe.fromMaybe 0 isInlined) > limit
      then do
        let ty = termType tcm scrut
        traceIf True (concat [$(curLoc) ++ "InlineNonRep: " ++ show f
                             ," already inlined " ++ show limit ++ " times in:"
                             , show cf
                             , "\nType of the subject is: " ++ showPpr ty
                             , "\nFunction " ++ show cf
                             , " will not reach a normal form, and compilation"
                             , " might fail."
                             , "\nRun with '-fclash-inline-limit=N' to increase"
                             , " the inlining limit to N."
                             ])
                     (return e)
      else do
        bodyMaybe   <- lookupVarEnv f <$> Lens.use bindings
        nonRepScrut <- not <$> (representableType <$> Lens.view typeTranslator
                                                  <*> Lens.view customReprs
                                                  <*> pure False
                                                  <*> Lens.view tcCache
                                                  <*> pure scrutTy)
        case (nonRepScrut, bodyMaybe) of
          (True,Just (_,_,_,scrutBody)) -> do
            Monad.when noException (zoomExtra (addNewInline f cf))
            changed $ Case (mkApps scrutBody args) altsTy alts
          _ -> return e
  where
    exception tcm ((tyView . typeKind tcm) -> TyConApp (nameOcc -> "GHC.Types.Constraint") _) = True
    exception _ _ = False

inlineNonRep _ e = return e

-- | Specialize a Case-decomposition (replace by the RHS of an alternative) if
-- the subject is (an application of) a DataCon; or if there is only a single
-- alternative that doesn't reference variables bound by the pattern.
--
-- Note [CaseCon deshadow]
--
-- Imagine:
--
-- @
-- case D (f a b) (g x y) of
--   D a b -> h a
-- @
--
-- rewriting this to:
--
-- @
-- let a = f a b
-- in  h a
-- @
--
-- is very bad because the newly introduced let-binding now captures the free
-- variable 'a' in 'f a b'.
--
-- instead me must rewrite to:
--
-- @
-- let a1 = f a b
-- in  h a1
-- @
caseCon :: NormRewrite
caseCon (TransformContext is0 _) (Case scrut ty alts)
  | (Data dc, args) <- collectArgs scrut
  = case List.find (equalCon dc . fst) alts of
      Just (DataPat _ tvs xs, e) -> do
        let is1 = extendInScopeSetList (extendInScopeSetList is0 tvs) xs
        is2 <- unionInScope is1 <$> Lens.use globalInScope
        let fvs = Lens.foldMapOf termFreeIds unitVarSet e
            (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                      $ zip xs (Either.lefts args)
            e' = case binds of
                  [] -> e
                  _  ->
                    -- See Note [CaseCon deshadow]
                    let ((is3,substIds),binds') = List.mapAccumL newBinder
                                                    (is2,[]) binds
                        subst = extendIdSubstList (mkSubst is3) substIds
                    in  Letrec binds' (substTm "caseCon0" subst e)
        let subst = extendTvSubstList (mkSubst is2)
                  $ zip tvs (drop (length (dcUnivTyVars dc)) (Either.rights args))
        changed (substTm "caseCon1" subst e')
      _ -> case alts of
             ((DefaultPat,e):_) -> changed e
             _ -> changed (mkApps (Prim "Clash.Transformations.undefined" undefinedTy) [Right ty])
  where
    equalCon dc (DataPat dc' _ _) = dcTag dc == dcTag dc'
    equalCon _  _                 = False

    newBinder (isN0,substN) (x,arg) =
      let x'   = uniqAway isN0 x
          isN1 = extendInScopeSet isN0 x'
      in  ((isN1,(x,Var x'):substN),(x',arg))

caseCon _ c@(Case (Literal l) _ alts) = case List.find (equalLit . fst) alts of
    Just (LitPat _,e) -> changed e
    _ -> matchLiteralContructor c l alts
  where
    equalLit (LitPat l')     = l == l'
    equalLit _               = False

caseCon ctx@(TransformContext is0 _) e@(Case subj ty alts)
  | (Prim _ _,_) <- collectArgs subj = do
    reprs <- Lens.view customReprs
    tcm <- Lens.view tcCache
    bndrs <- Lens.use bindings
    primEval <- Lens.view evaluator
    ids <- Lens.use uniqSupply
    let (ids1,ids2) = splitSupply ids
    uniqSupply Lens..= ids2
    gh <- Lens.use globalHeap
    lvl <- Lens.view dbgLevel
    is1 <- unionInScope is0 <$> Lens.use globalInScope
    case whnf' primEval bndrs tcm gh ids1 is1 True subj of
      (gh',ph',v) -> do
        globalHeap Lens..= gh'
        bindPureHeap ctx tcm ph' $ \ctx' -> case v of
          Literal l -> caseCon ctx' (Case (Literal l) ty alts)
          subj' -> case collectArgs subj' of
            (Data _,_) -> caseCon ctx' (Case subj' ty alts)
#if MIN_VERSION_ghc(8,2,2)
            (Prim nm ty',_:msgOrCallStack:_)
              | nm == "Control.Exception.Base.absentError" ->
                let e' = mkApps (Prim nm ty') [Right ty,msgOrCallStack]
                in  changed e'
#endif

            (Prim nm ty',repTy:_:msgOrCallStack:_)
              | nm `elem` ["Control.Exception.Base.patError"
#if !MIN_VERSION_ghc(8,2,2)
                          ,"Control.Exception.Base.absentError"
#endif
                          ,"GHC.Err.undefined"] ->
                let e' = mkApps (Prim nm ty') [repTy,Right ty,msgOrCallStack]
                in  changed e'
            (Prim nm ty',[_])
              | nm `elem` ["Clash.Transformations.undefined"] ->
                let e' = mkApps (Prim nm ty') [Right ty]
                in changed e'
            (Prim nm _,[_])
              | nm `elem` ["EmptyCase"] ->
                changed (Prim nm ty)
            _ -> do
              let subjTy = termType tcm subj
              tran <- Lens.view typeTranslator
              case coreTypeToHWType tran reprs tcm subjTy of
                Right (FilteredHWType (Void (Just hty)) _areVoids)
                  | hty `elem` [BitVector 0, Unsigned 0, Signed 0, Index 1]
                  -> caseCon ctx' (Case (Literal (IntegerLiteral 0)) ty alts)
                _ -> traceIf (lvl > DebugNone && isConstant subj)
                       ("Irreducible constant as case subject: " ++ showPpr subj ++ "\nCan be reduced to: " ++ showPpr subj')
                       (caseOneAlt e)

caseCon ctx e@(Case subj ty alts) = do
  reprs <- Lens.view customReprs
  tcm <- Lens.view tcCache
  let subjTy = termType tcm subj
  tran <- Lens.view typeTranslator
  case coreTypeToHWType tran reprs tcm subjTy of
    Right (FilteredHWType (Void (Just hty)) _areVoids)
      | hty `elem` [BitVector 0, Unsigned 0, Signed 0, Index 1]
      -> caseCon ctx (Case (Literal (IntegerLiteral 0)) ty alts)
    _ -> caseOneAlt e

caseCon _ e = return e


-- | Binds variables on the PureHeap over the result of the rewrite
--
-- To prevent unnecessary rewrites only do this when rewrite changed something.
bindPureHeap
  :: TransformContext
  -> TyConMap
  -> PureHeap
  -> (TransformContext -> RewriteMonad extra Term)
  -> RewriteMonad extra Term
bindPureHeap (TransformContext is0 ctxs) tcm heap rw = do
  (e, Monoid.getAny -> hasChanged) <- listen $ rw ctx'
  if hasChanged && not (null bndrs)
    then return $ Letrec bndrs e
    else return e
  where
    bndrs = map toLetBinding $ toListUniqMap heap
    heapIds = map fst bndrs
    is1 = extendInScopeSetList is0 heapIds
    ctx' = TransformContext is1 (LetBody heapIds : ctxs)

    toLetBinding :: (Unique,Term) -> LetBinding
    toLetBinding (uniq,term) = (nm, term)
      where
        ty = termType tcm term
        nm = mkId ty (mkUnsafeSystemName "x" uniq) -- See [Note: Name re-creation]

{- [Note: Name re-creation]
The names of heap bound variables are safely generate with mkUniqSystemId in Clash.Core.Evaluator.newLetBinding.
But only their uniqs end up in the heap, not the complete names.
So we use mkUnsafeSystemName to recreate the same Name.
-}

matchLiteralContructor
  :: Term
  -> Literal
  -> [(Pat,Term)]
  -> NormalizeSession Term
matchLiteralContructor c (IntegerLiteral l) alts = go (reverse alts)
 where
  go [(DefaultPat,e)] = changed e
  go ((DataPat dc [] xs,e):alts')
    | dcTag dc == 1
    , l >= ((-2)^(63::Int)) &&  l < 2^(63::Int)
    = let fvs       = Lens.foldMapOf termFreeIds unitVarSet e
          (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                    $ zip xs [Literal (IntLiteral l)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec binds e
      in changed e'
    | dcTag dc == 2
    , l >= 2^(63::Int)
    = let !(Jp# !(BN# ba)) = l
          ba'       = BA.ByteArray ba
          bv        = PV.Vector 0 (BA.sizeofByteArray ba') ba'
          fvs       = Lens.foldMapOf termFreeIds unitVarSet e
          (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                    $ zip xs [Literal (ByteArrayLiteral bv)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec binds e
      in changed e'
    | dcTag dc == 3
    , l < ((-2)^(63::Int))
    = let !(Jn# !(BN# ba)) = l
          ba'       = BA.ByteArray ba
          bv        = PV.Vector 0 (BA.sizeofByteArray ba') ba'
          fvs       = Lens.foldMapOf termFreeIds unitVarSet e
          (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                    $ zip xs [Literal (ByteArrayLiteral bv)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec binds e
      in changed e'
    | otherwise
    = go alts'
  go _ = error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c

matchLiteralContructor c (NaturalLiteral l) alts = go (reverse alts)
 where
  go [(DefaultPat,e)] = changed e
  go ((DataPat dc [] xs,e):alts')
    | dcTag dc == 1
    , l >= 0 && l < 2^(64::Int)
    = let fvs       = Lens.foldMapOf termFreeIds unitVarSet e
          (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                    $ zip xs [Literal (WordLiteral l)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec binds e
      in changed e'
    | dcTag dc == 2
    , l >= 2^(64::Int)
    = let !(Jp# !(BN# ba)) = l
          ba'       = BA.ByteArray ba
          bv        = PV.Vector 0 (BA.sizeofByteArray ba') ba'
          fvs       = Lens.foldMapOf termFreeIds unitVarSet e
          (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                    $ zip xs [Literal (ByteArrayLiteral bv)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec binds e
      in changed e'
    | otherwise
    = go alts'
  go _ = error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c

matchLiteralContructor _ _ ((DefaultPat,e):_) = changed e
matchLiteralContructor c _ _ =
  error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c

caseOneAlt :: Term -> RewriteMonad extra Term
caseOneAlt e@(Case _ _ [(pat,altE)]) = case pat of
  DefaultPat -> changed altE
  LitPat _ -> changed altE
  DataPat _ tvs xs
    | (coerce tvs ++ coerce xs) `varsDoNotOccurIn` altE
    -> changed altE
    | otherwise
    -> return e

caseOneAlt e = return e

-- | Bring an application of a DataCon or Primitive in ANF, when the argument is
-- is considered non-representable
nonRepANF :: NormRewrite
nonRepANF ctx e@(App appConPrim arg)
  | (conPrim, _) <- collectArgs e
  , isCon conPrim || isPrim conPrim
  = do
    untranslatable <- isUntranslatable False arg
    case (untranslatable,arg) of
      (True,Letrec binds body) -> changed (Letrec binds (App appConPrim body))
      (True,Case {})  -> specializeNorm ctx e
      (True,Lam {})   -> specializeNorm ctx e
      (True,TyLam {}) -> specializeNorm ctx e
      _               -> return e

nonRepANF _ e = return e

-- | Ensure that top-level lambda's eventually bind a let-expression of which
-- the body is a variable-reference.
topLet :: NormRewrite
topLet (TransformContext is0 ctx) e
  | all isLambdaBodyCtx ctx && not (isLet e)
  = do
  untranslatable <- isUntranslatable False e
  if untranslatable
    then return e
    else do tcm <- Lens.view tcCache
            is1 <- unionInScope is0 <$> Lens.use globalInScope
            argId <- mkTmBinderFor is1 tcm (mkUnsafeSystemName "result" 0) e
            changed (Letrec [(argId, e)] (Var argId))

topLet (TransformContext is0 ctx) e@(Letrec binds body)
  | all isLambdaBodyCtx ctx
  = do
    localVar       <- isNonGlobalVar body
    untranslatable <- isUntranslatable False body
    if localVar || untranslatable
      then return e
      else do tcm <- Lens.view tcCache
              is1 <- unionInScope is0 <$> Lens.use globalInScope
              argId <- mkTmBinderFor is1 tcm (mkUnsafeSystemName "result" 0) body
              changed (Letrec (binds ++ [(argId,body)]) (Var argId))

topLet _ e = return e

-- Misc rewrites

-- | Remove unused let-bindings
deadCode :: NormRewrite
deadCode _ e@(Letrec xes body) = do
    let bodyFVs = Lens.foldMapOf termFreeIds unitVarSet body
        (xesUsed,xesOther) = List.partition((`elemVarSet` bodyFVs) . fst) xes
        xesUsed' = findUsedBndrs [] xesUsed xesOther
    if length xesUsed' /= length xes
      then case xesUsed' of
              [] -> changed body
              _  -> changed (Letrec xesUsed' body)
      else return e
  where
    findUsedBndrs :: [(Id, Term)] -> [(Id, Term)]
                  -> [(Id, Term)] -> [(Id, Term)]
    findUsedBndrs used []      _     = used
    findUsedBndrs used explore other =
      let fvsUsed = List.foldl' unionVarSet
                                emptyVarSet
                                (map (Lens.foldMapOf termFreeIds unitVarSet . snd) explore)
          (explore',other') = List.partition
                                ((`elemVarSet` fvsUsed) . fst) other
      in findUsedBndrs (used ++ explore) explore' other'

deadCode _ e = return e

removeUnusedExpr :: NormRewrite
removeUnusedExpr _ e@(collectArgs -> (p@(Prim nm _),args)) = do
  bbM <- HashMap.lookup nm <$> Lens.use (extra.primitives)
  case bbM of
    Just (BlackBox pNm _ _ _ _ _ inc templ) -> do
      let usedArgs = if isFromInt pNm
                        then [0,1,2]
                        else usedArguments templ ++
                             concatMap (usedArguments . snd) inc
      tcm <- Lens.view tcCache
      args' <- go tcm 0 usedArgs args
      if args == args'
         then return e
         else changed (mkApps p args')
    _ -> return e
  where
    go _ _ _ [] = return []
    go tcm n used (Right ty:args') = do
      args'' <- go tcm n used args'
      return (Right ty : args'')
    go tcm n used (Left tm : args') = do
      args'' <- go tcm (n+1) used args'
      let ty = termType tcm tm
          p' = mkApps (Prim "Clash.Transformations.removedArg" undefinedTy) [Right ty]
      if n `elem` used
         then return (Left tm : args'')
         else return (Left p' : args'')

removeUnusedExpr _ e@(Case _ _ [(DataPat _ [] xs,altExpr)]) =
  if xs `idsDoNotOccurIn` altExpr
     then changed altExpr
     else return e

-- Replace any expression that creates a Vector of size 0 within the application
-- of the Cons constructor, by the Nil constructor.
removeUnusedExpr _ e@(collectArgs -> (Data dc, [_,Right aTy,Right nTy,_,Left a,Left nil]))
  | nameOcc (dcName dc) == "Clash.Sized.Vector.Cons"
  = do
    tcm <- Lens.view tcCache
    case runExcept (tyNatSize tcm nTy) of
      Right 0
        | (con, _) <- collectArgs nil
        , not (isCon con)
        -> let eTy = termType tcm e
               (TyConApp vecTcNm _) = tyView eTy
               (Just vecTc) = lookupUniqMap vecTcNm tcm
               [nilCon,consCon] = tyConDataCons vecTc
               v = mkVec nilCon consCon aTy 1 [a]
           in  changed v
      _ -> return e

removeUnusedExpr _ e = return e

-- | Inline let-bindings when the RHS is either a local variable reference or
-- is constant (except clock or reset generators)
bindConstantVar :: NormRewrite
bindConstantVar = inlineBinders test
  where
    test _ (_, e) = isNonGlobalVar e >>= \case
      True -> return True
      _    -> isConstantNotClockReset e >>= \case
        True -> Lens.use (extra.inlineConstantLimit) >>= \case
          0 -> return True
          n -> return (termSize e <= n)
        _ -> return False
    -- test _ _ = return False

-- | Push a cast over a case into it's alternatives.
caseCast :: NormRewrite
caseCast _ (Cast (Case subj ty alts) ty1 ty2) = do
  let alts' = map (\(p,e) -> (p, Cast e ty1 ty2)) alts
  changed (Case subj ty alts')
caseCast _ e = return e

-- | Push a cast over a Letrec into it's body
letCast :: NormRewrite
letCast _ (Cast (Letrec binds body) ty1 ty2) =
  changed $ Letrec binds (Cast body ty1 ty2)
letCast _ e = return e


-- | Push cast over an argument to a funtion into that function
--
-- This is done by specializing on the casted argument.
-- Example:
-- @
--   y = f (cast a)
--     where f x = g x
-- @
-- transforms to:
-- @
--   y = f' a
--     where f' x' = (\x -> g x) (cast x')
-- @
argCastSpec :: NormRewrite
argCastSpec ctx e@(App _ (Cast e' _ _)) = case e' of
  Var {} -> go
  Cast (Var {}) _ _ -> go
  _ -> warn go
  where
    go = specializeNorm ctx e
    warn = trace (unlines ["WARNING: " ++ $(curLoc) ++ "specializing a function on a possibly non work-free cast."
                          ,"Generated HDL implementation might contain duplicate work."
                          ,"Please report this as a bug."
                          ,""
                          ,"Expression where this occurs:"
                          ,showPpr e
                          ])
argCastSpec _ e = return e

-- | Only inline casts that just contain a 'Var', because these are guaranteed work-free.
-- These are the result of the 'splitCastWork' transformation.
inlineCast :: NormRewrite
inlineCast = inlineBinders test
  where
    test _ (_, (Cast (Var {}) _ _)) = return True
    test _ _ = return False

-- | Eliminate two back to back casts where the type going in and coming out are the same
--
-- @
--   (cast :: b -> a) $ (cast :: a -> b) x   ==> x
-- @
eliminateCastCast :: NormRewrite
eliminateCastCast _ c@(Cast (Cast e tyA tyB) tyB' tyC) = do
  tcm <- Lens.view tcCache
  let ntyA  = normalizeType tcm tyA
      ntyB  = normalizeType tcm tyB
      ntyB' = normalizeType tcm tyB'
      ntyC  = normalizeType tcm tyC
  if ntyB == ntyB' && ntyA == ntyC then changed e
                                   else throwError
  where throwError = do
          (nm,sp) <- Lens.use curFun
          throw (ClashException sp ($(curLoc) ++ showPpr nm
                  ++ ": Found 2 nested casts whose types don't line up:\n"
                  ++ showPpr c)
                Nothing)

eliminateCastCast _ e = return e

-- | Make a cast work-free by splitting the work of to a separate binding
--
-- @
-- let x = cast (f a b)
-- ==>
-- let x  = cast x'
--     x' = f a b
-- @
splitCastWork :: NormRewrite
splitCastWork ctx@(TransformContext is0 _) unchanged@(Letrec vs e') = do
  is1 <- unionInScope is0 <$> Lens.use globalInScope
  (vss', Monoid.getAny -> hasChanged) <- listen (mapM (splitCastLetBinding is1) vs)
  let vs' = concat vss'
  if hasChanged then changed (Letrec vs' e')
                else return unchanged
  where
    splitCastLetBinding
      :: InScopeSet
      -> LetBinding
      -> RewriteMonad extra [LetBinding]
    splitCastLetBinding isN x@(nm, e) = case e of
      Cast (Var {}) _ _  -> return [x]  -- already work-free
      Cast (Cast {}) _ _ -> return [x]  -- casts will be eliminated
      Cast e0 ty1 ty2 -> do
        tcm <- Lens.view tcCache
        nm' <- mkTmBinderFor isN tcm (mkDerivedName ctx (nameOcc $ varName nm)) e0
        changed [(nm',e0)
                ,(nm, Cast (Var nm') ty1 ty2)
                ]
      _ -> return [x]

splitCastWork _ e = return e


-- | Inline work-free functions, i.e. fully applied functions that evaluate to
-- a constant
inlineWorkFree :: NormRewrite
inlineWorkFree (TransformContext localScope _) e@(collectArgs -> (Var f,args))
  = do
    tcm <- Lens.view tcCache
    let eTy = termType tcm e
    argsHaveWork <- or <$> mapM (either expressionHasWork
                                        (const (pure False)))
                                args
    untranslatable <- isUntranslatableType True eTy
    let isSignal = isSignalType tcm eTy
    let isLocalVar = f `elemInScopeSet` localScope
    if untranslatable || isSignal || argsHaveWork || isLocalVar
      then return e
      else do
        bndrs <- Lens.use bindings
        case lookupVarEnv f bndrs of
          -- Don't inline recursive expressions
          Just (_,_,_,body) -> do
            isRecBndr <- isRecursiveBndr f
            if isRecBndr
               then return e
               else changed (mkApps body args)
          _ -> return e
  where
    -- an expression is has work when it contains free local variables,
    -- or has a Signal type, i.e. it does not evaluate to a work-free
    -- constant.
    expressionHasWork e' = do
      fvIds <- Lens.toListOf <$> localFreeIds <*> pure e'
      tcm   <- Lens.view tcCache
      let e'Ty     = termType tcm e'
          isSignal = isSignalType tcm e'Ty
      return (not (null fvIds) || isSignal)

inlineWorkFree (TransformContext localScope _) e@(Var f) = do
  tcm <- Lens.view tcCache
  let fTy      = varType f
      closed   = not (isPolyFunCoreTy tcm fTy)
      isSignal = isSignalType tcm fTy
  untranslatable <- isUntranslatableType True fTy
  let isLocalVar = f `elemInScopeSet` localScope
  if closed && not untranslatable && not isSignal && not isLocalVar
    then do
      bndrs <- Lens.use bindings
      case lookupVarEnv f bndrs of
        -- Don't inline recursive expressions
        Just (_,_,_,body) -> do
          isRecBndr <- isRecursiveBndr f
          if isRecBndr
             then return e
             else changed body
        _ -> return e
    else return e

inlineWorkFree _ e = return e

-- | Inline small functions
inlineSmall :: NormRewrite
inlineSmall (TransformContext localScope _) e@(collectArgs -> (Var f,args)) = do
  untranslatable <- isUntranslatable True e
  topEnts <- Lens.view topEntities
  let isLocalVar = f `elemInScopeSet` localScope
  if untranslatable || f `elemVarSet` topEnts || isLocalVar
    then return e
    else do
      bndrs <- Lens.use bindings
      sizeLimit <- Lens.use (extra.inlineFunctionLimit)
      case lookupVarEnv f bndrs of
        -- Don't inline recursive expressions
        Just (_,_,inl,body) -> do
          isRecBndr <- isRecursiveBndr f
          if not isRecBndr && inl /= NoInline && termSize body < sizeLimit
             then changed (mkApps body args)
             else return e
        _ -> return e

inlineSmall _ e = return e

-- | Specialise functions on arguments which are constant, except when they
-- are clock or reset generators
constantSpec :: NormRewrite
constantSpec ctx e@(App e1 e2)
  | (Var {}, args) <- collectArgs e1
  , (_, []) <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  , isConstant e2
  = do tcm <- Lens.view tcCache
       let e2Ty = termType tcm e2
       -- Don't specialise on clock or reset generators
       case isClockOrReset tcm e2Ty of
          False -> specializeNorm ctx e
          _ -> return e

constantSpec _ e = return e


-- Experimental

-- | Propagate arguments of application inwards; except for 'Lam' where the
-- argument becomes let-bound.
--
-- Note [AppProp deshadow]
--
-- Imagine:
--
-- @
-- (case x of
--    D a b -> h a) (f x y)
-- @
--
-- rewriting this to:
--
-- @
-- let b = f x y
-- in  case x of
--       D a b -> h a b
-- @
--
-- is very bad because 'b' in 'h a b' is now bound by the pattern instead of the
-- newly introduced let-binding
--
-- instead me must rewrite to:
--
-- @
-- let b1 = f x y
-- in  case x of
--       D a b -> h a b1
-- @
appProp :: NormRewrite
appProp (TransformContext is0 _) (App (Lam v e) arg) = do
  if isConstant arg || isVar arg
    then do
      is1 <- unionInScope is0 <$> Lens.use globalInScope
      let subst = extendIdSubst (mkSubst is1) v arg
      changed $ substTm "appProp.AppLam" subst e
    else changed $ Letrec [(v, arg)] e

appProp _ (App (Letrec v e) arg) = do
  changed (Letrec v (App e arg))

appProp ctx@(TransformContext is0 _) (App (Case scrut ty alts) arg) = do
  tcm <- Lens.view tcCache
  let argTy = termType tcm arg
      ty' = applyFunTy tcm ty argTy
  if isConstant arg || isVar arg
    then do
      let alts' = map (second (`App` arg)) alts
      changed $ Case scrut ty' alts'
    else do
      -- See Note [AppProp deshadow]
      is1 <- unionInScope is0 <$> Lens.use globalInScope
      let is2 = unionInScope is1 ((mkInScopeSet . mkVarSet . concatMap (patVars . fst)) alts)
      boundArg <- mkTmBinderFor is2 tcm (mkDerivedName ctx "app_arg") arg
      let alts' = map (second (`App` (Var boundArg))) alts
      changed (Letrec [(boundArg, arg)] (Case scrut ty' alts'))

appProp (TransformContext is0 _) (TyApp (TyLam tv e) t) = do
  is1 <- unionInScope is0 <$> Lens.use globalInScope
  let subst = extendTvSubst (mkSubst is1) tv t
  changed $ substTm "appProp.TyAppTyLam" subst e

appProp _ (TyApp (Letrec v e) t) = do
  changed (Letrec v (TyApp e t))

appProp _ (TyApp (Case scrut altsTy alts) ty) = do
  let alts' = map (second (`TyApp` ty)) alts
  tcm <- Lens.view tcCache
  let ty' = piResultTy tcm altsTy ty
  changed (Case scrut ty' alts')

appProp _ e = return e

-- | Flatten ridiculous case-statements generated by GHC
--
-- For case-statements in haskell of the form:
--
-- @
-- f :: Unsigned 4 -> Unsigned 4
-- f x = case x of
--   0 -> 3
--   1 -> 2
--   2 -> 1
--   3 -> 0
-- @
--
-- GHC generates Core that looks like:
--
-- @
-- f = \(x :: Unsigned 4) -> case x == fromInteger 3 of
--                             False -> case x == fromInteger 2 of
--                               False -> case x == fromInteger 1 of
--                                 False -> case x == fromInteger 0 of
--                                   False -> error "incomplete case"
--                                   True  -> fromInteger 3
--                                 True -> fromInteger 2
--                               True -> fromInteger 1
--                             True -> fromInteger 0
-- @
--
-- Which would result in a priority decoder circuit where a normal decoder
-- circuit was desired.
--
-- This transformation transforms the above Core to the saner:
--
-- @
-- f = \(x :: Unsigned 4) -> case x of
--        _ -> error "incomplete case"
--        0 -> fromInteger 3
--        1 -> fromInteger 2
--        2 -> fromInteger 1
--        3 -> fromInteger 0
-- @
caseFlat :: NormRewrite
caseFlat _ e@(Case (collectEqArgs -> Just (scrut',_)) ty _)
  = do
       case collectFlat scrut' e of
         Just alts' -> changed (Case scrut' ty (last alts' : init alts'))
         Nothing    -> return e

caseFlat _ e = return e

collectFlat :: Term -> Term -> Maybe [(Pat,Term)]
collectFlat scrut (Case (collectEqArgs -> Just (scrut', val)) _ty [lAlt,rAlt])
  | scrut' == scrut
  = case collectArgs val of
      (Prim nm' _,args') | isFromInt nm'
        -> case last args' of
            Left (Literal i) -> case (lAlt,rAlt) of
              ((pl,el),(pr,er))
                | isFalseDcPat pl || isTrueDcPat pr ->
                   case collectFlat scrut el of
                     Just alts' -> Just ((LitPat i, er) : alts')
                     Nothing    -> Just [(LitPat i, er)
                                        ,(DefaultPat, el)
                                        ]
                | otherwise ->
                   case collectFlat scrut er of
                     Just alts' -> Just ((LitPat i, el) : alts')
                     Nothing    -> Just [(LitPat i, el)
                                        ,(DefaultPat, er)
                                        ]
            _ -> Nothing
      _ -> Nothing
  where
    isFalseDcPat (DataPat p _ _)
      = ((== "GHC.Types.False") . nameOcc . dcName) p
    isFalseDcPat _ = False

    isTrueDcPat (DataPat p _ _)
      = ((== "GHC.Types.True") . nameOcc . dcName) p
    isTrueDcPat _ = False

collectFlat _ _ = Nothing

collectEqArgs :: Term -> Maybe (Term,Term)
collectEqArgs (collectArgs -> (Prim nm _, args))
  | nm == "Clash.Sized.Internal.BitVector.eq#"
    = let [_,_,Left scrut,Left val] = args
      in Just (scrut,val)
  | nm == "Clash.Sized.Internal.Index.eq#"  ||
    nm == "Clash.Sized.Internal.Signed.eq#" ||
    nm == "Clash.Sized.Internal.Unsigned.eq#"
    = let [_,Left scrut,Left val] = args
      in Just (scrut,val)
collectEqArgs _ = Nothing


isFromInt :: Text -> Bool
isFromInt nm = nm == "Clash.Sized.Internal.BitVector.fromInteger##" ||
               nm == "Clash.Sized.Internal.BitVector.fromInteger#" ||
               nm == "Clash.Sized.Internal.Index.fromInteger#" ||
               nm == "Clash.Sized.Internal.Signed.fromInteger#" ||
               nm == "Clash.Sized.Internal.Unsigned.fromInteger#"

type NormRewriteW = Transform (StateT ([LetBinding],InScopeSet) (RewriteMonad NormalizeState))

-- | See Note [ANF InScopeSet]
tellBinders :: Monad m => [LetBinding] -> StateT ([LetBinding],InScopeSet) m ()
tellBinders bs = modify ((bs ++) *** (`extendInScopeSetList` (map fst bs)))

-- NOTE [unsafeUnbind]: Use unsafeUnbind (which doesn't freshen pattern
-- variables). Reason: previously collected expression still reference
-- the 'old' variable names created by the traversal!

-- | Turn an expression into a modified ANF-form. As opposed to standard ANF,
-- constants do not become let-bound.
makeANF :: NormRewrite
makeANF (TransformContext is0 ctx) (Lam bndr e) = do
  e' <- makeANF (TransformContext (extendInScopeSet is0 bndr)
                                  (LamBody bndr:ctx))
                e
  return (Lam bndr e')

makeANF _ e@(TyLam {}) = return e

makeANF ctx@(TransformContext is0 _) e0
  = do
    is1 <- unionInScope is0 <$> Lens.use globalInScope
    -- We need to freshen all binders in `e` because we're shuffling them around
    -- into a single let-binder, because even when binders don't shadow, they
    -- don't have to be unique within an expression. And so lifting them all
    -- to a single let-binder will cause issues when they're not unique.
    --
    -- We cannot make freshening part of collectANF, because when we generate
    -- new binders, we need to make sure those names do not conflict with _any_
    -- of the existing binders in the expression.
    --
    -- See also Note [ANF InScopeSet]
    let (is2,e1) = freshenTm is1 e0
    (e2,(bndrs,_)) <- runStateT (bottomupR collectANF ctx e1) ([],is2)
    case bndrs of
      [] -> return e0
      _  -> changed (Letrec bndrs e2)

-- | Note [ANF InScopeSet]
--
-- The InScopeSet contains:
--
--    1. All the free variables of the expression we are traversing
--
--    2. All the bound variables of the expression we are traversing
--
--    3. The newly created let-bindings as we recurse back up the traversal
--
-- All of these are needed to created let-bindings that
--
--    * Do not shadow
--    * Are not shadowed
--    * Nor conflict with each other (i.e. have the same unique)
--
-- Initially we start with the local InScopeSet and add the global variables:
--
-- @
-- is1 <- unionInScope is0 <$> Lens.use globalInScope
-- @
--
-- Which will gives us the (superset of) free variables of the expression. Then
-- we call  'freshenTm'
--
-- @
-- let (is2,e1) = freshenTm is1 e0
-- @
--
-- Which extends the InScopeSet with all the bound variables in 'e1', the
-- version of 'e0' where all binders are unique (not just deshadowed).
--
-- So we start out with an InScopeSet that satisfies points 1 and 2, now every
-- time we create a new binder we must add it to the InScopeSet to satisfy
-- point 3.
collectANF :: NormRewriteW
collectANF ctx e@(App appf arg)
  | (conVarPrim, _) <- collectArgs e
  , isCon conVarPrim || isPrim conVarPrim || isVar conVarPrim
  = do
    untranslatable <- lift (isUntranslatable False arg)
    localVar       <- lift (isNonGlobalVar arg)
    constantNoCR   <- lift (isConstantNotClockReset arg)
    case (untranslatable,localVar || constantNoCR,arg) of
      (False,False,_) -> do
        tcm <- Lens.view tcCache
        -- See Note [ANF InScopeSet]
        is1   <- Lens.use _2
        argId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "app_arg") arg)
        -- See Note [ANF InScopeSet]
        tellBinders [(argId,arg)]
        return (App appf (Var argId))
      (True,False,Letrec binds body) -> do
        tellBinders binds
        return (App appf body)
      _ -> return e

collectANF _ (Letrec binds body) = do
  tellBinders binds
  untranslatable <- lift (isUntranslatable False body)
  localVar       <- lift (isNonGlobalVar body)
  if localVar || untranslatable
    then return body
    else do
      tcm <- Lens.view tcCache
      -- See Note [ANF InScopeSet]
      is1 <- Lens.use _2
      argId <- lift (mkTmBinderFor is1 tcm (mkUnsafeSystemName "result" 0) body)
      -- See Note [ANF InScopeSet]
      tellBinders [(argId,body)]
      return (Var argId)

-- TODO: The code below special-cases ANF for the ':-' constructor for the
-- 'Signal' type. The 'Signal' type is essentially treated as a "transparent"
-- type by the Clash compiler, so observing its constructor leads to all kinds
-- of problems. In this case that "Clash.Rewrite.Util.mkSelectorCase" will
-- try to project the LHS and RHS of the ':-' constructor, however,
-- 'mkSelectorCase' uses 'coreView' to find the "real" data-constructor.
-- 'coreView' however looks through the 'Signal' type, and hence 'mkSelector'
-- finds the data constructors for the element type of Signal. This resulted in
-- error #24 (https://github.com/christiaanb/clash2/issues/24), where we
-- try to get the first field out of the 'Vec's 'Nil' constructor.
--
-- Ultimately we should stop treating Signal as a "transparent" type and deal
-- handling of the Signal type, and the involved co-recursive functions,
-- properly. At the moment, Clash cannot deal with this recursive type and the
-- recursive functions involved, hence the need for special-casing code. After
-- everything is done properly, we should remove the two lines below.
collectANF _ e@(Case _ _ [(DataPat dc _ _,_)])
  | nameOcc (dcName dc) == "Clash.Signal.Internal.:-" = return e

collectANF ctx (Case subj ty alts) = do
    localVar <- lift (isNonGlobalVar subj)

    subj' <- if localVar || isConstant subj
      then return subj
      else do
        tcm <- Lens.view tcCache
        -- See Note [ANF InScopeSet]
        is1 <- Lens.use _2
        argId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "case_scrut") subj)
        -- See Note [ANF InScopeSet]
        tellBinders [(argId,subj)]
        return (Var argId)

    alts' <- mapM (doAlt subj') alts

    case alts' of
      [(DataPat _ [] xs,altExpr)]
        | xs `idsDoNotOccurIn` altExpr
        -> return altExpr
      _ -> return (Case subj' ty alts')
  where
    doAlt
      :: Term -> (Pat,Term)
      -> StateT ([LetBinding],InScopeSet) (RewriteMonad NormalizeState)
                (Pat,Term)
    doAlt subj' alt@(DataPat dc [] xs,altExpr) = do
      lv  <- lift (isNonGlobalVar altExpr)
      patSels <- Monad.zipWithM (doPatBndr subj' dc) xs [0..]
      let usesXs (Var n) = any (== n) xs
          usesXs _       = False
      if (lv && not (usesXs altExpr)) || isConstant altExpr
        then do
          -- See Note [ANF InScopeSet]
          tellBinders patSels
          return alt
        else do
          tcm <- Lens.view tcCache
          -- See Note [ANF InScopeSet]
          is1 <- Lens.use _2
          altId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "case_alt") altExpr)
          -- See Note [ANF InScopeSet]
          tellBinders ((altId,altExpr):patSels)
          return (DataPat dc [] xs,Var altId)
    doAlt _ alt@(DataPat {}, _) = return alt
    doAlt _ alt@(pat,altExpr) = do
      lv <- lift (isNonGlobalVar altExpr)
      if lv || isConstant altExpr
        then return alt
        else do
          tcm <- Lens.view tcCache
          -- See Note [ANF InScopeSet]
          is1 <- Lens.use _2
          altId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "case_alt") altExpr)
          tellBinders [(altId,altExpr)]
          return (pat,Var altId)

    doPatBndr
      :: Term -> DataCon -> Id -> Int
      -> StateT ([LetBinding],InScopeSet) (RewriteMonad NormalizeState)
                LetBinding
    doPatBndr subj' dc pId i
      = do
        tcm <- Lens.view tcCache
        -- See Note [ANF InScopeSet]
        is1 <- Lens.use _2
        patExpr <- lift (mkSelectorCase ($(curLoc) ++ "doPatBndr") is1 tcm subj' (dcTag dc) i)
        -- No need to 'tellBinders' here because 'pId' is already in the ANF
        -- InScopeSet.
        --
        -- See also Note [ANF InScopeSet]
        return (pId,patExpr)

collectANF _ e = return e

-- | Eta-expand top-level lambda's (DON'T use in a traversal!)
etaExpansionTL :: NormRewrite
etaExpansionTL (TransformContext is0 ctx) e = do
  is1 <- unionInScope is0 <$> Lens.use globalInScope
  etaExpansionTL' (TransformContext is1 ctx) e

etaExpansionTL' :: NormRewrite
etaExpansionTL' (TransformContext is0 ctx) (Lam bndr e) = do
  e' <- etaExpansionTL'
          (TransformContext (extendInScopeSet is0 bndr) (LamBody bndr:ctx))
          e
  return $ Lam bndr e'

etaExpansionTL' (TransformContext is0 ctx) (Letrec xes e) = do
  let bndrs = map fst xes
  e' <- etaExpansionTL'
          (TransformContext (extendInScopeSetList is0 bndrs)
                            (LetBody bndrs:ctx))
          e
  case stripLambda e' of
    (bs@(_:_),e2) -> do
      let e3 = Letrec xes e2
      changed (mkLams e3 bs)
    _ -> return (Letrec xes e')
  where
    stripLambda :: Term -> ([Id],Term)
    stripLambda (Lam bndr e0) =
      let (bndrs,e1) = stripLambda e0
      in  (bndr:bndrs,e1)
    stripLambda e' = ([],e')

etaExpansionTL' (TransformContext is0 ctx) e
  = do
    tcm <- Lens.view tcCache
    if isFun tcm e
      then do
        let argTy = ( fst
                    . Maybe.fromMaybe (error $ $(curLoc) ++ "etaExpansion splitFunTy")
                    . splitFunTy tcm
                    . termType tcm
                    ) e
        newId <- mkInternalVar is0 "arg" argTy
        e' <- etaExpansionTL' (TransformContext (extendInScopeSet is0 newId)
                                               (LamBody newId:ctx))
                             (App e (Var newId))
        changed (Lam newId e')
      else return e

-- | Turn a  normalized recursive function, where the recursive calls only pass
-- along the unchanged original arguments, into let-recursive function. This
-- means that all recursive calls are replaced by the same variable reference as
-- found in the body of the top-level let-expression.
recToLetRec :: NormRewrite
recToLetRec (TransformContext is0 []) e = do
  (fn,_) <- Lens.use curFun
  tcm    <- Lens.view tcCache
  case splitNormalized tcm e of
    Right (args,bndrs,res) -> do
      let v                 = Var fn
          args'             = map Var args
          (toInline,others) = List.partition (eqApp tcm v args' . snd) bndrs
          resV              = Var res
      case (toInline,others) of
        (_:_,_:_) -> do
          let is1          = extendInScopeSetList is0 (args ++ map fst bndrs)
          is2 <- unionInScope is1 <$> Lens.use globalInScope
          let substsInline = extendIdSubstList (mkSubst is2)
                           $ map (second (const resV)) toInline
              others'      = map (second (substTm "recToLetRec" substsInline))
                                 others
          changed $ mkLams (Letrec others' resV) args
        _ -> return e
    _ -> return e
  where
    -- This checks whether things are semantically equal
    --
    -- i.e. that
    --
    -- xs == (fst xs, snd xs)
    --
    -- TODO: this is far from complete
    eqApp tcm v args (collectArgs -> (v',args'))
      | v == v'
      , let args2 = Either.lefts args'
      , length args == length args2
      = and (zipWith (eqArg tcm) args args2)
      | otherwise
      = False

    eqArg _ v1 v2@(Var {})
      = v1 == v2
    eqArg tcm v1 v2@(collectArgs -> (Data _,args'))
      | termType tcm v1 == termType tcm v2
      = and (zipWith (isNthProjection v1) [0..] (Either.lefts args'))
    eqArg _ _ _
      = False

    -- `isNthProjection s n c` checks that `c` is the `n`th projection
    -- of `s`.
    isNthProjection :: Term -> Int -> Term -> Bool
    isNthProjection v n (Case v' _ [(DataPat _ _ xs, Var s)])
      | v == v'
      , Just n' <- List.elemIndex s xs
      = n == n'
    isNthProjection _ _ _ = False

recToLetRec _ e = return e

-- | Inline a function with functional arguments
inlineHO :: NormRewrite
inlineHO _ e@(App _ _)
  | (Var f, args) <- collectArgs e
  = do
    tcm <- Lens.view tcCache
    let hasPolyFunArgs = or (map (either (isPolyFun tcm) (const False)) args)
    if hasPolyFunArgs
      then do (cf,_)    <- Lens.use curFun
              isInlined <- zoomExtra (alreadyInlined f cf)
              limit     <- Lens.use (extra.inlineLimit)
              if (Maybe.fromMaybe 0 isInlined) > limit
                then do
                  lvl <- Lens.view dbgLevel
                  traceIf (lvl > DebugNone) ($(curLoc) ++ "InlineHO: " ++ show f ++ " already inlined " ++ show limit ++ " times in:" ++ show cf) (return e)
                else do
                  bodyMaybe <- lookupVarEnv f <$> Lens.use bindings
                  case bodyMaybe of
                    Just (_,_,_,body) -> do
                      zoomExtra (addNewInline f cf)
                      changed (mkApps body args)
                    _ -> return e
      else return e

inlineHO _ e = return e

-- | Simplified CSE, only works on let-bindings, works from top to bottom
simpleCSE :: NormRewrite
simpleCSE (TransformContext is0 _) e@(Letrec binders body) = do
  let is1 = extendInScopeSetList is0 (map fst binders)
  is2 <- unionInScope is1 <$> Lens.use globalInScope
  let (reducedBindings,body') = reduceBindersFix is2 binders body
  if length binders /= length reducedBindings
     then changed (Letrec reducedBindings body')
     else return e

simpleCSE _ e = return e

reduceBindersFix
  :: InScopeSet
  -> [LetBinding]
  -> Term
  -> ([LetBinding],Term)
reduceBindersFix is binders body =
  if length binders /= length reduced
     then reduceBindersFix is reduced body'
     else (binders,body)
 where
  (reduced,body') = reduceBinders is [] body binders

reduceBinders
  :: InScopeSet
  -> [LetBinding]
  -> Term
  -> [LetBinding]
  -> ([LetBinding],Term)
reduceBinders _  processed body [] = (processed,body)
reduceBinders is processed body ((id_,expr):binders) = case List.find ((== expr) . snd) processed of
    Just (id2,_) ->
      let subst      = extendIdSubst (mkSubst is) id_ (Var id2)
          processed' = map (second (substTm "reduceBinders.processed" subst)) processed
          binders'   = map (second (substTm "reduceBinders.binders"   subst)) binders
          body'      = substTm "reduceBinders.body" subst body
      in  reduceBinders is processed' body' binders'
    Nothing -> reduceBinders is ((id_,expr):processed) body binders

reduceConst :: NormRewrite
reduceConst ctx@(TransformContext is0 _) e@(App _ _)
  | isConstant e
  , (conPrim, _) <- collectArgs e
  , isPrim conPrim
  = do
    tcm <- Lens.view tcCache
    bndrs <- Lens.use bindings
    primEval <- Lens.view evaluator
    ids <- Lens.use uniqSupply
    let (ids1,ids2) = splitSupply ids
    uniqSupply Lens..= ids2
    gh <- Lens.use globalHeap
    is1 <- unionInScope is0 <$> Lens.use globalInScope
    case whnf' primEval bndrs tcm gh ids1 is1 False e of
      (gh',ph',e') -> do
        globalHeap Lens..= gh'
        bindPureHeap ctx tcm ph' $ \_ctx' -> case e' of
          (Literal _) -> changed e'
          (collectArgs -> (Prim nm _, _))
            | isFromInt nm
            , e /= e'
            -> changed e'
          (collectArgs -> (Data _,_)) -> changed e'
          _                           -> return e

reduceConst _ e = return e

-- | Replace primitives by their "definition" if they would lead to let-bindings
-- with a non-representable type when a function is in ANF. This happens for
-- example when Clash.Size.Vector.map consumes or produces a vector of
-- non-representable elements.
--
-- Basically what this transformation does is replace a primitive the completely
-- unrolled recursive definition that it represents. e.g.
--
-- > zipWith ($) (xs :: Vec 2 (Int -> Int)) (ys :: Vec 2 Int)
--
-- is replaced by:
--
-- > let (x0  :: (Int -> Int))       = case xs  of (:>) _ x xr -> x
-- >     (xr0 :: Vec 1 (Int -> Int)) = case xs  of (:>) _ x xr -> xr
-- >     (x1  :: (Int -> Int)(       = case xr0 of (:>) _ x xr -> x
-- >     (y0  :: Int)                = case ys  of (:>) _ y yr -> y
-- >     (yr0 :: Vec 1 Int)          = case ys  of (:>) _ y yr -> xr
-- >     (y1  :: Int                 = case yr0 of (:>) _ y yr -> y
-- > in  (($) x0 y0 :> ($) x1 y1 :> Nil)
--
-- Currently, it only handles the following functions:
--
-- * Clash.Sized.Vector.map
-- * Clash.Sized.Vector.zipWith
-- * Clash.Sized.Vector.traverse#
-- * Clash.Sized.Vector.foldr
-- * Clash.Sized.Vector.fold
-- * Clash.Sized.Vector.dfold
-- * Clash.Sized.Vector.(++)
-- * Clash.Sized.Vector.head
-- * Clash.Sized.Vector.tail
-- * Clash.Sized.Vector.unconcat
-- * Clash.Sized.Vector.transpose
-- * Clash.Sized.Vector.replicate
-- * Clash.Sized.Vector.dtfold
reduceNonRepPrim :: NormRewrite
reduceNonRepPrim (TransformContext is0 _) e@(App _ _) | (Prim f _, args) <- collectArgs e = do
  tcm <- Lens.view tcCache
  is1 <- unionInScope is0 <$> Lens.use globalInScope
  let eTy = termType tcm e
  case tyView eTy of
    (TyConApp vecTcNm@(nameOcc -> "Clash.Sized.Vector.Vec")
              [runExcept . tyNatSize tcm -> Right 0, aTy]) -> do
      let (Just vecTc) = lookupUniqMap vecTcNm tcm
          [nilCon,consCon] = tyConDataCons vecTc
          nilE = mkVec nilCon consCon aTy 0 []
      changed nilE
    tv -> case f of
      "Clash.Sized.Vector.zipWith" | length args == 7 -> do
        let [lhsElTy,rhsElty,resElTy,nTy] = Either.rights args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTys <- mapM isUntranslatableType_not_poly [lhsElTy,rhsElty,resElTy]
            if or untranslatableTys
               then let [fun,lhsArg,rhsArg] = Either.lefts args
                    in  reduceZipWith is1 n lhsElTy rhsElty resElTy fun lhsArg rhsArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.map" | length args == 5 -> do
        let [argElTy,resElTy,nTy] = Either.rights args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTys <- mapM isUntranslatableType_not_poly [argElTy,resElTy]
            if or untranslatableTys
               then let [fun,arg] = Either.lefts args
                    in  reduceMap is1 n argElTy resElTy fun arg
               else return e
          _ -> return e
      "Clash.Sized.Vector.traverse#" | length args == 7 ->
        let [aTy,fTy,bTy,nTy] = Either.rights args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n ->
            let [dict,fun,arg] = Either.lefts args
            in  reduceTraverse is1 n aTy fTy bTy dict fun arg
          _ -> return e
      "Clash.Sized.Vector.fold" | length args == 4 -> do
        let [aTy,nTy] = Either.rights args
            isPow2 x  = x /= 0 && (x .&. (complement x + 1)) == x
        untranslatableTy <- isUntranslatableType_not_poly aTy
        case runExcept (tyNatSize tcm nTy) of
          Right n | not (isPow2 (n + 1)) || untranslatableTy ->
            let [fun,arg] = Either.lefts args
            in  reduceFold is1 (n + 1) aTy fun arg
          _ -> return e
      "Clash.Sized.Vector.foldr" | length args == 6 ->
        let [aTy,bTy,nTy] = Either.rights args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTys <- mapM isUntranslatableType_not_poly [aTy,bTy]
            if or untranslatableTys
              then let [fun,start,arg] = Either.lefts args
                   in  reduceFoldr is1 n aTy fun start arg
              else return e
          _ -> return e
      "Clash.Sized.Vector.dfold" | length args == 8 ->
        let ([_kn,_motive,fun,start,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> reduceDFold is1 n aTy fun start arg
          _ -> return e
      "Clash.Sized.Vector.++" | length args == 5 ->
        let [nTy,aTy,mTy] = Either.rights args
            [lArg,rArg]   = Either.lefts args
        in case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
              (Right n, Right m)
                | n == 0 -> changed rArg
                | m == 0 -> changed lArg
                | otherwise -> do
                    untranslatableTy <- isUntranslatableType_not_poly aTy
                    if untranslatableTy
                       then reduceAppend is1 n m aTy lArg rArg
                       else return e
              _ -> return e
      "Clash.Sized.Vector.head" | length args == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTy <- isUntranslatableType_not_poly aTy
            if untranslatableTy
               then reduceHead is1 n aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.tail" | length args == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTy <- isUntranslatableType_not_poly aTy
            if untranslatableTy
               then reduceTail is1 n aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.last" | length args == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTy <- isUntranslatableType_not_poly aTy
            if untranslatableTy
               then reduceLast is1 n aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.init" | length args == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTy <- isUntranslatableType_not_poly aTy
            if untranslatableTy
               then reduceInit is1 n aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.unconcat" | length args == 6 -> do
        let ([_knN,_sm,arg],[mTy,nTy,aTy]) = Either.partitionEithers args
        case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
          (Right n, Right 0) -> reduceUnconcat n 0 aTy arg
          _ -> return e
      "Clash.Sized.Vector.transpose" | length args == 5 -> do
        let ([_knN,arg],[mTy,nTy,aTy]) = Either.partitionEithers args
        case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
          (Right n, Right 0) -> reduceTranspose n 0 aTy arg
          _ -> return e
      "Clash.Sized.Vector.replicate" | length args == 4 -> do
        let ([_sArg,vArg],[nTy,aTy]) = Either.partitionEithers args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTy <- isUntranslatableType_not_poly aTy
            if untranslatableTy
               then reduceReplicate n aTy eTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.imap" | length args == 6 -> do
        let [nTy,argElTy,resElTy] = Either.rights args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTys <- mapM isUntranslatableType_not_poly [argElTy,resElTy]
            if or untranslatableTys
               then let [_,fun,arg] = Either.lefts args
                    in  reduceImap is1 n argElTy resElTy fun arg
               else return e
          _ -> return e
      "Clash.Sized.Vector.dtfold" | length args == 8 ->
        let ([_kn,_motive,lrFun,brFun,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> reduceDTFold is1 n aTy lrFun brFun arg
          _ -> return e
      "Clash.Sized.RTree.tdfold" | length args == 8 ->
        let ([_kn,_motive,lrFun,brFun,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> reduceTFold is1 n aTy lrFun brFun arg
          _ -> return e
      "Clash.Sized.RTree.treplicate" | length args == 4 -> do
        let ([_sArg,vArg],[nTy,aTy]) = Either.partitionEithers args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTy <- isUntranslatableType False aTy
            if untranslatableTy
               then reduceReplicate n aTy eTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Internal.BitVector.split#" | length args == 4 -> do
        let ([_knArg,bvArg],[nTy,mTy]) = Either.partitionEithers args
        case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy), tv) of
          (Right n, Right m, TyConApp tupTcNm [lTy,rTy])
            | n == 0 -> do
              let (Just tupTc) = lookupUniqMap tupTcNm tcm
                  [tupDc]      = tyConDataCons tupTc
                  tup          = mkApps (Data tupDc)
                                    [Right lTy
                                    ,Right rTy
                                    ,Left  bvArg
                                    ,Left  (mkApps (Prim "Clash.Transformations.removedArg" undefinedTy)
                                                   [Right rTy])
                                    ]

              changed tup
            | m == 0 -> do
              let (Just tupTc) = lookupUniqMap tupTcNm tcm
                  [tupDc]      = tyConDataCons tupTc
                  tup          = mkApps (Data tupDc)
                                    [Right lTy
                                    ,Right rTy
                                    ,Left  (mkApps (Prim "Clash.Transformations.removedArg" undefinedTy)
                                                   [Right lTy])
                                    ,Left  bvArg
                                    ]

              changed tup
          _ -> return e
      "Clash.Sized.Internal.BitVector.eq#"
        | ([_,_],[nTy]) <- Either.partitionEithers args
        , Right 0 <- runExcept (tyNatSize tcm nTy)
        , TyConApp boolTcNm [] <- tv
        -> let (Just boolTc) = lookupUniqMap boolTcNm tcm
               [_falseDc,trueDc] = tyConDataCons boolTc
           in  changed (Data trueDc)
      _ -> return e
  where
    isUntranslatableType_not_poly t = do
      u <- isUntranslatableType False t
      if u
         then return (null $ Lens.toListOf typeFreeVars t)
         else return False

reduceNonRepPrim _ e = return e

-- | This transformation lifts applications of global binders out of
-- alternatives of case-statements.
--
-- e.g. It converts:
--
-- @
-- case x of
--   A -> f 3 y
--   B -> f x x
--   C -> h x
-- @
--
-- into:
--
-- @
-- let f_arg0 = case x of {A -> 3; B -> x}
--     f_arg1 = case x of {A -> y; B -> x}
--     f_out  = f f_arg0 f_arg1
-- in  case x of
--       A -> f_out
--       B -> f_out
--       C -> h x
-- @
disjointExpressionConsolidation :: NormRewrite
disjointExpressionConsolidation ctx@(TransformContext is0 _) e@(Case _scrut _ty _alts@(_:_:_)) = do
    is1 <- unionInScope is0 <$> Lens.use globalInScope
    (_,collected) <- collectGlobals is1 [] [] e
    let disJoint = filter (isDisjoint . snd . snd) collected
    if null disJoint
       then return e
       else do
         exprs <- mapM (mkDisjointGroup is1) disJoint
         tcm   <- Lens.view tcCache
         lids  <- Monad.zipWithM (mkFunOut is1 tcm) disJoint exprs
         let substitution = zip (map fst disJoint) (map Var lids)
             subsMatrix   = l2m substitution
         (exprs',_) <- unzip <$> Monad.zipWithM
                        (\s (e',seen) -> collectGlobals is1 s seen e')
                        subsMatrix
                        exprs
         (e',_) <- collectGlobals is1 substitution [] e
         let lb = Letrec (zip lids exprs') e'
         lb' <- bottomupR deadCode ctx lb
         changed lb'
  where
    mkFunOut isN tcm (fun,_) (e',_) = do
      let ty  = termType tcm e'
          nm  = case collectArgs fun of
                   (Var v,_)      -> nameOcc (varName v)
                   (Prim nm' _,_) -> nm'
                   _             -> "complex_expression_"
          nm'' = last (Text.splitOn "." nm) `Text.append` "Out"
      mkInternalVar isN nm'' ty

    l2m = go []
      where
        go _  []     = []
        go xs (y:ys) = (xs ++ ys) : go (xs ++ [y]) ys

disjointExpressionConsolidation _ e = return e

-- | Given a function in the desired normal form, inline all the following
-- let-bindings:
--
-- Let-bindings with an internal name that is only used once, where it binds:
--   * a primitive that will be translated to an HDL expression (as opposed to
--     a HDL declaration)
--   * a projection case-expression (1 alternative)
--   * a data constructor
inlineCleanup :: NormRewrite
inlineCleanup (TransformContext is0 _) (Letrec binds body) = do
  prims <- Lens.use (extra.primitives)
      -- For all let-bindings, count the number of times they are referenced.
      -- We only inline let-bindings which are referenced only once, otherwise
      -- we would lose sharing.
  -- let allOccs       = List.foldl' (HashMap.unionWith (+)) HashMap.empty
  --                   $ map ( List.foldl' countOcc HashMap.empty
  --                         . Lens.toListOf termFreeIds . unembed . snd) binds
  let is1 = extendInScopeSetList is0 (map fst binds)
  is2 <- unionInScope is1 <$> Lens.use globalInScope
  let allOccs       = List.foldl' (unionVarEnvWith (+)) emptyVarEnv
                    $ map (Lens.foldMapByOf termFreeIds (unionVarEnvWith (+))
                            emptyVarEnv (`unitVarEnv` 1) . snd)
                          binds
      bodyFVs       = Lens.foldMapOf termFreeIds unitVarSet body
      (il,keep)     = List.partition (isInteresting allOccs prims bodyFVs) binds
      keep'         = inlineBndrs is2 keep il
  if null il then return  (Letrec binds body)
             else changed (Letrec keep' body)
  where
    -- Determine whether a let-binding is interesting to inline
    isInteresting
      :: VarEnv Int
      -> PrimMap (Primitive a b c)
      -> VarSet
      -> (Id, Term)
      -> Bool
    isInteresting allOccs prims bodyFVs (id_,(fst.collectArgs) -> tm)
      | nameSort (varName id_) /= User
      , id_ `notElemVarSet` bodyFVs
      = case tm of
          Prim nm _
            | Just p@(BlackBox {}) <- HashMap.lookup nm prims
            , TExpr <- kind p
            , Just occ <- lookupVarEnv id_ allOccs
            , occ < 2
            -> True
          Case _ _ [_] -> True
          Data _ -> True
          _ -> False
      | id_ `notElemVarSet` bodyFVs
      = case tm of
          -- Inlines WW projection that exposes internals of the BitVector types
          Case _ _ [(DataPat dcE _ _,_)]
            -> let nm = (nameOcc (dcName dcE))
               in nm == "Clash.Sized.Internal.BitVector.BV"  ||
                  nm == "Clash.Sized.Internal.BitVector.Bit"
          _ -> False

    isInteresting _ _ _ _ = False

    -- Inline let-bindings we want to inline into let-bindings we want to keep.
    inlineBndrs
      :: InScopeSet
      -> [(Id, Term)]
      -- let-bindings we keep
      -> [(Id, Term)]
      -- let-bindings we want to inline
      -> [(Id, Term)]
    inlineBndrs _   keep [] = keep
    inlineBndrs isN keep ((v,e):il) =
      let subst = extendIdSubst (mkSubst isN) v e
      in  inlineBndrs isN
            (map (second (substTm "inlineCleanup.inlineBndrs" subst)) keep)
            (map (second (substTm "inlineCleanup.inlineBndrs" subst)) il)
      -- We must not forget to inline the /current/ @to-inline@ let-binding into
      -- the list of /remaining/ @to-inline@ let-bindings, because it might
      -- only occur in /remaining/ @to-inline@ bindings. If we don't, we would
      -- introduce free variables, because the @to-inline@ bindings are removed.

inlineCleanup _ e = return e

-- | Flatten's letrecs after `inlineCleanup`
--
-- `inlineCleanup` sometimes exposes additional possibilities for `caseCon`,
-- which then introduces let-bindings in what should be ANF. This transformation
-- flattens those nested let-bindings again.
--
-- NB: must only be called in the cleaning up phase.
flattenLet :: NormRewrite
flattenLet (TransformContext is0 _) letrec@(Letrec _ _) = do
  is1 <- unionInScope is0 <$> Lens.use globalInScope
  let (is2, Letrec binds body) = freshenTm is1 letrec
  binds' <- concat <$> mapM (go is2) binds
  case binds' of
    -- inline binders into the body when there's only a single binder
    [(id',e')] ->
      if id' `idOccursIn` e'
         -- Except when the binder is recursive!
         then return (Letrec binds' body)
         else let subst = extendIdSubst (mkSubst is2) id' e'
              in changed (substTm "flattenLet" subst body)
    _ -> return (Letrec binds' body)
  where
    go :: InScopeSet -> LetBinding -> NormalizeSession [LetBinding]
    go isN (id_,Letrec binds' body') = case binds' of
       -- inline binders into the body when there's only a single binder
      [(id',e')] ->
        if id' `idOccursIn` e'
           -- Except when the binder is recursive!
           then changed [(id',e'),(id_, body')]
           else let subst = extendIdSubst (mkSubst isN) id' e'
                in  changed [(id_, (substTm "flattenLetGo" subst body'))]
      bs -> changed (bs ++ [(id_, body')])
    go _ b = return [b]

flattenLet _ e = return e
