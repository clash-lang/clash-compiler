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
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.Writer
  (WriterT (..), censor, lift, listen, tell)
import           Control.Monad.Trans.Except  (runExcept)
import           Data.Bits                   ((.&.), complement)
import qualified Data.Either                 as Either
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.HashSet                as HashSet
import qualified Data.List                   as List
import qualified Data.Maybe                  as Maybe
import qualified Data.Monoid                 as Monoid
import qualified Data.Primitive.ByteArray    as BA
import qualified Data.Set                    as Set
import qualified Data.Set.Lens               as Lens
import           Data.Text                   (Text, unpack)
import qualified Data.Vector.Primitive       as PV
import           Debug.Trace                 (trace)
import           GHC.Integer.GMP.Internals   (Integer (..), BigNat (..))
import           Unbound.Generics.LocallyNameless
  (Bind, Embed (..), bind, embed, rec, runFreshM, unbind, unembed, unrebind, unrec)
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           BasicTypes                  (InlineSpec (..))

import           Clash.Core.DataCon          (DataCon (..))
import           Clash.Core.Evaluator        (whnf')
import           Clash.Core.Name
  (Name (..), NameSort (..), name2String, string2InternalName, string2SystemName)
import           Clash.Core.FreeVars         (termFreeIds, termFreeTyVars,
                                              typeFreeVars)
import           Clash.Core.Literal          (Literal (..))
import           Clash.Core.Pretty           (showDoc)
import           Clash.Core.Subst
  (substBndr, substTm, substTms, substTyInTm, substTysinTm)
import           Clash.Core.Term             (LetBinding, Pat (..), Term (..), TmOccName)
import           Clash.Core.Type             (TypeView (..), applyFunTy,
                                              applyTy, isPolyFunCoreTy,
                                              normalizeType,
                                              splitFunTy, typeKind,
                                              tyView, undefinedTy)
import           Clash.Core.TyCon            (tyConDataCons)
import           Clash.Core.Util
  (collectArgs, idToVar, isClockOrReset, isCon, isFun, isLet, isPolyFun, isPrim,
   isSignalType, isVar, mkApps, mkLams, mkVec, termSize, termType,
   tyNatSize)
import           Clash.Core.Var              (Id, Var (..))
import           Clash.Driver.Types          (DebugLevel (..), ClashException (..))
import           Clash.Netlist.BlackBox.Util (usedArguments)
import           Clash.Netlist.Types         (HWType (..))
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
import           Clash.Util

inlineOrLiftNonRep :: NormRewrite
inlineOrLiftNonRep = inlineOrLiftBinders nonRepTest inlineTest
  where
    nonRepTest :: (Var Term, Embed Term) -> RewriteMonad extra Bool
    nonRepTest ((Id _ tyE), _)
      = not <$> (representableType <$> Lens.view typeTranslator
                                   <*> Lens.view customReprs
                                   <*> pure False
                                   <*> Lens.view tcCache
                                   <*> pure (unembed tyE))
    nonRepTest _ = return False

    inlineTest :: Term -> (Var Term, Embed Term) -> RewriteMonad extra Bool
    inlineTest e (id_@(Id (nameOcc -> idName) _), exprE)
      = let e' = unembed exprE
        in  not . or <$> sequence -- We do __NOT__ inline:
              [ -- 1. recursive let-binders
                elem idName <$> (Lens.toListOf <$> localFreeIds <*> pure e')
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
          Letrec b -> do
            -- It is safe to use unsafeUnbind because the expression @e@ is
            -- the original let-expression, unbound and bound again, so no
            -- bound variables have changed.
            let (_,res) = unsafeUnbind b
            fvOcc <-Lens.toListOf <$> localFreeIds <*> pure res
            return (length $ filter (== idName) fvOcc)
          _ -> return 0

    inlineTest _ _ = return True

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
  | (Var _ _,  args) <- collectArgs e1
  , null $ Lens.toListOf typeFreeVars ty
  , (_, []) <- Either.partitionEithers args
  = specializeNorm ctx e

typeSpec _ e = return e

-- | Specialize functions on their non-representable argument
nonRepSpec :: NormRewrite
nonRepSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  = do tcm <- Lens.view tcCache
       e2Ty <- termType tcm e2
       localVar <- isLocalVar e2
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
      | (Var _ f,fArgs) <- collectArgs app
      = do
        fTmM <- fmap (HashMap.lookup (nameOcc f)) $ Lens.use bindings
        case fTmM of
          Just (fNm,_,_,_,tm)
            | nameSort fNm == Internal
            -> do
              tm' <- censor (const mempty) (bottomupR appProp ctx (mkApps tm fArgs))
              return tm'
          _ -> return app
      | otherwise = return app

nonRepSpec _ e = return e

-- | Lift the let-bindings out of the subject of a Case-decomposition
caseLet :: NormRewrite
caseLet _ (Case (Letrec b) ty alts) = do
  (xes,e) <- unbind b
  changed (Letrec (bind xes (Case e ty alts)))

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
      then do newAlts <- mapM ( return
                                  . uncurry bind
                                  . second (\altE -> Case altE alts2Ty alts2)
                                  <=< unbind
                                  ) alts1
              changed $ Case scrut alts2Ty newAlts
      else return e

caseCase _ e = return e

-- | Inline function with a non-representable result if it's the subject
-- of a Case-decomposition
inlineNonRep :: NormRewrite
inlineNonRep _ e@(Case scrut altsTy alts)
  | (Var _ (nameOcc -> f), args) <- collectArgs scrut
  = do
    (nameOcc -> cf,_)    <- Lens.use curFun
    isInlined <- zoomExtra (alreadyInlined f cf)
    limit     <- Lens.use (extra.inlineLimit)
    tcm       <- Lens.view tcCache
    scrutTy   <- termType tcm scrut
    let noException = not (exception tcm scrutTy)
    if noException && (Maybe.fromMaybe 0 isInlined) > limit
      then do
        ty <- termType tcm scrut
        traceIf True (concat [$(curLoc) ++ "InlineNonRep: " ++ show f
                             ," already inlined " ++ show limit ++ " times in:"
                             , show cf
                             , "\nType of the subject is: " ++ showDoc ty
                             , "\nFunction " ++ show cf
                             , " will not reach a normal form, and compilation"
                             , " might fail."
                             , "\nRun with '-fclash-inline-limit=N' to increase"
                             , " the inlining limit to N."
                             ])
                     (return e)
      else do
        bodyMaybe   <- fmap (HashMap.lookup f) $ Lens.use bindings
        nonRepScrut <- not <$> (representableType <$> Lens.view typeTranslator
                                                  <*> Lens.view customReprs
                                                  <*> pure False
                                                  <*> Lens.view tcCache
                                                  <*> pure scrutTy)
        case (nonRepScrut, bodyMaybe) of
          (True,Just (_,_,_,_,scrutBody)) -> do
            Monad.when noException (zoomExtra (addNewInline f cf))
            changed $ Case (mkApps scrutBody args) altsTy alts
          _ -> return e
  where
    exception tcm ((tyView . typeKind tcm) -> TyConApp (name2String -> "GHC.Types.Constraint") _) = True
    exception _ _ = False

inlineNonRep _ e = return e

-- | Specialize a Case-decomposition (replace by the RHS of an alternative) if
-- the subject is (an application of) a DataCon; or if there is only a single
-- alternative that doesn't reference variables bound by the pattern.
caseCon :: NormRewrite
caseCon _ (Case scrut ty alts)
  | (Data dc, args) <- collectArgs scrut
  = do
    alts' <- mapM unbind alts
    let dcAltM = List.find (equalCon dc . fst) alts'
    case dcAltM of
      Just (DataPat _ pxs, e) ->
        let (tvs,xs) = unrebind pxs
            fvs = Lens.toListOf termFreeIds e
            (binds,_) = List.partition ((`elem` fvs) . nameOcc . varName . fst)
                      $ zip xs (Either.lefts args)
            e' = case binds of
                  [] -> e
                  _  -> Letrec $ bind (rec $ map (second embed) binds) e
            substTyMap = zip (map (nameOcc.varName) tvs) (drop (length $ dcUnivTyVars dc) (Either.rights args))
        in  changed (substTysinTm substTyMap e')
      _ -> case alts' of
             ((DefaultPat,e):_) -> changed e
             _ -> changed (mkApps (Prim "Clash.Transformations.undefined" undefinedTy) [Right ty])
  where
    equalCon dc (DataPat dc' _) = dcTag dc == dcTag (unembed dc')
    equalCon _  _               = False

caseCon _ c@(Case (Literal l) _ alts) = do
  alts' <- mapM unbind alts
  let ltAltsM = List.find (equalLit . fst) alts'
  case ltAltsM of
    Just (LitPat _,e) -> changed e
    _ -> matchLiteralContructor c l alts'
  where
    equalLit (LitPat l')     = l == (unembed l')
    equalLit _               = False

caseCon ctx e@(Case subj ty alts)
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
    case whnf' primEval bndrs tcm gh ids1 True subj of
      (gh',v) -> globalHeap Lens..= gh' >> case v of
        Literal l -> caseCon ctx (Case (Literal l) ty alts)
        subj' -> case collectArgs subj' of
          (Data _,_) -> caseCon ctx (Case subj' ty alts)
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
          (Prim nm _,[])
            | nm `elem` ["EmptyCase"] ->
              changed (Prim nm ty)
          _ -> do
            subjTy <- termType tcm subj
            tran   <- Lens.view typeTranslator
            case coreTypeToHWType tran reprs tcm False subjTy of
              Right (Void (Just hty))
                | hty `elem` [BitVector 0, Unsigned 0, Signed 0, Index 1]
                -> caseCon ctx (Case (Literal (IntegerLiteral 0)) ty alts)
              _ -> traceIf (lvl > DebugNone && isConstant subj)
                     ("Irreducible constant as case subject: " ++ showDoc subj ++ "\nCan be reduced to: " ++ showDoc subj')
                     (caseOneAlt e)

caseCon ctx e@(Case subj ty alts) = do
  reprs <- Lens.view customReprs
  tcm <- Lens.view tcCache
  subjTy <- termType tcm subj
  tran <- Lens.view typeTranslator
  case coreTypeToHWType tran reprs tcm False subjTy of
    Right (Void (Just hty))
      | hty `elem` [BitVector 0, Unsigned 0, Signed 0, Index 1]
      -> caseCon ctx (Case (Literal (IntegerLiteral 0)) ty alts)
    _ -> caseOneAlt e

caseCon _ e = return e

matchLiteralContructor
  :: Term
  -> Literal
  -> [(Pat,Term)]
  -> NormalizeSession Term
matchLiteralContructor c (IntegerLiteral l) alts = go (reverse alts)
 where
  go [(DefaultPat,e)] = changed e
  go ((DataPat dc pxs,e):alts')
    | dcTag (unembed dc) == 1
    , l >= ((-2)^(63::Int)) &&  l < 2^(63::Int)
    = let ([],xs)   = unrebind pxs
          fvs       = Lens.toListOf  termFreeIds e
          (binds,_) = List.partition ((`elem` fvs) . nameOcc . varName . fst)
                    $ zip xs [Literal (IntLiteral l)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec $ bind (rec $ map (second embed) binds) e
      in changed e'
    | dcTag (unembed dc) == 2
    , l >= 2^(63::Int)
    = let !(Jp# !(BN# ba)) = l
          ba'       = BA.ByteArray ba
          bv        = PV.Vector 0 (BA.sizeofByteArray ba') ba'
          ([],xs)   = unrebind pxs
          fvs       = Lens.toListOf  termFreeIds e
          (binds,_) = List.partition ((`elem` fvs) . nameOcc . varName . fst)
                    $ zip xs [Literal (ByteArrayLiteral bv)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec $ bind (rec $ map (second embed) binds) e
      in changed e'
    | dcTag (unembed dc) == 3
    , l < ((-2)^(63::Int))
    = let !(Jn# !(BN# ba)) = l
          ba'       = BA.ByteArray ba
          bv        = PV.Vector 0 (BA.sizeofByteArray ba') ba'
          ([],xs)   = unrebind pxs
          fvs       = Lens.toListOf  termFreeIds e
          (binds,_) = List.partition ((`elem` fvs) . nameOcc . varName . fst)
                    $ zip xs [Literal (ByteArrayLiteral bv)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec $ bind (rec $ map (second embed) binds) e
      in changed e'
    | otherwise
    = go alts'
  go _ = error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showDoc c

matchLiteralContructor c (NaturalLiteral l) alts = go (reverse alts)
 where
  go [(DefaultPat,e)] = changed e
  go ((DataPat dc pxs,e):alts')
    | dcTag (unembed dc) == 1
    , l >= 0 && l < 2^(64::Int)
    = let ([],xs)   = unrebind pxs
          fvs       = Lens.toListOf  termFreeIds e
          (binds,_) = List.partition ((`elem` fvs) . nameOcc . varName . fst)
                    $ zip xs [Literal (WordLiteral l)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec $ bind (rec $ map (second embed) binds) e
      in changed e'
    | dcTag (unembed dc) == 2
    , l >= 2^(64::Int)
    = let !(Jp# !(BN# ba)) = l
          ba'       = BA.ByteArray ba
          bv        = PV.Vector 0 (BA.sizeofByteArray ba') ba'
          ([],xs)   = unrebind pxs
          fvs       = Lens.toListOf  termFreeIds e
          (binds,_) = List.partition ((`elem` fvs) . nameOcc . varName . fst)
                    $ zip xs [Literal (ByteArrayLiteral bv)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec $ bind (rec $ map (second embed) binds) e
      in changed e'
    | otherwise
    = go alts'
  go _ = error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showDoc c

matchLiteralContructor _ _ ((DefaultPat,e):_) = changed e
matchLiteralContructor c _ _ =
  error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showDoc c

caseOneAlt :: Term -> RewriteMonad extra Term
caseOneAlt e@(Case _ _ [alt]) = do
  (pat,altE) <- unbind alt
  case pat of
    DefaultPat    -> changed altE
    LitPat _      -> changed altE
    DataPat _ pxs -> let (tvs,xs)   = unrebind pxs
                         ftvs       = Lens.toListOf termFreeTyVars altE
                         fvs        = Lens.toListOf termFreeIds altE
                         usedTvs    = filter ((`elem` ftvs) . nameOcc . varName) tvs
                         usedXs     = filter ((`elem` fvs) . nameOcc . varName) xs
                     in  case (usedTvs,usedXs) of
                           ([],[]) -> changed altE
                           _       -> return e

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
      (True,Letrec b) -> do (binds,body) <- unbind b
                            changed (Letrec (bind binds (App appConPrim body)))
      (True,Case {})  -> specializeNorm ctx e
      (True,Lam _)    -> specializeNorm ctx e
      (True,TyLam _)  -> specializeNorm ctx e
      _               -> return e

nonRepANF _ e = return e

-- | Ensure that top-level lambda's eventually bind a let-expression of which
-- the body is a variable-reference.
topLet :: NormRewrite
topLet ctx e
  | all isLambdaBodyCtx ctx && not (isLet e)
  = do
  untranslatable <- isUntranslatable False e
  if untranslatable
    then return e
    else do tcm <- Lens.view tcCache
            (argId,argVar) <- mkTmBinderFor tcm (string2SystemName "result") e
            changed . Letrec $ bind (rec [(argId,embed e)]) argVar

topLet ctx e@(Letrec b)
  | all isLambdaBodyCtx ctx
  = do
    (binds,body)   <- unbind b
    localVar       <- isLocalVar body
    untranslatable <- isUntranslatable False body
    if localVar || untranslatable
      then return e
      else do tcm <- Lens.view tcCache
              (argId,argVar) <- mkTmBinderFor tcm (string2SystemName "result") body
              changed . Letrec $ bind (rec $ unrec binds ++ [(argId,embed body)]) argVar

topLet _ e = return e

-- Misc rewrites

-- | Remove unused let-bindings
deadCode :: NormRewrite
deadCode _ e@(Letrec binds) = do
    (xes, body) <- fmap (first unrec) $ unbind binds
    let bodyFVs = Lens.toListOf termFreeIds body
        (xesUsed,xesOther) = List.partition
                               ( (`elem` bodyFVs )
                               . nameOcc
                               . varName
                               . fst
                               ) xes
        xesUsed' = findUsedBndrs [] xesUsed xesOther
    if length xesUsed' /= length xes
      then case xesUsed' of
              [] -> changed body
              _  -> changed . Letrec $ bind (rec xesUsed') body
      else return e
  where
    findUsedBndrs :: [(Var Term, Embed Term)] -> [(Var Term, Embed Term)]
                  -> [(Var Term, Embed Term)] -> [(Var Term, Embed Term)]
    findUsedBndrs used []      _     = used
    findUsedBndrs used explore other =
      let fvsUsed = concatMap (Lens.toListOf termFreeIds . unembed . snd) explore
          (explore',other') = List.partition
                                ( (`elem` fvsUsed)
                                . nameOcc
                                . varName
                                . fst
                                ) other
      in findUsedBndrs (used ++ explore) explore' other'

deadCode _ e = return e

removeUnusedExpr :: NormRewrite
removeUnusedExpr _ e@(collectArgs -> (p@(Prim nm _),args)) = do
  bbM <- HashMap.lookup nm <$> Lens.use (extra.primitives)
  case bbM of
    Just (BlackBox pNm _ _ _ _ inc templ) -> do
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
      ty <- termType tcm tm
      let p' = mkApps (Prim "Clash.Transformations.removedArg" undefinedTy) [Right ty]
      if n `elem` used
         then return (Left tm : args'')
         else return (Left p' : args'')

removeUnusedExpr _ e@(Case _ _ [alt]) = do
  (pat,altExpr) <- unbind alt
  case pat of
    DataPat _ (unrebind -> ([],xs)) -> do
      let altFreeIds = Lens.setOf termFreeIds altExpr
      if Set.null (Set.intersection (Set.fromList (map (nameOcc.varName) xs)) altFreeIds)
         then changed altExpr
         else return e
    _ -> return e

-- Replace any expression that creates a Vector of size 0 within the application
-- of the Cons constructor, by the Nil constructor.
removeUnusedExpr _ e@(collectArgs -> (Data dc, [_,Right aTy,Right nTy,_,Left a,Left nil]))
  | name2String (dcName dc) == "Clash.Sized.Vector.Cons"
  = do
    tcm <- Lens.view tcCache
    case runExcept (tyNatSize tcm nTy) of
      Right 0
        | (con, _) <- collectArgs nil
        , not (isCon con)
        -> do eTy <- termType tcm e
              let (TyConApp vecTcNm _) = tyView eTy
                  (Just vecTc) = HashMap.lookup (nameOcc vecTcNm) tcm
                  [nilCon,consCon] = tyConDataCons vecTc
                  v = mkVec nilCon consCon aTy 1 [a]
              changed v
      _ -> return e

removeUnusedExpr _ e = return e

-- | Inline let-bindings when the RHS is either a local variable reference or
-- is constant (except clock or reset generators)
bindConstantVar :: NormRewrite
bindConstantVar = inlineBinders test
  where
    test _ (_,Embed e) = isLocalVar e >>= \case
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
  alts' <- mapM castAlt alts
  changed $ Case subj ty alts'
    where
      castAlt alt = do
        (pat,altExpr) <- unbind alt
        return $ bind pat (Cast altExpr ty1 ty2)
caseCast _ e = return e

-- | Push a cast over a Letrec into it's body
letCast :: NormRewrite
letCast _ (Cast (Letrec b) ty1 ty2) = do
  let (binds,body) = unsafeUnbind b
  changed $ Letrec $ bind binds (Cast body ty1 ty2)
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
  Var _ _ -> go
  Cast (Var _ _) _ _ -> go
  _ -> warn go
  where
    go = specializeNorm ctx e
    warn = trace (unlines ["WARNING: " ++ $(curLoc) ++ "specializing a function on a possibly non work-free cast."
                          ,"Generated HDL implementation might contain duplicate work."
                          ,"Please report this as a bug."
                          ,""
                          ,"Expression where this occurs:"
                          ,showDoc e
                          ])
argCastSpec _ e = return e

-- | Only inline casts that just contain a 'Var', because these are guaranteed work-free.
-- These are the result of the 'splitCastWork' transformation.
inlineCast :: NormRewrite
inlineCast = inlineBinders test
  where
    test _ (_, Embed (Cast (Var _ _) _ _)) = return True
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
          throw (ClashException sp ($(curLoc) ++ showDoc nm
                  ++ ": Found 2 nested casts whose types don't line up:\n"
                  ++ showDoc c)
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
splitCastWork ctx unchanged@(Letrec b) = do
  (v,e') <- unbind b
  let vs = unrec v
  (vss', Monoid.getAny -> hasChanged) <- listen (mapM splitCastLetBinding vs)
  let vs' = concat vss'
  if hasChanged then changed . Letrec $ bind (rec vs') (e')
                else return unchanged
  where
    splitCastLetBinding :: LetBinding -> RewriteMonad extra [LetBinding]
    splitCastLetBinding x@(nm, Embed e) = case e of
      Cast (Var _ _) _ _    -> return [x]  -- already work-free
      Cast (Cast _ _ _) _ _ -> return [x]  -- casts will be eliminated
      Cast e' ty1 ty2 -> do
        tcm <- Lens.view tcCache
        (nm',var) <- mkTmBinderFor tcm (mkDerivedName ctx (name2String $ varName nm)) e'
        changed [(nm',Embed e')
                ,(nm, Embed $ Cast var ty1 ty2)
                ]
      _ -> return [x]

splitCastWork _ e = return e


-- | Inline work-free functions, i.e. fully applied functions that evaluate to
-- a constant
inlineWorkFree :: NormRewrite
inlineWorkFree _ e@(collectArgs -> (Var _ (nameOcc -> f),args))
  = do
    tcm <- Lens.view tcCache
    eTy <- termType tcm e
    argsHaveWork <- or <$> mapM (either expressionHasWork
                                        (const (pure False)))
                                args
    untranslatable <- isUntranslatableType True eTy
    let isSignal = isSignalType tcm eTy
    if untranslatable || isSignal || argsHaveWork
      then return e
      else do
        bndrs <- Lens.use bindings
        case HashMap.lookup f bndrs of
          -- Don't inline recursive expressions
          Just (_,_,_,_,body) -> do
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
      e'Ty  <- termType tcm e'
      let isSignal = isSignalType tcm e'Ty
      return (not (null fvIds) || isSignal)

inlineWorkFree _ e@(Var fTy (nameOcc -> f)) = do
  tcm <- Lens.view tcCache
  let closed   = not (isPolyFunCoreTy tcm fTy)
      isSignal = isSignalType tcm fTy
  untranslatable <- isUntranslatableType True fTy
  if closed && not untranslatable && not isSignal
    then do
      bndrs <- Lens.use bindings
      case HashMap.lookup f bndrs of
        -- Don't inline recursive expressions
        Just (_,_,_,_,body) -> do
          isRecBndr <- isRecursiveBndr f
          if isRecBndr
             then return e
             else changed body
        _ -> return e
    else return e

inlineWorkFree _ e = return e

-- | Inline small functions
inlineSmall :: NormRewrite
inlineSmall _ e@(collectArgs -> (Var _ (nameOcc -> f),args)) = do
  untranslatable <- isUntranslatable True e
  topEnts <- Lens.view topEntities
  if untranslatable || f `HashSet.member` topEnts
    then return e
    else do
      bndrs <- Lens.use bindings
      sizeLimit <- Lens.use (extra.inlineFunctionLimit)
      case HashMap.lookup f bndrs of
        -- Don't inline recursive expressions
        Just (_,_,_,inl,body) -> do
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
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  , isConstant e2
  = do tcm <- Lens.view tcCache
       e2Ty <- termType tcm e2
       -- Don't specialise on clock or reset generators
       case isClockOrReset tcm e2Ty of
          False -> specializeNorm ctx e
          _ -> return e

constantSpec _ e = return e


-- Experimental

-- | Propagate arguments of application inwards; except for 'Lam' where the
-- argument becomes let-bound.
appProp :: NormRewrite
appProp _ (App (Lam b) arg) = do
  (v,e) <- unbind b
  if isConstant arg || isVar arg
    then changed $ substTm (nameOcc (varName v)) arg e
    else changed . Letrec $ bind (rec [(v,embed arg)]) e

appProp _ (App (Letrec b) arg) = do
  (v,e) <- unbind b
  changed . Letrec $ bind v (App e arg)

appProp ctx (App (Case scrut ty alts) arg) = do
  tcm <- Lens.view tcCache
  argTy <- termType tcm arg
  let ty' = applyFunTy tcm ty argTy
  if isConstant arg || isVar arg
    then do
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` arg)
                    <=< unbind
                    ) alts
      changed $ Case scrut ty' alts'
    else do
      (boundArg,argVar) <- mkTmBinderFor tcm (mkDerivedName ctx "app_arg") arg
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` argVar)
                    <=< unbind
                    ) alts
      changed . Letrec $ bind (rec [(boundArg,embed arg)]) (Case scrut ty' alts')

appProp _ (TyApp (TyLam b) t) = do
  (tv,e) <- unbind b
  changed $ substTyInTm (nameOcc (varName tv)) t e

appProp _ (TyApp (Letrec b) t) = do
  (v,e) <- unbind b
  changed . Letrec $ bind v (TyApp e t)

appProp _ (TyApp (Case scrut altsTy alts) ty) = do
  alts' <- mapM ( return
                . uncurry bind
                . second (`TyApp` ty)
                <=< unbind
                ) alts
  tcm <- Lens.view tcCache
  ty' <- applyTy tcm altsTy ty
  changed $ Case scrut ty' alts'

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

collectFlat :: Term -> Term -> Maybe [Bind Pat Term]
collectFlat scrut (Case (collectEqArgs -> Just (scrut', val)) _ty [lAlt,rAlt])
  | scrut' == scrut
  = case collectArgs val of
      (Prim nm' _,args') | isFromInt nm'
        -> case last args' of
            Left (Literal i) -> case (unsafeUnbind lAlt,unsafeUnbind rAlt) of
              ((pl,el),(pr,er))
                | isFalseDcPat pl || isTrueDcPat pr ->
                   case collectFlat scrut el of
                     Just alts' -> Just (bind (LitPat (embed i)) er : alts')
                     Nothing    -> Just [bind (LitPat (embed i)) er
                                        ,bind DefaultPat el
                                        ]
                | otherwise ->
                   case collectFlat scrut er of
                     Just alts' -> Just (bind (LitPat (embed i)) el : alts')
                     Nothing    -> Just [bind (LitPat (embed i)) el
                                        ,bind DefaultPat er
                                        ]
            _ -> Nothing
      _ -> Nothing
  where
    isFalseDcPat (DataPat p _)
      = ((== "GHC.Types.False") . name2String . dcName . unembed) p
    isFalseDcPat _ = False

    isTrueDcPat (DataPat p _)
      = ((== "GHC.Types.True") . name2String . dcName . unembed) p
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

type NormRewriteW = Transform (WriterT [LetBinding] (RewriteMonad NormalizeState))

-- NOTE [unsafeUnbind]: Use unsafeUnbind (which doesn't freshen pattern
-- variables). Reason: previously collected expression still reference
-- the 'old' variable names created by the traversal!

-- | Turn an expression into a modified ANF-form. As opposed to standard ANF,
-- constants do not become let-bound.
makeANF :: NormRewrite
makeANF ctx (Lam b) = do
  -- See NOTE [unsafeUnbind]
  let (bndr,e) = unsafeUnbind b
  e' <- makeANF (LamBody bndr:ctx) e
  return $ Lam (bind bndr e')

makeANF _ (TyLam b) = return (TyLam b)

makeANF ctx e
  = do
    (e',bndrs) <- runWriterT $ bottomupR collectANF ctx e
    case bndrs of
      [] -> return e
      _  -> changed . Letrec $ bind (rec bndrs) e'

collectANF :: NormRewriteW
collectANF ctx e@(App appf arg)
  | (conVarPrim, _) <- collectArgs e
  , isCon conVarPrim || isPrim conVarPrim || isVar conVarPrim
  = do
    untranslatable <- lift (isUntranslatable False arg)
    localVar       <- lift (isLocalVar arg)
    constantNoCR   <- lift (isConstantNotClockReset arg)
    case (untranslatable,localVar || constantNoCR,arg) of
      (False,False,_) -> do tcm <- Lens.view tcCache
                            (argId,argVar) <- lift (mkTmBinderFor tcm (mkDerivedName ctx "app_arg") arg)
                            tell [(argId,embed arg)]
                            return (App appf argVar)
      (True,False,Letrec b) -> do (binds,body) <- unbind b
                                  tell (unrec binds)
                                  return (App appf body)
      _ -> return e

collectANF _ (Letrec b) = do
  -- See NOTE [unsafeUnbind]
  let (binds,body) = unsafeUnbind b
  tell (unrec binds)
  untranslatable <- lift (isUntranslatable False body)
  localVar       <- lift (isLocalVar body)
  if localVar || untranslatable
    then return body
    else do
      tcm <- Lens.view tcCache
      (argId,argVar) <- lift (mkTmBinderFor tcm (string2SystemName "result") body)
      tell [(argId,embed body)]
      return argVar

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
collectANF _ e@(Case _ _ [unsafeUnbind -> (DataPat dc _,_)])
  | name2String (dcName $ unembed dc) == "Clash.Signal.Internal.:-" = return e

collectANF ctx (Case subj ty alts) = do
    localVar     <- lift (isLocalVar subj)
    (bndr,subj') <- if localVar || isConstant subj
      then return ([],subj)
      else do tcm <- Lens.view tcCache
              (argId,argVar) <- lift (mkTmBinderFor tcm (mkDerivedName ctx "case_scrut") subj)
              return ([(argId,embed subj)],argVar)

    (binds,alts') <- fmap (first concat . unzip) $ mapM (lift . doAlt subj') alts

    tell (bndr ++ binds)
    case alts' of
      [unsafeUnbind -> (DataPat _ (unrebind -> ([],xs)),altExpr)]
        | let altFreeIds = Lens.setOf termFreeIds altExpr
        , Set.null (Set.intersection (Set.fromList (map (nameOcc.varName) xs)) altFreeIds)
        -> return altExpr
      _ -> return (Case subj' ty alts')
  where
    doAlt :: Term -> Bind Pat Term -> RewriteMonad NormalizeState ([LetBinding],Bind Pat Term)
    -- See NOTE [unsafeUnbind]
    doAlt subj' = fmap (second (uncurry bind)) . doAlt' subj' . unsafeUnbind

    doAlt' :: Term -> (Pat,Term) -> RewriteMonad NormalizeState ([LetBinding],(Pat,Term))
    doAlt' subj' alt@(DataPat dc pxs@(unrebind -> ([],xs)),altExpr) = do
      lv      <- isLocalVar altExpr
      patSels <- Monad.zipWithM (doPatBndr subj' (unembed dc)) xs [0..]
      let usesXs (Var _ n) = any ((== n) . varName) xs
          usesXs _         = False
      if (lv && not (usesXs altExpr)) || isConstant altExpr
        then return (patSels,alt)
        else do tcm <- Lens.view tcCache
                (altId,altVar) <- mkTmBinderFor tcm (mkDerivedName ctx "case_alt") altExpr
                return ((altId,embed altExpr):patSels,(DataPat dc pxs,altVar))
    doAlt' _ alt@(DataPat _ _, _) = return ([],alt)
    doAlt' _ alt@(pat,altExpr) = do
      lv <- isLocalVar altExpr
      if lv || isConstant altExpr
        then return ([],alt)
        else do tcm <- Lens.view tcCache
                (altId,altVar) <- mkTmBinderFor tcm (mkDerivedName ctx "case_alt") altExpr
                return ([(altId,embed altExpr)],(pat,altVar))

    doPatBndr :: Term -> DataCon -> Id -> Int -> RewriteMonad NormalizeState LetBinding
    doPatBndr subj' dc pId i
      = do tcm <- Lens.view tcCache
           patExpr <- mkSelectorCase ($(curLoc) ++ "doPatBndr") tcm subj' (dcTag dc) i
           return (pId,embed patExpr)

collectANF _ e = return e

-- | Eta-expand top-level lambda's (DON'T use in a traversal!)
etaExpansionTL :: NormRewrite
etaExpansionTL ctx (Lam b) = do
  (bndr,e) <- unbind b
  e' <- etaExpansionTL (LamBody bndr:ctx) e
  return $ Lam (bind bndr e')

etaExpansionTL ctx (Letrec b) = do
  (xesR,e) <- unbind b
  let xes   = unrec xesR
      bndrs = map fst xes
  e' <- etaExpansionTL (LetBody bndrs:ctx) e
  e'' <- stripLambda e'
  case e'' of
    (bs@(_:_),e2) -> do
      let e3 = Letrec (bind xesR e2)
      changed (mkLams e3 bs)
    _ -> return (Letrec (bind xesR e'))
  where
    stripLambda :: Term -> RewriteMonad NormalizeState ([Id],Term)
    stripLambda (Lam b') = do
      (bndr,e)   <- unbind b'
      (bndrs,e') <- stripLambda e
      return (bndr:bndrs,e')
    stripLambda e = return ([],e)

etaExpansionTL ctx e
  = do
    tcm <- Lens.view tcCache
    isF <- isFun tcm e
    if isF
      then do
        argTy <- ( return
                 . fst
                 . Maybe.fromMaybe (error $ $(curLoc) ++ "etaExpansion splitFunTy")
                 . splitFunTy tcm
                 <=< termType tcm
                 ) e
        (newIdB,newIdV) <- mkInternalVar (string2InternalName "arg") argTy
        e' <- etaExpansionTL (LamBody newIdB:ctx) (App e newIdV)
        changed . Lam $ bind newIdB e'
      else return e

-- | Turn a  normalized recursive function, where the recursive calls only pass
-- along the unchanged original arguments, into let-recursive function. This
-- means that all recursive calls are replaced by the same variable reference as
-- found in the body of the top-level let-expression.
recToLetRec :: NormRewrite
recToLetRec [] e = do
  (fn,_)      <- Lens.use curFun
  bodyM       <- fmap (HashMap.lookup (nameOcc fn)) $ Lens.use bindings
  tcm         <- Lens.view tcCache
  normalizedE <- splitNormalized tcm e
  case (normalizedE,bodyM) of
    (Right (args,bndrs,res), Just (_,bodyTy,_,_,_)) -> do
      let v                 = Var bodyTy fn
          args'             = map idToVar args
          (toInline,others) = List.partition (eqApp tcm v args' . unembed . snd) bndrs
          resV              = idToVar res
      case (toInline,others) of
        (_:_,_:_) -> do
          let substsInline = map (\(id_,_) -> (nameOcc (varName id_),resV)) toInline
              others'      = map (second (embed . substTms substsInline . unembed)) others
          changed $ mkLams (Letrec $ bind (rec others') resV) args
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

    eqArg _ v1 v2@(Var _ _)
      = v1 == v2
    eqArg tcm v1 v2@(collectArgs -> (Data _,args'))
      | runFreshM (termType tcm v1) == runFreshM (termType tcm v2)
      = and (zipWith (isNthProjection v1) [0..] (Either.lefts args'))
    eqArg _ _ _
      = False

    -- `isNthProjection s n c` checks that `c` is the `n`th projection
    -- of `s`.
    isNthProjection :: Term -> Int -> Term -> Bool
    isNthProjection v n (Case v' altTy [alt])
      | v == v'
      , (DataPat _ pxs,Var _ s) <- unsafeUnbind alt
      , let (_,xs) = unrebind pxs
      , Just n' <- List.elemIndex (Id s (embed altTy)) xs
      = n == n'
    isNthProjection _ _ _ = False

recToLetRec _ e = return e

-- | Inline a function with functional arguments
inlineHO :: NormRewrite
inlineHO _ e@(App _ _)
  | (Var _ (nameOcc -> f), args) <- collectArgs e
  = do
    tcm <- Lens.view tcCache
    hasPolyFunArgs <- or <$> mapM (either (isPolyFun tcm) (const (return False))) args
    if hasPolyFunArgs
      then do (nameOcc -> cf,_)    <- Lens.use curFun
              isInlined <- zoomExtra (alreadyInlined f cf)
              limit     <- Lens.use (extra.inlineLimit)
              if (Maybe.fromMaybe 0 isInlined) > limit
                then do
                  lvl <- Lens.view dbgLevel
                  traceIf (lvl > DebugNone) ($(curLoc) ++ "InlineHO: " ++ show f ++ " already inlined " ++ show limit ++ " times in:" ++ show cf) (return e)
                else do
                  bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
                  case bodyMaybe of
                    Just (_,_,_,_,body) -> do
                      zoomExtra (addNewInline f cf)
                      changed (mkApps body args)
                    _ -> return e
      else return e

inlineHO _ e = return e

-- | Simplified CSE, only works on let-bindings, works from top to bottom
simpleCSE :: NormRewrite
simpleCSE _ e@(Letrec b) = do
  (binders,body) <- first unrec <$> unbind b
  let (reducedBindings,body') = reduceBindersFix binders body
  if length binders /= length reducedBindings
     then changed (Letrec (bind (rec reducedBindings) body'))
     else return e

simpleCSE _ e = return e

reduceBindersFix :: [LetBinding]
                 -> Term
                 -> ([LetBinding],Term)
reduceBindersFix binders body = if length binders /= length reduced
                                   then reduceBindersFix reduced body'
                                   else (binders,body)
  where
    (reduced,body') = reduceBinders [] body binders

reduceBinders :: [LetBinding]
              -> Term
              -> [LetBinding]
              -> ([LetBinding],Term)
reduceBinders processed body [] = (processed,body)
reduceBinders processed body ((id_,expr):binders) = case List.find ((== expr) . snd) processed of
    Just (id2,_) ->
      let var        = Var (unembed (varType id2)) (varName id2)
          idName     = nameOcc (varName id_)
          processed' = map (second (Embed . (substTm idName var) . unembed)) processed
          binders'   = map (second (Embed . (substTm idName var) . unembed)) binders
          body'      = substTm idName var body
      in  reduceBinders processed' body' binders'
    Nothing -> reduceBinders ((id_,expr):processed) body binders

reduceConst :: NormRewrite
reduceConst _ e@(App _ _)
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
    case whnf' primEval bndrs tcm gh ids1 False e of
      (gh',e') -> do
        globalHeap Lens..= gh'
        case e' of
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
reduceNonRepPrim _ e@(App _ _) | (Prim f _, args) <- collectArgs e = do
  tcm <- Lens.view tcCache
  eTy <- termType tcm e
  case tyView eTy of
    (TyConApp vecTcNm@(name2String -> "Clash.Sized.Vector.Vec")
              [runExcept . tyNatSize tcm -> Right 0, aTy]) -> do
      let (Just vecTc) = HashMap.lookup (nameOcc vecTcNm) tcm
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
                    in  reduceZipWith n lhsElTy rhsElty resElTy fun lhsArg rhsArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.map" | length args == 5 -> do
        let [argElTy,resElTy,nTy] = Either.rights args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTys <- mapM isUntranslatableType_not_poly [argElTy,resElTy]
            if or untranslatableTys
               then let [fun,arg] = Either.lefts args
                    in  reduceMap n argElTy resElTy fun arg
               else return e
          _ -> return e
      "Clash.Sized.Vector.traverse#" | length args == 7 ->
        let [aTy,fTy,bTy,nTy] = Either.rights args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n ->
            let [dict,fun,arg] = Either.lefts args
            in  reduceTraverse n aTy fTy bTy dict fun arg
          _ -> return e
      "Clash.Sized.Vector.fold" | length args == 4 -> do
        let [aTy,nTy] = Either.rights args
            isPow2 x  = x /= 0 && (x .&. (complement x + 1)) == x
        untranslatableTy <- isUntranslatableType_not_poly aTy
        case runExcept (tyNatSize tcm nTy) of
          Right n | not (isPow2 (n + 1)) || untranslatableTy ->
            let [fun,arg] = Either.lefts args
            in  reduceFold (n + 1) aTy fun arg
          _ -> return e
      "Clash.Sized.Vector.foldr" | length args == 6 ->
        let [aTy,bTy,nTy] = Either.rights args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTys <- mapM isUntranslatableType_not_poly [aTy,bTy]
            if or untranslatableTys
              then let [fun,start,arg] = Either.lefts args
                   in  reduceFoldr n aTy fun start arg
              else return e
          _ -> return e
      "Clash.Sized.Vector.dfold" | length args == 8 ->
        let ([_kn,_motive,fun,start,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> reduceDFold n aTy fun start arg
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
                       then reduceAppend n m aTy lArg rArg
                       else return e
              _ -> return e
      "Clash.Sized.Vector.head" | length args == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTy <- isUntranslatableType_not_poly aTy
            if untranslatableTy
               then reduceHead n aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.tail" | length args == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTy <- isUntranslatableType_not_poly aTy
            if untranslatableTy
               then reduceTail n aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.last" | length args == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTy <- isUntranslatableType_not_poly aTy
            if untranslatableTy
               then reduceLast n aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.init" | length args == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            untranslatableTy <- isUntranslatableType_not_poly aTy
            if untranslatableTy
               then reduceInit n aTy vArg
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
                    in  reduceImap n argElTy resElTy fun arg
               else return e
          _ -> return e
      "Clash.Sized.Vector.dtfold" | length args == 8 ->
        let ([_kn,_motive,lrFun,brFun,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> reduceDTFold n aTy lrFun brFun arg
          _ -> return e
      "Clash.Sized.RTree.tdfold" | length args == 8 ->
        let ([_kn,_motive,lrFun,brFun,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> reduceTFold n aTy lrFun brFun arg
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
              let (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
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
              let (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
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
        -> let (Just boolTc) = HashMap.lookup (nameOcc boolTcNm) tcm
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
disjointExpressionConsolidation ctx e@(Case _scrut _ty _alts@(_:_:_)) = do
    let eFreeIds = Lens.setOf termFreeIds e
    (_,collected) <- collectGlobals eFreeIds [] [] e
    let disJoint = filter (isDisjoint . snd. snd) collected
    if null disJoint
       then return e
       else do
         exprs <- mapM (mkDisjointGroup eFreeIds) disJoint
         tcm <- Lens.view tcCache
         (lids,lvs) <- unzip <$> Monad.zipWithM (mkFunOut tcm) disJoint exprs
         let substitution = zip (map fst disJoint) lvs
             subsMatrix   = l2m substitution
         (exprs',_) <- unzip <$> Monad.zipWithM (\s (e',seen) -> collectGlobals eFreeIds s seen e')
                                                subsMatrix
                                                exprs
         (e',_) <- collectGlobals eFreeIds substitution [] e
         let lb = Letrec (bind (rec (zip lids (map embed exprs'))) e')
         lb' <- bottomupR deadCode ctx lb
         changed lb'
  where
    mkFunOut tcm (fun,_) (e',_) = do
      ty <- termType tcm e'
      let nm  = case collectArgs fun of
                   (Var _ nm',_)  -> name2String nm'
                   (Prim nm' _,_) -> unpack nm'
                   _             -> "complex_expression_"
          nm'' = (reverse . List.takeWhile (/='.') . reverse) nm ++ "Out"
      mkInternalVar (string2InternalName nm'') ty

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
inlineCleanup _ (Letrec b) = do
  prims <- Lens.use (extra.primitives)
  let (bindsR,body) = unsafeUnbind b
      binds         = unrec bindsR
      -- For all let-bindings, count the number of times they are referenced.
      -- We only inline let-bindings which are referenced only once, otherwise
      -- we would lose sharing.
      allOccs       = List.foldl' (HashMap.unionWith (+)) HashMap.empty
                    $ map ( List.foldl' countOcc HashMap.empty
                          . Lens.toListOf termFreeIds . unembed . snd) binds
      bodyFVs       = Lens.toListOf termFreeIds body
      (il,keep)     = List.partition (isInteresting  allOccs prims bodyFVs) binds
      keep'         = inlineBndrs keep il
  if null il then return  (Letrec b)
             else changed (Letrec (bind (rec keep') body))
  where
    -- Count the number of occurrences of a variable
    countOcc
      :: HashMap.HashMap TmOccName Int
      -> TmOccName
      -> HashMap.HashMap TmOccName Int
    countOcc m nm = HashMap.insertWith (+) nm (1::Int) m

    -- Determine whether a let-binding is interesting to inline
    isInteresting
      :: HashMap.HashMap TmOccName Int
      -> PrimMap (Primitive a b c)
      -> [TmOccName]
      -> (Id,Embed Term)
      -> Bool
    isInteresting allOccs prims bodyFVs (id_,(fst.collectArgs.unembed) -> tm)
      | nameSort (varName id_) /= User
      , nameOcc (varName id_) `notElem` bodyFVs
      = case tm of
          Prim nm _
            | Just p@(BlackBox {}) <- HashMap.lookup nm prims
            , TExpr <- kind p
            , Just occ <- HashMap.lookup (nameOcc (varName id_)) allOccs
            , occ < 2
            -> True
          Case _ _ [_] -> True
          Data _ -> True
          _ -> False
      | nameOcc (varName id_) `notElem` bodyFVs
      = case tm of
          -- Inlines WW projection that exposes internals of the BitVector types
          Case _ _ [unsafeUnbind -> (DataPat dcE _,_)]
            -> let nm = (name2String $ dcName $ unembed dcE)
               in nm == "Clash.Sized.Internal.BitVector.BV"  ||
                  nm == "Clash.Sized.Internal.BitVector.Bit"
          _ -> False

    isInteresting _ _ _ _ = False

    -- Inline let-bindings we want to inline into let-bindings we want to keep.
    inlineBndrs
      :: [(Id, Embed Term)]
      -- let-bindings we keep
      -> [(Id, Embed Term)]
      -- let-bindings we want to inline
      -> [(Id, Embed Term)]
    inlineBndrs keep [] = keep
    inlineBndrs keep (((nameOcc . varName) -> nm,unembed -> tm):il) =
      inlineBndrs (map (substBndr nm tm) keep)
                  (map (substBndr nm tm) il)
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
flattenLet _ (Letrec b) = do
  let (binds,body) = unsafeUnbind b
  binds' <- concat <$> mapM go (unrec binds)
  case binds' of
    -- inline binders into the body when there's only a single binder
    [(id',e')] -> do
      let fvs = Lens.toListOf termFreeIds (unembed e')
          nm  = nameOcc (varName id')
      if nm `elem` fvs
         -- Except when the binder is recursive!
         then return (Letrec (bind (rec binds') body))
         else changed (substTm nm (unembed e') body)
    _ -> return (Letrec (bind (rec binds') body))
  where
    go :: LetBinding -> NormalizeSession [LetBinding]
    go (id_,e) = case unembed e of
      Letrec b' -> do
        let (binds,body) = unsafeUnbind b'
        case unrec binds of
          -- inline binders into the body when there's only a single binder
          [(id',e')] -> do
            let fvs = Lens.toListOf termFreeIds (unembed e')
                nm  = nameOcc (varName id')
            if nm `elem` fvs
               -- Except when the binder is recursive!
               then changed [(id',e'),(id_,embed body)]
               else changed [(id_,embed (substTm nm (unembed e') body))]
          bs -> changed (bs ++ [(id_,embed body)])
      _ -> return [(id_,e)]

flattenLet _ e = return e
