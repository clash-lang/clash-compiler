{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module CLaSH.Normalize.Transformations
  ( appProp
  , bindNonRep
  , liftNonRep
  , caseLet
  , caseCon
  , caseCase
  , inlineNonRep
  , typeSpec
  , nonRepSpec
  , etaExpansionTL
  , inlineClosedTerm
  , nonRepANF
  , bindConstantVar
  , constantSpec
  , makeANF
  , deadCode
  , topLet
  , inlineWrapper
  , classOpResolution
  , inlineSingularDFun
  , recToLetRec
  )
where

import           Control.Lens                ((%=))
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.Writer        (WriterT (..), lift, tell)
import qualified Data.Either                 as Either
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.List                   as List
import qualified Data.Maybe                  as Maybe
import           Unbound.LocallyNameless     (Bind, Embed (..), bind, embed,
                                              rec, unbind, unembed, unrebind,
                                              unrec)
import           Unbound.LocallyNameless.Ops (unsafeUnbind)

import           CLaSH.Core.DataCon          (DataCon, dcTag, dcUnivTyVars)
import           CLaSH.Core.FreeVars         (termFreeIds, termFreeTyVars,
                                              termFreeVars, typeFreeVars)
import           CLaSH.Core.Pretty           (showDoc)
import           CLaSH.Core.Prim             (Prim (..))
import           CLaSH.Core.Subst            (substTm, substTms, substTyInTm,
                                              substTysinTm)
import           CLaSH.Core.Term             (LetBinding, Pat (..), Term (..),
                                              TmName)
import           CLaSH.Core.Type             (TyName, Type, applyFunTy, applyTy,
                                              isDictType, splitFunTy)
import           CLaSH.Core.Util             (collectArgs, idToVar, isCon,
                                              isFun, isLet, isPrim, isVar,
                                              mkApps, mkLams, mkTmApps,
                                              termType)
import           CLaSH.Core.Var              (Id, Var (..))
import           CLaSH.Netlist.Util          (representableType,
                                              splitNormalized)
import           CLaSH.Normalize.Types
import           CLaSH.Normalize.Util
import           CLaSH.Rewrite.Combinators
import           CLaSH.Rewrite.Types
import           CLaSH.Rewrite.Util
import           CLaSH.Util

bindNonRep :: NormRewrite
bindNonRep = inlineBinders nonRepTest
  where
    nonRepTest (Id idName tyE, exprE)
      = (&&) <$> (not <$> (representableType <$> Lens.use typeTranslator <*> pure (unembed tyE)))
             <*> ((notElem idName . snd) <$> localFreeVars (unembed exprE))

    nonRepTest _ = return False

liftNonRep :: NormRewrite
liftNonRep = liftBinders nonRepTest
  where
    nonRepTest (Id idName tyE, exprE)
      = (&&) <$> (not <$> (representableType <$> Lens.use typeTranslator <*> pure (unembed tyE)))
             <*> ((elem idName . snd) <$> localFreeVars (unembed exprE))

    nonRepTest _ = return False


typeSpec :: NormRewrite
typeSpec ctx e@(TyApp e1 ty)
  | (Var _ _,  args) <- collectArgs e1
  , null $ typeFreeVars ty
  , (_, []) <- Either.partitionEithers args
  = specialise specialisations ctx e

typeSpec _ e = return e

nonRepSpec :: NormRewrite
nonRepSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ termFreeTyVars e2
  = R $ do e2Ty <- termType e2
           localVar <- isLocalVar e2
           nonRepE2 <- not <$> (representableType <$> Lens.use typeTranslator <*> pure e2Ty)
           if nonRepE2 && not localVar
             then runR $ specialise specialisations ctx e
             else return e

nonRepSpec _ e = return e

caseLet :: NormRewrite
caseLet _ (Case (Letrec b) ty alts) = R $ do
  (xes,e) <- unbind b
  changed . Letrec $ bind xes (Case e ty alts)

caseLet _ e = return e

caseCase :: NormRewrite
caseCase _ e@(Case (Case scrut ty1 alts1) ty2 alts2)
  = R $ do
    ty1Rep <- representableType <$> Lens.use typeTranslator <*> pure ty1
    if ty1Rep
      then do newAlts <- mapM ( return
                                  . uncurry bind
                                  . second (\altE -> Case altE ty2 alts2)
                                  <=< unbind
                                  ) alts1
              changed $ Case scrut ty2 newAlts
      else return e

caseCase _ e = return e

inlineNonRep :: NormRewrite
inlineNonRep ctx e@(Case scrut ty alts)
  | (Var _ f, args) <- collectArgs scrut
  = R $ do
    isInlined <- liftR $ alreadyInlined f
    if isInlined
      then do
        cf <- liftR $ Lens.use curFun
        traceIf True ($(curLoc) ++ "InlineNonRep: " ++ show f ++ " already inlined in: " ++ show cf) $ return e
      else do
        scrutTy     <- termType scrut
        bodyMaybe   <- fmap (HashMap.lookup f) $ Lens.use bindings
        nonRepScrut <- not <$> (representableType <$> Lens.use typeTranslator <*> pure scrutTy)
        case (nonRepScrut, bodyMaybe) of
          (True,Just (_, scrutBody)) -> do
            liftR $ newInlined %= (f:)
            changed $ Case (mkApps scrutBody args) ty alts
          _ -> return e

inlineNonRep _ e = return e

caseCon :: NormRewrite
caseCon _ (Case scrut ty alts)
  | (Data dc, args) <- collectArgs scrut
  = R $ do
    alts' <- mapM unbind alts
    let dcAltM = List.find (equalCon dc . fst) alts'
    case dcAltM of
      Just (DataPat _ pxs, e) ->
        let (tvs,xs) = unrebind pxs
            fvs = termFreeIds e
            (binds,_) = List.partition ((`elem` fvs) . varName . fst)
                      $ zip xs (Either.lefts args)
            e' = case binds of
                  [] -> e
                  _  -> Letrec $ bind (rec $ map (second embed) binds) e
            substTyMap = zip (map varName tvs) (drop (length $ dcUnivTyVars dc) (Either.rights args))
        in  changed (substTysinTm substTyMap e')
      Nothing -> do
        let defAltM = List.find (isDefPat . fst) alts'
        case defAltM of
          Just (DefaultPat, e) -> changed e
          Nothing -> error $ $(curLoc) ++ "Non-exhaustive case-statement"
          Just _ -> error $ $(curLoc) ++ "Report as bug: caseCon error"
      Just _ -> error $ $(curLoc) ++ "Report as bug: caseCon error"
  where
    equalCon dc (DataPat dc' _) = dcTag dc == dcTag (unembed dc')
    equalCon _  _               = False

    isDefPat DefaultPat = True
    isDefPat _          = False

caseCon _ e@(Case _ _ [alt]) = R $ do
  (pat,altE) <- unbind alt
  case pat of
    DefaultPat    -> changed altE
    DataPat _ pxs -> let (tvs,xs)   = unrebind pxs
                         (ftvs,fvs) = termFreeVars altE
                         usedTvs    = filter ((`elem` ftvs) . varName) tvs
                         usedXs     = filter ((`elem` fvs) . varName) xs
                     in  case (usedTvs,usedXs) of
                           ([],[]) -> changed altE
                           _       -> return e
    _             -> return e

caseCon _ e = return e

nonRepANF :: NormRewrite
nonRepANF ctx e@(App appConPrim arg)
  | (conPrim, _) <- collectArgs e
  , isCon conPrim || isPrim conPrim
  = R $ do
    localVar       <- isLocalVar arg
    untranslatable <- isUntranslatable arg
    case (localVar || not untranslatable,arg) of
      (False,Letrec b) -> do (binds,body) <- unbind b
                             changed . Letrec $ bind binds (App appConPrim body)
      (False,Case {})  -> runR $ specialise specialisations ctx e
      (False,Lam _)    -> runR $ specialise specialisations ctx e
      _                -> return e

nonRepANF _ e = return e

topLet :: NormRewrite
topLet ctx e
  | all isLambdaBodyCtx ctx && not (isLet e)
  = R $ do
  untranslatable <- isUntranslatable e
  if untranslatable
    then return e
    else do (argId,argVar) <- mkTmBinderFor "topLet" e
            changed . Letrec $ bind (rec [(argId,embed e)]) argVar

topLet ctx e@(Letrec b)
  | all isLambdaBodyCtx ctx
  = R $ do
    (binds,body)   <- unbind b
    localVar       <- isLocalVar body
    untranslatable <- isUntranslatable body
    if localVar || untranslatable
      then return e
      else do (argId,argVar) <- mkTmBinderFor "topLet" body
              changed . Letrec $ bind (rec $ unrec binds ++ [(argId,embed body)]) argVar

topLet _ e = return e

-- Misc rewrites
deadCode :: NormRewrite
deadCode _ e@(Letrec binds) = R $ do
    (xes, body) <- fmap (first unrec) $ unbind binds
    let bodyFVs = termFreeIds body
        (xesUsed,xesOther) = List.partition
                               ( (`elem` bodyFVs )
                               . varName
                               . fst
                               ) xes
        xesUsed' = findUsedBndrs [] xesUsed xesOther
    if length xesUsed' /= length xes
      then changed . Letrec $ bind (rec xesUsed') body
      else return e
  where
    findUsedBndrs used []      _     = used
    findUsedBndrs used explore other =
      let fvsUsed = concatMap (termFreeIds . unembed . snd) explore
          (explore',other') = List.partition
                                ( (`elem` fvsUsed)
                                . varName
                                . fst
                                ) other
      in findUsedBndrs (used ++ explore) explore' other'

deadCode _ e = return e

bindConstantVar :: NormRewrite
bindConstantVar = inlineBinders test
  where
    test (_,Embed e) = (||) <$> isLocalVar e <*> pure (isConstant e)

inlineClosedTerm :: NormRewrite
inlineClosedTerm _ e@(Var _ f) = R $ do
  bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
  case bodyMaybe of
    Just (_,body) -> do
      closed <- isClosed body
      untranslatable <- isUntranslatable body
      if closed && not untranslatable
        then changed body
        else return e
    _ -> return e

inlineClosedTerm _ e = return e

constantSpec :: NormRewrite
constantSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ termFreeTyVars e2
  , isConstant e2
  = specialise specialisations ctx e

constantSpec _ e = return e

inlineWrapper :: NormRewrite
inlineWrapper [] e = R $ do
  normalizedM <- splitNormalized e
  case normalizedM of
    Right (_,[(_,bExpr)],_) -> case collectArgs (unembed bExpr) of
      (Var _ fn,args) -> do allLocal <- fmap and $ mapM (either isLocalVar (\_ -> return True)) args
                            bodyMaybe <- fmap (HashMap.lookup fn) $ Lens.use bindings
                            case (bodyMaybe,allLocal) of
                              (Just (bodyTy,body),True) -> do
                                eTy <- termType e
                                if eTy == bodyTy
                                  then changed body
                                  else return e
                              _ -> return e
      _ -> return e
    _ -> return e

inlineWrapper _ e@(Var _ f) = R $ do
  bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
  case bodyMaybe of
    Just (_,body) -> do
      wrappedF_maybe <- getWrappedF body
      case wrappedF_maybe of
        Just wrappedF -> changed wrappedF
        Nothing       -> return e
    _ -> return e

inlineWrapper _ e = return e

-- Class Operator Resolution
classOpResolution :: NormRewrite
classOpResolution ctx e@(App (TyApp (collectArgs -> (Prim (PrimFun sel _),_)) _) dict) = R $ do
  classSelM <- fmap (fmap snd . HashMap.lookup sel) $ Lens.use classOps
  isDict    <- fmap isDictType $ termType dict
  case classSelM of
    Just classSel | isDict -> case collectArgs dict of
      (Prim (PrimDFun dfun _),dfunArgs) -> do
        dfunOpsM <- fmap (fmap snd . HashMap.lookup dfun) $ Lens.use dictFuns
        case dfunOpsM of
          Just ((dfunTyBndrs,dfunTmBndrs),dfunOps)
            | classSel < length dfunOps
            , (length dfunTyBndrs + length dfunTmBndrs) == length dfunArgs
            -> let (tySubst,tmSubst) = dfunSubst dfunTyBndrs dfunTmBndrs dfunArgs
               in  changed $! substTms tmSubst $ substTysinTm tySubst (dfunOps !! classSel)
          Nothing -> error $ $(curLoc) ++ "No DFun for: " ++ showDoc e
          _ -> error $ $(curLoc) ++ "Class selector larger than number of expressions in Dfun: " ++ showDoc e ++ show dfunOpsM
      (Var _ fdict,dfunArgs) -> do
        dictBindingM <- fmap (fmap snd . HashMap.lookup fdict) $ Lens.use bindings
        case dictBindingM of
          Just dictBinding | isDict -> do
            clsExpr <- chaseDfun classSel ctx dictBinding
            changed $ mkApps clsExpr dfunArgs
          _ -> return e
      _ -> return e
    _ -> return e

classOpResolution ctx e@(Case scrut ty [alt]) = R $
  case collectArgs scrut of
    (Prim (PrimDFun df t), dfunArgs) -> do
      (pat,altExpr) <- unbind alt
      dfunOpsM <- fmap (fmap snd . HashMap.lookup df) $ Lens.use dictFuns
      case (dfunOpsM,pat) of
        (Just ((dfunTyBndrs,dfunTmBndrs),dfunOps), DataPat _ pxs)
          | (length dfunTyBndrs + length dfunTmBndrs) == length dfunArgs ->
            let (tySubst,tmSubst) = dfunSubst dfunTyBndrs dfunTmBndrs dfunArgs
                dfunOps'  = map (substTms tmSubst . substTysinTm tySubst) dfunOps
                (_,xs)    = unrebind pxs
                fvs       = termFreeIds altExpr
                (binds,_) = List.partition ((`elem` fvs) . varName . fst) $ zip xs dfunOps'
                altExpr'  = case binds of
                               [] -> altExpr
                               _  -> Letrec $ bind (rec $ map (second embed) binds) altExpr
            in changed altExpr'
        (Nothing,_)  -> error $ $(curLoc) ++ "No DFun for: " ++ showDoc e
        _            -> error $ $(curLoc) ++ "No DataPat: " ++ showDoc e
    (Prim (PrimFun _ _), _)  -> error $ "FUN: " ++ showDoc scrut
    (Prim (PrimDict _ _), _) -> error $ "DICT: " ++ showDoc scrut
    _ -> return e

classOpResolution _ e = return e

chaseDfun ::
  (Monad m, Functor m)
  => Int
  -> [CoreContext]
  -> Term
  -> RewriteMonad m Term
chaseDfun classSel ctx e = case collectArgs e of
  (Prim (PrimDFun dfun _), dfunArgs) -> do
    dfunOpsM  <- fmap (fmap snd . HashMap.lookup dfun) $ Lens.use dictFuns
    case dfunOpsM of
      Just ((dfunTyBndrs,dfunTmBndrs),dfunOps)
        | classSel < length dfunOps
        , (length dfunTyBndrs + length dfunTmBndrs) == length dfunArgs
        -> let (tySubst,tmSubst) = dfunSubst dfunTyBndrs dfunTmBndrs dfunArgs
               dfunOp  = substTms tmSubst $ substTysinTm tySubst (dfunOps !! classSel)
           in return $! dfunOp
      Nothing -> error $ $(curLoc) ++ "No DFun for: " ++ showDoc e
      _ -> error $ $(curLoc) ++ "Class selector larger than number of expressions in Dfun: " ++ showDoc e
  (Literal _, _) -> return e
  _ -> mkSelectorCase "chaseDfun" ctx e 1 classSel

inlineSingularDFun :: NormRewrite
inlineSingularDFun _ e@(collectArgs -> (Prim (PrimDFun dfun _),dfunArgs)) = R $ do
  bodyMaybe <- fmap (fmap snd . HashMap.lookup dfun) $ Lens.use dictFuns
  case bodyMaybe of
    Just ((dfunTyBndrs,dfunTmBndrs),[dfunOp])
      | (length dfunTyBndrs + length dfunTmBndrs) == length dfunArgs
      -> let (tySubst,tmSubst) = dfunSubst dfunTyBndrs dfunTmBndrs dfunArgs
             dfunOp' = substTms tmSubst $ substTysinTm tySubst dfunOp
         in return $! dfunOp'
      | otherwise -> return e
    _ -> return e

inlineSingularDFun _ e = return e

dfunSubst :: [TyName]
          -> [TmName]
          -> [Either Term Type]
          -> ([(TyName,Type)],[(TmName,Term)])
dfunSubst dfunTyBndrs dfunTmBndrs dfunArgs = (tySubst,tmSubst)
  where
    (dfunTms,dfunTys) = Either.partitionEithers dfunArgs
    tySubst           = zip dfunTyBndrs dfunTys
    tmSubst           = zip dfunTmBndrs dfunTms

-- Experimental
appProp :: NormRewrite
appProp _ (App (Lam b) arg) = R $ do
  (v,e) <- unbind b
  if isConstant arg || isVar arg
    then changed $ substTm (varName v) arg e
    else changed . Letrec $ bind (rec [(v,embed arg)]) e

appProp _ (App (Letrec b) arg) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (App e arg)

appProp _ (App (Case scrut ty alts) arg) = R $ do
  argTy <- termType arg
  let ty' = applyFunTy ty argTy
  if isConstant arg || isVar arg
    then do
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` arg)
                    <=< unbind
                    ) alts
      changed $ Case scrut ty' alts'
    else do
      (boundArg,argVar) <- mkTmBinderFor "caseApp" arg
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` argVar)
                    <=< unbind
                    ) alts
      changed . Letrec $ bind (rec [(boundArg,embed arg)]) (Case scrut ty' alts')

appProp _ (TyApp (TyLam b) t) = R $ do
  (tv,e) <- unbind b
  changed $ substTyInTm (varName tv) t e

appProp _ (TyApp (Letrec b) t) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (TyApp e t)

appProp _ (TyApp (Case scrut ty' alts) ty) = R $ do
  alts' <- mapM ( return
                . uncurry bind
                . second (`TyApp` ty)
                <=< unbind
                ) alts
  ty'' <- applyTy ty' ty
  changed $ Case scrut ty'' alts'

appProp _ e = return e

type NormRewriteW = Transform (WriterT [LetBinding] (R NormalizeMonad))

liftNormR :: RewriteMonad NormalizeMonad a
          -> WriterT [LetBinding] (R NormalizeMonad) a
liftNormR = lift . R

-- NOTE [unsafeUnbind]: Use unsafeUnbind (which doesn't freshen pattern
-- variables). Reason: previously collected expression still reference
-- the 'old' variable names created by the traversal!
makeANF :: NormRewrite
makeANF ctx (Lam b) = do
  -- See NOTE [unsafeUnbind]
  let (bndr,e) = unsafeUnbind b
  e' <- makeANF (LamBody bndr:ctx) e
  return $ Lam (bind bndr e')

makeANF ctx e
  = R $ do
    (e',bndrs) <- runR $ runWriterT $ bottomupR collectANF ctx e
    case bndrs of
      [] -> return e
      _  -> changed . Letrec $ bind (rec bndrs) e'

collectANF :: NormRewriteW
collectANF _ e@(App appf arg)
  | (conVarPrim, _) <- collectArgs e
  , isCon conVarPrim || isPrim conVarPrim || isVar conVarPrim
  = do
    untranslatable <- liftNormR $ isUntranslatable arg
    localVar       <- liftNormR $ isLocalVar arg
    case (untranslatable,localVar || isConstant arg,arg) of
      (False,False,_) -> do (argId,argVar) <- liftNormR $ mkTmBinderFor "repANF" arg
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
  untranslatable <- liftNormR $ isUntranslatable body
  localVar       <- liftNormR $ isLocalVar body
  if localVar || untranslatable
    then return body
    else do
      (argId,argVar) <- liftNormR $ mkTmBinderFor "bodyVar" body
      tell [(argId,embed body)]
      return argVar

collectANF ctx e@(Case subj ty alts) = do
    untranslatableSubj <- liftNormR $ isUntranslatable subj
    localVar           <- liftNormR $ isLocalVar subj
    (bndr,subj') <- if localVar || untranslatableSubj || isConstant subj
      then return ([],subj)
      else do (argId,argVar) <- liftNormR $ mkTmBinderFor "subjLet" subj
              return ([(argId,embed subj)],argVar)

    untranslatableE <- liftNormR $ isUntranslatable e
    (binds,alts') <- if untranslatableE
      then return ([],alts)
      else fmap (first concat . unzip) $ liftNormR $ mapM doAlt alts

    tell (bndr ++ binds)
    return (Case subj' ty alts')
  where
    doAlt :: Bind Pat Term -> RewriteMonad NormalizeMonad ([LetBinding],Bind Pat Term)
    -- See NOTE [unsafeUnbind]
    doAlt = fmap (second (uncurry bind)) . doAlt' . unsafeUnbind

    doAlt' :: (Pat,Term) -> RewriteMonad NormalizeMonad ([LetBinding],(Pat,Term))
    doAlt' alt@(DataPat dc pxs@(unrebind -> ([],xs)),altExpr) = do
      lv      <- isLocalVar altExpr
      patSels <- Monad.zipWithM (doPatBndr (unembed dc)) xs [0..]
      if lv || isConstant altExpr
        then return (patSels,alt)
        else do (altId,altVar) <- mkTmBinderFor "altLet" altExpr
                return ((altId,embed altExpr):patSels,(DataPat dc pxs,altVar))
    doAlt' alt@(DataPat _ _, _) = return ([],alt)
    doAlt' alt@(pat,altExpr) = do
      lv <- isLocalVar altExpr
      if lv || isConstant altExpr
        then return ([],alt)
        else do (altId,altVar) <- mkTmBinderFor "altLet" altExpr
                return ([(altId,embed altExpr)],(pat,altVar))

    doPatBndr :: DataCon -> Id -> Int -> RewriteMonad NormalizeMonad LetBinding
    doPatBndr dc pId i
      = do patExpr <- mkSelectorCase "doPatBndr" ctx subj (dcTag dc) i
           return (pId,embed patExpr)

collectANF _ e = return e

etaExpansionTL :: NormRewrite
etaExpansionTL ctx (Lam b) = do
  (bndr,e) <- unbind b
  e' <- etaExpansionTL (LamBody bndr:ctx) e
  return $ Lam (bind bndr e')

etaExpansionTL ctx e
  = R $ do
    isF <- isFun e
    if isF
      then do
        argTy <- ( return
                 . fst
                 . Maybe.fromMaybe (error "etaExpansion splitFunTy")
                 . splitFunTy
                 <=< termType
                 ) e
        (newIdB,newIdV) <- mkInternalVar "eta" argTy
        e' <- runR $ etaExpansionTL (LamBody newIdB:ctx) (App e newIdV)
        changed . Lam $ bind newIdB e'
      else return e

recToLetRec :: NormRewrite
recToLetRec [] e = R $ do
  fn <- liftR $ Lens.use curFun
  bodyM       <- fmap (HashMap.lookup fn) $ Lens.use bindings
  normalizedE <- splitNormalized e
  case (normalizedE,bodyM) of
    (Right (args,bndrs,res), Just (bodyTy,_)) -> do
      let appF              = mkTmApps (Var bodyTy fn) (map idToVar args)
          (toInline,others) = List.partition ((==) appF . unembed . snd) bndrs
          resV              = idToVar res
      case (toInline,others) of
        (_:_,_:_) -> do
          let substsInline = map (\(id_,_) -> (varName id_,resV)) toInline
              others'      = map (second (embed . substTms substsInline . unembed)) others
          changed $ mkLams (Letrec $ bind (rec others') resV) args
        _ -> return e
    _ -> return e

recToLetRec _ e = return e
