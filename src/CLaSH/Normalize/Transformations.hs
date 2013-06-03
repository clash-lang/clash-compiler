{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns  #-}
module CLaSH.Normalize.Transformations where

import           Control.Lens            ((%=))
import qualified Control.Lens            as Lens
import qualified Control.Monad     as Monad
import Control.Monad.Writer        (WriterT(..),lift,tell)
import qualified Data.Either       as Either
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List         as List
import qualified Data.Maybe        as Maybe
import Unbound.LocallyNameless        (Bind,Embed(..),bind,embed,rec,unbind,unembed,unrebind,unrec)
import Unbound.LocallyNameless.Ops (unsafeUnbind)

import CLaSH.Core.DataCon    (DataCon,dcTag,dcUnivTyVars)
import CLaSH.Core.FreeVars   (typeFreeVars,termFreeIds,termFreeTyVars,termFreeVars)
import CLaSH.Core.Pretty     (showDoc)
import CLaSH.Core.Prim       (Prim(..))
import CLaSH.Core.Subst      (substTm,substTyInTm,substTysinTm)
import CLaSH.Core.Term       (Term(..),TmName,LetBinding,Pat(..))
import CLaSH.Core.Type       (splitFunTy,applyFunTy,applyTy)
import CLaSH.Core.Util       (collectArgs,mkApps,isFun,isLam,termType,isVar,isCon,isLet,isPrim)
import CLaSH.Core.Var        (Var(..),Id)
import CLaSH.Netlist.Util    (splitNormalized)
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Types
import CLaSH.Rewrite.Util
import CLaSH.Util

tauReduction :: NormRewrite
tauReduction _ (TyApp (TyLam b) t) = R $ do
  (tv,e) <- unbind b
  changed $ substTyInTm (varName tv) t e

tauReduction _ e = return e

letTyApp :: NormRewrite
letTyApp _ (TyApp (Letrec b) t) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (TyApp e t)

letTyApp _ e = return e

caseTyApp :: NormRewrite
caseTyApp _ (TyApp (Case scrut ty' alts) ty) = R $ do
  alts' <- mapM ( return
                . uncurry bind
                . second (`TyApp` ty)
                <=< unbind
                ) alts
  ty'' <- applyTy ty' ty
  changed $ Case scrut ty'' alts'

caseTyApp _ e = return e

lamApp :: NormRewrite
lamApp _ (App (Lam b) arg) = R $ do
  (v,e) <- unbind b
  case (isConstant arg || isVar arg) of
    True  -> changed $ substTm (varName v) arg e
    False -> changed . Letrec $ bind (rec [(v,embed arg)]) e

lamApp _ e = return e

letApp :: NormRewrite
letApp _ (App (Letrec b) arg) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (App e arg)

letApp _ e = return e

caseApp :: NormRewrite
caseApp _ (App (Case scrut ty alts) arg) = R $ do
  argTy <- termType arg
  let ty' = applyFunTy ty argTy
  case (isConstant arg || isVar arg) of
    True  -> do
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` arg)
                    <=< unbind
                    ) alts
      changed $ Case scrut ty' alts'
    False -> do
      (boundArg,argVar) <- mkTmBinderFor "caseApp" arg
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` argVar)
                    <=< unbind
                    ) alts
      changed . Letrec $ bind (rec [(boundArg,embed arg)]) (Case scrut ty' alts')

caseApp _ e = return e

bindNonRep :: NormRewrite
bindNonRep = inlineBinders nonRepTest
  where
    nonRepTest (Id idName tyE, exprE)
      | nonRep (unembed tyE)
      = fmap (notElem idName . snd) $ localFreeVars (unembed exprE)

    nonRepTest _ = return False

liftNonRep :: NormRewrite
liftNonRep = liftBinders nonRepTest
  where
    nonRepTest (Id idName tyE, exprE)
      | nonRep (unembed tyE)
      = fmap (elem idName . snd) $ localFreeVars (unembed exprE)

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
           if (nonRep e2Ty && (not localVar))
             then runR $ specialise specialisations ctx e
             else return e

nonRepSpec _ e = return e

caseLet :: NormRewrite
caseLet _ (Case (Letrec b) ty alts) = R $ do
  (xes,e) <- unbind b
  changed . Letrec $ bind xes (Case e ty alts)

caseLet _ e = return e

caseCase :: NormRewrite
caseCase _ (Case (Case scrut ty1 alts1) ty2 alts2)
  | nonRep ty1
  = R $ do
    newAlts  <- mapM ( return
                     . uncurry bind
                     . second (\altE -> Case altE ty2 alts2)
                     <=< unbind
                     ) alts1
    changed $ Case scrut ty2 newAlts

caseCase _ e = return e

inlineNonRep :: NormRewrite
inlineNonRep ctx e@(Case scrut ty alts)
  | (Var _ f, args) <- collectArgs scrut
  = R $ do
    isInlined <- liftR $ alreadyInlined f
    case isInlined of
      True -> do
        cf <- liftR $ Lens.use curFun
        traceIf True ($(curLoc) ++ "InlineBox: " ++ show f ++ " already inlined in: " ++ show cf) $ return e
      False -> do
        scrutTy   <- termType scrut
        bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
        case (nonRep scrutTy, bodyMaybe) of
          (True,Just (_, scrutBody)) -> do
            liftR $ newInlined %= (f:)
            changed $ Case (mkApps scrutBody args) ty alts
          _ -> return e

inlineNonRep _ e = return e

caseCon :: NormRewrite
caseCon _ (Case scrut ty alts)
  | (Data _ dc, args) <- collectArgs scrut
  = R $ do
    alts' <- mapM unbind alts
    let dcAltM = List.find (equalCon dc . fst) alts'
    case dcAltM of
      Just (DataPat _ pxs, e) -> do
        let (tvs,xs) = unrebind pxs
        let fvs = termFreeIds e
        let (binds,_) = List.partition ((`elem` fvs) . varName . fst)
                      $ zip xs (Either.lefts args)
        let e' = case binds of
                  [] -> e
                  _  -> Letrec $ bind (rec $ map (second embed) binds) e
        let substTyMap = zip (map varName tvs) (drop (length $ dcUnivTyVars dc) (Either.rights args))

        changed (substTysinTm substTyMap e')
      Nothing -> do
        let defAltM = List.find (isDefPat . fst) alts'
        case defAltM of
          Just (DefaultPat, e) -> do
            changed e
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
    DataPat _ pxs -> do let (tvs,xs)   = unrebind pxs
                            (ftvs,fvs) = termFreeVars altE
                            usedTvs    = filter ((`elem` ftvs) . varName) tvs
                            usedXs     = filter ((`elem` fvs) . varName) xs
                        case (usedTvs,usedXs) of
                          ([],[]) -> changed altE
                          _       -> return e
    _             -> return e

caseCon _ e = return e

repANF :: NormRewrite
repANF _ e@(App appf arg)
  | (conVarPrim, _) <- collectArgs e
  , isCon conVarPrim || isPrim conVarPrim || isVar conVarPrim
  = R $ do
    localVar       <- isLocalVar arg
    untranslatable <- isUntranslatable arg
    case localVar || untranslatable || isConstant arg of
      True  -> return e
      False -> do (argId,argVar) <- mkTmBinderFor "repANF" arg
                  changed . Letrec $ bind (rec [(argId,embed arg)]) (App appf argVar)

repANF _ e = return e

nonRepANF :: NormRewrite
nonRepANF ctx e@(App appConPrim arg)
  | (conPrim, _) <- collectArgs e
  , isCon conPrim || isPrim conPrim
  = R $ do
    localVar       <- isLocalVar arg
    untranslatable <- isUntranslatable arg
    case (localVar || not untranslatable,arg) of
      (False,Letrec b)   -> do (binds,body) <- unbind b
                               changed . Letrec $ bind binds (App appConPrim body)
      (False,Case _ _ _) -> runR $ specialise specialisations ctx e
      (False,Lam _)      -> runR $ specialise specialisations ctx e
      _                  -> return e

nonRepANF _ e = return e

subjLet :: NormRewrite
subjLet _ e@(Case subj ty alts) = R $ do
  localVar       <- isLocalVar subj
  untranslatable <- isUntranslatable subj
  case localVar || untranslatable || isConstant subj of
    True  -> return e
    False -> do (argId,argVar) <- mkTmBinderFor "subjLet" subj
                changed . Letrec $ bind (rec [(argId,embed subj)]) (Case argVar ty alts)

subjLet _ e = return e

altLet :: NormRewrite
altLet ctx e@(Case subj ty alts) = R $ do
    untranslatable <- isUntranslatable e
    case untranslatable of
      True  -> return e
      False -> do (bndrs,alts') <- fmap (first concat . unzip) $ mapM doAlt alts
                  case bndrs of
                    [] -> return e
                    _  -> changed . Letrec $ bind (rec bndrs) (Case subj ty alts')
  where
    doAlt :: Bind Pat Term -> RewriteMonad NormalizeMonad ([LetBinding],Bind Pat Term)
    doAlt = fmap (second (uncurry bind)) . doAlt' <=< unbind

    doAlt' :: (Pat,Term) -> RewriteMonad NormalizeMonad ([LetBinding],(Pat,Term))
    doAlt' alt@(DataPat dc pxs@(unrebind -> ([],xs)),altExpr) = do
      lv <- isLocalVar altExpr
      case (lv || isConstant altExpr) of
        True  -> return ([],alt)
        False -> do let fvs = termFreeIds altExpr
                    patSels <- fmap Maybe.catMaybes $ Monad.zipWithM (doPatBndr fvs (unembed dc)) xs [0..]
                    (altId,altVar) <- mkTmBinderFor "altLet" altExpr
                    return ((altId,embed altExpr):patSels,(DataPat dc pxs,altVar))
    doAlt' alt@(DataPat _ _, _) = return ([],alt)
    doAlt' alt@(pat,altExpr) = do
      lv <- isLocalVar altExpr
      case (lv || isConstant altExpr) of
        True  -> return ([],alt)
        False -> do (altId,altVar) <- mkTmBinderFor "altLet" altExpr
                    return ([(altId,embed altExpr)],(pat,altVar))

    doPatBndr :: [TmName] -> DataCon -> Id -> Int -> RewriteMonad NormalizeMonad (Maybe LetBinding)
    doPatBndr fvs dc pId i
      | (varName pId) `notElem` fvs = return Nothing
      | otherwise
      = do patExpr <- mkSelectorCase "doPatBndr" ctx subj (dcTag dc) i
           return (Just (pId,embed patExpr))

altLet _ e = return e

bodyVar :: NormRewrite
bodyVar _ e@(Letrec b) = R $ do
  (binds,body)   <- unbind b
  localVar       <- isLocalVar body
  untranslatable <- isUntranslatable body
  case localVar || untranslatable of
    True  -> return e
    False -> do (argId,argVar) <- mkTmBinderFor "bodyVar" body
                changed . Letrec $ bind (rec $ unrec binds ++ [(argId,embed body)]) argVar

bodyVar _ e = return e

letFlat :: NormRewrite
letFlat _ e@(Letrec binds) = R $ do
    (xes, body) <- fmap (first unrec) $ unbind binds
    (binds',updated) <- fmap unzip $ mapM flatBind xes
    case (or updated) of
      True  -> changed . Letrec $ bind (rec (concat binds')) body
      False -> return e
  where
    flatBind :: Monad m => LetBinding -> RewriteMonad m ([LetBinding],Bool)
    flatBind (bndr, Embed (Letrec binds')) = do
      (r,body) <- unbind binds'
      let r' = unrec r
      return ((bndr, Embed body):r', True)
    flatBind b = return ([b],False)

letFlat _ e = return e

topLet :: NormRewrite
topLet ctx e
  | all isLambdaBodyCtx ctx && not (isLet e)
  = R $ do
  untranslatable <- isUntranslatable e
  case untranslatable of
    True  -> return e
    False -> do (argId,argVar) <- mkTmBinderFor "topLet" e
                changed . Letrec $ bind (rec [(argId,embed e)]) argVar

topLet _ e = return e

etaExpansion :: NormRewrite
etaExpansion (AppFirst:_)  e = return e
etaExpansion (AppSecond:_) e = return e
etaExpansion _ e
  | not (isLam e)
  = R $ do
    isF <- isFun e
    case isF of
      True -> do
        argTy <- ( return
                 . fst
                 . Maybe.fromMaybe (error "etaExpansion splitFunTy")
                 . splitFunTy
                 <=< termType
                 ) e
        (newIdB,newIdV) <- mkInternalVar "eta" argTy
        changed . Lam $ bind newIdB (App e newIdV)
      False -> return e

etaExpansion _ e = return e

-- Misc rewrites
deadCode :: NormRewrite
deadCode _ e@(Letrec binds) = R $ do
    (xes, body) <- fmap (first unrec) $ unbind binds
    let bodyFVs = termFreeIds body
    let (xesUsed,xesOther) = List.partition
                               ( (`elem` bodyFVs )
                               . varName
                               . fst
                               ) xes
    let xesUsed' = findUsedBndrs [] xesUsed xesOther
    case (length xesUsed' /= length xes) of
      True  -> changed . Letrec $ bind (rec xesUsed') body
      False -> return e
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
bindConstantVar ctx e@(Letrec bnd) = do
  (bndrs,_) <- unbind bnd
  let test (_,Embed e') = (||) <$> isLocalVar e' <*> (pure $ isConstant e')
  if ((all isLambdaBodyCtx ctx) && (length (unrec bndrs) < 2))
    then return e
    else inlineBinders test ctx e


bindConstantVar _ e = return e

inlineClosedTerm :: NormRewrite
inlineClosedTerm _ e@(Var _ f) = R $ do
  bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
  case bodyMaybe of
    Just (_,body) -> do
      closed <- isClosed body
      untranslatable <- isUntranslatable body
      if (closed && not untranslatable)
        then changed body
        else return e
    _ -> return e

inlineClosedTerm _ e = return e

forceToplet :: NormRewrite
forceToplet _ (Letrec b) = R $ do
  case unsafeUnbind b of
    (unrec -> [(_,Embed e)],v@(Var _ _)) -> do
      lv <- isLocalVar v
      case lv of
        True  -> changed e
        False -> error $ $(curLoc) ++ "forceTopLet called on let-binding where body is not a local variable: " ++ showDoc (Letrec b)
    _ -> error $ $(curLoc) ++ "forceTopLet called on let-binding with more than one binding: " ++ showDoc (Letrec b)

forceToplet _ e
  | isConstant e = R $ changed e
  | otherwise    = error $ $(curLoc) ++ "forceTopLet not called on a let-binding: " ++ showDoc e

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
    Right ((_,[(_,bExpr)],_)) -> case collectArgs (unembed bExpr) of
      (Var _ fn,args) -> do allLocal <- fmap and $ mapM (either isLocalVar (\_ -> return True)) args
                            bodyMaybe <- fmap (HashMap.lookup fn) $ Lens.use bindings
                            case (bodyMaybe,allLocal) of
                              (Just (_,body),True) -> changed body
                              _                    -> return e
      _               -> return e
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
classOpResolution ctx e@(App (TyApp (Prim (PrimFun sel _)) _) dict) = R $ do
  classSelM <- fmap (fmap snd . HashMap.lookup sel) $ Lens.use classOps
  case classSelM of
    Just classSel -> case collectArgs dict of
      (Prim (PrimDFun dfun _),dfunArgs) -> do
        dfunOpsM <- fmap (fmap snd . HashMap.lookup dfun) $ Lens.use dictFuns
        case dfunOpsM of
          Just dfunOps | classSel < length dfunOps -> changed $ mkApps (dfunOps !! classSel) dfunArgs
          Nothing -> error $ $(curLoc) ++ "No DFun for: " ++ showDoc e
          _ -> error $ $(curLoc) ++ "Class selector larger than number of expressions in Dfun: " ++ showDoc e
      (Var _ fdict,dfunArgs) -> do
        dictBindingM <- fmap (fmap snd . HashMap.lookup fdict) $ Lens.use bindings
        case dictBindingM of
          Just dictBinding -> do
            clsExpr <- chaseDfun classSel ctx dictBinding
            changed $ mkApps clsExpr dfunArgs
          Nothing -> return e
      _ -> return e
    Nothing -> return e

classOpResolution ctx e@(Case scrut ty [alt]) = R $ do
  case (collectArgs scrut) of
    (Prim (PrimDFun df t), args) -> do
      (pat,altExpr) <- unbind alt
      dfunOpsM <- fmap (fmap snd . HashMap.lookup df) $ Lens.use dictFuns
      case (dfunOpsM,pat) of
        (Just dfunOps, DataPat _ pxs) -> do
          let dfunOps' = map (`mkApps` args) dfunOps
          let (_,xs)   = unrebind pxs
          let fvs      = termFreeIds altExpr
          let (binds,_) = List.partition ((`elem` fvs) . varName . fst) $ zip xs dfunOps'
          let altExpr' = case binds of
                            [] -> altExpr
                            _  -> Letrec $ bind (rec $ map (second embed) binds) altExpr
          changed altExpr'
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
chaseDfun classSel ctx e = case (collectArgs e) of
  (Prim (PrimDFun dfun _), args) -> do
    dfunOpsM  <- fmap (fmap snd . HashMap.lookup dfun) $ Lens.use dictFuns
    case dfunOpsM of
      Just dfunOps | classSel < length dfunOps -> do
        let dfunOp = dfunOps !! classSel
        return $ mkApps dfunOp args
      Nothing -> error $ $(curLoc) ++ "No DFun for: " ++ showDoc e
      _ -> error $ $(curLoc) ++ "Class selector larger than number of expressions in Dfun: " ++ showDoc e
  _ -> mkSelectorCase "chaseDfun" ctx e 1 classSel

inlineSingularDFun :: NormRewrite
inlineSingularDFun _ e@(Prim (PrimDFun f _)) = R $ do
  bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use dictFuns
  case bodyMaybe of
    Just (_,[dfunOp]) -> changed dfunOp
    _ -> return e

inlineSingularDFun _ e = return e

-- Experimental
appProp :: NormRewrite
appProp _ (App (Lam b) arg) = R $ do
  (v,e) <- unbind b
  case (isConstant arg || isVar arg) of
    True  -> changed $ substTm (varName v) arg e
    False -> changed . Letrec $ bind (rec [(v,embed arg)]) e

appProp _ (App (Letrec b) arg) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (App e arg)

appProp _ (App (Case scrut ty alts) arg) = R $ do
  argTy <- termType arg
  let ty' = applyFunTy ty argTy
  case (isConstant arg || isVar arg) of
    True  -> do
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` arg)
                    <=< unbind
                    ) alts
      changed $ Case scrut ty' alts'
    False -> do
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
    localVar       <- liftNormR $ isLocalVar arg
    untranslatable <- liftNormR $ isUntranslatable arg
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
  localVar       <- liftNormR $ isLocalVar body
  untranslatable <- liftNormR $ isUntranslatable body
  tell (unrec binds)
  case localVar || untranslatable of
    True  -> return body
    False -> do (argId,argVar) <- liftNormR $ mkTmBinderFor "bodyVar" body
                tell [(argId,embed body)]
                return argVar

collectANF ctx e@(Case subj ty alts) = do
    localVar           <- liftNormR $ isLocalVar subj
    untranslatableSubj <- liftNormR $ isUntranslatable subj
    (bndr,subj') <- case localVar || untranslatableSubj || isConstant subj of
      True  -> return ([],subj)
      False -> do (argId,argVar) <- liftNormR $ mkTmBinderFor "subjLet" subj
                  return ([(argId,embed subj)],argVar)

    untranslatableE <- liftNormR $ isUntranslatable e
    (binds,alts') <- case untranslatableE of
      True  -> return ([],alts)
      False -> fmap (first concat . unzip) $ liftNormR $ mapM doAlt alts

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
      case (lv || isConstant altExpr) of
        True  -> return (patSels,alt)
        False -> do (altId,altVar) <- mkTmBinderFor "altLet" altExpr
                    return ((altId,embed altExpr):patSels,(DataPat dc pxs,altVar))
    doAlt' alt@(DataPat _ _, _) = return ([],alt)
    doAlt' alt@(pat,altExpr) = do
      lv <- isLocalVar altExpr
      case (lv || isConstant altExpr) of
        True  -> return ([],alt)
        False -> do (altId,altVar) <- mkTmBinderFor "altLet" altExpr
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
    case isF of
      True -> do
        argTy <- ( return
                 . fst
                 . Maybe.fromMaybe (error "etaExpansion splitFunTy")
                 . splitFunTy
                 <=< termType
                 ) e
        (newIdB,newIdV) <- mkInternalVar "eta" argTy
        e' <- runR $ etaExpansionTL (LamBody newIdB:ctx) (App e newIdV)
        changed . Lam $ bind newIdB e'
      False -> return e
