{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns  #-}
module CLaSH.Normalize.Transformations where

import           Control.Lens            ((%=))
import qualified Control.Lens            as Lens
import qualified Control.Monad     as Monad
import qualified Data.Either       as Either
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List         as List
import qualified Data.Maybe        as Maybe
import Unbound.LocallyNameless        (Bind,Embed(..),bind,embed,rec,unbind,unembed,unrebind,unrec)

import CLaSH.Core.DataCon    (DataCon,dcTag,dcUnivTyVars)
import CLaSH.Core.FreeVars   (typeFreeVars,termFreeIds,termFreeTyVars)
import CLaSH.Core.Pretty     (showDoc)
import CLaSH.Core.Prim       (Prim(..))
import CLaSH.Core.Subst      (substTyInTm,substTysinTm)
import CLaSH.Core.Term       (Term(..),TmName,LetBinding,Pat(..))
import CLaSH.Core.Type       (splitFunTy,applyFunTy,applyTy)
import CLaSH.Core.Util       (collectArgs,mkApps,isFun,isLam,termType,isVar,isCon,isLet,isPrim)
import CLaSH.Core.Var        (Var(..),Id)
import CLaSH.Netlist.Util    (splitNormalized)
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Types
import CLaSH.Rewrite.Util
import CLaSH.Util

----------------------------------------------------------
-- = Definitions =
--
-- FUN (a -> b) = True
-- FUN _        = False
--
-- BOX (T ts) = any (\t -> BOX t || FUN t) (CONARGS T ts)
-- BOX _      = False
--
-- POLY (∀a.t)   = True
-- POLY (a -> b) = POLY b
-- POLY _        = False
--
-- POLYFUN (∀a.t)   = POLYFUN t
-- POLYFUN (a -> b) = True
-- POLYFUN _        = False
--
-- = Inline, Lift, or Specialize =
--
-- Function Application:
-- Has FUN arguments?   => Inline
-- Has BOXed arguments? => Specialize
-- Has Type arguments?  => Specialize
-- Others               => Do Nothing
--
-- Let-binding:
-- Has POLYFUNction type?              => Lift
-- Takes Type arguments, non-recursive => Inline
-- Takes Type arguments, recursive     => Lift
-- Has BOX type, non-recursive?        => Inline
-- Has BOX type, recursive?            => Lift
-- Others                              => Do Nothing
--
-----------------------------------------------------------
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
  changed . Letrec $ bind (rec [(v,embed arg)]) e

lamApp _ e = return e

letApp :: NormRewrite
letApp _ (App (Letrec b) arg) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (App e arg)

letApp _ e = return e

caseApp :: NormRewrite
caseApp _ (App (Case scrut ty alts) arg) = R $ do
  (boundArg,argVar) <- mkTmBinderFor "caseApp" arg
  alts' <- mapM ( return
                . uncurry bind
                . second (`App` argVar)
                <=< unbind
                ) alts
  argTy <- termType arg
  let ty' = applyFunTy ty argTy
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
  = do e2Ty <- termType e2
       if nonRep e2Ty
         then specialise specialisations ctx e
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

        traceIf True ("substTyMap:" ++ show substTyMap) $ changed (substTysinTm substTyMap e')
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

caseCon _ (Case _ _ [alt]) = R $ do
  (pat,e) <- unbind alt
  case pat of
    DefaultPat -> changed e
    _          -> return e

caseCon _ e = return e

repANF :: NormRewrite
repANF _ e@(App appf arg)
  | (conVarPrim, _) <- collectArgs e
  , isCon conVarPrim || isPrim conVarPrim || isVar conVarPrim
  = R $ do
    localVar       <- isLocalVar arg
    untranslatable <- isUntranslatable arg
    case localVar || untranslatable of
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
  case localVar || untranslatable of
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
      case lv of
        True  -> return ([],alt)
        False -> do let fvs = termFreeIds altExpr
                    patSels <- fmap Maybe.catMaybes $ Monad.zipWithM (doPatBndr fvs (unembed dc)) xs [0..]
                    (altId,altVar) <- mkTmBinderFor "altLet" altExpr
                    return ((altId,embed altExpr):patSels,(DataPat dc pxs,altVar))
    doAlt' alt@(DataPat _ _, _) = return ([],alt)
    doAlt' alt@(pat,altExpr) = do
      lv <- isLocalVar altExpr
      case lv of
        True  -> return ([],alt)
        False -> do (altId,altVar) <- mkTmBinderFor "altLet" altExpr
                    return ([(altId,embed altExpr)],(pat,altVar))

    doPatBndr :: [TmName] -> DataCon -> Id -> Int -> RewriteMonad NormalizeMonad (Maybe LetBinding)
    doPatBndr fvs dc pId i
      | (varName pId) `notElem` fvs = return Nothing
      | otherwise
      = do patExpr <- mkSelectorCase ctx subj (dcTag dc) i
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

inlineVar :: NormRewrite
inlineVar ctx e@(Letrec bnd) = do
  (bndrs,_) <- unbind bnd
  if length (unrec bndrs) > 1
    then inlineBinders (isLocalVar . unembed . snd) ctx e
    else return e

inlineVar _ e = return e


inlineWrapper :: NormRewrite
inlineWrapper [] e = R $ do
  (_,lbs,_) <- splitNormalized e
  case lbs of
    [(_,bExpr)] -> case collectArgs (unembed bExpr) of
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
  where

inlineWrapper _ e = return e

-- Class Operator Resolution
classOpResolution :: NormRewrite
classOpResolution ctx e@(App (TyApp (Prim (PrimFun sel _)) _) dfunE)
  | (Var _ dfun, dfunArgs) <- collectArgs dfunE = R $ do
    classSelM <- fmap (fmap snd . HashMap.lookup sel)  $ Lens.use classOps
    dfunOpsM  <- fmap (fmap snd . HashMap.lookup dfun) $ Lens.use dictFuns
    bindingsM <- fmap (fmap snd . HashMap.lookup dfun) $ Lens.use bindings
    case (classSelM,dfunOpsM,bindingsM) of
      (Just classSel,Just dfunOps,Nothing)
        | classSel < length dfunOps -> do
          let clsExpr = dfunOps !! classSel
          changed (mkApps clsExpr dfunArgs)
      (Just classSel,Nothing, Just binding) -> do
        clsExpr <- chaseDfun classSel ctx binding
        changed (mkApps clsExpr dfunArgs)
      (Just classSel,Nothing,Nothing) -> do
        traceIf True ($(curLoc) ++ "No binding or dfun for classOP?: " ++ showDoc e) $ return e
      _ -> return e

classOpResolution _ e = return e

chaseDfun ::
  (Monad m, Functor m)
  => Int
  -> [CoreContext]
  -> Term
  -> RewriteMonad m Term
chaseDfun classSel ctx e
  | (Var _ f, args) <- collectArgs e
  = do
    dfunOpsM  <- fmap (fmap snd . HashMap.lookup f) $ Lens.use dictFuns
    case dfunOpsM of
      Just dfunOps | classSel < length dfunOps -> do
        let dfunOp = dfunOps !! classSel
        return $ mkApps dfunOp args
      _ -> error $ $(curLoc) ++ "No binding or dfun for classOP?: " ++ show e
  | otherwise
  = do
    selCase <- mkSelectorCase ctx e 0 classSel
    return selCase
