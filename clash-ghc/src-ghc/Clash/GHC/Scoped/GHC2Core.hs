{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Conversion of GHC Core to closed terms of scoped Clash Core
  ("Clash.Core.Scoped.Syntax").

  This is 'Clash.GHC.GHC2Core.coreToTerm', building scoped Core instead of
  Clash Core. It gives the same terms as converting the result of
  'Clash.GHC.GHC2Core.coreToTerm' with
  'Clash.Core.Scoped.Convert.fromClashTermClosed', with the same raw names,
  except for primitives of open types, see "Primitives" below. It converts in
  two phases, see "Clash.Core.Scoped.Builder".

  = Types

  The types in terms, and those of global variables, primitives and data
  constructors, are types of scoped Core, which
  'Clash.GHC.GHC2Core.coreToTypeB' builds from GHC's types directly.

  = Primitives

  The type of a primitive is closed in scoped Core. Primitives that stand in
  for a term of some type, @_RUBBISH_@, @_TY_@ and @_CO_@, have the shape of
  'Clash.Core.Scoped.Util.primAtType': if the type is open, they are a
  primitive of type @forall a. a@ applied to the type. 'coreToTerm' gives
  @_RUBBISH_@ the open type itself.

  = Recursive groups

  A recursive group of global binders (see
  @Clash.GHC.GenerateBindings.processBind@) refers to its own binders as local
  variables, which the caller binds: 'coreToTermB' gets the set of these
  binders.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.Scoped.GHC2Core
  ( coreToTermB
  , coreToTypeB
  , coreToIdB
  , varKey
  ) where

import           Control.Lens                ((^.), (<>=), view)
import qualified Control.Monad.RWS.Strict    as RWS
import           Data.Bifunctor              (second)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as Text
import           Data.Text.Extra             (showt)
import qualified GHC.Magic
import qualified GHC.Prim
import qualified GHC.Stack
import qualified GHC.Stack.Types
import qualified Text.Read                   as Text
import qualified GHC.Data.Strict             as GHC

-- GHC API
import GHC.Builtin.Types (falseDataCon)
import GHC.Core.Coercion (coercionType, coercionKind)
import GHC.Core.FVs  (exprSomeFreeVars)
import GHC.Core
  (AltCon (..), Bind (..), CoreExpr, Expr (..), Unfolding (..),
   Alt(..),
#if MIN_VERSION_ghc(9,14,0)
   isTypeArg, isValArg,
#endif
   collectArgs, rhssOfAlts)
import GHC.Types.Tickish (GenTickish (..))
import GHC.Core.DataCon (dataConTyCon)
import GHC.Core.FamInstEnv (emptyFamInstEnvs)
import GHC.Types.Id (isDataConId_maybe)
import GHC.Types.Id.Info (unfoldingInfo)
import GHC.Types.Literal (Literal (..), literalType)
import GHC.Types.Name (getSrcSpan)
import GHC.Data.Pair (Pair (..))
import GHC.Types.SrcLoc (SrcSpan (..), noSrcSpan)
import GHC.Core.TyCon
  (isNewTyCon
#if MIN_VERSION_ghc(9,14,0)
  , isUnaryClassTyCon
#endif
  )
import GHC.Core.TyCo.Rep (Type (..))
import GHC.Core.Type (coreView, tyConAppFunTy_maybe)
import GHC.Types.Var
  (Id, TyVar, Var, VarBndr (..), isTyVar, varName, varType, idInfo,
   isGlobalId)
import GHC.Types.Var.Set (VarSet, elemVarSet, isEmptyVarSet)
import GHC.Utils.Outputable (showPprUnsafe)

-- Local imports
import Clash.Core.Basic (IsMultiPrim (..), NameMod (..), WorkInfo (..), undefinedXPrims)
import           Clash.Annotations.Primitive (extractPrim)
import qualified Clash.Core.Name             as C
import           Clash.Core.Scoped.Syntax    (mkPrimInfo)
import qualified Clash.Core.Scoped.Syntax    as S
import           Clash.Core.Scoped.TysPrim   (undefinedTy)
import           Clash.Driver.Types          (HasClashOpts(..))
import           Clash.GHC.GHC2Core
  (C2C, GHC2CoreEnv (..), SrcSpanRB (..), addUsefull, addUsefullR,
   convertedTyVars, coreToDataCon, coreToGlobalIdB, coreToLiteral,
   coreToTyVarB, coreToTypeB, coreToPrimVar, coreToVar, coreToVarTypeB,
   emptyGHC2CoreState, famInstEnvs, hasPrimCo, isDataConWrapId,
   isSizedCast, pendingWarnings, qualifiedNameString, srcSpan, varKey,
   withoutSrcSpan)
import           Clash.Core.Scoped.Builder
import qualified Clash.Normalize.Scoped.Primitives as NP
import           Clash.Primitives.Types      hiding (name)
import           Clash.Util                  (curLoc)

-- * Terms

-- | Convert a GHC term, see the module header. Like
-- 'Clash.GHC.GHC2Core.coreToTerm', given the compiled primitives and the
-- unlocatable variables. The given global binders are local variables of the
-- term, see "Recursive groups" in the module header.
coreToTermB
  :: CompiledPrimMap
  -> [Var]
  -> VarSet
  -> CoreExpr
  -> C2C TmB
coreToTermB primMap unlocs recLocals = term
  where
    term :: CoreExpr -> C2C TmB
    term e
#if MIN_VERSION_ghc(9,14,0)
      -- See 'Clash.GHC.GHC2Core.coreToTerm'
      | (Var x, args) <- collectArgs e
      , Just dc <- isDataConId_maybe x
      , isUnaryClassTyCon (dataConTyCon dc)
      , (tyArgs, [val]) <- break isValArg args
      , all isTypeArg tyArgs
      , isValArg val
      = term val
#endif
      | (Var x,args) <- collectArgs e
      = do
        opts <- askClashOpts
        let (nm, st, _) = RWS.runRWS (qualifiedNameString (varName x))
                                     (GHC2CoreEnv noSrcSpan emptyFamInstEnvs opts)
                                     emptyGHC2CoreState
        pendingWarnings <>= st ^. pendingWarnings
        go nm args
      | otherwise
      = term' e
      where
        -- Remove most Signal transformers
        go "Clash.Signal.Internal.mapSignal#"  args
          | length args == 5
          = term (App (args!!3) (args!!4))
        go "Clash.Signal.Internal.signal#"     args
          | length args == 3
          = term (args!!2)
        go "Clash.Signal.Internal.appSignal#"  args
          | length args == 5
          = term (App (args!!3) (args!!4))
        go "Clash.Signal.Internal.joinSignal#" args
          | length args == 3
          = term (args!!2)
        go "Clash.Signal.Bundle.vecBundle#"    args
          | length args == 4
          = term (args!!3)
        --- Remove `$`
        go nm args
          | nm == showt '($), length args == 5
          = term (App (args!!3) (args!!4))
        go nm args  -- noinline :: forall a. a -> a
          | nm == showt 'GHC.Magic.noinline, [_ty, x] <- args
          = term x
        -- Remove most CallStack logic
        go nm args
          | nm == showt 'GHC.Stack.Types.PushCallStack
          = term (last args)
        go nm args
          | nm == showt 'GHC.Stack.Types.FreezeCallStack
          = term (last args)
        go nm args
          | nm == showt 'GHC.Stack.withFrozenCallStack, length args == 3
          = term (App (args!!2) (args!!1))
        go "Clash.Sized.BitVector.Internal.checkUnpackUndef" args
          | [_nTy,_aTy,_kn,_typ,f] <- args
          = term f
        go "Clash.Magic.prefixName" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod PrefixName nmTy f
        go "Clash.Magic.suffixName" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod SuffixName nmTy f
        go "Clash.Magic.suffixNameFromNat" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod SuffixName nmTy f
        go "Clash.Magic.suffixNameP" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod SuffixNameP nmTy f
        go "Clash.Magic.suffixNameFromNatP" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod SuffixNameP nmTy f
        go "Clash.Magic.setName" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod SetName nmTy f
        go "Clash.Magic.deDup" args
          | [_aTy,f] <- args
          = tickB deDupTickB <$> term f
        go "Clash.Magic.noDeDup" args
          | [_aTy,f] <- args
          = tickB noDeDupTickB <$> term f
        go "Clash.Magic.clashSimulation" _
          = dataB <$> coreToDataCon falseDataCon
        go "Clash.XException.xToErrorCtx" args
          -- xToErrorCtx :: forall a. String -> a -> a
          | [_ty, _msg, x] <- args
          = term x
        go "Clash.Annotations.SynthesisAttributes.annotateReg" args
          | [ Type nTy, _domTy, _aTy, attrs, x] <- args
          = tickB <$> (attributesTickB <$> coreToTypeB nTy <*> term attrs) <*> term x
        go nm args
          | Just n <- parseBundle "bundle" nm
            -- length args = domain tyvar + signal arg + number of type vars
          , length args == 2 + n
          = term (last args)
        go nm args
          | Just n <- parseBundle "unbundle" nm
            -- length args = domain tyvar + signal arg + number of type vars
          , length args == 2 + n
          = term (last args)
        go _ _ = term' e

        nameMod m nmTy f = tickB <$> (nameModTickB m <$> coreToTypeB nmTy) <*> term f

    parseBundle :: Text -> Text -> Maybe Int
    parseBundle fNm nm0 = do
      nm1 <- Text.stripPrefix ("Clash.Signal.Bundle." <> fNm) nm0
      nm2 <- Text.stripSuffix "#" nm1
      Text.readMaybe (Text.unpack nm2)

    term' (Var x)                 = var x
    term' (Lit l@LitRubbish{}) = do
      let rubbish ty = mkPrimInfo (pack "_RUBBISH_") ty WorkNever SingleResult S.NoUnfolding
      envs <- view famInstEnvs
      ty <- coreToTypeB (literalType l)
      -- The shape of 'Clash.Core.Scoped.Util.primAtType', see the module header
      if null (convertedTyVars envs (literalType l))
        then pure (primB (rubbish (runClosedType ty)))
        else pure (tyAppB (primB (rubbish undefinedTy)) ty)
    term' (Lit l)                 = return $ literalB (coreToLiteral l)
    term' (App eFun (Type tyArg)) = tyAppB <$> term eFun <*> coreToTypeB tyArg
    term' (App eFun eArg)         = appB   <$> term eFun <*> term eArg
    term' (Lam x e)
      | isTyVar x
      = do
        (key, occ, k) <- coreToTyVarB x
        tyLamB key occ k <$> addUsefull (getSrcSpan x) (term e)
      | otherwise
      = do
        (e',sp) <- termSP (getSrcSpan x) e
        (ty, occ) <- coreToIdSP sp x
        return (lamB (varKey x) occ ty e')
    term' (Let (NonRec x e1) e2)  = do
      (e1',sp) <- termSP (getSrcSpan x) e1
      (ty, occ)  <- coreToIdSP sp x
      e2' <- term e2
      return (letB (varKey x) occ ty e1' e2')

    term' (Let (Rec xes) e) = do
      xes' <- mapM go xes
      e'   <- term e
      return (letrecB xes' e')
     where
      go (x,b) = do
        (b',sp) <- termSP (getSrcSpan x) b
        (ty, occ) <- coreToIdSP sp x
        return (varKey x, occ, ty, b')

    term' (Case s _ ty [])  = do
      s'  <- term' s
      ty' <- coreToTypeB ty
      case tmbHead s' of
        Just p | S.primName p `elem` undefinedXPrims ->
          -- See 'Clash.GHC.GHC2Core.coreToTerm'
          return (tyAppB (primB NP.undefinedX) ty')
        _ ->
          return (tyAppB (primB NP.undefined) ty')

    term' (Case e b ty alts) = do
     let usesBndr = any ( not . isEmptyVarSet . exprSomeFreeVars (== b))
                  $ rhssOfAlts alts
     (e',sp) <- termSP (getSrcSpan b) e
     (bTy, bOcc) <- coreToIdSP sp b
     ty' <- coreToTypeB ty
     let caseTerm v =
             caseB v ty' <$> mapM (addUsefull sp . alt sp) alts
     if usesBndr
      then do
        ct <- caseTerm (varB (varKey b))
        return (letB (varKey b) bOcc bTy e' ct)
      else caseTerm e'

    term' (Cast e co) = do
      let (Pair ty1 ty2) = coercionKind co
      hasPrimCoM <- hasPrimCo co
      sizedCast <- isSizedCast ty1 ty2
      case hasPrimCoM of
        Just _ | sizedCast
          -> castB <$> term e <*> coreToTypeB ty1 <*> coreToTypeB ty2
        _ -> term e
    term' (Tick (SourceNote rsp _) e) =
      tickB (srcSpanTickB (RealSrcSpan rsp GHC.Nothing)) <$>
             addUsefull (RealSrcSpan rsp GHC.Nothing) (term e)
    term' (Tick _ e) = term e
    term' (Type t) = tyAppB (primB tyPrim) <$> coreToTypeB t
    term' (Coercion co) = tyAppB (primB coPrim) <$> coreToTypeB (coercionType co)

    termSP sp = fmap (second unSrcSpanRB) . RWS.listen . addUsefullR sp . term
    coreToIdSP sp = addUsefullR sp . coreToIdB

    lookupPrim :: Text -> Maybe (Maybe CompiledPrimitive)
    lookupPrim nm = extractPrim <$> HashMap.lookup nm primMap

    var x = do
        xPrim <- if isGlobalId x then coreToPrimVar x else coreToVar x
        -- Converted for every variable, as 'coreToTerm' does, although only
        -- primitives use it: converting it caches the names in it, see
        -- 'coreToTypeB'. Its closed type is only built when used.
        xType  <- coreToVarTypeB x
        let xNameS = C.nameOcc xPrim
            -- The builder terms split the GHC type
            xTy = varType x
            prim wi unf = primB (mkPrimInfo xNameS xType wi SingleResult unf)
            primVar wi =
              if x `elem` unlocs
                then return (prim wi S.NoUnfolding)
                else prim wi . S.Unfolding <$> coreToGlobalIdB x
        case isDataConId_maybe x of
          Just dc -> case lookupPrim xNameS of
            Just p  ->
              -- Primitive will be marked MultiResult in Transformations if it
              -- is a multi result primitive.
              return $ prim (maybe WorkVariable workInfo p) S.NoUnfolding
            Nothing -> if isDataConWrapId x && not (isNewTyCon (dataConTyCon dc))
              then let xInfo = idInfo x
                       unfolding = unfoldingInfo xInfo
                   in  case unfolding of
                          CoreUnfolding {uf_tmpl} -> do
                            sp <- view srcSpan
                            RWS.censor (const (SrcSpanRB sp)) (term uf_tmpl)
                          NoUnfolding -> error ("No unfolding for DC wrapper: " ++ showPprUnsafe x)
                          _ -> error ("Unexpected unfolding for DC wrapper: " ++ showPprUnsafe x)
              else dataB <$> coreToDataCon dc
          Nothing -> case lookupPrim xNameS of
            Just (Just (Primitive f wi _))
              | Just n <- parseBundle "bundle" f        -> bundleUnbundleB (n+1) xTy
              | Just n <- parseBundle "unbundle" f      -> bundleUnbundleB (n+1) xTy
              | f == "Clash.Signal.Internal.mapSignal#" -> mapSignalB xTy
              | f == "Clash.Signal.Internal.signal#"    -> signalB xTy
              | f == "Clash.Signal.Internal.appSignal#" -> appSignalB xTy
              | f == "Clash.Signal.Internal.traverse#"  -> traverseB xTy
              | f == "Clash.Signal.Internal.joinSignal#" -> joinB xTy
              | f == "Clash.Signal.Bundle.vecBundle#"   -> vecUnwrapB xTy
              | f == showt '($)                           -> dollarB xTy
              | f == showt 'GHC.Stack.withFrozenCallStack -> withFrozenCallStackB xTy
              | f == showt 'GHC.Magic.noinline            -> idB xTy
              | f == showt 'GHC.Magic.lazy                -> idB xTy
              -- See 'Clash.GHC.GHC2Core.coreToTerm'
              | f == "GHC.Magic.nospec"                   -> idB xTy
              | f == "GHC.Internal.Magic.nospec"          -> idB xTy
              | f == showt 'GHC.Magic.runRW#              -> runRWB xTy
              | f == "Clash.Sized.Internal.BitVector.checkUnpackUndef" -> checkUnpackUndefB xTy
              | f == "Clash.Magic.prefixName"
              -> nameModB PrefixName xTy
              | f == "Clash.Magic.postfixName"
              -> nameModB SuffixName xTy
              | f == "Clash.Magic.setName"
              -> nameModB SetName xTy
              | f == "Clash.XException.xToErrorCtx"
              -> xToErrorCtxB xTy
              | f == "Clash.Annotations.SynthesisAttributes.annotateReg"
              -> annotateRegB xTy
              | otherwise
              -> primVar wi
            Just (Just (BlackBox {workInfo = wi}))
              -> primVar wi
            Just (Just (BlackBoxHaskell {workInfo = wi}))
              -> primVar wi
            Just Nothing ->
              -- Was guarded by "DontTranslate". We don't know yet if Clash will
              -- actually use it later on, so we don't err here.
              return $ prim WorkVariable S.NoUnfolding
            Nothing
              | x `elem` unlocs ->
                  return (prim WorkVariable S.NoUnfolding)
              | isGlobalId x, not (x `elemVarSet` recLocals)
              -> globalB <$> coreToGlobalIdB x
              | otherwise
              -> return (varB (varKey x))

    alt _   (Alt DEFAULT      _  e) = defaultAltB <$> term e
    alt _   (Alt (LitAlt l)   _  e) = litAltB (coreToLiteral l) <$> term e
    alt sp0 (Alt (DataAlt dc) xs e) = case span isTyVar xs of
      (tyvs,tmvs) -> do
        (e',sp1) <- termSP sp0 e
        dc' <- coreToDataCon dc
        tyvs' <- mapM coreToTyVarB tyvs
        tmvs' <- mapM (\v -> (\(ty, occ) -> (varKey v, occ, ty)) <$> coreToIdSP sp1 v) tmvs
        return (dataAltB dc' tyvs' tmvs' e')

-- | Convert a term variable binder: its type and name. Like
-- 'Clash.GHC.GHC2Core.coreToId', which converts the type of a global through
-- 'coreToVarType'.
coreToIdB :: Id -> C2C (TyB, C.Name S.Term)
coreToIdB i = (,) <$> coreToTypeB (varType i) <*> coreToVar i

-- | The primitives that stand in for a type and a coercion, see the module
-- header
tyPrim, coPrim :: S.PrimInfo
tyPrim = mkPrimInfo (pack "_TY_") undefinedTy WorkNever SingleResult S.NoUnfolding
coPrim = mkPrimInfo (pack "_CO_") undefinedTy WorkNever SingleResult S.NoUnfolding

-- * Builder terms
--
-- Closed terms for primitives that GHC2Core replaces, given the GHC type of
-- the primitive. Their bodies only refer to their own binders, so the keys
-- of their term variables are arbitrary, see "Clash.Core.Scoped.Builder".
--
-- They split the GHC type as 'Clash.GHC.GHC2Core' splits its conversion to
-- Clash Core, and convert the parts. These are part of the conversion of the
-- whole type, which the caller converts first, so this looks up no new names.

-- | Split a forall-type, like matching 'C.ForAllTy' on the conversion of the
-- type
forAllView :: Type -> Maybe (TyVar, Type)
forAllView ty | Just ty' <- coreView ty = forAllView ty'
forAllView (ForAllTy (Bndr tv _) ty) = Just (tv, ty)
forAllView _ = Nothing

-- | Split a function type, like matching 'C.FunTy' of 'C.tyView' on the
-- conversion of the type
funView :: Type -> Maybe (Type, Type)
funView ty | Just ty' <- coreView ty = funView ty'
funView (FunTy _ _ a r) = Just (a, r)
funView (TyConApp tc args)
  | Just (FunTy _ _ a r) <- tyConAppFunTy_maybe tc args = Just (a, r)
funView _ = Nothing

-- | Type lambdas binding the given type variables. Their names are those of
-- the conversion of the type they come from, which runs without a source
-- span, see 'coreToTypeB'.
tyLamsB :: [TyVar] -> TmB -> C2C TmB
tyLamsB tvs body = foldr tyLam (pure body) tvs
 where
  tyLam tv r = do
    (key, occ, k) <- withoutSrcSpan (coreToTyVarB tv)
    tyLamB key occ k <$> r

-- | See 'Clash.GHC.GHC2Core.bundleUnbundleTerm'
bundleUnbundleB :: Int -> Type -> C2C TmB
bundleUnbundleB nTyVarsExpected = go []
 where
  go tvs ty
    | Just (tv, ty') <- forAllView ty = go (tv:tvs) ty'
    | Just (argTy, _resTy) <- funView ty =
        if length tvs /= nTyVarsExpected then
          error $ $(curLoc) ++ show (length tvs) ++ " vs " ++ show nTyVarsExpected
        else do
          argTyB <- coreToTypeB argTy
          tyLamsB (reverse tvs) (lamB 0 (C.mkUnsafeSystemName "c$s" 0) argTyB (varB 0))
    | otherwise = error $ $(curLoc) ++ showPprUnsafe ty ++ " " ++ showPprUnsafe tvs

-- | @\\f x -> f x@, for 'mapSignalB', 'appSignalB' and 'dollarB'
applyB :: TyB -> TyB -> TmB
applyB fTy xTy =
  lamB 0 (C.mkUnsafeSystemName "f" 0) fTy $
  lamB 1 (C.mkUnsafeSystemName "x" 1) xTy $
  appB (varB 0) (varB 1)

-- | See 'Clash.GHC.GHC2Core.mapSignalTerm'
mapSignalB :: Type -> C2C TmB
mapSignalB ty
  | Just (aTV, ty1) <- forAllView ty
  , Just (bTV, ty2) <- forAllView ty1
  , Just (clkTV, funTy) <- forAllView ty2
  , Just (_, funTy'') <- funView funTy
  , Just (aTy, bTy) <- funView funTy''
  = do
    aTyB <- coreToTypeB aTy
    bTyB <- coreToTypeB bTy
    tyLamsB [aTV, bTV, clkTV] (applyB (funTyB [aTyB] bTyB) aTyB)
mapSignalB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.signalTerm'
signalB :: Type -> C2C TmB
signalB ty
  | Just (aTV, ty1) <- forAllView ty
  , Just (domTV, funTy) <- forAllView ty1
  , Just (_, saTy) <- funView funTy
  = do
    saTyB <- coreToTypeB saTy
    tyLamsB [aTV, domTV] (lamB 0 (C.mkUnsafeSystemName "x" 0) saTyB (varB 0))
signalB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.appSignalTerm'
appSignalB :: Type -> C2C TmB
appSignalB ty
  | Just (domTV, ty1) <- forAllView ty
  , Just (aTV, ty2) <- forAllView ty1
  , Just (bTV, funTy) <- forAllView ty2
  , Just (_, funTy'') <- funView funTy
  , Just (saTy, sbTy) <- funView funTy''
  = do
    saTyB <- coreToTypeB saTy
    sbTyB <- coreToTypeB sbTy
    tyLamsB [domTV, aTV, bTV] (applyB (funTyB [saTyB] sbTyB) saTyB)
appSignalB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.vecUnwrapTerm'
vecUnwrapB :: Type -> C2C TmB
vecUnwrapB ty
  | Just (tTV, ty1) <- forAllView ty
  , Just (nTV, ty2) <- forAllView ty1
  , Just (aTV, funTy) <- forAllView ty2
  , Just (_, vsTy) <- funView funTy
  = do
    vsTyB <- coreToTypeB vsTy
    tyLamsB [tTV, nTV, aTV] (lamB 0 (C.mkUnsafeSystemName "vs" 0) vsTyB (varB 0))
vecUnwrapB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.traverseTerm'
traverseB :: Type -> C2C TmB
traverseB ty
  | Just (fTV, ty1) <- forAllView ty
  , Just (aTV, ty2) <- forAllView ty1
  , Just (bTV, ty3) <- forAllView ty2
  , Just (domTV, funTy) <- forAllView ty3
  , Just (dictTy, funTy1) <- funView funTy
  , Just (gTy, funTy2) <- funView funTy1
  , Just (xTy, _) <- funView funTy2
  = do
    dictTyB <- coreToTypeB dictTy
    gTyB <- coreToTypeB gTy
    xTyB <- coreToTypeB xTy
    tyLamsB [fTV, aTV, bTV, domTV] $
      lamB 0 (C.mkUnsafeSystemName "dict" 0) dictTyB $
      lamB 1 (C.mkUnsafeSystemName "g" 1) gTyB $
      lamB 2 (C.mkUnsafeSystemName "x" 2) xTyB $
      appB (varB 1) (varB 2)
traverseB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.dollarTerm'
dollarB :: Type -> C2C TmB
#if MIN_VERSION_ghc(9,8,0)
dollarB ty
  | Just (raTV, ty1) <- forAllView ty
  , Just (rbTV, ty2) <- forAllView ty1
  , Just (aTV, ty3) <- forAllView ty2
  , Just (bTV, funTy) <- forAllView ty3
  , Just (fTy, funTy'') <- funView funTy
  , Just (aTy, _) <- funView funTy''
  = do
    fTyB <- coreToTypeB fTy
    aTyB <- coreToTypeB aTy
    tyLamsB [raTV, rbTV, aTV, bTV] (applyB fTyB aTyB)
#else
dollarB ty
  | Just (rTV, ty1) <- forAllView ty
  , Just (aTV, ty2) <- forAllView ty1
  , Just (bTV, funTy) <- forAllView ty2
  , Just (fTy, funTy'') <- funView funTy
  , Just (aTy, _) <- funView funTy''
  = do
    fTyB <- coreToTypeB fTy
    aTyB <- coreToTypeB aTy
    tyLamsB [rTV, aTV, bTV] (applyB fTyB aTyB)
#endif
dollarB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.joinTerm'
joinB :: Type -> C2C TmB
joinB ty
  | Just _ <- forAllView ty = signalB ty
  | otherwise = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.withFrozenCallStackTerm'
withFrozenCallStackB :: Type -> C2C TmB
withFrozenCallStackB ty
  | Just (aTV, funTy) <- forAllView ty
  , Just (callStackTy, fTy) <- funView funTy
  = do
    callStackTyB <- coreToTypeB callStackTy
    fTyB <- coreToTypeB fTy
    tyLamsB [aTV] $
      lamB 0 (C.mkUnsafeSystemName "callStack" 0) callStackTyB $
      lamB 1 (C.mkUnsafeSystemName "f" 1) fTyB $
      appB (varB 1) (varB 0)
withFrozenCallStackB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.idTerm'
idB :: Type -> C2C TmB
idB ty
  | Just (aTV, funTy) <- forAllView ty
  , Just (xTy, _) <- funView funTy
  = do
    xTyB <- coreToTypeB xTy
    tyLamsB [aTV] (lamB 0 (C.mkUnsafeSystemName "x" 0) xTyB (varB 0))
idB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.runRWTerm'
runRWB :: Type -> C2C TmB
runRWB ty
  | Just (rTV, ty1) <- forAllView ty
  , Just (oTV, funTy) <- forAllView ty1
  , Just (fTy, _) <- funView funTy
  , Just (rwTy, _) <- funView fTy
  = do
    fTyB <- coreToTypeB fTy
    rwTyB <- coreToTypeB rwTy
    let realWorld = mkPrimInfo (showt 'GHC.Prim.realWorld#) (runClosedType rwTyB)
                      WorkNever SingleResult S.NoUnfolding
    tyLamsB [rTV, oTV] $
      lamB 0 (C.mkUnsafeSystemName "f" 0) fTyB $
      appB (varB 0) (primB realWorld)
runRWB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.checkUnpackUndefTerm'
checkUnpackUndefB :: Type -> C2C TmB
checkUnpackUndefB ty
  | Just (nTV, ty1) <- forAllView ty
  , Just (aTV, funTy) <- forAllView ty1
  , Just (knTy, r0Ty) <- funView funTy
  , Just (tpTy, r1Ty) <- funView r0Ty
  , Just (fTy, _) <- funView r1Ty
  = do
    knTyB <- coreToTypeB knTy
    tpTyB <- coreToTypeB tpTy
    fTyB <- coreToTypeB fTy
    tyLamsB [nTV, aTV] $
      lamB 0 (C.mkUnsafeSystemName "kn" 0) knTyB $
      lamB 1 (C.mkUnsafeSystemName "tp" 1) tpTyB $
      lamB 2 (C.mkUnsafeSystemName "f" 2) fTyB $
      varB 2
checkUnpackUndefB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.nameModTerm'
nameModB :: NameMod -> Type -> C2C TmB
nameModB sa ty
  | Just (nmTV, ty1) <- forAllView ty
  , Just (aTV, funTy) <- forAllView ty1
  , Just (xTy, _) <- funView funTy
  = do
    xTyB <- coreToTypeB xTy
    tyLamsB [nmTV, aTV] $
      lamB 0 (C.mkUnsafeSystemName "x" 0) xTyB $
      tickB (nameModTickB sa (varTyB (varKey nmTV))) (varB 0)
nameModB _ ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.xToErrorCtxTerm'
xToErrorCtxB :: Type -> C2C TmB
xToErrorCtxB ty
  | Just (aTV, funTy) <- forAllView ty
  , Just (ctxTy, rTy) <- funView funTy
  , Just (xTy, _) <- funView rTy
  = do
    ctxTyB <- coreToTypeB ctxTy
    xTyB <- coreToTypeB xTy
    tyLamsB [aTV] $
      lamB 0 (C.mkUnsafeSystemName "ctx" 0) ctxTyB $
      lamB 1 (C.mkUnsafeSystemName "x" 1) xTyB $
      varB 1
xToErrorCtxB ty = error $ $(curLoc) ++ showPprUnsafe ty

-- | See 'Clash.GHC.GHC2Core.annotateRegTerm'
annotateRegB :: Type -> C2C TmB
annotateRegB ty
  | Just (nTV, ty1) <- forAllView ty
  , Just (domTV, ty2) <- forAllView ty1
  , Just (aTV, funTy) <- forAllView ty2
  , Just (attrTy, rTy) <- funView funTy
  , Just (xTy, _) <- funView rTy
  = do
    attrTyB <- coreToTypeB attrTy
    xTyB <- coreToTypeB xTy
    tyLamsB [nTV, domTV, aTV] $
      lamB 1 (C.mkUnsafeSystemName "attrs" 1) attrTyB $
      lamB 0 (C.mkUnsafeSystemName "x" 0) xTyB $
      tickB (attributesTickB (varTyB (varKey nTV)) (varB 1)) (varB 0)
annotateRegB ty = error ($(curLoc) ++ showPprUnsafe ty)
