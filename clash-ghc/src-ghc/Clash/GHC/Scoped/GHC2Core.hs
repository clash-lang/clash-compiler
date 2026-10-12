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

  The types in terms are types of scoped Core, built by 'coreToTypeB' from
  the types of Clash Core that 'Clash.GHC.GHC2Core.coreToType' gives. The
  types of global variables, data constructors and primitives stay types of
  Clash Core.

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

import           Control.Lens                ((^.), (%=), (<>=), use, view)
import qualified Control.Lens                as Lens
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
import GHC.Types.Var
  (Id, TyVar, Var, isTyVar, varName, varType, varUnique, idInfo,
   isGlobalId)
import GHC.Types.Var.Set (VarSet, elemVarSet, isEmptyVarSet)
import GHC.Utils.Outputable (showPprUnsafe)

-- Local imports
import           Clash.Annotations.Primitive (extractPrim)
import qualified Clash.Core.FreeVars         as C
import qualified Clash.Core.Name             as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Util             as C (undefinedTy, undefinedXPrims)
import           Clash.Core.Scoped.Convert (mkGlobalIdFromClash)
import           Clash.Driver.Types          (HasClashOpts(..))
import           Clash.GHC.GHC2Core
  (C2C, GHC2CoreEnv (..), SrcSpanRB (..), StructuralType (..), addUsefull,
   addUsefullR, coreToDataCon, coreToId, coreToLiteral, coreToType,
   coreToPrimVar, coreToVar, coreToVarType, emptyGHC2CoreState, hasPrimCo,
   isDataConWrapId, isSizedCast, pendingWarnings, qualifiedNameString,
   scopedTypeCache, srcSpan)
import           Clash.Core.Scoped.Builder
import qualified Clash.Normalize.Primitives  as C
import           Clash.Primitives.Types      hiding (name)
import           Clash.Unique                (fromGhcUnique)
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
          = nameMod C.PrefixName nmTy f
        go "Clash.Magic.suffixName" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod C.SuffixName nmTy f
        go "Clash.Magic.suffixNameFromNat" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod C.SuffixName nmTy f
        go "Clash.Magic.suffixNameP" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod C.SuffixNameP nmTy f
        go "Clash.Magic.suffixNameFromNatP" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod C.SuffixNameP nmTy f
        go "Clash.Magic.setName" args
          | [Type nmTy,_aTy,f] <- args
          = nameMod C.SetName nmTy f
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
      let rubbish ty = C.PrimInfo (pack "_RUBBISH_") ty C.WorkNever C.SingleResult C.NoUnfolding
      ty <- coreToType (literalType l)
      -- The shape of 'Clash.Core.Scoped.Util.primAtType', see the module header
      if Lens.notNullOf C.typeFreeVars ty
        then tyAppB (primB (rubbish C.undefinedTy)) <$> coreToTypeB (literalType l)
        else pure (primB (rubbish ty))
    term' (Lit l)                 = return $ literalB (coreToLiteral l)
    term' (App eFun (Type tyArg)) = tyAppB <$> term eFun <*> coreToTypeB tyArg
    term' (App eFun eArg)         = appB   <$> term eFun <*> term eArg
    term' (Lam x e)
      | isTyVar x
      = do
        (k, occ) <- coreToTyVarB x
        tyLamB (varKey x) occ k <$> addUsefull (getSrcSpan x) (term e)
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
        Just p | C.primName p `elem` C.undefinedXPrims ->
          -- See 'Clash.GHC.GHC2Core.coreToTerm'
          return (tyAppB (primB C.undefinedX) ty')
        _ ->
          return (tyAppB (primB C.undefined) ty')

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
    term' (Type t) =
      tyAppB (primB (C.PrimInfo (pack "_TY_") C.undefinedTy C.WorkNever C.SingleResult C.NoUnfolding))
        <$> coreToTypeB t
    term' (Coercion co) =
      tyAppB (primB (C.PrimInfo (pack "_CO_") C.undefinedTy C.WorkNever C.SingleResult C.NoUnfolding))
        <$> coreToTypeB (coercionType co)

    termSP sp = fmap (second unSrcSpanRB) . RWS.listen . addUsefullR sp . term
    coreToIdSP sp = addUsefullR sp . coreToIdB

    lookupPrim :: Text -> Maybe (Maybe CompiledPrimitive)
    lookupPrim nm = extractPrim <$> HashMap.lookup nm primMap

    var x = do
        xPrim <- if isGlobalId x then coreToPrimVar x else coreToVar x
        let xNameS = C.nameOcc xPrim
        -- Converted for every variable, as 'coreToTerm' does, although only
        -- primitives use it: converting it caches the names in it, see
        -- 'coreToTypeB'.
        xType  <- coreToVarType x
        case isDataConId_maybe x of
          Just dc -> case lookupPrim xNameS of
            Just p  ->
              -- Primitive will be marked MultiResult in Transformations if it
              -- is a multi result primitive.
              return $ primB (C.PrimInfo xNameS xType (maybe C.WorkVariable workInfo p) C.SingleResult C.NoUnfolding)
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
              | Just n <- parseBundle "bundle" f        -> return (bundleUnbundleB (n+1) xType)
              | Just n <- parseBundle "unbundle" f      -> return (bundleUnbundleB (n+1) xType)
              | f == "Clash.Signal.Internal.mapSignal#" -> return (mapSignalB xType)
              | f == "Clash.Signal.Internal.signal#"    -> return (signalB xType)
              | f == "Clash.Signal.Internal.appSignal#" -> return (appSignalB xType)
              | f == "Clash.Signal.Internal.traverse#"  -> return (traverseB xType)
              | f == "Clash.Signal.Internal.joinSignal#" -> return (joinB xType)
              | f == "Clash.Signal.Bundle.vecBundle#"   -> return (vecUnwrapB xType)
              | f == showt '($)                           -> return (dollarB xType)
              | f == showt 'GHC.Stack.withFrozenCallStack -> return (withFrozenCallStackB xType)
              | f == showt 'GHC.Magic.noinline            -> return (idB xType)
              | f == showt 'GHC.Magic.lazy                -> return (idB xType)
              -- See 'Clash.GHC.GHC2Core.coreToTerm'
              | f == "GHC.Magic.nospec"                   -> return (idB xType)
              | f == "GHC.Internal.Magic.nospec"          -> return (idB xType)
              | f == showt 'GHC.Magic.runRW#              -> return (runRWB xType)
              | f == "Clash.Sized.Internal.BitVector.checkUnpackUndef" -> return (checkUnpackUndefB xType)
              | f == "Clash.Magic.prefixName"
              -> return (nameModB C.PrefixName xType)
              | f == "Clash.Magic.postfixName"
              -> return (nameModB C.SuffixName xType)
              | f == "Clash.Magic.setName"
              -> return (nameModB C.SetName xType)
              | f == "Clash.XException.xToErrorCtx"
              -> return (xToErrorCtxB xType)
              | f == "Clash.Annotations.SynthesisAttributes.annotateReg"
              -> return (annotateRegB xType)
              | otherwise
              -> primVar xNameS xType wi
            Just (Just (BlackBox {workInfo = wi}))
              -> primVar xNameS xType wi
            Just (Just (BlackBoxHaskell {workInfo = wi}))
              -> primVar xNameS xType wi
            Just Nothing ->
              -- Was guarded by "DontTranslate". We don't know yet if Clash will
              -- actually use it later on, so we don't err here.
              return $ primB (C.PrimInfo xNameS xType C.WorkVariable C.SingleResult C.NoUnfolding)
            Nothing
              | x `elem` unlocs ->
                  return (primB (C.PrimInfo xNameS xType C.WorkVariable C.SingleResult C.NoUnfolding))
              | isGlobalId x, not (x `elemVarSet` recLocals)
              -> globalB . mkGlobalIdFromClash <$> coreToId x
              | otherwise
              -> return (varB (varKey x))
      where
        primVar xNameS xType wi =
          if x `elem` unlocs
            then return (primB (C.PrimInfo xNameS xType wi C.SingleResult C.NoUnfolding))
            else do
              bndr <- coreToId x
              return (primB (C.PrimInfo xNameS xType wi C.SingleResult (C.Unfolding bndr)))

    alt _   (Alt DEFAULT      _  e) = defaultAltB <$> term e
    alt _   (Alt (LitAlt l)   _  e) = litAltB (coreToLiteral l) <$> term e
    alt sp0 (Alt (DataAlt dc) xs e) = case span isTyVar xs of
      (tyvs,tmvs) -> do
        (e',sp1) <- termSP sp0 e
        dc' <- coreToDataCon dc
        tyvs' <- mapM (\tv -> (\(k, occ) -> (varKey tv, occ, k)) <$> coreToTyVarB tv) tyvs
        tmvs' <- mapM (\v -> (\(ty, occ) -> (varKey v, occ, ty)) <$> coreToIdSP sp1 v) tmvs
        return (dataAltB dc' tyvs' tmvs' e')

-- | The key of a GHC variable
varKey :: Var -> Key
varKey = uniqueKey . fromGhcUnique . varUnique

-- | Convert a term variable binder: its type and name. Like
-- 'Clash.GHC.GHC2Core.coreToId', which converts the type of a global through
-- 'coreToVarType'.
coreToIdB :: Id -> C2C (TyB, C.Name C.Term)
coreToIdB i = (,) <$> coreToTypeB (varType i) <*> coreToVar i

-- | Convert a type variable binder: its kind and name. Like
-- 'Clash.GHC.GHC2Core.coreToTyVar'.
coreToTyVarB :: TyVar -> C2C (TyB, C.Name C.Type)
coreToTyVarB tv = (,) <$> coreToTypeB (varType tv) <*> coreToVar tv

-- * Types

-- | Convert a GHC type to a type of scoped Core: the type of Clash Core that
-- 'Clash.GHC.GHC2Core.coreToType' gives, as a builder ('clashTyB').
--
-- Converting a GHC type caches the names in it by unique (see
-- 'Clash.GHC.GHC2Core.qualifiedNameStringM'), and different GHC names can
-- share a unique. So going through 'Clash.GHC.GHC2Core.coreToType', and its
-- cache, at the same points as 'Clash.GHC.GHC2Core.coreToTerm' does, keeps
-- the names the same as those of the conversion through Clash Core.
--
-- Memoized on the GHC type, like 'Clash.GHC.GHC2Core.coreToType', which
-- shares the closed types without binders, see "Clash.Core.Scoped.Builder".
coreToTypeB :: Type -> C2C TyB
coreToTypeB ty = do
  cache <- use scopedTypeCache
  case HashMap.lookup (StructuralType ty) cache of
    Just ty1 -> pure ty1
    Nothing -> do
      ty1 <- clashTyB <$> coreToType ty
      scopedTypeCache %= HashMap.insert (StructuralType ty) ty1
      pure ty1

-- * Builder terms
--
-- Closed terms for primitives that GHC2Core replaces, given the (closed) type
-- of the primitive. Their bodies only refer to their own binders, so the keys
-- of their term variables are arbitrary, see "Clash.Core.Scoped.Builder".

-- | See 'Clash.GHC.GHC2Core.bundleUnbundleTerm'
bundleUnbundleB :: Int -> C.Type -> TmB
bundleUnbundleB nTyVarsExpected = go []
 where
  go :: [C.TyVar] -> C.Type -> TmB
  go tvs (C.ForAllTy tv typ) = go (tv:tvs) typ
  go tvs (C.tyView -> C.FunTy argTy _resTy) =
    if length tvs /= nTyVarsExpected then
      error $ $(curLoc) ++ show (length tvs) ++ " vs " ++ show nTyVarsExpected
    else
      foldr tyLamClashB (lamB 0 (C.mkUnsafeSystemName "c$s" 0) (clashTyB argTy) (varB 0)) (reverse tvs)
  go tvs ty = error $ $(curLoc) ++ show ty ++ " " ++ show tvs

-- | @\\f x -> f x@, for 'mapSignalB', 'appSignalB' and 'dollarB'
applyB :: C.Type -> C.Type -> TmB
applyB fTy xTy =
  lamB 0 (C.mkUnsafeSystemName "f" 0) (clashTyB fTy) $
  lamB 1 (C.mkUnsafeSystemName "x" 1) (clashTyB xTy) $
  appB (varB 0) (varB 1)

-- | See 'Clash.GHC.GHC2Core.mapSignalTerm'
mapSignalB :: C.Type -> TmB
mapSignalB (C.ForAllTy aTV (C.ForAllTy bTV (C.ForAllTy clkTV funTy)))
  | (C.FunTy _ funTy'') <- C.tyView funTy
  , (C.FunTy aTy bTy)   <- C.tyView funTy''
  = tyLamClashB aTV $ tyLamClashB bTV $ tyLamClashB clkTV $
    applyB (C.mkFunTy aTy bTy) aTy
mapSignalB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.signalTerm'
signalB :: C.Type -> TmB
signalB (C.ForAllTy aTV (C.ForAllTy domTV funTy))
  | (C.FunTy _ saTy) <- C.tyView funTy
  = tyLamClashB aTV $ tyLamClashB domTV $
    lamB 0 (C.mkUnsafeSystemName "x" 0) (clashTyB saTy) (varB 0)
signalB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.appSignalTerm'
appSignalB :: C.Type -> TmB
appSignalB (C.ForAllTy domTV (C.ForAllTy aTV (C.ForAllTy bTV funTy)))
  | (C.FunTy _ funTy'') <- C.tyView funTy
  , (C.FunTy saTy sbTy) <- C.tyView funTy''
  = tyLamClashB domTV $ tyLamClashB aTV $ tyLamClashB bTV $
    applyB (C.mkFunTy saTy sbTy) saTy
appSignalB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.vecUnwrapTerm'
vecUnwrapB :: C.Type -> TmB
vecUnwrapB (C.ForAllTy tTV (C.ForAllTy nTV (C.ForAllTy aTV funTy)))
  | (C.FunTy _ vsTy) <- C.tyView funTy
  = tyLamClashB tTV $ tyLamClashB nTV $ tyLamClashB aTV $
    lamB 0 (C.mkUnsafeSystemName "vs" 0) (clashTyB vsTy) (varB 0)
vecUnwrapB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.traverseTerm'
traverseB :: C.Type -> TmB
traverseB (C.ForAllTy fTV (C.ForAllTy aTV (C.ForAllTy bTV (C.ForAllTy domTV funTy))))
    | (C.FunTy dictTy funTy1) <- C.tyView funTy
    , (C.FunTy gTy    funTy2) <- C.tyView funTy1
    , (C.FunTy xTy    _)      <- C.tyView funTy2
    = tyLamClashB fTV $ tyLamClashB aTV $ tyLamClashB bTV $ tyLamClashB domTV $
      lamB 0 (C.mkUnsafeSystemName "dict" 0) (clashTyB dictTy) $
      lamB 1 (C.mkUnsafeSystemName "g" 1) (clashTyB gTy) $
      lamB 2 (C.mkUnsafeSystemName "x" 2) (clashTyB xTy) $
      appB (varB 1) (varB 2)
traverseB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.dollarTerm'
dollarB :: C.Type -> TmB
#if MIN_VERSION_ghc(9,8,0)
dollarB (C.ForAllTy raTV (C.ForAllTy rbTV (C.ForAllTy aTV (C.ForAllTy bTV funTy))))
  | (C.FunTy fTy funTy'') <- C.tyView funTy
  , (C.FunTy aTy _)       <- C.tyView funTy''
  = tyLamClashB raTV $ tyLamClashB rbTV $ tyLamClashB aTV $ tyLamClashB bTV $
    applyB fTy aTy
#else
dollarB (C.ForAllTy rTV (C.ForAllTy aTV (C.ForAllTy bTV funTy)))
  | (C.FunTy fTy funTy'') <- C.tyView funTy
  , (C.FunTy aTy _)       <- C.tyView funTy''
  = tyLamClashB rTV $ tyLamClashB aTV $ tyLamClashB bTV $
    applyB fTy aTy
#endif
dollarB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.joinTerm'
joinB :: C.Type -> TmB
joinB ty@(C.ForAllTy {}) = signalB ty
joinB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.withFrozenCallStackTerm'
withFrozenCallStackB :: C.Type -> TmB
withFrozenCallStackB (C.ForAllTy aTV funTy)
  | (C.FunTy callStackTy fTy) <- C.tyView funTy
  = tyLamClashB aTV $
    lamB 0 (C.mkUnsafeSystemName "callStack" 0) (clashTyB callStackTy) $
    lamB 1 (C.mkUnsafeSystemName "f" 1) (clashTyB fTy) $
    appB (varB 1) (varB 0)
withFrozenCallStackB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.idTerm'
idB :: C.Type -> TmB
idB (C.ForAllTy aTV funTy)
  | (C.FunTy xTy _) <- C.tyView funTy
  = tyLamClashB aTV $
    lamB 0 (C.mkUnsafeSystemName "x" 0) (clashTyB xTy) (varB 0)
idB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.runRWTerm'
runRWB :: C.Type -> TmB
runRWB (C.ForAllTy rTV (C.ForAllTy oTV funTy))
  | (C.FunTy fTy _)  <- C.tyView funTy
  , (C.FunTy rwTy _) <- C.tyView fTy
  = tyLamClashB rTV $ tyLamClashB oTV $
    lamB 0 (C.mkUnsafeSystemName "f" 0) (clashTyB fTy) $
    appB (varB 0)
         (primB (C.PrimInfo (showt 'GHC.Prim.realWorld#) rwTy C.WorkNever C.SingleResult C.NoUnfolding))
runRWB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.checkUnpackUndefTerm'
checkUnpackUndefB :: C.Type -> TmB
checkUnpackUndefB (C.ForAllTy nTV (C.ForAllTy aTV funTy))
  | C.FunTy knTy r0Ty <- C.tyView funTy
  , C.FunTy tpTy r1Ty <- C.tyView r0Ty
  , C.FunTy fTy _     <- C.tyView r1Ty
  = tyLamClashB nTV $ tyLamClashB aTV $
    lamB 0 (C.mkUnsafeSystemName "kn" 0) (clashTyB knTy) $
    lamB 1 (C.mkUnsafeSystemName "tp" 1) (clashTyB tpTy) $
    lamB 2 (C.mkUnsafeSystemName "f" 2) (clashTyB fTy) $
    varB 2
checkUnpackUndefB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.nameModTerm'
nameModB :: C.NameMod -> C.Type -> TmB
nameModB sa (C.ForAllTy nmTV (C.ForAllTy aTV funTy))
  | (C.FunTy xTy _) <- C.tyView funTy
  = tyLamClashB nmTV $ tyLamClashB aTV $
    lamB 0 (C.mkUnsafeSystemName "x" 0) (clashTyB xTy) $
    tickB (nameModTickB sa (clashTyB (C.VarTy nmTV))) (varB 0)
nameModB _ ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.xToErrorCtxTerm'
xToErrorCtxB :: C.Type -> TmB
xToErrorCtxB (C.ForAllTy aTV funTy)
  | (C.FunTy ctxTy rTy) <- C.tyView funTy
  , (C.FunTy xTy _)     <- C.tyView rTy
  = tyLamClashB aTV $
    lamB 0 (C.mkUnsafeSystemName "ctx" 0) (clashTyB ctxTy) $
    lamB 1 (C.mkUnsafeSystemName "x" 1) (clashTyB xTy) $
    varB 1
xToErrorCtxB ty = error $ $(curLoc) ++ show ty

-- | See 'Clash.GHC.GHC2Core.annotateRegTerm'
annotateRegB :: C.Type -> TmB
annotateRegB (C.ForAllTy nTV (C.ForAllTy domTV (C.ForAllTy aTV funTy)))
  | C.FunTy attrTy rTy <- C.tyView funTy
  , C.FunTy xTy _ <- C.tyView rTy
  = tyLamClashB nTV $ tyLamClashB domTV $ tyLamClashB aTV $
    lamB 1 (C.mkUnsafeSystemName "attrs" 1) (clashTyB attrTy) $
    lamB 0 (C.mkUnsafeSystemName "x" 0) (clashTyB xTy) $
    tickB (attributesTickB (clashTyB (C.VarTy nTV)) (varB 1)) (varB 0)
annotateRegB ty = error ($(curLoc) ++ show ty)
