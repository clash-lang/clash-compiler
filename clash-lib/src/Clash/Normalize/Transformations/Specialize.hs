{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021     , QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for specialisation.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Transformations.Specialize
  ( appProp
  , constantSpec
  , specialize
  , nonRepSpec
  , typeSpec
  , zeroWidthSpec
  ) where

import Control.Arrow ((***), (&&&))
import Control.DeepSeq (deepseq)
import Control.Exception (throw)
import Control.Lens ((%=))
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Control.Monad.Extra (orM)
import qualified Control.Monad.Writer as Writer (listen)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import qualified Data.Either as Either
import Data.Functor.Const (Const(..))
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid (getAny)
import qualified Data.Set.Ordered as OSet
import qualified Data.Set.Ordered.Extra as OSet
import qualified Data.Text as Text
import qualified Data.Text.Extra as Text
import GHC.Stack (HasCallStack)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic (InlineSpec (..))
#else
import BasicTypes (InlineSpec (..))
#endif

import qualified Clash.Sized.Internal.BitVector as BV (BitVector, fromInteger#)
import qualified Clash.Sized.Internal.Index as I (Index, fromInteger#)
import qualified Clash.Sized.Internal.Signed as S (Signed, fromInteger#)
import qualified Clash.Sized.Internal.Unsigned as U (Unsigned, fromInteger#)

import Clash.Core.DataCon (DataCon(dcArgTys))
import Clash.Core.FreeVars (freeLocalVars, termFreeTyVars, typeFreeVars)
import Clash.Core.Literal (Literal(..))
import Clash.Core.Name
  (NameSort(..), Name(..), appendToName, mkUnsafeInternalName, mkUnsafeSystemName)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Subst
import Clash.Core.Term
  ( Term(..), TickInfo, collectArgs, collectArgsTicks, mkApps, mkTmApps, mkTicks, patIds
  , patVars, mkAbstraction, PrimInfo(..), WorkInfo(..), IsMultiPrim(..), PrimUnfolding(..))
import Clash.Core.TermInfo (isLocalVar, isVar, piResultTy, termType, isPolyFun)
import Clash.Core.TyCon (TyConMap, tyConDataCons)
import Clash.Core.Type
  (LitTy(NumTy), Type(LitTy,VarTy), applyFunTy, splitTyConAppM, normalizeType
  , mkPolyFunTy, mkTyConApp)
import Clash.Core.TysPrim
import Clash.Core.Var (Var(..), Id, TyVar, mkTyVar)
import Clash.Core.VarEnv
  ( InScopeSet, extendInScopeSet, extendInScopeSetList, lookupVarEnv
  , mkInScopeSet, mkVarSet, unionInScope, elemVarSet)
import Clash.Debug (traceIf, traceM)
import Clash.Driver.Types (Binding(..), TransformationInfo(..), hasTransformationInfo)
import Clash.Netlist.Util (representableType)
import Clash.Rewrite.Combinators (topdownR)
import Clash.Rewrite.Types
  ( TransformContext(..), bindings, censor, curFun, customReprs, extra, tcCache
  , typeTranslator, workFreeBinders, debugOpts, topEntities)
import Clash.Rewrite.Util
  ( mkBinderFor, mkDerivedName, mkFunction, mkTmBinderFor, setChanged, changed
  , normalizeTermTypes, normalizeId)
import Clash.Rewrite.WorkFree (isWorkFree)
import Clash.Normalize.Types
  ( NormRewrite, NormalizeSession, specialisationCache, specialisationHistory
  , specialisationLimit)
import Clash.Normalize.Util
  (constantSpecInfo, csrFoundConstant, csrNewBindings, csrNewTerm)
import Clash.Unique
  ( eltsUniqMap, eltsUniqSet, extendUniqMapWith, unitUniqSet, filterUniqMap
  , lookupUniqMap)
import Clash.Util (ClashException(..))

-- | Propagate arguments of application inwards; except for 'Lam' where the
-- argument becomes let-bound. 'appProp' tries to propagate as many arguments
-- as possible, down as many levels as possible; and should be called in a
-- top-down traversal.
--
-- The idea is that this reduces the number of traversals, which hopefully leads
-- to shorter compile times.
--
-- Note [AppProp no shadowing]
--
-- Case 1.
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
-- instead me must deshadow w.r.t. the new variable and rewrite to:
--
-- @
-- let b = f x y
-- in  case x of
--       D a b1 -> h a b
-- @
--
-- Case 2.
--
-- Imagine
--
-- @
-- (\x -> e) u
-- @
--
-- where @u@ has a free variable named @x@, rewriting this to:
--
-- @
-- let x = u
-- in  e
-- @
--
-- would be very bad, because the let-binding suddenly captures the free
-- variable in @u@. To prevent this from happening we over-approximate and check
-- whether @x@ is in the current InScopeSet, and deshadow if that's the case,
-- i.e. we then rewrite to:
--
-- let x1 = u
-- in  e [x:=x1]
--
-- Case 3.
--
-- The same for:
--
-- @
-- (let x = w in e) u
-- @
--
-- where @u@ again has a free variable @x@, rewriting this to:
--
-- @
-- let x = w in (e u)
-- @
--
-- would be bad because the let-binding now captures the free variable in @u@.
--
-- To prevent this from happening, we unconditionally deshadow the function part
-- of the application w.r.t. the free variables in the argument part of the
-- application. It is okay to over-approximate in this case and deshadow w.r.t
-- the current InScopeSet.
appProp :: HasCallStack => NormRewrite
appProp ctx@(TransformContext is _) = \case
  e@App {}
    | let (fun,args,ticks) = collectArgsTicks e
    -> go is (deShadowTerm is fun) args ticks
  e@TyApp {}
    | let (fun,args,ticks) = collectArgsTicks e
    -> go is (deShadowTerm is fun) args ticks
  e          -> return e
 where
  go :: InScopeSet -> Term -> [Either Term Type] -> [TickInfo] -> NormalizeSession Term
  go is0 (collectArgsTicks -> (fun,args0@(_:_),ticks0)) args1 ticks1 =
    go is0 fun (args0 ++ args1) (ticks0 ++ ticks1)

  go is0 (Lam v e) (Left arg:args) ticks = do
    setChanged
    bndrs <- Lens.use bindings
    orM [pure (isVar arg), isWorkFree workFreeBinders bndrs arg] >>= \case
      True ->
        let subst = extendIdSubst (mkSubst is0) v arg in
        (`mkTicks` ticks) <$> go is0 (substTm "appProp.AppLam" subst e) args []
      False ->
        let is1 = extendInScopeSet is0 v in
        Letrec [(v, arg)] <$> go is1 (deShadowTerm is1 e) args ticks

  go is0 (Letrec vs e) args@(_:_) ticks = do
    setChanged
    let vbs  = map fst vs
        is1  = extendInScopeSetList is0 vbs
    -- XXX: 'vs' should already be deshadowed w.r.t. 'is0'
    Letrec vs <$> go is1 e args ticks

  go is0 (TyLam tv e) (Right t:args) ticks = do
    setChanged
    let subst = extendTvSubst (mkSubst is0) tv t
    (`mkTicks` ticks) <$> go is0 (substTm "appProp.TyAppTyLam" subst e) args []

  go is0 (Case scrut ty0 alts) args0@(_:_) ticks = do
    setChanged
    let isA1 = unionInScope
                 is0
                 ((mkInScopeSet . mkVarSet . concatMap (patVars . fst)) alts)
    (ty1,vs,args1) <- goCaseArg isA1 ty0 [] args0
    case vs of
      [] -> (`mkTicks` ticks) . Case scrut ty1 <$> mapM (goAlt is0 args1) alts
      _  -> do
        let vbs   = map fst vs
            is1   = extendInScopeSetList is0 vbs
            alts1 = map (deShadowAlt is1) alts
        Letrec vs . (`mkTicks` ticks) . Case scrut ty1 <$> mapM (goAlt is1 args1) alts1

  go is0 (Tick sp e) args ticks = do
    setChanged
    go is0 e args (sp:ticks)

  go _ fun args ticks = return (mkApps (mkTicks fun ticks) args)

  goAlt is0 args0 (p,e) = do
    let (tvs,ids) = patIds p
        is1       = extendInScopeSetList (extendInScopeSetList is0 tvs) ids
    (p,) <$> go is1 e args0 []

  goCaseArg isA ty0 ls0 (Right t:args0) = do
    tcm <- Lens.view tcCache
    let ty1 = piResultTy tcm ty0 t
    (ty2,ls1,args1) <- goCaseArg isA ty1 ls0 args0
    return (ty2,ls1,Right t:args1)

  goCaseArg isA0 ty0 ls0 (Left arg:args0) = do
    tcm <- Lens.view tcCache
    bndrs <- Lens.use bindings
    let argTy = termType tcm arg
        ty1   = applyFunTy tcm ty0 argTy
    orM [pure (isVar arg), isWorkFree workFreeBinders bndrs arg] >>= \case
      True -> do
        (ty2,ls1,args1) <- goCaseArg isA0 ty1 ls0 args0
        return (ty2,ls1,Left arg:args1)
      False -> do
        boundArg <- mkTmBinderFor isA0 tcm (mkDerivedName ctx "app_arg") arg
        let isA1 = extendInScopeSet isA0 boundArg
        (ty2,ls1,args1) <- goCaseArg isA1 ty1 ls0 args0
        return (ty2,(boundArg,arg):ls1,Left (Var boundArg):args1)

  goCaseArg _ ty ls [] = return (ty,ls,[])
{-# SCC appProp #-}

-- | Specialise functions on arguments which are constant, except when they
-- are clock, reset generators.
constantSpec :: HasCallStack => NormRewrite
constantSpec ctx@(TransformContext is0 tfCtx) e@(App e1 e2)
  | (Var {}, args) <- collectArgs e1
  , (_, []) <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  = do specInfo<- constantSpecInfo ctx e2
       if csrFoundConstant specInfo then
         let newBindings = csrNewBindings specInfo in
         if null newBindings then
           -- Whole of e2 is constant
           specialize ctx (App e1 e2)
         else do
           -- Parts of e2 are constant
           let is1 = extendInScopeSetList is0 (fst <$> csrNewBindings specInfo)
           (body, isSpec) <- Writer.listen $ specialize
             (TransformContext is1 tfCtx)
             (App e1 (csrNewTerm specInfo))

           if Monoid.getAny isSpec
             then changed (Letrec newBindings body)
             else return e
       else
        -- e2 has no constant parts
        return e
constantSpec _ e = return e
{-# SCC constantSpec #-}

-- | Specialise an application on its argument
specialize :: NormRewrite
specialize ctx e = case e of
  (TyApp e1 ty) -> specialize' ctx e (collectArgsTicks e1) (Right ty)
  (App e1 e2)   -> specialize' ctx e (collectArgsTicks e1) (Left  e2)
  _             -> return e

-- | Specialise an application on its argument
specialize'
  :: TransformContext
  -- ^ Transformation context
  -> Term
  -- ^ Original term
  -> (Term, [Either Term Type], [TickInfo])
  -- ^ Function part of the term, split into root and applied arguments
  -> Either Term Type
  -- ^ Argument to specialize on
  -> NormalizeSession Term
specialize' (TransformContext is0 _) e (Var f, args, ticks) specArgIn = do
  opts <- Lens.view debugOpts
  tcm <- Lens.view tcCache

  -- Don't specialise TopEntities
  topEnts <- Lens.view topEntities
  if f `elemVarSet` topEnts
  then do
    case specArgIn of
      Left _ -> do
        traceM ("Not specializing TopEntity: " ++ showPpr (varName f))
        return e
      Right tyArg ->
        traceIf (hasTransformationInfo AppliedTerm opts) ("Dropping type application on TopEntity: " ++ showPpr (varName f) ++ "\ntype:\n" ++ showPpr tyArg) $
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
      specBndrs = map (Lens.over Lens._Left (normalizeId tcm)) specBndrsIn
      specAbs :: Either Term Type
      specAbs = either (Left . (`mkAbstraction` specBndrs)) (Right . id) specArg
  -- Determine if 'f' has already been specialized on (a type-normalized) 'specArg'
  specM <- Map.lookup (f,argLen,specAbs) <$> Lens.use (extra.specialisationCache)
  case specM of
    -- Use previously specialized function
    Just f' ->
      traceIf (hasTransformationInfo AppliedTerm opts)
        ("Using previous specialization of " ++ showPpr (varName f) ++ " on " ++
          (either showPpr showPpr) specAbs ++ ": " ++ showPpr (varName f')) $
        changed $ mkApps (mkTicks (Var f') ticks) (args ++ specVars)
    -- Create new specialized function
    Nothing -> do
      -- Determine if we can specialize f
      bodyMaybe <- fmap (lookupUniqMap (varName f)) $ Lens.use bindings
      case bodyMaybe of
        Just (Binding _ sp inl _ bodyTm) -> do
          -- Determine if we see a sequence of specialisations on a growing argument
          specHistM <- lookupUniqMap f <$> Lens.use (extra.specialisationHistory)
          specLim   <- Lens.use (extra . specialisationLimit)
          if maybe False (> specLim) specHistM
            then throw (ClashException
                        sp
                        (unlines [ "Hit specialization limit " ++ show specLim ++ " on function `" ++ showPpr (varName f) ++ "'.\n"
                                 , "The function `" ++ showPpr f ++ "' is most likely recursive, and looks like it is being indefinitely specialized on a growing argument.\n"
                                 , "Body of `" ++ showPpr f ++ "':\n" ++ showPpr bodyTm ++ "\n"
                                 , "Argument (in position: " ++ show argLen ++ ") that triggered termination:\n" ++ (either showPpr showPpr) specArg
                                 , "Run with '-fclash-spec-limit=N' to increase the specialization limit to N."
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
              (extra.specialisationHistory) %= extendUniqMapWith f 1 (+)
              (extra.specialisationCache)  %= Map.insert (f,argLen,specAbs) newf
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

-- Specialising non Var's is used by nonRepANF
specialize' _ctx _ (appE,args,ticks) (Left specArg) = do
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
             mkFunction (appendToName (varName cf) "_specF") sp NoUserInline newBody
    (b:_) -> return (bindingId b)
  -- Create specialized argument
  let newArg  = Left $ mkApps (Var newf) specVars
  -- Use specialized argument
  let newExpr = mkApps (mkTicks appE ticks) (args ++ [newArg])
  changed newExpr

specialize' _ e _ _ = return e

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

-- | Specialize functions on their non-representable argument
nonRepSpec :: HasCallStack => NormRewrite
nonRepSpec ctx e@(App e1 e2)
  | (Var {}, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  = do tcm <- Lens.view tcCache
       let e2Ty = termType tcm e2
       let localVar = isLocalVar e2
       nonRepE2 <- not <$> (representableType <$> Lens.view typeTranslator
                                              <*> Lens.view customReprs
                                              <*> pure False
                                              <*> Lens.view tcCache
                                              <*> pure e2Ty)
       if nonRepE2 && not localVar
         then do
           e2' <- inlineInternalSpecialisationArgument e2
           specialize ctx (App e1 e2')
         else return e
  where
    -- | If the argument on which we're specialising ia an internal function,
    -- one created by the compiler, then inline that function before we
    -- specialise.
    --
    -- We need to do this because otherwise the specialisation history won't
    -- recognize the new specialisation argument as something the function has
    -- already been specialized on
    inlineInternalSpecialisationArgument
      :: Term
      -> NormalizeSession Term
    inlineInternalSpecialisationArgument app
      | (Var f,fArgs,ticks) <- collectArgsTicks app
      = do
        fTmM <- lookupVarEnv f <$> Lens.use bindings
        case fTmM of
          Just b
            | nameSort (varName (bindingId b)) == Internal
            -> censor (const mempty)
                      (topdownR appProp ctx
                        (mkApps (mkTicks (bindingTerm b) ticks) fArgs))
          _ -> return app
      | otherwise = return app

nonRepSpec _ e = return e
{-# SCC nonRepSpec #-}

-- | Specialize functions on their type
typeSpec :: HasCallStack => NormRewrite
typeSpec ctx e@(TyApp e1 ty)
  | (Var {},  args) <- collectArgs e1
  , null $ Lens.toListOf typeFreeVars ty
  , (_, []) <- Either.partitionEithers args
  = specialize ctx e

typeSpec _ e = return e
{-# SCC typeSpec #-}

-- | Specialize functions on arguments which are zero-width. These arguments
-- can have only one possible value, and specialising on this value may create
-- additional oppourtunities for transformations to fire.
--
-- As we can't remove zero-width arguements (as transformations cannot change
-- the type of a term), we instead substitute all occurances of a lambda-bound
-- variable with a zero-width type with the only value of that type.
--
zeroWidthSpec :: HasCallStack => NormRewrite
zeroWidthSpec (TransformContext is _) e@(Lam i x0) = do
  tcm <- Lens.view tcCache
  let bndrTy = normalizeType tcm (varType i)

  case zeroWidthTypeElem tcm bndrTy of
    Just tm ->
      let subst = extendIdSubst (mkSubst is) i tm
          x1 = substTm "zeroWidthSpec" subst x0
       in changed (Lam i x1)

    Nothing ->
      return e

zeroWidthSpec _ e = return e
{-# SCC zeroWidthSpec #-}

-- Get the only element of a type, if it is zero-width.
--
zeroWidthTypeElem :: TyConMap -> Type -> Maybe Term
zeroWidthTypeElem tcm ty = do
  (tcNm, args) <- splitTyConAppM ty

  if | nameOcc tcNm == Text.showt ''BV.BitVector
     , [LitTy (NumTy 0)] <- args
     -> return (bitVectorZW tcNm args)

     | nameOcc tcNm == Text.showt ''I.Index
     , [LitTy (NumTy 1)] <- args
     -> return (indexZW tcNm args)

     | nameOcc tcNm == Text.showt ''S.Signed
     , [LitTy (NumTy 0)] <- args
     -> return (signedZW tcNm args)

     | nameOcc tcNm == Text.showt ''U.Unsigned
     , [LitTy (NumTy 0)] <- args
     -> return (unsignedZW tcNm args)

     -- Any other zero-width type should only have a single data constructor
     -- where all fields are also zero-width.
     | otherwise
     -> do
       tc <- lookupUniqMap tcNm tcm

       case tyConDataCons tc of
         [dc] -> do
           zwArgs <- traverse (zeroWidthTypeElem tcm) (dcArgTys dc)
           return (mkTmApps (Data dc) zwArgs)

         _ ->
           Nothing
 where
  nNm = mkUnsafeSystemName "n" 0
  nTv = mkTyVar typeNatKind nNm

  mkBitVector tcNm =
    let prTy = mkPolyFunTy (mkTyConApp tcNm [VarTy nTv])
                 [Left nTv, Right naturalPrimTy, Right naturalPrimTy, Right integerPrimTy]
     in PrimInfo (Text.showt 'BV.fromInteger#) prTy WorkNever SingleResult NoUnfolding

  bitVectorZW tcNm tyArgs =
    let pr = mkBitVector tcNm
     in mkApps (Prim pr) $ fmap Right tyArgs <>
          [ Left (Literal (NaturalLiteral 0))
          , Left (Literal (NaturalLiteral 0))
          , Left (Literal (IntegerLiteral 0))
          ]

  mkSizedNum tcNm n =
    let prTy = mkPolyFunTy (mkTyConApp tcNm [VarTy nTv])
                 [Left nTv, Right naturalPrimTy, Right integerPrimTy]
     in PrimInfo n prTy WorkNever SingleResult NoUnfolding

  indexZW tcNm tyArgs =
    let pr = mkSizedNum tcNm (Text.showt 'I.fromInteger#)
     in mkApps (Prim pr) $ fmap Right tyArgs <>
          [ Left (Literal (NaturalLiteral 1))
          , Left (Literal (IntegerLiteral 0))
          ]

  signedZW tcNm tyArgs =
    let pr = mkSizedNum tcNm (Text.showt 'S.fromInteger#)
     in mkApps (Prim pr) $ fmap Right tyArgs <>
          [ Left (Literal (NaturalLiteral 0))
          , Left (Literal (IntegerLiteral 0))
          ]

  unsignedZW tcNm tyArgs =
    let pr = mkSizedNum tcNm (Text.showt 'U.fromInteger#)
     in mkApps (Prim pr) $ fmap Right tyArgs <>
          [ Left (Literal (NaturalLiteral 0))
          , Left (Literal (IntegerLiteral 0))
          ]
