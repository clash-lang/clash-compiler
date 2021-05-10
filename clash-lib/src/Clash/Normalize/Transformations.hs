{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Transformations of the Normalization process
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations
  ( module X
  , typeSpec
  , nonRepSpec
  , etaExpansionTL
  , constantSpec
  , etaExpandSyn
  , appPropFast
  , separateArguments
  , separateLambda
  , xOptimize
  , setupMultiResultPrim
  )
where

import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.Extra         (orM)
import           Control.Monad.Writer        (listen)
import qualified Data.Either                 as Either
import qualified Data.List                   as List
import qualified Data.List.Extra             as List
import qualified Data.Maybe                  as Maybe
import qualified Data.Monoid                 as Monoid
import           Data.Text.Extra             (showt)
import           GHC.Stack                   (HasCallStack)

import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Core.DataCon          (DataCon (..))
import           Clash.Core.Name
  (mkUnsafeInternalName, Name (..), NameSort (..), nameOcc)
import           Clash.Core.FreeVars (termFreeTyVars, typeFreeVars)
import           Clash.Core.Subst
import           Clash.Core.Term
import           Clash.Core.TermInfo
import           Clash.Core.Type             (Type (..), applyFunTy,
                                              splitFunForallTy, splitFunTy,
                                              mkPolyFunTy)
import           Clash.Core.TyCon            (TyConMap)
import           Clash.Core.Util (Projections (..), shouldSplit, mkInternalVar)
import           Clash.Core.Var
  (Id, TyVar, Var (..), isGlobalId, mkLocalId)
import           Clash.Core.VarEnv
  (InScopeSet, elemVarSet, extendInScopeSet, extendInScopeSetList,
   lookupVarEnv, unionInScope, mkVarSet, mkInScopeSet, uniqAway)
import           Clash.Driver.Types          (Binding(..))
import           Clash.Netlist.BlackBox.Types (Element(Err))
import           Clash.Netlist.Types         (BlackBox(..))
import           Clash.Netlist.Util (representableType)
import Clash.Normalize.Transformations.ANF as X
import Clash.Normalize.Transformations.Case as X
import Clash.Normalize.Transformations.Cast as X
import Clash.Normalize.Transformations.DEC as X
import Clash.Normalize.Transformations.Inline as X
import Clash.Normalize.Transformations.Letrec as X
import Clash.Normalize.Transformations.Reduce as X
import           Clash.Normalize.Types
import           Clash.Normalize.Util
import           Clash.Primitives.Types (Primitive(..))
import           Clash.Rewrite.Combinators
import           Clash.Rewrite.Types
import           Clash.Rewrite.Util
import           Clash.Util

-- | Specialize functions on their type
typeSpec :: HasCallStack => NormRewrite
typeSpec ctx e@(TyApp e1 ty)
  | (Var {},  args) <- collectArgs e1
  , null $ Lens.toListOf typeFreeVars ty
  , (_, []) <- Either.partitionEithers args
  = specializeNorm ctx e

typeSpec _ e = return e
{-# SCC typeSpec #-}

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
           specializeNorm ctx (App e1 e2')
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
                      (topdownR appPropFast ctx
                        (mkApps (mkTicks (bindingTerm b) ticks) fArgs))
          _ -> return app
      | otherwise = return app

nonRepSpec _ e = return e
{-# SCC nonRepSpec #-}

-- Misc rewrites

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
           specializeNorm ctx (App e1 e2)
         else do
           -- Parts of e2 are constant
           let is1 = extendInScopeSetList is0 (fst <$> csrNewBindings specInfo)
           Letrec newBindings
            <$> specializeNorm
                  (TransformContext is1 tfCtx)
                  (App e1 (csrNewTerm specInfo))

       else
        -- e2 has no constant parts
        return e
constantSpec _ e = return e
{-# SCC constantSpec #-}


-- Experimental

-- | Propagate arguments of application inwards; except for 'Lam' where the
-- argument becomes let-bound. 'appPropFast' tries to propagate as many arguments
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
appPropFast :: HasCallStack => NormRewrite
appPropFast ctx@(TransformContext is _) = \case
  e@App {}
    | let (fun,args,ticks) = collectArgsTicks e
    -> go is (deShadowTerm is fun) args ticks
  e@TyApp {}
    | let (fun,args,ticks) = collectArgsTicks e
    -> go is (deShadowTerm is fun) args ticks
  e          -> return e
 where
  go :: InScopeSet -> Term -> [Either Term Type] -> [TickInfo]
     -> NormalizeSession Term
  go is0 (collectArgsTicks -> (fun,args0@(_:_),ticks0)) args1 ticks1 =
    go is0 fun (args0 ++ args1) (ticks0 ++ ticks1)

  go is0 (Lam v e) (Left arg:args) ticks = do
    setChanged
    bndrs <- Lens.use bindings
    orM [pure (isVar arg), isWorkFree workFreeBinders bndrs arg] >>= \case
      True ->
        let subst = extendIdSubst (mkSubst is0) v arg in
        (`mkTicks` ticks) <$> go is0 (substTm "appPropFast.AppLam" subst e) args []
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
    (`mkTicks` ticks) <$> go is0 (substTm "appPropFast.TyAppTyLam" subst e) args []

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
{-# SCC appPropFast #-}

-- | Eta-expand top-level lambda's (DON'T use in a traversal!)
etaExpansionTL :: HasCallStack => NormRewrite
etaExpansionTL (TransformContext is0 ctx) (Lam bndr e) = do
  e' <- etaExpansionTL
          (TransformContext (extendInScopeSet is0 bndr) (LamBody bndr:ctx))
          e
  return $ Lam bndr e'

etaExpansionTL (TransformContext is0 ctx) (Letrec xes e) = do
  let bndrs = map fst xes
  e' <- etaExpansionTL
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

etaExpansionTL (TransformContext is0 ctx) e
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
        e' <- etaExpansionTL (TransformContext (extendInScopeSet is0 newId)
                                               (LamBody newId:ctx))
                             (App e (Var newId))
        changed (Lam newId e')
      else return e
{-# SCC etaExpansionTL #-}

-- | Eta-expand functions with a Synthesize annotation, needed to allow such
-- functions to appear as arguments to higher-order primitives.
etaExpandSyn :: HasCallStack => NormRewrite
etaExpandSyn (TransformContext is0 ctx) e@(collectArgs -> (Var f, _)) = do
  topEnts <- Lens.view topEntities
  tcm <- Lens.view tcCache
  let isTopEnt = f `elemVarSet` topEnts
      isAppFunCtx =
        \case
          AppFun:_ -> True
          TickC _:c -> isAppFunCtx c
          _ -> False
      argTyM = fmap fst (splitFunTy tcm (termType tcm e))
  case argTyM of
    Just argTy | isTopEnt && not (isAppFunCtx ctx) -> do
      newId <- mkInternalVar is0 "arg" argTy
      changed (Lam newId (App e (Var newId)))
    _ -> return e

etaExpandSyn _ e = return e
{-# SCC etaExpandSyn #-}


-- | Worker function of 'separateArguments'.
separateLambda
  :: TyConMap
  -> TransformContext
  -> Id
  -- ^ Lambda binder
  -> Term
  -- ^ Lambda body
  -> Maybe Term
  -- ^ If lambda is split up, this function returns a Just containing the new term
separateLambda tcm ctx@(TransformContext is0 _) b eb0 =
  case shouldSplit tcm (varType b) of
    Just (dc, _, argTys) ->
      let
        nm = mkDerivedName ctx (nameOcc (varName b))
        bs0 = map (`mkLocalId` nm) argTys
        (is1, bs1) = List.mapAccumL newBinder is0 bs0
        subst = extendIdSubst (mkSubst is1) b (dc (map Var bs1))
        eb1 = substTm "separateArguments" subst eb0
      in
        Just (mkLams eb1 bs1)
    _ ->
      Nothing
 where
  newBinder isN0 x =
    let
      x' = uniqAway isN0 x
      isN1 = extendInScopeSet isN0 x'
    in
      (isN1, x')
{-# SCC separateLambda #-}

-- | Split apart (global) function arguments that contain types that we
-- want to separate off, e.g. Clocks. Works on both the definition side (i.e. the
-- lambda), and the call site (i.e. the application of the global variable). e.g.
-- turns
--
-- > f :: (Clock System, Reset System) -> Signal System Int
--
-- into
--
-- > f :: Clock System -> Reset System -> Signal System Int
separateArguments :: HasCallStack => NormRewrite
separateArguments ctx e0@(Lam b eb) = do
  tcm <- Lens.view tcCache
  case separateLambda tcm ctx b eb of
    Just e1 -> changed e1
    Nothing -> return e0

separateArguments (TransformContext is0 _) e@(collectArgsTicks -> (Var g, args, ticks))
  | isGlobalId g = do
  -- We ensure that both the type of the global variable reference is updated
  -- to take into account the changed arguments, and that we apply the global
  -- function with the split apart arguments.
  let (argTys0,resTy) = splitFunForallTy (varType g)
  (concat -> args1, Monoid.getAny -> hasChanged)
    <- listen (mapM (uncurry splitArg) (zip argTys0 args))
  if hasChanged then
    let (argTys1,args2) = unzip args1
        gTy = mkPolyFunTy resTy argTys1
    in  return (mkApps (mkTicks (Var g {varType = gTy}) ticks) args2)
  else
    return e

 where
  -- Split a single argument
  splitArg
    :: Either TyVar Type
    -- The quantifier/function argument type of the global variable
    -> Either Term Type
    -- The applied type argument or term argument
    -> NormalizeSession [(Either TyVar Type,Either Term Type)]
  splitArg tv arg@(Right _)    = return [(tv,arg)]
  splitArg ty arg@(Left tmArg) = do
    tcm <- Lens.view tcCache
    let argTy = termType tcm tmArg
    case shouldSplit tcm argTy of
      Just (_,Projections projections,_) -> do
        tmArgs <- projections is0 tmArg
        changed (map ((ty,) . Left) tmArgs)
      _ ->
        return [(ty,arg)]

separateArguments _ e = return e
{-# SCC separateArguments #-}

-- | Remove all undefined alternatives from case expressions, replacing them
-- with the value of another defined alternative. If there is one defined
-- alternative, the entire expression is replaced with that alternative. If
-- there are no defined alternatives, the entire expression is replaced with
-- a call to 'errorX'.
--
-- e.g. It converts
--
--     case x of
--       D1 a -> f a
--       D2   -> undefined
--       D3   -> undefined
--
-- to
--
--     let subj = x
--         a    = case subj of
--                  D1 a -> field0
--      in f a
--
-- where fieldN is an internal variable referring to the nth argument of a
-- data constructor.
--
xOptimize :: HasCallStack => NormRewrite
xOptimize (TransformContext is0 _) e@(Case subj ty alts) = do
  runXOpt <- Lens.view aggressiveXOpt

  if runXOpt then do
    defPart <- List.partitionM (isPrimError . snd) alts

    case defPart of
      ([], _)    -> return e
      (_, [])    -> changed (Prim (PrimInfo "Clash.XException.errorX" ty WorkConstant SingleResult))
      (_, [alt]) -> xOptimizeSingle is0 subj alt
      (_, defs)  -> xOptimizeMany is0 subj ty defs
  else
    return e

xOptimize _ e = return e
{-# SCC xOptimize #-}

-- Return an expression equivalent to the alternative given. When only one
-- alternative is defined the result of this function is used to replace the
-- case expression.
--
xOptimizeSingle :: InScopeSet -> Term -> Alt -> NormalizeSession Term
xOptimizeSingle is subj (DataPat dc tvs vars, expr) = do
  tcm    <- Lens.view tcCache
  subjId <- mkInternalVar is "subj" (termType tcm subj)

  let fieldTys = fmap varType vars
  lets <- Monad.zipWithM (mkFieldSelector is subjId dc tvs fieldTys) vars [0..]

  changed (Letrec ((subjId, subj) : lets) expr)

xOptimizeSingle _ _ (_, expr) = changed expr

-- Given a list of alternatives which are defined, create a new case
-- expression which only ever returns a defined value.
--
xOptimizeMany
  :: HasCallStack
  => InScopeSet
  -> Term
  -> Type
  -> [Alt]
  -> NormalizeSession Term
xOptimizeMany is subj ty defs@(d:ds)
  | isAnyDefault defs = changed (Case subj ty defs)
  | otherwise = do
      newAlt <- xOptimizeSingle is subj d
      changed (Case subj ty $ ds <> [(DefaultPat, newAlt)])
 where
  isAnyDefault :: [Alt] -> Bool
  isAnyDefault = any ((== DefaultPat) . fst)

xOptimizeMany _ _ _ [] =
  error $ $(curLoc) ++ "Report as bug: xOptimizeMany error: No defined alternatives"

mkFieldSelector
  :: MonadUnique m
  => InScopeSet
  -> Id
  -- ^ subject id
  -> DataCon
  -> [TyVar]
  -> [Type]
  -- ^ concrete types of fields
  -> Id
  -> Int
  -> m LetBinding
mkFieldSelector is0 subj dc tvs fieldTys nm index = do
  fields <- mapM (\ty -> mkInternalVar is0 "field" ty) fieldTys
  let alt = (DataPat dc tvs fields, Var $ fields !! index)
  return (nm, Case (Var subj) (fieldTys !! index) [alt])

-- Check whether a term is really a black box primitive representing an error.
-- Such values are undefined and are removed in X Optimization.
--
isPrimError :: Term -> NormalizeSession Bool
isPrimError (collectArgs -> (Prim pInfo, _)) = do
  prim <- Lens.use (extra . primitives . Lens.at (primName pInfo))

  case prim >>= extractPrim of
    Just p  -> return (isErr p)
    Nothing -> return False
 where
  isErr BlackBox{template=(BBTemplate [Err _])} = True
  isErr _ = False

isPrimError _ = return False

-- A multi result primitive assigns its results to multiple result variables
-- instead of one. Besides producing nicer HDL it works around issues with
-- synthesis tooling described in:
--
--   https://github.com/clash-lang/clash-compiler/issues/1555
--
-- This transformation rewrites primitives indicating they can assign their
-- results to multiple signals, such that netlist can easily render it.
--
-- Example:
--
-- @
-- prim :: forall a. a -> (a, a)
-- @
--
-- will be rewritten to:
--
-- @
--   \a0 -> let
--            r  = prim @t0 a0 r0 r1     -- With 'Clash.Core.Term.MultiPrim'
--            r0 = multiPrimSelect r0 r
--            r1 = multiPrimSelect r1 r
--          in
--            (x, y)
-- @
--
-- Netlist will not render any @multiPrimSelect@ primitives. Similar to
-- primitives having a /void/ return type, /r/ is not rendered either.
--
-- This transformation is currently hardcoded to recognize tuples as return
-- types, not any product type. It will error if it sees a multi result primitive
-- with a non-tuple return type.
--
setupMultiResultPrim :: HasCallStack => NormRewrite
setupMultiResultPrim _ctx e@(Prim pInfo@PrimInfo{primMultiResult=SingleResult}) = do
  tcm <- Lens.view tcCache
  prim <- Lens.use (extra . primitives . Lens.at (primName pInfo))

  case prim >>= extractPrim of
    Just (BlackBoxHaskell{multiResult=True}) ->
      changed (setupMultiResultPrim' tcm pInfo)
    Just (BlackBox{multiResult=True}) ->
      changed (setupMultiResultPrim' tcm pInfo)
    _ ->
      return e

setupMultiResultPrim _ e = return e

setupMultiResultPrim' :: HasCallStack => TyConMap -> PrimInfo -> Term
setupMultiResultPrim' tcm primInfo@PrimInfo{primType} =
  mkAbstraction letTerm (map Right typeVars <> map Left argIds)
 where
  typeVars = Either.lefts pArgs

  internalNm prefix n = mkUnsafeInternalName (prefix <> showt n) n
  internalId prefix typ n = mkLocalId typ (internalNm prefix n)

  nTermArgs = length (Either.rights pArgs)
  argIds = zipWith (internalId "a") (Either.rights pArgs) [1..nTermArgs]
  resIds = zipWith (internalId "r") resTypes [nTermArgs+1..nTermArgs+length resTypes]
  resId = mkLocalId pResTy (mkUnsafeInternalName "r" (nTermArgs+length resTypes+1))

  (pArgs, pResTy) = splitFunForallTy primType
  MultiPrimInfo{mpi_resultDc=tupTc, mpi_resultTypes=resTypes} =
    multiPrimInfo' tcm primInfo

  multiPrimSelect r t = (r, mkTmApps (Prim (multiPrimSelectInfo t)) [Var r, Var resId])
  multiPrimSelectBinds = zipWith multiPrimSelect  resIds resTypes
  multiPrimTermArgs = map (Left . Var) (argIds <> resIds)
  multiPrimTypeArgs = map (Right . VarTy) typeVars
  multiPrimBind =
    mkApps
      (Prim primInfo{primMultiResult=MultiResult})
      (multiPrimTypeArgs <> multiPrimTermArgs)

  multiPrimSelectInfo t = PrimInfo
    { primName = "c$multiPrimSelect"
    , primType = mkPolyFunTy pResTy [Right pResTy, Right t]
    , primWorkInfo = WorkAlways
    , primMultiResult = SingleResult }

  letTerm =
    Letrec
      ((resId,multiPrimBind):multiPrimSelectBinds)
      (mkTmApps (mkTyApps (Data tupTc) resTypes) (map Var resIds))


