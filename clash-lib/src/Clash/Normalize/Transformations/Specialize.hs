{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021     , QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for specialisation.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Normalize.Transformations.Specialize
  ( appProp
  , constantSpec
  , nonRepSpec
  , typeSpec
  ) where

import qualified Control.Lens as Lens
import Control.Monad.Extra (orM)
import qualified Control.Monad.Writer as Writer (listen)
import qualified Data.Either as Either
import qualified Data.Monoid as Monoid (getAny)
import GHC.Stack (HasCallStack)

import Clash.Core.FreeVars (termFreeTyVars, typeFreeVars)
import Clash.Core.Name (NameSort(..), nameSort)
import Clash.Core.Subst
  (deShadowAlt, deShadowTerm, extendIdSubst, extendTvSubst, mkSubst, substTm)
import Clash.Core.Term
  ( Term(..), TickInfo, collectArgs, collectArgsTicks, mkApps, mkTicks, patIds
  , patVars)
import Clash.Core.TermInfo (isLocalVar, isVar, piResultTy, termType)
import Clash.Core.Type (Type, applyFunTy)
import Clash.Core.Var (Var(..))
import Clash.Core.VarEnv
  ( InScopeSet, extendInScopeSet, extendInScopeSetList, lookupVarEnv
  , mkInScopeSet, mkVarSet, unionInScope)
import Clash.Driver.Types (Binding(..))
import Clash.Netlist.Util (representableType)
import Clash.Rewrite.Combinators (topdownR)
import Clash.Rewrite.Types
  ( TransformContext(..), bindings, censor, customReprs, tcCache
  , typeTranslator, workFreeBinders)
import Clash.Rewrite.Util (mkDerivedName, mkTmBinderFor, setChanged, changed)
import Clash.Rewrite.WorkFree (isWorkFree)
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Normalize.Util
  ( constantSpecInfo, csrFoundConstant, csrNewBindings, csrNewTerm
  , specializeNorm)

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
           specializeNorm ctx (App e1 e2)
         else do
           -- Parts of e2 are constant
           let is1 = extendInScopeSetList is0 (fst <$> csrNewBindings specInfo)
           (body, isSpec) <- Writer.listen $ specializeNorm
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
  = specializeNorm ctx e

typeSpec _ e = return e
{-# SCC typeSpec #-}
