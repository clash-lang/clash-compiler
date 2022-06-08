{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2022, Google Inc.,
                    2021-2022, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
  Transformations on case-expressions
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations.Case
  ( caseCase
  , caseCon
  , caseElemNonReachable
  , caseFlat
  , caseLet
  , caseOneAlt
  , elimExistentials
  ) where

import qualified Control.Lens as Lens
import Control.Monad.State.Strict (evalState)
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Maybe as Maybe
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Text.Extra as Text (showt)
import GHC.Stack (HasCallStack)

#if MIN_VERSION_base(4,15,0)
import GHC.Num.Integer (Integer(..))
#else
import GHC.Integer.GMP.Internals (BigNat(..), Integer(..))
#endif

import Clash.Sized.Internal.BitVector as BV (BitVector, eq#)
import Clash.Sized.Internal.Index as I (Index, eq#)
import Clash.Sized.Internal.Signed as S (Signed, eq#)
import Clash.Sized.Internal.Unsigned as U (Unsigned, eq#)

import Clash.Core.DataCon (DataCon(..))
import Clash.Core.EqSolver
import Clash.Core.FreeVars (freeLocalIds, localVarsDoNotOccurIn)
import Clash.Core.HasType
import Clash.Core.Literal (Literal(..))
import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Subst
import Clash.Core.Term
  ( Alt, Pat(..), PrimInfo(..), Term(..), collectArgs, collectArgsTicks
  , collectTicks, mkApps, mkTicks, patIds, stripTicks, Bind(..))
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type (LitTy(..), Type(..), TypeView(..), coreView1, tyView)
import Clash.Core.Util (listToLets, mkInternalVar)
import Clash.Core.VarEnv
  ( InScopeSet, elemVarSet, extendInScopeSet, extendInScopeSetList, mkVarSet
  , unitVarSet, uniqAway)
import Clash.Debug (traceIf)
import Clash.Driver.Types (DebugOpts(_dbg_invariants))
import Clash.Netlist.Types (FilteredHWType(..), HWType(..))
import Clash.Netlist.Util (coreTypeToHWType, representableType)
import qualified Clash.Normalize.Primitives as NP (undefined, undefinedX)
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Rewrite.Combinators ((>-!))
import Clash.Rewrite.Types
  ( TransformContext(..), bindings, customReprs, debugOpts, tcCache
  , typeTranslator, workFreeBinders)
import Clash.Rewrite.Util (changed, isFromInt, whnfRW)
import Clash.Rewrite.WorkFree
import Clash.Util (curLoc)

-- | Move a Case-decomposition from the subject of a Case-decomposition to the
-- alternatives
caseCase :: HasCallStack => NormRewrite
caseCase (TransformContext is0 _) e@(Case (stripTicks -> Case scrut alts1Ty alts1) alts2Ty alts2) = do
  ty1Rep <- representableType
    <$> Lens.view typeTranslator
    <*> Lens.view customReprs
    <*> pure False
    <*> Lens.view tcCache
    <*> pure alts1Ty

  -- This is only worth doing if the inner case-expression has a
  -- non-representable alternative type.
  if ty1Rep then return e else
    -- Deshadow to prevent accidental capture of free variables of inner
    -- case. Imagine:
    --
    --   case (case a of {x -> x}) of {_ -> x}
    --
    -- 'x' is introduced the inner 'case' and used (as a free variable) in
    -- the outer one. The goal of 'caseCase' is to rewrite cases such that
    -- their subjects aren't cases. This is achieved by 'pushing' the outer
    -- case to all the alternatives of the inner one. Naively doing so in
    -- this example would cause an accidental capture:
    --
    --   case a of {x -> case x of {_ -> x}}
    --
    -- Suddenly, the 'x' in the alternative of the inner case statement
    -- refers to the one introduced by the outer one, instead of being a
    -- free variable. To prevent this, we deshadow the alternatives of the
    -- original inner case. We now end up with:
    --
    --   case a of {x1 -> case x1 of {_ -> x}}
    --
    let newAlts = fmap (second (\altE -> Case altE alts2Ty alts2))
                      (fmap (deShadowAlt is0) alts1)
     in changed $ Case scrut alts2Ty newAlts

caseCase _ e = return e
{-# SCC caseCase #-}

{-
NOTE: caseOneAlt before caseCon'

When you put a bang on a signal argument:
    f :: Signal d a -> _
    f !x = ...
GHC generates a case like:
    case x of
      _ :- _ -> ...

When this f is inlined in an:
    g = f (pure False)
And clash does its Signal d a ~ a thing we get:
    g = case False of
      _ :- _ -> ...
Because no pattern matches caseCon transforms this into
    g = undefined

By trying caseOneAlt first clash can instead drop the case
and use the body of the single alternative.
-}
caseCon :: HasCallStack => NormRewrite
caseCon = const caseOneAlt >-! caseCon'

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
caseCon' :: HasCallStack => NormRewrite
caseCon' ctx@(TransformContext is0 _) e@(Case subj ty alts) = do
 tcm <- Lens.view tcCache
 case collectArgsTicks subj of
  -- The subject is an applied data constructor
  (Data dc, args, ticks) -> case List.find (equalCon . fst) alts of
    Just (DataPat _ tvs xs, altE) -> do
     let
      -- Create the substitution environment for all the existential
      -- type variables.
      exTysList = List.zipEqual tvs (drop (length (dcUnivTyVars dc)) (Either.rights args))
      exTySubst = extendTvSubstList (mkSubst is0) exTysList
      -- Apply the type-substitution in all the pattern variables, we need
      -- to do this because we might use them as let-bindings later on,
      -- and they should have the correct type.
      xs1 = fmap (substTyInVar exTySubst) xs
      -- Create an initial set of let-binders for all variables used in the
      -- RHS of the alternative. We might later decide to substitute instead
      -- of let-bind in case the RHS of the let-binder is work-free.
      fvs = Lens.foldMapOf freeLocalIds unitVarSet altE
      (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                $ List.zipEqual xs1 (Either.lefts args)
      binds1 = fmap (second (`mkTicks` ticks)) binds
     altE1 <-
       case binds1 of
        [] ->
          -- Apply the type-substitution for the existential type variables
          pure (substTm "caseCon'" exTySubst altE)
        _  -> do
          -- See Note [CaseCon deshadow]
          let
            -- Only let-bind expression that perform work.
            is1 = extendInScopeSetList (extendInScopeSetList is0 tvs) xs1
          ((is3,substIds),binds2) <- List.mapAccumLM newBinder (is1,[]) binds1
          let
            -- Create a substitution for all the existential type variables
            -- and the work-free expressions
            subst = mkSubst is3
                      `extendTvSubstList` exTysList
                      `extendIdSubstList` substIds
            body  = substTm "caseCon'" subst altE
          case Maybe.catMaybes binds2 of
            []     -> pure body
            -- Use listToLets to create a series of non-recursive lets instead
            -- of a recursive group. We know these binders will not form a group.
            binds3 -> pure (listToLets binds3 body)
     changed altE1
    _ -> case alts of
           -- In Core, default patterns always come first, so we match against
           -- that if there is one, and we couldn't match with any of the data
           -- patterns.
           ((DefaultPat,altE):_) -> changed altE
           _ -> changed (TyApp (Prim NP.undefined) ty)
    where
      -- Check whether the pattern matches the data constructor
      equalCon (DataPat dcPat _ _) = dcTag dc == dcTag dcPat
      equalCon _ = False

      -- Decide whether the applied arguments of the data constructor should
      -- be let-bound, or substituted into the alternative. We decide this
      -- based on the fact on whether the argument has the potential to make
      -- the circuit larger than needed if we were to duplicate that argument.
      newBinder (isN0, substN) (x, arg) = do
        bndrs <- Lens.use bindings
        isWorkFree workFreeBinders bndrs arg >>= \case
          True -> pure ((isN0, (x, arg):substN), Nothing)
          False ->
            let
              x' = uniqAway isN0 x
              isN1 = extendInScopeSet isN0 x'
            in
              pure ((isN1, (x, Var x'):substN), Just (x', arg))

  -- The subject is a literal
  (Literal l,_,_) -> case List.find (equalLit . fst) alts of
    Just (LitPat _,altE) -> changed altE
    _ -> matchLiteralContructor e l alts
    where
      equalLit (LitPat l') = l == l'
      equalLit _ = False

  -- The subject is an applied primitive
  (Prim _,_,_) ->
    -- We try to reduce the applied primitive to WHNF
    whnfRW True ctx subj $ \ctx1 subj1 -> case collectArgsTicks subj1 of
      -- WHNF of subject is a literal, try `caseCon` with that
      (Literal l,_,_) -> caseCon ctx1 (Case (Literal l) ty alts)
      -- WHNF of subject is a data-constructor, try `caseCon` with that
      (Data _,_,_) -> caseCon ctx1 (Case subj1 ty alts)
      -- WHNF of subject is _|_, in the form of `error`: that means that the
      -- entire case-expression is evaluates to _|_
      (Prim pInfo,repTy:_:callStack:msg:_,ticks)
        | primName pInfo == "GHC.Err.error" ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks)
                        [repTy,Right ty,callStack,msg]
         in changed e1
      -- WHNF of subject is _|_, in the form of `absentError`: that means that
      -- the entire case-expression is evaluates to _|_
      (Prim pInfo,_:msgOrCallStack:_,ticks)
        | primName pInfo == "Control.Exception.Base.absentError" ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks)
                        [Right ty,msgOrCallStack]
        in  changed e1
      -- WHNF of subject is _|_, in the form of  `patError`, `undefined`, or
      -- `errorWithoutStackTrace`: that means the entire case-expression is _|_
      (Prim pInfo,repTy:_:msgOrCallStack:_,ticks)
        | primName pInfo `elem` ["Control.Exception.Base.patError"
                                ,"GHC.Err.undefined"
                                ,"GHC.Err.errorWithoutStackTrace"] ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks)
                        [repTy,Right ty,msgOrCallStack]
        in  changed e1
      -- WHNF of subject is _|_, in the form of our internal _|_-values: that
      -- means the entire case-expression is _|_
      (Prim pInfo,[_],ticks)
        | primName pInfo `elem` [ Text.showt 'NP.undefined
                                , Text.showt 'NP.undefinedX ] ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks) [Right ty]
        in changed e1
      -- WHNF of subject is _|_, in the form of `errorX`: that means that
      -- the entire case-expression is evaluates to _|_
      (Prim pInfo,_:callStack:msg:_,ticks)
        | primName pInfo == "Clash.XException.errorX"
        -> let e1 = mkApps (mkTicks (Prim pInfo) ticks) [Right ty,callStack,msg]
            in changed e1
      -- WHNF of subject is non of the above, so either a variable reference,
      -- or a primitive for which the evaluator doesn't have any evaluation
      -- rules.
      _ -> do
        let subjTy = inferCoreTypeOf tcm subj
        tran <- Lens.view typeTranslator
        reprs <- Lens.view customReprs
        case (`evalState` mempty) (coreTypeToHWType tran reprs tcm subjTy) of
          Right (FilteredHWType (Void (Just hty)) _areVoids)
            | hty `elem` [BitVector 0, Unsigned 0, Signed 0, Index 1]
            -- If we know that the type of the subject is zero-bits wide and
            -- one of the Clash number types. Then the only valid alternative is
            -- the one that can match on the literal "0", so try 'caseCon' with
            -- that.
            -> caseCon ctx1 (Case (Literal (IntegerLiteral 0)) ty alts)
          _ -> do
            opts <- Lens.view debugOpts
            -- When invariants are being checked, report missing evaluation
            -- rules for the primitive evaluator.
            traceIf (_dbg_invariants opts && isConstant subj)
              ("Unmatchable constant as case subject: " ++ showPpr subj ++
                 "\nWHNF is: " ++ showPpr subj1)
              -- Otherwise check whether the entire case-expression has a
              -- single alternative, and pick that one.
              (caseOneAlt e)

  -- The subject is a variable
  (Var v, [], _) | isNum0 (coreTypeOf v) ->
    -- If we know that the type of the subject is zero-bits wide and
    -- one of the Clash number types. Then the only valid alternative is
    -- the one that can match on the literal "0", so try 'caseCon' with
    -- that.
    caseCon ctx (Case (Literal (IntegerLiteral 0)) ty alts)
   where
    isNum0 (tyView -> TyConApp (nameOcc -> tcNm) [arg])
      | tcNm `elem`
        [ Text.showt ''BitVector
        , Text.showt ''Signed
        , Text.showt ''Unsigned
        ]
      = isLitX 0 arg
      | tcNm == Text.showt ''Index
      = isLitX 1 arg
    isNum0 (coreView1 tcm -> Just t) = isNum0 t
    isNum0 _ = False

    isLitX n (LitTy (NumTy m)) = n == m
    isLitX n (coreView1 tcm -> Just t) = isLitX n t
    isLitX _ _ = False

  -- Otherwise check whether the entire case-expression has a single
  -- alternative, and pick that one.
  _ -> caseOneAlt e

caseCon' _ e = return e
{-# SCC caseCon' #-}

{- [Note: Name re-creation]
The names of heap bound variables are safely generate with mkUniqSystemId in Clash.Core.Evaluator.newLetBinding.
But only their uniqs end up in the heap, not the complete names.
So we use mkUnsafeSystemName to recreate the same Name.
-}

matchLiteralContructor
  :: Term
  -> Literal
  -> [Alt]
  -> NormalizeSession Term
matchLiteralContructor c (IntegerLiteral l) alts = go (reverse alts)
 where
  go [(DefaultPat,e)] = changed e
  go ((DataPat dc [] [x],e):alts')
    | dcTag dc == 1
    , l >= ((-2)^(63::Int)) &&  l < 2^(63::Int)
    = let fvs = Lens.foldMapOf freeLocalIds unitVarSet e
          bind = NonRec x (Literal (IntLiteral l))
       in if x `elemVarSet` fvs
            then changed (Let bind e)
            else changed e
    | dcTag dc == 2
    , l >= 2^(63::Int)
#if MIN_VERSION_base(4,15,0)
    = let !(IP ba) = l
#else
    = let !(Jp# !(BN# ba)) = l
#endif
          ba'       = BA.ByteArray ba
          fvs       = Lens.foldMapOf freeLocalIds unitVarSet e
          bind      = NonRec x (Literal (ByteArrayLiteral ba'))
       in if x `elemVarSet` fvs
            then changed (Let bind e)
            else changed e
    | dcTag dc == 3
    , l < ((-2)^(63::Int))
#if MIN_VERSION_base(4,15,0)
    = let !(IN ba) = l
#else
    = let !(Jn# !(BN# ba)) = l
#endif
          ba'       = BA.ByteArray ba
          fvs       = Lens.foldMapOf freeLocalIds unitVarSet e
          bind      = NonRec x (Literal (ByteArrayLiteral ba'))
       in if x `elemVarSet` fvs
            then changed (Let bind e)
            else changed e
    | otherwise
    = go alts'
  go ((LitPat l', e):alts')
    | IntegerLiteral l == l'
    = changed e
    | otherwise
    = go alts'
  go _ = error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c

matchLiteralContructor c (NaturalLiteral l) alts = go (reverse alts)
 where
  go [(DefaultPat,e)] = changed e
  go ((DataPat dc [] [x],e):alts')
    | dcTag dc == 1
    , l >= 0 && l < 2^(64::Int)
    = let fvs       = Lens.foldMapOf freeLocalIds unitVarSet e
          bind      = NonRec x (Literal (WordLiteral l))
       in if x `elemVarSet` fvs
            then changed (Let bind e)
            else changed e
    | dcTag dc == 2
    , l >= 2^(64::Int)
#if MIN_VERSION_base(4,15,0)
    = let !(IP ba) = l
#else
    = let !(Jp# !(BN# ba)) = l
#endif
          ba'       = BA.ByteArray ba
          fvs       = Lens.foldMapOf freeLocalIds unitVarSet e
          bind      = NonRec x (Literal (ByteArrayLiteral ba'))
       in if x `elemVarSet` fvs
            then changed (Let bind e)
            else changed e
    | otherwise
    = go alts'
  go ((LitPat l', e):alts')
    | NaturalLiteral l == l'
    = changed e
    | otherwise
    = go alts'
  go _ = error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c

matchLiteralContructor _ _ ((DefaultPat,e):_) = changed e
matchLiteralContructor c _ _ =
  error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c
{-# SCC matchLiteralContructor #-}

-- | Remove non-reachable alternatives. For example, consider:
--
--    data STy ty where
--      SInt :: Int -> STy Int
--      SBool :: Bool -> STy Bool
--
--    f :: STy ty -> ty
--    f (SInt b) = b + 1
--    f (SBool True) = False
--    f (SBool False) = True
--    {-# NOINLINE f #-}
--
--    g :: STy Int -> Int
--    g = f
--
-- @f@ is always specialized on @STy Int@. The SBool alternatives are therefore
-- unreachable. Additional information can be found at:
-- https://github.com/clash-lang/clash-compiler/pull/465
caseElemNonReachable :: HasCallStack => NormRewrite
caseElemNonReachable _ case0@(Case scrut altsTy alts0) = do
  tcm <- Lens.view tcCache

  let (altsAbsurd, altsOther) = List.partition (isAbsurdPat tcm . fst) alts0
  case altsAbsurd of
    [] -> return case0
    _  -> changed =<< caseOneAlt (Case scrut altsTy altsOther)

caseElemNonReachable _ e = return e
{-# SCC caseElemNonReachable #-}

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
caseFlat :: HasCallStack => NormRewrite
caseFlat (TransformContext is0 _) e@(Case (collectEqArgs -> Just (scrut',val)) ty _) =
  case collectFlat scrut' e of
    Just alts' -> case collectArgs val of
      -- When we're pattern matching on `Int`, extract the `Int#` first before
      -- we do the Literal matching branches.
      (Data dc,_)
        | nameOcc (dcName dc) == "GHC.Types.I#"
        , [argTy] <- dcArgTys dc
        -> do
          wild <- mkInternalVar is0 "wild" argTy
          changed (Case scrut' ty
                    [(DataPat dc [] [wild]
                     ,Case (Var wild) ty (last alts' : init alts'))])
      _ -> changed (Case scrut' ty (last alts' : init alts'))
    Nothing -> return e

caseFlat _ e = return e
{-# SCC caseFlat #-}

collectFlat :: Term -> Term -> Maybe [Alt]
collectFlat scrut (Case (collectEqArgs -> Just (scrut', val)) _ty [lAlt,rAlt])
  | scrut' == scrut
  = case collectArgs val of
      (Prim p,args') | isFromInt (primName p) ->
        go (last args')
      (Data dc,args') | nameOcc (dcName dc) == "GHC.Types.I#" ->
        go (last args')
      _ -> Nothing
  where
    go (Left (Literal i)) = case (lAlt,rAlt) of
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
    go _ = Nothing

    isFalseDcPat (DataPat p _ _)
      = ((== "GHC.Types.False") . nameOcc . dcName) p
    isFalseDcPat _ = False

    isTrueDcPat (DataPat p _ _)
      = ((== "GHC.Types.True") . nameOcc . dcName) p
    isTrueDcPat _ = False

collectFlat _ _ = Nothing
{-# SCC collectFlat #-}

collectEqArgs :: Term -> Maybe (Term,Term)
collectEqArgs (collectArgsTicks -> (Prim p, args, ticks))
  | nm == Text.showt 'BV.eq#
    = let [_,_,Left scrut,Left val] = args
      in Just (mkTicks scrut ticks,val)
  | nm == Text.showt 'I.eq#  ||
    nm == Text.showt 'S.eq# ||
    nm == Text.showt 'U.eq#
    = let [_,Left scrut,Left val] = args
      in Just (mkTicks scrut ticks,val)
  | nm == "GHC.Classes.eqInt"
    = let [Left scrut,Left val] = args
      in  Just (mkTicks scrut ticks,val)
 where
  nm = primName p

collectEqArgs _ = Nothing

-- | Lift the let-bindings out of the subject of a Case-decomposition
caseLet :: HasCallStack => NormRewrite
caseLet (TransformContext is0 _) (Case (collectTicks -> (Let xes e,ticks)) ty alts) = do
  -- Note [CaseLet deshadow]
  -- Imagine
  --
  -- @
  -- case (let x = u in e) of {p -> a}
  -- @
  --
  -- where `a` has a free variable named `x`.
  --
  -- Simply transforming the above to:
  --
  -- @
  -- let x = u in case e of {p -> a}
  -- @
  --
  -- would be very bad, because now the let-binding captures the free x variable
  -- in a.
  --
  -- We must therefor rename `x` so that it doesn't capture the free variables
  -- in the alternative:
  --
  -- @
  -- let x1 = u[x:=x1] in case e[x:=x1] of {p -> a}
  -- @
  --
  -- It is safe to over-approximate the free variables in `a` by simply taking
  -- the current InScopeSet.
  let (xes1,e1) = deshadowLetExpr is0 xes e
  changed (Let (fmap (`mkTicks` ticks) xes1)
                  (Case (mkTicks e1 ticks) ty alts))

caseLet _ e = return e
{-# SCC caseLet #-}

caseOneAlt :: Term -> NormalizeSession Term
caseOneAlt e@(Case _ _ [(pat,altE)]) =
  case pat of
    DefaultPat -> changed altE
    LitPat _ -> changed altE
    DataPat _ tvs xs
      | (coerce tvs ++ coerce xs) `localVarsDoNotOccurIn` altE
      -> changed altE
      | otherwise
      -> return e

caseOneAlt (Case _ _ alts@((pat,alt):_:_))
  | all ((== alt) . snd) (tail alts)
  , (tvs,xs) <- patIds pat
  , (coerce tvs ++ coerce xs) `localVarsDoNotOccurIn` alt
  = changed alt

caseOneAlt e = return e
{-# SCC caseOneAlt #-}

-- | Tries to eliminate existentials by using heuristics to determine what the
-- existential should be. For example, consider Vec:
--
--    data Vec :: Nat -> Type -> Type where
--      Nil       :: Vec 0 a
--      Cons x xs :: a -> Vec n a -> Vec (n + 1) a
--
-- Thus, 'null' (annotated with existentials) could look like:
--
--    null :: forall n . Vec n Bool -> Bool
--    null v =
--      case v of
--        Nil  {n ~ 0}                                     -> True
--        Cons {n1:Nat} {n~n1+1} (x :: a) (xs :: Vec n1 a) -> False
--
-- When it's applied to a vector of length 5, this becomes:
--
--    null :: Vec 5 Bool -> Bool
--    null v =
--      case v of
--        Nil  {5 ~ 0}                                     -> True
--        Cons {n1:Nat} {5~n1+1} (x :: a) (xs :: Vec n1 a) -> False
--
-- This function solves 'n1' and replaces every occurrence with its solution. A
-- very limited number of solutions are currently recognized: only adds (such
-- as in the example) will be solved.
elimExistentials :: HasCallStack => NormRewrite
elimExistentials (TransformContext is0 _) (Case scrut altsTy alts0) = do
  tcm <- Lens.view tcCache
  alts1 <- traverse (go is0 tcm) alts0
  caseOneAlt (Case scrut altsTy alts1)
 where
    -- Eliminate free type variables if possible
    go :: InScopeSet -> TyConMap -> Alt -> NormalizeSession Alt
    go is2 tcm alt@(pat@(DataPat dc exts0 xs0), term0) =
      case solveNonAbsurds tcm (mkVarSet exts0) (patEqs tcm pat) of
        -- No equations solved:
        [] -> return alt
        -- One or more equations solved:
        sols ->
          changed =<< go is2 tcm (DataPat dc exts1 xs1, term1)
          where
            -- Substitute solution in existentials and applied types
            is3 = extendInScopeSetList is2 exts0
            xs1 = fmap (substTyInVar (extendTvSubstList (mkSubst is3) sols)) xs0
            exts1 = substInExistentialsList is2 exts0 sols

            -- Substitute solution in term.
            is4 = extendInScopeSetList is3 xs1
            subst = extendTvSubstList (mkSubst is4) sols
            term1 = substTm "Replacing tyVar due to solved eq" subst term0

    go _ _ alt = return alt

elimExistentials _ e = return e
{-# SCC elimExistentials #-}
