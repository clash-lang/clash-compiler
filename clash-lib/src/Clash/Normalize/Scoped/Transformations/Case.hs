{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2022, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on case-expressions of scoped Clash Core: the ported
  counterparts of "Clash.Normalize.Transformations.Case".

  The capture bugs that the current transformations guard against with
  deshadowing ('caseCase', 'caseLet') or with fresh let-binders ('caseCon')
  are type errors here: moving the alternatives of a case-expression under the
  binders of a pattern or a let-expression needs the scope those extend, which
  only opening them gives ('Clash.Core.Scoped.Subst.openAlt',
  'Clash.Core.Scoped.Subst.openBind'). Returning the right-hand side of an
  alternative without its pattern needs that the variables the pattern binds
  don't occur in it ('Clash.Core.Scoped.Subst.unsinkAlt').

  Differences with the current transformations:

  * 'caseCon' evaluates the subject with the evaluator of scoped Clash Core
    ('Clash.Rewrite.Scoped.Whnf.whnfRW'), which uses the let-bindings of the
    context that the current one drops because they are shadowed. Its
    primitive reductions build coercions as @_CO_ \@ty@
    ('Clash.Core.Scoped.Util.primCo').

  * When 'caseCon' let-binds the fields of a data constructor, it binds them
    in the order of the fields. The current one binds them in the order that
    'Clash.Core.Util.listToLets' gives independent bindings, which depends on
    the uniques of their binders (by descending unique).
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Normalize.Scoped.Transformations.Case
  ( caseCase
  , caseCaseWorker
  , caseCon
  , caseConWorker
  , caseEliminateNonReachable
  , caseEliminateNonReachableWorker
  , caseFlat
  , caseFlatWorker
  , caseLet
  , caseLetWorker
  , caseOneAlt
  , elimExistentials
  , elimExistentialsWorker
  , elimCaseBigNumInternals
  , elimCaseBigNumInternalsWorker
  ) where

import Control.Exception.Base (patError)
import Control.Monad (when)
import GHC.Prim.Panic (absentError)
import qualified Control.Lens as Lens
import qualified Control.Monad.Writer as Writer
import qualified Data.Monoid as Monoid
import Data.Either (rights)
import qualified Data.List as List
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Text.Extra as Text (showt)
import qualified GHC.Classes
import GHC.Exts (Int(..))
import GHC.Stack (HasCallStack)

import GHC.Num.Integer (Integer(..))

import Clash.Sized.Internal.BitVector as BV (BitVector, eq#)
import Clash.Sized.Internal.Index as I (Index, eq#)
import Clash.Sized.Internal.Signed as S (Signed, eq#)
import Clash.Sized.Internal.Unsigned as U (Unsigned, eq#)

import Clash.Core.DataCon (DataCon(..), dcUnivTyVars)
import Clash.Core.Literal (Literal(..))
import qualified Clash.Core.Name as C (nameOcc)
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Type as C (LitTy(..))
import Clash.Core.TysPrim (integerIsDc, naturalNsDc)
import qualified Clash.Core.Var as C (varName)
import Clash.Driver.Types (DebugOpts(dbg_invariants))
import Clash.Driver.Warning (warnAboutM)
import Clash.Netlist.Types (FilteredHWType(..), HWType(..))
import Clash.Netlist.Util (coreTypeToHWType)
import qualified Clash.Normalize.Primitives as NP (undefined, undefinedX)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL (Transformation, onCase, toScopedTransformation)
import Clash.Rewrite.Types
  ( curFun, customReprs, debugOpts, scopedTypeEnv, typeTranslator)
import Clash.Util (curLoc)
import Clash.Warning (ClashWarning(WarnUnmatchableConstant))

import Clash.XException (errorX)

import Clash.Core.Scoped.Alpha (aeqTm, commonAltBody)
import Clash.Core.Scoped.DataCon (dataConInstArgTys)
import Clash.Core.Scoped.EqSolver (isAbsurdPat, solvePatExistentials)
import Clash.Core.Scoped.FreeVars (freeTmVars)
import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.NameMap (NameSet, memberNameSet)
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Convert (mkPrimInfoFromClash)
import Clash.Core.Scoped.Term
  (collectArgs, collectArgsTicks, collectTicks, mkApps, mkTicks, stripTicks)
import Clash.Core.Scoped.Type (TypeView(..), coreView1, tyView)
import Clash.Core.Scoped.TypeEnv (TypeEnv)
import Clash.Core.Scoped.Util (internalName)
import Clash.Rewrite.Scoped (TransformContext, changed, tfScope, whnfRW)
import Clash.Rewrite.Scoped.Globals (isWorkFreeTerm)
import Clash.Rewrite.Scoped.Util
  (isFromInt, isUntranslatableType, runWithHWTypeCache)
import Clash.Rewrite.Scoped.WorkFree (isConstant)

-- | Move a Case-decomposition from the subject of a Case-decomposition to the
-- alternatives
caseCase :: Transformation
caseCase = toScopedTransformation "caseCase" (onCase 'caseCaseWorker)

-- | The 'Case' handler of 'caseCase'.
--
-- The alternatives of the outer case-expression are moved under the patterns
-- of the inner one, which needs the scopes that opening those patterns gives:
-- a pattern that binds a variable that occurs free in the outer alternatives
-- is renamed. See 'Clash.Normalize.Transformations.Case.caseCaseWorker'.
caseCaseWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
caseCaseWorker ctx e (stripTicks -> Case scrut alts1Ty alts1) alts2Ty alts2 = do
  ty1Rep <- not <$> isUntranslatableType False (tfScope ctx) alts1Ty

  -- This is only worth doing if the inner case-expression has a
  -- non-representable alternative type.
  if ty1Rep then return e else
    let newAlt alt = openAlt (tfScope ctx) alt $ \_ p altE ->
          Alt p (Case altE (sink alts2Ty) (sink1 alts2))
     in changed (Case scrut alts2Ty (map newAlt alts1))

caseCaseWorker _ e _ _ _ = return e
{-# SCC caseCaseWorker #-}

-- | Specialize a Case-decomposition (replace by the RHS of an alternative) if
-- the subject is (an application of) a DataCon; or if there is only a single
-- alternative that doesn't reference variables bound by the pattern. See
-- 'Clash.Normalize.Transformations.Case.caseCon'.
--
-- Tries 'caseOneAlt' first, see NOTE: caseOneAlt before caseCon' in
-- "Clash.Normalize.Transformations.Case".
caseCon :: Transformation
caseCon = toScopedTransformation "caseCon" (onCase 'caseConWorker)

-- | The 'Case' handler of 'caseCon': tries 'caseOneAlt' first and only when
-- that did not fire, 'caseCon''.
caseConWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
caseConWorker ctx node subj ty alts = do
  (node1, oneAltChanged) <- Writer.listen (caseOneAlt node)
  if Monoid.getAny oneAltChanged
    then return node1
    else caseCon' ctx node subj ty alts

-- | Apply 'caseCon' to a freshly constructed case-expression; used by the
-- recursive calls inside 'caseCon''.
caseConOn
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Type n -> [Alt n] -> NormalizeSession (Term n)
caseConOn ctx subj ty alts = caseConWorker ctx (Case subj ty alts) subj ty alts

-- | See 'Clash.Normalize.Transformations.Case.caseCon''.
--
-- When the subject is an applied data constructor, the fields that the
-- matching alternative uses are substituted for the variables of its pattern
-- if they are work free, and let-bound otherwise. The let-binders get fresh
-- names, so they can't capture the free variables of the fields (see
-- Note [CaseCon deshadow] in "Clash.Normalize.Transformations.Case").
caseCon'
  :: forall n. (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
caseCon' ctx e subj ty alts = do
 te <- Lens.view scopedTypeEnv
 case collectArgsTicks subj of
  -- The subject is an applied data constructor
  (Data dc, args, ticks) -> case findAlt equalCon alts of
    Just alt -> openAlt sc alt $ \_ pat altE -> case pat of
      DataPat _ tvs xs -> do
        let
          -- Substitute the existential type variables
          exTys = drop (length (dcUnivTyVars dc)) (rights args)
          s0 = substExistentials (idSubst sc) tvs exTys
          -- Only the fields used in the alternative need to be substituted
          -- or let-bound
          fvs = freeTmVars altE
          used = map (isUsed fvs) (snd (patVars pat))
          fieldArgs = [mkTicks a ticks | Left a <- args]
        bindFields sc s0 xs (zipFields used fieldArgs) $ \_ s1 wrapLets ->
          changed (wrapLets (substTm s1 altE))
      _ -> noMatchingAlt
    Nothing -> noMatchingAlt
   where
    -- Check whether the pattern matches the data constructor
    equalCon :: Pat n l -> Bool
    equalCon (DataPat dcPat _ _) = dcTag dc == dcTag dcPat
    equalCon _ = False

    noMatchingAlt = case alts of
      -- In Core, default patterns always come first, so we match against
      -- that if there is one, and we couldn't match with any of the data
      -- patterns.
      (Alt DefaultPat altE : _) -> changed altE
      _ -> changed (TyApp (Prim (mkPrimInfoFromClash NP.undefined)) ty)

  -- The subject is a literal
  (Literal l, _, _) -> case findAlt equalLit alts of
    Just (Alt (LitPat _) altE) -> changed altE
    _ -> matchLiteralConstructor e l alts
   where
    equalLit :: Pat n l -> Bool
    equalLit (LitPat l') = l == l'
    equalLit _ = False

  -- The subject is an applied primitive
  (Prim _, _, _) ->
    -- We try to reduce the applied primitive to WHNF
    whnfRW True ctx subj $ \ctx1 subj1 -> case collectArgsTicks subj1 of
      -- WHNF of subject is a literal, try `caseCon` with that
      (Literal l, _, _) -> caseConOn ctx1 (Literal l) (sink ty) (sink1 alts)
      -- WHNF of subject is a data-constructor, try `caseCon` with that
      (Data _, _, _) -> caseConOn ctx1 subj1 (sink ty) (sink1 alts)
      -- WHNF of subject is _|_, in the form of `error`: that means that the
      -- entire case-expression is evaluates to _|_
      (Prim pInfo, repTy:_:callStack:msg:_, ticks1)
        | primName pInfo == Text.showt 'error ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks1)
                        [repTy, Right (sink ty), callStack, msg]
         in changed e1
      -- WHNF of subject is _|_, in the form of `absentError`: that means that
      -- the entire case-expression is evaluates to _|_
      (Prim pInfo, _:msgOrCallStack:_, ticks1)
        | primName pInfo == Text.showt 'absentError ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks1)
                        [Right (sink ty), msgOrCallStack]
         in changed e1
      -- WHNF of subject is _|_, in the form of  `patError`, `undefined`, or
      -- `errorWithoutStackTrace`: that means the entire case-expression is _|_
      (Prim pInfo, repTy:_:msgOrCallStack:_, ticks1)
        | primName pInfo `elem` [ Text.showt 'patError
                                , Text.showt 'undefined
                                , Text.showt 'errorWithoutStackTrace] ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks1)
                        [repTy, Right (sink ty), msgOrCallStack]
         in changed e1
      -- WHNF of subject is _|_, in the form of our internal _|_-values: that
      -- means the entire case-expression is _|_
      (Prim pInfo, [_], ticks1)
        | primName pInfo `elem` [ Text.showt 'NP.undefined
                                , Text.showt 'NP.undefinedX ] ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks1) [Right (sink ty)]
         in changed e1
      -- WHNF of subject is _|_, in the form of `errorX`: that means that
      -- the entire case-expression is evaluates to _|_
      (Prim pInfo, _:callStack:msg:_, ticks1)
        | primName pInfo == Text.showt 'errorX ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks1) [Right (sink ty), callStack, msg]
         in changed e1
      -- WHNF of subject is non of the above, so either a variable reference,
      -- or a primitive for which the evaluator doesn't have any evaluation
      -- rules.
      _ -> do
        let subjTy = inferCoreTypeOf te sc subj
        tran <- Lens.view typeTranslator
        reprs <- Lens.view customReprs
        subjHwty <- runWithHWTypeCache (coreTypeToHWType tran reprs te sc subjTy)
        case subjHwty of
          Right (FilteredHWType (Void (Just hty)) _areVoids)
            | hty `elem` [BitVector 0, Unsigned 0, Signed 0, Index 1]
            -- If we know that the type of the subject is zero-bits wide and
            -- one of the Clash number types. Then the only valid alternative is
            -- the one that can match on the literal "0", so try 'caseCon' with
            -- that.
            -> caseConOn ctx1 (Literal (IntegerLiteral 0)) (sink ty) (sink1 alts)
          _ -> do
            opts <- Lens.view debugOpts
            (curFunId, sp) <- Lens.use curFun
            -- When invariants are being checked, report missing evaluation
            -- rules for the primitive evaluator.
            when (dbg_invariants opts && isConstant subj)
              $ warnAboutM WarnUnmatchableConstant sp
              $ "Unmatchable constant as case subject in " ++
                  showPpr (C.varName curFunId) ++ ": " ++ showPpr subj ++
                  "\nWHNF is: " ++ showPpr subj1
            -- Otherwise check whether the entire case-expression has a
            -- single alternative, and pick that one.
            caseOneAlt (sink e)

  -- The subject is a variable of a zero-width number type: the only valid
  -- alternative is the one that can match on the literal "0", so try
  -- 'caseCon' with that.
  (Var _ vTy _, [], _) | isNum0 te vTy ->
    caseConOn ctx (Literal (IntegerLiteral 0)) ty alts
  (GlobalVar g, [], _) | isNum0 te (sinkClosed (globalIdType g)) ->
    caseConOn ctx (Literal (IntegerLiteral 0)) ty alts

  -- Otherwise check whether the entire case-expression has a single
  -- alternative, and pick that one.
  _ -> caseOneAlt e
 where
  sc = tfScope ctx

  isNum0 :: TypeEnv -> Type n -> Bool
  isNum0 te (tyView -> TyConApp (C.nameOcc -> tcNm) [arg])
    | tcNm `elem`
      [ Text.showt ''BitVector
      , Text.showt ''Signed
      , Text.showt ''Unsigned
      ]
    = isLitX te 0 arg
    | tcNm == Text.showt ''Index
    = isLitX te 1 arg
  isNum0 te (coreView1 te sc -> Just t) = isNum0 te t
  isNum0 _ _ = False

  isLitX :: TypeEnv -> Integer -> Type n -> Bool
  isLitX _ n (LitTy (C.NumTy m)) = n == m
  isLitX te n (coreView1 te sc -> Just t) = isLitX te n t
  isLitX _ _ _ = False
{-# SCC caseCon' #-}

-- | The first alternative whose pattern satisfies a predicate
findAlt :: (forall l. Pat n l -> Bool) -> [Alt n] -> Maybe (Alt n)
findAlt p = List.find (\(Alt pat _) -> p pat)

-- | Whether a variable occurs in a set of free variables
isUsed :: NameSet TmName l -> Term l -> Bool
isUsed fvs = \case
  Var _ _ nm -> nm `memberNameSet` fvs
  _ -> True

-- | Pair the fields of a pattern with the arguments of the data constructor,
-- see 'Data.List.Extra.zipEqual'
zipFields :: HasCallStack => [Bool] -> [Term n] -> [(Bool, Term n)]
zipFields (u:us) (a:as) = (u, a) : zipFields us as
zipFields [] [] = []
zipFields _ _ =
  error "caseCon: the numbers of fields of a pattern and of its data constructor differ"

-- | Substitute the given types for the existential type variables of a
-- pattern
substExistentials
  :: HasCallStack => Subst x n -> Tele TyBinder x i -> [Type n] -> Subst i n
substExistentials s TNil [] = s
substExistentials s (TCons b bs) (t:ts) = substExistentials (extendTySubst s b t) bs ts
substExistentials _ _ _ =
  error "caseCon: the numbers of existential type variables of a pattern and of its data constructor differ"

-- | Substitute the fields of a data constructor for the variables of a
-- pattern. A field that the alternative uses is substituted if it is work
-- free, and bound by a let-binding with a fresh binder otherwise. Gives the
-- substitution to apply to the alternative, and the let-bindings to wrap
-- around it, in the order of the fields.
bindFields
  :: (HasCallStack, Distinct o)
  => Scope o
  -> Subst x o
  -> Tele IdBinder x l
  -> [(Bool, Term o)]
  -- ^ Whether the alternative uses a field, and the field
  -> (forall o'. DExt o o' => Scope o' -> Subst l o' -> (Term o' -> Term o) -> NormalizeSession r)
  -> NormalizeSession r
bindFields sc s TNil [] k = k sc s id
bindFields sc s (TCons b bs) ((used, arg):args) k
  -- Unused: the variable is never looked up
  | not used = bindFields sc (extendTmSubst s b arg) bs args k
  | otherwise = isWorkFreeTerm arg >>= \case
      True -> bindFields sc (extendTmSubst s b arg) bs args k
      False ->
        withFreshIdBinder sc (idBinderOcc b) (substTy s (idBinderType b)) $ \sc1 b1 ->
          let s1 = extendTmSubst (extendSubstScope sc1 s) b (idBinderVar b1)
              args1 = [(u, sink a) | (u, a) <- args]
          in bindFields sc1 s1 bs args1 $ \sc2 s2 wrapLets ->
               k sc2 s2 (Let (NonRec b1 arg) . wrapLets)
bindFields _ _ _ _ _ =
  error "caseCon: the numbers of fields of a pattern and of its data constructor differ"

-- | See 'Clash.Normalize.Transformations.Case.matchLiteralContructor'. The
-- variable of the pattern of the matching alternative, if it is used, is
-- let-bound to the field of the literal, with the binder of the pattern.
matchLiteralConstructor
  :: Term n
  -> Literal
  -> [Alt n]
  -> NormalizeSession (Term n)
matchLiteralConstructor c (IntegerLiteral l) alts = go (reverse alts)
 where
  go [Alt DefaultPat e] = changed e
  go (Alt (DataPat dc TNil (TCons x TNil)) e : alts')
    | dcTag dc == 1
    , l >= ((-2)^(63::Int)) &&  l < 2^(63::Int)
    = bindField x (Literal (IntLiteral l)) e
    | dcTag dc == 2
    , l >= 2^(63::Int)
    = let !(IP ba) = l
       in bindField x (Literal (ByteArrayLiteral (BA.ByteArray ba))) e
    | dcTag dc == 3
    , l < ((-2)^(63::Int))
    = let !(IN ba) = l
       in bindField x (Literal (ByteArrayLiteral (BA.ByteArray ba))) e
    | otherwise
    = go alts'
  go (Alt (LitPat l') e : alts')
    | IntegerLiteral l == l'
    = changed e
    | otherwise
    = go alts'
  go _ = error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c

matchLiteralConstructor c (NaturalLiteral l) alts = go (reverse alts)
 where
  go [Alt DefaultPat e] = changed e
  go (Alt (DataPat dc TNil (TCons x TNil)) e : alts')
    | dcTag dc == 1
    , l >= 0 && l < 2^(64::Int)
    = bindField x (Literal (WordLiteral l)) e
    | dcTag dc == 2
    , l >= 2^(64::Int)
    = let !(IP ba) = l
       in bindField x (Literal (ByteArrayLiteral (BA.ByteArray ba))) e
    | otherwise
    = go alts'
  go (Alt (LitPat l') e : alts')
    | NaturalLiteral l == l'
    = changed e
    | otherwise
    = go alts'
  go _ = error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c

matchLiteralConstructor _ _ (Alt DefaultPat e : _) = changed e
matchLiteralConstructor c _ _ =
  error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c
{-# SCC matchLiteralConstructor #-}

-- | Bind the single field of a pattern with a let-binding, if it is used. The
-- binder of the pattern becomes the let-binder: it extends the scope in the
-- same way.
bindField :: IdBinder n l -> Term n -> Term l -> NormalizeSession (Term n)
bindField x val e = case unsinkIdBinder x e of
  Just e' -> changed e'
  Nothing -> changed (Let (NonRec x val) e)

-- | Remove non-reachable alternatives, see
-- 'Clash.Normalize.Transformations.Case.caseEliminateNonReachable'
caseEliminateNonReachable :: Transformation
caseEliminateNonReachable =
  toScopedTransformation "caseEliminateNonReachable"
    (onCase 'caseEliminateNonReachableWorker)

-- | The 'Case' handler of 'caseEliminateNonReachable'.
caseEliminateNonReachableWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
caseEliminateNonReachableWorker ctx case0 scrut altsTy alts0 = do
  te <- Lens.view scopedTypeEnv

  let isAbsurdAlt (Alt p _) = isAbsurdPat te (tfScope ctx) p
      (altsAbsurd, altsOther) = List.partition isAbsurdAlt alts0
  case altsAbsurd of
    [] -> return case0
    _  -> changed =<< caseOneAlt (Case scrut altsTy altsOther)
{-# SCC caseEliminateNonReachableWorker #-}

-- | Flatten ridiculous case-statements generated by GHC, see
-- 'Clash.Normalize.Transformations.Case.caseFlat'
caseFlat :: Transformation
caseFlat = toScopedTransformation "caseFlat" (onCase 'caseFlatWorker)

-- | The 'Case' handler of 'caseFlat'.
caseFlatWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
caseFlatWorker ctx e (collectEqArgs -> Just (scrut', val)) ty _ =
  case collectFlat scrut' e of
    Just alts' -> do
      te <- Lens.view scopedTypeEnv
      case collectArgs val of
        -- When we're pattern matching on `Int`, extract the `Int#` first before
        -- we do the Literal matching branches.
        (Data dc,_)
          | C.nameOcc (dcName dc) == Text.showt 'I#
          , Just [argTy] <- dataConInstArgTys te (tfScope ctx) dc []
          -> withFreshIdBinder (tfScope ctx) (internalName "wild") argTy $ \_ wild ->
               changed (Case scrut' ty
                         [Alt (DataPat dc TNil (TCons wild TNil))
                              (Case (idBinderVar wild) (sink ty)
                                    (sink1 (last alts' : init alts')))])
        _ -> changed (Case scrut' ty (last alts' : init alts'))
    Nothing -> return e

caseFlatWorker _ e _ _ _ = return e
{-# SCC caseFlatWorker #-}

-- | See 'Clash.Normalize.Transformations.Case.collectFlat'. The right-hand
-- sides of the alternatives of the comparisons are moved out of the scopes of
-- their patterns: the patterns of 'True' and 'False' don't bind variables.
collectFlat :: Term n -> Term n -> Maybe [Alt n]
collectFlat scrut (Case (collectEqArgs -> Just (scrut', val)) _ty [lAlt,rAlt])
  | scrut' `aeqTm` scrut
  = case collectArgs val of
      (Prim p,args') | isFromInt (primName p) ->
        go (last args')
      (Data dc,args') | C.nameOcc (dcName dc) == Text.showt 'I# ->
        go (last args')
      _ -> Nothing
  where
    go (Left (Literal i)) = case (lAlt,rAlt) of
      (Alt pl (unsinkAlt pl -> Just el), Alt pr (unsinkAlt pr -> Just er))
        | isFalseDcPat pl || isTrueDcPat pr ->
           case collectFlat scrut el of
             Just alts' -> Just (Alt (LitPat i) er : alts')
             Nothing    -> Just [Alt (LitPat i) er
                                ,Alt DefaultPat el
                                ]
        | otherwise ->
           case collectFlat scrut er of
             Just alts' -> Just (Alt (LitPat i) el : alts')
             Nothing    -> Just [Alt (LitPat i) el
                                ,Alt DefaultPat er
                                ]
      _ -> Nothing
    go _ = Nothing

    isFalseDcPat :: Pat n l -> Bool
    isFalseDcPat (DataPat p _ _)
      = ((== Text.showt 'False) . C.nameOcc . dcName) p
    isFalseDcPat _ = False

    isTrueDcPat :: Pat n l -> Bool
    isTrueDcPat (DataPat p _ _)
      = ((== Text.showt 'True) . C.nameOcc . dcName) p
    isTrueDcPat _ = False

collectFlat _ _ = Nothing
{-# SCC collectFlat #-}

-- | See 'Clash.Normalize.Transformations.Case.collectEqArgs'
collectEqArgs :: Term n -> Maybe (Term n, Term n)
collectEqArgs f@(collectArgsTicks -> (Prim p, args, ticks))
  | nm == Text.showt 'BV.eq#
    = case args of
        [_,_,Left scrut,Left val] -> Just (mkTicks scrut ticks,val)
        _ -> error ("collectEqArgs: BV.eq expects 4 arguments, but got: " <> showPpr f)
  | nm == Text.showt 'I.eq#  ||
    nm == Text.showt 'S.eq# ||
    nm == Text.showt 'U.eq#
    = case args of
        [_,Left scrut,Left val] -> Just (mkTicks scrut ticks,val)
        _ -> error (show nm <> " expects 3 arguments, but got: " <> showPpr f)
  | nm == Text.showt 'GHC.Classes.eqInt
    = case args of
        [Left scrut,Left val] -> Just (mkTicks scrut ticks,val)
        _ -> error ("eqInt expects 2 arguments, but got: " <> showPpr f)
 where
  nm = primName p

collectEqArgs _ = Nothing

-- | Lift the let-bindings out of the subject of a Case-decomposition
caseLet :: Transformation
caseLet = toScopedTransformation "caseLet" (onCase 'caseLetWorker)

-- | The 'Case' handler of 'caseLet'.
--
-- The alternatives are moved under the let-binders, which needs the scope
-- that opening them gives: a let-binder that binds a variable that occurs
-- free in the alternatives is renamed. See Note [CaseLet deshadow] in
-- "Clash.Normalize.Transformations.Case".
caseLetWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
caseLetWorker ctx _node (collectTicks -> (Let bind body, ticks)) ty alts =
  openBind (tfScope ctx) bind body $ \_ bind1 body1 ->
    let bind2 = case bind1 of
          NonRec b x -> NonRec b (mkTicks x ticks)
          Rec bs xs -> Rec bs (map (`mkTicks` sink1 ticks) xs)
    in changed (Let bind2 (Case (mkTicks body1 (sink1 ticks)) (sink ty) (sink1 alts)))

caseLetWorker _ e _ _ _ = return e
{-# SCC caseLetWorker #-}

-- | Replace a case-expression by the right-hand side of its alternative, if
-- it has one alternative, or by that of all its alternatives, if they are
-- equal. Only if the variables that the patterns bind don't occur in it. See
-- 'Clash.Normalize.Transformations.Case.caseOneAlt'.
caseOneAlt :: Term n -> NormalizeSession (Term n)
caseOneAlt e = case e of
  Case _ _ [Alt p altE]
    | Just altE1 <- unsinkAlt p altE -> changed altE1
  Case _ _ alts@(_:_:_)
    | Just altE <- commonAltBody alts -> changed altE
  _ -> return e
{-# SCC caseOneAlt #-}

-- | Tries to eliminate existentials by using heuristics to determine what the
-- existential should be, see
-- 'Clash.Normalize.Transformations.Case.elimExistentials'
elimExistentials :: Transformation
elimExistentials =
  toScopedTransformation "elimExistentials" (onCase 'elimExistentialsWorker)

-- | The 'Case' handler of 'elimExistentials'.
--
-- A solution replaces the occurrences of an existentially quantified type
-- variable, in the types of the fields and in the right-hand side of the
-- alternative, but the pattern still binds it (see
-- 'Clash.Core.Scoped.EqSolver.solvePatExistentials'). This repeats until no
-- more equations are solved.
elimExistentialsWorker
  :: forall n. (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
elimExistentialsWorker ctx _node scrut altsTy alts0 = do
  te <- Lens.view scopedTypeEnv
  alts1 <- traverse (go te) alts0
  caseOneAlt (Case scrut altsTy alts1)
 where
  -- Eliminate free type variables if possible
  go :: TypeEnv -> Alt n -> NormalizeSession (Alt n)
  go te alt@(Alt pat term0) =
    case solvePatExistentials te (tfScope ctx) pat (\s pat1 -> Alt pat1 (substTm s term0)) of
      -- No equations solved:
      Nothing -> return alt
      -- One or more equations solved:
      Just alt1 -> changed =<< go te alt1
{-# SCC elimExistentialsWorker #-}

-- | This finds cases on Integers and Naturals and rewrites them to remove the
-- alternatives with bignums/bytearrays in them, see
-- 'Clash.Normalize.Transformations.Case.elimCaseBigNumInternals'
elimCaseBigNumInternals :: Transformation
elimCaseBigNumInternals =
  toScopedTransformation "elimCaseBigNum" (onCase 'elimCaseBigNumInternalsWorker)

-- | The 'Case' handler of 'elimCaseBigNumInternals'.
elimCaseBigNumInternalsWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
elimCaseBigNumInternalsWorker _ e scrut altsTy alts0@(_:_:_) =
  go alts0
 where
  go [] = return e
  go (alt@(Alt pat altE):alts) = case pat of
    DataPat dc TNil (TCons x TNil) | (dc == integerIsDc || dc == naturalNsDc) ->
      case unsinkIdBinder x altE of
        -- field not used, eliminate the case completely
        Just altE1 -> changed altE1
        -- field used, turn the case into a projection
        -- It seems this pattern never happens after ANF.
        Nothing -> changed (Case scrut altsTy [alt])
    _ -> go alts

elimCaseBigNumInternalsWorker _ e _ _ _ = return e
