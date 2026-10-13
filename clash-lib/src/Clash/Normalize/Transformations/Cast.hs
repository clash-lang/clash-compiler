{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on casts of Clash Core.

  The transformations that move a term under binders, or that create new
  binders, can't capture variables:

  * 'caseCast' moves the cast under the binders of the patterns. Those can't
    capture the free type variables of the cast, as the cast is sunk into the
    scope of the alternative, which renames the binders that clash with the
    scope.

  * 'splitCastWork' creates its new binders in the scope of the let-binders,
    so they are fresh with respect to those, and to each other.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Normalize.Transformations.Cast
  ( argCastSpec
  , argCastSpecWorker
  , caseCast
  , caseCastWorker
  , elimCastCast
  , elimCastCastWorker
  , letCast
  , letCastWorker
  , splitCastWork
  , splitCastWorkWorker
  ) where

import Control.Exception (throw)
import qualified Control.Lens as Lens
import Data.Maybe (isNothing)
import GHC.Stack (HasCallStack)

import Clash.Core.Name (nameOcc)
import qualified Clash.Core.Name as C
import Clash.Core.Pretty (showPpr)
import Clash.Driver.Warning (warnAboutM)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL
  (Transformation, onApp, onCast, onLet, toTransformation)
import Clash.Rewrite.Types (curFun, typeEnv)
import Clash.Util (ClashException (..), curLoc)
import Clash.Warning (ClashWarning (WarnCastSpecialization))

import Clash.Core.Alpha (aeqTy)
import Clash.Core.HasType (inferCoreTypeOf)
import Clash.Core.Subst
  ( extendSubstScope, extendTmSubstTele, idSubst, openAlt, openBind
  , strengthenTyIds, substTm, substTy, teleIdBinderTypes, withFreshIdBinders )
import Clash.Core.Syntax
import Clash.Core.Term (collectArgs, stripTicks)
import Clash.Core.TermInfo (isCast)
import Clash.Core.Type (normalizeType)
import Clash.Normalize.Transformations.Specialize (specialize)
import Clash.Rewrite (TransformContext, changed, mkDerivedName, tfScope)
import Clash.Rewrite.Globals (isWorkFreeTerm)
import Clash.Rewrite.Util (mkTmBinderFor)

-- | Push a cast over an argument to a function into that function
--
-- This is done by specializing on the casted argument.
-- Example:
--
-- @
--   y = f (cast a)
--     where f x = g x
-- @
--
-- transforms to:
--
-- @
--   y = f' a
--     where f' x' = (\\x -> g x) (cast x')
-- @
--
-- The reason d'etre for this transformation is that we hope to end up with
-- and expression where two casts are "back-to-back" after which we can
-- eliminate them in 'elimCastCast'.
argCastSpec :: Transformation
argCastSpec = toTransformation "argCastSpec" (onApp 'argCastSpecWorker)

-- | The 'App' handler of 'argCastSpec'
argCastSpecWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Term n
  -> NormalizeSession (Term n)
argCastSpecWorker ctx node f arg
  | Cast e' _ _ <- stripTicks arg
  -- Don't specialise when the arguments are casts-of-casts, these
  -- casts-of-casts will be eliminated by 'elimCastCast' during the
  -- normalization of the "current" function. We thus prevent the unnecessary
  -- introduction of a specialized version of 'f'.
  , not (isCast e')
  -- We can only push casts into global binders
  , (GlobalVar _, _) <- collectArgs f
  = isWorkFreeTerm e' >>= \case
      True -> specialize ctx node
      False -> warn >> specialize ctx node
  | otherwise = return node
 where
  warn = do
    (_, sp) <- Lens.use curFun
    warnAboutM WarnCastSpecialization sp $ unwords
      [ $(curLoc) ++ "specializing a function on a non work-free"
      , "cast. Generated HDL implementation might contain duplicate work."
      , "Please report this as a bug.", "\n\nExpression where this occured:"
      , "\n\n" ++ showPpr node
      ]
{-# SCC argCastSpecWorker #-}

-- | Push a cast over a case into its alternatives
caseCast :: Transformation
caseCast = toTransformation "caseCast" (onCast 'caseCastWorker)

-- | The 'Cast' handler of 'caseCast'
caseCastWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> Type n
  -> NormalizeSession (Term n)
caseCastWorker ctx node body ty1 ty2 = case stripTicks body of
  Case subj _ alts -> changed (Case subj ty2 (map castAlt alts))
  _ -> return node
 where
  castAlt alt = openAlt (tfScope ctx) alt $ \_ p e -> Alt p (Cast e (sink ty1) (sink ty2))
{-# SCC caseCastWorker #-}

-- | Eliminate two back to back casts where the type going in and coming out
-- are the same
--
-- @
--   (cast :: b -> a) $ (cast :: a -> b) x   ==> x
-- @
elimCastCast :: Transformation
elimCastCast = toTransformation "elimCastCast" (onCast 'elimCastCastWorker)

-- | The 'Cast' handler of 'elimCastCast'
elimCastCastWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> Type n
  -> NormalizeSession (Term n)
elimCastCastWorker ctx node body tyB' tyC = case stripTicks body of
  Cast e tyA tyB -> do
    te <- Lens.view typeEnv
    let norm = normalizeType te (tfScope ctx)
    if aeqTy (norm tyB) (norm tyB') && aeqTy (norm tyA) (norm tyC)
      then changed e
      else do
        (nm, sp) <- Lens.use curFun
        throw (ClashException sp ($(curLoc) ++ showPpr nm
                ++ ": Found 2 nested casts whose types don't line up:\n"
                ++ showPpr node)
              Nothing)
  _ -> return node
{-# SCC elimCastCastWorker #-}

-- | Push a cast over a let-expression into its body
letCast :: Transformation
letCast = toTransformation "letCast" (onCast 'letCastWorker)

-- | The 'Cast' handler of 'letCast'
letCastWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> Type n
  -> NormalizeSession (Term n)
letCastWorker ctx node body ty1 ty2 = case stripTicks body of
  Let bind e -> openBind (tfScope ctx) bind e $ \_ bind' e' ->
    changed (Let bind' (Cast e' (sink ty1) (sink ty2)))
  _ -> return node
{-# SCC letCastWorker #-}

-- | Make a cast work-free by splitting the work off to a separate binding:
--
-- @
-- let x = cast (f a b)
-- ==>
-- let x' = f a b
--     x  = cast x'
-- @
splitCastWork :: Transformation
splitCastWork = toTransformation "splitCastWork" (onLet 'splitCastWorkWorker)

-- | The 'Let' handler of 'splitCastWork'.
--
-- A non-recursive binding stays non-recursive: the new binding goes in a
-- let-expression around it. In a recursive let-expression, the new binding
-- goes right before the binding it splits, which renames all binders of the
-- let-expression.
splitCastWorkWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Bind n l
  -> Term l
  -> NormalizeSession (Term n)
splitCastWorkWorker ctx node bind body = do
  te <- Lens.view typeEnv
  case bind of
    NonRec b (castWork -> Just (e0, _, _)) ->
      mkTmBinderFor te sc (derivedName (idBinderOcc b)) e0 $ \_ b' ->
        -- The original let-expression, in the scope with the new binder
        case sink node of
          Let (NonRec bS (castWork -> Just (_, ty1, ty2))) bodyS ->
            changed (Let (NonRec b' e0) (Let (NonRec bS (Cast (idBinderVar b') ty1 ty2)) bodyS))
          _ -> error ($(curLoc) ++ "splitCastWork: not a let-expression")
    NonRec {} -> return node
    Rec {} -> openBind sc bind body $ \sc1 bind1 body1 -> case bind1 of
      Rec bs xs
        | let works = map castWork xs
        , not (all isNothing works) ->
            let
              -- The binders in their new order: that of the work of a
              -- binding goes right before it
              binders = concat
                [ case w of
                    Just (e0, _, _) ->
                      [ (derivedName occ, strengthenTyIds bs (inferCoreTypeOf te sc1 e0))
                      , (occ, ty) ]
                    Nothing -> [(occ, ty)]
                | (occ, ty, w) <- zip3 (teleOccs bs) (teleIdBinderTypes bs) works ]
            in
              withFreshIdBinders sc binders $ \sc2 bs' vs' ->
                case extendTmSubstTele (extendSubstScope sc2 (idSubst sc)) bs (oldVars works vs') of
                  Just s ->
                    let
                      rhss ((_, Just (e0, ty1, ty2)) : rest) (vNew : _ : vs) =
                        substTm s e0 : Cast vNew (substTy s ty1) (substTy s ty2) : rhss rest vs
                      rhss ((x, Nothing) : rest) (_ : vs) = substTm s x : rhss rest vs
                      rhss _ _ = []
                    in
                      changed (Let (Rec bs' (rhss (zip xs works) vs')) (substTm s body1))
                  Nothing -> error ($(curLoc) ++ "splitCastWork: binders and variables don't match")
      _ -> return node
 where
  sc = tfScope ctx

  -- The name of a new binder, derived from that of the binding it splits
  derivedName :: C.Name Term -> C.Name Term
  derivedName occ = mkDerivedName ctx (nameOcc occ)

  -- The variables of the original binders, among those in the new order
  oldVars :: [Maybe a] -> [Term x] -> [Term x]
  oldVars (Just _ : ws) (_ : v : vs) = v : oldVars ws vs
  oldVars (Nothing : ws) (v : vs) = v : oldVars ws vs
  oldVars _ _ = []

  teleOccs :: Tele IdBinder x y -> [C.Name Term]
  teleOccs = \case
    TNil -> []
    TCons b rest -> idBinderOcc b : teleOccs rest
{-# SCC splitCastWorkWorker #-}

-- | The work, and the types, of a cast that 'splitCastWork' splits off. A cast
-- of a variable is already work-free, and casts of casts are eliminated by
-- 'elimCastCast'.
castWork :: Term n -> Maybe (Term n, Type n, Type n)
castWork e = case stripTicks e of
  Cast Var {} _ _ -> Nothing
  Cast GlobalVar {} _ _ -> Nothing
  Cast Cast {} _ _ -> Nothing
  Cast e0 ty1 ty2 -> Just (e0, ty1, ty2)
  _ -> Nothing
