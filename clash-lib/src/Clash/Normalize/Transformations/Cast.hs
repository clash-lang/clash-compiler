{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations.Cast
  ( argCastSpec
  , caseCast
  , elimCastCast
  , letCast
  , splitCastWork
  ) where

import Control.Exception (throw)
import qualified Control.Lens as Lens
import Control.Monad.Writer (listen)
import qualified Data.Monoid as Monoid (Any(..))
import GHC.Stack (HasCallStack)

import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Term
  (Bind(..), LetBinding, PrimInfo(..), Term(..), collectArgs,
   collectArgsTicks, collectTicks, mkTicks, stripTicks)
import Clash.Core.TermInfo (isCast)
import Clash.Core.Util (castEqType, undefinedPrims, undefinedXPrims)
import qualified Clash.Normalize.Primitives as NP (undefined, undefinedX)
import Clash.Core.Var (isGlobalId, varName)
import Clash.Core.VarEnv (InScopeSet)
import Clash.Normalize.Transformations.Specialize (specialize)
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Rewrite.Types
  (TransformContext(..), bindings, curFun, tcCache, workFreeBinders)
import Clash.Rewrite.Util
  (changed, isUntranslatableType, mkDerivedName, mkTmBinderFor)
import Clash.Rewrite.WorkFree (isWorkFree)
import Clash.Util (ClashException(..), curLoc)

-- | Push cast over an argument to a function into that function
--
-- This is done by specializing on the casted argument.
-- Example:
-- @
--   y = f (cast a)
--     where f x = g x
-- @
-- transforms to:
-- @
--   y = f' a
--     where f' x' = (\\x -> g x) (cast x')
-- @
--
-- The reason d'etre for this transformation is that we hope to end up with
-- and expression where two casts are "back-to-back" after which we can
-- eliminate them in 'elimCastCast'.
argCastSpec :: HasCallStack => NormRewrite
argCastSpec ctx@(TransformContext is0 _) e@(App f (collectTicks -> (Cast e' t1 t2, ticks)))
 -- Don't specialise when the arguments are casts-of-casts, these casts-of-casts
 -- will be eliminated by 'elimCastCast' during the normalization of the
 -- "current" function. We thus prevent the unnecessary introduction of a
 -- specialized version of 'f'.
 | not (isCast e')
 -- We can only push casts into global binders
 , (Var g, _) <- collectArgs f
 , isGlobalId g = do
  tcm <- Lens.view tcCache
  -- A cast that is refl under the cast-equality oracle is not worth
  -- specializing on: 'elimCastCast' simply drops it. Specializing on it
  -- instead pollutes the specialization cache with (cast-decorated) variants
  -- of otherwise identical arguments, which can hit the specialization limit
  -- on dictionary-heavy code.
  if castEqType tcm t1 t2 then return e else do
  bndrs <- Lens.use bindings
  isWorkFree workFreeBinders bndrs e' >>= \case
    True -> specialize ctx e
    False -> do
      nonRep <- isUntranslatableType False t1
      if nonRep then
        -- A let-binding with a non-representable type would immediately be
        -- inlined again by 'inlineOrLiftNonRep'; specialize directly instead.
        specialize ctx e
      else do
        -- Bind the work to a new binder, so we only specialize on a
        -- work-free cast of a variable reference. A later pass then
        -- specializes on @f (cast x)@ while the work is shared through the
        -- let-binding.
        x <- mkTmBinderFor is0 tcm (mkDerivedName ctx "argCastSpec") e'
        changed (Let (NonRec x (mkTicks e' ticks)) (App f (Cast (Var x) t1 t2)))
argCastSpec _ e = return e
{-# SCC argCastSpec #-}

-- | Push a cast over a case into it's alternatives.
caseCast :: HasCallStack => NormRewrite
caseCast _ (Cast (collectTicks -> (Case subj _ty alts, ticks)) ty1 ty2) = do
  let alts' = map (\(p,e) -> (p, Cast e ty1 ty2)) alts
  changed (mkTicks (Case subj ty2 alts') ticks)
caseCast _ e = return e
{-# SCC caseCast #-}

-- | Eliminate or merge two back to back casts, and eliminate casts between
-- equal types:
--
-- @
--   (e ▷ A ~ B) ▷ B ~ A  ==> e
--   (e ▷ A ~ B) ▷ B ~ C  ==> e ▷ A ~ C
--   e ▷ A ~ A            ==> e
-- @
--
-- Type equality is 'castEqType', see Note [Cast-equality oracle].
elimCastCast :: HasCallStack => NormRewrite
elimCastCast _ c@(Cast (collectTicks -> (Cast e tyA tyB, ticks)) tyB' tyC) = do
  tcm <- Lens.view tcCache
  if castEqType tcm tyB tyB' then
    if castEqType tcm tyA tyC
      then changed (mkTicks e ticks)
      else changed (Cast (mkTicks e ticks) tyA tyC)
  else throwError
  where throwError = do
          (nm,sp) <- Lens.use curFun
          throw (ClashException sp ($(curLoc) ++ showPpr nm
                  ++ ": Found 2 nested casts whose types don't line up:\n"
                  ++ showPpr c)
                Nothing)

-- A cast of an undefined value is an undefined value at the target type.
elimCastCast _ (Cast (collectArgsTicks -> (Prim p, _, ticks)) _tyA tyB)
  | primName p `elem` undefinedXPrims
  = changed (mkTicks (TyApp (Prim NP.undefinedX) tyB) ticks)
  | primName p `elem` undefinedPrims
  = changed (mkTicks (TyApp (Prim NP.undefined) tyB) ticks)

elimCastCast _ (Cast e tyA tyB) = do
  tcm <- Lens.view tcCache
  if castEqType tcm tyA tyB then changed e else return (Cast e tyA tyB)

elimCastCast _ e = return e
{-# SCC elimCastCast #-}

-- | Push a cast over a Let into it's body
letCast :: HasCallStack => NormRewrite
letCast _ (Cast (collectTicks -> (Let binds body, ticks)) ty1 ty2) =
  changed $ mkTicks (Let binds (Cast body ty1 ty2)) ticks
letCast _ e = return e
{-# SCC letCast #-}

-- | Make a cast work-free by splitting the work of to a separate binding
--
-- @
-- let x = cast (f a b)
-- ==>
-- let x  = cast x'
--     x' = f a b
-- @
splitCastWork :: HasCallStack => NormRewrite
splitCastWork ctx@(TransformContext is0 _) unchanged@(Letrec vs e') = do
  (vss', Monoid.getAny -> hasChanged) <- listen (mapM (splitCastLetBinding is0) vs)
  let vs' = concat vss'
  if hasChanged then changed (Letrec vs' e')
                else return unchanged
  where
    splitCastLetBinding
      :: InScopeSet
      -> LetBinding
      -> NormalizeSession [LetBinding]
    splitCastLetBinding isN x@(nm, e) = case stripTicks e of
      Cast (Var {}) _ _  -> return [x]  -- already work-free
      Cast (Cast {}) _ _ -> return [x]  -- casts will be eliminated
      Cast e0 ty1 ty2 -> do
        tcm <- Lens.view tcCache
        nm' <- mkTmBinderFor isN tcm (mkDerivedName ctx (nameOcc $ varName nm)) e0
        changed [(nm',e0)
                ,(nm, Cast (Var nm') ty1 ty2)
                ]
      _ -> return [x]

splitCastWork _ e = return e
{-# SCC splitCastWork #-}
