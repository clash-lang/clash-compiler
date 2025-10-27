{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations.Cast
  ( argCastSpec
  , caseCast
  , elimCastCast
  , letCast
  , splitCastWork
  ) where

import Control.Concurrent.Lifted (myThreadId)
import qualified Clash.Normalize.TracedMVar as MVar
import Control.Exception (throw)
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad (when)
import Control.Monad.Writer (listen)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Monoid as Monoid (Any(..))
import GHC.Stack (HasCallStack)

import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Term (LetBinding, Term(..), collectArgs, stripTicks)
import Clash.Core.TermInfo (isCast)
import Clash.Core.Type (normalizeType)
import Clash.Core.Var (isGlobalId, varName)
import Clash.Core.VarEnv (InScopeSet)
import Clash.Debug (traceM)
import Clash.Normalize.Transformations.Specialize (specialize)
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Rewrite.Types
  (TransformContext(..), bindings, curFun, tcCache, workFreeBinders, ioLock)
import Clash.Rewrite.Util (changed, mkDerivedName, mkTmBinderFor)
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
-- eliminate them in 'eliminateCastCast'.
argCastSpec :: HasCallStack => NormRewrite
argCastSpec ctx e@(App f (stripTicks -> Cast e' _ _))
 -- Don't specialise when the arguments are casts-of-casts, these casts-of-casts
 -- will be eliminated by 'eliminateCastCast' during the normalization of the
 -- "current" function. We thus prevent the unnecessary introduction of a
 -- specialized version of 'f'.
 | not (isCast e')
 -- We can only push casts into global binders
 , (Var g, _) <- collectArgs f
 , isGlobalId g = do
  bndrsV <- Lens.use bindings
  wf <- MVar.withMVar "bindings" bndrsV (\bndrs -> isWorkFree workFreeBinders bndrs e')

  ioLockV <- Lens.use ioLock

  Monad.when (not wf) $
    MVar.withMVar "ioLock" ioLockV $ \() -> traceM warn

  specialize ctx e
 where
  warn = unwords
    [ "WARNING:", $(curLoc), "specializing a function on a non work-free"
    , "cast. Generated HDL implementation might contain duplicate work."
    , "Please report this as a bug.", "\n\nExpression where this occured:"
    , "\n\n" ++ showPpr e
    ]

argCastSpec _ e = return e
{-# SCC argCastSpec #-}

-- | Push a cast over a case into it's alternatives.
caseCast :: HasCallStack => NormRewrite
caseCast _ (Cast (stripTicks -> Case subj ty alts) ty1 ty2) = do
  let alts' = map (\(p,e) -> (p, Cast e ty1 ty2)) alts
  changed (Case subj ty alts')
caseCast _ e = return e
{-# SCC caseCast #-}

-- | Eliminate two back to back casts where the type going in and coming out are the same
--
-- @
--   (cast :: b -> a) $ (cast :: a -> b) x   ==> x
-- @
elimCastCast :: HasCallStack => NormRewrite
elimCastCast _ c@(Cast (stripTicks -> Cast e tyA tyB) tyB' tyC) = do
  tcm <- Lens.view tcCache
  let ntyA  = normalizeType tcm tyA
      ntyB  = normalizeType tcm tyB
      ntyB' = normalizeType tcm tyB'
      ntyC  = normalizeType tcm tyC
  if ntyB == ntyB' && ntyA == ntyC then changed e
                                   else throwError
  where throwError = do
          curFunsV <- Lens.use curFun
          thread <- myThreadId
          Just (nm,sp) <- MVar.withMVar "curFun" curFunsV (pure . HashMap.lookup thread)
          throw (ClashException sp ($(curLoc) ++ showPpr nm
                  ++ ": Found 2 nested casts whose types don't line up:\n"
                  ++ showPpr c)
                Nothing)

elimCastCast _ e = return e
{-# SCC elimCastCast #-}

-- | Push a cast over a Let into it's body
letCast :: HasCallStack => NormRewrite
letCast _ (Cast (stripTicks -> Let binds body) ty1 ty2) =
  changed $ Let binds (Cast body ty1 ty2)
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
