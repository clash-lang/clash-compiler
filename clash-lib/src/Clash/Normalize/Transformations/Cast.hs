{-# LANGUAGE LambdaCase #-}
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
  (LetBinding, Term(..), bindToList, collectArgs, stripTicks)
import Clash.Core.TermInfo (isCast)
import Clash.Core.Type (normalizeType)
import Clash.Core.Var (isGlobalId, varName)
import Clash.Core.VarEnv (InScopeSet)
import Clash.Debug (trace)
import Clash.Normalize.Transformations.Specialize (specialize)
import Clash.Normalize.Types (NormShapedTransformation, NormalizeSession)
import Clash.Rewrite.Shape (applyApp, applyCast, applyLet)
import Clash.Rewrite.Types
  (TransformContext(..), bindings, curFun, tcCache, workFreeBinders)
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
-- eliminate them in 'elimCastCast'.
argCastSpec :: HasCallStack => NormShapedTransformation
argCastSpec = applyApp "argCastSpec" go
 where
  go ctx node f (stripTicks -> Cast e' _ _)
   -- Don't specialise when the arguments are casts-of-casts, these
   -- casts-of-casts will be eliminated by 'elimCastCast' during the
   -- normalization of the "current" function. We thus prevent the unnecessary
   -- introduction of a specialized version of 'f'.
   | not (isCast e')
   -- We can only push casts into global binders
   , (Var g, _) <- collectArgs f
   , isGlobalId g = do
    bndrs <- Lens.use bindings
    isWorkFree workFreeBinders bndrs e' >>= \case
      True -> specializeNode
      False -> warn specializeNode
   where
    specializeNode = specialize ctx node
    warn = trace (unwords
      [ "WARNING:", $(curLoc), "specializing a function on a non work-free"
      , "cast. Generated HDL implementation might contain duplicate work."
      , "Please report this as a bug.", "\n\nExpression where this occured:"
      , "\n\n" ++ showPpr node
      ])
  go _ctx node _function _argument = return node
{-# SCC argCastSpec #-}

-- | Push a cast over a case into it's alternatives.
caseCast :: HasCallStack => NormShapedTransformation
caseCast = applyCast "caseCast" go
 where
  go _ctx _node (stripTicks -> Case subj ty alts) ty1 ty2 = do
    let alts' = map (\(p,e) -> (p, Cast e ty1 ty2)) alts
    changed (Case subj ty alts')
  go _ctx node _body _fromType _toType = return node
{-# SCC caseCast #-}

-- | Eliminate two back to back casts where the type going in and coming out are the same
--
-- @
--   (cast :: b -> a) $ (cast :: a -> b) x   ==> x
-- @
elimCastCast :: HasCallStack => NormShapedTransformation
elimCastCast = applyCast "elimCastCast" go
 where
  go _ctx node (stripTicks -> Cast e tyA tyB) tyB' tyC = do
    tcm <- Lens.view tcCache
    let ntyA  = normalizeType tcm tyA
        ntyB  = normalizeType tcm tyB
        ntyB' = normalizeType tcm tyB'
        ntyC  = normalizeType tcm tyC
    if ntyB == ntyB' && ntyA == ntyC then changed e
                                     else throwError
   where
    throwError = do
      (nm,sp) <- Lens.use curFun
      throw (ClashException sp ($(curLoc) ++ showPpr nm
              ++ ": Found 2 nested casts whose types don't line up:\n"
              ++ showPpr node)
            Nothing)
  go _ctx node _body _fromType _toType = return node
{-# SCC elimCastCast #-}

-- | Push a cast over a Let into it's body
letCast :: HasCallStack => NormShapedTransformation
letCast = applyCast "letCast" go
 where
  go _ctx _node (stripTicks -> Let binds body) ty1 ty2 =
    changed $ Let binds (Cast body ty1 ty2)
  go _ctx node _body _fromType _toType = return node
{-# SCC letCast #-}

-- | Make a cast work-free by splitting the work of to a separate binding
--
-- @
-- let x = cast (f a b)
-- ==>
-- let x  = cast x'
--     x' = f a b
-- @
splitCastWork :: HasCallStack => NormShapedTransformation
splitCastWork = applyLet "splitCastWork" go
 where
  go ctx@(TransformContext is0 _) node bind body = do
    (vss', Monoid.getAny -> hasChanged) <-
      listen (mapM (splitCastLetBinding is0) (bindToList bind))
    let vs' = concat vss'
    if hasChanged then changed (Letrec vs' body)
                  else return node
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
{-# SCC splitCastWork #-}
