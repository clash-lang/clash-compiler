{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

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
import Clash.Core.Term
  (Bind, LetBinding, Term(..), bindToList, collectArgs, stripTicks)
import Clash.Core.TermInfo (isCast)
import Clash.Core.Type (Type, normalizeType)
import Clash.Core.Var (isGlobalId, varName)
import Clash.Core.VarEnv (InScopeSet)
import Clash.Debug (traceM)
import Clash.Normalize.Transformations.Specialize (specialize)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL
  (Transformation, onApp, onCast, onLet, toTransformation)
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
-- eliminate them in 'elimCastCast'.
argCastSpec :: Transformation
argCastSpec = toTransformation "argCastSpec" (onApp 'argCastSpecWorker)

-- | The 'App' handler of 'argCastSpec'.
argCastSpecWorker
  :: HasCallStack
  => TransformContext -> Term -> Term -> Term -> NormalizeSession Term
argCastSpecWorker ctx node f (stripTicks -> Cast e' _ _)
 -- Don't specialise when the arguments are casts-of-casts, these
 -- casts-of-casts will be eliminated by 'elimCastCast' during the
 -- normalization of the "current" function. We thus prevent the unnecessary
 -- introduction of a specialized version of 'f'.
 | not (isCast e')
 -- We can only push casts into global binders
 , (Var g, _) <- collectArgs f
 , isGlobalId g = do
  bndrsV <- Lens.use bindings
  wf <- MVar.withMVar "bindings" bndrsV (\bndrs -> isWorkFree workFreeBinders bndrs e')

  ioLockV <- Lens.use ioLock

  Monad.when (not wf) $
    MVar.withMVar "ioLock" ioLockV $ \() -> traceM warn

  specialize ctx node
 where
  warn = unwords
    [ "WARNING:", $(curLoc), "specializing a function on a non work-free"
    , "cast. Generated HDL implementation might contain duplicate work."
    , "Please report this as a bug.", "\n\nExpression where this occured:"
    , "\n\n" ++ showPpr node
    ]
argCastSpecWorker _ctx node _function _argument = return node
{-# SCC argCastSpecWorker #-}

-- | Push a cast over a case into it's alternatives.
caseCast :: Transformation
caseCast = toTransformation "caseCast" (onCast 'caseCastWorker)

-- | The 'Cast' handler of 'caseCast'.
caseCastWorker
  :: HasCallStack
  => TransformContext -> Term -> Term -> Type -> Type -> NormalizeSession Term
caseCastWorker _ctx _node (stripTicks -> Case subj ty alts) ty1 ty2 = do
  let alts' = map (\(p,e) -> (p, Cast e ty1 ty2)) alts
  changed (Case subj ty alts')
caseCastWorker _ctx node _body _fromType _toType = return node
{-# SCC caseCastWorker #-}

-- | Eliminate two back to back casts where the type going in and coming out are the same
--
-- @
--   (cast :: b -> a) $ (cast :: a -> b) x   ==> x
-- @
elimCastCast :: Transformation
elimCastCast = toTransformation "elimCastCast" (onCast 'elimCastCastWorker)

-- | The 'Cast' handler of 'elimCastCast'.
elimCastCastWorker
  :: HasCallStack
  => TransformContext -> Term -> Term -> Type -> Type -> NormalizeSession Term
elimCastCastWorker _ctx node (stripTicks -> Cast e tyA tyB) tyB' tyC = do
  tcm <- Lens.view tcCache
  let ntyA  = normalizeType tcm tyA
      ntyB  = normalizeType tcm tyB
      ntyB' = normalizeType tcm tyB'
      ntyC  = normalizeType tcm tyC
  if ntyB == ntyB' && ntyA == ntyC then changed e
                                   else throwError
 where
  throwError = do
    curFunsV <- Lens.use curFun
    thread <- myThreadId
    Just (nm,sp) <- MVar.withMVar "curFun" curFunsV (pure . HashMap.lookup thread)
    throw (ClashException sp ($(curLoc) ++ showPpr nm
            ++ ": Found 2 nested casts whose types don't line up:\n"
            ++ showPpr node)
          Nothing)
elimCastCastWorker _ctx node _body _fromType _toType = return node
{-# SCC elimCastCastWorker #-}

-- | Push a cast over a Let into it's body
letCast :: Transformation
letCast = toTransformation "letCast" (onCast 'letCastWorker)

-- | The 'Cast' handler of 'letCast'.
letCastWorker
  :: HasCallStack
  => TransformContext -> Term -> Term -> Type -> Type -> NormalizeSession Term
letCastWorker _ctx _node (stripTicks -> Let binds body) ty1 ty2 =
  changed $ Let binds (Cast body ty1 ty2)
letCastWorker _ctx node _body _fromType _toType = return node
{-# SCC letCastWorker #-}

-- | Make a cast work-free by splitting the work of to a separate binding
--
-- @
-- let x = cast (f a b)
-- ==>
-- let x  = cast x'
--     x' = f a b
-- @
splitCastWork :: Transformation
splitCastWork = toTransformation "splitCastWork" (onLet 'splitCastWorkWorker)

-- | The 'Let' handler of 'splitCastWork'.
splitCastWorkWorker
  :: HasCallStack
  => TransformContext -> Term -> Bind Term -> Term -> NormalizeSession Term
splitCastWorkWorker ctx@(TransformContext is0 _) node bind body = do
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
{-# SCC splitCastWorkWorker #-}
