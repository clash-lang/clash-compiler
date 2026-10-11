{-# LANGUAGE GADTs #-}
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

import Control.Exception (throw)
import qualified Control.Lens as Lens
import Control.Monad.Writer (listen)
import qualified Data.List.Extra as List
import qualified Data.Monoid as Monoid (Any(..))
import GHC.Stack (HasCallStack)

import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Subst (deShadowAlt)
import Clash.Core.Term
  (Bind(..), LetBinding, Term(..), bindToList, collectArgs, stripTicks)
import Clash.Core.TermInfo (isCast)
import Clash.Core.Type (Type, normalizeType)
import Clash.Core.Var (isGlobalId, varName)
import Clash.Core.VarEnv (InScopeSet, extendInScopeSet, extendInScopeSetList)
import qualified Clash.Core.Scoped.Syntax as S
import Clash.Driver.Warning (warnAboutM)
import qualified Clash.Normalize.Scoped.Transformations.Cast as Scoped
import Clash.Normalize.Transformations.Specialize (specialize)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL
  (Transformation, onApp, onLet, toTransformation)
import Clash.Rewrite.Scoped.ViaFoil (scopedAppWorker, viaFoilWhen)
import Clash.Rewrite.Types
  (TransformContext(..), bindings, curFun, tcCache, workFreeBinders)
import Clash.Rewrite.Scoped (ScopedTransform (..))
import qualified Clash.Rewrite.Scoped as Scoped (TransformContext)
import Clash.Rewrite.Util (changed, mkDerivedName, mkTmBinderFor)
import Clash.Rewrite.WorkFree (isWorkFree)
import Clash.Util (ClashException(..), curLoc)
import Clash.Warning (ClashWarning(WarnCastSpecialization))

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
argCastSpec = toTransformation "argCastSpec" (onApp 'argCastSpecViaFoil)

-- | 'Scoped.argCastSpec', which replaced 'argCastSpecWorker', through
-- 'viaFoilWhen' at the applications at which it can fire, see
-- 'argCastSpecWorker'
argCastSpecViaFoil
  :: HasCallStack
  => TransformContext -> Term -> Term -> Term -> NormalizeSession Term
argCastSpecViaFoil ctx node f arg =
  viaFoilWhen "argCastSpec" (\_ _ -> pure fires)
    (scopedAppWorker Scoped.argCastSpecWorker) ctx node
 where
  fires
    | Cast e' _ _ <- stripTicks arg
    , not (isCast e')
    , (Var g, _) <- collectArgs f
    = isGlobalId g
    | otherwise = False

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
  bndrs <- Lens.use bindings
  isWorkFree workFreeBinders bndrs e' >>= \case
    True -> specializeNode
    False -> warn >> specializeNode
 where
  specializeNode = specialize ctx node
  warn = do
    (_,sp) <- Lens.use curFun
    warnAboutM WarnCastSpecialization sp $ unwords
      [ $(curLoc) ++ "specializing a function on a non work-free"
      , "cast. Generated HDL implementation might contain duplicate work."
      , "Please report this as a bug.", "\n\nExpression where this occured:"
      , "\n\n" ++ showPpr node
      ]
argCastSpecWorker _ctx node _function _argument = return node
{-# SCC argCastSpecWorker #-}

-- | Push a cast over a case into it's alternatives.
caseCast :: Transformation
caseCast = Scoped.caseCast

-- | The 'Cast' handler of 'caseCast'.
caseCastWorker
  :: HasCallStack
  => TransformContext -> Term -> Term -> Type -> Type -> NormalizeSession Term
caseCastWorker (TransformContext is0 _) _node (stripTicks -> Case subj _ alts) ty1 ty2 = do
  -- The cast moves under the binders of the patterns, so these must not
  -- capture the free type variables of its types.
  let alts' = map ((\(p,e) -> (p, Cast e ty1 ty2)) . deShadowAlt is0) alts
  changed (Case subj ty2 alts')
caseCastWorker _ctx node _body _fromType _toType = return node
{-# SCC caseCastWorker #-}

-- | Eliminate two back to back casts where the type going in and coming out are the same
--
-- @
--   (cast :: b -> a) $ (cast :: a -> b) x   ==> x
-- @
elimCastCast :: Transformation
elimCastCast = Scoped.elimCastCast

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
    (nm,sp) <- Lens.use curFun
    throw (ClashException sp ($(curLoc) ++ showPpr nm
            ++ ": Found 2 nested casts whose types don't line up:\n"
            ++ showPpr node)
          Nothing)
elimCastCastWorker _ctx node _body _fromType _toType = return node
{-# SCC elimCastCastWorker #-}

-- | Push a cast over a Let into it's body
letCast :: Transformation
letCast = Scoped.letCast

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
--
-- This is the port 'Scoped.splitCastWork', which only converts the
-- let-expressions at which it can fire, see 'splitCastWorkViaFoil'.
splitCastWork :: Transformation
splitCastWork = toTransformation "splitCastWork" (onLet 'splitCastWorkViaFoil)

-- | The 'Let' handler of 'splitCastWork': 'Scoped.splitCastWorkWorker' at the
-- let-expressions with a binding that it splits.
--
-- The strategy compiler runs a scoped transformation through 'viaFoil' at
-- every node it is offered at, which converts the whole subterm to scoped
-- Core: for 'splitCastWork', every let-expression. This goes away together
-- with 'viaFoil'.
splitCastWorkViaFoil
  :: HasCallStack
  => TransformContext -> Term -> Bind Term -> Term -> NormalizeSession Term
splitCastWorkViaFoil ctx node bind _ =
  viaFoilWhen "splitCastWork" (\_ _ -> pure (any (splits . snd) (bindToList bind)))
    (ScopedTransform splitCastWorkLet) ctx node
 where
  splits e = case stripTicks e of
    Cast (Var {}) _ _ -> False
    Cast (Cast {}) _ _ -> False
    Cast {} -> True
    _ -> False

-- | 'Scoped.splitCastWorkWorker' at a let-expression
splitCastWorkLet
  :: (HasCallStack, S.Distinct n)
  => Scoped.TransformContext n -> S.Term n -> NormalizeSession (S.Term n)
splitCastWorkLet ctx e = case e of
  S.Let bind body -> Scoped.splitCastWorkWorker ctx e bind body
  _ -> return e

-- | The 'Let' handler of 'splitCastWork'.
splitCastWorkWorker
  :: HasCallStack
  => TransformContext -> Term -> Bind Term -> Term -> NormalizeSession Term
splitCastWorkWorker ctx@(TransformContext is0 _) node bind body = do
  (vss', Monoid.getAny -> hasChanged) <-
    listen (snd <$> List.mapAccumLM splitCastLetBinding is1 (bindToList bind))
  let vs' = concat vss'
  if hasChanged then changed (rebuild vs')
                else return node
 where
  -- The binding keeps its form: rebuilding a 'NonRec' as a 'Rec' would bring
  -- its binder into scope of its own right-hand side.
  rebuild vs' = case bind of
    NonRec {} -> foldr (\(i, x) -> Let (NonRec i x)) body vs'
    Rec {} -> Let (Rec vs') body

  splitCastLetBinding
    :: InScopeSet
    -> LetBinding
    -> NormalizeSession (InScopeSet, [LetBinding])
  splitCastLetBinding isN x@(nm, e) = case stripTicks e of
    Cast (Var {}) _ _  -> return (isN, [x])  -- already work-free
    Cast (Cast {}) _ _ -> return (isN, [x])  -- casts will be eliminated
    Cast e0 ty1 ty2 -> do
      tcm <- Lens.view tcCache
      nm' <- mkTmBinderFor isN tcm (mkDerivedName ctx (nameOcc $ varName nm)) e0
      changed ( extendInScopeSet isN nm'
              , [(nm',e0)
                ,(nm, Cast (Var nm') ty1 ty2)
                ] )
    _ -> return (isN, [x])

  -- The new binders end up next to the let's binders, so they have to be
  -- fresh with respect to them, and to each other.
  is1 = extendInScopeSetList is0 (map fst (bindToList bind))
{-# SCC splitCastWorkWorker #-}
