{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2022, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for inlining on scoped Clash Core: the ported counterparts
  of "Clash.Normalize.Transformations.Inline".

  = Globals

  Only global variables ('GlobalVar') are inlined, looked up in the global
  bindings ('Clash.Rewrite.Scoped.Globals.lookupBinding'), counted in the
  inline history, and taken to be no-ops by 'collapseRHSNoops'. A local
  variable with the unique of a global binder is a 'Var', which never matches
  (#405). The body of a global binding is a closed term, which is moved into
  the scope of the inlined occurrence for free ('sinkClosed'). Its binders get
  fresh names when a traversal goes under them.

  The function being normalized ('Clash.Rewrite.Types.curFun') is a global,
  which need not have a binding: e.g. the fake global of
  'Clash.Normalize.Scoped.Util.normalizeWorkFreeApp' doesn't.

  = Let-bindings

  The transformations on let-expressions work on its bindings in the scope of
  the body ('Clash.Rewrite.Scoped.Util.openLetBindings'), so a right-hand side
  of a non-recursive binding can't refer to its own binder: unlike in Clash
  Core, where such a reference is to a shadowed variable with the same
  unique.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Normalize.Scoped.Transformations.Inline
  ( bindConstantVar
  , bindConstantVarWorker
  , inlineCast
  , inlineCastWorker
  , inlineOrLiftNonRep
  , inlineOrLiftNonRepWorker
  , inlineSimIO
  , inlineSimIOWorker
  ) where

import qualified Control.Lens as Lens
import qualified Data.Text.Extra as Text
import GHC.Stack (HasCallStack)

import qualified Clash.Explicit.SimIO as SimIO

import qualified Clash.Core.Name as C
import Clash.Rewrite.StrategyDSL (Transformation, onLet, toScopedTransformation)
import Clash.Rewrite.Types (curFun, inlineConstantLimit, scopedTypeEnv)
import Clash.Normalize.Types (NormalizeSession)

import Clash.Core.Scoped.FreeVars (freeTmVarOccurrences, tmNameOccursIn)
import Clash.Core.Scoped.NameMap (lookupNameMap)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (stripTicks)
import Clash.Core.Scoped.TermInfo (isLocalVar, termSizeSmallerThan)
import Clash.Core.Scoped.Type (TypeView (..), tyView)
import Clash.Rewrite.Scoped (TransformContext)
import Clash.Rewrite.Scoped.Util
  ( LetBinding (..), inlineBinders, inlineOrLiftBinders, isJoinPointIn
  , isUntranslatableType, isVoidWrapper )
import Clash.Rewrite.Scoped.WorkFree (isWorkFreeIsh)

-- | The function being normalized, see 'Clash.Rewrite.Types.curFun'. It is
-- always a global, but need not have a binding.
curFunGlobal :: NormalizeSession GlobalId
curFunGlobal = mkGlobalId . fst <$> Lens.use curFun

-- | Inline let-bindings when the RHS is either a local variable reference or
-- is constant (except clock or reset generators). See
-- 'Clash.Normalize.Transformations.Inline.bindConstantVar'.
bindConstantVar :: Transformation
bindConstantVar =
  toScopedTransformation "bindConstantVar" (onLet 'bindConstantVarWorker)

-- | The 'Let' handler of 'bindConstantVar'
bindConstantVarWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n)
bindConstantVarWorker = inlineBinders test
 where
  test :: Distinct o => Scope o -> LetBinding o -> NormalizeSession Bool
  test sc (LetBinding {lbName, lbRhs = stripTicks -> e}) = case isLocalVar e of
    -- Don't inline `let x = x in x`, it throws  us in an infinite loop
    True -> return (not (lbName `tmNameOccursIn` e))
    _ -> do
      cf <- curFunGlobal
      -- Don't inline globally recursive calls, it prevents the recToLetRec
      -- transformation from transforming global recursion to local recursion.
      -- See https://github.com/clash-lang/clash-compiler/issues/2839
      case e of
        GlobalVar g | g == cf -> return False
        _ -> do
          te <- Lens.view scopedTypeEnv
          -- Don't inline things that perform work, it increases the circuit
          -- size.
          case isWorkFreeIsh te sc e of
            True -> Lens.view inlineConstantLimit >>= \case
              0 -> return True
              n -> return (termSizeSmallerThan (n + 1) e)
            _ -> return False
{-# SCC bindConstantVarWorker #-}

-- | Only inline casts that just contain a variable, because these are
-- guaranteed work-free. These are the result of the 'splitCastWork'
-- transformation. See 'Clash.Normalize.Transformations.Inline.inlineCast'.
inlineCast :: Transformation
inlineCast = toScopedTransformation "inlineCast" (onLet 'inlineCastWorker)

-- | The 'Let' handler of 'inlineCast'
inlineCastWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n)
inlineCastWorker = inlineBinders test
 where
  test :: Scope o -> LetBinding o -> NormalizeSession Bool
  test _ lb = return $ case lbRhs lb of
    Cast (stripTicks -> Var {}) _ _ -> True
    Cast (stripTicks -> GlobalVar {}) _ _ -> True
    _ -> False
{-# SCC inlineCastWorker #-}

-- | Inline anything of type `SimIO`: IO actions cannot be shared. See
-- 'Clash.Normalize.Transformations.Inline.inlineSimIO'.
inlineSimIO :: Transformation
inlineSimIO = toScopedTransformation "bindSimIO" (onLet 'inlineSimIOWorker)

-- | The 'Let' handler of 'inlineSimIO'
inlineSimIOWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n)
inlineSimIOWorker = inlineBinders test
 where
  test :: Scope o -> LetBinding o -> NormalizeSession Bool
  test _ lb = case tyView (lbType lb) of
    TyConApp tc _ -> return $! C.nameOcc tc == Text.showt ''SimIO.SimIO
    _ -> return False
{-# SCC inlineSimIOWorker #-}

-- | Inline or lift let-bindings of a non-representable type. See
-- 'Clash.Normalize.Transformations.Inline.inlineOrLiftNonRep'.
inlineOrLiftNonRep :: Transformation
inlineOrLiftNonRep =
  toScopedTransformation "bindOrLiftNonRep" (onLet 'inlineOrLiftNonRepWorker)

-- | The 'Let' handler of 'inlineOrLiftNonRep'
inlineOrLiftNonRepWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n)
inlineOrLiftNonRepWorker = inlineOrLiftBinders nonRepTest inlineTest
 where
  nonRepTest :: Scope o -> LetBinding o -> NormalizeSession Bool
  nonRepTest _ lb = isUntranslatableType False (lbType lb)

  inlineTest :: [LetBinding o] -> Term o -> LetBinding o -> Bool
  inlineTest lbs body = \lb ->
    -- We do __NOT__ inline:
    not $ or
      [ -- 1. recursive let-binders
        -- (already checked in inlineOrLiftBinders)
        -- 2. join points (which are not void-wrappers)
        isJoinPointIn (lbName lb) lbs body && not (isVoidWrapper (lbRhs lb))
        -- 3. binders that are used more than once in the body, because
        --    it makes CSE a whole lot more difficult.
        --
        -- XXX: Check whether we can extend this to the binders as well
      , maybe False (> 1) (lookupNameMap (lbName lb) bodyFreeOccs)
      ]
   where
    bodyFreeOccs = freeTmVarOccurrences body
{-# SCC inlineOrLiftNonRepWorker #-}
