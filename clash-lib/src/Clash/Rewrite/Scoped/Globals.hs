{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The global bindings of the rewrite state, for scoped Clash Core: closed
  terms (@'Term' ''VoidS'@) in 'Clash.Rewrite.Types.scopedBindings'. They
  move into any scope for free, with 'sinkClosed'.

  Globals are looked up by 'GlobalId', so a local variable never finds the
  binding of a global with the same unique. New globals get a unique from the
  unique supply that no global binding has, never one derived from a local
  variable (decision D5 of the port). Note that a 'GlobalId' doesn't imply
  that there is a binding for it: e.g. 'Clash.Rewrite.Types.curFun' is a
  global without a binding during
  'Clash.Normalize.Scoped.Util.normalizeWorkFreeApp'.

  = The boundary with Clash Core

  The pipeline converts no bindings: GHC2Core produces scoped Clash Core, and
  Netlist generation consumes it. 'fromClashBindings' and 'toClashBindings'
  convert bindings of Clash Core, e.g. for the comparisons of
  @clash-differential@, and those of tests. TODO(Phase 7): remove, with Clash
  Core.
-}

{-# LANGUAGE DataKinds #-}

module Clash.Rewrite.Scoped.Globals
  ( lookupBinding
  , freshGlobalId
  , addGlobalBind
  , mkFunction
  , findAlphaEqBinding
  , isTopEntity
  , isWorkFreeTerm
    -- * The boundary with Clash Core
  , fromClashBindings
  , toClashBindings
  , toClashTermFresh
  ) where

import           Control.Lens                ((%=))
import qualified Control.Lens                as Lens
import qualified Control.Monad.State.Strict  as State
import           Data.List                   (find)
import           GHC.Stack                   (HasCallStack)
import           GHC.Types.Basic             (InlineSpec)
import           GHC.Types.SrcLoc            (SrcSpan)

import qualified Clash.Core.Name             as C
import qualified Clash.Core.Term             as C
import           Clash.Core.VarEnv           (elemVarSet, uniqAway')
import qualified Clash.Data.UniqMap          as UniqMap
import           Clash.Driver.Types          (Binding (..), BindingMap, IsPrim (IsFun))
import           Clash.Rewrite.Types
  (RewriteMonad, scopedBindings, scopedTypeEnv, scopedWorkFreeBinders, topEntities,
   uniqSupply)
import           Clash.Unique                (setUnique)
import           Clash.Util                  (MonadUnique (..))
import           Clash.Util.Supply           (Supply, splitSupply)

import           Clash.Core.Scoped.Alpha     (aeqTm)
import           Clash.Core.Scoped.Convert   (fromClashTermClosed, mkGlobalId)
import           Clash.Core.Scoped.FreeVars  (termGlobalIds)
import           Clash.Core.Scoped.HasType   (inferCoreTypeOf)
import           Clash.Core.Scoped.NameMap
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Convert.Fresh (toClashTermFreshClosed)
import qualified Clash.Rewrite.Scoped.WorkFree as WorkFree

-- | The binding of a global, as a closed term
lookupBinding :: GlobalId -> RewriteMonad extra (Maybe (Binding (Term 'VoidS)))
lookupBinding g = lookupGlobalEnv g <$> Lens.use scopedBindings

-- | A new global variable with the given name and closed type. It gets a
-- unique from the unique supply that no global binding has.
freshGlobalId :: C.Name C.Term -> Type 'VoidS -> RewriteMonad extra GlobalId
freshGlobalId nm ty = do
  bndrs <- Lens.use scopedBindings
  u <- getUniqueM
  let global nm' = mkGlobalId nm' ty
      taken u' = memberGlobalEnv (global (setUnique nm u')) bndrs
  pure (global (uniqAway' taken u (setUnique nm u)))

-- | Add a function to the global bindings. It is marked recursive when its
-- body refers to the function itself.
addGlobalBind
  :: GlobalId
  -> SrcSpan
  -> InlineSpec
  -> Term 'VoidS
  -> RewriteMonad extra ()
addGlobalBind g sp inl body =
  scopedBindings %= insertGlobalEnv g (Binding g sp inl IsFun body r)
 where
  r = g `memberGlobalSet` termGlobalIds body

-- | Make a global function of a closed term, and give its variable: a
-- global variable with the given name, a fresh unique, and the type of the
-- term.
mkFunction
  :: HasCallStack
  => C.Name C.Term
  -- ^ Name of the function. Its unique is replaced by a fresh one.
  -> SrcSpan
  -> InlineSpec
  -> Term 'VoidS
  -> RewriteMonad extra GlobalId
mkFunction nm sp inl body = do
  te <- Lens.view scopedTypeEnv
  g <- freshGlobalId nm (inferCoreTypeOf te emptyScope body)
  addGlobalBind g sp inl body
  pure g

-- | A global binding whose body is alpha-equivalent to the given closed
-- term, if there is one. The first in the order of the uniques of the
-- globals.
findAlphaEqBinding :: Term 'VoidS -> RewriteMonad extra (Maybe GlobalId)
findAlphaEqBinding body = do
  bndrs <- Lens.use scopedBindings
  pure (fst <$> find (aeqTm body . bindingTerm . snd) (globalEnvToList bndrs))

-- | Whether a global is a top entity
isTopEntity :: GlobalId -> RewriteMonad extra Bool
isTopEntity g = (globalIdLegacy g `elemVarSet`) <$> Lens.view topEntities

-- | Whether a term is work free, see 'Clash.Rewrite.Scoped.WorkFree.isWorkFree'.
-- Caches the work-freeness of global binders in the rewrite state.
isWorkFreeTerm :: HasCallStack => Term n -> RewriteMonad extra Bool
isWorkFreeTerm e = do
  bndrs <- Lens.use scopedBindings
  WorkFree.isWorkFree scopedWorkFreeBinders bndrs e

-- * The boundary with Clash Core

-- | The global bindings of Clash Core as closed terms of scoped Clash Core.
-- A body is converted when it is first used: most bindings that GHC2Core
-- produces are never looked at.
fromClashBindings :: HasCallStack => BindingMap -> GlobalEnv (Binding (Term 'VoidS))
fromClashBindings bm =
  globalEnvFromList
    [ (bindingId b, fromClashTermClosed <$> b) | b <- UniqMap.elems bm ]

-- | Global bindings of scoped Clash Core as bindings of Clash Core. Every
-- binder gets a fresh unique from the supply, so that a local binder never
-- shares its unique with a global one, see
-- "Clash.Core.Scoped.Convert.Fresh".
toClashBindings :: HasCallStack => Supply -> GlobalEnv (Binding (Term 'VoidS)) -> BindingMap
toClashBindings sup bndrs =
  UniqMap.fromList [ (globalIdLegacy g, b') | (g, b') <- State.evalState (mapM conv (globalEnvToList bndrs)) sup ]
 where
  conv (g, b) = do
    tm <- toClashTermFreshClosed (bindingTerm b)
    pure (g, b {bindingTerm = tm})

-- | A closed term as a term of Clash Core whose binders get fresh uniques
-- from the unique supply of the rewrite state, see "Clash.Core.Scoped.Convert.Fresh"
toClashTermFresh :: HasCallStack => Term 'VoidS -> RewriteMonad extra C.Term
toClashTermFresh e = State.evalState (toClashTermFreshClosed e) <$> splitUniqSupply

-- | A split of the unique supply of the rewrite state
splitUniqSupply :: RewriteMonad extra Supply
splitUniqSupply = do
  sup <- Lens.use uniqSupply
  let (sup1, sup2) = splitSupply sup
  uniqSupply Lens..= sup2
  pure sup1
