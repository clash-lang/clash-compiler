{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The global bindings of the rewrite state, for scoped Clash Core.

  During the port, the binding map of the rewrite state
  ('Clash.Rewrite.Types.bindings') holds Clash Core, while scoped
  transformations read and create global bindings as closed scoped terms
  (@'Term' ''VoidS'@). This module is the boundary between the two: it
  converts closed terms when they are read from or written to the binding
  map. Closed terms convert cheaply, and 'sinkClosed' moves them into any
  scope for free. Phase 4 of the port, which switches the binding map to
  scoped Core, only changes this module.

  Globals are looked up by 'GlobalId', so a local variable never finds the
  binding of a global with the same unique. New globals get a unique from the
  unique supply that no global binding has, never one derived from a local
  variable (decision D5 of the port). Note that a 'GlobalId' doesn't imply
  that there is a binding for it: e.g. 'Clash.Rewrite.Types.curFun' is a
  global without a binding during 'Clash.Normalize.Util.normalizeWorkFreeApp'.
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
  ) where

import           Control.DeepSeq             (deepseq)
import           Control.Lens                ((%=))
import qualified Control.Lens                as Lens
import           Data.Maybe                  (listToMaybe)
import           GHC.Stack                   (HasCallStack)
import           GHC.Types.Basic             (InlineSpec)
import           GHC.Types.SrcLoc            (SrcSpan)

import qualified Clash.Core.Name             as C
import           Clash.Core.Subst            (aeqTerm)
import qualified Clash.Core.Term             as C
import qualified Clash.Core.Var              as C
import           Clash.Core.VarEnv           (elemVarSet)
import qualified Clash.Data.UniqMap          as UniqMap
import           Clash.Driver.Types          (Binding (..), IsPrim (IsFun))
import           Clash.Rewrite.Types
  (RewriteMonad, bindings, scopedTypeEnv, topEntities, workFreeBinders)
import           Clash.Rewrite.Util          (cloneNameWithBindingMap)
import qualified Clash.Rewrite.WorkFree      as C

import           Clash.Core.Scoped.Convert   (fromClashTermClosed, toClashTerm, toClashType)
import           Clash.Core.Scoped.FreeVars  (termGlobalIds)
import           Clash.Core.Scoped.HasType   (inferCoreTypeOf)
import           Clash.Core.Scoped.NameMap   (memberGlobalSet)
import           Clash.Core.Scoped.Syntax
import           Clash.Rewrite.Scoped.ViaFoil (toClashTermFresh)

-- | The binding of a global, as a closed term
lookupBinding :: HasCallStack => GlobalId -> RewriteMonad extra (Maybe (Binding (Term 'VoidS)))
lookupBinding g =
  fmap (fmap fromClashTermClosed) . UniqMap.lookup (globalIdVar g) <$> Lens.use bindings

-- | A new global variable with the given name and closed type. It gets a
-- unique from the unique supply that no global binding has, see
-- 'Clash.Rewrite.Util.cloneNameWithBindingMap'.
freshGlobalId :: C.Name C.Term -> Type 'VoidS -> RewriteMonad extra GlobalId
freshGlobalId nm ty = do
  bndrs <- Lens.use bindings
  nm' <- cloneNameWithBindingMap bndrs nm
  pure (mkGlobalId (C.mkGlobalId (toClashType ty) nm'))

-- | Add a global binding, see 'Clash.Rewrite.Util.addGlobalBind'
addGlobalBind
  :: HasCallStack
  => GlobalId
  -> SrcSpan
  -> InlineSpec
  -> Term 'VoidS
  -> RewriteMonad extra ()
addGlobalBind g sp inl body = do
  body' <- toClashTermFresh body
  let v = globalIdVar g
      r = g `memberGlobalSet` termGlobalIds body
  body' `deepseq` bindings %= UniqMap.insert v (Binding v sp inl IsFun body' r)

-- | Make a global function of a closed term, and give its variable. See
-- 'Clash.Rewrite.Util.mkFunction'.
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
-- term, if there is one
findAlphaEqBinding :: HasCallStack => Term 'VoidS -> RewriteMonad extra (Maybe GlobalId)
findAlphaEqBinding body = do
  bndrs <- Lens.use bindings
  let body' = toClashTerm body
  pure (mkGlobalId . bindingId <$>
          listToMaybe (UniqMap.elems (UniqMap.filter ((`aeqTerm` body') . bindingTerm) bndrs)))

-- | Whether a global is a top entity
isTopEntity :: GlobalId -> RewriteMonad extra Bool
isTopEntity g = (globalIdVar g `elemVarSet`) <$> Lens.view topEntities

-- | Whether a term is work free, see 'Clash.Rewrite.WorkFree.isWorkFree'.
-- Caches the work-freeness of global binders in the rewrite state.
--
-- This converts the term, rather than the bindings of the globals it refers
-- to, as 'Clash.Rewrite.Scoped.WorkFree.isWorkFree' needs all of them.
isWorkFreeTerm :: HasCallStack => Term n -> RewriteMonad extra Bool
isWorkFreeTerm e = do
  bndrs <- Lens.use bindings
  C.isWorkFree workFreeBinders bndrs (toClashTerm e)
