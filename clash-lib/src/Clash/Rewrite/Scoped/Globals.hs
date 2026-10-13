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
  variable. Note that a 'GlobalId' doesn't imply that there is a binding for
  it: e.g. 'Clash.Rewrite.Types.curFun' is a global without a binding during
  'Clash.Normalize.Scoped.Util.normalizeWorkFreeApp'.
-}

{-# LANGUAGE PolyKinds #-}
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

import           Control.Lens                ((%=))
import qualified Control.Lens                as Lens
import           Data.List                   (find)
import           GHC.Stack                   (HasCallStack)
import           GHC.Types.Basic             (InlineSpec)
import           GHC.Types.SrcLoc            (SrcSpan)

import qualified Clash.Core.Name             as C
import           Clash.Driver.Types (Binding (..), IsPrim (IsFun))
import           Clash.Rewrite.Types
  ( RewriteMonad, scopedBindings, scopedTypeEnv, scopedWorkFreeBinders,
    topEntities )
import           Clash.Unique                (Unique, getUnique, setUnique)
import           Clash.Util                  (MonadUnique (..))

import           Clash.Core.Scoped.Alpha     (aeqTm)
import           Clash.Core.Scoped.FreeVars  (termGlobalIds)
import           Clash.Core.Scoped.HasType   (inferCoreTypeOf)
import           Clash.Core.Scoped.NameMap
import           Clash.Core.Scoped.Syntax
import qualified Clash.Rewrite.Scoped.WorkFree as WorkFree

-- | The binding of a global, as a closed term
lookupBinding :: GlobalId -> RewriteMonad extra (Maybe (Binding (Term 'VoidS)))
lookupBinding g = lookupGlobalEnv g <$> Lens.use scopedBindings

-- | A new global variable with the given name and closed type. It gets a
-- unique from the unique supply that no global binding has.
freshGlobalId :: C.Name Term -> Type 'VoidS -> RewriteMonad extra GlobalId
freshGlobalId nm ty = do
  bndrs <- Lens.use scopedBindings
  u <- getUniqueM
  let global nm' = mkGlobalId nm' ty
      taken u' = memberGlobalEnv (global (setUnique nm u')) bndrs
  pure (global (uniqAway taken u (setUnique nm u)))

-- | Change the unique of a name, @u@, to one for which the given test is
-- false. It tries @u@, then @u + seed@, @u + 2 * seed@, and so on.
uniqAway :: (Unique -> Bool) -> Unique -> C.Name a -> C.Name a
uniqAway taken seed nm = go 0
 where
  u0 = getUnique nm
  go k
    | taken u = go (k + 1)
    | otherwise = setUnique nm u
   where
    u = u0 + seed * k

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
  => C.Name Term
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
isTopEntity g = memberGlobalSet g <$> Lens.view topEntities

-- | Whether a term is work free, see 'Clash.Rewrite.Scoped.WorkFree.isWorkFree'.
-- Caches the work-freeness of global binders in the rewrite state.
isWorkFreeTerm :: HasCallStack => Term n -> RewriteMonad extra Bool
isWorkFreeTerm e = do
  bndrs <- Lens.use scopedBindings
  WorkFree.isWorkFree scopedWorkFreeBinders bndrs e
