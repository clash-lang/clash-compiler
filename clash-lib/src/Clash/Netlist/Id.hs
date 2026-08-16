{-|
  Copyright  :  (C) 2020, QBayLogic B.V.
                    2022, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transform/format a Netlist Identifier so that it is acceptable as a HDL identifier
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Netlist.Id
  ( -- * Utilities to use IdentifierSet
    IdentifierSet
  , emptyIdentifierSet
  , makeSet
  , clearSet

    -- * Utilities to use IdentifierScopes
  , Scope(..)
  , IdentifierScopes(..)
  , IdentifierScopesMonad(..)
  , HasIdentifierScopes(..)
  , globalIds
  , localIds
  , emptyIdentifierScopes
  , fromGlobalSet
  , setLocalScope

    -- * Unsafe creation and extracting identifiers
  , Identifier
  , IdentifierType (..)
  , unsafeMake
  , unsafeFromCoreId
  , toText
  , toLazyText
  , toList
  , union

    -- * Creating and extending identifiers
  , make
  , makeBasic
  , makeBasicOr
  , makeAs
  , add
  , addMultiple
  , addRaw
  , deepen
  , deepenN
  , next
  , nextN
  , prefix
  , suffix
  , fromCoreId

  -- * Misc. and internals
  , VHDL.stripDollarPrefixes
  , toBasicId#
  , isBasic#
  , isExtended#
  )
where

import           Clash.Annotations.Primitive (HDL (..))
import           Clash.Core.Name (nameOcc)
import           Clash.Core.Var (Id, varName)
import           Clash.Debug (debugIsOn)
import           Clash.Netlist.Types
  (PreserveCase(..), IdentifierSet(..), Identifier(..), IdentifierType(..),
   HasIdentifierScopes(..), IdentifierScopes(..), Scope(..),
   IdentifierScopesMonad(identifierScopesM), globalIds, localIds)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import           GHC.Stack

import qualified Clash.Netlist.Id.VHDL as VHDL
import           Clash.Netlist.Id.Internal

-- | Identifier set without identifiers
emptyIdentifierSet
  :: Bool
  -- ^ Allow escaped identifiers?
  -> PreserveCase
  -- ^ Should all basic identifiers be lower case?
  -> HDL
  -- ^ HDL to generate names for
  -> IdentifierSet
emptyIdentifierSet esc lw hdl = makeSet esc lw hdl mempty

-- | Union of two identifier sets. Errors if given sets have been made with
-- different options enabled.
union :: HasCallStack => IdentifierSet -> IdentifierSet -> IdentifierSet
union (IdentifierSet escL lwL hdlL freshL idsL) (IdentifierSet escR lwR hdlR freshR idsR)
  | escL /= escR = error $ "Internal error: escL /= escR, " <> show (escL, escR)
  | hdlL /= hdlR = error $ "Internal error: hdlL /= hdlR, " <> show (hdlL, hdlR)
  | lwL /= lwR = error $ "Internal error: lwL /= lwR , " <> show (lwL, lwR)
  | otherwise = IdentifierSet escR lwR hdlR fresh ids
 where
  fresh = HashMap.unionWith (IntMap.unionWith max) freshL freshR
  ids = HashSet.union idsL idsR

-- | Make a identifier set filled with given identifiers
makeSet
  :: Bool
  -- ^ Allow escaped identifiers?
  -> PreserveCase
  -- ^ Should all basic identifiers be lower case?
  -> HDL
  -- ^ HDL to generate names for
  -> HashSet.HashSet Identifier
  -- ^ Identifiers to add to set
  -> IdentifierSet
makeSet esc lw hdl ids = IdentifierSet esc lw hdl fresh ids
 where
  fresh = List.foldl' updateFreshCache# mempty ids

-- | Remove all identifiers from a set
clearSet :: IdentifierSet -> IdentifierSet
clearSet (IdentifierSet escL lwL hdlL _ _) =
  IdentifierSet escL lwL hdlL mempty mempty

-- | Identifier scopes without identifiers
emptyIdentifierScopes
  :: Bool
  -- ^ Allow escaped identifiers?
  -> PreserveCase
  -- ^ Should all basic identifiers be lower case?
  -> HDL
  -- ^ HDL to generate names for
  -> IdentifierScopes
emptyIdentifierScopes esc lw hdl = IdentifierScopes is is
 where
  is = emptyIdentifierSet esc lw hdl

-- | Identifier scopes with the given set as global scope and an empty local
-- scope. Used to run 'Global' name generation over a bare set of design-wide
-- names, such as the component name set threaded through the driver.
fromGlobalSet :: IdentifierSet -> IdentifierScopes
fromGlobalSet is = IdentifierScopes is (clearSet is)

-- | Start a fresh local scope: the local set is replaced by the given
-- identifiers. Names only present in the previous local scope go out of
-- scope.
setLocalScope :: HasCallStack => IdentifierSet -> IdentifierScopes -> IdentifierScopes
setLocalScope seed (IdentifierScopes glob _) =
  -- 'union' with an emptied copy of the global set checks that the seed was
  -- created with the same options.
  IdentifierScopes glob (union (clearSet glob) seed)

toList :: IdentifierSet -> [Identifier]
toList (IdentifierSet _ _ _ _ idStore) = HashSet.toList idStore

-- | Convert an identifier to string
toText :: Identifier -> Text
toText = toText#

-- | Convert an identifier to string
toLazyText :: Identifier -> LT.Text
toLazyText = LT.fromStrict . toText

-- | Helper function to define pure Id functions in terms of a IdentifierScopesMonad
withScopesM'
  :: IdentifierScopesMonad m
  => (IdentifierScopes -> a -> IdentifierScopes)
  -> a
  -> m ()
withScopesM' f a = do
  is0 <- identifierScopesM id
  identifierScopesM (const (f is0 a)) >> pure ()

-- | Helper function to define pure Id functions in terms of a IdentifierScopesMonad
withScopesM
  :: IdentifierScopesMonad m
  => (IdentifierScopes -> a -> (IdentifierScopes, b))
  -> a
  -> m b
withScopesM f a = do
  is0 <- identifierScopesM id
  let (is1, b) = f is0 a
  _ <- identifierScopesM (const is1)
  pure b

-- | Like 'addRaw', 'unsafeMake' creates an identifier that will be spliced
-- at verbatim in the HDL. As opposed to 'addRaw', the resulting Identifier
-- might be generated at a later point as it is NOT added to an IdentifierSet.
unsafeMake :: HasCallStack => Text -> Identifier
unsafeMake t =
  RawIdentifier t Nothing (if debugIsOn then callStack else emptyCallStack)

-- | Add an identifier to the set given by the scope
add :: HasCallStack => IdentifierScopesMonad m => Scope -> Identifier -> m ()
add scope = withScopesM' (add# scope)

-- | Add identifiers to the set given by the scope
addMultiple
  :: (HasCallStack, IdentifierScopesMonad m, Foldable t)
  => Scope -> t Identifier -> m ()
addMultiple scope = withScopesM' (addMultiple# scope)

-- | Add a string as is to the set given by the scope. Should only be used for
-- identifiers that should be spliced at verbatim in HDL, such as port names.
-- It's sanitized version will still be added to the identifier set, to
-- prevent freshly generated variables clashing with the raw one.
addRaw :: (HasCallStack, IdentifierScopesMonad m) => Scope -> Text -> m Identifier
addRaw scope = withScopesM (addRaw# scope)

-- | Make unique identifier based on given string, and register it in the set
-- given by the scope
make :: (HasCallStack, IdentifierScopesMonad m) => Scope -> Text -> m Identifier
make scope = withScopesM (make# scope)

-- | Make unique basic identifier based on given string, and register it in
-- the set given by the scope
makeBasic :: (HasCallStack, IdentifierScopesMonad m) => Scope -> Text -> m Identifier
makeBasic scope = withScopesM (makeBasic# scope)

-- | Make unique basic identifier based on given string. If given string can't
-- be converted to a basic identifier (i.e., it would yield an empty string) the
-- alternative name is used.
makeBasicOr
  :: (HasCallStack, IdentifierScopesMonad m)
  => Scope
  -> Text
  -- ^ Name hint
  -> Text
  -- ^ If name hint can't be converted to a sensible basic id, use this instead
  -> m Identifier
makeBasicOr scope hint altHint =
  withScopesM
    (\is0 -> uncurry (makeBasicOr# scope is0))
    (hint, altHint)

-- | Make unique identifier. Uses 'makeBasic' if second argument is 'Basic'
makeAs
  :: (HasCallStack, IdentifierScopesMonad m)
  => Scope -> IdentifierType -> Text -> m Identifier
makeAs scope Basic = makeBasic scope
makeAs scope Extended = make scope

-- | Given identifier "foo_1_2" return "foo_1_3". If "foo_1_3" is already a
-- member of either scope, return "foo_1_4" instead, etc. Identifier returned
-- is guaranteed to be unique.
next :: (HasCallStack, IdentifierScopesMonad m) => Scope -> Identifier -> m Identifier
next scope = withScopesM (next# scope)

-- | Same as 'next', but returns N fresh identifiers
nextN :: (HasCallStack, IdentifierScopesMonad m) => Scope -> Int -> Identifier -> m [Identifier]
nextN scope n = withScopesM (nextN# n scope)

-- | Given identifier "foo_1_2" return "foo_1_2_0". If "foo_1_2_0" is already a
-- member of either scope, return "foo_1_2_1" instead, etc. Identifier returned
-- is guaranteed to be unique.
deepen :: (HasCallStack, IdentifierScopesMonad m) => Scope -> Identifier -> m Identifier
deepen scope = withScopesM (deepen# scope)

-- | Same as 'deepen', but returns N fresh identifiers. For example, given
-- "foo_23" is would return "foo_23_0", "foo_23_1", ...
deepenN :: (HasCallStack, IdentifierScopesMonad m) => Scope -> Int -> Identifier -> m [Identifier]
deepenN scope n = withScopesM (deepenN# n scope)

-- | Given identifier "foo_1_2" and a suffix "bar", return an identifier called
-- "foo_bar". Identifier returned is guaranteed to be unique according to the
-- rules of 'next'.
suffix :: (HasCallStack, IdentifierScopesMonad m) => Scope -> Identifier -> Text -> m Identifier
suffix scope id0 suffix_ = withScopesM (\is id1 -> suffix# scope is id1 suffix_) id0

-- | Given identifier "foo_1_2" and a prefix "bar", return an identifier called
-- "bar_foo". Identifier returned is guaranteed to be unique according to the
-- rules of 'next'.
prefix :: (HasCallStack, IdentifierScopesMonad m) => Scope -> Identifier -> Text -> m Identifier
prefix scope id0 prefix_ = withScopesM (\is id1 -> prefix# scope is id1 prefix_) id0

-- | Convert a Clash Core Id to an identifier. Makes sure returned identifier
-- is unique.
fromCoreId :: (HasCallStack, IdentifierScopesMonad m) => Scope -> Id -> m Identifier
fromCoreId scope = withScopesM (fromCoreId# scope)

-- | Like 'fromCoreId, 'unsafeFromCoreId' creates an identifier that will be
-- spliced at verbatim in the HDL. As opposed to 'fromCoreId', the resulting
-- Identifier might be generated at a later point as it is NOT added to an
-- IdentifierSet.
unsafeFromCoreId :: HasCallStack => Id -> Identifier
unsafeFromCoreId = unsafeMake . nameOcc . varName
