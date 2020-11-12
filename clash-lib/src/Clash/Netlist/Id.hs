{-|
  Copyright  :  (C) 2020, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transform/format a Netlist Identifier so that it is acceptable as a HDL identifier
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Netlist.Id
  ( -- * Utilities to use IdentifierSet
    IdentifierSet
  , IdentifierSetMonad(..)
  , HasIdentifierSet(..)
  , emptyIdentifierSet
  , makeSet

    -- * Unsafe creation and extracting identifiers
  , Identifier
  , IdentifierType (..)
  , unsafeMake
  , toText
  , toLazyText
  , toList
  , union

    -- * Creating and extending identifiers
  , make
  , makeBasic
  , makeBasicOr
  , makeAs
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
import           Clash.Core.Var (Id)
import           Clash.Debug (debugIsOn)
import {-# SOURCE #-} Clash.Netlist.Types
  (PreserveCase(..), HasIdentifierSet(..), IdentifierSet(..), Identifier(..),
   IdentifierType(..), IdentifierSetMonad(identifierSetM))
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

toList :: IdentifierSet -> [Identifier]
toList (IdentifierSet _ _ _ _ idStore) = HashSet.toList idStore

-- | Convert an identifier to string. Use 'unmake' if you need the
-- "IdentifierType" too.
toText :: Identifier -> Text
toText = toText#

-- | Convert an identifier to string. Use 'unmake' if you need the
-- "IdentifierType" too.
toLazyText :: Identifier -> LT.Text
toLazyText = LT.fromStrict . toText

-- | Helper function to define pure Id functions in terms of a IdentifierSetMonad
withIdentifierSetM
  :: IdentifierSetMonad m
  => (IdentifierSet -> a -> (IdentifierSet, b))
  -> a
  -> m b
withIdentifierSetM f a = do
  is0 <- identifierSetM id
  let (is1, b) = f is0 a
  _ <- identifierSetM (const is1)
  pure b

-- | Like 'addRaw', 'unsafeMake' creates an identifier that will be spliced
-- at verbatim in the HDL. As opposed to 'addRaw', the resulting Identifier
-- might be generated at a later point as it is NOT added to an IdentifierSet.
unsafeMake :: HasCallStack => Text -> Identifier
unsafeMake t =
  RawIdentifier t Nothing (if debugIsOn then callStack else emptyCallStack)

-- | Add a string as is to an IdentifierSet. Should only be used for identifiers
-- that should be spliced at verbatim in HDL, such as port names. It's sanitized
-- version will still be added to the identifier set, to prevent freshly
-- generated variables clashing with the raw one.
addRaw :: (HasCallStack, IdentifierSetMonad m) => Text -> m Identifier
addRaw = withIdentifierSetM addRaw#

-- | Make unique identifier based on given string
make :: (HasCallStack, IdentifierSetMonad m) => Text -> m Identifier
make = withIdentifierSetM make#

-- | Make unique basic identifier based on given string
makeBasic :: (HasCallStack, IdentifierSetMonad m) => Text -> m Identifier
makeBasic = withIdentifierSetM makeBasic#

-- | Make unique basic identifier based on given string. If given string can't
-- be converted to a basic identifier (i.e., it would yield an empty string) the
-- alternative name is used.
makeBasicOr
  :: (HasCallStack, IdentifierSetMonad m)
  => Text
  -- ^ Name hint
  -> Text
  -- ^ If name hint can't be converted to a sensible basic id, use this instead
  -> m Identifier
makeBasicOr hint altHint =
  withIdentifierSetM
    (\is0 -> uncurry (makeBasicOr# is0))
    (hint, altHint)

-- | Make unique identifier. Uses 'makeBasic' if first argument is 'Basic'
makeAs :: (HasCallStack, IdentifierSetMonad m) => IdentifierType -> Text -> m Identifier
makeAs Basic = makeBasic
makeAs Extended = make

-- | Given identifier "foo_1_2" return "foo_1_3". If "foo_1_3" is already a
-- member of the given set, return "foo_1_4" instead, etc. Identifier returned
-- is guaranteed to be unique.
next :: (HasCallStack, IdentifierSetMonad m) => Identifier -> m Identifier
next = withIdentifierSetM next#

-- | Same as 'nextM', but returns N fresh identifiers
nextN :: (HasCallStack, IdentifierSetMonad m) => Int -> Identifier -> m [Identifier]
nextN n = withIdentifierSetM (nextN# n)

-- | Given identifier "foo_1_2" return "foo_1_2_0". If "foo_1_2_0" is already a
-- member of the given set, return "foo_1_2_1" instead, etc. Identifier returned
-- is guaranteed to be unique.
deepen :: (HasCallStack, IdentifierSetMonad m) => Identifier -> m Identifier
deepen = withIdentifierSetM deepen#

-- | Same as 'deepenM', but returns N fresh identifiers. For example, given
-- "foo_23" is would return "foo_23_0", "foo_23_1", ...
deepenN :: (HasCallStack, IdentifierSetMonad m) => Int -> Identifier -> m [Identifier]
deepenN n = withIdentifierSetM (deepenN# n)

-- | Given identifier "foo_1_2" and a suffix "bar", return an identifier called
-- "foo_bar". Identifier returned is guaranteed to be unique according to the
-- rules of 'nextIdentifier'.
suffix :: (HasCallStack, IdentifierSetMonad m) => Identifier -> Text -> m Identifier
suffix id0 suffix_ = withIdentifierSetM (\is id1 -> suffix# is id1 suffix_) id0

-- | Given identifier "foo_1_2" and a prefix "bar", return an identifier called
-- "bar_foo". Identifier returned is guaranteed to be unique according to the
-- rules of 'nextIdentifier'.
prefix :: (HasCallStack, IdentifierSetMonad m) => Identifier -> Text -> m Identifier
prefix id0 prefix_ = withIdentifierSetM (\is id1 -> prefix# is id1 prefix_) id0

-- | Convert a Clash Core Id to an identifier. Makes sure returned identifier
-- is unique.
fromCoreId :: (HasCallStack, IdentifierSetMonad m) => Id -> m Identifier
fromCoreId = withIdentifierSetM fromCoreId#
