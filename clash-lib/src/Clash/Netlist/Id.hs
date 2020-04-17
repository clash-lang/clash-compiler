{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

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

{-# OPTIONS_GHC -Wno-orphans #-}

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
import           Clash.Core.Name (nameOcc)
import           Clash.Core.Var (Id, varName)
import {-# SOURCE #-} Clash.Netlist.Types
  (HasIdentifierSet(..), IdentifierSet(..), Identifier(..), IdentifierType(..),
   IdentifierSetMonad(identifierSetM), FreshCache)
import           Control.Arrow (second)
import qualified Data.Char as Char
import           Data.Function (on)
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Maybe as Maybe
import           Data.Hashable (Hashable(..))
import           Text.Read (readMaybe)
import           TextShow (showt)
import           GHC.Stack (HasCallStack)

import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import qualified Data.Text.Prettyprint.Doc as PP

import qualified Clash.Netlist.Id.SystemVerilog as SystemVerilog
import qualified Clash.Netlist.Id.Verilog as Verilog
import qualified Clash.Netlist.Id.VHDL as VHDL
import qualified Clash.Netlist.Id.Common as Common

-- | Identifier set without identifiers
emptyIdentifierSet
  :: Bool
  -- ^ Allow escaped identifiers?
  -> HDL
  -- ^ HDL to generate names for
  -> IdentifierSet
emptyIdentifierSet esc hdl = makeSet esc hdl []

-- | Union of two identifier sets. Errors if given sets have been made with
-- different options enabled.
union :: HasCallStack => IdentifierSet -> IdentifierSet -> IdentifierSet
union is1@(IdentifierSet esc1 hdl1 _ _) is2@(IdentifierSet esc2 hdl2 _ _)
  | esc1 /= esc2 = error $ "Internal error: esc1 /= esc2, " <> show (esc1, esc2)
  | hdl1 /= hdl2 = error $ "Internal error: hdl1 /= hdl2, " <> show (hdl1, hdl2)
  | otherwise = makeSet esc1 hdl1 (toList is1 ++ toList is2)

-- | Make a identifier set filled with given identifiers
makeSet
  :: Bool
  -- ^ Allow escaped identifiers?
  -> HDL
  -- ^ HDL to generate names for
  -> [Identifier]
  -- ^ Identifiers to add to set
  -> IdentifierSet
makeSet esc hdl ids = IdentifierSet esc hdl fresh store
 where
  fresh = List.foldl' updateFreshCache# mempty ids
  store = HashSet.fromList ids

toList :: IdentifierSet -> [Identifier]
toList (IdentifierSet _ _ _ idStore) = HashSet.toList idStore

lookupFreshCache# :: FreshCache -> Identifier -> Maybe Word
lookupFreshCache# fresh0 id0 = do
  fresh1 <- HashMap.lookup (i_baseNameCaseFold id0) fresh0
  IntMap.lookup (length (i_extensionsRev id0)) fresh1

updateFreshCache# :: HasCallStack => FreshCache -> Identifier -> FreshCache
updateFreshCache# _fresh (RawIdentifier _s Nothing) =
  error "Internal error: updateFreshCache# called with unsafely made identifier"
updateFreshCache# fresh (RawIdentifier _s (Just id_)) =
  updateFreshCache# fresh id_
updateFreshCache# fresh id_ =
  go0 (go1 (max (Maybe.fromMaybe 0 (Maybe.listToMaybe exts))))
 where
  go0 f = HashMap.alter (Just . f . Maybe.fromMaybe mempty) base fresh
  go1 f = IntMap.alter (Just . f . Maybe.fromMaybe 0) (length exts)

  exts = i_extensionsRev id_
  base = i_baseNameCaseFold id_

-- | Adds identifier at verbatim if its basename hasn't been used before.
-- Otherwise it will return the first free identifier.
mkUnique#
  :: HasCallStack
  => IdentifierSet
  -> Identifier
  -> (IdentifierSet, Identifier)
mkUnique# _is0 (RawIdentifier {}) =
  error "Internal error: mkUnique# cannot be used on RawIdentifiers"
mkUnique# is0 id_@(i_extensionsRev -> []) = deepen# is0 id_
mkUnique# is id0 = (is{is_freshCache=freshCache, is_store=isStore}, id1)
 where
  freshCache = updateFreshCache# (is_freshCache is) id1
  isStore = HashSet.insert id1 (is_store is)
  id1 = case lookupFreshCache# (is_freshCache is) id0 of
    Just currentMax ->
      id0{i_extensionsRev=currentMax+1 : tail (i_extensionsRev id0)}
    Nothing ->
      -- Identifier doesn't exist in set yet, so just return it.
      id0

addRaw# :: IdentifierSet -> Text -> (IdentifierSet, Identifier)
addRaw# is0 id0 = second (RawIdentifier id0 . Just) (make# is0 (unextend id0))
 where
  unextend = case is_hdl is0 of
    VHDL -> VHDL.unextend
    Verilog -> Verilog.unextend
    SystemVerilog -> SystemVerilog.unextend

make# :: IdentifierSet -> Text -> (IdentifierSet, Identifier)
make# is0@(IdentifierSet esc hdl fresh0 ids0) (Common.prettyName -> id0) =
  if id1 `HashSet.member` ids0 then
    -- Ideally we'd like to continue with the id from the HashSet so all the old
    -- strings can be garbage collected, but I haven't found an efficient way of
    -- doing so. I also doubt that this case will get hit often..
    deepen# is0 id1
  else
    (is0{is_freshCache=fresh1, is_store=ids1}, id1)
 where
  ids1 = HashSet.insert id1 ids0
  fresh1 = updateFreshCache# fresh0 id1
  id1 = make## (is_hdl is0) (if esc then id0 else toBasicId# hdl id0)

makeBasic# :: IdentifierSet -> Text -> (IdentifierSet, Identifier)
makeBasic# is0 = make# is0 . toBasicId# (is_hdl is0)

makeBasicOr# :: IdentifierSet -> Text -> Text -> (IdentifierSet, Identifier)
makeBasicOr# is0 hint altHint = make# is0 id1
 where
  id0 = toBasicId# (is_hdl is0) hint
  id1 = if Text.null id0 then toBasicId# (is_hdl is0) altHint else id0

next# :: IdentifierSet -> Identifier ->  (IdentifierSet, Identifier)
next# is0 (RawIdentifier t Nothing) = uncurry next# (make# is0 t)
next# is0 (RawIdentifier _ (Just id_)) = next# is0 id_
next# is0 id_@(i_extensionsRev -> []) = deepen# is0 id_
next# is0 id_ = mkUnique# is0 id_

nextN# :: Int -> IdentifierSet -> Identifier ->  (IdentifierSet, [Identifier])
nextN# n is0 id0 = List.mapAccumL (\is1 _n -> next# is1 id0) is0 [1..n]
-- TODO: ^ More efficient implementation.

deepenN# :: Int -> IdentifierSet -> Identifier ->  (IdentifierSet, [Identifier])
deepenN# n is0 id0 = List.mapAccumL (\is1 _n -> deepen# is1 id0) is0 [1..n]
-- TODO: ^ More efficient implementation.

deepen# :: IdentifierSet -> Identifier ->  (IdentifierSet, Identifier)
deepen# is0 (RawIdentifier t Nothing) = uncurry deepen# (make# is0 t)
deepen# is0 (RawIdentifier _ (Just id_)) = deepen# is0 id_
deepen# is0 id_ = mkUnique# is0 (id_{i_extensionsRev=0:i_extensionsRev id_})

suffix# :: IdentifierSet -> Identifier -> Text -> (IdentifierSet, Identifier)
suffix# is0 (RawIdentifier t Nothing) suffix_ = (uncurry suffix# (make# is0 t)) suffix_
suffix# is0 (RawIdentifier _ (Just id_)) suffix_ = suffix# is0 id_ suffix_
suffix# is0 id0 suffix_ = make# is0 (i_baseName id0 <> "_" <> suffix_)

prefix# :: IdentifierSet -> Identifier -> Text -> (IdentifierSet, Identifier)
prefix# is0 (RawIdentifier t Nothing) prefix_ = (uncurry prefix# (make# is0 t)) prefix_
prefix# is0 (RawIdentifier _ (Just id_)) prefix_ = prefix# is0 id_ prefix_
prefix# is0 id0 prefix_ = make# is0 (prefix_ <> "_" <> i_baseName id0)

toText# :: Identifier -> Text
toText# (RawIdentifier t _) = t
toText# (UniqueIdentifier{..}) =
  case i_hdl of
    VHDL -> VHDL.toText i_idType basicId
    Verilog -> Verilog.toText i_idType basicId
    SystemVerilog -> SystemVerilog.toText i_idType basicId
 where
  exts = map showt (reverse i_extensionsRev)
  basicId = Text.intercalate "_" (i_baseName : exts)

-- | Convert an identifier to string. Use 'unmake' if you need the
-- "IdentifierType" too.
toText :: Identifier -> Text
toText = toText#

-- | Convert an identifier to string. Use 'unmake' if you need the
-- "IdentifierType" too.
toLazyText :: Identifier -> LT.Text
toLazyText = LT.fromStrict . toText

-- | Convert a Clash Core Id to an identifier. Makes sure returned identifier
-- is unique.
fromCoreId# :: IdentifierSet -> Id -> (IdentifierSet, Identifier)
fromCoreId# is0 id0 = make# is0 (nameOcc (varName id0))

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
unsafeMake :: Text -> Identifier
unsafeMake t = RawIdentifier t Nothing

-- | Add a string as is to an IdentifierSet. Should only be used for identifiers
-- that should be spliced at verbatim in HDL, such as port names. It's sanitized
-- version will still be added to the identifier set, to prevent freshly
-- generated variables clashing with the raw one.
addRaw :: IdentifierSetMonad m => Text -> m Identifier
addRaw = withIdentifierSetM addRaw#

-- | Make unique identifier based on given string
make :: IdentifierSetMonad m => Text -> m Identifier
make = withIdentifierSetM make#

-- | Make unique basic identifier based on given string
makeBasic :: IdentifierSetMonad m => Text -> m Identifier
makeBasic = withIdentifierSetM makeBasic#

-- | Make unique basic identifier based on given string. If given string can't
-- be converted to a basic identifier (i.e., it would yield an empty string) the
-- alternative name is used.
makeBasicOr
  :: IdentifierSetMonad m
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
makeAs :: IdentifierSetMonad m => IdentifierType -> Text -> m Identifier
makeAs Basic = makeBasic
makeAs Extended = make

-- | Given identifier "foo_1_2" return "foo_1_3". If "foo_1_3" is already a
-- member of the given set, return "foo_1_4" instead, etc. Identifier returned
-- is guaranteed to be unique.
next :: IdentifierSetMonad m => Identifier -> m Identifier
next = withIdentifierSetM next#

-- | Same as 'nextM', but returns N fresh identifiers
nextN :: IdentifierSetMonad m => Int -> Identifier -> m [Identifier]
nextN n = withIdentifierSetM (nextN# n)

-- | Given identifier "foo_1_2" return "foo_1_2_0". If "foo_1_2_0" is already a
-- member of the given set, return "foo_1_2_1" instead, etc. Identifier returned
-- is guaranteed to be unique.
deepen :: IdentifierSetMonad m => Identifier -> m Identifier
deepen = withIdentifierSetM deepen#

-- | Same as 'deepenM', but returns N fresh identifiers. For example, given
-- "foo_23" is would return "foo_23_0", "foo_23_1", ...
deepenN :: IdentifierSetMonad m => Int -> Identifier -> m [Identifier]
deepenN n = withIdentifierSetM (deepenN# n)

-- | Given identifier "foo_1_2" and a suffix "bar", return an identifier called
-- "foo_bar". Identifier returned is guaranteed to be unique according to the
-- rules of 'nextIdentifier'.
suffix :: IdentifierSetMonad m => Identifier -> Text -> m Identifier
suffix id0 suffix_ = withIdentifierSetM (\is id1 -> suffix# is id1 suffix_) id0

-- | Given identifier "foo_1_2" and a prefix "bar", return an identifier called
-- "bar_foo". Identifier returned is guaranteed to be unique according to the
-- rules of 'nextIdentifier'.
prefix :: IdentifierSetMonad m => Identifier -> Text -> m Identifier
prefix id0 prefix_ = withIdentifierSetM (\is id1 -> prefix# is id1 prefix_) id0

-- | Convert a Clash Core Id to an identifier. Makes sure returned identifier
-- is unique.
fromCoreId :: IdentifierSetMonad m => Id -> m Identifier
fromCoreId = withIdentifierSetM fromCoreId#

instance PP.Pretty Identifier where
  pretty = PP.pretty . toText

instance Hashable Identifier where
  hashWithSalt salt = hashWithSalt salt . hash
  hash = uncurry hash# . eqTup#

instance Eq Identifier where
  i1 == i2 = eqTup# i1 == eqTup# i2
  i1 /= i2 = eqTup# i1 /= eqTup# i2

instance Ord Identifier where
  compare = compare `on` eqTup#

eqTup# :: Identifier -> ((Text, Bool), [Word])
eqTup# (RawIdentifier t _id) = ((t, True), [])
eqTup# id_ = ((i_baseNameCaseFold id_, False), i_extensionsRev id_)

hash# :: Hashable a => a -> [Word] -> Int
hash# a extensions =
  -- 'hash' has an identity around zero, e.g. `hash (0, 2) == 2`. Because a lot
  -- of zeros can be expected, extensions are fuzzed in order to keep efficient
  -- `HashMap`s.
  let fuzz fuzzFactor ext = fuzzFactor * fuzzFactor * ext in
  hash (a, List.foldl' fuzz 2 extensions)

-- | Is given string a valid basic identifier in given HDL?
isBasic# :: HDL -> Text -> Bool
isBasic# VHDL = VHDL.parseBasic
isBasic# Verilog = Verilog.parseBasic
isBasic# SystemVerilog = SystemVerilog.parseBasic

-- | Is given string a valid extended identifier in given HDL?
isExtended# :: HDL -> Text -> Bool
isExtended# VHDL = VHDL.parseExtended
isExtended# Verilog = Verilog.parseExtended
isExtended# SystemVerilog = SystemVerilog.parseExtended

-- | Convert given string to ASCII. Retains all printable ASCII. All other
-- characters are thrown out.
toPrintableAscii# :: Text -> Text
toPrintableAscii# = Text.filter (\c -> Char.isPrint c && Char.isAscii c)

-- | Split identifiers such as "foo_1_2" into ("foo", [2, 1]).
parseIdentifier# :: Text -> (Text, [Word])
parseIdentifier# t =
  let (tsRev, extsRev) = go (List.reverse (Text.splitOn "_" t)) in
  (Text.intercalate "_" (List.reverse tsRev), extsRev)
 where
  go :: [Text] -> ([Text], [Word])
  go [] = go ["clash", "internal"]
  go (i:is) = case readMaybe @Word (Text.unpack i) of
    Just w -> second (w:) (go is)
    Nothing -> (i:is, [])

make## :: HDL -> Text -> Identifier
make## hdl =
    go
  . Text.strip
  . Text.replace "\\" ""
  . toPrintableAscii#
 where
  go s | Text.null s = go "clash_internal"
       | otherwise =
          let
            (baseName, extensions) = parseIdentifier# s
            idType = if isBasic# hdl s then Basic else Extended

            -- VHDL is a case insensitive language, so we convert the given
            -- text to lowercase. Note that 'baseNameCaseFold' is used in the
            -- Eq for Identifier.
            baseNameCaseFold = case hdl of
              VHDL -> Text.toCaseFold baseName
              _ -> baseName
          in
            UniqueIdentifier baseName baseNameCaseFold extensions idType hdl

toBasicId# :: HDL -> Text -> Text
toBasicId# VHDL          = VHDL.toBasic
toBasicId# Verilog       = Verilog.toBasic
toBasicId# SystemVerilog = SystemVerilog.toBasic
