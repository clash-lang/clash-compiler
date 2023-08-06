{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Netlist.Id.Internal where

import           Clash.Annotations.Primitive (HDL (..))
import           Clash.Core.Name (Name(nameOcc))
import           Clash.Core.Var (Id, varName)
import           Clash.Debug (debugIsOn)
import           Clash.Netlist.Types
  (PreserveCase(..), IdentifierSet(..), Identifier(..), FreshCache,
   IdentifierType(..))
import           Control.Arrow (second)
import qualified Data.Char as Char
import qualified Data.List as List

#if MIN_VERSION_prettyprinter(1,7,0)
import qualified Prettyprinter as PP
#else
import qualified Data.Text.Prettyprint.Doc as PP
#endif

import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Text.Extra (showt)
import qualified Data.Maybe as Maybe
import           Text.Read (readMaybe)
import           GHC.Stack

import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import qualified Clash.Netlist.Id.SystemVerilog as SystemVerilog
import qualified Clash.Netlist.Id.Verilog as Verilog
import qualified Clash.Netlist.Id.VHDL as VHDL
import qualified Clash.Netlist.Id.Common as Common

-- | Return identifier with highest extension for given identifier. See
-- 'is_freshCache' for more information.
--
-- For example, if the FreshCache contains "foo_12_25" and the given identifier
-- is "foo_12_13" this function would return "Just 25". In this case, "foo_12_26"
-- is guaranteed to be a fresh identifier.
lookupFreshCache# :: FreshCache -> Identifier -> Maybe Word
lookupFreshCache# fresh0 id0 = do
  fresh1 <- HashMap.lookup (i_baseNameCaseFold id0) fresh0
  IntMap.lookup (length (i_extensionsRev id0)) fresh1

-- | Add new identifier to FreshCache, see 'is_freshCache' for more information.
updateFreshCache# :: HasCallStack => FreshCache -> Identifier -> FreshCache
updateFreshCache# _fresh (RawIdentifier _s Nothing _) =
  error "Internal error: updateFreshCache# called with unsafely made identifier"
updateFreshCache# fresh (RawIdentifier _s (Just id_) _) =
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
mkUnique# is id0 = (is{is_freshCache=freshCache, is_store=isStore}, id2)
 where
  freshCache = updateFreshCache# (is_freshCache is) id2
  isStore = HashSet.insert id2 (is_store is)
  id2 = case id1 of
          x@RawIdentifier{} -> x
          y -> y{i_provenance=if debugIsOn then callStack else emptyCallStack}
  id1 = case lookupFreshCache# (is_freshCache is) id0 of
    Just currentMax ->
      id0{i_extensionsRev=currentMax+1 : drop 1 (i_extensionsRev id0)}
    Nothing ->
      -- Identifier doesn't exist in set yet, so just return it.
      id0

-- | Non-monadic, internal version of 'add'
add# :: HasCallStack => IdentifierSet -> Identifier -> IdentifierSet
add# is0@(IdentifierSet{..}) (RawIdentifier t Nothing _) = add# is0 (make## is_hdl t)
add# is0 (RawIdentifier _ (Just id0) _) = add# is0 id0
add# is0@(IdentifierSet{..}) id0 = is0{is_freshCache=fresh1, is_store=ids1}
 where
  ids1 = HashSet.insert id0 is_store
  fresh1 = updateFreshCache# is_freshCache id0

-- | Non-monadic, internal version of 'addMultiple'
addMultiple# :: (HasCallStack, Foldable t) => IdentifierSet -> t Identifier -> IdentifierSet
addMultiple# is ids = List.foldl' add# is ids

-- | Non-monadic, internal version of 'addRaw'
addRaw# :: HasCallStack => IdentifierSet -> Text -> (IdentifierSet, Identifier)
addRaw# is0 id0 =
  second
    (\i -> RawIdentifier id0 (Just i) (if debugIsOn then callStack else emptyCallStack))
    (make# is0 (unextend id0))
 where
  unextend = case is_hdl is0 of
    VHDL -> VHDL.unextend
    Verilog -> Verilog.unextend
    SystemVerilog -> SystemVerilog.unextend

-- | Non-monadic, internal version of 'make'
make# :: HasCallStack => IdentifierSet -> Text -> (IdentifierSet, Identifier)
make# is0@(IdentifierSet esc lw hdl fresh0 ids0) (Common.prettyName -> id0) =
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
  id1 = make## (is_hdl is0) (if esc then id0 else toBasicId# hdl lw id0)

-- | Non-monadic, internal version of 'makeBasic'
makeBasic# :: HasCallStack => IdentifierSet -> Text -> (IdentifierSet, Identifier)
makeBasic# is0 = make# is0 . toBasicId# (is_hdl is0) (is_lowerCaseBasicIds is0)

-- | Non-monadic, internal version of 'makeBasicOr'
makeBasicOr# :: HasCallStack => IdentifierSet -> Text -> Text -> (IdentifierSet, Identifier)
makeBasicOr# is0 hint altHint = make# is0 id1
 where
  id0 = toBasicId# (is_hdl is0) (is_lowerCaseBasicIds is0) hint
  id1 = if Text.null id0
        then toBasicId# (is_hdl is0) (is_lowerCaseBasicIds is0) altHint
        else id0

-- | Non-monadic, internal version of 'next'
next# :: HasCallStack => IdentifierSet -> Identifier ->  (IdentifierSet, Identifier)
next# is0 (RawIdentifier t Nothing _) = uncurry next# (make# is0 t)
next# is0 (RawIdentifier _ (Just id_) _) = next# is0 id_
next# is0 id_@(i_extensionsRev -> []) = deepen# is0 id_
next# is0 id_ = mkUnique# is0 id_

-- | Non-monadic, internal version of 'nextN'
nextN# :: HasCallStack => Int -> IdentifierSet -> Identifier ->  (IdentifierSet, [Identifier])
nextN# n is0 id0 = List.mapAccumL (\is1 _n -> next# is1 id0) is0 [1..n]
-- TODO: ^ More efficient implementation.

-- | Non-monadic, internal version of 'deepenN'
deepenN# :: HasCallStack => Int -> IdentifierSet -> Identifier ->  (IdentifierSet, [Identifier])
deepenN# n is0 id0 = List.mapAccumL (\is1 _n -> deepen# is1 id0) is0 [1..n]
-- TODO: ^ More efficient implementation.

-- | Non-monadic, internal version of 'deepen'
deepen# :: HasCallStack => IdentifierSet -> Identifier ->  (IdentifierSet, Identifier)
deepen# is0 (RawIdentifier t Nothing _) = uncurry deepen# (make# is0 t)
deepen# is0 (RawIdentifier _ (Just id_) _) = deepen# is0 id_
deepen# is0 id_ = mkUnique# is0 (id_{i_extensionsRev=0:i_extensionsRev id_})

-- | Non-monadic, internal version of 'suffix'
suffix# :: HasCallStack => IdentifierSet -> Identifier -> Text -> (IdentifierSet, Identifier)
suffix# is0 (RawIdentifier t Nothing _) suffix_ = (uncurry suffix# (make# is0 t)) suffix_
suffix# is0 (RawIdentifier _ (Just id_) _) suffix_ = suffix# is0 id_ suffix_
suffix# is0 id0 suffix_ = make# is0 (i_baseName id0 <> "_" <> suffix_)

-- | Non-monadic, internal version of 'prefix'
prefix# :: HasCallStack => IdentifierSet -> Identifier -> Text -> (IdentifierSet, Identifier)
prefix# is0 (RawIdentifier t Nothing _) prefix_ = (uncurry prefix# (make# is0 t)) prefix_
prefix# is0 (RawIdentifier _ (Just id_) _) prefix_ = prefix# is0 id_ prefix_
prefix# is0 id0 prefix_ = make# is0 (prefix_ <> "_" <> i_baseName id0)

toText# :: Identifier -> Text
toText# (RawIdentifier t _ _) = t
toText# (UniqueIdentifier{..}) =
  case i_hdl of
    VHDL -> VHDL.toText i_idType basicId
    Verilog -> Verilog.toText i_idType basicId
    SystemVerilog -> SystemVerilog.toText i_idType basicId
 where
  exts = map showt (reverse i_extensionsRev)
  basicId = Text.intercalate "_" (i_baseName : exts)

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

make## :: HasCallStack => HDL -> Text -> Identifier
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
            UniqueIdentifier
              baseName baseNameCaseFold extensions idType hdl
              (if debugIsOn then callStack else emptyCallStack)

toBasicId# :: HDL -> PreserveCase -> Text -> Text
toBasicId# hdl lw id0 =
  case hdl of
    VHDL -> VHDL.toBasic id1
    Verilog -> Verilog.toBasic id1
    SystemVerilog -> SystemVerilog.toBasic id1
 where
  id1 = case lw of {PreserveCase -> id0; ToLower -> Text.toLower id0}

-- | Convert a Clash Core Id to an identifier. Makes sure returned identifier
-- is unique.
fromCoreId# :: IdentifierSet -> Id -> (IdentifierSet, Identifier)
fromCoreId# is0 id0 = make# is0 (nameOcc (varName id0))

instance PP.Pretty Identifier where
  pretty = PP.pretty . toText#
