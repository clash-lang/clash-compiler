{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Netlist.Id.Internal where

import           Control.DeepSeq (NFData)
import           Control.Arrow (second)
import           Data.Binary (Binary)
import qualified Data.Char as Char
import           Data.Function (on)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable(..))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Extra (showt)
import           GHC.Generics (Generic)
import           GHC.Stack
import           Text.Read (readMaybe)

#if MIN_VERSION_prettyprinter(1,7,0)
import qualified Prettyprinter as PP
#else
import qualified Data.Text.Prettyprint.Doc as PP
#endif

import           Clash.Annotations.Primitive (HDL (..))
import           Clash.Core.Name (Name(nameOcc))
import           Clash.Core.Var (Id, varName)
import           Clash.Debug (debugIsOn)
-- import {-# SOURCE #-} Clash.Netlist.Id
import qualified Clash.Netlist.Id.SystemVerilog as SystemVerilog
import qualified Clash.Netlist.Id.Verilog as Verilog
import qualified Clash.Netlist.Id.VHDL as VHDL
import qualified Clash.Netlist.Id.Common as Common

-- | Whether to preserve casing in ids or converted everything to
--  lowercase. Influenced by '-fclash-lower-case-basic-identifiers'
data PreserveCase
  = PreserveCase
  | ToLower
  deriving (Show, Generic, NFData, Eq, Binary, Hashable)

-- | A collection of unique identifiers. Allows for fast fresh identifier
-- generation.
--
-- __NB__: use the functions in Clash.Netlist.Id. Don't use the constructor directly.
data IdentifierSet
  = IdentifierSet {
      is_allowEscaped :: !Bool
      -- ^ Allow escaped ids? If set to False, "make" will always behave like
      -- "makeBasic".
    , is_lowerCaseBasicIds :: !PreserveCase
      -- ^ Force all generated basic identifiers to lowercase.
    , is_hdl :: !HDL
      -- ^ HDL to generate fresh identifiers for
    , is_freshCache :: !FreshCache
      -- ^ Maps an 'i_baseNameCaseFold' to a map mapping the number of
      -- extensions (in 'i_extensionsRev') to the maximum word at that
      -- basename/level. For example, if a set would contain the identifiers:
      --
      --   [foo, foo_1, foo_2, bar_5, bar_7_8]
      --
      -- the map would look like:
      --
      --   [(foo, [(0, 0), (1, 2)]), (bar, [(1, 5), (2, 8)])]
      --
      -- This mapping makes sure we can quickly generate fresh identifiers. For
      -- example, generating a new id for "foo_1" would be a matter of looking
      -- up the base name in this map, concluding that the maximum identifier
      -- with this basename and this number of extensions is "foo_2",
      -- subsequently generating "foo_3".
      --
      -- Note that an identifier with no extensions is also stored in this map
      -- for practical purposes, but the maximum ext is invalid.
    , is_store :: !(HashSet Identifier)
      -- ^ Identifier store
    } deriving (Generic, NFData, Show)

-- | See 'is_freshCache'
type FreshCache = HashMap Text (IntMap Word)

-- See: http://vhdl.renerta.com/mobile/source/vhd00037.htm
--      http://www.verilog.renerta.com/source/vrg00018.htm
data IdentifierType
  = Basic
  -- ^ A basic identifier: does not have to be escaped in order to be a valid
  -- identifier in HDL.
  | Extended
  -- ^ An extended identifier: has to be escaped, wrapped, or otherwise
  -- postprocessed before writhing it to HDL.
  deriving (Show, Generic, NFData, Eq)

-- | HDL identifier. Consists of a base name and a number of extensions. An
-- identifier with a base name of "foo" and a list of extensions [1, 2] will be
-- rendered as "foo_1_2".
--
-- Note: The Eq instance of "Identifier" is case insensitive! E.g., two
-- identifiers with base names 'fooBar' and 'FoObAR' are considered the same.
-- However, identifiers are stored case preserving. This means Clash won't
-- generate two identifiers with differing case, but it will try to keep
-- capitalization.
--
-- The goal of this data structure is to greatly simplify how Clash deals with
-- identifiers internally. Any Identifier should be trivially printable to any
-- HDL.
--
-- __NB__: use the functions in Clash.Netlist.Id. Don't use these constructors
-- directly.
data Identifier
  -- | Unparsed identifier. Used for things such as port names, which should
  -- appear in the HDL exactly as the user specified.
  = RawIdentifier
      !Text
      -- ^ An identifier exactly as given by the user
      (Maybe Identifier)
      -- ^ Parsed version of raw identifier. Will not be populated if this
      -- identifier was created with an unsafe function.
      !CallStack
      -- ^ Stores where this identifier was generated. Tracking is only enabled
      -- is 'debugIsOn', otherwise this field will be populated by an empty
      -- callstack.

  -- | Parsed and sanitized identifier. See various fields for more information
  -- on its invariants.
  | UniqueIdentifier {
      i_baseName :: !Text
    -- ^ Base name of identifier. 'make' makes sure this field:
    --
    --    * does not end in '_num' where 'num' is a digit.
    --    * is solely made up of printable ASCII characters
    --    * has no leading or trailing whitespace
    --
    , i_baseNameCaseFold :: !Text
    -- ^ Same as 'i_baseName', but can be used for equality testing that doesn't
    -- depend on capitalization.
    , i_extensionsRev :: [Word]
    -- ^ Extensions applied to base identifier. E.g., an identifier with a base
    -- name of 'foo' and an extension of [6, 5] would render as 'foo_5_6'. Note
    -- that extensions are stored in reverse order for easier manipulation.
    , i_idType :: !IdentifierType
    -- ^ See 'IdentifierType'.
    , i_hdl :: !HDL
    -- ^ HDL this identifier is generated for.
    , i_provenance :: !CallStack
    -- ^ Stores where this identifier was generated. Tracking is only enabled
    -- is 'debugIsOn', otherwise this field will be populated by an empty
    -- callstack.
    } deriving (Show, Generic, NFData)

identifierKey# :: Identifier -> ((Text, Bool), [Word])
identifierKey# (RawIdentifier t _id _) = ((t, True), [])
identifierKey# id_ = ((i_baseNameCaseFold id_, False), i_extensionsRev id_)

instance Hashable Identifier where
  hashWithSalt salt = hashWithSalt salt . hash
  hash = uncurry hash# . identifierKey#
   where
    hash# a extensions =
      -- 'hash' has an identity around zero, e.g. `hash (0, 2) == 2`. Because a
      -- lot of zeros can be expected, extensions are fuzzed in order to keep
      -- efficient `HashMap`s.
      let fuzz fuzzFactor ext = fuzzFactor * fuzzFactor * ext in
      hash (a, List.foldl' fuzz 2 extensions)

instance Eq Identifier where
  (==) = (==) `on` identifierKey#

instance Ord Identifier where
  compare = compare `on` identifierKey#

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
  id2 = id1{i_provenance=if debugIsOn then callStack else emptyCallStack}
  id1 = case lookupFreshCache# (is_freshCache is) id0 of
    Just currentMax ->
      id0{i_extensionsRev=currentMax+1 : tail (i_extensionsRev id0)}
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
