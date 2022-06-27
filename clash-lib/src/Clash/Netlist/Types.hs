{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd,
                    2017-2018, Google Inc.
                    2020-2022, QBayLogic B.V.
                    2022     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Type and instance definitions for Netlist modules
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Netlist.Types
  ( Declaration (..,NetDecl)
  , CompDecl (..)
  , module Clash.Netlist.Types
  )
where

import Control.DeepSeq
import qualified Control.Lens               as Lens
import Control.Lens                         (Lens', (.=))
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail                   (MonadFail)
#endif
import Control.Monad.Reader                 (ReaderT, MonadReader)
import qualified Control.Monad.State        as Lazy (State)
import qualified Control.Monad.State.Strict as Strict
  (State, MonadIO, MonadState, StateT)
import Data.Aeson                           (FromJSON(..))
import qualified Data.Aeson as Aeson
import Data.Bits                            (testBit)
import Data.Binary                          (Binary(..))
import Data.Function                        (on)
import Data.Hashable                        (Hashable(hash,hashWithSalt))
import Data.HashMap.Strict                  (HashMap)
import Data.HashSet                         (HashSet)
import qualified Data.List                  as List
import Data.IntMap                          (IntMap, empty)
import Data.Map.Ordered                     (OMap)
import Data.Map                             (Map)
import qualified Data.Map as Map
import Data.Maybe                           (mapMaybe)
import Data.Monoid                          (Ap(..))
import qualified Data.Set                   as Set
import Data.Text                            (Text)

import Data.Typeable                        (Typeable)
import Data.Text.Prettyprint.Doc.Extra      (Doc)
import GHC.Generics                         (Generic)
import GHC.Stack
import Language.Haskell.TH.Syntax           (Lift)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.SrcLoc                     (SrcSpan)
#else
import SrcLoc                               (SrcSpan)
#endif

import Clash.Annotations.BitRepresentation  (FieldAnn)
import Clash.Annotations.Primitive          (HDL(..))
import Clash.Annotations.TopEntity          (TopEntity)
import Clash.Backend                        (Backend, HasUsageMap (..))
import Clash.Core.HasType
import Clash.Core.Type                      (Type)
import Clash.Core.Var                       (Attr', Id)
import Clash.Core.TyCon                     (TyConMap)
import Clash.Core.VarEnv                    (VarEnv)
import Clash.Driver.Types                   (BindingMap, ClashEnv(..), ClashOpts(..))
import Clash.Netlist.BlackBox.Types         (BlackBoxTemplate)
import Clash.Primitives.Types               (CompiledPrimMap)
import Clash.Signal.Internal
  (ResetPolarity, ActiveEdge, ResetKind, InitBehavior)
import Clash.Unique                         (Unique)

import Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, DataRepr', ConstrRepr')

import {-# SOURCE #-} qualified Clash.Netlist.Id as Id

-- | Structure describing a top entity: it's id and its port annotations.
data TopEntityT = TopEntityT
  { topId :: Id
  -- ^ Id of top entity
  , topAnnotation :: Maybe TopEntity
  -- ^ (Maybe) a topentity annotation
  , topIsTestBench :: Bool
  -- ^ Whether this entity is a test bench
  } deriving (Generic, Show, Eq)

-- | Same as "TopEntity", but with all port names that end up in HDL specified
data ExpandedTopEntity a = ExpandedTopEntity
  { et_inputs :: [Maybe (ExpandedPortName a)]
  -- ^ Inputs with fully expanded port names. /Nothing/ if port is void.
  , et_output :: Maybe (ExpandedPortName a)
  -- ^ Output with fully expanded port names. /Nothing/ if port is void or
  -- BiDirectionalOut.
  } deriving (Show, Functor, Foldable, Traversable)

-- | See "ExpandedTopEntity"
data ExpandedPortName a
  -- | Same as "PortName", but fully expanded
  = ExpandedPortName HWType a

  -- | Same as "PortProduct", but fully expanded
  | ExpandedPortProduct
      Text
      -- ^ Name hint. Can be used to create intermediate signal names.
      HWType
      -- ^ Type of product
      [ExpandedPortName a]
      -- ^ Product fields
  deriving (Show, Functor, Foldable, Traversable)

-- | Monad that caches generated components (StateT) and remembers hidden inputs
-- of components that are being generated (WriterT)
newtype NetlistMonad a =
  NetlistMonad { runNetlist :: Strict.StateT NetlistState (ReaderT NetlistEnv IO) a }
  deriving newtype (Functor, Monad, Applicative, MonadReader NetlistEnv,
                    Strict.MonadState NetlistState, Strict.MonadIO, MonadFail)

type HWMap = Map Type (Either String FilteredHWType)

-- | See 'is_freshCache'
type FreshCache = HashMap Text (IntMap Word)

type IdentifierText = Text

-- | Whether to preserve casing in ids or converted everything to
--  lowercase. Influenced by '-fclash-lower-case-basic-identifiers'
data PreserveCase
  = PreserveCase
  | ToLower
  deriving (Show, Generic, NFData, Eq, Binary, Hashable)

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
    -- ^ See "IdentifierType".
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
  i1 == i2 = identifierKey# i1 == identifierKey# i2
  i1 /= i2 = identifierKey# i1 /= identifierKey# i2

instance Ord Identifier where
  compare = compare `on` identifierKey#

-- | Environment of the NetlistMonad
data NetlistEnv
  = NetlistEnv
  { _clashEnv :: ClashEnv
  , _prefixName  :: Text
  -- ^ Prefix for instance/register names
  , _suffixName :: Text
  -- ^ Postfix for instance/register names
  , _setName :: Maybe Text
  -- ^ (Maybe) user given instance/register name
  }

data ComponentMeta = ComponentMeta
  { cmWereVoids :: [Bool]
  , cmLoc :: SrcSpan
  , cmScope :: IdentifierSet
  , cmUsage :: UsageMap
  } deriving (Generic, Show, NFData)

type ComponentMap = OMap Unique (ComponentMeta, Component)

-- | State of the NetlistMonad
data NetlistState
  = NetlistState
  { _bindings       :: BindingMap
  -- ^ Global binders
  , _components     :: ComponentMap
  -- ^ Cached components. Is an insertion ordered map to preserve a topologically
  -- sorted component list for the manifest file.
  , _typeTranslator :: CustomReprs -> TyConMap -> Type
                    -> Strict.State HWMap (Maybe (Either String FilteredHWType))
  -- ^ Hardcoded Type -> HWType translator
  , _curCompNm      :: !(Identifier,SrcSpan)
  , _seenIds        :: IdentifierSet
  -- ^ All names currently in scope.
  , _seenComps      :: IdentifierSet
  -- ^ Components (to be) generated during this netlist run. This is always a
  -- subset of 'seenIds'. Reason d'etre: we currently generate components in a
  -- top down manner. E.g. given:
  --
  --   - A
  --   -- B
  --   -- C
  --
  -- we would generate component 'A' first. Before trying to generate 'B' and
  -- 'C'. 'A' might introduce a number of signal declarations. The names of these
  -- signals can't clash with the name of component 'B', hence we need to pick a
  -- name for B unique w.r.t. all these signal names. If we would postpone
  -- generating a unqiue name for 'B' til _after_ generating all the signal
  -- names, the signal names would get all the "nice" names. E.g., a signal
  -- would be called "foo", thereby forcing the component 'B' to be called
  -- "foo_1". Ideally, we'd use the "nice" names for components, and the "ugly"
  -- names for signals. To achieve this, we generate all the component names
  -- up front and subsequently store them in '_seenComps'.
  , _seenPrimitives :: Set.Set Text
  -- ^ Keeps track of invocations of ´mkPrimitive´. It is currently used to
  -- filter duplicate warning invocations for dubious blackbox instantiations,
  -- see GitHub pull request #286.
  , _componentNames :: VarEnv Identifier
  -- ^ Names of components (to be) generated during this netlist run. Includes
  -- top entity names.
  , _topEntityAnns  :: VarEnv TopEntityT
  , _hdlDir         :: FilePath
  , _curBBlvl       :: Int
  -- ^ The current scoping level assigned to black box contexts
  , _isTestBench    :: Bool
  -- ^ Whether we're compiling a testbench (suppresses some warnings)
  , _backEndITE :: Bool
  -- ^ Whether the backend supports ifThenElse expressions
  , _backend :: SomeBackend
  -- ^ The current HDL backend
  , _htyCache :: HWMap
  , _usages :: UsageMap
  -- ^ The current way signals are assigned in netlist. This is used to
  -- determine how signals are rendered in HDL (i.e. wire/reg in Verilog, or
  -- signal/variable in VHDL).
  }

data ComponentPrefix
  = ComponentPrefix
  { componentPrefixTop :: Maybe Text
    -- ^ Prefix for top-level components
  , componentPrefixOther :: Maybe Text
    -- ^ Prefix for all other components
  } deriving Show

-- | Existentially quantified backend
data SomeBackend where
  SomeBackend :: Backend backend => backend -> SomeBackend

type Comment = Text
type Directive = Text

data CommentOrDirective
  = Comment Comment
  | Directive Directive
  deriving Show

-- | Component: base unit of a Netlist
data Component
  = Component
  { componentName :: !Identifier -- ^ Name of the component
  , inputs        :: [(Identifier,HWType)] -- ^ Input ports
  , outputs       :: [(Usage,(Identifier,HWType),Maybe Expr)] -- ^ Output ports
  , declarations  :: [Declaration] -- ^ Internal declarations
  }
  deriving (Show, Generic, NFData)

-- | Check if an input port is really an inout port.
--
isBiDirectional :: (Identifier, HWType) -> Bool
isBiDirectional (_, BiDirectional _ _) = True
isBiDirectional _ = False

-- | Find the name and domain name of each clock argument of a component.
--
findClocks :: Component -> [(Text, Text)]
findClocks (Component _ is _ _) =
  mapMaybe isClock is
 where
  isClock (i, Clock d) = Just (Id.toText i, d)
  isClock (i, Annotated _ t) = isClock (i,t)
  isClock _ = Nothing

-- | Size indication of a type (e.g. bit-size or number of elements)
type Size = Int

type IsVoid = Bool

-- | Tree structure indicating which constructor fields were filtered from
-- a type due to them being void. We need this information to generate stable
-- and/or user-defined port mappings.
data FilteredHWType =
  FilteredHWType HWType [[(IsVoid, FilteredHWType)]]
    deriving (Eq, Show)

type DomainName = Text

-- | Representable hardware types
data HWType
  = Void (Maybe HWType)
  -- ^ Empty type. @Just Size@ for "empty" Vectors so we can still have
  -- primitives that can traverse e.g. Vectors of unit and know the length of
  -- that vector.
  | String
  -- ^ String type
  | Integer
  -- ^ Integer type (for parameters only)
  | Bool
  -- ^ Boolean type
  | Bit
  -- ^ Bit type
  | BitVector !Size
  -- ^ BitVector of a specified size
  | Index !Integer
  -- ^ Unsigned integer with specified (exclusive) upper bounder
  | Signed !Size
  -- ^ Signed integer of a specified size
  | Unsigned !Size
  -- ^ Unsigned integer of a specified size
  | Vector !Size !HWType
  -- ^ Vector type
  | MemBlob !Size !Size
  -- ^ MemBlob type
  | RTree !Size !HWType
  -- ^ RTree type
  | Sum !Text [Text]
  -- ^ Sum type: Name and Constructor names
  | Product !Text (Maybe [Text]) [HWType]
  -- ^ Product type: Name, field names, and field types. Field names will be
  -- populated when using records.
  | SP !Text [(Text, [HWType])]
  -- ^ Sum-of-Product type: Name and Constructor names + field types
  | Clock !DomainName
  -- ^ Clock type corresponding to domain /DomainName/
  | Reset !DomainName
  -- ^ Reset type corresponding to domain /DomainName/
  | Enable !DomainName
  -- ^ Enable type corresponding to domain /DomainName/
  | BiDirectional !PortDirection !HWType
  -- ^ Tagging type indicating a bidirectional (inout) port
  | CustomSP !Text !DataRepr' !Size [(ConstrRepr', Text, [HWType])]
  -- ^ Same as Sum-Of-Product, but with a user specified bit representation. For
  -- more info, see: Clash.Annotations.BitRepresentations.
  | CustomSum !Text !DataRepr' !Size [(ConstrRepr', Text)]
  -- ^ Same as Sum, but with a user specified bit representation. For more info,
  -- see: Clash.Annotations.BitRepresentations.
  | CustomProduct !Text !DataRepr' !Size (Maybe [Text]) [(FieldAnn, HWType)]
  -- ^ Same as Product, but with a user specified bit representation. For more
  -- info, see: Clash.Annotations.BitRepresentations.
  | Annotated [Attr'] !HWType
  -- ^ Annotated with HDL attributes
  | KnownDomain !DomainName !Integer !ActiveEdge !ResetKind !InitBehavior !ResetPolarity
  -- ^ Domain name, period, active edge, reset kind, initial value behavior
  | FileType
  -- ^ File type for simulation-level I/O
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

hwTypeDomain :: HWType -> Maybe DomainName
hwTypeDomain = \case
  Clock dom -> Just dom
  Reset dom -> Just dom
  Enable dom -> Just dom
  KnownDomain dom _ _ _ _ _ -> Just dom
  _ -> Nothing

-- | Extract hardware attributes from Annotated. Returns an empty list if
-- non-Annotated given or if Annotated has an empty list of attributes.
hwTypeAttrs :: HWType -> [Attr']
hwTypeAttrs (Annotated attrs _type) = attrs
hwTypeAttrs _                       = []

-- | Specifies how to wire up a component instance
data PortMap
  = IndexedPortMap [(PortDirection, HWType, Expr)]
  -- ^ Port map based on port positions (port direction, type, assignment)
  --
  -- HDL Example:
  --
  --     bytemaster bytemaster_ds
  --       ( clk_1
  --       , rst_1
  --       , bitCtrl_0 );
  --
  | NamedPortMap [(Expr, PortDirection, HWType, Expr)]
  -- ^ Port map based on port names (port name, port direction, type, assignment)
  --
  -- HDL Example:
  --
  --     bytemaster bytemaster_ds
  --       ( .clk (clk_1)
  --       , .rst (rst_1)
  --       , .bitCtrl (bitCtrl_0) );
  --
  deriving (Show)

-- | @component@ declaration
data CompDecl
  = VHDLComp !Text [(Text,PortDirection,HWType)]

-- | Internals of a Component
data Declaration
  -- | Signal assignment
  = Assignment
      !Identifier -- ^ Signal to assign
      !Usage      -- ^ How the signal is assigned
      !Expr       -- ^ Assigned expression

  -- | Conditional signal assignment:
  | CondAssignment
      !Identifier            -- ^ Signal to assign
      !HWType                -- ^ Type of the result/alternatives
      !Expr                  -- ^ Scrutinized expression
      !HWType                -- ^ Type of the scrutinee
      [(Maybe Literal,Expr)] -- ^ List of: (Maybe expression scrutinized expression is compared with,RHS of alternative)

  -- | Instantiation of another component:
  | InstDecl
      EntityOrComponent                  -- ^ Whether it's an entity or a component
      (Maybe Text)                       -- ^ Library instance is defined in
      [Attr']                            -- ^ Attributes to add to the generated code
      !Identifier                        -- ^ The component's (or entity's) name
      !Identifier                        -- ^ Instance label
      [(Expr,HWType,Expr)]               -- ^ List of parameters for this component (param name, param type, param value)
      PortMap

  -- | Instantiation of blackbox declaration
  | BlackBoxD
      !Text                    -- ^ Primitive name
      [BlackBoxTemplate]       -- ^ VHDL only: add @library@ declarations
      [BlackBoxTemplate]       -- ^ VHDL only: add @use@ declarations
      [((Text,Text),BlackBox)] -- ^ Intel Quartus only: create a @.qsys@ file from given template
      !BlackBox                -- ^ Template tokens
      BlackBoxContext          -- ^ Context in which tokens should be rendered

  -- | @component@ declaration (VHDL).
  --
  -- See [this tutorial](https://www.ics.uci.edu/~jmoorkan/vhdlref/compdec.html);
  -- refer to §4.5 of IEEE 1076-1993
  | CompDecl
      !Text
      [(Text, PortDirection, HWType)]

  -- | Signal declaration
  | NetDecl'
      (Maybe Comment)                -- ^ Note; will be inserted as a comment in target hdl
      !Identifier                    -- ^ Name of signal
      HWType                         -- ^ Type of signal
      (Maybe Expr)                   -- ^ Initial value
      -- ^ Signal declaration

  -- | HDL tick corresponding to a Core tick
  | TickDecl CommentOrDirective

  -- | Sequential statement
  | Seq [Seq]

  -- | Compilation conditional on some preprocessor symbol, note that
  -- declarations here are ignored for VHDL. See here for a discussion
  -- https://github.com/clash-lang/clash-compiler/pull/1798#discussion_r648571862
  | ConditionalDecl
      !Text -- ^ condition text, for example @FORMAL@
      [Declaration]
  deriving Show

-- | Sequential statements
data Seq
  -- | Clocked sequential statements
  = AlwaysClocked
      ActiveEdge -- ^ Edge of the clock the statement should be executed
      Expr       -- ^ Clock expression
      [Seq]      -- ^ Statements to be executed on the active clock edge
  -- | Statements running at simulator start
  | Initial
      [Seq] -- ^ Statements to run at simulator start
  -- | Statements to run always
  | AlwaysComb
      [Seq] -- ^ Statements to run always
  -- | Declaration in sequential form
  | SeqDecl
      Declaration -- ^ The declaration
  -- | Branching statement
  | Branch
      !Expr                    -- ^ Scrutinized expresson
      !HWType                  -- ^ Type of the scrutinized expression
      [(Maybe Literal,[Seq])]  -- ^ List of: (Maybe match, RHS of Alternative)
  deriving Show

-- | Procedural assignment in HDL can be blocking or non-blocking. This
-- determines when the assignment takes place in simulation. The name refers to
-- whether evaluation of the remaining statements in a process is blocked
-- until the assignment is performed or not.
--
-- See Also:
--
-- IEEE 1364-2001, sections 9.2.1 and 9.2.2
-- IEEE 1076-1993, sections 8.4 and 8.5
--
data Blocking
  = NonBlocking
  -- ^ A non-blocking assignment means the new value is not observed until the
  -- next time step in simulation. Using the signal later in the process will
  -- continue to return the old value.
  | Blocking
  -- ^ A blocking assignment means the new value is observed immediately. Using
  -- the signal later in the process will return the new value.
  deriving (Binary, Eq, Generic, Hashable, NFData, Show)

-- NOTE [`Semigroup` instances for `Blocking` and `Usage`]
instance Semigroup Blocking where
  NonBlocking <> y = y
  Blocking    <> _ = Blocking

-- | The usage of a signal refers to how the signal is written to in netlist.
-- This is used to determine if the signal should be a @wire@ or @reg@ in
-- (System)Verilog, or a @signal@ or @variable@ in VHDL.
--
data Usage
  = Cont
  -- ^ Continuous assignment, which occurs in a concurrent context.
  | Proc Blocking
  -- ^ Procedural assignment, which occurs in a sequential context.
  deriving (Binary, Eq, Generic, Hashable, NFData, Show)

-- NOTE [`Semigroup` instances for `Blocking` and `Usage`]
instance Semigroup Usage where
  Cont    <> y      = y
  Proc x  <> Proc y = Proc (x <> y)
  Proc x  <> _      = Proc x

{-
NOTE [`Semigroup` instances for `Blocking` and `Usage`]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Usages (and Blocking) are combined by taking the most restrictive usage, where
most restrictive means "has the most influence over the choice of declaration".
Clash produces three types of assignment:

  * continuous
  * prodecural non-blocking
  * prodecural blocking

Both VHDl and (System)Verilog have a type of declaration which only admits one
type of assignment. This is the most restrictive for that HDL. However, since
that would involve knowing the HDL type in these Semigroup instances, the
most restrictive here is based on ordering where the most restrictive for each
HDL is an extreme value (max for VHDL, min for Verilog). i.e.

          |-------------------------------------|
          | Continuous | NonBlocking | Blocking |
|---------|-------------------------------------|
|    VHDL |         signal           | variable |
|---------|-------------------------------------|
| Verilog |   wire     |          reg           |
|---------|-------------------------------------|
-}

instance FromJSON Usage where
  parseJSON = Aeson.withText "Usage" $ \case
    "Continuous"  -> pure Cont
    "NonBlocking" -> pure (Proc NonBlocking)
    "Blocking"    -> pure (Proc Blocking)
    str           -> fail $ mconcat
      [ "Could not parse usage: "
      , show str
      , "\nRecognized values are 'Continuous', 'NonBlocking' and 'Blocking'"
      ]

-- See NOTE [`Text` key for `UsageMap`]
type UsageMap = Map Text Usage

lookupUsage :: Identifier -> UsageMap -> Maybe Usage
lookupUsage i = Map.lookup (Id.toText i)

{-
NOTE [`Text` key for `UsageMap`]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We would like to use netlist identifiers as the key for the usage map, since
concepturally it is a map from an identifier to how it is used in assignments.
However, in practice we commonly end up with the same textual identifier
appearing in different ways in the netlist.

The most obvious example of this are identifiers that appear as both
`UniqueIdentifier` and `RawIdentifier`. If we track the usage on the raw
identifier, but the `NetDecl` uses the `UniqueIdentifier` then the wrong
declaration may be used in the rendered HDL.

Attempting to fix this by not generating the same textual identifier in
different ways proved difficult, so for now the key type is `Text` instead.
-}

data EntityOrComponent = Entity | Comp | Empty
  deriving Show

pattern NetDecl
  :: Maybe Comment
  -- ^ Note; will be inserted as a comment in target hdl
  -> Identifier
  -- ^ Name of signal
  -> HWType
  -- ^ Type of signal
  -> Declaration
pattern NetDecl note d ty <- NetDecl' note d ty _
  where
    NetDecl note d ty = NetDecl' note d ty Nothing

data PortDirection = In | Out
  deriving (Eq,Ord,Show,Generic,NFData,Hashable)

instance NFData Declaration where
  rnf a = a `seq` ()

-- | Expression Modifier
data Modifier
  = Indexed (HWType,Int,Int) -- ^ Index the expression: (Type of expression,DataCon tag,Field Tag)
  | DC (HWType,Int)          -- ^ See expression in a DataCon context: (Type of the expression, DataCon tag)
  | VecAppend                -- ^ See the expression in the context of a Vector append operation
  | RTreeAppend              -- ^ See the expression in the context of a Tree append operation
  | Sliced (HWType,Int,Int)  -- ^ Slice the identifier of the given type from start to end
  | Nested Modifier Modifier
  deriving Show

-- | Expression used in RHS of a declaration
data Expr
  = Literal    !(Maybe (HWType,Size)) !Literal -- ^ Literal expression
  | DataCon    !HWType       !Modifier  [Expr] -- ^ DataCon application
  | Identifier !Identifier   !(Maybe Modifier) -- ^ Signal reference
  | DataTag    !HWType       !(Either Identifier Identifier) -- ^ @Left e@: tagToEnum\#, @Right e@: dataToTag\#

  -- | Instantiation of a BlackBox expression
  | BlackBoxE
      !Text                    -- ^ Primitive name
      [BlackBoxTemplate]       -- ^ VHDL only: add @library@ declarations
      [BlackBoxTemplate]       -- ^ VHDL only: add @use@ declarations:
      [((Text,Text),BlackBox)] -- ^ Intel/Quartus only: create a @.qsys@ file from given template.
      !BlackBox                -- ^ Template tokens
      !BlackBoxContext         -- ^ Context in which tokens should be rendered
      !Bool                    -- ^ Wrap in parentheses?

  -- | Convert some type to a BitVector.
  | ToBv
      (Maybe Identifier) -- ^ Type prefix
      HWType             -- ^ Type to convert _from_
      Expr               -- ^ Expression to convert to BitVector

  -- | Convert BitVector to some type.
  | FromBv
      (Maybe Identifier) -- ^ Type prefix
      HWType             -- ^ Type to convert _to_
      Expr               -- ^ BitVector to convert

  | IfThenElse Expr Expr Expr
  -- | Do nothing
  | Noop
  deriving Show

instance NFData Expr where
  rnf x = x `seq` ()

isConstExpr :: Expr -> Bool
isConstExpr = \case
  Literal{} -> True
  DataCon _ _ es -> all isConstExpr es
  Identifier{} -> False
  DataTag{} -> False
  BlackBoxE nm _ _ _ _ ctx _
    -- When using SimIO, `reg` creates (in Haskell) the mutable reference to
    -- some value. The blackbox for this however is simply `~ARG[0]`, so if
    -- the argument given is constant, the rendered HDL will also be constant.
    | nm == "Clash.Explicit.SimIO.reg" ->
        all (\(e, _, _) -> isConstExpr e) (bbInputs ctx)
    | otherwise -> False
  ToBv _ _ e -> isConstExpr e
  FromBv _ _ e -> isConstExpr e
  IfThenElse{} -> False
  Noop -> False

-- | Literals used in an expression
data Literal
  = NumLit    !Integer          -- ^ Number literal
  | BitLit    !Bit              -- ^ Bit literal
  | BitVecLit !Integer !Integer -- ^ BitVector literal
  | BoolLit   !Bool             -- ^ Boolean literal
  | VecLit    [Literal]         -- ^ Vector literal
  | StringLit !String           -- ^ String literal
  deriving (Eq,Show)

-- | Bit literal
data Bit
  = H -- ^ High
  | L -- ^ Low
  | U -- ^ Undefined
  | Z -- ^ High-impedance
  deriving (Eq,Show,Typeable,Lift)


toBit :: Integer -- ^ mask
      -> Integer -- ^ value
      -> Bit
toBit m i = if testBit m 0
            then U
            else if testBit i 0 then H else L

-- | Context used to fill in the holes of a BlackBox template
data BlackBoxContext
  = Context
  { bbName :: Text
  -- ^ Blackbox function name (for error reporting)
  , bbResults :: [(Expr,HWType)]
  -- ^ Result names and types. Will typically be a list with a single item.
  -- Multiple result targets will be used for "multi result primitives". See
  -- 'Clash.Normalize.Transformations.setupMultiResultPrim'.
  , bbInputs :: [(Expr,HWType,Bool)]
  -- ^ Argument names, types, and whether it is a literal
  , bbFunctions :: IntMap [(Either BlackBox (Identifier,[Declaration])
                          ,Usage
                          ,[BlackBoxTemplate]
                          ,[BlackBoxTemplate]
                          ,[((Text,Text),BlackBox)]
                          ,BlackBoxContext)]
  -- ^ Function arguments (subset of inputs):
  --
  -- * ( Blackbox Template
  --   , Whether the result should be /reg/ or a /wire/ (Verilog only)
  --   , Partial Blackbox Context
  --   )
  , bbQsysIncName :: [IdentifierText]
  , bbLevel :: Int
  -- ^ The scoping level this context is associated with, ensures that
  -- @~ARGN[k][n]@ holes are only filled with values from this context if @k@
  -- is equal to the scoping level of this context.
  , bbCompName :: Identifier
  -- ^ The component the BlackBox is instantiated in
  , bbCtxName :: Maybe IdentifierText
  -- ^ The "context name", name set by `Clash.Magic.setName`, defaults to the
  -- name of the closest binder
  }
  deriving Show

type BBName = String
type BBHash = Int

data BlackBox
  = BBTemplate BlackBoxTemplate
  | BBFunction BBName BBHash TemplateFunction
  deriving (Generic, NFData, Binary)

data TemplateFunction where
  TemplateFunction
    :: [Int]
    -- ^ Used arguments
    -> (BlackBoxContext -> Bool)
    -- ^ Validation function. Should return 'False' if function can't render
    -- given a certain context.
    -> (forall s . Backend s => BlackBoxContext -> Lazy.State s Doc)
    -- ^ Render function
    -> TemplateFunction

instance Show BlackBox where
  showsPrec d (BBTemplate t) =
    showParen (d > 10) $ ("BBTemplate " ++) . showsPrec 11 t
  showsPrec _ (BBFunction nm hsh _) =
    ("<TemplateFunction(nm=" ++) . shows nm . (", hash=" ++) . shows hsh .
    (")>" ++)

instance NFData TemplateFunction where
  rnf (TemplateFunction is f _) = rnf is `seq` f `seq` ()

-- | __NB__: serialisation doesn't preserve the embedded function
instance Binary TemplateFunction where
  put (TemplateFunction is _ _ ) = put is
  get = (\is -> TemplateFunction is err err) <$> get
    where err = const $ error "TemplateFunction functions can't be preserved by serialisation"

-- | Netlist-level identifier
data NetlistId
  = NetlistId Identifier Type
  -- ^ Identifier generated in the NetlistMonad, always derived from another
  -- 'NetlistId'
  | CoreId Id
  -- ^ An original Core identifier
  | MultiId [Id]
  -- ^ A split identifier (into several sub-identifiers), needed to assign
  -- expressions of types that have to be split apart (e.g. tuples of Files)
  deriving (Eq, Show)

-- | Eliminator for 'NetlistId', fails on 'MultiId'
netlistId1
  :: HasCallStack
  => (Identifier -> r)
  -- ^ Eliminator for Identifiers generated in the NetlistMonad
  -> (Id -> r)
  -- ^ Eliminator for original Core Identifiers
  -> NetlistId
  -> r
netlistId1 f g = \case
  NetlistId i _ -> f i
  CoreId i -> g i
  m -> error ("netlistId1 MultiId: " ++ show m)

-- | Return the type(s) of a 'NetListId', returns multiple types when given a
-- 'MultiId'
netlistTypes
  :: NetlistId
  -> [Type]
netlistTypes = \case
  NetlistId _ t -> [t]
  CoreId i -> [coreTypeOf i]
  MultiId is -> map coreTypeOf is

-- | Return the type of a 'NetlistId', fails on 'MultiId'
netlistTypes1
  :: HasCallStack
  => NetlistId
  -> Type
netlistTypes1 = \case
  NetlistId _ t -> t
  CoreId i -> coreTypeOf i
  m -> error ("netlistTypes1 MultiId: " ++ show m)

-- | Type of declaration, concurrent or sequential
data DeclarationType
  = Concurrent
  | Sequential

emptyBBContext :: Text -> BlackBoxContext
emptyBBContext name
  = Context
  { bbName        = name
  , bbResults     = []
  , bbInputs      = []
  , bbFunctions   = empty
  , bbQsysIncName = []
  , bbLevel       = (-1)
  , bbCompName    = UniqueIdentifier
                      "__NOCOMPNAME__" "__NOCOMPNAME__" []
                      Basic VHDL emptyCallStack
  , bbCtxName     = Nothing
  }

Lens.makeLenses ''NetlistEnv
Lens.makeLenses ''NetlistState

intWidth :: Lens.Getter NetlistEnv Int
intWidth = clashEnv . Lens.to (opt_intWidth . envOpts)

customReprs :: Lens.Getter NetlistEnv CustomReprs
customReprs = clashEnv . Lens.to envCustomReprs

tcCache :: Lens.Getter NetlistEnv TyConMap
tcCache = clashEnv . Lens.to envTyConMap

primitives :: Lens.Getter NetlistEnv CompiledPrimMap
primitives = clashEnv . Lens.to envPrimitives

clashOpts :: Lens.Getter NetlistEnv ClashOpts
clashOpts = clashEnv . Lens.to envOpts

-- | Structures that hold an 'IdentifierSet'
class HasIdentifierSet s where
  identifierSet :: Lens' s IdentifierSet

instance HasIdentifierSet IdentifierSet where
  identifierSet = ($)

instance HasUsageMap NetlistState where
  usageMap = usages

instance HasIdentifierSet s => HasIdentifierSet (s, a) where
  identifierSet = Lens._1 . identifierSet

-- | An "IdentifierSetMonad" supports unique name generation for Clash Netlist
class Monad m => IdentifierSetMonad m where
  identifierSetM :: (IdentifierSet -> IdentifierSet) -> m IdentifierSet

instance IdentifierSetMonad NetlistMonad where
  identifierSetM f = do
    is0 <- Lens.use seenIds
    let is1 = f is0
    seenIds .= is1
    pure is1
  {-# INLINE identifierSetM #-}

instance HasIdentifierSet s => IdentifierSetMonad (Strict.State s) where
  identifierSetM f = do
    is0 <- Lens.use identifierSet
    identifierSet .= f is0
    Lens.use identifierSet
  {-# INLINE identifierSetM #-}

instance HasIdentifierSet s => IdentifierSetMonad (Lazy.State s) where
  identifierSetM f = do
    is0 <- Lens.use identifierSet
    identifierSet .= f is0
    Lens.use identifierSet
  {-# INLINE identifierSetM #-}

instance IdentifierSetMonad m => IdentifierSetMonad (Ap m) where
  identifierSetM = Ap . identifierSetM
