{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd,
                    2017-2018, Google Inc.
                    2020-2023, QBayLogic B.V.
                    2022-2023, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Type and instance definitions for Netlist modules
-}
module Clash.Netlist.Types (
  Declaration (.., NetDecl),
  module Clash.Netlist.Types,
)
where

import Control.DeepSeq
import Control.Lens (Lens', (.=))
import qualified Control.Lens as Lens
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Control.Monad.State as Lazy (State)
import qualified Control.Monad.State.Strict as Strict (
  MonadIO,
  MonadState,
  State,
  StateT,
 )
import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as Aeson
import Data.Binary (Binary (..))
import Data.Bits (testBit)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.IntMap (IntMap, empty)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Ordered (OMap)
import Data.Maybe (mapMaybe)
import Data.Monoid (Ap (..))
import qualified Data.Set as Set
import Data.Text (Text)

import Data.Text.Prettyprint.Doc.Extra (Doc)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Stack
import Language.Haskell.TH.Syntax (Lift)

import GHC.Types.SrcLoc (SrcSpan)

import Clash.Annotations.BitRepresentation (FieldAnn)
import Clash.Annotations.Primitive (HDL (..))
import Clash.Annotations.SynthesisAttributes (Attr)
import Clash.Annotations.TopEntity (TopEntity)
import Clash.Backend (Backend, HasUsageMap (..))
import Clash.Core.HasType
import Clash.Core.PartialEval (Evaluator)
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type (Type)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (VarEnv)
import Clash.Driver.Types (
  BindingMap,
  ClashEnv (..),
  ClashOpts (..),
 )
import Clash.Netlist.BlackBox.Types (BlackBoxTemplate)
import Clash.Primitives.Types (CompiledPrimMap)
import Clash.Signal.Internal (
  ActiveEdge,
  InitBehavior,
  ResetKind,
  ResetPolarity,
 )
import Clash.Unique (Unique)

import Clash.Annotations.BitRepresentation.Internal (
  ConstrRepr',
  CustomReprs,
  DataRepr',
 )

import {-# SOURCE #-} qualified Clash.Netlist.Id as Id (toText)

-- | Structure describing a top entity: it's id and its port annotations.
data TopEntityT = TopEntityT
  { topId :: Id
  -- ^ Id of top entity
  , topAnnotation :: Maybe TopEntity
  -- ^ (Maybe) a topentity annotation
  , topIsTestBench :: Bool
  -- ^ Whether this entity is a test bench
  }
  deriving (Generic, Show, Eq)

-- | Same as "TopEntity", but with all port names that end up in HDL specified
data ExpandedTopEntity a = ExpandedTopEntity
  { et_inputs :: [Maybe (ExpandedPortName a)]
  -- ^ Inputs with fully expanded port names. /Nothing/ if port is void.
  , et_output :: Maybe (ExpandedPortName a)
  {- ^ Output with fully expanded port names. /Nothing/ if port is void or
  BiDirectionalOut.
  -}
  }
  deriving (Show, Functor, Foldable, Traversable)

-- | See "ExpandedTopEntity"
data ExpandedPortName a
  = -- | Same as "PortName", but fully expanded
    ExpandedPortName HWType a
  | -- | Same as "PortProduct", but fully expanded
    ExpandedPortProduct
      -- | Name hint. Can be used to create intermediate signal names.
      Text
      -- | Type of product
      HWType
      -- | Product fields
      [ExpandedPortName a]
  deriving (Show, Functor, Foldable, Traversable)

{- | Monad that caches generated components (StateT) and remembers hidden inputs
of components that are being generated (WriterT)
-}
newtype NetlistMonad a
  = NetlistMonad {runNetlist :: Strict.StateT NetlistState (ReaderT NetlistEnv IO) a}
  deriving newtype
    ( Functor
    , Monad
    , Applicative
    , MonadReader NetlistEnv
    , Strict.MonadState NetlistState
    , Strict.MonadIO
    , MonadFail
    )

type HWMap = Map Type (Either String FilteredHWType)

-- | See 'is_freshCache'
type FreshCache = HashMap Text (IntMap Word)

type IdentifierText = Text

{- | Whether to preserve casing in ids or converted everything to
 lowercase. Influenced by '-fclash-lower-case-basic-identifiers'
-}
data PreserveCase
  = PreserveCase
  | ToLower
  deriving (Show, Generic, NFData, Eq, Binary, Hashable)

-- See: http://vhdl.renerta.com/mobile/source/vhd00037.htm
--      http://www.verilog.renerta.com/source/vrg00018.htm
data IdentifierType
  = {- | A basic identifier: does not have to be escaped in order to be a valid
    identifier in HDL.
    -}
    Basic
  | {- | An extended identifier: has to be escaped, wrapped, or otherwise
    postprocessed before writhing it to HDL.
    -}
    Extended
  deriving (Show, Generic, NFData, Eq)

{- | A collection of unique identifiers. Allows for fast fresh identifier
generation.

__NB__: use the functions in Clash.Netlist.Id. Don't use the constructor directly.
-}
data IdentifierSet
  = IdentifierSet
  { is_allowEscaped :: !Bool
  {- ^ Allow escaped ids? If set to False, "make" will always behave like
  "makeBasic".
  -}
  , is_lowerCaseBasicIds :: !PreserveCase
  -- ^ Force all generated basic identifiers to lowercase.
  , is_hdl :: !HDL
  -- ^ HDL to generate fresh identifiers for
  , is_freshCache :: !FreshCache
  {- ^ Maps an 'i_baseNameCaseFold' to a map mapping the number of
  extensions (in 'i_extensionsRev') to the maximum word at that
  basename/level. For example, if a set would contain the identifiers:

  [foo, foo_1, foo_2, bar_5, bar_7_8]

  the map would look like:

  [(foo, [(0, 0), (1, 2)]), (bar, [(1, 5), (2, 8)])]

  This mapping makes sure we can quickly generate fresh identifiers. For
  example, generating a new id for "foo_1" would be a matter of looking
  up the base name in this map, concluding that the maximum identifier
  with this basename and this number of extensions is "foo_2",
  subsequently generating "foo_3".

  Note that an identifier with no extensions is also stored in this map
  for practical purposes, but the maximum ext is invalid.
  -}
  , is_store :: !(HashSet Identifier)
  -- ^ Identifier store
  }
  deriving (Generic, NFData, Show)

{- | HDL identifier. Consists of a base name and a number of extensions. An
identifier with a base name of "foo" and a list of extensions [1, 2] will be
rendered as "foo_1_2".

Note: The Eq instance of "Identifier" is case insensitive! E.g., two
identifiers with base names 'fooBar' and 'FoObAR' are considered the same.
However, identifiers are stored case preserving. This means Clash won't
generate two identifiers with differing case, but it will try to keep
capitalization.

The goal of this data structure is to greatly simplify how Clash deals with
identifiers internally. Any Identifier should be trivially printable to any
HDL.

__NB__: use the functions in "Clash.Netlist.Id". Don't use these constructors
directly.
-}
data Identifier
  = {- | Unparsed identifier. Used for things such as port names, which should
    appear in the HDL exactly as the user specified.
    -}
    RawIdentifier
      -- | An identifier exactly as given by the user
      !Text
      {- | Parsed version of raw identifier. Will not be populated if this
      identifier was created with an unsafe function.
      -}
      (Maybe Identifier)
      {- | Stores where this identifier was generated. Tracking is only enabled
      is 'debugIsOn', otherwise this field will be populated by an empty
      callstack.
      -}
      !CallStack
  | {- | Parsed and sanitized identifier. See various fields for more information
    on its invariants.
    -}
    UniqueIdentifier
      { i_baseName :: !Text
      {- ^ Base name of identifier. 'make' makes sure this field:

      * does not end in '_num' where 'num' is a digit.
      * is solely made up of printable ASCII characters
      * has no leading or trailing whitespace
      -}
      , i_baseNameCaseFold :: !Text
      {- ^ Same as 'i_baseName', but can be used for equality testing that doesn't
      depend on capitalization.
      -}
      , i_extensionsRev :: [Word]
      {- ^ Extensions applied to base identifier. E.g., an identifier with a base
      name of 'foo' and an extension of [6, 5] would render as 'foo_5_6'. Note
      that extensions are stored in reverse order for easier manipulation.
      -}
      , i_idType :: !IdentifierType
      -- ^ See 'IdentifierType'.
      , i_hdl :: !HDL
      -- ^ HDL this identifier is generated for.
      , i_provenance :: !CallStack
      {- ^ Stores where this identifier was generated. Tracking is only enabled
      is 'debugIsOn', otherwise this field will be populated by an empty
      callstack.
      -}
      }
  deriving (Show, Generic, NFData)

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
      let fuzz fuzzFactor ext = fuzzFactor * fuzzFactor * ext
       in hash (a, List.foldl' fuzz 2 extensions)

instance Eq Identifier where
  i1 == i2 = identifierKey# i1 == identifierKey# i2
  i1 /= i2 = identifierKey# i1 /= identifierKey# i2

instance Ord Identifier where
  compare = compare `on` identifierKey#

-- | Environment of the NetlistMonad
data NetlistEnv
  = NetlistEnv
  { _clashEnv :: ClashEnv
  , _prefixName :: Text
  -- ^ Prefix for instance/register names
  , _suffixName :: Text
  -- ^ Postfix for instance/register names
  , _setName :: Maybe Text
  -- ^ (Maybe) user given instance/register name
  , _localAttrs :: [Attr Text]
  {- ^ Synthesis attributes brough into scope by
  'Clash.Annotations.SynthesisAttributes.annotateReg'
  -}
  , _peEvaluator :: Evaluator
  -- ^ Evaluator to evaluate a term to Normal Form
  }

data ComponentMeta = ComponentMeta
  { cmWereVoids :: [Bool]
  , cmLoc :: SrcSpan
  , cmScope :: IdentifierSet
  , cmUsage :: UsageMap
  }
  deriving (Generic, Show, NFData)

type ComponentMap = OMap Unique (ComponentMeta, Component)

-- | State of the NetlistMonad
data NetlistState
  = NetlistState
  { _bindings :: BindingMap
  -- ^ Global binders
  , _components :: ComponentMap
  {- ^ Cached components. Is an insertion ordered map to preserve a topologically
  sorted component list for the manifest file.
  -}
  , _typeTranslator ::
      CustomReprs ->
      TyConMap ->
      Type ->
      Strict.State HWMap (Maybe (Either String FilteredHWType))
  -- ^ Hardcoded Type -> HWType translator
  , _curCompNm :: !(Identifier, SrcSpan)
  , _seenIds :: IdentifierSet
  -- ^ All names currently in scope.
  , _seenComps :: IdentifierSet
  {- ^ Components (to be) generated during this netlist run. This is always a
  subset of 'seenIds'. Reason d'etre: we currently generate components in a
  top down manner. E.g. given:

  - A
  -- B
  -- C

  we would generate component 'A' first. Before trying to generate 'B' and
  'C'. 'A' might introduce a number of signal declarations. The names of these
  signals can't clash with the name of component 'B', hence we need to pick a
  name for B unique w.r.t. all these signal names. If we would postpone
  generating a unqiue name for 'B' til _after_ generating all the signal
  names, the signal names would get all the "nice" names. E.g., a signal
  would be called "foo", thereby forcing the component 'B' to be called
  "foo_1". Ideally, we'd use the "nice" names for components, and the "ugly"
  names for signals. To achieve this, we generate all the component names
  up front and subsequently store them in '_seenComps'.
  -}
  , _seenPrimitives :: Set.Set Text
  {- ^ Keeps track of invocations of ´mkPrimitive´. It is currently used to
  filter duplicate warning invocations for dubious blackbox instantiations,
  see GitHub pull request #286.
  -}
  , _componentNames :: VarEnv Identifier
  {- ^ Names of components (to be) generated during this netlist run. Includes
  top entity names.
  -}
  , _topEntityAnns :: VarEnv TopEntityT
  , _hdlDir :: FilePath
  , _curBBlvl :: Int
  -- ^ The current scoping level assigned to black box contexts
  , _isTestBench :: Bool
  -- ^ Whether we're compiling a testbench (suppresses some warnings)
  , _backEndITE :: Bool
  -- ^ Whether the backend supports ifThenElse expressions
  , _backend :: SomeBackend
  -- ^ The current HDL backend
  , _htyCache :: HWMap
  , _usages :: UsageMap
  {- ^ The current way signals are assigned in netlist. This is used to
  determine how signals are rendered in HDL (i.e. wire/reg in Verilog, or
  signal/variable in VHDL).
  -}
  }

data ComponentPrefix
  = ComponentPrefix
  { componentPrefixTop :: Maybe Text
  -- ^ Prefix for top-level components
  , componentPrefixOther :: Maybe Text
  -- ^ Prefix for all other components
  }
  deriving (Show)

-- | Existentially quantified backend
data SomeBackend where
  SomeBackend :: (Backend backend) => backend -> SomeBackend

onSomeBackend :: (forall b. (Backend b) => b -> a) -> SomeBackend -> a
onSomeBackend f (SomeBackend b) = f b

fromSomeBackend :: (forall b. (Backend b) => b -> a) -> Lens.Getter SomeBackend a
fromSomeBackend f = Lens.to (onSomeBackend f)

type Comment = Text
type Directive = Text

data CommentOrDirective
  = Comment Comment
  | Directive Directive
  deriving (Show)

-- | Component: base unit of a Netlist
data Component
  = Component
  { componentName :: !Identifier
  -- ^ Name of the component
  , inputs :: [(Identifier, HWType)]
  -- ^ Input ports
  , outputs :: [(Usage, (Identifier, HWType), Maybe Expr)]
  -- ^ Output ports
  , declarations :: [Declaration]
  -- ^ Internal declarations
  }
  deriving (Show, Generic, NFData)

-- | Check if an input port is really an inout port.
isBiDirectional :: (Identifier, HWType) -> Bool
isBiDirectional = go . snd
 where
  go BiDirectional{} = True
  go (Annotated _ hwty) = go hwty
  go _ = False

{- | Find the name and domain name of each clock argument of a component.

This will not consider @ClockN@ to be a clock argument, which means only the
positive phase of a differential pair will be added to @sdcClock@.
-}
findClocks :: Component -> [(Text, Text)]
findClocks (Component _ is _ _) =
  mapMaybe isClock is
 where
  isClock (i, Clock d) = Just (Id.toText i, d)
  isClock (i, Annotated _ t) = isClock (i, t)
  isClock _ = Nothing

-- | Size indication of a type (e.g. bit-size or number of elements)
type Size = Int

type IsVoid = Bool

{- | Tree structure indicating which constructor fields were filtered from
a type due to them being void. We need this information to generate stable
and/or user-defined port mappings.
-}
data FilteredHWType
  = FilteredHWType HWType [[(IsVoid, FilteredHWType)]]
  deriving (Eq, Show)

type DomainName = Text

-- | Representable hardware types
data HWType
  = {- | Empty type. @Just Size@ for "empty" Vectors so we can still have
    primitives that can traverse e.g. Vectors of unit and know the length of
    that vector.
    -}
    Void (Maybe HWType)
  | -- | String type
    String
  | -- | Integer type (for parameters only)
    Integer
  | -- | Boolean type
    Bool
  | -- | Bit type
    Bit
  | -- | BitVector of a specified size
    BitVector !Size
  | -- | Unsigned integer with specified (exclusive) upper bounder
    Index !Integer
  | -- | Signed integer of a specified size
    Signed !Size
  | -- | Unsigned integer of a specified size
    Unsigned !Size
  | -- | Vector type
    Vector !Size !HWType
  | -- | MemBlob type
    MemBlob !Size !Size
  | -- | RTree type
    RTree !Size !HWType
  | -- | Sum type: Name and Constructor names
    Sum !Text [Text]
  | {- | Product type: Name, field names, and field types. Field names will be
    populated when using records.
    -}
    Product !Text (Maybe [Text]) [HWType]
  | -- | Sum-of-Product type: Name and Constructor names + field types
    SP !Text [(Text, [HWType])]
  | -- | Clock type corresponding to domain /DomainName/
    Clock !DomainName
  | -- | ClockN type corresponding to domain /DomainName/
    ClockN !DomainName
  | -- | Reset type corresponding to domain /DomainName/
    Reset !DomainName
  | -- | Enable type corresponding to domain /DomainName/
    Enable !DomainName
  | -- | Tagging type indicating a bidirectional (inout) port
    BiDirectional !PortDirection !HWType
  | {- | Same as Sum-Of-Product, but with a user specified bit representation. For
    more info, see: Clash.Annotations.BitRepresentations.
    -}
    CustomSP !Text !DataRepr' !Size [(ConstrRepr', Text, [HWType])]
  | {- | Same as Sum, but with a user specified bit representation. For more info,
    see: Clash.Annotations.BitRepresentations.
    -}
    CustomSum !Text !DataRepr' !Size [(ConstrRepr', Text)]
  | {- | Same as Product, but with a user specified bit representation. For more
    info, see: Clash.Annotations.BitRepresentations.
    -}
    CustomProduct !Text !DataRepr' !Size (Maybe [Text]) [(FieldAnn, HWType)]
  | -- | Annotated with HDL attributes
    Annotated [Attr Text] !HWType
  | -- | Domain name, period, active edge, reset kind, initial value behavior
    KnownDomain !DomainName !Integer !ActiveEdge !ResetKind !InitBehavior !ResetPolarity
  | -- | File type for simulation-level I/O
    FileType
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

{- | Smart constructor for 'Annotated'. Wraps the given type in an 'Annotated'
if the attribute list is non-empty. If it is empty, it will return the given
'HWType' unchanged.
-}
annotated :: [Attr Text] -> HWType -> HWType
annotated [] t = t
annotated attrs t = Annotated attrs t

hwTypeDomain :: HWType -> Maybe DomainName
hwTypeDomain = \case
  Clock dom -> Just dom
  ClockN dom -> Just dom
  Reset dom -> Just dom
  Enable dom -> Just dom
  KnownDomain dom _ _ _ _ _ -> Just dom
  _ -> Nothing

{- | Extract hardware attributes from Annotated. Returns an empty list if
non-Annotated given or if Annotated has an empty list of attributes.
-}
hwTypeAttrs :: HWType -> [Attr Text]
hwTypeAttrs (Annotated attrs _type) = attrs
hwTypeAttrs _ = []

-- | Specifies how to wire up a component instance
data PortMap
  = {- | Port map based on port positions (port direction, type, assignment)

    HDL Example:

    bytemaster bytemaster_ds
      ( clk_1
      , rst_1
      , bitCtrl_0 );
    -}
    IndexedPortMap [(PortDirection, HWType, Expr)]
  | {- | Port map based on port names (port name, port direction, type, assignment)

    HDL Example:

    bytemaster bytemaster_ds
      ( .clk (clk_1)
      , .rst (rst_1)
      , .bitCtrl (bitCtrl_0) );
    -}
    NamedPortMap [(Expr, PortDirection, HWType, Expr)]
  deriving (Show)

-- | Internals of a Component
data Declaration
  = -- | Signal assignment
    Assignment
      -- | Signal to assign
      !Identifier
      -- | How the signal is assigned
      !Usage
      -- | Assigned expression
      !Expr
  | -- | Conditional signal assignment:
    CondAssignment
      -- | Signal to assign
      !Identifier
      -- | Type of the result/alternatives
      !HWType
      -- | Scrutinized expression
      !Expr
      -- | Type of the scrutinee
      !HWType
      -- | List of: (Maybe expression scrutinized expression is compared with,RHS of alternative)
      [(Maybe Literal, Expr)]
  | -- | Instantiation of another component:
    InstDecl
      -- | Whether it's an entity or a component
      EntityOrComponent
      -- | Library instance is defined in
      (Maybe Text)
      -- | Attributes to add to the generated code
      [Attr Text]
      -- | The component's (or entity's) name
      !Identifier
      -- | Instance label
      !Identifier
      -- | List of parameters for this component (param name, param type, param value)
      [(Expr, HWType, Expr)]
      PortMap
  | -- | Instantiation of blackbox declaration
    BlackBoxD
      -- | Primitive name
      !Text
      -- | VHDL only: add @library@ declarations
      [BlackBoxTemplate]
      -- | VHDL only: add @use@ declarations
      [BlackBoxTemplate]
      -- | Intel Quartus only: create a @.qsys@ file from given template
      [((Text, Text), BlackBox)]
      -- | Template tokens
      !BlackBox
      -- | Context in which tokens should be rendered
      BlackBoxContext
  | {- | @component@ declaration (VHDL).

    See [this tutorial](https://www.ics.uci.edu/~jmoorkan/vhdlref/compdec.html);
    refer to §4.5 of IEEE 1076-1993
    -}
    CompDecl
      !Text
      [(Text, PortDirection, HWType)]
  | -- | Signal declaration
    NetDecl'
      -- | Note; will be inserted as a comment in target hdl
      (Maybe Comment)
      -- | Name of signal
      !Identifier
      -- | Type of signal
      HWType
      {- | Initial value
      ^ Signal declaration
      -}
      (Maybe Expr)
  | -- | HDL tick corresponding to a Core tick
    TickDecl CommentOrDirective
  | -- | Sequential statement
    Seq [Seq]
  | {- | Compilation conditional on some preprocessor symbol, note that
    declarations here are ignored for VHDL. See here for a discussion
    https://github.com/clash-lang/clash-compiler/pull/1798#discussion_r648571862
    -}
    ConditionalDecl
      -- | condition text, for example @FORMAL@
      !Text
      [Declaration]
  deriving (Show)

-- | Sequential statements
data Seq
  = -- | Clocked sequential statements
    AlwaysClocked
      -- | Edge of the clock the statement should be executed
      ActiveEdge
      -- | Clock expression
      Expr
      {- | Statements to be executed on the active clock edge
      | Statements running at simulator start
      -}
      [Seq]
  | {- | Statements to run at simulator start
    | Statements to run always
    -}
    Initial
      [Seq]
  | {- | Statements to run always
    | Declaration in sequential form
    -}
    AlwaysComb
      [Seq]
  | {- | The declaration
    | Branching statement
    -}
    SeqDecl
      Declaration
  | Branch
      -- | Scrutinized expresson
      !Expr
      -- | Type of the scrutinized expression
      !HWType
      -- | List of: (Maybe match, RHS of Alternative)
      [(Maybe Literal, [Seq])]
  deriving (Show)

{- | Procedural assignment in HDL can be blocking or non-blocking. This
determines when the assignment takes place in simulation. The name refers to
whether evaluation of the remaining statements in a process is blocked
until the assignment is performed or not.

See Also:

IEEE 1364-2001, sections 9.2.1 and 9.2.2
IEEE 1076-1993, sections 8.4 and 8.5
-}
data Blocking
  = {- | A non-blocking assignment means the new value is not observed until the
    next time step in simulation. Using the signal later in the process will
    continue to return the old value.
    -}
    NonBlocking
  | {- | A blocking assignment means the new value is observed immediately. Using
    the signal later in the process will return the new value.
    -}
    Blocking
  deriving (Binary, Eq, Generic, Hashable, NFData, Show)

-- NOTE [`Semigroup` instances for `Blocking` and `Usage`]
instance Semigroup Blocking where
  NonBlocking <> y = y
  Blocking <> _ = Blocking

{- | The usage of a signal refers to how the signal is written to in netlist.
This is used to determine if the signal should be a @wire@ or @reg@ in
(System)Verilog, or a @signal@ or @variable@ in VHDL.
-}
data Usage
  = -- | Continuous assignment, which occurs in a concurrent context.
    Cont
  | -- | Procedural assignment, which occurs in a sequential context.
    Proc Blocking
  deriving (Binary, Eq, Generic, Hashable, NFData, Show)

-- NOTE [`Semigroup` instances for `Blocking` and `Usage`]
instance Semigroup Usage where
  Cont <> y = y
  Proc x <> Proc y = Proc (x <> y)
  Proc x <> _ = Proc x

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
\|---------|-------------------------------------|
\|    VHDL |         signal           | variable |
\|---------|-------------------------------------|
\| Verilog |   wire     |          reg           |
\|---------|-------------------------------------|
-}

instance FromJSON Usage where
  parseJSON = Aeson.withText "Usage" $ \case
    "Continuous" -> pure Cont
    "NonBlocking" -> pure (Proc NonBlocking)
    "Blocking" -> pure (Proc Blocking)
    str ->
      fail $
        mconcat
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
conceptually it is a map from an identifier to how it is used in assignments.
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
  deriving (Show)

pattern NetDecl ::
  -- | Note; will be inserted as a comment in target hdl
  Maybe Comment ->
  -- | Name of signal
  Identifier ->
  -- | Type of signal
  HWType ->
  Declaration
pattern NetDecl note d ty <- NetDecl' note d ty _
  where
    NetDecl note d ty = NetDecl' note d ty Nothing

data PortDirection = In | Out
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance NFData Declaration where
  rnf a = a `seq` ()

-- | Expression Modifier
data Modifier
  = {- | Index the expression: (Type of expression, DataCon tag, Field Tag). Note
    that the type of the expression is the type we are slicing from, not the type
    returned by the index operation.
    -}
    Indexed (HWType, Int, Int)
  | -- | See expression in a DataCon context: (Type of the expression, DataCon tag)
    DC (HWType, Int)
  | -- | See the expression in the context of a Vector append operation
    VecAppend
  | -- | See the expression in the context of a Tree append operation
    RTreeAppend
  | -- | Slice the identifier of the given type from start to end
    Sliced (HWType, Int, Int)
  | Nested Modifier Modifier
  deriving (Show)

-- | Expression used in RHS of a declaration
data Expr
  = -- | Literal expression
    Literal !(Maybe (HWType, Size)) !Literal
  | -- | DataCon application
    DataCon !HWType !Modifier [Expr]
  | -- | Signal reference
    Identifier !Identifier !(Maybe Modifier)
  | -- | @Left e@: tagToEnum\#, @Right e@: dataToTag\#
    DataTag !HWType !(Either Identifier Identifier)
  | -- | Instantiation of a BlackBox expression
    BlackBoxE
      -- | Primitive name
      !Text
      -- | VHDL only: add @library@ declarations
      [BlackBoxTemplate]
      -- | VHDL only: add @use@ declarations:
      [BlackBoxTemplate]
      -- | Intel/Quartus only: create a @.qsys@ file from given template.
      [((Text, Text), BlackBox)]
      -- | Template tokens
      !BlackBox
      -- | Context in which tokens should be rendered
      !BlackBoxContext
      -- | Wrap in parentheses?
      !Bool
  | -- | Convert some type to a BitVector.
    ToBv
      -- | Type prefix
      (Maybe Identifier)
      -- | Type to convert _from_
      HWType
      -- | Expression to convert to BitVector
      Expr
  | -- | Convert BitVector to some type.
    FromBv
      -- | Type prefix
      (Maybe Identifier)
      -- | Type to convert _to_
      HWType
      -- | BitVector to convert
      Expr
  | IfThenElse Expr Expr Expr
  | -- | Do nothing
    Noop
  deriving (Show)

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
  = -- | Number literal
    NumLit !Integer
  | -- | Bit literal
    BitLit !Bit
  | -- | BitVector literal
    BitVecLit !Integer !Integer
  | -- | Boolean literal
    BoolLit !Bool
  | -- | Vector literal
    VecLit [Literal]
  | -- | String literal
    StringLit !String
  deriving (Eq, Show)

-- | Bit literal
data Bit
  = -- | High
    H
  | -- | Low
    L
  | -- | Undefined
    U
  | -- | High-impedance
    Z
  deriving (Eq, Show, Typeable, Lift)

toBit ::
  -- | mask
  Integer ->
  -- | value
  Integer ->
  Bit
toBit m i =
  if testBit m 0
    then U
    else if testBit i 0 then H else L

-- | Context used to fill in the holes of a BlackBox template
data BlackBoxContext
  = Context
  { bbName :: Text
  -- ^ Blackbox function name (for error reporting)
  , bbResults :: [(Expr, HWType)]
  {- ^ Result names and types. Will typically be a list with a single item.
  Multiple result targets will be used for "multi result primitives". See
  'Clash.Normalize.Transformations.setupMultiResultPrim'.
  -}
  , bbInputs :: [(Expr, HWType, Bool)]
  -- ^ Argument names, types, and whether it is a literal
  , bbFunctions ::
      IntMap
        [ ( Either BlackBox (Identifier, [Declaration])
          , Usage
          , [BlackBoxTemplate]
          , [BlackBoxTemplate]
          , [((Text, Text), BlackBox)]
          , BlackBoxContext
          )
        ]
  {- ^ Function arguments (subset of inputs):

  * ( Blackbox Template
  , Whether the result should be /reg/ or a /wire/ (Verilog only)
  , Partial Blackbox Context
  )
  -}
  , bbQsysIncName :: [IdentifierText]
  , bbLevel :: Int
  {- ^ The scoping level this context is associated with, ensures that
  @~ARGN[k][n]@ holes are only filled with values from this context if @k@
  is equal to the scoping level of this context.
  -}
  , bbCompName :: Identifier
  -- ^ The component the BlackBox is instantiated in
  , bbCtxName :: Maybe IdentifierText
  {- ^ The "context name", name set by `Clash.Magic.setName`, defaults to the
  name of the closest binder
  -}
  }
  deriving (Show)

type BBName = String
type BBHash = Int

data BlackBox
  = BBTemplate BlackBoxTemplate
  | BBFunction BBName BBHash TemplateFunction
  deriving (Generic, NFData, Binary)

data TemplateFunction where
  TemplateFunction ::
    -- | Used arguments
    [Int] ->
    {- | Validation function. Should return 'False' if function can't render
    given a certain context.
    -}
    (BlackBoxContext -> Bool) ->
    -- | Render function
    (forall s. (Backend s) => BlackBoxContext -> Lazy.State s Doc) ->
    TemplateFunction

instance Show BlackBox where
  showsPrec d (BBTemplate t) =
    showParen (d > 10) $ ("BBTemplate " ++) . showsPrec 11 t
  showsPrec _ (BBFunction nm hsh _) =
    ("<TemplateFunction(nm=" ++)
      . shows nm
      . (", hash=" ++)
      . shows hsh
      . (")>" ++)

instance NFData TemplateFunction where
  rnf (TemplateFunction is f _) = rnf is `seq` f `seq` ()

-- | __NB__: serialisation doesn't preserve the embedded function
instance Binary TemplateFunction where
  put (TemplateFunction is _ _) = put is
  get = (\is -> TemplateFunction is err err) <$> get
   where
    err = const $ error "TemplateFunction functions can't be preserved by serialisation"

-- | Netlist-level identifier
data NetlistId
  = {- | Identifier generated in the NetlistMonad, always derived from another
    'NetlistId'
    -}
    NetlistId Identifier Type
  | -- | An original Core identifier
    CoreId Id
  | {- | A split identifier (into several sub-identifiers), needed to assign
    expressions of types that have to be split apart (e.g. tuples of Files)
    -}
    MultiId [Id]
  deriving (Eq, Show)

-- | Eliminator for 'NetlistId', fails on 'MultiId'
netlistId1 ::
  (HasCallStack) =>
  -- | Eliminator for Identifiers generated in the NetlistMonad
  (Identifier -> r) ->
  -- | Eliminator for original Core Identifiers
  (Id -> r) ->
  NetlistId ->
  r
netlistId1 f g = \case
  NetlistId i _ -> f i
  CoreId i -> g i
  m -> error ("netlistId1 MultiId: " ++ show m)

{- | Return the type(s) of a 'NetListId', returns multiple types when given a
'MultiId'
-}
netlistTypes ::
  NetlistId ->
  [Type]
netlistTypes = \case
  NetlistId _ t -> [t]
  CoreId i -> [coreTypeOf i]
  MultiId is -> map coreTypeOf is

-- | Return the type of a 'NetlistId', fails on 'MultiId'
netlistTypes1 ::
  (HasCallStack) =>
  NetlistId ->
  Type
netlistTypes1 = \case
  NetlistId _ t -> t
  CoreId i -> coreTypeOf i
  m -> error ("netlistTypes1 MultiId: " ++ show m)

-- | Type of declaration, concurrent or sequential
data DeclarationType
  = Concurrent
  | Sequential

-- | Default usage for a type of declaration (concurrent or sequential)
declTypeUsage :: DeclarationType -> Usage
declTypeUsage Concurrent = Cont
declTypeUsage Sequential = Proc Blocking

emptyBBContext :: Text -> BlackBoxContext
emptyBBContext name =
  Context
    { bbName = name
    , bbResults = []
    , bbInputs = []
    , bbFunctions = empty
    , bbQsysIncName = []
    , bbLevel = (-1)
    , bbCompName =
        UniqueIdentifier
          "__NOCOMPNAME__"
          "__NOCOMPNAME__"
          []
          Basic
          VHDL
          emptyCallStack
    , bbCtxName = Nothing
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

instance (HasIdentifierSet s) => HasIdentifierSet (s, a) where
  identifierSet = Lens._1 . identifierSet

-- | An "IdentifierSetMonad" supports unique name generation for Clash Netlist
class (Monad m) => IdentifierSetMonad m where
  identifierSetM :: (IdentifierSet -> IdentifierSet) -> m IdentifierSet

instance IdentifierSetMonad NetlistMonad where
  identifierSetM f = do
    is0 <- Lens.use seenIds
    let is1 = f is0
    seenIds .= is1
    pure is1
  {-# INLINE identifierSetM #-}

instance (HasIdentifierSet s) => IdentifierSetMonad (Strict.State s) where
  identifierSetM f = do
    is0 <- Lens.use identifierSet
    identifierSet .= f is0
    Lens.use identifierSet
  {-# INLINE identifierSetM #-}

instance (HasIdentifierSet s) => IdentifierSetMonad (Lazy.State s) where
  identifierSetM f = do
    is0 <- Lens.use identifierSet
    identifierSet .= f is0
    Lens.use identifierSet
  {-# INLINE identifierSetM #-}

instance (IdentifierSetMonad m) => IdentifierSetMonad (Ap m) where
  identifierSetM = Ap . identifierSetM
