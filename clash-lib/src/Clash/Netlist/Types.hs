{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd,
                    2017-2018, Google Inc.
                    2020-2022, QBayLogic B.V.
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
  , module Clash.Netlist.Types
  )
where

import Control.DeepSeq
import qualified Control.Lens               as Lens
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail                   (MonadFail)
#endif
import Control.Monad.Reader                 (ReaderT, MonadReader)
import qualified Control.Monad.State        as Lazy (State)
import qualified Control.Monad.State.Strict as Strict
  (State, MonadIO, MonadState, StateT, state)
import Data.Bits                            (testBit)
import Data.Binary                          (Binary(..))
import Data.IntMap                          (IntMap, empty)
import Data.Map.Ordered                     (OMap)
import Data.Map                             (Map)
import Data.Maybe                           (mapMaybe)
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

import Clash.Annotations.Primitive          (HDL(..))
import Clash.Annotations.TopEntity          (TopEntity)
import Clash.Backend                        (Backend, SomeBackend)
import Clash.Core.HasType
import Clash.Core.Type                      (Type)
import Clash.Core.Var                       (Attr', Id)
import Clash.Core.TyCon                     (TyConMap)
import Clash.Core.VarEnv                    (VarEnv)
import Clash.Driver.Types                   (BindingMap, ClashOpts)
import Clash.Netlist.Ast.Type
import Clash.Netlist.BlackBox.Types         (BlackBoxTemplate)
import Clash.Netlist.Id
import qualified Clash.Netlist.Id as Id
import Clash.Primitives.Types               (CompiledPrimMap)
import Clash.Signal.Internal                (ActiveEdge)
import Clash.Unique                         (Unique)

import Clash.Annotations.BitRepresentation.Internal (CustomReprs)


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

instance IdentifierSetMonad NetlistMonad where
  identifierSetM f =
    Strict.state $ \st ->
      let is = f (_seenIds st)
       in (is, st { _seenIds = is })
  {-# INLINE identifierSetM #-}

type HWMap = Map Type (Either String FilteredHWType)

-- | Environment of the NetlistMonad
data NetlistEnv
  = NetlistEnv
  { _prefixName  :: Text
  -- ^ Prefix for instance/register names
  , _suffixName :: Text
  -- ^ Postfix for instance/register names
  , _setName :: Maybe Text
  -- ^ (Maybe) user given instance/register name
  }

type ComponentMap = OMap Unique Component

-- | State of the NetlistMonad
data NetlistState
  = NetlistState
  { _bindings       :: BindingMap
  -- ^ Global binders
  , _components     :: ComponentMap
  -- ^ Cached components. Is an insertion ordered map to preserve a topologically
  -- sorted component list for the manifest file.
  , _primitives     :: CompiledPrimMap
  -- ^ Primitive Definitions
  , _typeTranslator :: CustomReprs -> TyConMap -> Type
                    -> Strict.State HWMap (Maybe (Either String FilteredHWType))
  -- ^ Hardcoded Type -> HWType translator
  , _tcCache        :: TyConMap
  -- ^ TyCon cache
  , _curCompNm      :: !(Identifier,SrcSpan)
  , _intWidth       :: Int
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
  , _customReprs    :: CustomReprs
  , _clashOpts      :: ClashOpts
  -- ^ Settings Clash was called with
  , _isTestBench    :: Bool
  -- ^ Whether we're compiling a testbench (suppresses some warnings)
  , _backEndITE :: Bool
  -- ^ Whether the backend supports ifThenElse expressions
  , _backend :: SomeBackend
  -- ^ The current HDL backend
  , _htyCache :: HWMap
  }

data ComponentPrefix
  = ComponentPrefix
  { componentPrefixTop :: Maybe Text
    -- ^ Prefix for top-level components
  , componentPrefixOther :: Maybe Text
    -- ^ Prefix for all other components
  } deriving Show

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
  , outputs       :: [(WireOrReg,(Identifier,HWType),Maybe Expr)] -- ^ Output ports
  , declarations  :: [Declaration] -- ^ Internal declarations
  , componentVoids :: [Bool]
  , componentLoc :: SrcSpan
  , componentScope :: IdentifierSet
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
findClocks = mapMaybe isClock . inputs
 where
  isClock (i, Clock d) = Just (Id.toText i, d)
  isClock (i, Annotated _ t) = isClock (i,t)
  isClock _ = Nothing

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

-- | Internals of a Component
data Declaration
  -- | Signal assignment
  = Assignment
      !Identifier -- ^ Signal to assign
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

  -- | Signal declaration
  | NetDecl'
      (Maybe Comment)                -- ^ Note; will be inserted as a comment in target hdl
      WireOrReg                      -- ^ Wire or register
      !Identifier                    -- ^ Name of signal
      (Either IdentifierText HWType) -- ^ Pointer to type of signal or type of signal
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

data EntityOrComponent = Entity | Comp | Empty
  deriving Show

data WireOrReg = Wire | Reg
  deriving (Show,Generic)

instance NFData WireOrReg

pattern NetDecl
  :: Maybe Comment
  -- ^ Note; will be inserted as a comment in target hdl
  -> Identifier
  -- ^ Name of signal
  -> HWType
  -- ^ Type of signal
  -> Declaration
pattern NetDecl note d ty <- NetDecl' note Wire d (Right ty) _
  where
    NetDecl note d ty = NetDecl' note Wire d (Right ty) Nothing


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
                          ,WireOrReg
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
  deriving Show

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
