{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd,
                    2017-2018, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type and instance definitions for Netlist modules
-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}


module Clash.Netlist.Types
  ( Declaration (..,NetDecl)
  , module Clash.Netlist.Types
  )
where

import Control.DeepSeq
import Control.Monad.Fail                   (MonadFail)
import Control.Monad.State                  (State)
import Control.Monad.State.Strict           (MonadIO, MonadState, StateT)
import Data.Bits                            (testBit)
import Data.Binary                          (Binary(..))
import Data.Hashable                        (Hashable)
import Data.HashMap.Strict                  (HashMap)
import Data.IntMap                          (IntMap, empty)
import qualified Data.Set                   as Set
import Data.Text                            (Text, pack)
import Data.Typeable                        (Typeable)
import Data.Text.Prettyprint.Doc.Extra      (Doc)
import GHC.Generics                         (Generic)
import Language.Haskell.TH.Syntax           (Lift)

import SrcLoc                               (SrcSpan)

import Clash.Annotations.TopEntity          (TopEntity)
import Clash.Backend                        (Backend)
import Clash.Core.Type                      (Type)
import Clash.Core.Var                       (Attr')
import Clash.Core.TyCon                     (TyConMap)
import Clash.Core.VarEnv                    (VarEnv)
import Clash.Driver.Types                   (BindingMap, ClashOpts)
import Clash.Netlist.BlackBox.Types         (BlackBoxTemplate)
import Clash.Netlist.Id                     (IdType)
import Clash.Primitives.Types               (CompiledPrimMap)
import Clash.Signal.Internal
  (ResetPolarity, ActiveEdge, ResetKind, InitBehavior)
import Clash.Util                           (makeLenses)

import Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, DataRepr', ConstrRepr')

-- | Monad that caches generated components (StateT) and remembers hidden inputs
-- of components that are being generated (WriterT)
newtype NetlistMonad a =
  NetlistMonad { runNetlist :: StateT NetlistState IO a }
  deriving newtype (Functor, Monad, Applicative, MonadState NetlistState, MonadIO, MonadFail)

-- | State of the NetlistMonad
data NetlistState
  = NetlistState
  { _bindings       :: BindingMap
  -- ^ Global binders
  , _varCount       :: !Int
  -- ^ Number of signal declarations
  , _components     :: VarEnv ([Bool],SrcSpan,HashMap Identifier Word,Component)
  -- ^ Cached components
  , _primitives     :: CompiledPrimMap
  -- ^ Primitive Definitions
  , _typeTranslator :: CustomReprs -> TyConMap -> Type -> Maybe (Either String FilteredHWType)
  -- ^ Hardcoded Type -> HWType translator
  , _tcCache        :: TyConMap
  -- ^ TyCon cache
  , _curCompNm      :: !(Identifier,SrcSpan)
  , _intWidth       :: Int
  , _mkIdentifierFn :: IdType -> Identifier -> Identifier
  , _extendIdentifierFn :: IdType -> Identifier -> Identifier -> Identifier
  , _seenIds        :: HashMap Identifier Word
  , _seenComps      :: HashMap Identifier Word
  , _seenPrimitives :: Set.Set Text
  -- ^ Keeps track of invocations of ´mkPrimitive´. It is currently used to
  -- filter duplicate warning invocations for dubious blackbox instantiations,
  -- see GitHub pull request #286.
  , _componentNames :: VarEnv Identifier
  , _topEntityAnns  :: VarEnv (Type, Maybe TopEntity)
  , _hdlDir         :: FilePath
  , _curBBlvl       :: Int
  -- ^ The current scoping level assigned to black box contexts
  , _componentPrefix :: (Maybe Identifier,Maybe Identifier)
  -- ^ Prefix for top-level components, and prefix for all other components
  , _customReprs    :: CustomReprs
  , _clashOpts      :: ClashOpts
  -- ^ Settings Clash was called with
  , _isTestBench    :: Bool
  -- ^ Whether we're compiling a testbench (suppresses some warnings)
  , _backEndITE :: Bool
  -- ^ Whether the backend supports ifThenElse expressions
  }

-- | Signal reference
type Identifier = Text

-- | Component: base unit of a Netlist
data Component
  = Component
  { componentName :: !Identifier -- ^ Name of the component
  , inputs        :: [(Identifier,HWType)] -- ^ Input ports
  , outputs       :: [(WireOrReg,(Identifier,HWType))] -- ^ Output ports
  , declarations  :: [Declaration] -- ^ Internal declarations
  }
  deriving Show

instance NFData Component where
  rnf c = case c of
    Component nm inps outps decls -> rnf nm    `seq` rnf inps `seq`
                                     rnf outps `seq` rnf decls

-- | Size indication of a type (e.g. bit-size or number of elements)
type Size = Int

type IsVoid = Bool

-- | Tree structure indicating which constructor fields were filtered from
-- a type due to them being void. We need this information to generate stable
-- and/or user-defined port mappings.
data FilteredHWType =
  FilteredHWType HWType [[(IsVoid, FilteredHWType)]]
    deriving (Eq, Show)

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
  | RTree !Size !HWType
  -- ^ RTree type
  | Sum !Identifier [Identifier]
  -- ^ Sum type: Name and Constructor names
  | Product !Identifier (Maybe [Text]) [HWType]
  -- ^ Product type: Name, field names, and field types. Field names will be
  -- populated when using records.
  | SP !Identifier [(Identifier,[HWType])]
  -- ^ Sum-of-Product type: Name and Constructor names + field types
  | Clock !Identifier
  -- ^ Clock type corresponding to domain /Identifier/
  | Reset !Identifier
  -- ^ Reset type corresponding to domain /Identifier/
  | BiDirectional !PortDirection !HWType
  -- ^ Tagging type indicating a bidirectional (inout) port
  | CustomSP !Identifier !DataRepr' !Size [(ConstrRepr', Identifier, [HWType])]
  -- ^ Same as Sum-Of-Product, but with a user specified bit representation. For
  -- more info, see: Clash.Annotations.BitRepresentations.
  | CustomSum !Identifier !DataRepr' !Size [(ConstrRepr', Identifier)]
  -- ^ Same as Sum, but with a user specified bit representation. For more info,
  -- see: Clash.Annotations.BitRepresentations.
  | Annotated [Attr'] !HWType
  -- ^ Annotated with HDL attributes
  | KnownDomain !Identifier !Integer !ActiveEdge !ResetKind !InitBehavior !ResetPolarity
  -- ^ Domain name, period, active edge, reset kind, initial value behavior
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

-- | Extract hardware attributes from Annotated. Returns an empty list if
-- non-Annotated given or if Annotated has an empty list of attributes.
hwTypeAttrs :: HWType -> [Attr']
hwTypeAttrs (Annotated attrs _type) = attrs
hwTypeAttrs _                       = []

-- | Internals of a Component
data Declaration
  = Assignment !Identifier !Expr
  -- ^ Signal assignment:
  --
  -- * Signal to assign
  --
  -- * Assigned expression
  | CondAssignment !Identifier !HWType !Expr !HWType [(Maybe Literal,Expr)]
  -- ^ Conditional signal assignment:
  --
  -- * Signal to assign
  --
  -- * Type of the result/alternatives
  --
  -- * Scrutinized expression
  --
  -- * Type of the scrutinee
  --
  -- * List of: (Maybe expression scrutinized expression is compared with,RHS of alternative)
  | InstDecl EntityOrComponent (Maybe Identifier) !Identifier !Identifier [(Expr,HWType,Expr)] [(Expr,PortDirection,HWType,Expr)]
  -- ^ Instantiation of another component:
  --
  -- * Whether it's an entity or a component
  --
  -- * Comment to add to the generated code
  --
  -- * The component's (or entity's) name
  --
  -- * Instance label
  --
  -- * List of parameters for this component (param name, param type, param value)
  --
  -- * Ports (port name, port direction, type, assignment)
  | BlackBoxD
      -- Primitive name:
      !Text
      -- VHDL only: add /library/ declarations:
      [BlackBoxTemplate]
      -- VHDL only: add /use/ declarations:
      [BlackBoxTemplate]
      -- Intel/Quartus only: create a /.qsys/ file from given template:
      [((Text,Text),BlackBox)]
      -- Template tokens:
      !BlackBox
      -- Context in which tokens should be rendered:
      BlackBoxContext
  -- ^ Instantiation of blackbox declaration
  | NetDecl'
      (Maybe Identifier)         -- Note; will be inserted as a comment in target hdl
      WireOrReg                  -- Wire or register
      !Identifier                -- Name of signal
      (Either Identifier HWType) -- Pointer to type of signal or type of signal
      -- ^ Signal declaration
  deriving Show

data EntityOrComponent = Entity | Comp
  deriving Show

data WireOrReg = Wire | Reg
  deriving (Show,Generic)

instance NFData WireOrReg

pattern NetDecl
  :: Maybe Identifier
  -- ^ Note; will be inserted as a comment in target hdl
  -> Identifier
  -- ^ Name of signal
  -> HWType
  -- ^ Type of signal
  -> Declaration
pattern NetDecl note d ty <- NetDecl' note Wire d (Right ty)
  where
    NetDecl note d ty = NetDecl' note Wire d (Right ty)

data PortDirection = In | Out
  deriving (Eq,Ord,Show,Generic,NFData,Hashable)

instance NFData Declaration where
  rnf a = a `seq` ()

-- | Expression Modifier
data Modifier
  = Indexed (HWType,Int,Int) -- ^ Index the expression: (Type of expression,DataCon tag,Field Tag)
  | DC (HWType,Int) -- ^ See expression in a DataCon context: (Type of the expression, DataCon tag)
  | VecAppend -- ^ See the expression in the context of a Vector append operation
  | RTreeAppend -- ^ See the expression in the context of a Tree append operation
  | Sliced (HWType,Int,Int)
  -- ^ Slice the identifier of the given type from start to end
  | Nested Modifier Modifier
  deriving Show

-- | Expression used in RHS of a declaration
data Expr
  = Literal    !(Maybe (HWType,Size)) !Literal -- ^ Literal expression
  | DataCon    !HWType       !Modifier  [Expr] -- ^ DataCon application
  | Identifier !Identifier   !(Maybe Modifier) -- ^ Signal reference
  | DataTag    !HWType       !(Either Identifier Identifier) -- ^ @Left e@: tagToEnum#, @Right e@: dataToTag#
  | BlackBoxE
      -- Primitive name:
      !Text
      -- VHDL only: add /library/ declarations:
      [BlackBoxTemplate]
      -- VHDL only: add /use/ declarations:
      [BlackBoxTemplate]
      -- Intel/Quartus only: create a /.qsys/ file from given template.
      [((Text,Text),BlackBox)]
      -- Template tokens:
      !BlackBox
      -- Context in which tokens should be rendered:
      !BlackBoxContext
      -- Wrap in paretheses?:
      !Bool
  -- ^ Instantiation of a BlackBox expression
  | ConvBV     (Maybe Identifier) HWType Bool Expr
  | IfThenElse Expr Expr Expr
  deriving Show

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
  { bbName      :: Text -- ^ Blackbox function name (for error reporting)
  , bbResult    :: (Expr,HWType) -- ^ Result name and type
  , bbInputs    :: [(Expr,HWType,Bool)] -- ^ Argument names, types, and whether it is a literal
  , bbFunctions :: IntMap (Either BlackBox (Identifier,[Declaration])
                          ,WireOrReg
                          ,[BlackBoxTemplate]
                          ,[BlackBoxTemplate]
                          ,[((Text,Text),BlackBox)]
                          ,BlackBoxContext)
  -- ^ Function arguments (subset of inputs):
  --
  -- * ( Blackbox Template
  --   , Whether the result should be /reg/ or a /wire/ (Verilog only)
  --   , Partial Blackbox Context
  --   )
  , bbQsysIncName :: [Identifier]
  , bbLevel :: Int
  -- ^ The scoping level this context is associated with, ensures that
  -- @~ARGN[k][n]@ holes are only filled with values from this context if @k@
  -- is equal to the scoping level of this context.
  , bbCompName :: Identifier
  -- ^ The component the BlackBox is instantiated in
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
    -> (BlackBoxContext -> Bool)
    -> (forall s . Backend s => BlackBoxContext -> State s Doc)
    -> TemplateFunction

instance Show BlackBox where
  show (BBTemplate t)  = show t
  show (BBFunction nm hsh _) =
    "<TemplateFunction(nm=" ++ show nm ++ ", hash=" ++ show hsh ++ ")>"

instance NFData TemplateFunction where
  rnf (TemplateFunction is f _) = rnf is `seq` f `seq` ()

-- | __NB__: serialisation doesn't preserve the embedded function
instance Binary TemplateFunction where
  put (TemplateFunction is _ _ ) = put is
  get = (\is -> TemplateFunction is err err) <$> get
    where err = const $ error "TemplateFunction functions can't be preserved by serialisation"

emptyBBContext :: Text -> BlackBoxContext
emptyBBContext n
  = Context
  { bbName        = n
  , bbResult      = (Identifier (pack "__EMPTY__") Nothing, Void Nothing)
  , bbInputs      = []
  , bbFunctions   = empty
  , bbQsysIncName = []
  , bbLevel       = (-1)
  , bbCompName    = pack "__NOCOMPNAME__"
  }

makeLenses ''NetlistState
