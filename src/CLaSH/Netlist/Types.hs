{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Type and instance definitions for Netlist modules
module CLaSH.Netlist.Types where

import Control.Monad.State                  (MonadIO, MonadState, StateT)
import Control.Monad.Writer                 (MonadWriter, WriterT)
import Data.Hashable
import Data.HashMap.Lazy                    (HashMap)
import Data.HashSet                         (HashSet)
import Data.Text.Lazy                       (Text)
import GHC.Generics                         (Generic)
import Text.PrettyPrint.Leijen.Text.Monadic (Doc)
import Unbound.LocallyNameless              (Fresh, FreshMT)

import CLaSH.Core.Term                      (Term, TmName)
import CLaSH.Core.Type                      (Type)
import CLaSH.Core.TyCon                     (TyCon, TyConName)
import CLaSH.Core.Util                      (Gamma)
import CLaSH.Primitives.Types               (PrimMap)
import CLaSH.Util

-- | Monad that caches generated components (StateT) and remembers hidden inputs
-- of components that are being generated (WriterT)
newtype NetlistMonad a =
    NetlistMonad { runNetlist :: WriterT [(Identifier,HWType)] (StateT NetlistState (FreshMT IO)) a }
  deriving (Functor, Monad, Applicative, MonadState NetlistState, MonadWriter [(Identifier,HWType)], Fresh, MonadIO)

-- | State for the 'CLaSH.Netlist.VHDL.VHDLM' monad:
--
-- * Previously encountered HWTypes
--
-- * Product type counter
--
-- * Cache for previously generated product type names
type VHDLState = (HashSet HWType,Int,HashMap HWType Doc)

-- | State of the NetlistMonad
data NetlistState
  = NetlistState
  { _bindings       :: HashMap TmName (Type,Term) -- ^ Global binders
  , _varEnv         :: Gamma -- ^ Type environment/context
  , _varCount       :: Int -- ^ Number of signal declarations
  , _cmpCount       :: Int -- ^ Number of create components
  , _components     :: HashMap TmName Component -- ^ Cached components
  , _primitives     :: PrimMap -- ^ Primitive Definitions
  , _vhdlMState     :: VHDLState -- ^ State for the 'CLaSH.Netlist.VHDL.VHDLM' Monad
  , _typeTranslator :: HashMap TyConName TyCon -> Type -> Maybe (Either String HWType) -- ^ Hardcoded Type -> HWType translator
  , _tcCache        :: HashMap TyConName TyCon -- ^ TyCon cache
  }

-- | Signal reference
type Identifier = Text

-- | Component: base unit of a Netlist
data Component
  = Component
  { componentName :: Identifier -- ^ Name of the component
  , hiddenPorts   :: [(Identifier,HWType)] -- ^ Ports that have no correspondence the original function definition
  , inputs        :: [(Identifier,HWType)] -- ^ Input ports
  , output        :: (Identifier,HWType) -- ^ Output port
  , declarations  :: [Declaration] -- ^ Internal declarations
  }
  deriving Show

-- | Size indication of a type (e.g. bit-size or number of elements)
type Size = Int

-- | Representable hardware types
data HWType
  = Void -- ^ Empty type
  | Bit -- ^ Bit type
  | Bool -- ^ Boolean type
  | Integer -- ^ Integer type
  | Signed   Size -- ^ Signed integer of a specified size
  | Unsigned Size -- ^ Unsigned integer of a specified size
  | Vector   Size       HWType -- ^ Vector type
  | Sum      Identifier [Identifier] -- ^ Sum type: Name and Constructor names
  | Product  Identifier [HWType] -- ^ Product type: Name and field types
  | SP       Identifier [(Identifier,[HWType])] -- ^ Sum-of-Product type: Name and Constructor names + field types
  | Clock    Int -- ^ Clock type with specified period
  | Reset    Int -- ^ Reset type corresponding to clock with a specified period
  deriving (Eq,Show,Generic)

instance Hashable HWType

-- | Internals of a Component
data Declaration
  = Assignment Identifier Expr
  -- ^ Signal assignment:
  --
  -- * Signal to assign
  --
  -- * Assigned expression
  | CondAssignment Identifier Expr [(Maybe Expr,Expr)]
  -- ^ Conditional signal assignment:
  --
  -- * Signal to assign
  --
  -- * Scrutinized expression
  --
  -- * List of: (Maybe expression scrutinized expression is compared with,RHS of alternative)
  | InstDecl Identifier Identifier [(Identifier,Expr)] -- ^ Instantiation of another component
  | BlackBoxD Text -- ^ Instantiation of blackbox declaration
  | NetDecl Identifier HWType (Maybe Expr) -- ^ Signal declaration
  deriving Show

-- | Expression Modifier
data Modifier
  = Indexed (HWType,Int,Int) -- ^ Index the expression: (Type of expression,DataCon tag,Field Tag)
  | DC (HWType,Int) -- ^ See expression in a DataCon context: (Type of the expression, DataCon tag)
  | VecAppend -- ^ See the expression in the context of a Vector append operation
  deriving Show

-- | Expression used in RHS of a declaration
data Expr
  = Literal    (Maybe Size) Literal -- ^ Literal expression
  | DataCon    HWType       (Maybe Modifier)  [Expr] -- ^ DataCon application
  | Identifier Identifier   (Maybe Modifier) -- ^ Signal reference
  | BlackBoxE Text (Maybe Modifier) -- ^ Instantiation of a BlackBox expression
  deriving Show

-- | Literals used in an expression
data Literal
  = NumLit  Int -- ^ Number literal
  | BitLit  Bit -- ^ Bit literal
  | BoolLit Bool -- ^ Boolean literal
  | VecLit  [Literal] -- ^ Vector literal
  deriving Show

-- | Bit literal
data Bit
  = H -- ^ High
  | L -- ^ Low
  | U -- ^ Undefined
  | Z -- ^ High-impedance
  deriving Show

makeLenses ''NetlistState
