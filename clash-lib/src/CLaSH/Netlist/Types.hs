{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Type and instance definitions for Netlist modules
module CLaSH.Netlist.Types where

import Control.DeepSeq
import Control.Monad.State                  (MonadIO, MonadState, StateT)
import Control.Monad.Writer                 (MonadWriter, WriterT)
import Data.Hashable
import Data.HashMap.Lazy                    (HashMap)
import qualified Data.Text                  as S
import Data.Text.Lazy                       (Text, pack)
import GHC.Generics                         (Generic)
import Unbound.LocallyNameless              (Fresh, FreshMT)

import CLaSH.Core.Term                      (Term, TmName)
import CLaSH.Core.Type                      (Type)
import CLaSH.Core.TyCon                     (TyCon, TyConName)
import CLaSH.Core.Util                      (Gamma)
import CLaSH.Netlist.BlackBox.Types
import CLaSH.Primitives.Types               (PrimMap)
import CLaSH.Util

-- | Monad that caches generated components (StateT) and remembers hidden inputs
-- of components that are being generated (WriterT)
newtype NetlistMonad a =
  NetlistMonad { runNetlist :: WriterT
                               [(Identifier,HWType)]
                               (StateT NetlistState (FreshMT IO))
                               a
               }
  deriving (Functor, Monad, Applicative, MonadWriter [(Identifier,HWType)],
            MonadState NetlistState, Fresh, MonadIO)

-- deriving instance MonadState (NetlistState backend) (NetlistMonad backend)

-- | State of the NetlistMonad
data NetlistState
  = NetlistState
  { _bindings       :: HashMap TmName (Type,Term) -- ^ Global binders
  , _varEnv         :: Gamma -- ^ Type environment/context
  , _varCount       :: Int -- ^ Number of signal declarations
  , _cmpCount       :: Int -- ^ Number of create components
  , _components     :: HashMap TmName Component -- ^ Cached components
  , _primitives     :: PrimMap -- ^ Primitive Definitions
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

instance NFData Component where
  rnf c = case c of
    Component nm hi inps outps decls -> rnf nm `seq` rnf hi `seq` rnf inps `seq`
                                        rnf outps `seq` rnf decls

-- | Size indication of a type (e.g. bit-size or number of elements)
type Size = Int

-- | Representable hardware types
data HWType
  = Void -- ^ Empty type
  | Bool -- ^ Boolean type
  | Integer -- ^ Integer type
  | BitVector Size -- ^ BitVector of a specified size
  | Index    Size -- ^ Unsigned integer with specified (exclusive) upper bounder
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
instance NFData HWType where
  rnf hwty = case hwty of
    Void -> ()
    Bool -> ()
    Integer -> ()
    BitVector s -> rnf s
    Index u -> rnf u
    Signed s -> rnf s
    Unsigned s -> rnf s
    Vector s el -> rnf s `seq` rnf el
    Sum i ids -> rnf i `seq` rnf ids
    Product i ids -> rnf i `seq` rnf ids
    SP i ids -> rnf i `seq` rnf ids
    Clock i -> rnf i
    Reset i -> rnf i

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
  | BlackBoxD S.Text BlackBoxTemplate BlackBoxContext -- ^ Instantiation of blackbox declaration
  | NetDecl Identifier HWType (Maybe Expr) -- ^ Signal declaration
  deriving Show

instance NFData Declaration where
  rnf a = a `seq` ()

-- | Expression Modifier
data Modifier
  = Indexed (HWType,Int,Int) -- ^ Index the expression: (Type of expression,DataCon tag,Field Tag)
  | DC (HWType,Int) -- ^ See expression in a DataCon context: (Type of the expression, DataCon tag)
  | VecAppend -- ^ See the expression in the context of a Vector append operation
  deriving Show

-- | Expression used in RHS of a declaration
data Expr
  = Literal    (Maybe (HWType,Size)) Literal -- ^ Literal expression
  | DataCon    HWType       Modifier  [Expr] -- ^ DataCon application
  | Identifier Identifier   (Maybe Modifier) -- ^ Signal reference
  | DataTag    HWType       (Either Expr Identifier) -- ^ @Left e@: tagToEnum#, @Right e@: dataToTag#
  | BlackBoxE S.Text BlackBoxTemplate BlackBoxContext Bool -- ^ Instantiation of a BlackBox expression
  deriving Show

-- | Literals used in an expression
data Literal
  = NumLit  Integer -- ^ Number literal
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

-- | Context used to fill in the holes of a BlackBox template
data BlackBoxContext
  = Context
  { bbResult    :: (SyncExpr,HWType) -- ^ Result name and type
  , bbInputs    :: [(SyncExpr,HWType)] -- ^ Argument names and types
  , bbLitInputs :: [Expr] -- ^ Literal arguments (subset of inputs)
  , bbFunInputs :: [(Either BlackBoxTemplate Declaration,BlackBoxContext)]
  -- ^ Function arguments (subset of inputs):
  --
  -- * (Blackbox Template,Partial Blackbox Concext)
  }
  deriving Show

emptyBBContext :: BlackBoxContext
emptyBBContext = Context (Left $ Identifier (pack "__EMPTY__") Nothing, Void) [] [] []

-- | Either the name of the identifier, or a tuple of the identifier and the
-- corresponding clock
type SyncIdentifier = Either Identifier (Identifier,(Identifier,Int))
type SyncExpr       = Either Expr       (Expr,(Identifier,Int))

makeLenses ''NetlistState
