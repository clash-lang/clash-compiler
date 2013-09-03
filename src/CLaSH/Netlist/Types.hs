{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module CLaSH.Netlist.Types where

import Control.Monad.State                  (MonadIO, MonadState,
                                             StateT)
import Control.Monad.Writer                 (MonadWriter, WriterT)
import Data.ByteString.Lazy                 (ByteString)
import Data.Hashable
import Data.HashMap.Lazy                    (HashMap)
import Data.Text.Lazy                       (Text)
import GHC.Generics                         (Generic)
import Text.PrettyPrint.Leijen.Text.Monadic (Doc)
import Unbound.LocallyNameless              (Fresh, FreshMT)

import CLaSH.Core.Term                      (Term, TmName)
import CLaSH.Core.Type                      (Type)
import CLaSH.Core.Util                      (Gamma)
import CLaSH.Primitives.Types               (Primitive)
import CLaSH.Util

newtype NetlistMonad a =
    NetlistMonad { runNetlist :: WriterT [(Identifier,HWType)] (StateT NetlistState (FreshMT IO)) a }
  deriving (Functor, Monad, Applicative, MonadState NetlistState, MonadWriter [(Identifier,HWType)], Fresh, MonadIO)

type VHDLState = (Int,Text,HashMap HWType (Text,Doc))

data NetlistState
  = NetlistState
  { _bindings       :: HashMap TmName (Type,Term)
  , _varEnv         :: Gamma
  , _varCount       :: Int
  , _cmpCount       :: Int
  , _components     :: HashMap TmName Component
  , _primitives     :: HashMap ByteString Primitive
  , _vhdlMState     :: VHDLState
  , _typeTranslator :: Type -> Maybe (Either String HWType)
  }

type Identifier = Text
type Label      = Identifier

data Component
  = Component
  { componentName :: Identifier
  , hiddenPorts   :: [(Identifier,HWType)]
  , inputs        :: [(Identifier,HWType)]
  , output        :: (Identifier,HWType)
  , declarations  :: [Declaration]
  }
  deriving Show

type Size = Int

data HWType
  = Void
  | Bit
  | Bool
  | Integer
  | Signed   Size
  | Unsigned Size
  | Vector   Size       HWType
  | Sum      Identifier [Identifier]
  | Product  Identifier [HWType]
  | SP       Identifier [(Identifier,[HWType])]
  | Clock    Int
  | Reset    Int
  deriving (Eq,Show,Generic)

instance Hashable HWType

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

data Modifier
  = Indexed (HWType,Int,Int)
  | Selected Label
  | DC (HWType,Int)
  | VecAppend
  deriving Show

data Expr
  = Literal    (Maybe Size) Literal -- ^ Literal expression
  | DataCon    HWType       (Maybe Modifier)  [Expr] -- ^ DataCon application
  | Identifier Identifier   (Maybe Modifier) -- ^ Signal reference
  | BlackBoxE Text (Maybe Modifier) -- ^ Instantiation of a BlackBox expression
  deriving Show

data Literal
  = NumLit  Int
  | BitLit  Bit
  | BoolLit Bool
  | VecLit  [Literal]
  deriving Show

data Bit = H | L | U | Z
  deriving Show

makeLenses ''NetlistState
