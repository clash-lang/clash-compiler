{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CLaSH.Netlist.Types where

import Control.Monad.State     (MonadIO,MonadState,StateT)
import Control.Monad.Writer    (MonadWriter,WriterT)
import Data.ByteString.Lazy    (ByteString)
import Data.Text.Lazy          (Text)
import Data.Hashable
import Data.HashMap.Lazy       (HashMap)
import GHC.Generics            (Generic)
import Text.PrettyPrint.Leijen.Text.Monadic (Doc)
import Unbound.LocallyNameless (Fresh,FreshMT)

import CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.Type (Type)
import CLaSH.Core.Util (Gamma)
import CLaSH.Primitives.Types (Primitive)
import CLaSH.Util

newtype NetlistMonad a =
    NetlistMonad { runNetlist :: WriterT [(Identifier,HWType)] (StateT NetlistState (FreshMT IO)) a }
  deriving (Functor, Monad, Applicative, MonadState NetlistState, MonadWriter [(Identifier,HWType)], Fresh, MonadIO)

type VHDLState = (Int,Text,HashMap HWType (Text,Doc))

data NetlistState
  = NetlistState
  { _bindings   :: HashMap TmName (Type,Term)
  , _varEnv     :: Gamma
  , _varCount   :: Integer
  , _cmpCount   :: Integer
  , _components :: HashMap TmName Component
  , _primitives :: HashMap ByteString Primitive
  , _vhdlMState :: VHDLState
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
  deriving (Eq,Show,Generic)

instance Hashable HWType

data Declaration
  = Assignment Identifier (Maybe Modifier) HWType [Expr]
  | InstDecl Identifier Identifier [(Identifier,Expr)]
  | BlackBox Text
  | NetDecl Identifier HWType (Maybe Expr)
  deriving Show

data Modifier
  = Indexed  Int
  | Selected Label
  | DC Int
  deriving Show

data Expr
  = Literal    (Maybe Size) Literal
  | Identifier Identifier   (Maybe Modifier)
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
