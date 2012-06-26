{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CLaSH.Netlist.Types where

import Control.Monad.Identity  (Identity)
import Control.Monad.State     (MonadState,StateT)
import Data.Text.Lazy          (Text)
import Data.HashMap.Lazy       (HashMap)
import Unbound.LocallyNameless (Fresh,FreshMT)

import CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.Type (Type)
import CLaSH.Core.Util (Gamma)
import CLaSH.Util

newtype NetlistMonad a =
    NetlistMonad { runNetlist :: StateT NetlistState (FreshMT Identity) a }
  deriving (Functor, Monad, Applicative, MonadState NetlistState, Fresh)

data NetlistState
  = NetlistState
  { _bindings  :: HashMap TmName (Type,Term)
  , _varEnv    :: Gamma
  }

mkLabels [''NetlistState]

type Identifier = Text
type Label      = Identifier

data Component
  = Component
  { componentName :: Identifier
  , inputs        :: [(Identifier,HWType)]
  , output        :: (Identifier,HWType)
  , declarations  :: [Declaration]
  }
  deriving Show

type Size = Int

data HWType
  = Bit
  | Bool
  | Signed   Size
  | Unsigned Size
  | Vector   Size       HWType
  | Sum      Identifier [Identifier]
  | Product  Identifier [HWType]
  | SP       Identifier [(Identifier,[HWType])]
  deriving Show

data Declaration
  = Assignment Identifier (Maybe Modifier) HWType [Expr]
  | BlackBox Text
  deriving Show

data Modifier
  = Indexed  Int
  | Selected Label
  | DC Int
  deriving Show

data Expr
  = Literal    Literal
  | Identifier Identifier (Maybe Modifier)
  deriving Show

data Literal
  = NumLit  Integer
  | BitLit  Bit
  | BoolLit Bool
  | VecLit  [Literal]
  deriving Show

data Bit = H | L | U | Z
  deriving Show
