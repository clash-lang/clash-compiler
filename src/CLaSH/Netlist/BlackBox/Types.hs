{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CLaSH.Netlist.BlackBox.Types where

import Control.Monad.State (MonadState, State)
import Control.Monad.Writer (MonadWriter,WriterT)
import Data.Text.Lazy (Text)

import CLaSH.Netlist.Types

data BlackBoxContext
  = Context
  { result    :: (SyncIdentifier,HWType)
  , inputs    :: [(SyncIdentifier,HWType)]
  , litInputs :: [Identifier]
  , funInputs :: [(Line,BlackBoxContext)]
  }
  deriving Show

type SyncIdentifier = Either Identifier (Identifier,Identifier)

type Line = [Element]

data Element = C  Text
             | D Decl
             | O
             | I   Int
             | L   Int
             | Sym Int
             | Clk (Maybe Int)
             | Rst (Maybe Int)
             | Typ (Maybe Int)
  deriving Show

data Decl = Decl Int [Line]
  deriving Show

newtype BlackBoxMonad a = B { runBlackBoxM :: WriterT [(Identifier,HWType)] (State VHDLState) a }
  deriving (Functor, Monad, MonadWriter [(Identifier,HWType)], MonadState VHDLState)
