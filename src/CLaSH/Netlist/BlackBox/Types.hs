{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CLaSH.Netlist.BlackBox.Types where

import Control.Monad.State  (MonadState, State)
import Control.Monad.Writer (MonadWriter,WriterT)
import Data.Text.Lazy       (Text)

import CLaSH.Netlist.Types

data BlackBoxContext
  = Context
  { result    :: (SyncIdentifier,HWType)
  , inputs    :: [(SyncIdentifier,HWType)]
  , litInputs :: [Identifier]
  , funInputs :: [(Line,BlackBoxContext)]
  }
  deriving Show

-- | Either the name of the identifier, or a tuple of the identifier and the
-- corresponding clock
type SyncIdentifier = Either Identifier (Identifier,Identifier)

type Line = [Element]

-- | Elements of a blackbox context
data Element = C   Text          -- ^ Constant
             | D   Decl          -- ^ Component instantiation hole
             | O                 -- ^ Output hole
             | I   Int           -- ^ Input hole
             | L   Int           -- ^ Literal hole
             | Sym Int           -- ^ Symbol hole
             | Clk (Maybe Int)   -- ^ Clock hole (Maybe clk corresponding to
                                 -- input, clk corresponding to output if Nothing)
             | Rst (Maybe Int)   -- ^ Reset hole
             | Typ (Maybe Int)   -- ^ Type declaration hole
             | TypM (Maybe Int)  -- ^ Type root hole
             | Def (Maybe Int)   -- ^ Default value hole
  deriving Show

-- | Component instantiation hole. First argument indicates which function argument
-- to instantiate. Second argument corresponds to output and input assignments,
-- where the first element is the output assignment, and the subsequent elements
-- are the consecutive input assignments.
data Decl = Decl Int [Line]
  deriving Show

newtype BlackBoxMonad a = B { runBlackBoxM :: WriterT [(Identifier,HWType)] (State VHDLState) a }
  deriving (Functor, Monad, MonadWriter [(Identifier,HWType)], MonadState VHDLState)
