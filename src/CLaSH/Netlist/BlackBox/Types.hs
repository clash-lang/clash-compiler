{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Types used in BlackBox modules
module CLaSH.Netlist.BlackBox.Types where

import Control.Monad.State  (MonadState, State)
import Control.Monad.Writer (MonadWriter,WriterT)
import Data.Text.Lazy       (Text)

import CLaSH.Netlist.Types

-- | Context used to fill in the holes of a BlackBox template
data BlackBoxContext
  = Context
  { result    :: (SyncIdentifier,HWType) -- ^ Result name and type
  , inputs    :: [(SyncIdentifier,HWType)] -- ^ Argument names and types
  , litInputs :: [Identifier] -- ^ Literal arguments (subset of inputs)
  , funInputs :: [(BlackBoxTemplate,BlackBoxContext)]
  -- ^ Function arguments (subset of inputs):
  --
  -- * (Blackbox Template,Partial Blackbox Concext)
  }
  deriving Show

-- | Either the name of the identifier, or a tuple of the identifier and the
-- corresponding clock
type SyncIdentifier = Either Identifier (Identifier,Identifier)

-- | A BlackBox Template is a List of Elements
type BlackBoxTemplate = [Element]

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
data Decl = Decl Int [BlackBoxTemplate]
  deriving Show

-- | Monad that caches VHDL information and remembers hidden inputs of
-- black boxes that are being generated (WriterT)
newtype BlackBoxMonad a = B { runBlackBoxM :: WriterT [(Identifier,HWType)] (State VHDLState) a }
  deriving (Functor, Monad, MonadWriter [(Identifier,HWType)], MonadState VHDLState)
