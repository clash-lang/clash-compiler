{-# LANGUAGE DeriveDataTypeable #-}
module CLaSH.Netlist.BlackBox.Types where

import Data.Data

import CLaSH.Netlist.Types

data BlackBoxContext
  = Context
  { result    :: VarInput
  , varInputs :: [VarInput]
  , litInputs :: [LitInput]
  } deriving (Show,Data,Typeable)

data VarInput
  = VarInput
  { varName   :: Identifier
  , varSize   :: Int
  , varLength :: Int
  } deriving (Show,Data,Typeable)

data LitInput
  = LitInput
  { lit :: Integer
  } deriving (Show,Data,Typeable)
