{-# LANGUAGE DeriveDataTypeable #-}
module CLaSH.Netlist.BlackBox.Types where

import Data.Data

import CLaSH.Netlist.Types

data BlackBoxContext
  = Context
  { result    :: VarInput
  , varInputs :: [VarInput]
  , litInputs :: [Integer]
  } deriving (Data,Typeable)

data VarInput
  = VarInput
  { varName   :: Identifier
  , varSize   :: Int
  , varLength :: Int
  } deriving (Data,Typeable)

