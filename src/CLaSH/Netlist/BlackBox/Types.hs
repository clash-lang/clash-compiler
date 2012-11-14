{-# LANGUAGE DeriveDataTypeable #-}
module CLaSH.Netlist.BlackBox.Types where

import Data.Data

import CLaSH.Netlist.Types

data BlackBoxContext
  = Context
  { result    :: Identifier
  , inputs    :: [Identifier]
  , litInputs :: [Identifier]
  , funInputs :: [Identifier]
  } deriving (Show,Data,Typeable)
