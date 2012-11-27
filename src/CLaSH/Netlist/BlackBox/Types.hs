{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
module CLaSH.Netlist.BlackBox.Types where

import Data.Text.Lazy (Text)
import CLaSH.Netlist.Types

data BlackBoxContext
  = Context
  { result    :: SyncIdentifier
  , inputs    :: [SyncIdentifier]
  , litInputs :: [Identifier]
  , funInputs :: [Identifier]
  }
  deriving Show

type SyncIdentifier = Either Identifier (Identifier,Identifier)

type Line = [Element]

data Element = C  Text
             | D Decl
             | O
             | I   Int
             | L   Int
             | Clk (Maybe Int)
             | Rst (Maybe Int)

data Decl = Decl Int [Line]
