{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Types used in BlackBox modules
-}

module Clash.Netlist.BlackBox.Types where

import Data.Text.Lazy (Text)

-- | A BlackBox Template is a List of Elements
type BlackBoxTemplate = [Element]

-- | Elements of a blackbox context
data Element = C   !Text         -- ^ Constant
             | D   !Decl         -- ^ Component instantiation hole
             | O   !Bool
             -- ^ Output hole; @Bool@ asserts escape marker stripping
             | I   !Bool !Int
             -- ^ Input hole; @Bool@ asserts escape marker stripping
             | N   !Int          -- ^ Name hole
             | L   !Int          -- ^ Literal hole
             | Var [Element] !Int    --
             | Sym !Text !Int    -- ^ Symbol hole
             | Typ !(Maybe Int)  -- ^ Type declaration hole
             | TypM !(Maybe Int) -- ^ Type root hole
             | Err !(Maybe Int)  -- ^ Error value hole
             | TypElem !Element  -- ^ Select element type from a vector type
             | CompName          -- ^ Hole for the name of the component in which
                                 -- the blackbox is instantiated
             | IncludeName !Int
             | IndexType !Element -- ^ Index data type hole, the field is the
                                  -- (exclusive) maximum index
             | Size !Element     -- ^ Size of a type hole
             | Length !Element   -- ^ Length of a vector hole
             | Depth !Element    -- ^ Depth of a tree hole
             | FilePath !Element -- ^ Hole containing a filepath for a data file
             | Gen !Bool         -- ^ Hole marking beginning (True) or end (False)
                                 -- of a generative construct
             | IF !Element [Element] [Element]
             | And [Element]
             | IW64              -- ^ Hole indicating whether Int/Word/Integer
                                 -- are 64-Bit
             | HdlSyn HdlSyn     -- ^ Hole indicating which synthesis tool we're
                                 -- generating HDL for
             | BV !Bool [Element] !Element -- ^ Convert to (True)/from(False) a bit-vector
             | Sel !Element !Int -- ^ Record selector of a type
             | IsLit !Int
             | IsVar !Int
             | IsGated !Int
             | IsSync !Int
             | StrCmp [Element] !Int
             | OutputWireReg !Int
             | Vars !Int
             | GenSym [Element] !Int
             | SigD [Element] !(Maybe Int)
  deriving Show

-- | Component instantiation hole. First argument indicates which function argument
-- to instantiate. Second argument corresponds to output and input assignments,
-- where the first element is the output assignment, and the subsequent elements
-- are the consecutive input assignments.
--
-- The LHS of the tuple is the name of the signal, while the RHS of the tuple
-- is the type of the signal
data Decl = Decl !Int [(BlackBoxTemplate,BlackBoxTemplate)]
  deriving Show

data HdlSyn = Vivado | Other
  deriving (Eq,Show,Read)
