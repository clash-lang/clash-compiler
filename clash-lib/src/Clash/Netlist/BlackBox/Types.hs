{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Types used in BlackBox modules
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Clash.Netlist.BlackBox.Types
 ( BlackBoxMeta(..)
 , emptyBlackBoxMeta
 , BlackBoxFunction
 , BlackBoxTemplate
 , TemplateKind (..)
 , Element(..)
 , Decl(..)
 , HdlSyn(..)
 ) where

import                Control.DeepSeq            (NFData)
import                Data.Binary                (Binary)
import                Data.Hashable              (Hashable)
import                Data.Text.Lazy             (Text)
import qualified      Data.Text                  as S
import                GHC.Generics               (Generic)

import                Clash.Core.Term            (Term)
import                Clash.Core.Type            (Type)
import                Clash.Core.Var             (Id)
import {-# SOURCE #-} Clash.Netlist.Types        (BlackBox, Identifier)

data TemplateKind
  = TDecl
  | TExpr
  deriving (Show, Eq, Generic, NFData, Binary, Hashable)

-- | See @Clash.Primitives.Types.BlackBox@ for documentation on this record's
-- fields. (They are intentionally renamed to prevent name clashes.)
data BlackBoxMeta =
  BlackBoxMeta { bbOutputReg :: Bool
               , bbKind      :: TemplateKind
               , bbLibrary   :: [BlackBoxTemplate]
               , bbImports   :: [BlackBoxTemplate]
               , bbIncludes  :: [((S.Text, S.Text), BlackBox)]
               }

-- | Use this value in your blackbox template function if you do want to
-- accept the defaults as documented in @Clash.Primitives.Types.BlackBox@.
emptyBlackBoxMeta :: BlackBoxMeta
emptyBlackBoxMeta = BlackBoxMeta False TExpr [] [] []

-- | A BlackBox function generates a blackbox template, given the inputs and
-- result type of the function it should provide a blackbox for. This is useful
-- when having a need for blackbox functions, ... TODO: docs
type BlackBoxFunction
   = Bool
  -- ^ Treat BlackBox expression as declaration
  -> (Either Identifier Id)
  -- ^ Id to assign the result to
  -> S.Text
  -- ^ Name of primitive
  -> [Either Term Type]
  -- ^ Arguments
  -> Type
  -- ^ Result type
  -> Either String (BlackBoxMeta, BlackBox)

-- | A BlackBox Template is a List of Elements
type BlackBoxTemplate = [Element]

-- | Elements of a blackbox context
data Element = C   !Text         -- ^ Constant
             | D   !Decl         -- ^ Component instantiation hole
             | O   !Bool
             -- ^ Output hole; @Bool@ asserts escape marker stripping
             | I   !Bool !Int
             -- ^ Input hole; @Bool@ asserts escape marker stripping
             | Arg !Int !Int
             -- ^ Generated input hole, first argument is the scoping level
             | N   !Int          -- ^ Name hole
             | L   !Int          -- ^ Literal hole
             | LC  !Int
             -- ^ Assert that argument is a literal, but treat it like a normal
             -- argument otherwise
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
             | Template [Element] [Element]
             -- ^ Create data file <HOLE0> with contents <HOLE1>
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
             | Repeat [Element] [Element] -- ^ Repeat <hole> n times
             | DevNull [Element]          -- ^ Evaluate <hole> but swallow output
             | SigD [Element] !(Maybe Int)
  deriving (Show, Generic, NFData, Binary, Hashable)

-- | Component instantiation hole. First argument indicates which function argument
-- to instantiate. Second argument corresponds to output and input assignments,
-- where the first element is the output assignment, and the subsequent elements
-- are the consecutive input assignments.
--
-- The LHS of the tuple is the name of the signal, while the RHS of the tuple
-- is the type of the signal
data Decl = Decl !Int [(BlackBoxTemplate,BlackBoxTemplate)]
  deriving (Show, Generic, NFData, Binary, Hashable)

data HdlSyn = Vivado | Quartus | Other
  deriving (Eq, Show, Read, Generic, NFData, Binary, Hashable)
