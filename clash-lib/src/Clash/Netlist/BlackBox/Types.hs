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
import {-# SOURCE #-} Clash.Netlist.Types
  (BlackBox, Identifier, NetlistMonad)

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
  -> NetlistMonad (Either String (BlackBoxMeta, BlackBox))

-- | A BlackBox Template is a List of Elements
-- TODO: Add name of function for better error messages
type BlackBoxTemplate = [Element]

-- | Elements of a blackbox context. If you extend this list, make sure to
-- update the following functions:
--
--  - Clash.Netlist.BlackBox.Types.prettyElem
--  - Clash.Netlist.BlackBox.Types.renderElem
--  - Clash.Netlist.BlackBox.Types.renderTag
--  - Clash.Netlist.BlackBox.Types.setSym
--  - Clash.Netlist.BlackBox.Types.usedArguments
--  - Clash.Netlist.BlackBox.Types.usedVariables
--  - Clash.Netlist.BlackBox.Types.verifyBlackBoxContext
--  - Clash.Netlist.BlackBox.Types.walkElement
data Element
  = Text !Text
  -- ^ Dumps given text without processing in HDL
  | Component !Decl
  -- ^ Component instantiation hole
  | Result !Bool
  -- ^ Output hole; @Bool@ asserts escape marker stripping
  | Arg !Bool !Int
  -- ^ Input hole; @Bool@ asserts escape marker stripping
  | ArgGen !Int !Int
  -- ^ Like Arg, but its first argument is the scoping level. For use in
  -- in generated code only.
  | Const !Int
  -- ^ Like Arg, but input hole must be a constant.
  | Lit !Int
  -- ^ Like Arg, but input hole must be a literal
  | Name !Int
  -- ^ Name hole
  | Var [Element] !Int
  -- ^ Like Arg but only insert variable reference (creating an assignment
  -- elsewhere if necessary).
  | Sym !Text !Int
  -- ^ Symbol hole
  | Typ !(Maybe Int)
  -- ^ Type declaration hole
  | TypM !(Maybe Int)
  -- ^ Type root hole
  | Err !(Maybe Int)
  -- ^ Error value hole
  | TypElem !Element
  -- ^ Select element type from a vector type
  | CompName
  -- ^ Hole for the name of the component in which the blackbox is instantiated
  | IncludeName !Int
  | IndexType !Element
  -- ^ Index data type hole, the field is the (exclusive) maximum index
  | Size !Element
  -- ^ Size of a type hole
  | Length !Element
  -- ^ Length of a vector hole
  | Depth !Element
  -- ^ Depth of a tree hole
  | MaxIndex !Element
  -- ^ Max index into a vector
  | FilePath !Element
  -- ^ Hole containing a filepath for a data file
  | Template [Element] [Element]
  -- ^ Create data file <HOLE0> with contents <HOLE1>
  | Gen !Bool
  -- ^ Hole marking beginning (True) or end (False) of a generative construct
  | IF !Element [Element] [Element]
  | And [Element]
  | IW64
  -- ^ Hole indicating whether Int/Word/Integer are 64-Bit
  | CmpLE !Element !Element
  -- ^ Compare less-or-equal
  | HdlSyn HdlSyn
  -- ^ Hole indicating which synthesis tool we're generating HDL for
  | BV !Bool [Element] !Element
  -- ^ Convert to (True)/from(False) a bit-vector
  | Sel !Element !Int
  -- ^ Record selector of a type
  | IsLit !Int
  | IsVar !Int
  | IsActiveHigh !Int
  -- ^ Whether a domain's reset lines are synchronous.
  | Tag !Int
  -- ^ Tag of a domain.
  | Period !Int
  -- ^ Period of a domain.
  | IsRisingEdge !Int
  -- ^ Which clock edge memory elements are sensitive to.
  | IsSync !Int
  -- ^ Whether a domain's reset lines are synchronous. Errors if not applied to
  -- a KnownDomain.
  | IsInitDefined !Int
  | IsEnabled !Int
  -- ^ Whether reset line is constantly enabled
  | StrCmp [Element] !Int
  | OutputWireReg !Int
  | Vars !Int
  | GenSym [Element] !Int
  | Repeat [Element] [Element]
  -- ^ Repeat <hole> n times
  | DevNull [Element]
  -- ^ Evaluate <hole> but swallow output
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
