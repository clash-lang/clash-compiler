{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd,
                    2021-2022, QBayLogic B.V.
                    2022     , LUMI GUIDE FIETSDETECTIE B.V.
                    2022     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Types used in BlackBox modules
-}

{-# LANGUAGE DeriveAnyClass #-}

module Clash.Netlist.BlackBox.Types
 ( BlackBoxMeta(..)
 , emptyBlackBoxMeta
 , BlackBoxFunction
 , BlackBoxTemplate
 , TemplateKind (..)
 , Element(..)
 , EscapedSymbol(..)
 , Decl(..)
 , HdlSyn(..)
 , RenderVoid(..)
 ) where

import                Control.DeepSeq            (NFData)
import                Data.Aeson                 (FromJSON)
import                Data.Binary                (Binary)
import                Data.Hashable              (Hashable)
import                Data.Text.Lazy             (Text)
import qualified      Data.Text                  as S
import                GHC.Generics               (Generic)

import                Clash.Core.Term            (Term)
import                Clash.Core.Type            (Type)
import {-# SOURCE #-} Clash.Netlist.Types
  (BlackBox, NetlistMonad, Usage(Cont))

import qualified      Clash.Signal.Internal      as Signal

-- | Whether this primitive should be rendered when its result type is void.
-- Defaults to 'NoRenderVoid'.
data RenderVoid
  = RenderVoid
  -- ^ Render blackbox, even if result type is void
  | NoRenderVoid
  -- ^ Don't render blackbox result type is void. Default for all blackboxes.
  deriving (Show, Generic, NFData, Binary, Eq, Hashable, FromJSON)

data TemplateKind
  = TDecl
  | TExpr
  deriving (Show, Eq, Generic, NFData, Binary, Hashable)

-- | See 'Clash.Primitives.Types.BlackBox' for documentation on this record's
-- fields. (They are intentionally renamed to prevent name clashes.)
data BlackBoxMeta =
  BlackBoxMeta { bbOutputUsage :: Usage
               , bbKind :: TemplateKind
               , bbLibrary :: [BlackBoxTemplate]
               , bbImports :: [BlackBoxTemplate]
               , bbFunctionPlurality :: [(Int, Int)]
               , bbIncludes :: [((S.Text, S.Text), BlackBox)]
               , bbRenderVoid :: RenderVoid
               , bbResultNames :: [BlackBox]
               , bbResultInits :: [BlackBox]
               }

-- | Use this value in your blackbox template function if you do want to
-- accept the defaults as documented in 'Clash.Primitives.Types.BlackBox'.
emptyBlackBoxMeta :: BlackBoxMeta
emptyBlackBoxMeta = BlackBoxMeta Cont TExpr [] [] [] [] NoRenderVoid [] []

-- | A BlackBox function generates a blackbox template, given the inputs and
-- result type of the function it should provide a blackbox for. This is useful
-- when having a need for blackbox functions, ... TODO: docs
type BlackBoxFunction
   = Bool
  -- ^ Indicates whether caller needs a declaration. If set, the function is
  -- still free to return an expression, but the caller will convert it to a
  -- declaration.
  -> S.Text
  -- ^ Name of primitive
  -> [Either Term Type]
  -- ^ Arguments
  -> [Type]
  -- ^ Result types
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
--  - Clash.Netlist.BlackBox.Util.inputHole
--  - Clash.Netlist.BlackBox.Types.getUsedArguments
--  - Clash.Netlist.BlackBox.Types.usedVariables
--  - Clash.Netlist.BlackBox.Types.verifyBlackBoxContext
--  - Clash.Netlist.BlackBox.Types.walkElement
data Element
  = Text !Text
  -- ^ Dumps given text without processing in HDL
  | Component !Decl
  -- ^ Component instantiation hole
  | Result
  -- ^ Output hole;
  | Arg !Int
  -- ^ Input hole
  | ArgGen !Int !Int
  -- ^ Like Arg, but its first argument is the scoping level. For use in
  -- in generated code only.
  | Const !Int
  -- ^ Like Arg, but input hole must be a constant.
  | Lit !Int
  -- ^ Like Arg, but input hole must be a literal
  | Name !Int
  -- ^ Name hole
  | ToVar [Element] !Int
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
  -- ^ Select element type from a vector-like type
  | CompName
  -- ^ Hole for the name of the component in which the blackbox is instantiated
  | IncludeName !Int
  | IndexType !Element
  -- ^ Index data type hole, the field is the (exclusive) maximum index
  | Size !Element
  -- ^ Size of a type hole
  | Length !Element
  -- ^ Length of a vector-like hole
  | Depth !Element
  -- ^ Depth of a tree hole
  | MaxIndex !Element
  -- ^ Max index into a vector-like type
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
  | IsScalar !Int
  -- ^ Whether element is scalar
  | IsActiveHigh !Int
  -- ^ Whether a domain's reset lines are active high. Errors if not applied to
  -- a @KnownDomain@ or @KnownConfiguration@.
  | Tag !Int
  -- ^ Tag of a domain.
  | Period !Int
  -- ^ Period of a domain. Errors if not applied to a @KnownDomain@ or
  -- @KnownConfiguration@.
  | LongestPeriod
  -- ^ Longest period of all known domains. The minimum duration returned is
  -- 100 ns, see https://github.com/clash-lang/clash-compiler/issues/2455.
  | ActiveEdge !Signal.ActiveEdge !Int
  -- ^ Test active edge of memory elements in a certain domain. Errors if not
  -- applied to a @KnownDomain@ or @KnownConfiguration@.
  | IsSync !Int
  -- ^ Whether a domain's reset lines are synchronous. Errors if not applied to
  -- a @KnownDomain@ or @KnownConfiguration@.
  | IsInitDefined !Int
  -- ^ Whether the initial (or "power up") value of memory elements in a domain
  -- are configurable to a specific value rather than unknown\/undefined. Errors
  -- if not applied to a @KnownDomain@ or @KnownConfiguration@.
  | IsActiveEnable !Int
  -- ^ Whether given enable line is active. More specifically, whether the
  -- enable line is NOT set to a constant 'True'.
  | IsUndefined !Int
  -- ^ Whether argument is undefined. E.g., an XException, error call,
  -- removed argument, or primitive that is undefined. This template tag will
  -- always return 0 (False) if `-fclash-aggressive-x-optimization-blackboxes`
  -- is NOT set.
  | StrCmp [Element] !Int
  | OutputUsage !Int
  | Vars !Int
  | GenSym [Element] !Int
  | Repeat [Element] [Element]
  -- ^ Repeat <hole> n times
  | DevNull [Element]
  -- ^ Evaluate <hole> but swallow output
  | SigD [Element] !(Maybe Int)
  | CtxName
  -- ^ The "context name", name set by `Clash.Magic.setName`, defaults to the
  -- name of the closest binder
  | EscapedSymbol EscapedSymbol
  -- ^ Used for "[\" and "\]", they'll be rendered as "[" and "]",
  -- but pretty printed as "[\" and "\]".
  deriving (Show, Generic, NFData, Binary, Eq, Hashable)

data EscapedSymbol = SquareBracketOpen | SquareBracketClose
  deriving (Show, Generic, NFData, Binary, Eq, Hashable)

-- | Component instantiation hole. First argument indicates which function argument
-- to instantiate. Third argument corresponds to output and input assignments,
-- where the first element is the output assignment, and the subsequent elements
-- are the consecutive input assignments.
--
-- The LHS of the tuple is the name of the signal, while the RHS of the tuple
-- is the type of the signal
data Decl
  = Decl
      !Int
      -- ^ Argument position of the function to instantiate
      !Int
      -- ^ Subposition of function: blackboxes can request multiple instances
      -- to be rendered of their given functions. This subposition indicates the
      -- nth function instance to be rendered (zero-indexed).
      --
      -- This is a hack: the proper solution would postpone rendering the
      -- function until the very last moment. The blackbox language has no way
      -- to indicate the subposition, and every ~INST will default its subposition
      -- to zero. Haskell blackboxes can use this data type.
      [(BlackBoxTemplate,BlackBoxTemplate)]
      -- ^ (name of signal, type of signal)
  deriving (Show, Generic, NFData, Binary, Eq, Hashable)

data HdlSyn = Vivado | Quartus | Other
  deriving (Eq, Show, Read, Generic, NFData, Binary, Hashable)
