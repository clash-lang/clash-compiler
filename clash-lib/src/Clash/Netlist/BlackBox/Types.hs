{-# LANGUAGE DeriveAnyClass #-}

{- |
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd,
                    2021-2022, QBayLogic B.V.
                    2022     , LUMI GUIDE FIETSDETECTIE B.V.
                    2022     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Types used in BlackBox modules
-}
module Clash.Netlist.BlackBox.Types (
  BlackBoxMeta (..),
  emptyBlackBoxMeta,
  BlackBoxFunction,
  BlackBoxTemplate,
  TemplateKind (..),
  Element (..),
  EscapedSymbol (..),
  Decl (..),
  HdlSyn (..),
  RenderVoid (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import qualified Data.Text as S
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)

import Clash.Core.Term (Term)
import Clash.Core.Type (Type)
import {-# SOURCE #-} Clash.Netlist.Types (
  BlackBox,
  NetlistMonad,
  Usage (Cont),
 )

import qualified Clash.Signal.Internal as Signal

{- | Whether this primitive should be rendered when its result type is void.
Defaults to 'NoRenderVoid'.
-}
data RenderVoid
  = -- | Render blackbox, even if result type is void
    RenderVoid
  | -- | Don't render blackbox result type is void. Default for all blackboxes.
    NoRenderVoid
  deriving (Show, Generic, NFData, Binary, Eq, Hashable, FromJSON)

data TemplateKind
  = TDecl
  | TExpr
  deriving (Show, Eq, Generic, NFData, Binary, Hashable)

{- | See 'Clash.Primitives.Types.BlackBox' for documentation on this record's
fields. (They are intentionally renamed to prevent name clashes.)
-}
data BlackBoxMeta
  = BlackBoxMeta
  { bbOutputUsage :: Usage
  , bbKind :: TemplateKind
  , bbLibrary :: [BlackBoxTemplate]
  , bbImports :: [BlackBoxTemplate]
  , bbFunctionPlurality :: [(Int, Int)]
  , bbIncludes :: [((S.Text, S.Text), BlackBox)]
  , bbRenderVoid :: RenderVoid
  , bbResultNames :: [BlackBox]
  , bbResultInits :: [BlackBox]
  }

{- | Use this value in your blackbox template function if you do want to
accept the defaults as documented in 'Clash.Primitives.Types.BlackBox'.
-}
emptyBlackBoxMeta :: BlackBoxMeta
emptyBlackBoxMeta = BlackBoxMeta Cont TExpr [] [] [] [] NoRenderVoid [] []

{- | A BlackBox function generates a blackbox template, given the inputs and
result type of the function it should provide a blackbox for. This is useful
when having a need for blackbox functions, ... TODO: docs
-}
type BlackBoxFunction =
  {- | Indicates whether caller needs a declaration. If set, the function is
  still free to return an expression, but the caller will convert it to a
  declaration.
  -}
  Bool ->
  -- | Name of primitive
  S.Text ->
  -- | Arguments
  [Either Term Type] ->
  -- | Result types
  [Type] ->
  NetlistMonad (Either String (BlackBoxMeta, BlackBox))

{- | A BlackBox Template is a List of Elements
TODO: Add name of function for better error messages
-}
type BlackBoxTemplate = [Element]

{- | Elements of a blackbox context. If you extend this list, make sure to
update the following functions:

 - Clash.Netlist.BlackBox.Types.prettyElem
 - Clash.Netlist.BlackBox.Types.renderElem
 - Clash.Netlist.BlackBox.Types.renderTag
 - Clash.Netlist.BlackBox.Types.setSym
 - Clash.Netlist.BlackBox.Util.inputHole
 - Clash.Netlist.BlackBox.Types.getUsedArguments
 - Clash.Netlist.BlackBox.Types.usedVariables
 - Clash.Netlist.BlackBox.Types.verifyBlackBoxContext
 - Clash.Netlist.BlackBox.Types.walkElement
-}
data Element
  = -- | Dumps given text without processing in HDL
    Text !Text
  | -- | Component instantiation hole
    Component !Decl
  | -- | Output hole;
    Result
  | -- | Input hole
    Arg !Int
  | {- | Like Arg, but its first argument is the scoping level. For use in
    in generated code only.
    -}
    ArgGen !Int !Int
  | -- | Like Arg, but input hole must be a constant.
    Const !Int
  | -- | Like Arg, but input hole must be a literal
    Lit !Int
  | -- | Name hole
    Name !Int
  | {- | Like Arg but only insert variable reference (creating an assignment
    elsewhere if necessary).
    -}
    ToVar [Element] !Int
  | -- | Symbol hole
    Sym !Text !Int
  | -- | Type declaration hole
    Typ !(Maybe Int)
  | -- | Type root hole
    TypM !(Maybe Int)
  | -- | Error value hole
    Err !(Maybe Int)
  | -- | Select element type from a vector-like type
    TypElem !Element
  | -- | Hole for the name of the component in which the blackbox is instantiated
    CompName
  | IncludeName !Int
  | -- | Index data type hole, the field is the (exclusive) maximum index
    IndexType !Element
  | -- | Size of a type hole
    Size !Element
  | -- | Length of a vector-like hole
    Length !Element
  | -- | Depth of a tree hole
    Depth !Element
  | -- | Max index into a vector-like type
    MaxIndex !Element
  | -- | Hole containing a filepath for a data file
    FilePath !Element
  | -- | Create data file <HOLE0> with contents <HOLE1>
    Template [Element] [Element]
  | -- | Hole marking beginning (True) or end (False) of a generative construct
    Gen !Bool
  | IF !Element [Element] [Element]
  | And [Element]
  | -- | Hole indicating whether Int/Word/Integer are 64-Bit
    IW64
  | -- | Compare less-or-equal
    CmpLE !Element !Element
  | -- | Hole indicating which synthesis tool we're generating HDL for
    HdlSyn HdlSyn
  | -- | Convert to (True)/from(False) a bit-vector
    BV !Bool [Element] !Element
  | -- | Record selector of a type
    Sel !Element !Int
  | IsLit !Int
  | IsVar !Int
  | -- | Whether element is scalar
    IsScalar !Int
  | {- | Whether a domain's reset lines are active high. Errors if not applied to
    a @KnownDomain@ or @KnownConfiguration@.
    -}
    IsActiveHigh !Int
  | -- | Tag of a domain.
    Tag !Int
  | {- | Period of a domain. Errors if not applied to a @KnownDomain@ or
    @KnownConfiguration@.
    -}
    Period !Int
  | {- | Longest period of all known domains. The minimum duration returned is
    100 ns, see https://github.com/clash-lang/clash-compiler/issues/2455.
    -}
    LongestPeriod
  | {- | Test active edge of memory elements in a certain domain. Errors if not
    applied to a @KnownDomain@ or @KnownConfiguration@.
    -}
    ActiveEdge !Signal.ActiveEdge !Int
  | {- | Whether a domain's reset lines are synchronous. Errors if not applied to
    a @KnownDomain@ or @KnownConfiguration@.
    -}
    IsSync !Int
  | {- | Whether the initial (or "power up") value of memory elements in a domain
    are configurable to a specific value rather than unknown\/undefined. Errors
    if not applied to a @KnownDomain@ or @KnownConfiguration@.
    -}
    IsInitDefined !Int
  | {- | Whether given enable line is active. More specifically, whether the
    enable line is NOT set to a constant 'True'.
    -}
    IsActiveEnable !Int
  | {- | Whether argument is undefined. E.g., an XException, error call,
    removed argument, or primitive that is undefined. This template tag will
    always return 0 (False) if `-fclash-aggressive-x-optimization-blackboxes`
    is NOT set.
    -}
    IsUndefined !Int
  | StrCmp [Element] !Int
  | OutputUsage !Int
  | Vars !Int
  | GenSym [Element] !Int
  | -- | Repeat <hole> n times
    Repeat [Element] [Element]
  | -- | Evaluate <hole> but swallow output
    DevNull [Element]
  | SigD [Element] !(Maybe Int)
  | {- | The "context name", name set by `Clash.Magic.setName`, defaults to the
    name of the closest binder
    -}
    CtxName
  | {- | Used for "[\" and "\]", they'll be rendered as "[" and "]",
    but pretty printed as "[\" and "\]".
    -}
    EscapedSymbol EscapedSymbol
  deriving (Show, Generic, NFData, Binary, Eq, Hashable)

data EscapedSymbol = SquareBracketOpen | SquareBracketClose
  deriving (Show, Generic, NFData, Binary, Eq, Hashable)

{- | Component instantiation hole. First argument indicates which function argument
to instantiate. Third argument corresponds to output and input assignments,
where the first element is the output assignment, and the subsequent elements
are the consecutive input assignments.

The LHS of the tuple is the name of the signal, while the RHS of the tuple
is the type of the signal
-}
data Decl
  = Decl
      -- | Argument position of the function to instantiate
      !Int
      {- | Subposition of function: blackboxes can request multiple instances
      to be rendered of their given functions. This subposition indicates the
      nth function instance to be rendered (zero-indexed).

      This is a hack: the proper solution would postpone rendering the
      function until the very last moment. The blackbox language has no way
      to indicate the subposition, and every ~INST will default its subposition
      to zero. Haskell blackboxes can use this data type.
      -}
      !Int
      -- | (name of signal, type of signal)
      [(BlackBoxTemplate, BlackBoxTemplate)]
  deriving (Show, Generic, NFData, Binary, Eq, Hashable)

data HdlSyn = Vivado | Quartus | Other
  deriving (Eq, Show, Read, Generic, NFData, Binary, Hashable)
