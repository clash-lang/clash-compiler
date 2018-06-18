{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type and instance definitions for Netlist modules
-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Netlist.Types
  (Declaration (..,NetDecl), module Clash.Netlist.Types)
where

import Control.DeepSeq
import Control.Monad.State.Strict           (MonadIO, MonadState, StateT)
import Data.Hashable
import Data.HashMap.Lazy                    (HashMap)
import Data.IntMap.Lazy                     (IntMap, empty)
import qualified Data.Text                  as S
import Data.Text.Lazy                       (Text, pack)
import GHC.Generics                         (Generic)
import Unbound.Generics.LocallyNameless              (Fresh, FreshMT)

import SrcLoc                               (SrcSpan)

import Clash.Annotations.TopEntity          (TopEntity)
import Clash.Core.Term                      (TmOccName)
import Clash.Core.Type                      (Type)
import Clash.Core.TyCon                     (TyCon, TyConOccName)
import Clash.Driver.Types                   (BindingMap)
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Id                     (IdType)
import Clash.Primitives.Types               (PrimMap)
import Clash.Signal.Internal                (ClockKind, ResetKind)
import Clash.Util

-- | Monad that caches generated components (StateT) and remembers hidden inputs
-- of components that are being generated (WriterT)
newtype NetlistMonad a =
  NetlistMonad { runNetlist :: StateT NetlistState (FreshMT IO) a }
  deriving (Functor, Monad, Applicative, MonadState NetlistState, Fresh, MonadIO)

-- | State of the NetlistMonad
data NetlistState
  = NetlistState
  { _bindings       :: BindingMap -- ^ Global binders
  , _varCount       :: !Int -- ^ Number of signal declarations
  , _components     :: HashMap TmOccName (SrcSpan,Component) -- ^ Cached components
  , _primitives     :: PrimMap BlackBoxTemplate -- ^ Primitive Definitions
  , _typeTranslator :: HashMap TyConOccName TyCon -> Bool -> Type -> Maybe (Either String HWType)
  -- ^ Hardcoded Type -> HWType translator
  , _tcCache        :: HashMap TyConOccName TyCon -- ^ TyCon cache
  , _curCompNm      :: !(Identifier,SrcSpan)
  , _dataFiles      :: [(String,FilePath)]
  , _intWidth       :: Int
  , _mkIdentifierFn :: IdType -> Identifier -> Identifier
  , _extendIdentifierFn :: IdType -> Identifier -> Identifier -> Identifier
  , _seenIds        :: [Identifier]
  , _seenComps      :: [Identifier]
  , _componentNames :: HashMap TmOccName Identifier
  , _topEntityAnns  :: HashMap TmOccName (Type, Maybe TopEntity)
  , _hdlDir         :: FilePath
  }

-- | Signal reference
type Identifier = Text

-- | Component: base unit of a Netlist
data Component
  = Component
  { componentName :: !Identifier -- ^ Name of the component
  , inputs        :: [(Identifier,HWType)] -- ^ Input ports
  , outputs       :: [(WireOrReg,(Identifier,HWType))] -- ^ Output ports
  , declarations  :: [Declaration] -- ^ Internal declarations
  }
  deriving Show

instance NFData Component where
  rnf c = case c of
    Component nm inps outps decls -> rnf nm    `seq` rnf inps `seq`
                                     rnf outps `seq` rnf decls

-- | Size indication of a type (e.g. bit-size or number of elements)
type Size = Int

-- | Representable hardware types
data HWType
  = Void (Maybe HWType)
  -- ^ Empty type. @Just Size@ for "empty" Vectors so we can still have
  -- primitives that can traverse e.g. Vectors of unit and know the lenght of
  -- that vector.
  | String -- ^ String type
  | Bool -- ^ Boolean type
  | Bit -- ^ Bit type
  | BitVector !Size -- ^ BitVector of a specified size
  | Index    !Integer -- ^ Unsigned integer with specified (exclusive) upper bounder
  | Signed   !Size -- ^ Signed integer of a specified size
  | Unsigned !Size -- ^ Unsigned integer of a specified size
  | Vector   !Size       !HWType -- ^ Vector type
  | RTree    !Size       !HWType -- ^ RTree type
  | Sum      !Identifier [Identifier] -- ^ Sum type: Name and Constructor names
  | Product  !Identifier [HWType] -- ^ Product type: Name and field types
  | SP       !Identifier [(Identifier,[HWType])] -- ^ Sum-of-Product type: Name and Constructor names + field types
  | Clock    !Identifier !Integer !ClockKind -- ^ Clock type with specified name and period
  | Reset    !Identifier !Integer !ResetKind -- ^ Reset type corresponding to clock with a specified name and period
  deriving (Eq,Ord,Show,Generic)

instance Hashable ClockKind
instance Hashable ResetKind

instance Hashable HWType
instance NFData HWType

-- | Internals of a Component
data Declaration
  = Assignment !Identifier !Expr
  -- ^ Signal assignment:
  --
  -- * Signal to assign
  --
  -- * Assigned expression
  | CondAssignment !Identifier !HWType !Expr !HWType [(Maybe Literal,Expr)]
  -- ^ Conditional signal assignment:
  --
  -- * Signal to assign
  --
  -- * Type of the result/alternatives
  --
  -- * Scrutinized expression
  --
  -- * Type of the scrutinee
  --
  -- * List of: (Maybe expression scrutinized expression is compared with,RHS of alternative)
  | InstDecl (Maybe Identifier) !Identifier !Identifier [(Expr,PortDirection,HWType,Expr)] -- ^ Instantiation of another component
  | BlackBoxD !S.Text [BlackBoxTemplate] [BlackBoxTemplate] [((S.Text,S.Text),BlackBoxTemplate)] !BlackBoxTemplate BlackBoxContext -- ^ Instantiation of blackbox declaration
  | NetDecl' (Maybe Identifier) WireOrReg !Identifier (Either Identifier HWType) -- ^ Signal declaration
  deriving Show

data WireOrReg = Wire | Reg
  deriving (Show,Generic)

instance NFData WireOrReg

pattern NetDecl :: Maybe Identifier -> Identifier -> HWType -> Declaration
pattern NetDecl note d ty <- NetDecl' note Wire d (Right ty)
  where
    NetDecl note d ty = NetDecl' note Wire d (Right ty)

data PortDirection = In | Out
  deriving Show

instance NFData Declaration where
  rnf a = a `seq` ()

-- | Expression Modifier
data Modifier
  = Indexed (HWType,Int,Int) -- ^ Index the expression: (Type of expression,DataCon tag,Field Tag)
  | DC (HWType,Int) -- ^ See expression in a DataCon context: (Type of the expression, DataCon tag)
  | VecAppend -- ^ See the expression in the context of a Vector append operation
  | RTreeAppend -- ^ See the expression in the context of a Tree append operation
  | Nested Modifier Modifier
  deriving Show

-- | Expression used in RHS of a declaration
data Expr
  = Literal    !(Maybe (HWType,Size)) !Literal -- ^ Literal expression
  | DataCon    !HWType       !Modifier  [Expr] -- ^ DataCon application
  | Identifier !Identifier   !(Maybe Modifier) -- ^ Signal reference
  | DataTag    !HWType       !(Either Identifier Identifier) -- ^ @Left e@: tagToEnum#, @Right e@: dataToTag#
  | BlackBoxE !S.Text [BlackBoxTemplate] [BlackBoxTemplate] [((S.Text,S.Text),BlackBoxTemplate)] !BlackBoxTemplate !BlackBoxContext !Bool -- ^ Instantiation of a BlackBox expression
  | ConvBV     (Maybe Identifier) HWType Bool Expr
  deriving Show

-- | Literals used in an expression
data Literal
  = NumLit    !Integer   -- ^ Number literal
  | BitLit    !Bit       -- ^ Bit literal
  | BoolLit   !Bool      -- ^ Boolean literal
  | VecLit    [Literal] -- ^ Vector literal
  | StringLit !String    -- ^ String literal
  deriving (Eq,Show)

-- | Bit literal
data Bit
  = H -- ^ High
  | L -- ^ Low
  | U -- ^ Undefined
  | Z -- ^ High-impedance
  deriving (Eq,Show)

-- | Context used to fill in the holes of a BlackBox template
data BlackBoxContext
  = Context
  { bbResult    :: (Expr,HWType) -- ^ Result name and type
  , bbInputs    :: [(Expr,HWType,Bool)] -- ^ Argument names, types, and whether it is a literal
  , bbFunctions :: IntMap (Either BlackBoxTemplate (Identifier,[Declaration])
                          ,WireOrReg
                          ,[BlackBoxTemplate]
                          ,[BlackBoxTemplate]
                          ,[((S.Text,S.Text),BlackBoxTemplate)]
                          ,BlackBoxContext)
  -- ^ Function arguments (subset of inputs):
  --
  -- * ( Blackbox Template
  --   , Whether the result should be /reg/ or a /wire/ (Verilog only)
  --   , Partial Blackbox Context
  --   )
  , bbQsysIncName :: [Identifier]
  }
  deriving Show

emptyBBContext :: BlackBoxContext
emptyBBContext = Context (Identifier (pack "__EMPTY__") Nothing, Void Nothing) [] empty []

makeLenses ''NetlistState
