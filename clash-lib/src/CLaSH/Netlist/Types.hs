{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type and instance definitions for Netlist modules
-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module CLaSH.Netlist.Types where

import Control.DeepSeq
import Control.Monad.State.Strict           (MonadIO, MonadState, StateT)
import Control.Monad.Writer.Strict          (MonadWriter, WriterT)
import Data.Hashable
import Data.HashMap.Lazy                    (HashMap)
import Data.IntMap.Lazy                     (IntMap, empty)
import Data.Set                             (Set)
import qualified Data.Text                  as S
import Data.Text.Lazy                       (Text, pack)
import GHC.Generics                         (Generic)
import Unbound.Generics.LocallyNameless              (Fresh, FreshMT)

import SrcLoc                               (SrcSpan)

import CLaSH.Core.Term                      (Term, TmName)
import CLaSH.Core.Type                      (Type)
import CLaSH.Core.TyCon                     (TyCon, TyConName)
import CLaSH.Core.Util                      (Gamma)
import CLaSH.Netlist.BlackBox.Types
import CLaSH.Primitives.Types               (PrimMap)
import CLaSH.Util

-- | Monad that caches generated components (StateT) and remembers hidden inputs
-- of components that are being generated (WriterT)
newtype NetlistMonad a =
  NetlistMonad { runNetlist :: WriterT
                               (Set (Identifier,HWType))
                               (StateT NetlistState (FreshMT IO))
                               a
               }
  deriving (Functor, Monad, Applicative, MonadWriter (Set (Identifier,HWType)),
            MonadState NetlistState, Fresh, MonadIO)

-- | State of the NetlistMonad
data NetlistState
  = NetlistState
  { _bindings       :: HashMap TmName (Type,SrcSpan,Term) -- ^ Global binders
  , _varEnv         :: Gamma -- ^ Type environment/context
  , _varCount       :: !Int -- ^ Number of signal declarations
  , _components     :: HashMap TmName (SrcSpan,Component) -- ^ Cached components
  , _primitives     :: PrimMap BlackBoxTemplate -- ^ Primitive Definitions
  , _typeTranslator :: HashMap TyConName TyCon -> Type -> Maybe (Either String HWType) -- ^ Hardcoded Type -> HWType translator
  , _tcCache        :: HashMap TyConName TyCon -- ^ TyCon cache
  , _curCompNm      :: !(Identifier,SrcSpan)
  , _dataFiles      :: [(String,FilePath)]
  , _intWidth       :: Int
  , _mkBasicIdFn    :: Identifier -> Identifier
  , _seenIds        :: [Identifier]
  , _seenComps      :: [Identifier]
  , _componentNames :: HashMap TmName Identifier
  }

-- | Signal reference
type Identifier = Text

-- | Component: base unit of a Netlist
data Component
  = Component
  { componentName :: !Identifier -- ^ Name of the component
  , hiddenPorts   :: [(Identifier,HWType)] -- ^ Ports that have no correspondence the original function definition
  , inputs        :: [(Identifier,HWType)] -- ^ Input ports
  , outputs       :: [(Identifier,HWType)] -- ^ Output ports
  , declarations  :: [Declaration] -- ^ Internal declarations
  }
  deriving Show

instance NFData Component where
  rnf c = case c of
    Component nm hi inps outps decls -> rnf nm `seq` rnf hi `seq` rnf inps `seq`
                                        rnf outps `seq` rnf decls

-- | Size indication of a type (e.g. bit-size or number of elements)
type Size = Int

-- | Representable hardware types
data HWType
  = Void -- ^ Empty type
  | String -- ^ String type
  | Bool -- ^ Boolean type
  | BitVector !Size -- ^ BitVector of a specified size
  | Index    !Integer -- ^ Unsigned integer with specified (exclusive) upper bounder
  | Signed   !Size -- ^ Signed integer of a specified size
  | Unsigned !Size -- ^ Unsigned integer of a specified size
  | Vector   !Size       !HWType -- ^ Vector type
  | RTree    !Size       !HWType -- ^ RTree type
  | Sum      !Identifier [Identifier] -- ^ Sum type: Name and Constructor names
  | Product  !Identifier [HWType] -- ^ Product type: Name and field types
  | SP       !Identifier [(Identifier,[HWType])] -- ^ Sum-of-Product type: Name and Constructor names + field types
  | Clock    !Identifier !Integer -- ^ Clock type with specified name and period
  | Reset    !Identifier !Integer -- ^ Reset type corresponding to clock with a specified name and period
  deriving (Eq,Ord,Show,Generic)

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
  | InstDecl !Identifier !Identifier [(Identifier,PortDirection,HWType,Expr)] -- ^ Instantiation of another component
  | BlackBoxD !S.Text [S.Text] [S.Text] !BlackBoxTemplate BlackBoxContext -- ^ Instantiation of blackbox declaration
  | NetDecl !Identifier !HWType -- ^ Signal declaration
  deriving Show

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
  deriving Show

-- | Expression used in RHS of a declaration
data Expr
  = Literal    !(Maybe (HWType,Size)) !Literal -- ^ Literal expression
  | DataCon    !HWType       !Modifier  [Expr] -- ^ DataCon application
  | Identifier !Identifier   !(Maybe Modifier) -- ^ Signal reference
  | DataTag    !HWType       !(Either Identifier Identifier) -- ^ @Left e@: tagToEnum#, @Right e@: dataToTag#
  | BlackBoxE !S.Text [S.Text] [S.Text] !BlackBoxTemplate !BlackBoxContext !Bool -- ^ Instantiation of a BlackBox expression
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
  { bbResult    :: (SyncExpr,HWType) -- ^ Result name and type
  , bbInputs    :: [(SyncExpr,HWType,Bool)] -- ^ Argument names, types, and whether it is a literal
  , bbFunctions :: IntMap (Either BlackBoxTemplate Declaration,BlackBoxContext)
  -- ^ Function arguments (subset of inputs):
  --
  -- * (Blackbox Template,Partial Blackbox Concext)
  }
  deriving Show

emptyBBContext :: BlackBoxContext
emptyBBContext = Context (Left $ Identifier (pack "__EMPTY__") Nothing, Void) [] empty

-- | Either the name of the identifier, or a tuple of the identifier and the
-- corresponding clock
type SyncIdentifier = Either Identifier (Identifier,(Identifier,Int))
type SyncExpr       = Either Expr       (Expr,(Identifier,Integer))

makeLenses ''NetlistState
