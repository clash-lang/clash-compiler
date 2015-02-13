{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PolyKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
module CLaSH.Backend.Verilog.BoringTypes where

import           Data.Foldable
import           Data.Traversable

--import           Control.Applicative
import           Control.DeepSeq

import           Language.Verilog.AST (SizedLiteral)

import qualified CLaSH.Netlist.Types as N

-- | Component: base unit of a Netlist
data Component blackbox
  = Component
  { componentName :: N.Identifier -- ^ Name of the component
  , hiddenPorts   :: [(N.Identifier, HWType)] -- ^ Ports that have no correspondence the original function definition
  , inputs        :: [(N.Identifier, HWType)] -- ^ Input ports
  , output        :: (N.Identifier, HWType) -- ^ Output port
  , declarations  :: [Either blackbox (Declaration blackbox)] -- ^ Internal declarations
  }
  deriving Show

instance NFData blackbox => NFData (Component blackbox) where
  rnf (Component nm hi inps outps decls) =
    rnf nm `seq` rnf hi `seq` rnf inps `seq` rnf outps `seq` rnf decls

data HWType
  = Integer              -- ^ Just for meta ?
  | Bits Bool N.Size     -- ^ Bits, is-signed
  | Vector N.Size HWType
  | Sum N.Identifier [N.Identifier]
  | Product N.Identifier [HWType]
  | SP N.Identifier [(N.Identifier,[HWType])]
  deriving (Eq, Show)

instance NFData HWType where
  rnf = \case
    Integer  -> ()
    Bits bool size -> rnf bool `seq` rnf size
    Vector s el -> rnf s `seq` rnf el
    Sum i ids -> rnf i `seq` rnf ids
    Product i ids -> rnf i `seq` rnf ids
    SP i ids -> rnf i `seq` rnf ids

data Declaration blackbox
  = Assignment      -- ^ Signal assignment
    [N.Identifier]  -- * Signal to assign (list because concats can be lvalues)
    (Expr blackbox) -- * Assigned expression

  | CondAssignment                           -- ^ Conditional signal assignment
    N.Identifier                             -- * Signal to assign
    (Expr blackbox)                          -- * Scrutinized expression
    [(Maybe (Expr blackbox), Expr blackbox)] -- * List of: (Maybe expression scrutinized expression is compared with,RHS of alternative)


  | InstDecl -- ^ Instantiation of another component
    N.Identifier
    N.Identifier
    [(N.Identifier, Expr blackbox)]

  | NetDecl N.Identifier HWType (Maybe (Expr blackbox)) -- ^ Signal declaration
  deriving Show
  --deriving (Show, Functor, Foldable, Traversable)

instance NFData (Declaration blackbox) where
  rnf a = a `seq` ()


-- | CoreExpression used in RHS of a declaration
data NonIndex recur
  = Literal    SizedLiteral -- ^ Literal expression
  | Concat     [recur]      -- ^ New!
  | Identifier N.Identifier -- ^ Signal reference
  | DataTag    Bool recur -- ^ @False e@: tagToEnum#, @True e@: dataToTag#
  | DataCon    [recur] -- ^ DataCon application
  deriving (Show, Functor, Foldable, Traversable)

-- | Core Expression type that doesn't permit indexing "twice in a row"
data CoreExpr index noIndex
  = E index
  | Index Int Int noIndex
  deriving (Show, Functor, Foldable, Traversable)

type CoreExprHelper (splice :: * -> *) recur = splice :$ CoreExpr (NonIndex :$ recur) (splice :$ NonIndex :$ recur)

-- | Add optional typing to every node
data MaybeTyped recur
  = MT (Maybe HWType) recur
  deriving (Show, Functor, Foldable, Traversable)

infixr 0 :$
type (:$) (f :: a -> *) (x :: a) = (f x)
infixr 9 :.
type (:.) (f :: b -> *) (g :: a -> b) (x :: a) = (f (g x))

data Mu (t :: * -> *) = Mu (t :$ Mu t)

type BigRecur blackbox = NonIndex :$ Expr blackbox
type Splice blackbox x = MaybeTyped :$ Either blackbox x

data Expr blackbox =
  MTBBE (Splice blackbox
                (CoreExpr (BigRecur blackbox)
                      (Splice blackbox (BigRecur blackbox))))
  deriving Show
  --deriving (Show, Functor, Foldable, Traversable)
