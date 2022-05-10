module Clash.Netlist.Ast.Concurrent where

import Data.Text (Text)

import Clash.Signal (ActiveEdge)

import Clash.Core.Var (Attr')
import Clash.Netlist.Ast.Declaration
import Clash.Netlist.Ast.Sequential
import Clash.Netlist.Ast.Type
import Clash.Netlist.Id (Identifier)
import Clash.Netlist.BlackBox.Types (BlackBoxTemplate)

import Clash.Netlist.Types hiding (Declaration)

data ConcurrentStmt
  -- | Unconditional continuous assignment
  = ContAssign !Identifier !Expr

  -- | Conditional continuous assignment
  | CondAssign
      !Identifier
      !HWType
      !Expr
      !HWType
      [(Maybe Literal, Expr)]

  -- | Component instantiation
  | Instantiate
      EntityOrComponent
      (Maybe Text)
      [Attr']
      !Identifier
      !Identifier
      [(Expr, HWType, Expr)]

  -- | Black box declaration
  | BlackBoxD
      !Text
      [BlackBoxTemplate]
      [BlackBoxTemplate]
      [((Text, Text), BlackBox)]
      !BlackBox
      BlackBoxContext

  -- | Generate construct
  | Generate !Generate

  -- | Concurrent block
  | ConcurrentBlock
      (Block [ConcurrentStmt])

  -- | Sequential block
  | SequentialBlock
      (Maybe Sensitivity)
      (Block [SequentialStmt])

  -- | HDL tick
  | Tick !CommentOrDirective

  -- | Conditional compilation (not supported in all backends)
  | Ifdef !Text [ConcurrentStmt]
  deriving Show

data Generate
  -- | A branching generate statement
  = GenBranch !Expr !HWType [(Maybe Literal, [ConcurrentStmt])]
  -- | A looping generate statement
  | GenLoop !Identifier !Expr !Expr [ConcurrentStmt]
  deriving Show

data Sensitivity
  -- | Evaluate when the clock reaches the specified edge
  = ClockEdge Expr ActiveEdge
  -- | Evaluate when one of the mentioned identifiers changes
  | ValueChange [Text]
  deriving Show

-- | A block is a scope over some netlist which contains local declarations.
-- In HDL which support local scoping like this, it allows declarations to
-- appear closer to their use sites.
data Block a
  = Block [Declaration] a
  deriving Show

