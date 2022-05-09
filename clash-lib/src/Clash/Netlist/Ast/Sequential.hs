module Clash.Netlist.Ast.Sequential where

import Clash.Netlist.Ast.Type
import Clash.Netlist.Types

data SequentialStmt
  -- | Procedural assignment
  = ProcAssign
      !Identifier
      !Blocking
      !Expr

  -- | Branching constructs, e.g. if statements and case statements
  | Branch
      !Expr
      !HWType
      [(Maybe Literal, [SequentialStmt])]

  -- | Looping constructs, e.g. while loops and for loops
  | Loop
      !LoopType
      [SequentialStmt]
  deriving Show

data Blocking
  -- | Assignment occurs immediately
  = Blocking
  -- | Assignment occurs in the next cycle
  | NonBlocking
  deriving Show

{-
NOTE [blocking / nonblocking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VHDL and (System)Verilog treat procedural assignment differently, providing
two alternative views of the world:

  VHDL
    Signals can be continuously or procedurally assigned, all procedural
    assignments are non-blocking. Variables can only be assigned with blocking
    procedural assignment.

  Verilog
    Wires can be continuously assigned only. Reg can be continuously or
    procedurally assigned and procedural assignment can be blocking or
    non-blocking.

The type of declaration can be determined if we know what type of assignment is
used and whether blocking and/or non-blocking procedural assignment is used.
During netlist generation declarations should be split if they would lead to
invalid HDL for the chosen backend (i.e. blocking and non-blocking procedural
assignment to the same identifier in VHDL).
-}

data LoopType
  -- | Loop until the condition is false
  = CondLoop !Expr
  -- | Loop for @id in [i .. j]@
  | CountLoop !Identifier !Expr !Expr
  deriving Show

{-
NOTE [looping constructs]
~~~~~~~~~~~~~~~~~~~~~~~~~
VHDL and (System)Verilog offer a slightly different selection of looping
constructs when writing sequential code:

  VHDL
    * `while <expr:bool>`
    * `for <id> in <range>`

  Verilog
    * `forever`
    * `repeat <expr:int>`
    * `while <expr:bool>`
    * `for <id> <test> <assign>`

In Clash netlist, we provide only the types of loop that VHDL provides, i.e.
simple conditional and counted loops. The other loops provided by Verilog can
be recovered, since the C style for loop in Verilog can be replaced by a simple
counted loop where inside the loop an expression in terms of the loop variable
is used.
-}
