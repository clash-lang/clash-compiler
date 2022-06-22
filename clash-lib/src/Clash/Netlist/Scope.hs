module Clash.Netlist.Scope
  ( Scope(..)
  , emptyScope
  , openScope
  , closeScope
  , addDecl
  , addDecls
  , floatDecl
  , addStmt
  , addStmts
  , floatStmt
  ) where

import Clash.Netlist.Types

-- | When producing netlist, it is sometimes important to have information
-- about different lexical scopes of netlist. This allows some parts of netlist
-- to be floated up as needed.
--
data Scope = Scope
  { decls :: [Declaration]
  , stmts :: [Declaration]
  , parentScope :: Maybe Scope
  } deriving (Show)

{-
NOTE [Fixed and floating declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When generating HDL (especially HDL that combines concurrent and sequential
style code), it may be the case that some declarations / statements are not
valid in a particular place in code. This can cause problems when generating
netlist, as netlist generation may naively wish to generate such invalidly
structured code. Some examples of this are

  * component instantiation, which can only occur in concurrent HDL
  * generate statements, which can only occur in concurrent HDL
  * internal `reg` declared in `begin` ... `end` blocks in verilog, which can
    only appear at the beginning of a named block

A more insidious example of this are black boxes, which may offer different
templates for concurrent and sequential code or restrict their instantiation to
only concurrent or sequential HDL.

Scopes allow these restrictions to be accounted for explicitly when generating
netlist. We can demonstrate this with some examples.

  1. Opening and closing subscopes

     When using scopes, the initial scope is the `emptyScope`, with no
     declarations or parent. When a construct is encountered which contains a
     subscope (e.g. beginning a sequential process), `openScope` is used to
     open the scope. Declarations are added to this subscope by default, and
     when `closeScope` is called, the scope is closed by flattening the list
     of declarations into some item. For example, the scope

     Scope [NetDecl "foo", NetDecl "bar"] [Assign "foo" "bar"] Nothing

     could be closed with a constructor Process to give

     Process [NetDecl "foo", NetDecl "bar", Assign "foo" "bar"]

     as a single declaration. When we close a scope which has a parent scope,
     the new single declaration is appended to the fixed declarations in the
     parent. Closing the outermost scope produces a single declaration which
     can be returned / inserted into the list of returned declarations of a
     normal netlist generation function.

  2. Floating declarations within the current scope

     Suppose Clash currently generated Verilog which looks like

     begin : some_label
       a = b;
       reg c;
       c = a;
       result = c | d;
     end

     This verilog is invalid, as new `reg` must be declared at the beginning of
     the block before any statements (i.e. the `a = b` must come after the
     `reg c` for this to be valid HDL). We can solve this by using
     `addDecl` when adding the `reg c` to the scope so it appears earlier
     in the generated HDL, i.e.

     Scope [NetDecl "c"] [Assign "a" "b", ...] mp

     which renders as

     begin : some_label
       reg c;
       a = b;
       c = a;
       result = c | d;
     end

  3. Floating declarations to the parent scope

     Suppose Clash currently generated VHDL which looks like

     process foo is
       signal x : std_logic;
       variable y : std_logic;
     begin
       ...
     end process;

     This is invalid VHDL, as VHDL does not allow signals to be declared in
     the declarative items of a `process`. In order to produce correct netlist,
     the `signal x` needs to be floated up to the parent scope (since we know
     processes cannot be nested, the parent must be concurrent).

     Instead of the previous

     Scope [NetDecl "x", NetDecl "y"] [...] (Just (Scope [...] [...] mp))

     we can instead float up the declaration of `x` and produce

     Scope [NetDecl "y"] [...] (Just (Scope [..., NetDecl "x"] [...] mp))

  4. Floating statements to the parent scope

     Suppose Clash currently generated HDL which instantiated components while
     inside a process, i.e.

     process foo begin
       bar : entity baz
         port map ( result => quux );
       ...
     end process;

     This is invalid in VHDL and (System)Verilog, as component instantiation
     is a concurrent statement and not sequential. This means the statement
     has to be floated out to the parent scope (since we know the top scope is
     concurrent, and processes cannot be nested, the parent is concurrent).

     Instead of the previous

     Scope [] [Component "bar" "baz", ...] (Just (Scope [...] [...] mp))

     we can instead float up the instantiation statement and produce

     Scope [] [...] (Just (Scope [...] [..., Component "bar" "baz"] mp))
-}

-- | An empty scope contains nothing.
emptyScope :: Scope
emptyScope = Scope [] [] Nothing

-- | Open a new scope with the given parent scope.
openScope :: Scope -> Scope
openScope = Scope [] [] . Just

-- | Flatten the outermost scope into a single statement, which is appended
-- to the parent scope, or if there is no parent scope returns the single new
-- declaration to be inserted into the output netlist.
--
closeScope
  :: ([Declaration] -> Declaration)
  -> Scope
  -> Either Declaration Scope
closeScope flatten scope =
  case parentScope scope of
    Nothing -> Left item
    Just ps -> Right (ps { stmts = stmts ps <> [item] })
 where
  item = flatten (decls scope <> stmts scope)

addDecl :: Declaration -> Scope -> Scope
addDecl d = addDecls [d]

addDecls :: [Declaration] -> Scope -> Scope
addDecls ds scope = scope { decls = decls scope <> ds }

floatDecl :: Declaration -> Scope -> Scope
floatDecl d scope =
  case parentScope scope of
    Just ps -> scope { parentScope = Just (addDecl d ps) }
    Nothing -> error "floatDecl: No parent scope to extend"

addStmt :: Declaration -> Scope -> Scope
addStmt s = addStmts [s]

addStmts :: [Declaration] -> Scope -> Scope
addStmts ss scope = scope { stmts = stmts scope <> ss }

floatStmt :: Declaration -> Scope -> Scope
floatStmt s scope =
  case parentScope scope of
    Just ps -> scope { parentScope = Just (addStmt s ps) }
    Nothing -> error "floatStmt: No parent scope to extend"
