{-|
  Copyright   :  (C) 2022 Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Builder for well-formed netlist according to lexical scope.
-}

module Clash.Netlist.Scope
  ( Scope(..)
  , emptyScope
  , isTopScope
  , openScope
  , closeScope
  , flattenScope
  , addDecl
  , addDecls
  , floatDecl
  , floatDeclTo
  , addStmt
  , addStmts
  , floatStmt
  , floatStmtTo
  ) where

import Data.Maybe (isNothing)

import {-# SOURCE #-} Clash.Netlist.Types (Declaration, DeclarationType(..))

-- | When producing netlist, it is sometimes important to have information
-- about different lexical scopes of netlist. This allows some parts of netlist
-- to be floated up as needed.
--
data Scope = Scope
  { decls :: [Declaration]
    -- ^ Declarations in the current scope. These appear first within a scope
    -- when it is flattened to a list of declarations.
  , stmts :: [Declaration]
    -- ^ Statements in the current scope. These appear last within a scope
    -- when it is flattened to a list of declarations.
  , scopeType :: DeclarationType
    -- ^ Whether the current scope is concurrent or sequential. Both
    -- declarations and statements may need to be floating up to the nearest
    -- compatible type of scope (i.e. component instantiation always floats to
    -- the nearest concurrent scope).
  , parentScope :: Maybe Scope
    -- ^ The parent scope of this scope, if it is not the top-level. Floating
    -- places a declaration / statement here (or in a parent of this scope).
  } deriving (Show)

{-
NOTE [Floating declarations out]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When generating HDL (especially HDL that combines concurrent and sequential
style code), it may be the case that some declarations / statements are not
valid in a particular place in code. This can cause problems when generating
netlist, as netlist generation may naively wish to generate such invalidly
structured code. Some examples of this are

  * component instantiation, which can only occur in concurrent HDL
  * generate statements, which can only occur in concurrent HDL
  * internal `reg` declared in `begin` ... `end` blocks in verilog, which can
    only appear at the beginning of a named block

A more insidious example of this is in higher order functions, where rendering
the function arguments may produce additional declarations which appear in an
invalid place for the target HDL.

Scopes allow these restrictions to be accounted for explicitly when generating
netlist. We can demonstrate this with some examples.

  1. Opening and closing sub-scopes

     When using scopes, the initial scope is the `emptyScope`, with no
     declarations or parent. When a construct is encountered which contains a
     sub-scope (e.g. beginning a sequential process), `openScope` is used to
     open the scope. Declarations are added to this sub-scope by default, and
     when `closeScope` is called, the scope is closed by flattening the list
     of declarations into some item. For example, the scope

     Scope [NetDecl "foo", NetDecl "bar"] [Assign "foo" "bar"] Nothing

     could be closed with a constructor `Process` to give

     Process [NetDecl "foo", NetDecl "bar", Assign "foo" "bar"]

     as a single declaration. When we close a scope which has a parent scope,
     the new single declaration is appended to the "statements" in the parent.
     Closing the outermost scope produces a single declaration which can be
     returned / inserted into the list of returned declarations of a normal
     netlist generation function.

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

     which renders as

     architecture rtl of some_entity is
       ...
       signal x : std_logic;
       ...
     begin
       ...
       process foo is
         variable y : std_logic;
       begin
         ...
       end process;
       ...
     end architecture;

  4. Floating statements to the parent scope

     Suppose Clash currently generated HDL which instantiated components while
     inside a process, i.e.

     process foo is
       signal quux : std_logic;
     begin
       bar : entity baz
         port map ( result => quux );
       ...
     end process;

     This is invalid in VHDL and (System)Verilog, as component instantiation
     is a concurrent statement and not sequential. This means the statement
     has to be floated out to the parent scope (since we know the top scope is
     concurrent, and processes cannot be nested, the parent is concurrent).

     Instead of the previous

     Scope [NetDecl "quux"] [Component "bar" "baz", ...]
       (Just (Scope [...] [...] mp))

     we can instead float up the instantiation statement and signal for the
     output of the component and produce

     Scope [] [...]
       (Just (Scope [..., NetDecl "quux"] [..., Component "bar" "baz"] mp))

     which renders as

     architecture rtl of some_entity is
       ...
       signal quux : std_logic;
     begin
       bar : entity baz
         port map ( result => quux );

       process foo begin
         ...
       end process;
       ...
     end architecture;

The examples involving floating declarations upwards can be even more tricky
to make correct when netlist functions simply handle lists of declarations. If
a declaration needs to be floated up in such functions, it is only convenient
to insert it at the start or end of a list of declarations that are merged to
produce the final list of declarations. Inserting into an arbitrary place in a
different list can be more tedious and error-prone.
-}

-- | An empty scope contains no declarations, and is in a concurrent context
-- (since this is the context all HDL generation starts in).
--
emptyScope :: Scope
emptyScope = Scope [] [] Concurrent Nothing

isTopScope :: Scope -> Bool
isTopScope = isNothing . parentScope

-- | Open a new scope with the given parent scope.
openScope :: DeclarationType -> Scope -> Scope
openScope ty = Scope [] [] ty . Just

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

flattenScope :: Scope -> [Declaration]
flattenScope scope
  | isTopScope scope
  = decls scope <> stmts scope

  | otherwise
  = error ("flattenScope: Cannot flatten unclosed subscope:\n" <> show scope)

addDecl :: Declaration -> Scope -> Scope
addDecl d = addDecls [d]

addDecls :: [Declaration] -> Scope -> Scope
addDecls ds scope = scope { decls = decls scope <> ds }

floatDecl :: Declaration -> Scope -> Scope
floatDecl d scope =
  case parentScope scope of
    Just ps -> scope { parentScope = Just (addDecl d ps) }
    Nothing -> error "floatDecl: No parent scope to extend"

floatDeclTo :: DeclarationType -> Declaration -> Scope -> Scope
floatDeclTo ty d = go
 where
  go scope
    | scopeType scope == ty
    = addDecl d scope

    | Just parent <- parentScope scope
    = scope { parentScope = Just (go parent) }

    | otherwise
    = error ("floatDeclTo: No scope with type " <> show ty <> " found")

addStmt :: Declaration -> Scope -> Scope
addStmt s = addStmts [s]

addStmts :: [Declaration] -> Scope -> Scope
addStmts ss scope = scope { stmts = stmts scope <> ss }

floatStmt :: Declaration -> Scope -> Scope
floatStmt s scope =
  case parentScope scope of
    Just ps -> scope { parentScope = Just (addStmt s ps) }
    Nothing -> error "floatStmt: No parent scope to extend"

floatStmtTo :: DeclarationType -> Declaration -> Scope -> Scope
floatStmtTo ty d = go
 where
  go scope
    | scopeType scope == ty
    = addStmt d scope

    | Just parent <- parentScope scope
    = scope { parentScope = Just (go parent) }

    | otherwise
    = error ("floatStmtTo: No scope with type " <> show ty <> " found")
