Clash/Haskell Style Guide
=========================

This is a short document describing the preferred coding style for this
project. When something isn't covered by this guide you should stay
consistent with the code in the other modules. The code style rules
should be considered strong suggestions but shouldn't be dogmatically
applied - if there's a good reason for breaking them *do it*. If you
can't or don't want to apply a guideline or if a guideline is missing,
consider:

-  **How your style affects future changes.** Does changing part of it
   cause a lot of realignments? Is it easily extendable by copy-pasting
   lines?
-  **Whether whitespace is effectively used.** Do new indent-blocks
   start 2 spaces deeper than the previous one? Is it easy to see which
   block is which?
-  **How it scales.** Is the style applicable to small examples as well
   as large ones?

The guidelines formulated below try to balance the points above.

Formatting
----------

Line Length
~~~~~~~~~~~

Try to keep below *80 characters* (soft), never exceed *90* (hard).

Indentation
~~~~~~~~~~~

Tabs are illegal. Use spaces for indenting. Indent your code blocks with
*2 spaces*. Indent the ``where`` keyword 1 space to set it apart from
the rest of the code and indent the definitions in a ``where`` clause 1
space. Some examples:

.. code:: haskell

    sayHello :: IO ()
    sayHello = do
      name <- getLine
      putStrLn $ greeting name
     where
      greeting name = "Hello, " ++ name ++ "!"

    filter
      :: (a -> Bool)
      -> [a]
      -> [a]
    filter _ [] = []
    filter p (x:xs)
      | p x       = x : filter p xs
      | otherwise = filter p xs

Blank Lines
~~~~~~~~~~~

One blank line between top-level definitions. No blank lines between
type signatures and function definitions. Add one blank line between
functions in a type class instance declaration if the function bodies
are large. Use your judgement.

Whitespace
~~~~~~~~~~

Surround binary operators with a single space on either side. Use your
better judgement for the insertion of spaces around arithmetic operators
but always be consistent about whitespace on either side of a binary
operator. Don't insert a space after a lambda. Add a space after each
comma in a tuple:

.. code:: haskell

    good = (a, b, c)
    bad = (a,b,c)

Refuse the temptation to use the latter when almost hitting the
line-length limit. Restructure your code or use multiline notation
instead. An example of a multiline tuple declaration is:

.. code:: haskell

    goodMulti =
      ( a
      , b
      , c )

    goodMulti2 =
      ( a
      , b
      , c
      )

Use nested tuples as such:

.. code:: haskell

    nested =
      ( ( a1
        , a2 )
      , b
      , c )

Similar to ``goodMulti2``, you can put the trailing ``)`` on a new line.
Use your judgement.

Data Declarations
~~~~~~~~~~~~~~~~~

Align the constructors in a data type definition. If a data type has
multiple constructors, each constructor will get its own line. Example:

.. code:: haskell

    data Tree a
      = Branch !a !(Tree a) !(Tree a)
      | Leaf
      deriving (Eq, Show)

Data types deriving lots of instances may be written like:

.. code:: haskell

    data Tree a
      = Branch !a !(Tree a) !(Tree a)
      | Leaf
      deriving
        ( Eq, Show, Ord, Read, Functor, Generic, NFData
        , NFDataX, BitPack, ShowX)

Data types with a single constructor may be written on a single line:

.. code:: haskell

    data Foo = Foo Int

Format records as follows:

.. code:: haskell

    data Person = Person
      { firstName :: !String
      -- ^ First name
      , lastName :: !String
      -- ^ Last name
      , age :: !Int
      -- ^ Age
      } deriving (Eq, Show)

List Declarations
~~~~~~~~~~~~~~~~~

Align the elements in the list. Example:

.. code:: haskell

    exceptions =
      [ InvalidStatusCode
      , MissingContentHeader
      , InternalServerError ]

You may put the closing bracket on a new line. Use your judgement.

.. code:: haskell

    exceptions =
      [ InvalidStatusCode
      , MissingContentHeader
      , InternalServerError
      ]

You may not skip the first newline.

.. code:: haskell

    -- WRONG!
    directions = [ North
                 , East
                 , South
                 , West
                 ]

*unless* it fits on a single line:

.. code:: haskell

    directions = [North, East, South, West]

Vector Declarations
~~~~~~~~~~~~~~~~~~~

Small vectors may be written on a single line:

.. code:: haskell

    nrs = 1 :> 2 :> 3 :> 4 :> 5 :> Nil

Large vectors should be written like:

.. code:: haskell

    exceptions =
         North
      :> East
      :> South
      :> West
      :> Nil

Or:

.. code:: haskell

    exceptions =
         North :> East :> South
      :> West :> Middle :> Nil

Language pragmas
~~~~~~~~~~~~~~~~

Place LANGUAGE pragmas right after a module's documentation. Do not
align the ``#-}``\ s. ``Safe``, ``Unsafe``, or in some way "special"
language pragmas should follow the normal ones separated by a single
blank line. Pragmas should be ordered alphabetically. Example:

.. code:: haskell

    {-|
      .. docs ..
    -}

    {-# LANGUAGE CPP #-}
    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE FlexibleInstances #-}
    {-# LANGUAGE QuasiQuotes #-}

    {-# LANGUAGE Safe #-}

Pragmas
~~~~~~~

Put pragmas immediately following the function they apply to. Example:

.. code:: haskell

    id :: a -> a
    id x = x
    {-# NOINLINE id #-}

Hanging Lambdas
~~~~~~~~~~~~~~~

You may or may not indent the code following a "hanging" lambda. Use
your judgement. Some examples:

.. code:: haskell

    bar :: IO ()
    bar =
      forM_ [1, 2, 3] $ \n -> do
        putStrLn "Here comes a number!"
        print n

    foo :: IO ()
    foo =
      alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b

Export Lists
~~~~~~~~~~~~

Format export lists as follows:

.. code:: haskell

    module Data.Set
      (
        -- * The @Set@ type
        Set
      , empty
      , singleton

        -- * Querying
      , member
      ) where

If-then-else clauses
~~~~~~~~~~~~~~~~~~~~

Generally, guards and pattern matches should be preferred over
if-then-else clauses. Short cases should usually be put on a single
line.

When writing non-monadic code (i.e. when not using ``do``) and guards
and pattern matches can't be used, you can align if-then-else clauses
like you would normal expressions:

.. code:: haskell

    foo =
      if cond0 then
        ...
      else
        ...

When used in monadic contexts, use:

.. code:: haskell

    foo =
      if cond0 then do
        ...
      else do
        ...

The same rule applies to nested do blocks:

.. code:: haskell

    foo = do
      instruction <- decodeInstruction
      skip <- load Memory.skip
      if skip == 0x0000 then do
        execute instruction
        addCycles $ instructionCycles instruction
      else do
        store Memory.skip 0x0000
        addCycles 1

Case expressions
~~~~~~~~~~~~~~~~

The alternatives in a case expression can be indented using either of
the two following styles:

.. code:: haskell

    foobar =
      case something of
        Just j  -> foo
        Nothing -> bar

or as

.. code:: haskell

    foobar =
      case something of
        Just j ->
          foo
        Nothing ->
          bar

In monadic contexts, use:

.. code:: haskell

    foobar =
      case something of
        Just j -> do
          foo
          bar
        Nothing -> do
          fizz
          buzz

Align the ``->`` arrows when it helps readability, but keep in mind that
any changes potentially trigger a lot of realignments. This increases
your VCS's diff sizes and becomes tedious quickly.

Type signatures
~~~~~~~~~~~~~~~

Small type signatures can be put on a single line:

.. code:: haskell

    f :: a -> a -> b

Longer ones should be put on multiple lines:

.. code:: haskell

    toInt
      :: Int
      -- ^ Shift char by /n/
      -> Char
      -- ^ Char to convert to ASCII integer
      -> Int

Multiple constraints can be added with a "tuple":

.. code:: haskell

    toInt
      :: (Num a, Show a)
      => a
      -- ^ Shift char by /n/
      -> Char
      -- ^ Char to convert to ASCII integer
      -> Int

Many constraints need to be split accross multiple lines too:

.. code:: haskell

    toInt
      :: ( Num a
         , Show a
         , Foo a
         , Bar a
         , Fizz a
         )
      => a
      -- ^ Shift char by /n/
      -> Char
      -- ^ Char to convert to ASCII integer
      -> Int

``forall``'s dot must be aligned:

.. code:: haskell

    toInt
      :: forall a
       . (Num a , Show a)
      => a
      -- ^ Shift char by /n/
      -> Char
      -- ^ Char to convert to ASCII integer
      -> Int

If you have many type variables, many constraints, and many arguments,
your function would end up looking like:

.. code:: haskell

    doSomething
      :: forall
           clockDomain
           resetDomain
           resetKind
           domainGatedness
       . ( NFDataX a
         , Ord b
         , NFData c
         , Functor f )
      => f a
      -> f b
      -> f c

Imports
-------

Imports should be grouped in the following order:

0. ``clash-prelude``\ †
1. standard library imports
2. related third party imports
3. local application/library specific imports

Put a blank line between each group of imports. Create subgroups per
your own judgement. The imports in each group should be sorted
alphabetically, by module name.

Always use explicit import lists or ``qualified`` imports for standard
and third party libraries. This makes the code more robust against
changes in these libraries. Exception: The Prelude.

† *When writing circuit designs. Does not apply when hacking on the
compiler itself.*

Comments
--------

Language
~~~~~~~~

Use American English. Initiali\ **z**\ ation, synchroni\ **z**\ ation,
..

Punctuation
~~~~~~~~~~~

Write proper sentences; start with a capital letter and use proper
punctuation.

Top-Level Definitions
~~~~~~~~~~~~~~~~~~~~~

Comment every top level function (particularly exported functions), and
provide a type signature; use Haddock syntax in the comments. Comment
every exported data type. Function example:

.. code:: haskell

    -- | Send a message on a socket. The socket must be in a connected
    -- state. Returns the number of bytes sent. Applications are
    -- responsible for ensuring that all data has been sent.
    send
      :: Socket
      -- ^ Connected socket
      -> ByteString
      -- ^ Data to send
      -> IO Int
      -- ^ Bytes sent

For functions the documentation should give enough information apply the
function without looking at the function's definition.

Record example:

.. code:: haskell

    -- | Bla bla bla.
    data Person = Person
      { age  :: !Int
      -- ^ Age
      , name :: !String
      -- ^ First name
      }

For fields that require longer comments format them like so:

.. code:: haskell

    data Record = Record
      { field1 :: !Text
      -- ^ This is a very very very long comment that is split over
      -- multiple lines.

      , field2 :: !Int
      -- ^ This is a second very very very long comment that is split
      -- over multiple lines.
      }

End-of-Line Comments
~~~~~~~~~~~~~~~~~~~~

Separate end-of-line comments from the code using 2 spaces. Align
comments for data type definitions. Some examples:

.. code:: haskell

    data Parser =
      Parser
        !Int         -- Current position
        !ByteString  -- Remaining input

    foo :: Int -> Int
    foo n = salt * 32 + 9
      where
        salt = 453645243  -- Magic hash salt.

Links
~~~~~

Use in-line links economically. You are encouraged to add links for API
names. It is not necessary to add links for all API names in a Haddock
comment. We therefore recommend adding a link to an API name if:

-  The user might actually want to click on it for more information (in
   your judgment), and

-  Only for the first occurrence of each API name in the comment (don't
   bother repeating a link)

Naming
------

Use camel case (e.g. ``functionName``) when naming functions and upper
camel case (e.g. ``DataType``) when naming data types.

For readability reasons, don't capitalize all letters when using an
abbreviation. For example, write ``HttpServer`` instead of
``HTTPServer``. Exception: Two letter abbreviations, e.g. ``IO``.

Use American English. That is, ``synchronizer``, not ``synchroniser``.

Modules
~~~~~~~

Use singular when naming modules e.g. use ``Data.Map`` and
``Data.ByteString.Internal`` instead of ``Data.Maps`` and
``Data.ByteString.Internals``.

Dealing with laziness
---------------------

By default, use strict data types and lazy functions.

Data types
~~~~~~~~~~

Constructor fields should be strict, unless there's an explicit reason
to make them lazy. This avoids many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.

.. code:: haskell

    -- Good
    data Point = Point
      { pointX :: !Double
      , pointY :: !Double
      }

.. code:: haskell

    -- Bad
    data Point = Point
      { pointX :: Double
      , pointY :: Double
      }

Functions
~~~~~~~~~

Have function arguments be lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

.. code:: haskell

    mysum :: [Int] -> Int
    mysum = go 0
      where
        go !acc []    = acc
        go acc (x:xs) = go (acc + x) xs

Misc
----

Point-free style
~~~~~~~~~~~~~~~~

Avoid over-using point-free style. For example, this is hard to read:

.. code:: haskell

    -- Bad:
    f = (g .) . h

Warnings
~~~~~~~~

Code should be compilable with ``-Wall -Werror``. There should be no
warnings.
