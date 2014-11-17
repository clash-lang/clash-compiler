{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|
Copyright : © Christiaan Baaij, 2014
Licence   : Creative Commons 4.0 (CC BY-NC 4.0) (http://creativecommons.org/licenses/by-nc/4.0/)
-}
module CLaSH.Tutorial (
  -- * Introduction
  -- $introduction

  -- * Installation
  -- $installation

  -- * Working with this tutorial
  -- $working

  -- * Your first circuit
  -- $mac_example

  -- *** Sequential circuit
  -- $mac2

  -- *** Creating VHDL
  -- $mac3

  -- *** Circuit testbench
  -- $mac4

  -- *** Alternative specifications
  -- $mac5

  -- * Higher-order functions
  -- $higher_order

  -- * Composition of sequential circuits
  -- $composition_sequential

  -- * Advanced: VHDL primitives
  -- $vhdlprimitives

  -- * Conclusion
  -- $conclusion

  -- * Troubleshooting
  -- $errorsandsolutions

  -- * Unsupported Haskell features
  -- $unsupported

  -- * CλaSH vs Lava
  -- $vslava
  )
where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import Data.Char
import Data.Int
import GHC.Word


{- $introduction
CλaSH (pronounced ‘clash’) is a functional hardware description language that
borrows both its syntax and semantics from the functional programming language
Haskell. The merits of using a functional language to describe hardware comes
from the fact that combinational circuits can be directly modeled as
mathematical functions and that functional languages lend themselves very well
at describing and (de-)composing mathematical functions. The CλaSH compiler
transforms these high-level descriptions to low-level synthesizable VHDL.

Although we say that CλaSH borrows the semantics of Haskell, that statement
should be taken with a grain of salt. What we mean to say is that the CλaSH
compiler views a circuit description as /structural/ description. This means,
in an academic handwavy way, that every function denotes a component and every
function application denotes an instantiation of said component. Now, this has
consequences on how we view /recursive/ functions: structurally, a recursive
function would denote an /infinitely/ deep / structured component, something
that cannot be turned into an actual circuit (See also <#unsupported Unsupported Haskell features>).
Of course there are variants of recursion that could be completely unfolded at
compile-time with a finite amount of steps and hence could be converted to a
realisable circuit. Sadly, this last feature is missing in the current version
of the compiler.

On the other hand, Haskell's by-default non-strict evaluation works very well
for the simulation of the feedback loops, which are ubiquitous in digital
circuits. That is, when we take our structural view to circuit descriptions,
value-recursion corresponds directly to a feedback loop:

@
counter = s
  where
    s = register 0 (s + 1)
@

Over time, you will get a better feeling for the consequences of taking a
/structural/ view on circuit descriptions. What is always important to
remember is that every applied functions results in an instantiated component,
and also that the compiler will /never/ infer / invent more logic than what is
specified in the circuit description.

With that out of the way, let us continue with installing CλaSH and building
our first circuit.
-}

{- $installation
The CλaSH compiler and Prelude library for circuit design only work with the
<http://haskell.org/ghc GHC> Haskell compiler version 7.8.* and up.

  (1) Install __GHC (version 7.8.* or higher)__

      * Download and install <http://www.haskell.org/ghc/download GHC for your platform>.
        Unix user can use @./configure prefix=\<LOCATION\>@ to set the installation
        location.

      * Make sure that the @bin@ directory of __GHC__ is in your @PATH@.

  (2) Install __Cabal__

      * Windows and OS X Mavericks:

          * Download the binary for <http://www.haskell.org/cabal/download.html cabal-install>
          * Put the binary in a location mentioned in your @PATH@

      * Other Unix systems:

          * Download the sources for <http://hackage.haskell.org/package/cabal-install cabal-install>
          * Unpack (@tar xf@) the archive and @cd@ to the directory
          * Run @sh bootstrap.sh@
          * Follow the instructions to add @cabal@ to your @PATH@

      * Run @cabal update@

  (2) Install __CλaSH__

      * Run @cabal install clash-ghc@

  (4) Verify that everything is working by:

      * Downloading the <https://raw.github.com/christiaanb/clash2/master/examples/FIR.hs Fir.hs> example
      * Run @clash --interactive FIR.hs@
      * Execute, in the interpreter, the @:vhdl@ command.
      * Exit the interpreter using @:q@
      * Examine the VHDL code in the @vhdl@ directory

-}

{- $working
This tutorial can be followed best whilst having the CλaSH interpreter running
at the same time. If you followed the installation instructions, you already
know how to start the CλaSH compiler in interpretive mode:

@
clash --interactive
@

For those familiar with Haskell/GHC, this is indeed just @GHCi@, with one added
command (@:vhdl@). You can load files into the interpreter using the
@:l \<FILENAME\>@ command. Now, depending on your choice in editor, the following
@edit-load-run@ cycle probably work best for you:

  * __Commandline (e.g. emacs, vim):__

      * You can run system commands using @:!@, for example @:! touch \<FILENAME\>@
      * Set the /editor/ mode to your favourite editor using: @:set editor \<EDITOR\>@
      * You can load files using @:l@ as noted above.
      * You can go into /editor/ mode using: @:e@
      * Leave the editor mode by quitting the editor (e.g. @:wq@ in @vim@)

  * __GUI (e.g. SublimeText, Notepad++):__

      * Just create new files in your editor.
      * Load the files using @:l@ as noted above.
      * Once a file has been edited and saved, type @:r@ to reload the files in
        the interpreter

You are of course free to deviate from these suggestions as you see fit :-) It
is just recommended that you have the CλaSH interpreter open during this
tutorial.
-}

{- $mac_example
The very first circuit that we will build is the \"classic\" multiply-and-accumulate
(MAC) circuit. This circuit is as simple as it sounds, it multiplies its inputs
and accumulates them. Before we describe any logic, we must first create the
file we will be working on and input some preliminaries:

* Create the file:

    @
    MAC.hs
    @

* Write on the first line the module header:

    @
    module MAC where
    @

    Module names must always start with a __C__apital letter. Also make sure that
    the file name corresponds to the module name.

* Add the import statement for the CλaSH prelude library:

    @
    import CLaSH.Prelude
    @

    This imports all the necessary functions and datatypes for circuit description.

We can now finally start describing the logic of our circuit, starting with just
the multiplication and addition:

@
ma acc (x,y) = acc + x * y
@

If you followed the instructions of running the interpreter side-by-side, you
can already test this function:

>>> ma 4 (8,9)
76
>>> ma 2 (3,4)
14

We can also examine the inferred type of @ma@ in the interpreter:

>>> :t ma
ma :: Num a => a -> (a, a) -> a

Talking about /types/ also brings us to one of the most important parts of this
tutorial: /types/ and /synchronous sequential logic/. Especially how we can
always determine, through the types of a specification, if it describes
combinational logic or (synchronous) sequential logic. We do this by examining
the type of one of the sequential primitives, the @register@ function:

@
register :: a -> Signal a -> Signal a
regiser i s = ...
@

Where we see that the second argument and the result are not just of the
/polymorphic/ @a@ type, but of the type: @'Signal' a@. All (synchronous)
sequential circuits work on values of type @'Signal' a@. Combinational
circuits always work on values of, well, not of type @'Signal' a@. A 'Signal'
is an (infinite) list of samples, where the samples correspond to the values
of the 'Signal' at discrete, consecutive, ticks of the /clock/. All (sequential)
components in the circuit are synchronized to this global /clock/. For the
rest of this tutorial, and probably at any moment where you will be working with
CλaSH, you should probably not actively think about 'Signal's as infinite lists
of samples, but just as values that are manipulated by sequential circuits. To
make this even easier, it actually not possible to manipulate the underlying
representation directly: you can only modify 'Signal' values through a set of
primitives such as the 'register' function above.

Now, let us get back to the functionality of the 'register' function: it is
a simple @latch@ that only changes state at the tick of the global /clock/, and
it has an initial value @a@ which is its output at time 0. We can further
examine the 'register' function by taking a look at the first 4 samples of the
'register' functions applied to a constant signal with the value 8:

>>> sampleN 4 (register 0 (signal 8))
[0,8,8,8]

Where we see that the initial value of the signal is the specified 0 value,
followed by 8's.
-}

{- $mac2
The 'register' function is our primary sequential building block to capture
/state/. It is used internally by one of the "CLaSH.Prelude" function that we
will use to describe our MAC circuit. Note that the following paragraphs will
only show one of many ways to specify a sequential circuit, at the section we
will show a couple more.

A principled way to describe a sequential circuit is to use one of the classic
machine models, within the CλaSH prelude library offer standard function to
support the <http://en.wikipedia.org/wiki/Mealy_machine Mealy machine>.
To improve sharing, we will combine the transition function and output function
into one. This gives rise to the following Mealy specification of the MAC
circuit:

@
macT acc (x,y) = (acc',o)
  where
    acc' = ma acc (x,y)
    o    = acc
@

Note that the @where@ clause and explicit tuple are just for demonstrative
purposes, without loss of sharing we could've also written:

@
macT acc inp = (ma acc inp,acc)
@

Going back to the original specification we note the following:

  * 'acc' is the current /state/ of the circuit.
  * '(x,y)' is its input.
  * 'acc'' is the updated, or next, /state/.
  * 'o' is the output.

When we examine the type of 'macT' we see that is still completely combinational:

>>> :t macT
macT :: Num t => t -> (t, t) -> (t, t)

The "CLaSH.Prelude" library contains a function that creates a sequential
circuit from a combinational circuit that has the same Mealy machine type /
shape of 'macT':

@
(\<^\>) :: (Bundle i, Bundle o)
      => (s -> i -> (s,o))
      -> s
      -> (Unbudled' i -> Unbundled' o)
f \<^\> initS = ...
@

The complete sequential MAC circuit can now be specified as:

@
mac = macT \<^\> 0
@

Where the LHS of '<^>' is our 'macT' function, and the RHS is the initial state,
in this case 0. We can see it is functioning correctly in our interpreter:

>>> Data.List.take 4 $ simulateB mac [(1::Int,1),(2,2),(3,3),(4,4)] :: [Int]
[0,1,5,14]

Where we simulate our sequential circuit over a list of input samples and take
the first 4 output samples. We have now completed our first sequential circuit
and have made an initial confirmation that it is working as expected.

The observant reader already saw that the '<^>' operator does not create a
function that works on 'Signal's, but on on 'SignalP's. Indeed, when we look at
the type of our 'mac' circuit:

>>> :t mac
mac :: (Bundle o, Num o) => Unbundled' (o, o) -> Unbundled o

We see that our 'mac' function work on a two-tuple of 'Signal's and not on a
'Signal' of a two-tuple. Indeed, the CλaSH prelude library defines that:

@
type instance Unbundled (a,b) = (Signal a, Signal b)
@

'Unbundled' is an <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-families.html#assoc-decl associated type family>
belonging to the 'Bundle' <http://en.wikipedia.org/wiki/Type_class type class>,
which, together with 'bundle' and 'unbundle' defines the isomorphism between a
product type of 'Signal's and a 'Signal' of a product type. That is, while
@(Signal a, Signal b)@ and @Signal (a,b)@ are not equal, they are /isomorphic/
and can be converted from on to the other using 'bundle' and 'unbundle'. Instances
of this 'Bundle' type-class are defined as /isomorphisms/ for:

  * All tuples until and including 8-tuples
  * The 'Vec'tor type

But they are defined as /identities/ for:

  * All elementary / primitive types such as: 'Bit', 'Bool', @'Signed' n@, etc.

That is:

@
instance Bundle Bool where
  type Unbundled Bool = Signal Bool
  bundle :: Unbundled Bool -> Signal Bool
  bundle = 'id'
  unpack :: Signal Bool -> Unbundled Bool
  unpack = 'id'
@

We will see later why this 'Bundle' type class is so convenient, for now, you just
have to remember that it exists. And more importantly, that you understand that
a product type of 'Signal's is not equal to a 'Signal' of a product type, but
that the functions of the 'Bunlde' type class allow easy conversion between the
two.
-}

{- $mac3
We are now almost at the point that we can create actual hardware, in the form
of a <http://en.wikipedia.org/wiki/VHDL VHDL> netlist, from our sequential
circuit specification. The first thing we have to do is create a function
called 'topEntity' and ensure that it has a __monomorphic__ type. In our case
that means that we have to give it an explicit type annotation. It might now
always be needed, you can always check the type with the @:t@ command and see
if the function is monomorphic:

@
topEntity :: (Signal (Signed 9),Signal (Signed 9)) -> Signal (Signed 9)
topEntity = mac
@

Which makes our circuit work on 9-bit signed integers. Including the above
definition, our complete @MAC.hs@ should now have the following content:

@
module MAC where

import CLaSH.Prelude

ma acc (x,y) = acc + x * y

macT acc (x,y) = (acc',o)
  where
    acc' = ma acc (x,y)
    o    = acc

mac = macT \<^\> 0

topEntity :: (Signal (Signed 9),Signal (Signed 9)) -> Signal (Signed 9)
topEntity = mac
@

The 'topEntity' function is the starting point for the CλaSH compiler to
transform your circuit description into a VHDL netlist. It must meet the
following restrictions in order for the CλaSH compiler to work:

  * It must be completely monomorphic
  * It must be completely first-order

Our 'topEntity' meets those restrictions, and so we can convert it successfully
to VHDL by executing the @:vhdl@ command in the interpreter. This will create
a directory called 'vhdl', which contains a directory called @MAC@, which
ultimately contains all the generated VHDL files. You can now load these files
(except @testbench.vhdl@) into your favourite VHDL synthesis tool, marking
@topEntity_0.vhdl@ as the file containing the top level entity.
-}

{- $mac4
There are multiple reasons as to why might you want to create a so-called
/testbench/ for the VHDL:

  * You want to compare post-synthesis / post-place&route behaviour to that of
    the behaviour of the original VHDL.
  * Need representative stimuli for your dynamic power calculations
  * Verify that the VHDL output of the CλaSH compiler has the same behaviour as
    the Haskell / CλaSH specification.

For these purposes, you can have CλaSH compiler generate a @testbench.vhdl@
file which contains a stimulus generator and an expected output verifier. The
CλaSH compiler looks for the following functions to generate these to aspects:

  1. @testInput@ for the stimulus generator.
  2. @expectedOutput@ for the output verification.

Given a 'topEntity' with the type:

@
topEntity :: SignalP a -> SignalP b
@

Where @a@ and @b@ are placeholders for monomorphic types: the 'topEntity' is
not allowed to be polymorphic. So given the above type for the 'topEntity', the
type of 'testInput' should be:

@
testInput :: Signal a
@

And the type of 'expectedOutput' should be:

@
expectedOutput :: Signal b -> Signal Bool
@

Where the 'expectedOutput' function should assert to 'True' once it has verified
all expected values. The "CLaSH.Prelude" module contains two standard functions
to serve the above purpose, but a user is free to use any CλaSH specification
to describe these two functions. For this tutorial we will be using the
functions specified in the "CLaSH.Prelude" module, which are 'stimuliGenerator'
and 'outputVerifier':

@
testInput :: Signal (Signed 9,Signed 9)
testInput = stimuliGenerator $(v [(1,1) :: (Signed 9,Signed 9),(2,2),(3,3),(4,4)])

expectedOutput :: Signal (Signed 9) -> Signal Bool
expectedOutput = outputVerifier $(v [0 :: Signed 9,1,5,14])
@

This will create a stimulus generator that creates the same inputs as we used
earlier for the simulation of the circuit, and creates an output verifier that
compares against the results we got from our earlier simulation. We can even
simulate the behaviour of the /testbench/:

>>> sampleN 7 $ expectedOutput (topEntity $ unpack testInput)
[False,False,False,False,
expected value: 14, not equal to actual value: 30
True,
expected value: 14, not equal to actual value: 46
True,
expected value: 14, not equal to actual value: 62
True]

We can see that for the first 4 samples, everything is working as expected,
after which warnings are being reported. The reason is that 'stimuliGenerator'
will keep on producing the last sample, (4,4), while the 'outputVerifier' will
keep on expecting the last sample, 14. In the VHDL testbench these errors won't
show, as the the global clock will be stopped after 4 ticks.

You should now again run @:vhdl@ in the interpreter; this time the compiler
will take a bit longer to generate all the circuits. After it is finished you
can load all the files in your favourite VHDL simulation tool that has support
for VHDL-2008. VHDL-2008 support is required because the output verifier will
use the VHDL-2008-only @to_string@ function. Once all files are loaded into
the VHDL simulator, run the simulation on the @testbench@ entity. On questasim /
modelsim: doing a @run -all@ will finish once the output verifier will assert
its output to @true@. The generated testbench, modulo the clock signal
generator(s), is completely synthesizable. This means that if you want to test
your circuit on an FPGA, you will only have to replace the clock signal
generator(s) by actual clock sources, such as an onboard PLL.

This concludes the main part of this section on \"Your first circuit\", read on
for alternative specifications for the same 'mac' circuit, or just skip to the
next section where we will describe another DSP classic: an FIR filter
structure.
-}

{- $mac5
* __'Num' instance for 'Signal'__:

    @'Signal' a@ is also also considered a 'Num'eric type as long as the value
    type /a/ is also 'Num'eric.  This means that we can also use the standard
    numeric operators, such as ('*') and ('+'), directly on signals. An
    alternative specification of the 'mac' circuit will also use the 'register'
    function directly:

    @
    macN (x,y) = acc
      where
        acc = register 0 (acc + x * y)
    @

* __'Applicative' instance for 'Signal'__:

    We can also mix the combinational 'ma' function, with the sequential
    'register' function, by lifting the 'ma' function to the sequential 'Signal'
    domain using the operators ('<$>' and '<*>') of the 'Applicative' type
    class:

    @
    macA (x,y) = acc
      where
        acc  = register 0 acc'
        acc' = ma \<$\> acc \<*\> pack (x,y)
    @

* __<http://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Lazy.html#t:State State> Monad__

    We can also implement the original 'macT' function as a
    @<http://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Lazy.html#t:State State>@
    monadic computation. First we must an extra import statement, right after
    the import of "CLaSH.Prelude":

    @
    import Control.Monad.State
    @

    We can then implement macT as follows:

    @
    macTS (x,y) = do
      acc <- get
      put (acc + x * y)
      return acc
    @

    We can use the '<^>' operator again, although we will have to change
    position of the arguments and result:

    @
    asStateM :: (Bundle o, Bundle i)
             => (i -> State s o)
             -> s
             -> (Unbundled i -> Unbundled o)
    asStateM f i = g \<^\> i
      where
        g s x = let (o,s') = runState (f x) s
                in  (s',o)
    @

    We can then create the complete 'mac' circuit as:

    @
    macS = asStateM macTS 0
    @
-}

{- $higher_order
An FIR filter is defined as: the dot-product of a set of filter coefficients and
a window over the input, where the size of the window matches the number
of coefficients.

@
dotp as bs = foldl (+) 0 (zipWith (*) as bs)

fir coeffs x_t = y_t
  where
    y_t = dotp coeffs xs
    xs  = window x_t

topEntity :: Signal (Signed 16) -> Signal (Signed 16)
topEntity = fir $(v [0::Signal (Signed 16),1,2,3])
@

Here we can see that, although the CλaSH compiler does not support recursion,
many of the regular patterns that we often encounter in circuit design are
already captured by the higher-order functions that are present for the 'Vec'tor
type.
-}

{- $composition_sequential

First we define some types:

@
module CalculatorTypes where

import CLaSH.Prelude

type Word = Signed 4
data OPC a = ADD | MUL | Imm a | Pop | Push

deriveLift ''OPC
@

Now we define the actual calculator:

@
module Calculator where

import CLaSH.Prelude
import CalculatorTypes

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) a b = f (g a b)

infixr 9 .:

alu :: Num a => OPC a -> a -> a -> Maybe a
alu ADD     = Just .: (+)
alu MUL     = Just .: (*)
alu (Imm i) = const . const (Just i)
alu _       = const . const Nothing

pu :: (Num a, Num b)
   => (OPC a -> a -> a -> Maybe a)
   -> (a, a, b)       -- Current state
   -> (a, OPC a)      -- Input
   -> ( (a, a, b)     -- New state
      , (b, Maybe a)  -- Output
      )
pu alu (op1,op2,cnt) (dmem,Pop)  = ((dmem,op1,cnt-1),(cnt,Nothing))
pu alu (op1,op2,cnt) (dmem,Push) = ((op1,op2,cnt+1) ,(cnt,Nothing))
pu alu (op1,op2,cnt) (dmem,opc)  = ((op1,op2,cnt)   ,(cnt,alu opc op1 op2))

datamem :: (KnownNat n, Integral i)
        => Vec n a       -- Current state
        -> (i, Maybe a)  -- Input
        -> (Vec n a, a)  -- (New state, Output)
datamem mem (addr,Nothing)  = (mem                 ,mem !! addr)
datamem mem (addr,Just val) = (replace mem addr val,mem !! addr)

topEntity :: Signal (OPC Word) -> Signal (Maybe Word)
topEntity i = val
  where
    (addr,val) = (pu alu \<^\> (0,0,0 :: Unsigned 3)) (mem,i)
    mem        = (datamem \<^\> initMem) (addr,val)
    initMem    = replicate d8 0
@

Here we can finally see the advantage of having the '<^>' return a function
of type: @('Unbundled' i -> 'Unbundled' o)@ (instead of:
@('Signal' i -> 'Signal' o)@):

  * We can use normal pattern matching to get parts of the result, and,
  * We can use normal tuple-constructors to build the input values for the
    circuits.
-}

{- $vhdlprimitives
There are times when you already have an existing piece of IP, or there are
times where you need the VHDL to have a specific shape so that the VHDL
synthesis tool can infer a specific component. In these specific cases you can
resort to defining your own VHDL primitives. Actually, most of the primitives
in CλaSH are specified in the same way as you will read about in this section.
There are perhaps 10 (at most) functions which are truly hard-coded into the
CλaSH compiler. You can take a look at the files in
<http://github.com/christiaanb/clash2/tree/master/primitives>
if you want to know which functions are defined as \"regular\" primitives. The
compiler looks for primitives in two locations:

* The official install location: e.g.
  @$CABAL_DIR\/share\/\<GHC_VERSION\>\/clash-ghc\-<VERSION\>\/primitives@
* The current directory (the location given by @pwd@)

Where redefined primitives in the current directory will overwrite those in
the official install location. For now, files containing primitive definitions
must end in the @.json@ file-extension.

CλaSH differentiates between two types of primitives, /expression/ primitives
and /declaration/ primitives, corresponding to whether the primitive is a VHDL
/expression/ or a VHDL /declaration/. We will first explore /expression/
primitives, using 'Signed' multiplication ('*') as an example. The
"CLaSH.Sized.Internal.Signed" module specifies multiplication as follows:

@
{\-\# NOINLINE (*#) \#-\}
(*#) :: KnownNat n => Signed n -> Signed n -> Signed n
(S a) *# (S b) = fromInteger_INLINE (a * b)
@

For which the /expression/ primitive is:

@
{ \"BlackBox\" :
  { "name"      : "CLaSH.Sized.Internal.Signed.*#"
  , "templateE" : "resize(~ARG[1] * ~ARG[2], ~LIT[0])"
  }
}
@

The @name@ of the primitive is the /fully qualified/ name of the function you
are creating the primitive for. Because we are creating an /expression/
primitive we define a @template__E__@ field. As the name suggest, it is a VHDL
/template/, meaning that the compiler must fill in the holes heralded by the
tilde (~). Here:

  * @~ARG[1]@ denotes the second argument given to the @timesS@ function, which
    corresponds to the LHS of the ('*') operator.
  * @~ARG[2]@ denotes the third argument given to the @timesS@ function, which
    corresponds to the RHS of the ('*') operator.
  * @~LIT[0]@ denotes the first argument given to the @timesS@ function, with
    the extra condition that it must be a @LIT@eral. If for some reason this
    first argument does not turn out to be a literal then the compiler will
    raise an error. This first arguments corresponds to the \"@'KnownNat' n@\"
    class constraint.

An extensive list with all of the template holes will be given the end of this
section. What we immediately notice is that class constraints are counted as
normal arguments in the primitive definition. This is because these class
constraints are actually represented by ordinary record types, with fields
corresponding to the methods of the type class. In the above case, 'KnownNat'
is actually just like a @newtype@ wrapper for 'Integer'.

The second kind of primitive that we will explore is the /declaration/ primitive.
We will use 'cblockRam' as an example, for which the Haskell/CλaSH code is:

@
{\-\# NOINLINE cblockRam \#-\}
-- | Create a blockRAM with space for @n@ elements
--
-- * \_\_NB\_\_: Read value is delayed by 1 cycle
-- * \_\_NB\_\_: Initial output value is \'undefined\'
--
-- > type ClkA = Clk \"A\" 100
-- >
-- > clkA100 :: SClock ClkA
-- > clkA100 = sclock
-- >
-- > bram40 :: CSignal ClkA (Unsigned 6) -> CSignal ClkA (Unsigned 6)
-- >        -> CSignal ClkA Bool -> CSignal ClkA Bit -> ClkA CSignal Bit
-- > bram40 = cblockRam clkA100 (replicate d40 H)
cblockRam :: (Bundle a, KnownNat n, KnownNat m)
          => SClock clk               -- ^ \'Clock\' to synchronize to
          -> Vec n a                  -- ^ Initial content of the BRAM, also
                                      -- determines the size, \@n\@, of the BRAM.
                                      --
                                      -- \_\_NB\_\_: \_\_MUST\_\_ be a constant.
          -> CSignal clk (Unsigned m) -- ^ Write address \@w\@
          -> CSignal clk (Unsigned m) -- ^ Read address \@r\@
          -> CSignal clk Bool         -- ^ Write enable
          -> CSignal clk a            -- ^ Value to write (at address \@w\@)
          -> CSignal clk a
          -- ^ Value of the \'blockRAM\' at address \@r\@ from the previous clock
          -- cycle
cblockRam clk binit wr rd en din =
    cmealy clk bram' (binit,undefined) (bundle clk (wr,rd,en,din))
  where
    bram' (ram,o) (w,r,e,d) = ((ram',o'),o)
      where
        ram' | e         = replace ram w d
             | otherwise = ram
        o'               = ram !! r
@

And for which the /definition/ primitive is:

@
{ \"BlackBox\" :
    { "name"      : "CLaSH.Prelude.BlockRam.cblockRam"
    , "templateD" :
"blockram_~SYM[0] : block
  signal ~SYM[1] : ~TYP[4] := ~LIT[4]; -- ram
  signal ~SYM[2] : ~TYP[8]; -- inp
  signal ~SYM[3] : ~TYP[8]; -- outp
begin
  ~SYM[2] <= ~ARG[8];

  process(~CLK[3])
  begin
    if rising_edge(~CLK[3]) then
      if ~ARG[7] then
        ~SYM[1](to_integer(~ARG[5])) <= ~SYM[2];
      end if;
      ~SYM[3] <= ~SYM[1](to_integer(~ARG[6]));
    end if;
  end process;

  ~RESULT <= ~SYM[3];
end block;"
    }
  }
@

Again, the @name@ of the primitive is the fully qualified name of the function
you are creating the primitive for. Because we are creating a /declaration/
primitive we define a @template__D__@ field. Instead of discussing what the
individual template holes mean in the above context, we will instead just give
a general listing of the available template holes:

* @~RESULT@: VHDL signal to which the result of a primitive must be assigned
  to. NB: Only used in a /definition/ primitive.
* @~ARG[N]@: @(N+1)@'th argument to the function.
* @~LIT[N]@: @(N+1)@'th argument to the function An extra condition that must
  hold is that this @(N+1)@'th argument is an (integer) literal.
* @~CLK[N]@: Clock signal to which the @(N+1)@'th argument is synchronized to.
* @~CLKO@: Clock signal to which the result is synchronized to.
* @~RST[N]@: Asynchronous reset signal to the clock to which the @(N+1)@'th
  argument is synchronized to.
* @~RSTO@: Asynchronous reset signal to the clock to which the result is
  synchronized to.
* @~TYP[N]@: VHDL type of the @(N+1)@'th argument.
* @~TYPO@: VHDL type of the result.
* @~TYPM[N]@: VHDL type/name/ of the @(N+1)@'th argument; used in /type/
  /qualification/.
* @~TYPM@: VHDL type/name/ of the result; used in /type qualification/.
* @~ERROR[N]@: Error value for the VHDL type of the @(N+1)@'th argument.
* @~ERRORO@: Error value for the VHDL type of the result.
* @~SYM[N]@: Randomly generated, but unique, symbol. Multiple occurrences of
  @~SYM[N]@ in the same primitive definition all refer to the same random, but
  unique, symbol.

Some final remarks to end this section: VHDL primitives are there to instruct the
CλaSH compiler to use the given VHDL template, instead of trying to do normal
synthesis. As a consequence you can use constructs inside the Haskell
definitions that are normally not synthesizable by the CλaSH compiler. However,
VHDL primitives do not give us /co-simulation/: where you would be able to
simulate VHDL and Haskell in a /single/ environment. If you still want to
simulate your design in Haskell, you will have to describe, in a cycle- and
bit-accurate way, the behaviour of that (potentially complex) IP you are trying
to include in your design.

Perhaps in the future, someone will figure out how to connect the two simulation
worlds, using e.g. VHDL's foreign function interface VHPI.
-}

{- $conclusion
For now, this is the end of this tutorial. We will be adding updates over time,
so check back from time to time. For now, we recommend that you continue with
exploring the "CLaSH.Prelude" module, and get a better understanding of the
capabilities of CλaSH in the process.
-}

{- $errorsandsolutions
A list of often encountered errors and their solutions:

* __Type error: Couldn't match expected type ‘Signal (a,b)’ with actual type__
  __‘(Signal a, Signal b)’__:

    Signals of product types and product types (to which tuples belong) of
    signals are __isomorphic__ due to synchronisity principle, but are not
    (structurally) equal. Use the 'pack' function to convert from a product type
    to the signal type. So if your code which gives the error looks like:

    @
    ... = f a b (c,d)
    @

    add the 'bundle'' function like so:

    @
    ... = f a b (bundle' (c,d))
    @

    Product types supported by 'bundle'' are:

    * All tuples until and including 8-tuples
    * The 'Vec'tor type

    NB: Use 'bundle' when you are using explicitly clocked 'CSignal's

* __Type error: Couldn't match expected type ‘(Signal a, Signal b)’ with__
  __ actual type ‘Signal (a,b)’__:

    Product types (to which tuples belong) of signals and signals of product
    types are __isomorphic__ due to synchronicity principle, but are not
    (structurally) equal. Use the 'unpack' function to convert from a signal
    type to the product type. So if your code which gives the error looks like:

    @
    (c,d) = f a b
    @

    add the 'unbundle'' function like so:

    @
    (c,d) = unbundle' (f a b)
    @

    Product types supported by 'unbundle'' are:

    * All tuples until and including 8-tuples
    * The 'Vec'tor type

    NB: Use 'unbundle' when you are using explicitly clocked 'CSignal's

* __CLaSH.Netlist(..): Not in normal form: \<REASON\>: \<EXPR\>__:

    A function could not be transformed into the expected normal form. This
    usually means one of the following:

    * The @topEntity@ has residual polymorphism.
    * The @topEntity@ has higher-order arguments, or a higher-order result.
    * You are using types which cannot be represented in hardware.

    The solution for all the above listed reasons is quite simple: remove them.
    That is, make sure that the @topEntity@ is completely monomorphic and
    first-order. Also remove any variables and constants/literals that have a
    non-representable type, see <#unsupported Unsupported Haskell features> to
    find out which types are not representable.

* __CLaSH.Normalize(94): Expr belonging to bndr: \<FUNCTION\> remains__
  __recursive after normalization__:

    * If you actually wrote a recursive function, rewrite it to a non-recursive
      one using e.g. one of the higher-order functions in "CLaSH.Sized.Vector" :-)

    * You defined a recursively defined value, but left it polymorphic:

    @
    topEntity x y = acc
      where
        acc = register 3 (acc + x * y)
    @

    The above function, works for any number-like type. This means that @acc@ is
    a recursively defined __polymorphic__ value. Adding a monomorphic type
    annotation makes the error go away:

    @
    topEntity :: Signal (Signed 8) -> Signal (Signed 8) -> Signal (Signed 8)
    topEntity x y = acc
      where
        acc = register 3 (acc + x * y)
    @

    Or, alternatively:

    @
    topEntity x y = acc
      where
        acc = register (3 :: Signed 8) (acc + x * y)
    @

* __CLaSH.Normalize.Transformations(155): InlineNonRep: \<FUNCTION\> already__
  __inlined 100 times in:\<FUNCTION\>, \<TYPE\>__:

    You left the @topEntity@ function polymorphic or higher-order: use
    @:t topEntity@ to check if the type is indeed polymorphic or higher-order.
    If it is, add a monomorphic type signature, and / or supply higher-order
    arguments.

* __Can't make testbench for: \<LONG_VERBATIM_COMPONENT_DESCRIPTION\>__:

    * Don't worry, it's actually only a warning.

    * The @topEntity@ function does __not__ have exactly 1 argument. If your
      @topEntity@ has no arguments, you're out of luck for now. If it has
      multiple arguments, consider bundling them in a tuple.

*  __\<*** Exception: \<\<loop\>\>__

    You are using value-recursion, but one of the 'Vec'tor functions that you
    are using is too /strict/ in one of the recursive arguments. For example:

    @
    -- Bubble sort for 1 iteration
    sortV xs = map fst sorted <: (snd (last sorted))
     where
       lefts  = head xs :> map snd (init sorted)
       rights = tail xs
       sorted = zipWith compareSwapL lefts rights

    -- Compare and swap
    compareSwapL a b = if a < b then (a,b)
                                else (b,a)
    @

    Will not terminate because 'zipWith' is too strict in its second argument:

    >>> sortV (4 :> 1 :> 2 :> 3 :> Nil)
    <*** Exception: <<loop>>

    In this case, adding 'lazyV' on 'zipWith's second argument:

    @
    sortVL xs = map fst sorted <: (snd (last sorted))
     where
       lefts  = head xs :> map snd (init sorted)
       rights = tail xs
       sorted = zipWith compareSwapL ('lazyV' lefts) rights
    @

    Results in a successful computation:

    >>> sortVL (4 :> 1 :> 2 :> 3 :> Nil)
    <1,2,3,4>
-}

{- $unsupported #unsupported#
Here is a list of Haskell features which the CλaSH compiler cannot synthesize
to VHDL (for now):

  [@Recursive functions@]

    Although it seems rather bad that a compiler for a
    functional language does not support recursion, this bug/feature of the
    CλaSH compiler is amortized by the builtin knowledge of all the functions
    listed in "CLaSH.Sized.Vector". And as you saw in this tutorial, the
    higher-order functions of "CLaSH.Sized.Vector" can cope with many of the
    recursive design patterns found in circuit design.

    Also note that although recursive functions are not supported, recursively
    (tying-the-knot) defined values are supported (as long as these values do
    not have a function type). An example that uses recursively defined values
    is the following function that performs one iteration of bubble sort:

    @
    sortV xs = map fst sorted <: (snd (last sorted))
     where
       lefts  = head xs :> map snd (init sorted)
       rights = tail xs
       sorted = zipWith compareSwapL lefts rights
    @

    Where we can clearly see that 'lefts' and 'sorted' are defined in terms of
    each other.

  [@Recursive datatypes@]

    The CλaSH compiler needs to be able to determine a bit-size for any value
    that will be represented in the eventual circuit. More specifically, we need
    to know the maximum number of bits needed to represent a value. While this
    is trivial for values of the elementary types, sum types, and product types,
    putting a fixed upper bound on recursive types is not (always) feasible.
    This means that the ubiquitous list type is unsupported! The only recursive
    type that is currently supported by the CλaSH compiler is the 'Vec'tor type,
    for which the compiler has hard-coded knowledge.

    For \"easy\" 'Vec'tor literals you should use Template Haskell splices and
    the 'v' /meta/-function that as we have seen earlier in this tutorial.

  [@GADT pattern matching@]

    While pattern matching for regular ADTs is supported, pattern matching for
    GADTs is __not__. The 'Vec'tor type, which is also a GADT, is __no__
    exception! You can use the extraction and indexing functions of
    "CLaSH.Sized.Vector" to get access to individual ranges / elements of a
    'Vec'tor.

  [@Floating point types@]

    There is no support for the 'Float' and 'Double' types, if you need numbers
    with a /fractional/ part you can use the 'Fixed' point type.

  [@Other primitive types@]

    Most primitive types are not supported, with the exception of 'Int',
    @<http://hackage.haskell.org/package/ghc-prim/docs/GHC-Prim.html#t:Int-35- Int#>@,
    and 'Integer'. This means that types such as: 'Word', 'Word8', 'Int8', 'Char',
    @<http://hackage.haskell.org/package/array/docs/Data-Array.html#t:Array Array>@,
    etc. cannot to translated to hardware.

    The translations of 'Int',
    @<http://hackage.haskell.org/package/ghc-prim/docs/GHC-Prim.html#t:Int-35- Int#>@,
    and 'Integer' are also incorrect: they are translated to the VHDL @integer@
    type, which can only represent 32-bit integer values. Use these types with
    due diligence.

  [@Side-effects: 'IO', <http://hackage.haskell.org/package/base/docs/Control-Monad-ST.html#t:ST ST>, etc.@]

    There is no support for side-effecting computations such as those in the
    'IO' or @<http://hackage.haskell.org/package/base/docs/Control-Monad-ST.html#t:ST ST>@
    monad. There is also no support for Haskell's
    <http://www.haskell.org/haskellwiki/Foreign_Function_Interface FFI>.
-}

{- $vslava
In Haskell land the most well-known way of describing digital circuits is the
Lava family of languages:

* <http://hackage.haskell.org/package/chalmers-lava2000 Chalmers Lava>
* <http://hackage.haskell.org/package/xilinx-lava Xilinx Lava>
* <http://hackage.haskell.org/package/york-lava York Lava>
* <http://hackage.haskell.org/package/kansas-lava Kansas Lava>

The big difference between CλaSH and Lava is that CλaSH uses a \"standard\"
compiler (static analysis) approach towards synthesis, where Lava is an
embedded domain specific language. One downside of static analysis vs. the
embedded language approach is already clearly visible: synthesis of recursive
descriptions does not come for \"free\". This will be implemented in CλaSH in
due time, but that doesn't help the circuit designer right now. As already
mentioned earlier, the lack of support for recursive functions is amortized by
the built-in support for the higher-order in "CLaSH.Sized.Vector".

The big upside of CλaSH and its static analysis approach is that CλaSH can
do synthesis of \"normal\" functions: there is no forced encasing datatype (often
called /Signal/ in Lava) on all the arguments and results of a synthesizable
function. This enables the following features not available to Lava:

* Automatic synthesis for user-defined ADTs
* Synthesis of all choice constructs (pattern matching, guards, etc.)
* 'Applicative' instance for the 'Signal' type
* Working with \"normal\" functions permits the use of e.g. the
  <http://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Lazy.html#t:State State>
  monad to describe the functionality of a circuit.

Although there are Lava alternatives to some of the above features (e.g.
first-class patterns to replace pattern matching) they are not as \"beautiful\"
and / or easy to use as the standard Haskell features.
-}
