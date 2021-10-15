{-|
Copyright : © 2014-2016, Christiaan Baaij,
              2017-2019, Myrtle Software Ltd
              2017     , QBayLogic, Google Inc.,
              2021     , QBayLogic B.V.

Licence   : Creative Commons 4.0 (CC BY 4.0) (https://creativecommons.org/licenses/by/4.0/)
Maintainer:  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Clash.Tutorial (
  -- * Introduction
  -- $introduction

  -- * Install Clash
  -- $installation

  -- * Working with this tutorial
  -- $working

  -- * Your first circuit
  -- $mac_example

  -- *** Sequential circuit
  -- $mac2

  -- *** Generating VHDL
  -- $mac3

  -- *** Circuit testbench
  -- $mac4

  -- *** Generating Verilog and SystemVerilog
  -- $mac5

  -- *** Alternative specifications
  -- $mac6

  -- * Higher-order functions
  -- $higher_order

  -- * Composition of sequential circuits
  -- $composition_sequential

  -- * Synthesize annotations: controlling the VHDL\/(System)Verilog generation.
  -- $annotations

  -- * Multiple clock domains
  -- $multiclock

  -- * Advanced: Primitives
  -- $primitives

  -- *** Verilog primitives
  -- $vprimitives

  -- *** SystemVerilog primitives
  -- $svprimitives

  -- * Conclusion
  -- $conclusion

  -- * Troubleshooting
  -- $errorsandsolutions

  -- * Limitations of Clash
  -- $limitations

  -- * Clash vs Lava
  -- $vslava

  -- * Migration guide from Clash 0.99
  -- $migration
  )
where

import Clash.Prelude
import Clash.Explicit.Testbench
import Clash.XException (NFDataX)
import Control.Monad.ST
import Data.Array
import Data.Char
import Data.Int
import GHC.Prim
import GHC.TypeLits
import GHC.Word
import GHC.Stack
import Data.Default.Class

{- $setup
>>> :set -XTemplateHaskell -XDataKinds -XConstraintKinds -XTypeApplications
>>> :m -Clash.Explicit.Prelude
>>> import Clash.Prelude
>>> import Clash.Explicit.Testbench
>>> :{
let ma :: Num a => a -> (a, a) -> a
    ma acc (x,y) = acc + x * y
:}

>>> :{
let macT :: Num a => a -> (a,a) -> (a,a)
    macT acc (x,y) = (acc',o)
       where
         acc' = ma acc (x,y)
         o    = acc
:}

>>> :set -XFlexibleContexts
>>> :set -fplugin GHC.TypeLits.Normalise
>>> let compareSwapL a b = if a < b then (a,b) else (b,a)
>>> :{
let sortV xs = map fst sorted :< (snd (last sorted))
      where
        lefts  = head xs :> map snd (init sorted)
        rights = tail xs
        sorted = zipWith compareSwapL lefts rights
:}

>>> :{
let sortVL xs = map fst sorted :< (snd (last sorted))
      where
        lefts  = head xs :> map snd (init sorted)
        rights = tail xs
        sorted = zipWith compareSwapL (lazyV lefts) rights
:}

>>> let mac = mealy macT 0
>>> :{
let topEntity
      :: Clock System
      -> Reset System
      -> Enable System
      -> Signal System (Signed 9, Signed 9)
      -> Signal System (Signed 9)
    topEntity = exposeClockResetEnable mac
:}

>>> :{
let testBench :: Signal System Bool
    testBench = done
      where
        testInput      = stimuliGenerator clk rst $(listToVecTH [(1,1) :: (Signed 9,Signed 9),(2,2),(3,3),(4,4)])
        expectedOutput = outputVerifier' clk rst $(listToVecTH [0 :: Signed 9,1,5,14,14,14,14])
        done           = expectedOutput (topEntity clk rst enableGen testInput)
        clk            = tbSystemClockGen (not <$> done)
        rst            = systemResetGen
:}

>>> :{
let fibR :: Unsigned 64 -> Unsigned 64
    fibR 0 = 0
    fibR 1 = 1
    fibR n = fibR (n-1) + fibR (n-2)
:}

>>> :{
let fibS :: SystemClockResetEnable => Signal System (Unsigned 64)
    fibS = r
      where r = register 0 r + register 0 (register 1 r)
:}

-}

{- $introduction
Clash is a functional hardware description language that borrows both its syntax
and semantics from the functional programming language Haskell. It provides a
familiar structural design approach to both combination and synchronous
sequential circuits. The Clash compiler transforms these high-level descriptions
to low-level synthesizable VHDL, Verilog, or SystemVerilog.

Features of Clash:

  * Strongly typed, but with a very high degree of type inference, enabling
    both safe and fast prototyping using concise descriptions.
  * Interactive REPL: load your designs in an interpreter and easily test all
    your component without needing to setup a test bench.
  * Compile your designs for fast simulation.
  * Higher-order functions, in combination with type inference, result in
    designs that are fully parametric by default.
  * Synchronous sequential circuit design based on streams of values, called
    @Signal@s, lead to natural descriptions of feedback loops.
  * Multiple clock domains, with type safe clock domain crossing.
  * Template language for introducing new VHDL/(System)Verilog primitives.

Although we say that Clash borrows the semantics of Haskell, that statement
should be taken with a grain of salt. What we mean to say is that the Clash
compiler views a circuit description as /structural/ description. This means,
in an academic handwavy way, that every function denotes a component and every
function application denotes an instantiation of said component. Now, this has
consequences on how we view /recursively/ defined functions: structurally, a
recursively defined function would denote an /infinitely/ deep / structured
component, something that cannot be turned into an actual circuit
(See also <#limitations Limitations of Clash>).

On the other hand, Haskell's by-default non-strict evaluation works very well
for the simulation of the feedback loops, which are ubiquitous in digital
circuits. That is, when we take our structural view to circuit descriptions,
value-recursion corresponds directly to a feedback loop:

@
counter = s
  where
    s = 'register' 0 (s + 1)
@

The above definition, which uses value-recursion, /can/ be synthesized to a
circuit by the Clash compiler.

Over time, you will get a better feeling for the consequences of taking a
/structural/ view on circuit descriptions. What is always important to
remember is that every applied functions results in an instantiated component,
and also that the compiler will /never/ infer / invent more logic than what is
specified in the circuit description.

With that out of the way, let us continue with installing Clash and building
our first circuit.
-}

{- $installation
For installation instructions, see <https://clash-lang.org/install/ clash-lang.org/install/>.
-}

{- $working
This tutorial can be followed best whilst having the Clash interpreter running
at the same time. If you followed the installation instructions based on
<https://docs.haskellstack.org/en/stable/README/#how-to-install Stack>, you can
start the Clash compiler in interpretive mode by:

@
stack exec --package clash-ghc -- clashi
@

If you followed the installation instruction based on
<https://snapcraft.io/clash snap> (Linux only), you can start the Clash compiler
in interpretive mode by:

@
clash.clashi
@

For those familiar with Haskell/GHC, this is indeed just @GHCi@, with three
added commands (@:vhdl@, @:verilog@, and @:systemverilog@). You can load files
into the interpreter using the @:l \<FILENAME\>@ command. Now, depending on your
choice in editor, the following @edit-load-run@ cycle probably work best for you:

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
is just recommended that you have the Clash interpreter open during this
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

* Add the import statement for the Clash prelude library:

    @
    import Clash.Prelude
    @

    This imports all the necessary functions and datatypes for circuit description.

We can now finally start describing the logic of our circuit, starting with just
the multiplication and addition:

@
ma acc (x, y) = acc + x * y
@

The circuit we just wrote is a combinational circuit: no registers are inserted
(you describe explicitly where Clash will insert registers, as we'll later see). We usually
refer to circuits as /functions/, similar to programming languages such as C,
Python, or Haskell. In this case, the function we just defined is called @ma@.
Its first argument is @acc@, its second is @(x, y)@ - a composite type called a
tuple. This component is "unpacked", and its first element is called @x@, its
second @y@. Everything to the right of the equals symbol is @ma@'s
result.
If you followed the instructions of running the interpreter side-by-side, you
can already test this function:

>>> ma 4 (8, 9)
76
>>> ma 2 (3, 4)
14

We can also examine the inferred type of @ma@ in the interpreter:

>>> :t ma
ma :: Num a => a -> (a, a) -> a

You should read this as follows:

 * __@ma ::@__, @ma@ is of type..

 * __@Num a@__, there is some type called @a@ that is a @'Num'@. Examples of
   instances of @'Num'@ are @'Int'@, @'Signed' 16@, @'Index' 32@, and @'Float'@.

 * __@a@__, @ma@'s first argument is of type @a@

 * __@(a, a)@__, @ma@'s second argument is of type @(a, a)@

 * __@a@__, @ma@'s result is of type @a@

Note that @ma@ therefore works on multiple types! The only condition we
imposed is that @a@ should be a @'Num'@ber type. In Clash this means it should
support the operations @'Prelude.+'@, @'Prelude.-'@, @'Prelude.*'@, and some
others. Indeed, this is why Clash adds the constraint in the first place: the
definition of @ma@ uses @+@ and @*@. Whenever a function works over multiple
types, we call it /polymorphic/ ("poly" meaning "many", "morphic" meaning
"forms"). While powerful, its not clear how Clash should synthesize this as
numbers come in a great variety in (bit)sizes. We will later see how to use this
function in a /monomorphic/ manner.

Talking about /types/ also brings us to one of the most important parts of this
tutorial: /types/ and /synchronous sequential logic/. Especially how we can
always determine, through the types of a specification, if it describes
combinational logic or (synchronous) sequential logic. We do this by examining
the definition of one of the sequential primitives, the @'register'@ function:

@
register
     ( 'HiddenClockResetEnable' dom
     , 'Clash.XException.NFDataX' a )
  => a
  -> 'Signal' dom a
  -> 'Signal' dom a
register i s = ...
@

Where we see that the second argument and the result are not just of the
/polymorphic/ @a@ type, but of the type: @'Signal' dom a@. All (synchronous)
sequential circuits work on values of type @'Signal' dom a@. Combinational
circuits always work on values of, well, not of type @'Signal' dom a@. A 'Signal'
is an (infinite) list of samples, where the samples correspond to the values
of the 'Signal' at discrete, consecutive, ticks of the /clock/. All (sequential)
components in the circuit are synchronized to this global /clock/. For the
rest of this tutorial, and probably at any moment where you will be working with
Clash, you should probably not actively think about 'Signal's as infinite lists
of samples, but just as values that are manipulated by sequential circuits. To
make this even easier, it actually not possible to manipulate the underlying
representation directly: you can only modify 'Signal' values through a set of
primitives such as the 'register' function above.

Now, let us get back to the functionality of the 'register' function: it is
a simple <https://en.wikipedia.org/wiki/Flip-flop_(electronics) latch> that
only changes state at the tick of the global /clock/, and
it has an initial value @a@ which is its output at time 0. We can further
examine the 'register' function by taking a look at the first 4 samples of the
'register' functions applied to a constant signal with the value 8:

>>> sampleN @System 4 (register 0 (pure (8 :: Signed 8)))
[0,0,8,8]

Where we see that the initial value of the signal is the specified 0 value,
followed by 8's. You might be surprised to see /two/ zeros instead of just a
single zero. What happens is that in Clash you get to see the output of the
circuit /before/ the clock becomes actives. In other words, in Clash you get to
describe the powerup values of registers too. Whether this is a defined or
unknown value depends on your hardware target, and can be configured by using a
different synthesis @'Domain'@. The default synthesis domain, @'System', assumes
that registers do have a powerup value - as is true for most FPGA platforms in
most contexts.
-}

{- $mac2
The 'register' function is our primary sequential building block to capture
/state/. It is used internally by one of the "Clash.Prelude" function that we
will use to describe our MAC circuit. Note that the following paragraphs will
only show one of many ways to specify a sequential circuit, in the section
<#mac6 Alternative specifications> we will show a couple more.

A principled way to describe a sequential circuit is to use one of the classic
machine models, within the Clash prelude library offer standard function to
support the <http://en.wikipedia.org/wiki/Mealy_machine Mealy machine>.
To improve sharing, we will combine the transition function and output function
into one. This gives rise to the following Mealy specification of the MAC
circuit:

@
macT acc (x, y) = (acc', o)
  where
    acc' = ma acc (x, y)
    o    = acc
@

Note that the @where@ clause and explicit tuple are just for demonstrative
purposes, without loss of sharing we could've also written:

@
macT acc inp = (ma acc inp, acc)
@

Going back to the original specification we note the following:

  * @acc@ is the current /state/ of the circuit.
  * @(x, y)@ is its input.
  * @acc'@ is the updated, or next, /state/.
  * @o@ is the output.

When we examine the type of @macT@ we see that is still completely combinational:

>>> :t macT
macT :: Num a => a -> (a, a) -> (a, a)

The "Clash.Prelude" library contains a function that creates a sequential
circuit from a combinational circuit that has the same Mealy machine type /
shape of @macT@:

@
mealy
  :: ('HiddenClockResetEnable' dom, 'Clash.XException.NFDataX' s)
  => (s -> i -> (s,o))
  -> s
  -> ('Signal' dom i -> 'Signal' dom o)
mealy f initS = ...
@

The complete sequential MAC circuit can now be specified as:

@
mac inp = 'mealy' macT 0 inp
@

Where the first argument of @'mealy'@ is our @macT@ function, and the second
argument is the initial state, in this case 0. We can see it is functioning
correctly in our interpreter:

>>> import qualified Data.List as L
>>> L.take 4 $ simulate @System mac [(1,1),(2,2),(3,3),(4,4)]
[0,1,5,14]

Where we simulate our sequential circuit over a list of input samples and take
the first 4 output samples. We have now completed our first sequential circuit
and have made an initial confirmation that it is working as expected.
-}

{- $mac3
We are now almost at the point that we can create actual hardware, in the form
of a <http://en.wikipedia.org/wiki/VHDL VHDL> netlist, from our sequential
circuit specification. The first thing we have to do is create a function
called @topEntity@ and ensure that it has a __monomorphic__ type. In our case
that means that we have to give it an explicit type annotation. It might not
always be needed, you can always check the type with the @:t@ command and see
if the function is monomorphic:

@
topEntity
  :: 'Clock' 'System'
  -> 'Reset' 'System'
  -> 'Enable' 'System'
  -> 'Signal' 'System' ('Signed' 9, 'Signed' 9)
  -> 'Signal' 'System' ('Signed' 9)
topEntity = 'exposeClockResetEnable' mac
@

Which makes our circuit work on 9-bit signed integers. Including the above
definition, our complete @MAC.hs@ should now have the following content:

@
module MAC where

import "Clash.Prelude"

ma acc (x,y) = acc + x * y

macT acc (x,y) = (acc',o)
  where
    acc' = ma acc (x,y)
    o    = acc

mac inp = 'mealy' macT 0 inp

topEntity
  :: 'Clock' 'System'
  -> 'Reset' 'System'
  -> 'Enable' 'System'
  -> 'Signal' 'System' ('Signed' 9, 'Signed' 9)
  -> 'Signal' 'System' ('Signed' 9)
topEntity = 'exposeClockResetEnable' mac
@

The @topEntity@ function is the starting point for the Clash compiler to
transform your circuit description into a VHDL netlist. It must meet the
following restrictions in order for the Clash compiler to work:

  * It must be completely monomorphic
  * It must be completely first-order
  * Although not strictly necessary, it is recommended to /expose/ 'Hidden'
    clock and reset arguments, as it makes user-controlled
    <Clash-Tutorial.html#annotations name assignment> in the generated HDL
    easier to do.

Our @topEntity@ meets those restrictions, and so we can convert it successfully
to VHDL by executing the @:vhdl@ command in the interpreter. This will create
a directory called @vhdl@, which contains a directory called @MAC@, which
ultimately contains all the generated VHDL files. You can now load these files
into your favourite VHDL synthesis tool, marking @topentity.vhdl@ as the file
containing the top level entity.
-}

{- $mac4
There are multiple reasons as to why you might want to create a so-called
/test bench/ for the generated HDL:

  * You want to compare post-synthesis / post-place&route behavior to that of
    the behavior of the original generated HDL.
  * Need representative stimuli for your dynamic power calculations.
  * Verify that the HDL output of the Clash compiler has the same behavior as
    the Haskell / Clash specification.

For these purposes, you can have the Clash compiler generate a /test bench/. In
order for the Clash compiler to do this you need to do one of the following:

  * Create a function called /testBench/ in the root module.
  * Annotate your /topEntity/ function (or function with a
    <Clash-Tutorial.html#g:12 Synthesize> annotation)
    with a 'TestBench' annotation.

For example, you can test the earlier defined /topEntity/ by:

@
import "Clash.Explicit.Testbench"

topEntity
  :: 'Clock' System
  -> 'Reset' System
  -> 'Enable' System
  -> 'Signal' System ('Signed' 9, 'Signed' 9)
  -> 'Signal' System ('Signed' 9)
topEntity = 'exposeClockResetEnable' mac

testBench :: 'Signal' System Bool
testBench = done
  where
    testInput    = 'stimuliGenerator' clk rst $('listToVecTH' [(1,1) :: ('Signed' 9,'Signed' 9),(2,2),(3,3),(4,4)])
    expectOutput = 'outputVerifier'' clk rst $('listToVecTH' [0 :: 'Signed' 9,1,5,14,14,14,14])
    done         = expectOutput (topEntity clk rst en testInput)
    en           = 'enableGen'
    clk          = 'tbSystemClockGen' (not '<$>' done)
    rst          = 'systemResetGen'
@

This will create a stimulus generator that creates the same inputs as we used
earlier for the simulation of the circuit, and creates an output verifier that
compares against the results we got from our earlier simulation. We can even
simulate the behavior of the /testBench/:

>>> sampleN 8 testBench
[False,False,False,False,False
cycle(<Clock: System>): 5, outputVerifier
expected value: 14, not equal to actual value: 30
,False
cycle(<Clock: System>): 6, outputVerifier
expected value: 14, not equal to actual value: 46
,False
cycle(<Clock: System>): 7, outputVerifier
expected value: 14, not equal to actual value: 62
,False]

We can see that for the first 4 samples, everything is working as expected,
after which warnings are being reported. The reason is that 'stimuliGenerator'
will keep on producing the last sample, (4,4), while the 'outputVerifier'' will
keep on expecting the last sample, 14. In the VHDL testbench these errors won't
show, as the global clock will be stopped after 4 ticks.

You should now again run @:vhdl@ in the interpreter; this time the compiler
will take a bit longer to generate all the circuits. Inside the @.\/vhdl\/MAC@
directory you will now also find a /testbench/ subdirectory containing all the
@vhdl@ files for the /test bench/.


After compilation is finished you  load all the files in your favourite VHDL
simulation tool. Once all files are loaded into the VHDL simulator, run the
simulation on the @testbench@ entity.
On questasim / modelsim: doing a @run -all@ will finish once the output verifier
will assert its output to @true@. The generated testbench, modulo the clock
signal generator(s), is completely synthesizable. This means that if you want to
test your circuit on an FPGA, you will only have to replace the clock signal
generator(s) by actual clock sources, such as an onboard PLL.
-}

{- $mac5
Aside from being able to generate VHDL, the Clash compiler can also generate Verilog
and SystemVerilog. You can repeat the previous two parts of the tutorial, but
instead of executing the @:vhdl@ command, you execute the @:verilog@ or
@:sytemverilog@ command in the interpreter. This will create a directory called
@verilog@, respectively @systemverilog@, which contains a directory called @MAC@,
which ultimately contains all the generated Verilog and SystemVerilog files.
Verilog files end in the file extension @v@, while SystemVerilog files end in
the file extension @sv@.

This concludes the main part of this section on \"Your first circuit\", read on
for alternative specifications for the same @mac@ circuit, or just skip to the
next section where we will describe another DSP classic: an FIR filter
structure.
-}

{- $mac6 #mac6#
* __'Num' instance for 'Signal'__:

    @'Signal' a@ is also also considered a 'Num'eric type as long as the value
    type /a/ is also 'Num'eric.  This means that we can also use the standard
    numeric operators, such as ('GHC.Num.*') and ('GHC.Num.+'), directly on signals. An
    alternative specification of the @mac@ circuit will also use the 'register'
    function directly:

    @
    macN (x,y) = acc
      where
        acc = 'register' 0 (acc + x * y)
    @

* __'Applicative' instance for 'Signal'__:

    We can also mix the combinational @ma@ function, with the sequential
    'register' function, by lifting the @ma@ function to the sequential 'Signal'
    domain using the operators ('<$>' and '<*>') of the 'Applicative' type
    class:

    @
    macA (x,y) = acc
      where
        acc  = 'register' 0 acc'
        acc' = ma '<$>' acc '<*>' 'bundle' (x,y)
    @

* __'Control.Monad.State.Lazy.State' Monad__

    We can also implement the original @macT@ function as a
    @'Control.Monad.State.Lazy.State'@
    monadic computation. First we must add an extra import statement, right
    after the import of "Clash.Prelude":

    @
    import Control.Monad.State
    @

    We can then implement macT as follows:

    @
    macTS (x,y) = do
      acc <- 'Control.Monad.State.Lazy.get'
      'Control.Monad.State.Lazy.put' (acc + x * y)
      return acc
    @

    We can use the 'mealy' function again, although we will have to change
    position of the arguments and result:

    @
    asStateM
      :: ( 'HiddenClockResetEnable' dom
         , 'NFDataX' s )
      => (i -> 'Control.Monad.State.Lazy.State' s o)
      -> s
      -> ('Signal' dom i -> 'Signal' dom o)
    asStateM f i = 'mealy' g i
      where
        g s x = let (o,s') = 'Control.Monad.State.Lazy.runState' (f x) s
                in  (s',o)
    @

    We can then create the complete @mac@ circuit as:

    @
    macS = asStateM macTS 0
    @
-}

{- $higher_order
An FIR filter is defined as: the dot-product of a set of filter coefficients and
a window over the input, where the size of the window matches the number
of coefficients.

@
dotp as bs = 'sum' ('zipWith' (*) as bs)

fir coeffs x_t = y_t
  where
    y_t = dotp coeffs xs
    xs  = 'window' x_t

topEntity
  :: 'Clock' 'System'
  -> 'Reset' 'System'
  -> 'Enable' 'System'
  -> 'Signal' 'System' ('Signed' 16)
  -> 'Signal' 'System' ('Signed' 16)
topEntity = exposeClockResetEnable (fir (0 ':>' 1 ':>' 2 ':>' 3 ':>' 'Nil'))
@

Here we can see that, although the Clash compiler handles recursive function
definitions poorly, many of the regular patterns that we often encounter in
circuit design are already captured by the higher-order functions that are
present for the 'Vec'tor type.
-}

{- $composition_sequential
Given a function @f@ of type:

@
__f__ :: Int -> (Bool, Int) -> (Int, (Int, Bool))
@

When we want to make compositions of @f@ in @g@ using 'mealy', we have to
write:

@
g a b c = (b1,b2,i2)
  where
    (i1,b1) = 'unbundle' ('mealy' f 0 ('bundle' (a,b)))
    (i2,b2) = 'unbundle' ('mealy' f 3 ('bundle' (c,i1)))
@

Why do we need these 'bundle', and 'unbundle' functions you might ask? When we
look at the type of 'mealy':

@
__mealy__
  :: HiddenClockResetEnable dom
  => (s -> i -> (s,o))
  -> s
  -> ('Signal' dom i -> 'Signal' dom o)
@

we see that the resulting function has an input of type @'Signal' dom i@, and
an output of @'Signal' dom o@. However, the type of @(a,b)@ in the definition
of @g@ is: @('Signal' dom Bool, 'Signal' dom Int)@. And the type of @(i1,b1)@
is of type @('Signal' dom Int, 'Signal' dom Bool)@.

Syntactically, @'Signal' dom (Bool,Int)@ and @('Signal' dom Bool,
'Signal' dom Int)@ are /unequal/.
So we need to make a conversion between the two, that is what 'bundle' and
'unbundle' are for. In the above case 'bundle' gets the type:

@
__bundle__ :: ('Signal' dom Bool, 'Signal' dom Int) -> 'Signal' dom (Bool,Int)
@

and 'unbundle':

@
__unbundle__ :: 'Signal' dom (Int,Bool) -> ('Signal' dom Int, 'Signal' dom Bool)
@

The /true/ types of these two functions are, however:

@
__bundle__   :: 'Bundle' a => 'Unbundled' dom a -> 'Signal' dom a
__unbundle__ :: 'Bundle' a => 'Signal' dom a -> 'Unbundled' dom a
@

'Unbundled' is an <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#associated-data-and-type-families associated type family>
belonging to the 'Bundle' <http://en.wikipedia.org/wiki/Type_class type class>,
which, together with 'bundle' and 'unbundle' defines the isomorphism between a
product type of 'Signal's and a 'Signal' of a product type. That is, while
@('Signal' dom a, 'Signal' dom b)@ and @'Signal' dom (a,b)@ are not equal,
they are /isomorphic/ and can be converted from, or to, the other using
'bundle' and 'unbundle'.

Instances of this 'Bundle' type-class are defined as /isomorphisms/ for:

  * All tuples up to and including 62-tuples (GHC limit)
  * The 'Vec'tor type

But they are defined as /identities/ for:

  * All elementary / primitive types such as: 'Bit', 'Bool', @'Signed' n@, etc.

That is:

@
instance 'Bundle' (a,b) where
  type 'Unbundled' dom (a,b) = ('Signal' dom a, 'Signal' dom b)
  bundle   (a,b) = (,) '<$>' a '<*>' b
  unbundle tup   = (fst '<$>' tup, snd '<*>' tup)
@

but,

@
instance 'Bundle' Bool where
  type 'Unbundled' dom Bool = 'Signal' dom Bool
  bundle   s = s
  unbundle s = s
@

What you need take away from the above is that a product type (e.g. a tuple) of
'Signal's is not syntactically equal to a 'Signal' of a product type, but that
the functions of the 'Bundle' type class allow easy conversion between the two.

As a final note on this section we also want to mention the 'mealyB' function,
which does the bundling and unbundling for us:

@
mealyB
  :: ('Bundle' i, 'Bundle' o)
  => (s -> i -> (s,o))
  -> s
  -> 'Unbundled' dom i
  -> 'Unbundled' dom o
@

Using 'mealyB' we can define @g@ as:

@
g a b c = (b1,b2,i2)
  where
    (i1,b1) = 'mealyB' f 0 (a,b)
    (i2,b2) = 'mealyB' f 3 (c,i1)
@

The general rule of thumb is: always use 'mealy', unless you do pattern matching
or construction of product types, then use 'mealyB'.
-}

{- $annotations #annotations#
'Synthesize' annotations allow us to control hierarchy and naming aspects of the
Clash compiler, specifically, they allow us to:

    * Assign names to entities (VHDL) \/ modules ((System)Verilog), and their
      ports.
    * Put generated HDL files of a logical (sub)entity in their own directory.
    * Use cached versions of generated HDL, i.e., prevent recompilation of
      (sub)entities that have not changed since the last run. Caching is based
      on a @.manifest@ which is generated alongside the HDL; deleting this file
      means deleting the cache; changing this file will result in /undefined/
      behavior.

Functions with a 'Synthesize' annotation must adhere to the following
restrictions:

    * Although functions with a 'Synthesize' annotation can of course depend
      on functions with another 'Synthesize' annotation, they must not be
      mutually recursive.
    * Functions with a 'Synthesize' annotation must be completely /monomorphic/
      and /first-order/, and cannot have any /non-representable/ arguments or
      result.

Also take the following into account when using 'Synthesize' annotations.

    * The Clash compiler is based on the GHC Haskell compiler, and the GHC
      machinery does not understand 'Synthesize' annotations and it might
      subsequently decide to inline those functions. You should therefor also
      add a @{\-\# NOINLINE f \#-\}@ pragma to the functions which you give
      a 'Synthesize' functions.
    * Functions with a 'Synthesize' annotation will not be specialized
      on constants.

Finally, the root module, the module which you pass as an argument to the
Clash compiler must either have:

    * A function with a 'Synthesize' annotation.
    * A function called /topEntity/.

You apply 'Synthesize' annotations to functions using an @ANN@ pragma:

@
{\-\# ANN topEntity (Synthesize {t_name = ..., ...  }) \#-\}
topEntity x = ...
@

For example, given the following specification:

@
module Blinker where

import "Clash.Signal"
import "Clash.Prelude"
import "Clash.Intel.ClockGen"

'createDomain' vSystem{vName=\"DomInput\", vPeriod=20000}
'createDomain' vSystem{vName=\"Dom50\", vPeriod=50000}

topEntity
  :: Clock DomInput
  -> Signal DomInput Bool
  -> Signal Dom50 Bit
  -> Signal Dom50 (BitVector 8)
topEntity clk rst =
    'exposeClockResetEnable' ('mealy' blinkerT (1,False,0) . Clash.Prelude.isRising 1) pllOut rstSync 'enableGen'
  where
    (pllOut,pllStable) = 'Clash.Intel.ClockGen.altpll' \@Dom50 (SSymbol \@\"altpll50\") clk ('Clash.Signal.unsafeFromLowPolarity' rst)
    rstSync            = 'Clash.Signal.resetSynchronizer' pllOut ('Clash.Signal.unsafeFromLowPolarity' pllStable)

blinkerT (leds,mode,cntr) key1R = ((leds',mode',cntr'),leds)
  where
    -- clock frequency = 50e6  (50 MHz)
    -- led update rate = 333e-3 (every 333ms)
    cnt_max = 16650000 :: ('Index' 16650001) -- 50e6 * 333e-3

    cntr' | cntr == cnt_max = 0
          | otherwise       = cntr + 1

    mode' | key1R     = not mode
          | otherwise = mode

    leds' | cntr == 0 = if mode then complement leds
                                else rotateL leds 1
          | otherwise = leds
@

The Clash compiler will normally generate the following @topentity.vhdl@ file:

@
-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.blinker_types.all;

entity topentity is
  port(-- clock
       clk  : in blinker_types.clk_dominput;
       rst  : in boolean;
       x    : in std_logic;
       leds : out std_logic_vector(7 downto 0));
end;

architecture structural of topentity is
 ...
end;
@

However, if we add the following 'Synthesize' annotation in the file:

@
{\-\# ANN topEntity
  ('Synthesize'
    { t_name   = "blinker"
    , t_inputs = [PortName \"CLOCK_50\", PortName \"KEY0\", PortName \"KEY1\"]
    , t_output = PortName \"LED\"
    }) \#-\}
@

The Clash compiler will generate the following @blinker.vhdl@ file instead:

@
-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.blinker_types.all;

entity blinker is
  port(-- clock
       CLOCK_50 : in blinker_types.clk_dominput;
       KEY0     : in boolean;
       KEY1     : in std_logic;
       LED      : out std_logic_vector(7 downto 0));
end;

architecture structural of blinker is
 ...
end;
@

Where we now have:

* A top-level component that is called @blinker@.
* Inputs and outputs that have a /user/-chosen name: @CLOCK_50@, @KEY0@, @KEY1@, @LED@, etc.

See the documentation of 'Synthesize' for the meaning of all its fields.
-}

{- $primitives #primitives#
There are times when you already have an existing piece of IP, or there are
times where you need the VHDL to have a specific shape so that the VHDL
synthesis tool can infer a specific component. In these specific cases you can
resort to defining your own VHDL primitives. Actually, most of the primitives
in Clash are specified in the same way as you will read about in this section.
There are perhaps 10 (at most) functions which are truly hard-coded into the
Clash compiler. You can take a look at the files in
<https://github.com/clash-lang/clash-compiler/tree/master/clash-lib/prims/vhdl>
(or <https://github.com/clash-lang/clash-compiler/tree/master/clash-lib/prims/verilog>
for the Verilog primitives or <https://github.com/clash-lang/clash-compiler/tree/master/clash-lib/prims/systemverilog>
for the SystemVerilog primitives) if you want to know which functions are defined
as \"regular\" primitives. The compiler looks for primitives in four locations:

* The official install location: e.g.

    * @$CABAL_DIR\/share\/\<GHC_VERSION\>\/clash-lib\-<VERSION\>\/prims\/common@
    * @$CABAL_DIR\/share\/\<GHC_VERSION\>\/clash-lib\-<VERSION\>\/prims\/commonverilog@
    * @$CABAL_DIR\/share\/\<GHC_VERSION\>\/clash-lib\-<VERSION\>\/prims\/systemverilog@
    * @$CABAL_DIR\/share\/\<GHC_VERSION\>\/clash-lib\-<VERSION\>\/prims\/verilog@
    * @$CABAL_DIR\/share\/\<GHC_VERSION\>\/clash-lib\-<VERSION\>\/prims\/vhdl@

* Directories indicated by a 'Clash.Annotations.Primitive.Primitive' annotation
* The current directory (the location given by @pwd@)
* The include directories specified on the command-line: @-i\<DIR\>@

Where redefined primitives in the current directory or include directories will
overwrite those in the official install location. For now, files containing
primitive definitions must have a @.primitives@ file-extension.

Clash differentiates between two types of primitives, /expression/ primitives
and /declaration/ primitives, corresponding to whether the primitive is a VHDL
/expression/ or a VHDL /declaration/. We will first explore /expression/
primitives, using 'Signed' multiplication (@*@) as an example. The
"Clash.Sized.Internal.Signed" module specifies multiplication as follows:

@
(*#) :: 'GHC.TypeLits.KnownNat' n => 'Signed' n -> 'Signed' n -> 'Signed' n
(S a) *# (S b) = fromInteger_INLINE (a * b)
{\-\# NOINLINE (*#) \#-\}
@

For which the VHDL /expression/ primitive is:

@
{ \"BlackBox\" :
  { "name"     : "Clash.Sized.Internal.Signed.*#"
  , "kind"     : \"Expression\"
  , "type"      : "(*#) :: KnownNat n => Signed n -> Signed n -> Signed n"
  , "template" : "resize(~ARG[1] * ~ARG[2], ~LIT[0])"
  }
}
@

The @name@ of the primitive is the /fully qualified/ name of the function you
are creating the primitive for. Because we are creating an /expression/
primitive the kind must be set to @Expression@. As the name suggest, it is a VHDL
/template/, meaning that the compiler must fill in the holes heralded by the
tilde (~). Here:

  * @~ARG[1]@ denotes the second argument given to the @(*#)@ function, which
    corresponds to the LHS of the (@*@) operator.
  * @~ARG[2]@ denotes the third argument given to the @(*#)@ function, which
    corresponds to the RHS of the (@*@) operator.
  * @~LIT[0]@ denotes the first argument given to the @(*#)@ function, with
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
We will use @blockRam#@ as an example, for which the Haskell/Clash code is:

@
{\-\# LANGUAGE BangPatterns \#\-\}

module BlockRam where

import Clash.Explicit.Prelude
import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Signal.Internal (Clock, Signal (..), (.&&.))
import Clash.Sized.Vector (Vec, toList)
import Clash.XException (defaultSeqX)

import qualified Data.Vector as V
import GHC.Stack (HasCallStack, withFrozenCallStack)

blockRam#
  :: ( KnownDomain dom
     , HasCallStack
     , NFDataX a )
  => 'Clock' dom           -- ^ 'Clock' to synchronize to
  -> 'Enable' dom          -- ^ Global enable
  -> 'Vec' n a             -- ^ Initial content of the BRAM, also
                           -- determines the size, @n@, of the BRAM.
                           --
                           -- __NB__: __MUST__ be a constant.
  -> 'Signal' dom Int      -- ^ Read address @r@
  -> 'Signal' dom Bool     -- ^ Write enable
  -> 'Signal' dom Int      -- ^ Write address @w@
  -> 'Signal' dom a        -- ^ Value to write (at address @w@)
  -> 'Signal' dom a        -- ^ Value of the @blockRAM@ at address @r@ from
                           -- the previous clock cycle
blockRam# (Clock _) gen content rd wen =
  go
    (V.fromList ('toList' content))
    (withFrozenCallStack ('deepErrorX' "blockRam: intial value undefined"))
    (fromEnable gen)
    rd
    (fromEnable gen '.&&.' wen)
 where
  go !ram o ret\@(~(re :- res)) rt\@(~(r :- rs)) et\@(~(e :- en)) wt\@(~(w :- wr)) dt\@(~(d :- din)) =
    let ram' = d ``defaultSeqX`` upd ram e (fromEnum w) d
        o'   = if re then ram V.! r else o
    in  o ``seqX`` o :- (ret ``seq`` rt ``seq`` et ``seq`` wt ``seq`` dt ``seq`` go ram' o' res rs en wr din)

  upd ram we waddr d = case maybeIsX we of
    Nothing -> case maybeIsX waddr of
      Nothing -> V.map (const (seq waddr d)) ram
      Just wa -> ram V.// [(wa,d)]
    Just True -> case maybeIsX waddr of
      Nothing -> V.map (const (seq waddr d)) ram
      Just wa -> ram V.// [(wa,d)]
    _ -> ram
{\-\# NOINLINE blockRam# \#\-\}
{\-\# ANN blockRam# hasBlackBox \#\-\}
@

And for which the /declaration/ primitive is:

@
{ \"BlackBox\" :
  { "name" : "Clash.Explicit.BlockRam.blockRam#"
  , "kind" : \"Declaration\"
  , "type" :
"blockRam#
  :: ( KnownDomain dom        ARG[0]
     , HasCallStack  --       ARG[1]
     , NFDataX a )   --       ARG[2]
  => Clock dom       -- clk,  ARG[3]
  -> Enable dom      -- en,   ARG[4]
  -> Vec n a         -- init, ARG[5]
  -> Signal dom Int  -- rd,   ARG[6]
  -> Signal dom Bool -- wren, ARG[7]
  -> Signal dom Int  -- wr,   ARG[8]
  -> Signal dom a    -- din,  ARG[9]
  -> Signal dom a"
    , "template" :
"-- blockRam begin
~GENSYM[~RESULT_blockRam][1] : block
  signal ~GENSYM[~RESULT_RAM][2] : ~TYP[5] := ~CONST[5];
  signal ~GENSYM[rd][4]  : integer range 0 to ~LENGTH[~TYP[5]] - 1;
  signal ~GENSYM[wr][5]  : integer range 0 to ~LENGTH[~TYP[5]] - 1;
begin
  ~SYM[4] <= to_integer(~ARG[6])
  -- pragma translate_off
                mod ~LENGTH[~TYP[5]]
  -- pragma translate_on
                ;

  ~SYM[5] <= to_integer(~ARG[8])
  -- pragma translate_off
                mod ~LENGTH[~TYP[5]]
  -- pragma translate_on
                ;
~IF ~VIVADO ~THEN
  ~SYM[6] : process(~ARG[3])
  begin
    if ~IF~ACTIVEEDGE[Rising][0]~THENrising_edge~ELSEfalling_edge~FI(~ARG[3]) then
      if ~ARG[7] ~IF ~ISACTIVEENABLE[4] ~THEN and ~ARG[4] ~ELSE ~FI then
        ~SYM[2](~SYM[5]) <= ~TOBV[~ARG[9]][~TYP[9]];
      end if;
      ~RESULT <= fromSLV(~SYM[2](~SYM[4]))
      -- pragma translate_off
      after 1 ps
      -- pragma translate_on
      ;
    end if;
  end process; ~ELSE
  ~SYM[6] : process(~ARG[3])
  begin
    if ~IF~ACTIVEEDGE[Rising][0]~THENrising_edge~ELSEfalling_edge~FI(~ARG[3]) then
      if ~ARG[7] ~IF ~ISACTIVEENABLE[4] ~THEN and ~ARG[4] ~ELSE ~FI then
        ~SYM[2](~SYM[5]) <= ~ARG[9];
      end if;
      ~RESULT <= ~SYM[2](~SYM[4])
      -- pragma translate_off
      after 1 ps
      -- pragma translate_on
      ;
    end if;
  end process; ~FI
end block;
--end blockRam"
  }
}
@

Again, the @name@ of the primitive is the fully qualified name of the function
you are creating the primitive for. Because we are creating a /declaration/
primitive the /kind/ must be set to @Declaration@. Instead of discussing what the
individual template holes mean in the above context, we will instead just give
a general listing of the available template holes:

* @~RESULT@: Signal to which the result of a primitive must be assigned
  to. NB: Only used in a /declaration/ primitive.
* @~ARG[N]@: @(N+1)@'th argument to the function.
* @~LIT[N]@: @(N+1)@'th argument to the function. An extra condition that must
  hold is that this @(N+1)@'th argument is an (integer) literal.
* @~CONST[N]@: @(N+1)@'th argument to the function. Clash will try to reduce
  this to a literal, even if it would otherwise consider it too expensive. As
  opposed to @~LIT@, @~CONST@ will render a valid HDL expression.
* @~TYP[N]@: VHDL type of the @(N+1)@'th argument.
* @~TYPO@: VHDL type of the result.
* @~TYPM[N]@: VHDL type/name/ of the @(N+1)@'th argument; used in /type/
  /qualification/.
* @~TYPMO@: VHDL type/name/ of the result; used in /type qualification/.
* @~ERROR[N]@: Error value for the VHDL type of the @(N+1)@'th argument.
* @~ERRORO@: Error value for the VHDL type of the result.
* @~GENSYM[\<NAME\>][N]@: Create a unique name, trying to stay as close to
  the given @\<NAME\>@ as possible. This unique symbol can be referred to in
  other places using @~SYM[N]@.
* @~SYM[N]@: a reference to the unique symbol created by @~GENSYM[\<NAME\>][N]@.
* @~SIGD[\<HOLE\>][N]@: Create a signal declaration, using @\<HOLE\>@ as the name
  of the signal, and the type of the @(N+1)@'th argument.
* @~SIGDO[\<HOLE\>]@: Create a signal declaration, using @\<HOLE\>@ as the name
  of the signal, and the type of the result.
* @~TYPEL[\<HOLE\>]@: The element type of the vector type represented by @\<HOLE\>@.
  The content of @\<HOLE\>@ must either be: @~TYP[N]@, @~TYPO@, or @~TYPEL[\<HOLE\>]@.
* @~COMPNAME@: The name of the component in which the primitive is instantiated.
* @~LENGTH[\<HOLE\>]@: The vector length of the type represented by @\<HOLE\>@.
* @~DEPTH[\<HOLE\>]@: The tree depth of the type represented by @\<HOLE\>@.
  The content of @\<HOLE\>@ must either be: @~TYP[N]@, @~TYPO@, or @~TYPEL[\<HOLE\>]@.
* @~SIZE[\<HOLE\>]@: The number of bits needed to encode the type represented by @\<HOLE\>@.
  The content of @\<HOLE\>@ must either be: @~TYP[N]@, @~TYPO@, or @~TYPEL[\<HOLE\>]@.
* @~IF \<CONDITION\> ~THEN \<THEN\> ~ELSE \<ELSE\> ~FI@: renders the @\<ELSE\>@
  part when @\<CONDITION\>@ evaluates to /0/, and renders the @\<THEN\>@ in all
  other cases. Valid @\<CONDITION\>@s are @~LENGTH[\<HOLE\>]@, @~SIZE[\<HOLE\>]@,
  @~DEPTH[\<HOLE\>]@, @~VIVADO@, @~IW64@, @~ISLIT[N]@, @~ISVAR[N]@, @~ISACTIVEENABLE[N]@,
  @~ISSYNC[N]@, and @~AND[\<HOLE1\>,\<HOLE2\>,..]@.
* @~VIVADO@: /1/ when Clash compiler is invoked with the @-fclash-xilinx@ or
  @-fclash-vivado@ flag. To be used with in an @~IF .. ~THEN .. ~ELSE .. ~FI@
  statement.
* @~TOBV[\<HOLE\>][\<TYPE\>]@: create conversion code that so that the
  expression in @\<HOLE\>@ is converted to a bit vector (@std_logic_vector@).
  The @\<TYPE\>@ hole indicates the type of the expression and must be either
  @~TYP[N]@, @~TYPO@, or @~TYPEL[\<HOLE\>]@.
* @~FROMBV[\<HOLE\>][\<TYPE\>]@: create conversion code that so that the
  expression in @\<HOLE\>@, which has a bit vector (@std_logic_vector@) type,
  is converted to type indicated by @\<TYPE\>@. The @\<TYPE\>@ hole must be
  either @~TYP[N]@, @~TYPO@, or @~TYPEL[\<HOLE\>]@.
* @~INCLUDENAME[N]@: the generated name of the @N@'th included component.
* @~FILEPATH[\<HOLE\>]@: The argument mentioned in @\<HOLE\>@ is a file which
  must be copied to the location of the generated HDL.
* @~GENERATE@: Verilog: create a /generate/ statement, except when already in
  a /generate/ context.
* @~ENDGENERATE@: Verilog: create an /endgenerate/ statement, except when already
  in a /generate/ context.
* @~ISLIT[N]@: Is the @(N+1)@'th argument to the function a literal.
* @~ISVAR[N]@: Is the @(N+1)@'th argument to the function explicitly not a
  literal.
* @~TAG[N]@: Name of given domain. Errors when called on an argument which is not
  a 'KnownDomain', 'Reset', or 'Clock'.
* @~PERIOD[N]@: Clock period of given domain. Errors when called on an argument
  which is not a 'KnownDomain' or 'KnownConf'.
* @~ISACTIVEENABLE[N]@: Is the @(N+1)@'th argument a an Enable line NOT set to a
  constant True. Can be used instead of deprecated (and removed) template tag
  @~ISGATED@. Errors when called on an argument which is not a signal of bools.
* @~ISSYNC[N]@: Does synthesis domain at the @(N+1)@'th argument have synchronous resets. Errors
  when called on an argument which is not a 'KnownDomain' or 'KnownConf'.
* @~ISINITDEFINED[N]@: Does synthesis domain at the @(N+1)@'th argument have defined initial
  values. Errors when called on an argument which is not a 'KnownDomain' or 'KnownConf'.
* @~ACTIVEEDGE[edge][N]@: Does synthesis domain at the @(N+1)@'th argument respond to
  /edge/. /edge/ must be one of 'Falling' or 'Rising'. Errors when called on an
  argument which is not a 'KnownDomain' or 'KnownConf'.
* @~AND[\<HOLE1\>,\<HOLE2\>,..]@: Logically /and/ the conditions in the @\<HOLE\>@'s
* @~VAR[\<NAME\>][N]@: Like @~ARG[N]@ but binds the argument to a variable named NAME.
* @~VARS[N]@: VHDL: Return the variables at the @(N+1)@'th argument.
* @~NAME[N]@: Render the @(N+1)@'th string literal argument as an identifier
  instead of a string literal. Fails when the @(N+1)@'th argument is not a
  string literal.
* @~DEVNULL[\<HOLE\>]@: Render all dependencies of @\<HOLE\>@, but disregard direct output.
* @~REPEAT[\<HOLE\>][N]@: Repeat literal value of @\<HOLE\>@ a total of @N@ times.
* @~TEMPLATE[\<HOLE1\>][\<HOLE2\>]@: Render a file @\<HOLE1\>@ with contents @\<HOLE2\>@.


Some final remarks to end this section: VHDL primitives are there to instruct the
Clash compiler to use the given VHDL template, instead of trying to do normal
synthesis. As a consequence you can use constructs inside the Haskell
definitions that are normally not synthesizable by the Clash compiler. However,
VHDL primitives do not give us /co-simulation/: where you would be able to
simulate VHDL and Haskell in a /single/ environment. If you still want to
simulate your design in Haskell, you will have to describe, in a cycle- and
bit-accurate way, the behavior of that (potentially complex) IP you are trying
to include in your design.

Perhaps in the future, someone will figure out how to connect the two simulation
worlds, using e.g. VHDL's foreign function interface VHPI.
-}

{- $vprimitives
For those who are interested, the equivalent Verilog primitives are:

@
{ \"BlackBox\" :
  { "name"     : "Clash.Sized.Internal.Signed.*#"
  , "kind"     : \"Expression\"
  , "type"     : "(*#) :: KnownNat n => Signed n -> Signed n -> Signed n"
  , "template" : "~ARG[1] * ~ARG[2]"
  }
}
@

and

@
{ \"BlackBox\" :
  { "name" : "Clash.Explicit.BlockRam.blockRam#"
  , "kind" : \"Declaration\"
  , "type" :
"blockRam#
  :: ( KnownDomain dom        ARG[0]
     , HasCallStack  --       ARG[1]
     , NFDataX a )   --       ARG[2]
  => Clock dom       -- clk,  ARG[3]
  => Enable dom      -- en,   ARG[4]
  -> Vec n a         -- init, ARG[5]
  -> Signal dom Int  -- rd,   ARG[6]
  -> Signal dom Bool -- wren, ARG[7]
  -> Signal dom Int  -- wr,   ARG[8]
  -> Signal dom a    -- din,  ARG[9]
  -> Signal dom a"
    , "outputReg" : true
    , "template" :
"// blockRam begin
reg ~TYPO ~GENSYM[~RESULT_RAM][1] [0:~LENGTH[~TYP[5]]-1];

reg ~TYP[5] ~GENSYM[ram_init][3];
integer ~GENSYM[i][4];
initial begin
  ~SYM[3] = ~CONST[5];
  for (~SYM[4]=0; ~SYM[4] < ~LENGTH[~TYP[5]]; ~SYM[4] = ~SYM[4] + 1) begin
    ~SYM[1][~LENGTH[~TYP[5]]-1-~SYM[4]] = ~SYM[3][~SYM[4]*~SIZE[~TYPO]+:~SIZE[~TYPO]];
  end
end
~IF ~ISACTIVEENABLE[4] ~THEN
always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~GENSYM[~RESULT_blockRam][5]~IF ~VIVADO ~THEN
  if (~ARG[4]) begin
    if (~ARG[7]) begin
      ~SYM[1][~ARG[8]] <= ~ARG[9];
    end
    ~RESULT <= ~SYM[1][~ARG[6]];
  end~ELSE
  if (~ARG[7] & ~ARG[4]) begin
    ~SYM[1][~ARG[8]] <= ~ARG[9];
  end
  if (~ARG[4]) begin
    ~RESULT <= ~SYM[1][~ARG[6]];
  end~FI
end~ELSE
always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~SYM[5]
  if (~ARG[7]) begin
    ~SYM[1][~ARG[8]] <= ~ARG[9];
  end
  ~RESULT <= ~SYM[1][~ARG[6]];
end~FI
// blockRam end"
  }
}
@

-}

{- $svprimitives
And the equivalent SystemVerilog primitives are:

@
{ \"BlackBox\" :
  { "name"     : "Clash.Sized.Internal.Signed.*#"
  , "kind"     : \"Expression\"
  , "type"     : "(*#) :: KnownNat n => Signed n -> Signed n -> Signed n"
  , "template" : "~ARG[1] * ~ARG[2]"
  }
}
@

and

@
{ \"BlackBox\" :
  { "name" : "Clash.Explicit.BlockRam.blockRam#"
  , "kind" : \"Declaration\"
  , "type" :
"blockRam#
  :: ( KnownDomain dom        ARG[0]
     , HasCallStack  --       ARG[1]
     , NFDataX a )   --       ARG[2]
  => Clock dom       -- clk,  ARG[3]
  -> Enable dom      -- en,   ARG[4]
  -> Vec n a         -- init, ARG[5]
  -> Signal dom Int  -- rd,   ARG[6]
  -> Signal dom Bool -- wren, ARG[7]
  -> Signal dom Int  -- wr,   ARG[8]
  -> Signal dom a    -- din,  ARG[9]
  -> Signal dom a"
    , "template" :
"// blockRam begin
~SIGD[~GENSYM[RAM][1]][5];
logic [~SIZE[~TYP[9]]-1:0] ~GENSYM[~RESULT_q][2];
initial begin
  ~SYM[1] = ~CONST[5];
end~IF ~ISACTIVEENABLE[4] ~THEN
always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~GENSYM[~COMPNAME_blockRam][3]~IF ~VIVADO ~THEN
  if (~ARG[4]) begin
    if (~ARG[7]) begin
      ~SYM[1][~ARG[8]] <= ~TOBV[~ARG[9]][~TYP[9]];
    end
    ~SYM[2] <= ~SYM[1][~ARG[6]];
  end~ELSE
  if (~ARG[7] & ~ARG[4]) begin
    ~SYM[1][~ARG[8]] <= ~TOBV[~ARG[9]][~TYP[9]];
  end
  if (~ARG[4]) begin
    ~SYM[2] <= ~SYM[1][~ARG[6]];
  end~FI
end~ELSE
always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~SYM[3]
  if (~ARG[7]) begin
    ~SYM[1][~ARG[8]] <= ~TOBV[~ARG[9]][~TYP[9]];
  end
  ~SYM[2] <= ~SYM[1][~ARG[6]];
end~FI
assign ~RESULT = ~FROMBV[~SYM[2]][~TYP[9]];
// blockRam end"
  }
}
@

-}

{- $multiclock #multiclock#
Clash supports designs multiple /clock/ (and /reset/) domains, though perhaps in
a slightly limited form. What is possible is:

* Create clock primitives, such as PPLs, which have an accompanying HDL primitive
  (described <#primitives later on> in this tutorial).
* Explicitly assign clocks to memory primitives.
* Synchronize between differently-clocked parts of your design in a type-safe
  way.

What is /not/ possible is:

* Directly generate a clock signal in module A, and assign this clock signal to
  a memory primitive in module B. For example, the following is not possible:

  @
  pow2Clocks
    :: ( 'KnownConfiguration' domIn (''DomainConfiguration' domIn pIn eIn rIn iIn polIn)
       , 'KnownConfiguration' dom2  (''DomainConfiguration' dom2 (2*pIn) e2 r2 i2 p2)
       , 'KnownConfiguration' dom4  (''DomainConfiguration' dom4 (4*pIn) e4 r4 i4 p4)
       , 'KnownConfiguration' dom8  (''DomainConfiguration' dom8 (8*pIn) e8 r8 i8 p8)
       , 'KnownConfiguration' dom16 (''DomainConfiguration' dom16 (16*pIn) e16 r16 i16 p16)
    => 'Clock' domIn
    -> 'Reset' domIn
    -> ( 'Clock' dom16
       , 'Clock' dom8
       , 'Clock' dom4
       , 'Clock' dom2 )
  pow2Clocks clk rst = (cnt!3, cnt!2, cnt!1, cnt!0)
    where
      cnt = 'Clash.Explicit.Signal.register' clk rst 0 (cnt + 1)
  @

  As it is not possible to convert the individual bits to a 'Clock'.

  However! What is possible is to do the following:

  @
  pow2Clocks'
    :: ( 'KnownConfiguration' domIn (''DomainConfiguration' domIn pIn eIn rIn iIn polIn)
       , 'KnownConfiguration' dom2  (''DomainConfiguration' dom2 (2*pIn) e2 r2 i2 p2)
       , 'KnownConfiguration' dom4  (''DomainConfiguration' dom4 (4*pIn) e4 r4 i4 p4)
       , 'KnownConfiguration' dom8  (''DomainConfiguration' dom8 (8*pIn) e8 r8 i8 p8)
       , 'KnownConfiguration' dom16 (''DomainConfiguration' dom16 (16*pIn) e16 r16 i16 p16)
    => 'Clock' domIn
    -> 'Reset' domIn
    -> ( 'Clock' dom16
       , 'Clock' dom8
       , 'Clock' dom4
       , 'Clock' dom2 )
  pow2Clocks' clk rst = ('clockGen', 'clockGen', 'clockGen', 'clockGen')
  {\-\# NOINLINE pow2Clocks' \#-\}
  @

  And then create a HDL primitive, as described in later on in
  this <#primitives tutorial>, to implement the desired behavior in HDL.

What this means is that when Clash converts your design to VHDL/(System)Verilog,
you end up with a top-level module/entity with multiple clock and reset ports
for the different clock domains. If you're targeting an FPGA, you can use e.g. a
<https://www.altera.com/literature/ug/ug_altpll.pdf PPL> or
<http://www.xilinx.com/support/documentation/user_guides/ug472_7Series_Clocking.pdf MMCM>
to provide the clock signals.

== Building a FIFO synchronizer

This part of the tutorial assumes you know what <https://en.wikipedia.org/wiki/Metastability_in_electronics metastability>
is, and how it can never truly be avoided in any asynchronous circuit. Also
it assumes that you are familiar with the design of synchronizer circuits, and
why a dual flip-flop synchronizer only works for bit-synchronization and not
word-synchronization.
The explicitly clocked versions of all synchronous functions and primitives can
be found in "Clash.Explicit.Prelude", which also re-exports the functions in
"Clash.Signal.Explicit". We will use those functions to create a FIFO where
the read and write port are synchronized to different clocks. Below you can find
the code to build the FIFO synchronizer based on the design described in:
<http://www.sunburst-design.com/papers/CummingsSNUG2002SJ_FIFO1.pdf>

We start with enable a few options that will make writing the type-signatures for
our components a bit easier. Instead of importing the standard "Clash.Prelude"
module, we will import the "Clash.Explicit.Prelude" module where all our clocks
and resets must be explicitly routed (other imports will be used later):

@
module MultiClockFifo where

import "Clash.Explicit.Prelude"
import "Clash.Prelude"          (mux)
import Data.Maybe             (isJust)
import Data.Constraint        (Dict (..), (:-)( Sub ))
import Data.Constraint.Nat    (leTrans)
@

Then we'll start with the /heart/ of the FIFO synchronizer, an asynchronous RAM
in the form of 'asyncRam'. It's called an asynchronous RAM because the read
port is not synchronized to any clock (though the write port is). Note that in
Clash we don't really have asynchronous logic, there is only combinational and
synchronous logic. As a consequence, we see in the type signature of
'Clash.Explicit.Prelude.asyncRam':

@
__asyncRam__
  :: ( 'Enum' addr
     , 'HasCallStack'
     , 'KnownDomain' wdom
     , 'KnownDomain' rdom
     )
  => 'Clock' wdom                     -- ^ 'Clock' to which to synchronize the write port of the RAM
  -> 'Clock' rdom                     -- ^ 'Clock' to which the read address signal, @r@, is synchronized to
  -> 'Enable' wdom                    -- ^ Global enable
  -> 'SNat' n                         -- ^ Size @n@ of the RAM
  -> 'Signal' rdom addr               -- ^ Read address @r@
  -> 'Signal' wdom (Maybe (addr, a))  -- ^ (write address @w@, value to write)
  -> 'Signal' rdom a                  -- ^ Value of the @RAM@ at address @r@
@

that the signal containing the read address __r__ is synchronized to a different
clock. That is, there is __no__ such thing as an @AsyncSignal@ in Clash.

We continue by instantiating the 'Clash.Explicit.Prelude.asyncRam':

@
fifoMem wclk rclk en addrSize\@SNat full raddr writeM =
  'Clash.Explicit.Prelude.asyncRam'
    wclk rclk en
    ('pow2SNat' addrSize)
    raddr
    ('mux' full (pure Nothing) writeM)
@

We see that we give it @2^addrSize@ elements, where @addrSize@ is the bit-size
of the address. Also, we only write new values to the RAM when a new write is
requested, indicated by @wdataM@ having a 'Data.Maybe.Just' value, and the
buffer is not full, indicated by @wfull@.

The next part of the design calculates the read and write address for the
asynchronous RAM, and creates the flags indicating whether the FIFO is full
or empty. The address and flag generator is given in 'mealy' machine style:

@
ptrCompareT
  :: SNat addrSize
  -> (BitVector (addrSize + 1) -> BitVector (addrSize + 1) -> Bool)
  -> ( BitVector (addrSize + 1)
     , BitVector (addrSize + 1)
     , Bool )
  -> ( BitVector (addrSize + 1)
     , Bool )
  -> ( ( BitVector (addrSize + 1)
       , BitVector (addrSize + 1)
       , Bool )
     , ( Bool
       , BitVector addrSize
       , BitVector (addrSize + 1)
       )
     )
ptrCompareT addrSize\@SNat flagGen (bin, ptr, flag) (s_ptr, inc) =
  ( (bin', ptr', flag')
  , (flag, addr, ptr) )
 where
  -- GRAYSTYLE2 pointer
  bin' = bin + 'boolToBV' (inc && not flag)
  ptr' = (bin' \`shiftR\` 1) \`xor\` bin'
  addr = 'truncateB' bin
  flag' = flagGen ptr' s_ptr
@

It is parametrized in both address size, @addrSize@, and status flag generator,
@flagGen@. It has two inputs, @s_ptr@, the synchronized pointer from the other
clock domain, and @inc@, which indicates we want to perform a write or read of
the FIFO. It creates three outputs: @flag@, the full or empty flag, @addr@, the
read or write address into the RAM, and @ptr@, the Gray-encoded version of the
read or write address which will be synchronized between the two clock domains.

Next follow the initial states of address generators, and the flag generators
for the empty and full flags:

@
-- FIFO empty: when next pntr == synchronized wptr or on reset
isEmpty       = (==)
rptrEmptyInit = (0, 0, True)

-- FIFO full: when next pntr == synchronized {~wptr[addrSize:addrSize-1],wptr[addrSize-2:0]}
isFull
  :: forall addrSize
   . (2 <= addrSize)
  => 'SNat' addrSize
  -> 'BitVector' (addrSize + 1)
  -> 'BitVector' (addrSize + 1)
  -> Bool
isFull addrSize\@SNat ptr s_ptr = case leTrans \@1 \@2 \@addrSize of
  Sub Dict ->
    let a1 = 'SNat' \@(addrSize - 1)
        a2 = 'SNat' \@(addrSize - 2)
    in  ptr == ('complement' ('slice' addrSize a1 s_ptr) '++#' 'slice' a2 d0 s_ptr)

wptrFullInit = (0, 0, False)
@

We create a dual flip-flop synchronizer to be used to synchronize the
Gray-encoded pointers between the two clock domains:

@
ptrSync clk1 clk2 rst2 en2 =
  'Clash.Explicit.Signal.register' clk2 rst2 en2 0 . 'Clash.Explicit.Signal.register' clk2 rst2 en2 0 . 'Clash.Explicit.Signal.unsafeSynchronizer' clk1 clk2
@

It uses the 'Clash.Explicit.Signal.unsafeSynchronizer' primitive, which is needed to go from one clock
domain to the other. All synchronizers are specified in terms of
'Clash.Explicit.Signal.unsafeSynchronizer' (see for example the <src/Clash-Prelude-RAM.html#line-103 source of asyncRam>).
The 'Clash.Explicit.Signal.unsafeSynchronizer' primitive is turned into a (bundle of) wire(s) by the
Clash compiler, so developers must ensure that it is only used as part of a
proper synchronizer.

Finally we combine all the components in:

@
asyncFIFOSynchronizer
  :: ( 'KnownDomain' wdom
     , 'KnownDomain' rdom
     , 2 <= addrSize )
  => SNat addrSize
  -- ^ Size of the internally used addresses, the  FIFO contains @2^addrSize@
  -- elements.
  -> 'Clock' wdom
  -- ^ 'Clock' to which the write port is synchronized
  -> 'Clock' rdom
  -- ^ 'Clock' to which the read port is synchronized
  -> 'Reset' wdom
  -> 'Reset' rdom
  -> 'Enable' wdom
  -> 'Enable' rdom
  -> 'Signal' rdom Bool
  -- ^ Read request
  -> 'Signal' wdom (Maybe a)
  -- ^ Element to insert
  -> ('Signal' rdom a, 'Signal' rdom Bool, 'Signal' wdom Bool)
  -- ^ (Oldest element in the FIFO, @empty@ flag, @full@ flag)
asyncFIFOSynchronizer addrSize\@SNat wclk rclk wrst rrst wen ren rinc wdataM =
  (rdata, rempty, wfull)
 where
  s_rptr = ptrSync rclk wclk wrst wen rptr
  s_wptr = ptrSync wclk rclk rrst ren wptr

  rdata =
    fifoMem
      wclk rclk wen
      addrSize wfull raddr
      (liftA2 (,) \<$\> (pure \<$\> waddr) \<*\> wdataM)

  (rempty, raddr, rptr) =
    'mealyB'
      rclk rrst ren
      (ptrCompareT addrSize isEmpty)
      (0, 0, True)
      (s_wptr, rinc)

  (wfull, waddr, wptr) =
    'mealyB'
      wclk wrst wen
      (ptrCompareT addrSize (isFull addrSize))
      (0, 0, False)
      (s_rptr, isJust \<$\> wdataM)
@

where we first specify the synchronization of the read and the write pointers,
instantiate the asynchronous RAM, and instantiate the read address \/ pointer \/
flag generator and write address \/ pointer \/ flag generator.

Ultimately, the whole file containing our FIFO design will look like this:

@
module MultiClockFifo where

import "Clash.Explicit.Prelude"
import "Clash.Prelude"          (mux)
import Data.Maybe             (isJust)
import Data.Constraint        (Dict (..), (:-)( Sub ))
import Data.Constraint.Nat    (leTrans)

fifoMem wclk rclk en addrSize\@SNat full raddr writeM =
  'Clash.Explicit.Prelude.asyncRam'
    wclk rclk en
    ('pow2SNat' addrSize)
    raddr
    ('mux' full (pure Nothing) writeM)

ptrCompareT
  :: SNat addrSize
  -> (BitVector (addrSize + 1) -> BitVector (addrSize + 1) -> Bool)
  -> ( BitVector (addrSize + 1)
     , BitVector (addrSize + 1)
     , Bool )
  -> ( BitVector (addrSize + 1)
     , Bool )
  -> ( ( BitVector (addrSize + 1)
       , BitVector (addrSize + 1)
       , Bool )
     , ( Bool
       , BitVector addrSize
       , BitVector (addrSize + 1)
       )
     )
ptrCompareT addrSize\@SNat flagGen (bin, ptr, flag) (s_ptr, inc) =
  ( (bin', ptr', flag')
  , (flag, addr, ptr) )
 where
  -- GRAYSTYLE2 pointer
  bin' = bin + 'boolToBV' (inc && not flag)
  ptr' = (bin' \`shiftR\` 1) \`xor\` bin'
  addr = 'truncateB' bin
  flag' = flagGen ptr' s_ptr

-- FIFO empty: when next pntr == synchronized wptr or on reset
isEmpty       = (==)
rptrEmptyInit = (0, 0, True)

-- FIFO full: when next pntr == synchronized {~wptr[addrSize:addrSize-1],wptr[addrSize-2:0]}
isFull
  :: forall addrSize
   . (2 <= addrSize)
  => 'SNat' addrSize
  -> 'BitVector' (addrSize + 1)
  -> 'BitVector' (addrSize + 1)
  -> Bool
isFull addrSize\@SNat ptr s_ptr = case leTrans \@1 \@2 \@addrSize of
  Sub Dict ->
    let a1 = 'SNat' \@(addrSize - 1)
        a2 = 'SNat' \@(addrSize - 2)
    in  ptr == ('complement' ('slice' addrSize a1 s_ptr) '++#' 'slice' a2 d0 s_ptr)

wptrFullInit = (0, 0, False)

-- Dual flip-flop synchronizer
ptrSync clk1 clk2 rst2 en2 =
  'Clash.Explicit.Signal.register' clk2 rst2 en2 0 . 'Clash.Explicit.Signal.register' clk2 rst2 en2 0 . 'Clash.Explicit.Signal.unsafeSynchronizer' clk1 clk2

-- Async FIFO synchronizer
asyncFIFOSynchronizer
  :: ( 'KnownDomain' wdom
     , 'KnownDomain' rdom
     , 2 <= addrSize )
  => SNat addrSize
  -- ^ Size of the internally used addresses, the  FIFO contains @2^addrSize@
  -- elements.
  -> 'Clock' wdom
  -- ^ Clock to which the write port is synchronized
  -> 'Clock' rdom
  -- ^ Clock to which the read port is synchronized
  -> 'Reset' wdom
  -> 'Reset' rdom
  -> 'Enable' wdom
  -> 'Enable' rdom
  -> 'Signal' rdom Bool
  -- ^ Read request
  -> 'Signal' wdom (Maybe a)
  -- ^ Element to insert
  -> ('Signal' rdom a, 'Signal' rdom Bool, 'Signal' wdom Bool)
  -- ^ (Oldest element in the FIFO, @empty@ flag, @full@ flag)
asyncFIFOSynchronizer addrSize\@SNat wclk rclk wrst rrst wen ren rinc wdataM =
  (rdata, rempty, wfull)
 where
  s_rptr = ptrSync rclk wclk wrst wen rptr
  s_wptr = ptrSync wclk rclk rrst ren wptr

  rdata =
    fifoMem
      wclk rclk wen
      addrSize wfull raddr
      (liftA2 (,) \<$\> (pure \<$\> waddr) \<*\> wdataM)

  (rempty, raddr, rptr) =
    'mealyB'
      rclk rrst ren
      (ptrCompareT addrSize isEmpty)
      (0, 0, True)
      (s_wptr, rinc)

  (wfull, waddr, wptr) =
    'mealyB'
      wclk wrst wen
      (ptrCompareT addrSize (isFull addrSize))
      (0, 0, False)
      (s_rptr, isJust \<$\> wdataM)
@

== Instantiating a FIFO synchronizer

Having finished our FIFO synchronizer it's time to instantiate with concrete
clock domains. Let us assume we have part of our system connected to an ADC
which runs at 20 MHz, and we have created an FFT component running at only 9
MHz. We want to connect part of our design connected to the ADC, and running
at 20 MHz, to part of our design connected to the FFT running at 9 MHz.

We can calculate the clock periods using 'hzToPeriod':

>>> hzToPeriod 20e6
50000
>>> hzToPeriod 9e6
111111

We can then create the clock and reset domains:

@
'createDomain' vSystem{vName=\"ADC\", vPeriod=hzToPeriod 20e6}
'createDomain' vSystem{vName=\"FFT\", vPeriod=hzToPeriod 9e6}
@

and subsequently a 256-space FIFO synchronizer that safely bridges the ADC clock
domain and to the FFT clock domain:

@
adcToFFT
  :: 'Clock' \"ADC\"
  -> 'Clock' \"FFT\"
  -> 'Reset' \"ADC\"
  -> 'Reset' \"FFT\"
  -> 'Enable' \"ADC\"
  -> 'Enable' \"FFT\"
  -> 'Signal' \"FFT\" Bool
  -> 'Signal' \"ADC\" (Maybe (SFixed 8 8))
  -> ( 'Signal' \"FFT\" (SFixed 8 8)
     , 'Signal' \"FFT\" Bool
     , 'Signal' \"ADC\" Bool )
adcToFFT = asyncFIFOSynchronizer d8
@

-}

{- $conclusion
For now, this is the end of this tutorial. We will be adding updates over time,
so check back from time to time. We recommend that you continue with
exploring the "Clash.Prelude" module, and get a better understanding of the
capabilities of Clash in the process.
-}

{- $errorsandsolutions
A list of often encountered errors and their solutions:

* __Type error: Couldn't match expected type @'Signal' dom (a,b)@ with actual type__
  __@('Signal' dom a, 'Signal' dom b)@__:

    Signals of product types and product types of signals are __isomorphic__
    due to synchronisity principle, but are not (structurally) equal. Tuples
    are a product type. Use the 'bundle' function to convert from a product
    type to the signal type. So if your code which gives the error looks like:

    @
    ... = f a b (c,d)
    @

    add the 'bundle' function like so:

    @
    ... = f a b ('bundle' (c,d))
    @

    Product types supported by 'bundle' are:

    * All tuples up to and including 62-tuples (GHC limit)
    * The 'Vec'tor type

* __Type error: Couldn't match expected type @('Signal' dom a, 'Signal' dom b)@ with__
  __ actual type @'Signal' dom (a,b)@__:

    Product types of signals and signals of product types are __isomorphic__
    due to synchronicity principle, but are not (structurally) equal. Tuples
    are a product type. Use the 'unbundle' function to convert from a signal
    type to the product type. So if your code which gives the error looks
    like:

    @
    (c,d) = f a b
    @

    add the 'unbundle' function like so:

    @
    (c,d) = 'unbundle' (f a b)
    @

    Product types supported by 'unbundle' are:

    * All tuples up to and including 62-tuples (GHC limit)
    * The 'Vec'tor type

* __Clash.Netlist(..): Not in normal form: \<REASON\>: \<EXPR\>__:

    A function could not be transformed into the expected normal form. This
    usually means one of the following:

    * The @topEntity@ has higher-order arguments, or a higher-order result.
    * You are using types which cannot be represented in hardware.

    The solution for all the above listed reasons is quite simple: remove them.
    That is, make sure that the @topEntity@ is completely monomorphic and
    first-order. Also remove any variables and constants/literals that have a
    non-representable type; see <#limitations Limitations of Clash> to find
    out which types are not representable.

* __Clash.Normalize(..): Clash can only normalize monomorphic functions, but this is polymorphic__:

    If this happens for a @topEntity@ or something with a @Synthesize@ annotation,
    add a monomorphic type signature.
    Non topEntites should be type-specialized by clash automatically, if not please report this as a bug.
    But adding a monomorphic type signature should still help (when possible).


* __Clash.Normalize(..): Expr belonging to bndr: \<FUNCTION\> remains__
  __recursive after normalization__:

    * If you actually wrote a recursive function, rewrite it to a non-recursive
      one using e.g. one of the higher-order functions in "Clash.Sized.Vector" :-)

    * You defined a recursively defined value, but left it polymorphic:

    @
    topEntity x y = acc
      where
        acc = 'register' 3 (acc + x * y)
    @

    The above function, works for any number-like type. This means that @acc@ is
    a recursively defined __polymorphic__ value. Adding a monomorphic type
    annotation makes the error go away:

    @
    topEntity
      :: 'SystemClockResetEnable'
      => 'Signal' 'System' ('Signed' 8)
      -> 'Signal' 'System' ('Signed' 8)
      -> 'Signal' 'System' ('Signed' 8)
    topEntity x y = acc
      where
        acc = 'register' 3 (acc + x * y)
    @

* __Clash.Normalize.Transformations(..): InlineNonRep: \<FUNCTION\> already__
  __inlined 100 times in:\<FUNCTION\>, \<TYPE\>__:

    You left the @topEntity@ function polymorphic or higher-order: use
    @:t topEntity@ to check if the type is indeed polymorphic or higher-order.
    If it is, add a monomorphic type signature, and / or supply higher-order
    arguments.

*  __\<*** Exception: \<\<loop\>\>__ or "blinking cursor"

    You are using value-recursion, but one of the 'Vec'tor functions that you
    are using is too /strict/ in one of the recursive arguments. For example:

    @
    -- Bubble sort for 1 iteration
    sortV xs = 'map' fst sorted ':<' (snd ('last' sorted))
     where
       lefts  = 'head' xs :> 'map' snd ('init' sorted)
       rights = 'tail' xs
       sorted = 'zipWith' compareSwapL lefts rights

    -- Compare and swap
    compareSwapL a b = if a < b then (a,b)
                                else (b,a)
    @

    Will not terminate because 'zipWith' is too strict in its second argument.

    In this case, adding 'lazyV' on 'zipWith's second argument:

    @
    sortVL xs = 'map' fst sorted ':<' (snd ('last' sorted))
     where
       lefts  = 'head' xs :> map snd ('init' sorted)
       rights = 'tail' xs
       sorted = 'zipWith' compareSwapL ('lazyV' lefts) rights
    @

    Results in a successful computation:

    >>> sortVL (4 :> 1 :> 2 :> 3 :> Nil)
    1 :> 2 :> 3 :> 4 :> Nil
-}

{- $limitations #limitations#
Here is a list of Haskell features for which the Clash compiler has only
/limited/ support (for now):

* __Recursively defined functions__

    At first hand, it seems rather bad that a compiler for a functional language
    cannot synthesize recursively defined functions to circuits. However, when
    viewing your functions as a /structural/ specification of a circuit, this
    /feature/ of the Clash compiler makes sense. Also, only certain types of
    recursion are considered non-synthesizable; recursively defined values are
    for example synthesizable: they are (often) synthesized to feedback loops.

    Let us distinguish between three variants of recursion:

    * __Dynamic data-dependent recursion__

        As demonstrated in this definition of a function that calculates the
        n'th Fibbonacci number:

        @
        fibR 0 = 0
        fibR 1 = 1
        fibR n = fibR (n-1) + fibR (n-2)
        @

        To get the first 10 numbers, we do the following:

        >>> import qualified Data.List as L
        >>> L.map fibR [0..9]
        [0,1,1,2,3,5,8,13,21,34]

        The @fibR@ function is not synthesizable by the Clash compiler, because,
        when we take a /structural/ view, @fibR@ describes an infinitely deep
        structure.

        In principal, descriptions like the above could be synthesized to a
        circuit, but it would have to be a /sequential/ circuit. Where the most
        general synthesis would then require a stack. Such a synthesis approach
        is also known as /behavioral/ synthesis, something which the Clash
        compiler simply does not do. One reason that Clash does not do this is
        because it does not fit the paradigm that only functions working on
        values of type 'Signal' result in sequential circuits, and all other
        (non higher-order) functions result in combinational circuits. This
        paradigm gives the designer the most straightforward mapping from the
        original Haskell description to generated circuit, and thus the greatest
        control over the eventual size of the circuit and longest propagation
        delay.

    * __Value-recursion__

        As demonstrated in this definition of a function that calculates the
        n'th Fibbonaci number on the n'th clock cycle:

        @
        fibS :: SystemClockResetEnable => Signal System (Unsigned 64)
        fibS = r
          where r = 'register' 0 r + 'register' 0 ('register' 1 r)
        @

        To get the first 10 numbers, we do the following:

        >>> sampleN @System 11 fibS
        [0,0,1,1,2,3,5,8,13,21,34]

        Unlike the @fibR@ function, the above @fibS@ function /is/ synthesizable
        by the Clash compiler. Where the recursively defined (non-function)
        value /r/ is synthesized to a feedback loop containing three registers
        and one adder.

        Note that not all recursively defined values result in a feedback loop.
        An example that uses recursively defined values which does not result
        in a feedback loop is the following function that performs one iteration
        of bubble sort:

        @
        sortVL xs = 'map' fst sorted ':<' (snd ('last' sorted))
         where
           lefts  = 'head' xs :> map snd ('init' sorted)
           rights = 'tail' xs
           sorted = 'zipWith' compareSwapL ('lazyV' lefts) rights
        @

        Where we can clearly see that @lefts@ and @sorted@ are defined in terms
        of each other. Also the above @sortV@ function /is/ synthesizable.

    * __Static/Structure-dependent recursion__

        Static, or, structure-dependent recursion is a rather /vague/ concept.
        What we mean by this concept are recursive definitions where a user can
        sensibly imagine that the recursive definition can be completely
        unfolded (all recursion is eliminated) at compile-time in a finite
        amount of time.

        Such definitions would e.g. be:

        @
        mapV :: (a -> b) -> Vec n a -> Vec n b
        mapV _ Nil         = Nil
        mapV f (Cons x xs) = Cons (f x) (mapV f xs)

        topEntity :: Vec 4 Int -> Vec 4 Int
        topEntity = mapV (+1)
        @

        Where one can imagine that a compiler can unroll the definition of
        @mapV@ four times, knowing that the @topEntity@ function applies @mapV@
        to a 'Vec' of length 4. Sadly, the compile-time evaluation mechanisms in
        the Clash compiler are very poor, and a user-defined function such as
        the @mapV@ function defined above, is /currently/ not synthesizable.
        We /do/ plan to add support for this in the future. In the mean time,
        this poor support for user-defined recursive functions is amortized by
        the fact that the Clash compiler has built-in support for the
        higher-order functions defined in "Clash.Sized.Vector". Most regular
        design patterns often encountered in circuit design are captured by the
        higher-order functions in "Clash.Sized.Vector".

* __Recursive datatypes__

    The Clash compiler needs to be able to determine a bit-size for any value
    that will be represented in the eventual circuit. More specifically, we need
    to know the maximum number of bits needed to represent a value. While this
    is trivial for values of the elementary types, sum types, and product types,
    putting a fixed upper bound on recursive types is not (always) feasible.
    This means that the ubiquitous list type is unsupported! The only recursive
    types that are currently supported by the Clash compiler is the 'Vec'tor and
    'RTree' types, for which the compiler has hard-coded knowledge.

    For \"easy\" 'Vec'tor literals you should use Template Haskell splices and
    the 'listToVecTH' /meta/-function that as we have seen earlier in this tutorial.

* __GADTs__

    Clash has experimental support for GADTs. Similar to recursive types, Clash
    can't determine bit-sizes of GADTs. Notable exceptions to this rule are
    'Vec' and 'RTree'. You can still use your own GADTs, as long as they can be
    removed through static analysis. For example, the following case will be
    optimized away and is therefore fine to use:

    @
    x =
      case 'resetKind' @@'System' of
        SAsynchronous -> \'a\'
        SSynchronous -> \'b\'
    @

* __Floating point types__

    There is no support for the 'Float' and 'Double' types, if you need numbers
    with a /fractional/ part you can use the 'Fixed' point type.

    As to why there is no support for these floating point types:

        1.  In order to achieve reasonable operating frequencies, arithmetic
            circuits for floating point data types must be pipelined.
        2.  Haskell's primitive arithmetic operators on floating point data types,
            such as 'plusFloat#'

            @
            __plusFloat#__ :: 'Float#' -> 'Float#' -> 'Float#'
            @

            which underlie @'Float'@'s 'Num' instance, must be implemented as
            purely combinational circuits according to their type. Remember,
            sequential circuits operate on values of type \"@'Signal' a@\".

    Although it is possible to implement purely combinational (not pipelined)
    arithmetic circuits for floating point data types, the circuit would be
    unreasonable slow. And so, without synthesis possibilities for the basic
    arithmetic operations, there is no point in supporting the floating point
    data types.

* __Haskell primitive types__

    Only the following primitive Haskell types are supported:

        * 'Integer'
        * 'Int'
        * 'Int8'
        * 'Int16'
        * 'Int32'
        * 'Int64' (not available when compiling with @-fclash-intwidth=32@ on a 64-bit machine)
        * 'Word'
        * 'Word8'
        * 'Word16'
        * 'Word32'
        * 'Word64' (not available when compiling with @-fclash-intwidth=32@ on a 64-bit machine)
        * 'Char'

    There are several aspects of which you should take note:

        *   'Int' and 'Word' are represented by the same number of bits as is
            native for the architecture of the computer on which the Clash
            compiler is executed. This means that if you are working on a 64-bit
            machine, 'Int' and 'Word' will be 64-bit. This might be problematic
            when you are working in a team, and one designer has a 32-bit
            machine, and the other has a 64-bit machine. In general, you should
            be avoiding 'Int' in such cases, but as a band-aid solution, you can
            force the Clash compiler to use a specific bit-width for `Int` and
            `Word` using the @-fclash-intwidth=N@ flag, where /N/ must either be
            /32/ or /64/.

        *   When you use the @-fclash-intwidth=32@ flag on a /64-bit/ machine,
            the 'Word64' and 'Int64' types /cannot/ be translated. This
            restriction does /not/ apply to the other three combinations of
            @-fclash-intwidth@ flag and machine type.

        *   The translation of 'Integer' is not meaning-preserving. 'Integer' in
            Haskell is an arbitrary precision integer, something that cannot
            be represented in a statically known number of bits. In the Clash
            compiler, we chose to represent 'Integer' by the same number of bits
            as we do for 'Int' and 'Word'. As you have read in a previous
            bullet point, this number of bits is either 32 or 64, depending on
            the architecture of the machine the Clash compiler is running on, or
            the setting of the @-fclash-intwidth@ flag.

            Consequently, you should use `Integer` with due diligence; be
            especially careful when using `fromIntegral` as it does a conversion
            via 'Integer'. For example:

                > signedToUnsigned :: Signed 128 -> Unsigned 128
                > signedToUnsigned = fromIntegral

            can either lose the top 64 or 96 bits depending on whether 'Integer'
            is represented by 64 or 32 bits. Instead, when doing such conversions,
            you should use 'bitCoerce':

                > signedToUnsigned :: Signed 128 -> Unsigned 128
                > signedToUnsigned = bitCoerce

* __Side-effects: 'IO', 'ST', etc.__

    There is no support for side-effecting computations such as those in the
    'IO' or 'ST' monad. There is also no support for Haskell's
    <http://www.haskell.org/haskellwiki/Foreign_Function_Interface FFI>.
-}

{- $vslava
In Haskell land the most well-known way of describing digital circuits is the
Lava family of languages:

* <http://hackage.haskell.org/package/chalmers-lava2000 Chalmers Lava>
* <http://hackage.haskell.org/package/xilinx-lava Xilinx Lava>
* <http://hackage.haskell.org/package/york-lava York Lava>
* <http://hackage.haskell.org/package/kansas-lava Kansas Lava>

The big difference between Clash and Lava is that Clash uses a \"standard\"
compiler (static analysis) approach towards synthesis, where Lava is an
embedded domain specific language. One downside of static analysis vs. the
embedded language approach is already clearly visible: synthesis of recursive
descriptions does not come for \"free\". This will be implemented in Clash in
due time, but that doesn't help the circuit designer right now. As already
mentioned earlier, the poor support for recursive functions is amortized by
the built-in support for the higher-order in "Clash.Sized.Vector".

The big upside of Clash and its static analysis approach is that Clash can
do synthesis of \"normal\" functions: there is no forced encasing datatype (often
called /Signal/ in Lava) on all the arguments and results of a synthesizable
function. This enables the following features not available to Lava:

* Automatic synthesis for user-defined ADTs
* Synthesis of all choice constructs (pattern matching, guards, etc.)
* 'Applicative' instance for the 'Signal' type
* Working with \"normal\" functions permits the use of e.g. the
  'Control.Monad.State.Lazy.State' monad to describe the functionality of a
  circuit.

Although there are Lava alternatives to some of the above features (e.g.
first-class patterns to replace pattern matching) they are not as \"beautiful\"
and / or easy to use as the standard Haskell features.
-}

{- $migration

* Clash has overhauled the way synthesis options are represented. You can read
  about this change in the blogpost: <https://clash-lang.org/blog/0005-synthesis-domain/ New feature: configurable initial values>.
  The executive summary is as follows:

+------------------------------------------------------------------------------+--------------------------------------------------------------------+
| __0.99__                                                                     | __1.0__                                                            |
+------------------------------------------------------------------------------+--------------------------------------------------------------------+
| @topEntity (clk::Clock d 'Source) rst = withClockReset f clk  rst@           | @topEntity clk rst = withClockResetEnable clk rst enableGen f@     |
+------------------------------------------------------------------------------+--------------------------------------------------------------------+
| @topEntity (clk::Clock d 'Gated) rst  = withClockReset f clk  rst@           | @topEntity clk rst enable = withClockResetEnable clk rst enable f@ |
+------------------------------------------------------------------------------+--------------------------------------------------------------------+
| @data A = ... @ (and @A@ is used as state, for example in register or mealy) | @data A = ... deriving (Generic,NFDataX)@                          |
+------------------------------------------------------------------------------+--------------------------------------------------------------------+
| @SystemClockReset@                                                           | @SystemClockResetEnable@                                           |
+------------------------------------------------------------------------------+--------------------------------------------------------------------+
| @HiddenClockReset dom gated sync@                                            | @HiddenClockResetEnable dom@                                       |
+------------------------------------------------------------------------------+--------------------------------------------------------------------+
| @HiddenClock dom gated@                                                      | @HiddenClock dom@                                                  |
+------------------------------------------------------------------------------+--------------------------------------------------------------------+
| @HiddenReset dom sync@                                                       | @HiddenReset dom@                                                  |
+------------------------------------------------------------------------------+--------------------------------------------------------------------+
| @Clock dom gated@                                                            | @Clock dom@                                                        |
+------------------------------------------------------------------------------+--------------------------------------------------------------------+
| @Reset dom sync@                                                             | @Reset dom@                                                        |
+------------------------------------------------------------------------------+--------------------------------------------------------------------+

* @outputVerifier@ now operates on two domains. If you only need one, simply
  change it to @outputVerifier'@

* For an overview of all other changes, check out <https://github.com/clash-lang/clash-compiler/blob/1.0/clash-ghc/CHANGELOG.md the changelog>

=== Examples

==== FIR filter

FIR filter in Clash 0.99:

@
module FIR where

import Clash.Prelude
import Clash.Explicit.Testbench

dotp :: SaturatingNum a
     => Vec (n + 1) a
     -> Vec (n + 1) a
     -> a
dotp as bs = fold boundedPlus (zipWith boundedMult as bs)

fir
  :: (Default a, KnownNat n, SaturatingNum a, HiddenClockReset domain gated synchronous)
  => Vec (n + 1) a -> Signal domain a -> Signal domain a
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs \<$\> bundle xs
    xs  = window x_t

topEntity
  :: Clock  System Source
  -> Reset  System Asynchronous
  -> Signal System (Signed 16)
  -> Signal System (Signed 16)
topEntity = exposeClockReset (fir (2:>3:>(-2):>8:>Nil))
{\-\# NOINLINE topEntity \#-\}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (2:>3:>(-2):>8:>Nil)
    expectedOutput = outputVerifier clk rst (4:>12:>1:>20:>Nil)
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not \<$\> done)
    rst            = systemResetGen
@

FIR filter in Clash 1.0:

@

module FIR where

import Clash.Prelude
import Clash.Explicit.Testbench

dotp :: SaturatingNum a
     => Vec (n + 1) a
     -> Vec (n + 1) a
     -> a
dotp as bs = fold boundedAdd (zipWith boundedMul as bs)

fir
  :: ( HiddenClockResetEnable dom
     , Default a
     , KnownNat n
     , SaturatingNum a
     , NFDataX a )
  => Vec (n + 1) a -> Signal dom a -> Signal dom a
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs \<$\> bundle xs
    xs  = window x_t

topEntity
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (Signed 16)
  -> Signal System (Signed 16)
topEntity = exposeClockResetEnable (fir (2:>3:>(-2):>8:>Nil))
{\-\# NOINLINE topEntity \#-\}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (2:>3:>(-2):>8:>Nil)
    expectedOutput = outputVerifier' clk rst (4:>12:>1:>20:>Nil)
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not \<$\> done)
    rst            = systemResetGen
@

==== Blinker circuit

Blinker circuit in Clash 0.99:

@
{\-\# LANGUAGE NoMonoLocalBinds \#-\}

module Blinker where

import Clash.Prelude
import Clash.Promoted.Symbol
import Clash.Intel.ClockGen

type Dom50 = Dom \"System\" 20000

{\-\# ANN topEntity
  (Synthesize
    { t_name   = \"blinker\"
    , t_inputs = [ PortName \"CLOCK_50\"
                 , PortName \"KEY0\"
                 , PortName \"KEY1\"
                 ]
    , t_output = PortName \"LED\"
    }) \#-\}
topEntity
  :: Clock Dom50 Source
  -> Reset Dom50 Asynchronous
  -> Signal Dom50 Bit
  -> Signal Dom50 (BitVector 8)
topEntity clk rst =
    exposeClockReset (mealy blinkerT (1,False,0) . isRising 1) pllOut rstSync
  where
    (pllOut,pllStable) = altpll \@Dom50 (SSymbol \@\"altpll50\") clk rst
    rstSync            = resetSynchronizer pllOut (unsafeToAsyncReset pllStable)

blinkerT (leds,mode,cntr) key1R = ((leds',mode',cntr'),leds)
  where
    -- clock frequency = 50e6  (50 MHz)
    -- led update rate = 333e-3 (every 333ms)
    cnt_max = 16650000 -- 50e6 * 333e-3

    cntr' | cntr == cnt_max = 0
          | otherwise       = cntr + 1

    mode' | key1R     = not mode
          | otherwise = mode

    leds' | cntr == 0 = if mode then complement leds
                                else rotateL leds 1
          | otherwise = leds
@

Blinker in Clash 1.0:

@
module Blinker where

import Clash.Prelude
import Clash.Intel.ClockGen

data LedMode
  = Rotate
  -- ^ After some period, rotate active led to the left
  | Complement
  -- ^ After some period, turn on all disable LEDs, and vice versa
  deriving (Generic, 'NFDataX')

-- Define a synthesis domain with a clock with a period of 20000 /ps/.
'createDomain' 'vSystem'{vName=\"Input\", vPeriod=20000}

-- Define a synthesis domain with a clock with a period of 50000 /ps/.
'createDomain' 'vSystem'{vName=\"Dom50\", vPeriod=50000}

{\-\# ANN topEntity
  ('Synthesize'
    { t_name   = \"blinker\"
    , t_inputs = [ PortName \"CLOCK_50\"
                 , PortName \"KEY0\"
                 , PortName \"KEY1\"
                 ]
    , t_output = PortName \"LED\"
    }) \#-\}
topEntity
  :: Clock Input
  -- ^ Incoming clock
  -> Signal Input Bool
  -- ^ Reset signal, straight from KEY0
  -> Signal Dom50 Bit
  -- ^ Mode choice, straight from KEY1. See \'LedMode\'.
  -> Signal Dom50 (BitVector 8)
  -- ^ Output containing 8 bits, corresponding to 8 LEDs
topEntity clk20 rstBtn modeBtn =
  exposeClockResetEnable
    (mealy blinkerT initialStateBlinkerT . isRising 1)
    clk50
    rstSync
    en
    modeBtn
 where
  -- | Enable line for subcomponents: we'll keep it always running
  en = enableGen

  -- Start with the first LED turned on, in rotate mode, with the counter on zero
  initialStateBlinkerT = (1, Rotate, 0)

  -- Signal coming from the reset button is low when pressed, and high when
  -- not pressed. We convert this signal to the polarity of our domain with
  -- 'unsafeFromLowPolarity'.
  rst = 'Clash.Signal.unsafeFromLowPolarity' rstBtn

  -- Instantiate a PLL: this stabilizes the incoming clock signal and indicates
  -- when the signal is stable. We're also using it to transform an incoming
  -- clock signal running at 20 MHz to a clock signal running at 50 MHz.
  (clk50, pllStable) =
    altpll
      \@Dom50
      (SSymbol \@\"altpll50\")
      clk20
      rst

  -- Synchronize reset to clock signal coming from PLL. We want the reset to
  -- remain active while the PLL is NOT stable, hence the conversion with
  -- 'unsafeFromLowPolarity'
  rstSync =
    'Clash.Signal.resetSynchronizer'
      clk50
      (unsafeFromLowPolarity pllStable)

flipMode :: LedMode -> LedMode
flipMode Rotate = Complement
flipMode Complement = Rotate

blinkerT
  :: (BitVector 8, LedMode, Index 16650001)
  -> Bool
  -> ((BitVector 8, LedMode, Index 16650001), BitVector 8)
blinkerT (leds, mode, cntr) key1R = ((leds', mode', cntr'), leds)
  where
    -- clock frequency = 50e6  (50 MHz)
    -- led update rate = 333e-3 (every 333ms)
    cnt_max = 16650000 :: Index 16650001 -- 50e6 * 333e-3

    cntr' | cntr == cnt_max = 0
          | otherwise       = cntr + 1

    mode' | key1R     = flipMode mode
          | otherwise = mode

    leds' | cntr == 0 =
              case mode of
                Rotate -> rotateL leds 1
                Complement -> complement leds
          | otherwise = leds
@
-}
