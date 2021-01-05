.. _example_mac:

Example: Multiply and Accumulate
================================

Combinatorial MAC
-----------------

With Clash installed, it is now possible to begin creating hardware designs.
To give a brief overview of Clash, we will define a simple
*multiply-and-accumulate* circuit. Make a new file called ``MAC.hs``, and enter
the following preamble:

.. code-block:: haskell

  module MAC where

  import Clash.Prelude
  import Clash.Explicit.Testbench

This declares the module and imports some useful modules from the Clash
standard library. The standard library contains necessary functions and data
types for writing circuit descriptions. As with Haskell, module identifiers in
Clash must always start with a capital letter and correspond to the name of the
file.

The logic of our circuit is expressed as a function which takes an accumulator
and two extra inputs, and outputs the new value of the accumulator -- which is
the old value plus the product of the two other inputs.

.. code-block:: haskell

  mac :: (Num a) => a -> (a, a) -> a
  mac acc (x, y) = acc + x * y

The type of the function is given after the ``::``, and says that the type
``a`` is some numeric type (e.g. ``Int``, ``Signed 8``, ``Double``), the first
argument is a number, the second value is a pair of numbers, and the result is
a number.

Synchronous MAC
---------------

By adding another output parameter to this function, with the previous value of
the accumulator, we can define the function as a `Mealy machine`_. This allows
us to use our combinatorial definition of ``mac`` to create a synchronous
circuit (which we call ``macS``).

.. code-block:: haskell

  mac :: (Num a) => a -> (a, a) -> (a, a)
  mac acc (x, y) = (acc + x * y, acc)

  macS :: (HiddenClockResetEnable dom, Num a, NFDataX a) => Signal dom (a, a) -> Signal dom a
  macS = mealy mac 0

.. _`Mealy machine`: https://en.wikipedia.org/wiki/Mealy_machine

The input and output of ``macS`` are values of the ``Signal`` type. This type
represents synchronous values (functions without signals are combinatorial).
There is also an additional ``dom`` type, for synthesis domain, and a
constraint ``HiddenClockResetEnable`` -- which says the synthesis domain has a
clock, reset and enable line. These are implicit, although they can be exposed
using the ``exposeClockResetEnable`` funcion.

HDL Generation and Testing
--------------------------

To generate HDL from a synchronous circuit, a function needs to be marked as
a ``topEntity``. The simplest way to achieve this is to create a function with
this name, as Clash will use this definition automatically (similar to how
``main`` is a special function in other languages).

.. code-block:: haskell

  topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Int, Int)
    -> Signal System Int
  topEntity = exposeClockResetEnable macS

It is now possible to generate HDL for this circuit description, by running
either ``clash --HDL`` from the command line, or running ``:HDL`` in ``clashi``
(where ``HDL`` is either ``vhdl``, ``verilog`` or ``systemverilog``). This
will generate the HDL in a subdirectory named after the HDL being output.

.. warning::
  Any function used to generate HDL from must have a monomorphic type. This
  means there can be no type variables in the type signature (i.e. for the
  circuit defined so far you need to specify both ``dom`` and ``a``.

We can test that this circuit works as expected by defining a test bench. 
This allows an input to be used and the actual output to be compared against
an expected output.

.. code-block:: haskell

  testBench :: Signal System Bool
  testBench = done
   where
    testInput    = stimuliGenerator clk rst ((1,1) :> (2,2) :> (3,3) :> (4,4))
    expectOutput = outputVerifier' clk rst (0 :> 1 :> 5 :> 14 :> 30 :> 46 :> 62)
    done         = expectOutput (topEntity clk rst en testInput)
    en           = enableGen
    clk          = tbSystemClockGen (fmap not done)
    rst          = systemResetGen

From ``clashi`` it is possible to sample this test bench, using the ``sampleN``
function, which takes in the number of samples to draw and the signal which
generates samples.

.. code-block:: haskell
 
  >>> sampleN 8 testBench
  [False, False, False, False, False, False, False, False]

