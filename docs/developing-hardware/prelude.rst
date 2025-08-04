.. _prelude:

Clash Prelude
=============

Basic Types
-----------

The Clash prelude includes many different numeric types, which are used to
safely define other types / functions. These include, but may not be limited to

- Type level natural numbers (``Nat``), which allow numbers to be used in
  types. Conceptually, this is similar to *const generics* in *C++*.

  It is possible to have term level values which refer to a type level number.
  This is called ``SNat n`` (for *singleton natural number*). These are defined
  up to 1024 with the prefix "d" (e.g. ``d256``).

- ``Unsigned n`` and ``Signed n`` numbers with an arbitrary width (given as a
  type level natural number). These allow fixed-width arithmetic to be used on
  arbitrary numbers.

- ``Index n`` provides natural numbers up to an arbitrary value (given as a
  type level natural number). These allow indexing into fixed width structures
  like ``Vec n a``.

Another commonly used type is ``BitVector n``. This provides a fixed size
vector of ``Bit`` values which can be indexed, and used to perform *unsigned
integer arithmetic*. Any type that can be marshalled to / from a ``BitVector
n`` implements the ``BitPack`` class, which defines the conversion.

.. note:: It is also possible to derive instances of ``BitPack`` using
  ``Generic``, by writing ``deriving (Generic, BitPack)`` in the type
  definition. This automatically determines how to do the conversion at
  compile-time.

More generally, there is a ``Vec n a`` type which allows collections of
arbitrary values to be used. These vectors are tagged with their length, to
prevent out of bounds access at compile-time.

Synthesis Domains
-----------------

Synchronous circuits have a synthesis domain, which determines the behaviour
of things which can affect signals in the domain. Domains consist of

- a name, which uniquely refers to the domain
- the clock period in ps
- the active edge of the clock
- whether resets are synchronous (edge-sensitive) or not
- whether the initial (power up) behaviour is defined
- whether resets are high or low polarity

The prelude provides some common domains, namely ``XilinxSystem`` and
``IntelSystem`` for the standard configurations of each vendor. There is also
a generic domain, ``System``, which can be used for vendor-agnostic purposes
(i.e. writing a generic test bench). It is possible to define new synthesis
domains for custom hardware using the ``createDomain`` function, which also
defines the necessary instances for domains.

A value in a synchronous circuit is wrapped in the ``Signal dom a`` type, which
specifies the synthesis domain and the type of value. Any function which needs
access to a domain can use the constraints ``HasDomain`` (to find it's domain)
or ``KnownDomain`` (to extract configuration).

The default API exposed by the prelude is implicit with regards to clocks,
reset lines and enable lines -- as these can be determined at compile time.
However, if they are needed the ``Clash.Explicit`` module contains explicit
versions of the API which expose these directly in function arguments. It is
also possible to use functions like ``exposeClockResetEnable`` to turn an
implicitly defined function to an explicitly defined function.

State Machines
--------------

The Clash prelude contains combinators for two classical finite state machines
which can be used to define synchronous circuits. The first of these is
``mealy``, which encodes a `Mealy machine`_. This is a machine specified by

- A transfer function of type ``state -> input -> (state, output)``
- An initial state
- An input signal which can change at each cycle

.. note:: The Mealy machine is similar to the State monad, which Haskell
  programmers may already be familar with. Practically speaking, the only
  difference is that this machine also has an input signal which is changed
  externally to the definition of the machine.

.. _`Mealy machine`: https://en.wikipedia.org/wiki/Mealy_machine

It is also possible to define a `Moore machine`_ using the ``moore`` function
in the Clash prelude. This differs to the Mealy machine by providing output
based on the previous state (as oppoesd to the newly calculated state), and is
specified by

- A transfer function of type ``state -> input -> state``
- An output function of type ``state -> output``
- An initial state
- An input signal which can change at each cycle

.. _`Moore machine`: https://en.wikipedia.org/wiki/Moore_machine

Sometimes, there may be multiple inputs / outputs needed for a machine. As
machines only input and output a single signal, there is a way to combine and
separate multiple signals. The ``Bundle`` class specifies how to convert
between some type which is a signal of a product, and some type which is a
product of signals, e.g.

.. code-block:: haskell

  bundle   :: (Signal dom a, Signal dom b) -> Signal dom (a, b)
  unbundle :: Signal dom (a, b) -> (Signal dom a, Signal dom b)

There are combinators which can automatically perform this bundling and
unbundling for you as required, called ``mealyB`` and ``mooreB``. The
``Bundle`` class is already defined for many types, including tuples (up to
62 elements), ``Maybe a``, ``Either a b`` and ``Vec n a``.

RAM and ROM
-----------

The Clash prelude provides the ability to work with synchronous and
asynchronous ROM, asynchronous RAM and synchronous Block RAM. The simplest of
these are ROM, which only allow indexing into a ``Vec n a`` of elements. ROM
is defined using the functions in ``Clash.Prelude.ROM``.

RAM is more complex, as it allows both reading and writing. The function to
define a RAM takes in a signal for the address to read, and a signal for an
optional address to update (bundled with the new value). At each cycle it
outputs the value of the memory address read in the previous cycle.
Asynchronous RAM is defined in ``Clash.Prelude.RAM``.

An FPGA may include a block RAM, which is a larger memory structure and more
suitable for some applications. Block RAM also has a synchronous read port,
allowing memory access to be synchronized to a clock. Block RAM is used the
same way as async RAM, allowing the two to be compared quickly. Block RAM is
defined in ``Clash.Prelude.BlockRam``.

Undefined Values
----------------

When working with hardware designs, there are times when undefined values may
be encountered in simulation. Clash provides a custom exception type,
``XException``, for cases when an undefined value is encountered. There are
also many utility functions for working with exceptions, such as

- ``errorX``, which throws an ``XException``
- ``isX`` and ``hasX``, which check for ``XExceptions`` when evaluating
- ``maybeIsX`` and ``maybeHasX``, which discard inforamtion about exceptions

There are also implementations of typical classes in Haskell which have been
changed to work with undefined values. Currently these are

- ``ShowX``, which works like the ``Show`` class in Haskell. When an undefined
  value is encountered an "X" is printed. ``Show`` can still be used, but will
  throw an exception if an undefined value is encountered.

- ``NFDataX``, which works like the ``NFData`` class in the ``deepseq``
  library. This allows evaluating values to normal form in code when undefined
  may be present. ``NFData`` can still be used, but will bubble up exceptions
  if undefined is encountered.
