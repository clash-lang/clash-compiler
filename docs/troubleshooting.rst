.. _troubleshooting:

===============
Troubleshooting
===============

Some errors that occur when using Clash are fairly frequent when getting
started, and are cataloged here.

----

**Type error: Couldn't match expected type** ``Signal (a,b)`` **with actual
type** ``(Signal a, Signal b)``

Signals of product types and product types (to which tuples belong) of signals
are **isomorphic** due to synchronisity principle, but are not (structurally)
equal. Use the ``bundle`` function to convert from a product type to the signal
type. So if your code which gives the error looks like:

.. code:: haskell

    z = f a b (c,d)

add the ``bundle`` function like so:

.. code:: haskell

    z = f a b (bundle (c,d))

Product types supported by ``bundle`` are:

* All tuple types, up-to and including 8-tuples.
* The ``Vec`` type, available in ``Clash.Sized.Vector``.

----

**Clash.Normalize.Transformations(155): InlineNonRep: <FUNCTION> already inlined
100 times in:<FUNCTION>, <TYPE>**

You left a ``TopEntity`` in your design with a polymorphic or higher order type.
Use ``:t <name>`` inside ``clashi`` in order to check if the type is truly
polymorphic and/or higher order. If it is, add a type signature, and supply all
the higher order arguments.
