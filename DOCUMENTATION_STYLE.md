This document tries to develop guidelines to have a clear and consistent
documentation style in our Haddock documentation. It is a work in progress, and
the existing documentation does not yet conform to this style throughout, though
ideally it should. A uniform style makes it easier for readers who have become
familiar with our documentation to look up and learn about the API.

*   Sentence fragments do not end in a period, unless part of a paragraph.
    Regular sentences end in a period. Examples:

    A sentence fragment

    A new paragraph with a sentence fragment

    This is a regular sentence in a third paragraph.

    Another sentence fragment. Yet another sentence fragment in the same
    paragraph.

    A sentence fragment. It is followed by a regular sentence.

*   Headings are sentence fragments with sentence case. Example:
    ```
    == Sapien suscipit tempus
    ```

*   We use a serial comma. So we write _this is called a serial comma, an_
    _Oxford comma, or Harvard comma_ rather than _this is called a serial_
    _comma, an Oxford comma or Harvard comma_.

*   Standard headings and phrases:

    ```
    === See also:

    === __Example__

    * __NB__: Not synthesizable

    * __NB__: Initial output value is /undefined/, reading it will throw an
    'Clash.XException.XException'

    => Clock dom
    -- ^ 'Clock' to synchronize to
    -> Reset dom
    -- ^ 'Reset' line
    -> Enable dom
    -- ^ 'Enable' line

    ```

    (note that if there is ample space in a Haddock page, examples need not be
    a collapsed section)

*   More generally, standard formattings:

    ```
    * __NB__: Donec sagittis quam eu mauris.
    * __NB__: Egestas. Duis sagittis fermentum nunc. Nullam.
    ```

*   The following is a list of common terms that indicates whether they are one
    word or two words and how they should be capitalized in documentation. Note
    that the capitalization rules for code are different, so a function
    implementing an asynchronous ROM is called `asyncRom` because abbreviations
    longer than two letters are not capitalized.

    * RAM
    * ROM
    * block RAM (when tight for space and the meaning is obvious from context,
      can be abbreviated to BRAM)
    * test bench
