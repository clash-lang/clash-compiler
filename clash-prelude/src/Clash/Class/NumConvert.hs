{- |
Copyright  :  (C) 2025     , Martijn Bastiaan
                  2026     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities for converting between Clash number types in a non-erroring way. Its
existence is motivated by the observation that Clash users often need to convert
between different number types (e.g., @Unsigned@ to @Signed@) and that it is not
always clear how to do so properly. Two classes are exported:

* 'NumConvert': for conversions that, based on types, are guaranteed to succeed.
* 'MaybeNumConvert': for conversions that may fail for some values.

As opposed to 'Prelude.fromIntegral', no conversion silently changes the value.

== __Choosing a conversion__
Clash offers several ways to convert between number types such as
'Clash.Sized.BitVector.BitVector', 'Clash.Sized.Unsigned.Unsigned',
'Clash.Sized.Signed.Signed', 'Clash.Sized.Index.Index', 'Int' and 'Data.Word.Word8'.
Which one to use depends on what should happen to the value.

=== Keep the value, and the target type can hold every value of the source type
Use 'numConvert'. If the target type cannot represent every value of the source
type, the conversion is rejected at compile time.

>>> numConvert (3 :: Index 8) :: Unsigned 3
3
>>> numConvert (200 :: Unsigned 8) :: Signed 9
200
>>> numConvert (-3 :: Signed 4) :: Int
-3

=== Keep the value, but the target type might not hold it
Use 'maybeNumConvert'. It returns 'Nothing' for values the target type cannot
represent.

>>> maybeNumConvert (-3 :: Signed 8) :: Maybe (Unsigned 8)
Nothing
>>> maybeNumConvert (100 :: Unsigned 8) :: Maybe (Index 128)
Just 100

=== Change the width, keep the type
Use 'Clash.Class.Resize.resize', 'Clash.Class.Resize.zeroExtend',
'Clash.Class.Resize.signExtend' or 'Clash.Class.Resize.truncateB' from the
'Clash.Class.Resize.Resize' class. Making a number narrower with
'Clash.Class.Resize.truncateB' drops its most significant bits.
'Clash.Class.Resize.resize' does the same, except for signed numbers: it keeps
the sign bit and drops the bits next to it. 'Clash.Sized.Index.Index' is an
exception: a value that does not fit results in an
'Clash.XException.XException'.

>>> resize (-3 :: Signed 4) :: Signed 8
-3
>>> truncateB (300 :: Unsigned 16) :: Unsigned 8
44
>>> resize (7 :: Signed 4) :: Signed 3
3
>>> truncateB (7 :: Signed 4) :: Signed 3
-1

If you know a value fits, use 'Clash.Class.Resize.checkedResize' or
'Clash.Class.Resize.checkedTruncateB' to have simulation check that assumption.
To handle values that do not fit, use 'maybeNumConvert'.

=== Keep the bits, change the type
Use 'Clash.Class.BitPack.bitCoerce' to convert between types of the same bit
width, or 'Clash.Class.BitPack.pack' and 'Clash.Class.BitPack.unpack' to convert
to and from 'Clash.Sized.BitVector.BitVector'. These keep the bit pattern, not
the value: a negative 'Clash.Sized.Signed.Signed' number becomes a large
'Clash.Sized.Unsigned.Unsigned' one.

>>> bitCoerce (-5 :: Signed 6) :: Unsigned 6
59
>>> pack (5 :: Unsigned 4)
0b0101

Not every bit pattern represents an 'Clash.Sized.Index.Index'. Use
'Clash.Class.BitPack.maybeUnpack' to reject such patterns, or
'Clash.Sized.Index.bv2i' to pick an 'Clash.Sized.Index.Index' that can hold all
of them:

>>> maybeUnpack (12 :: BitVector 4) :: Maybe (Index 10)
Nothing
>>> bv2i (12 :: BitVector 4)
12

=== Change the type and drop bits
Combine the conversions above. For example, to keep the 8 least significant bits
of a @Signed 16@ as an @Unsigned 8@:

>>> truncateB (bitCoerce (-1 :: Signed 16) :: Unsigned 16) :: Unsigned 8
255

=== Why not 'fromIntegral'?
'fromIntegral' converts through 'Integer', which has a fixed width in HDL (64
bits by default, see @-fclash-intwidth@). Bits above that width are lost. It
also hides what happens to values the target type cannot hold:
'Clash.Sized.Unsigned.Unsigned' and 'Clash.Sized.Signed.Signed' wrap around,
while 'Clash.Sized.Index.Index' results in an 'Clash.XException.XException'.
The functions above make that choice explicit.

== __Relation to @convertible@__
Type classes exported here are similar to the @convertible@ package in that it
aims to facilitate conversions between different types. It is different in three
ways:

 1. It offers no partial functions
 2. All its conversions are translatable to synthesizable HDL
 3. It is focused on (Clash's) number types

== __Implementing 'NumConvert' for your own type__
If you want to implement 'NumConvert' and 'MaybeNumConvert' for your own type,
see 'NumConvertCanonical'.
-}
module Clash.Class.NumConvert (
  NumConvert,
  numConvert,
  MaybeNumConvert,
  maybeNumConvert,

  -- * For implementers only
  NumConvertCanonical(..),
  MaybeNumConvertCanonical(..),
  Canonical,
) where

import Clash.Class.NumConvert.Internal.MaybeNumConvert
import Clash.Class.NumConvert.Internal.NumConvert
import Clash.Class.NumConvert.Internal.Canonical

{- $setup
>>> :set -fplugin GHC.TypeLits.Extra.Solver
>>> :set -fplugin GHC.TypeLits.KnownNat.Solver
>>> :set -fplugin GHC.TypeLits.Normalise
>>> import Clash.Prelude
>>> import Clash.Class.NumConvert
-}
