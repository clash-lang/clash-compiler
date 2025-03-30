{- |
Copyright  :  (C) 2025     , Martijn Bastiaan
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities for converting between Clash number types in a non-erroring way. Its
existence is motivated by the observation that Clash users often need to convert
between different number types (e.g., 'Clash.Sized.Unsigned.Unsigned' to
'Clash.Sized.Signed.Signed') and that it is not always clear how to do so
properly. Two classes are exported:

* 'NumConvert': for conversions that, based on types, are guaranteed to succeed.
* 'MaybeNumConvert': for conversions that may fail for some values.

As opposed to 'Prelude.fromIntegral', all conversions are translatable to
synthesizable HDL.

== __Relation to @convertible@__
Type classes exported here are similar to the @convertible@ package in that it
aims to facilitate conversions between different types. It is different in three
ways:

 1. It offers no partial functions.
 2. All its conversions are translatable to synthesizable HDL.
 3. It is focused on (Clash's) number types
-}
module Clash.Class.NumConvert (
  NumConvert (..),
  MaybeNumConvert (..),
) where

import Clash.Class.NumConvert.Internal.MaybeNumConvert
import Clash.Class.NumConvert.Internal.NumConvert
