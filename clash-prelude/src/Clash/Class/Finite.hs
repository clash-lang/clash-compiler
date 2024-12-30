{-|
Copyright  :  (C) 2024-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

The class of types holding only a finite number of elements. The
'Finite' class offers type level access to the number of elements @n@
and defines a total order on the elements via indexing them from @0@
to @n-1@. Therewith, it gives access to the vector of all inhabitants
of the type and allows to iterate over them in order or to map them back
and forth between their associated indices.

The class can be considered as a more hardware-friendly alternative to
'Bounded' and 'Enum', utilizing 'Clash.Sized.Index.Index' instead of
'Int' and vectors instead of lists.

In comparison, 'Finite' is well suited for types holding finitely many
elements, while the 'Enum' class is better suited for types with
infinitely many inhabitants. The type of @'succ', 'pred' :: 'Enum' a
=> a -> a@ clearly reflects this design choice, as it assumes that
every element has a successor and predecessor, which makes perfect
sense for infinite types, but requires finite types to error on
certain inputs. Wrapping behavior is forbidden according to the
documentation of 'Enum' (assuming that finite types usually have a
'Bounded' instance) such that 'Enum' instances must ship partial
functions for most finite types. Likewise, the number of inhabitants
does not align with the number of indices offered by 'Int' for most
types, which 'Finite' resolves by using @'Clash.Sized.Index.Index' n@
instead.
-}
module Clash.Class.Finite
  ( -- * Finite Class
    Finite(..)
    -- * Extensions
  , GenericReverse(..)
  , WithUndefined(..)
    -- * Deriving Helpers
  , FiniteDerive(..)
  , BoundedEnumEq(..)
  )
where

import Clash.Class.Finite.Internal
