{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Class.HasDomain.Common
  ( Unlines
  , (:<<>>:)
  , (:$$$:)
  , (:++:)
  ) where

import           GHC.TypeLits               (Symbol)
import           Type.Errors
  (ErrorMessage(Text, ShowType, (:<>:), (:$$:)))

type family ToEM (k :: t) :: ErrorMessage where
  ToEM (k :: Symbol)       = 'Text k
  ToEM (k :: ErrorMessage) = k
  ToEM (k :: t)            = 'ShowType k

infixl 5 :<<>>:
type (:<<>>:) (k1 :: t1) (k2 :: t2) = ToEM k1 ':<>: ToEM k2

infixl 4 :$$$:
type (:$$$:) (k1 :: t1) (k2 :: t2) = ToEM k1 ':$$: ToEM k2


{- | Combine multiple lines with line break. Type-level version of the @unlines@
function but for ErrorMessage. -}
type family Unlines (ln :: [k]) :: ErrorMessage where
  Unlines '[] = 'Text ""
  Unlines ((x :: Symbol) ': xs) = 'Text x ':$$: Unlines xs
  Unlines ((x :: ErrorMessage) ': xs) = x ':$$: Unlines xs

infixl 4 :++:
type family (:++:) (as :: [k]) (bs :: [k]) :: [k] where
  (:++:) a '[] = a
  (:++:) '[] b = b
  (:++:) (a ': as) bs = a ': (as :++: bs)
