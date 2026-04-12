module T3185.Types where

import Clash.Prelude hiding (Ordering(..))

import Clash.Annotations.BitRepresentation.Deriving
  ( deriveAnnotation, simpleDerivator, ConstructorType(OneHot), FieldsType(Wide)
  , deriveBitPack )

data Ordering
  = EQ
  | LT
  | GT
  | NaN
  deriving (Generic, NFDataX, Eq, ShowX, Show, Lift)
deriveAnnotation (simpleDerivator OneHot Wide) [t| Ordering |]
deriveBitPack [t| Ordering |]
