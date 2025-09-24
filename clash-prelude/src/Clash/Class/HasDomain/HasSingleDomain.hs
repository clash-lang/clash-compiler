{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
                  2022-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Internals for "Clash.Class.HasDomain"
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-methods -Wno-deprecations #-}
{-# OPTIONS_HADDOCK not-home #-}

module Clash.Class.HasDomain.HasSingleDomain {-# DEPRECATED "Experimental feature multiple hidden has been removed. This module will therefore be removed in Clash 1.12." #-} where

import           Clash.Class.HasDomain.Common
import           Clash.Class.HasDomain.CodeGen    (mkTryDomainTuples)

import           Clash.Sized.Vector               (Vec)
import           Clash.Sized.RTree                (RTree)
import           Clash.Sized.Index                (Index)
import           Clash.Sized.Unsigned             (Unsigned)
import           Clash.Sized.Signed               (Signed)
import           Clash.Sized.BitVector            (BitVector, Bit)
import           Clash.Sized.Fixed                (Fixed)

import           Clash.Signal.Internal
  (Signal, Domain, Clock, Reset, Enable)
import           Clash.Signal.Delayed.Internal    (DSignal)

import           Numeric.Natural                  (Natural)

import           Data.Kind                        (Type)
import           Data.Proxy                       (Proxy)
import           GHC.TypeLits                     (type (+))
import           Type.Errors
  (DelayError, TypeError, IfStuck, Pure)

type MissingInstance =
        "This might happen if an instance for TryDomain is missing. Try to determine"
  :$$$: "which of the types miss an instance, and add them. Example implementations:"
  :$$$: ""
  :$$$: " * type instance TryDomain t (MyVector n a)    = TryDomain t a"
  :$$$: " * type instance TryDomain t (MyCircuit dom a) = Found dom"
  :$$$: " * type instance TryDomain t Terminal          = NotFound"
  :$$$: ""
  :$$$: "Alternatively, use one of the withSpecific* functions."

type Outro =
         ""
   :$$$: "------"
   :$$$: ""
   :$$$: "You tried to apply an explicitly routed clock, reset, or enable line"
   :$$$: "to a construct with, possibly, an implicitly routed one. Clash failed to"
   :$$$: "unambigously determine a single domain and could therefore not route it."
   :$$$: "You possibly used one of these sets of functions:"
   :$$$: ""
   :$$$: " * with{ClockResetEnable,Clock,Reset,Enable}"
   :$$$: " * expose{ClockResetEnable,Clock,Reset,Enable}"
   :$$$: ""
   :$$$: "These functions are suitable for components defined over a single domain"
   :$$$: "only. If you want to use multiple domains, use the following instead:"
   :$$$: ""
   :$$$: " * withSpecific{ClockResetEnable,Clock,Reset,Enable}"
   :$$$: " * exposeSpecific{ClockResetEnable,Clock,Reset,Enable}"
   :$$$: ""

type NotFoundError (t :: Type) =
       "Could not find a non-ambiguous domain in the following type:"
  :$$$: ""
  :$$$: "  " :<<>>: t
  :$$$: ""
  :$$$: MissingInstance
  :$$$: Outro

type AmbiguousError (t :: Type) (dom1 :: Domain) (dom2 :: Domain) =
        "Could not determine that the domain '" :<<>>: dom1 :<<>>: "'"
  :$$$: "was equal to the domain '" :<<>>: dom2 :<<>>: "' in the type:"
  :$$$: ""
  :$$$: "  " :<<>>: t
  :$$$: ""
  :$$$: "This is usually resolved by adding explicit type signatures."
  :$$$: Outro

type StuckErrorMsg (orig :: Type) (n :: Type) =
        "Could not determine whether the following type contained a non-ambiguous domain:"
  :$$$: ""
  :$$$: "  " :<<>>: n
  :$$$: ""
  :$$$: "In the full type:"
  :$$$: ""
  :$$$: "  " :<<>>: orig
  :$$$: ""
  :$$$: "Does it contain one?"
  :$$$: ""
  :$$$: "------"
  :$$$: ""
  :$$$: MissingInstance
  :$$$: Outro

-- | Type that forces /dom/ to be the same in all subtypes of /r/ that might
-- contain a domain. If given a polymorphic domain not tied to /r/, GHC will
-- be allowed to infer that that domain is equal to the one in /r/ on the
-- condition that /r/ contains just a single domain.
type WithSingleDomain dom r =
  (HasSingleDomain r, dom ~ GetDomain r)

data TryDomainResult
  = NotFound
  | Ambiguous Domain Domain
  | Found Domain

-- | Type family to resolve type conflicts (if any)
type family Merge' (n :: TryDomainResult) (m :: TryDomainResult) :: TryDomainResult where
  Merge' 'NotFound              b                      = b
  Merge' ('Ambiguous dom1 dom2) b                      = 'Ambiguous dom1 dom2
  Merge' a                      'NotFound              = a
  Merge' a                      ('Ambiguous dom1 dom2) = 'Ambiguous dom1 dom2
  Merge' ('Found dom)           ('Found dom)           = 'Found dom
  Merge' ('Found dom1)          ('Found dom2)          = 'Ambiguous dom1 dom2

-- | Same as Merge', but will insert a type error if Merge' got stuck.
type family Merge (orig :: Type) (n :: Type) (m :: Type) :: TryDomainResult where
  Merge orig n m =
    IfStuck
      (TryDomain orig n)
      (DelayError (StuckErrorMsg orig n))
      (Pure
        (IfStuck
          (TryDomain orig m)
          (DelayError (StuckErrorMsg orig m))
          (Pure (Merge' (TryDomain orig n) (TryDomain orig m)))
         ))

type family ErrOnConflict (t :: Type) (n :: TryDomainResult) :: Domain where
  ErrOnConflict t 'NotFound              = TypeError (NotFoundError t)
  ErrOnConflict t ('Ambiguous dom1 dom2) = TypeError (AmbiguousError t dom1 dom2)
  ErrOnConflict t ('Found dom)           = dom

type family TryDomain (orig :: Type) (n :: Type) :: TryDomainResult

type instance TryDomain t (DSignal dom delay a) = 'Found dom
type instance TryDomain t (Signal dom a)        = 'Found dom
type instance TryDomain t (Clock dom)           = 'Found dom
type instance TryDomain t (Reset dom)           = 'Found dom
type instance TryDomain t (Enable dom)          = 'Found dom
type instance TryDomain t (Proxy dom)           = 'Found dom
type instance TryDomain t (Vec n a)             = TryDomain t a
type instance TryDomain t (RTree d a)           = TryDomain t a
type instance TryDomain t (Index n)             = 'NotFound
type instance TryDomain t (Unsigned n)          = 'NotFound
type instance TryDomain t (Signed n)            = 'NotFound
type instance TryDomain t (BitVector n)         = 'NotFound
type instance TryDomain t Bit                   = 'NotFound
type instance TryDomain t (Fixed a n m)         = TryDomain t (a (n + m))
type instance TryDomain t (a -> b)              = Merge t a b
type instance TryDomain t (a, b)                = Merge t a b

type instance TryDomain t ()                    = 'NotFound
type instance TryDomain t Bool                  = 'NotFound
type instance TryDomain t Integer               = 'NotFound
type instance TryDomain t Natural               = 'NotFound
type instance TryDomain t Int                   = 'NotFound
type instance TryDomain t Float                 = 'NotFound
type instance TryDomain t Double                = 'NotFound
type instance TryDomain t (Maybe a)             = TryDomain t a
type instance TryDomain t (Either a b)          = Merge t a b

-- | Type family that searches a type and checks whether all subtypes that can
-- contain a domain (for example, Signal) contain the /same/ domain. Its
-- associated type, GetDomain, will yield a type error if that doesn't hold OR
-- if it can't check it.
class HasSingleDomain (r :: Type) where
  type GetDomain r :: Domain
  type GetDomain r =
    -- Handle types not in TryDomain type family
    IfStuck
      (TryDomain r r)
      (DelayError (StuckErrorMsg r r))
      (Pure (ErrOnConflict r (TryDomain r r)))

instance HasSingleDomain a

mkTryDomainTuples ''TryDomain ''Merge
