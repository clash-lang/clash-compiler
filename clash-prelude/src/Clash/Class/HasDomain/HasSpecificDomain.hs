{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Internals for "Clash.Class.HasDomain"
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_HADDOCK not-home #-}

module Clash.Class.HasDomain.HasSpecificDomain where

import           Clash.Class.HasDomain.CodeGen  (mkHasDomainTuples)
import           Clash.Class.HasDomain.Common

import           Clash.Sized.Vector             (Vec)
import           Clash.Signal.Internal
  (Signal, Domain, Clock, Reset, Enable)
import           Clash.Signal.Delayed.Internal  (DSignal)

import           Data.Proxy                     (Proxy)
import           Data.Kind                      (Type)
import           Type.Errors
  (IfStuck, DelayError, Pure, ErrorMessage(ShowType))

type Outro =
         ""
   :$$$: "------"
   :$$$: ""
   :$$$: "You tried to apply an explicitly routed clock, reset, or enable line"
   :$$$: "to a construct with, possibly, an implicitly routed one. Clash failed to"
   :$$$: "unambigously link the given domain (by passing in a 'Clock', 'Reset', or"
   :$$$: "'Enable') to the component passed in."
   :$$$: ""

type NotFoundError (dom :: Domain) (t :: Type) =
       "Could not find domain '" :<<>>: 'ShowType dom :<<>>: "' in the following type:"
  :$$$: ""
  :$$$: "  " :<<>>: t
  :$$$: ""
  :$$$: "If that type contains that domain anyway, you might need to provide an"
  :$$$: "additional type instance of HasDomain. Example implementations:"
  :$$$: ""
  :$$$: " * type instance HasDomain dom  (MyVector n a)     = HasDomain dom a"
  :$$$: " * type instance HasDomain dom1 (MyCircuit dom2 a) = DomEq dom1 dom2"
  :$$$: " * type instance HasDomain dom1 (MyTuple a b)      = Merge dom a b"
  :$$$: ""
  :$$$: Outro

-- | Type that forces /dom/ to be present in /r/ at least once. Will resolve to
-- a type error if it doesn't. It will always fail if given /dom/ is completely
-- polymorphic and can't be tied to /r/ in any way.
type WithSpecificDomain dom r =
  (HasSpecificDomain dom r, dom ~ GetDomain dom r)

-- TODO: Extend HasDomainWrapperResult such that it keeps track of what it found /
-- TODO: which types are stuck, so that we can report better errors.
data HasDomainWrapperResult
  = NotFound
  -- ^ No domain found
  | Found
  -- ^ Found the specific domain caller was looking for

-- | Merge two 'HasDomainWrapperResult's according to the semantics of 'HasDomain.
type family MergeWorker (n :: HasDomainWrapperResult) (m :: HasDomainWrapperResult) :: HasDomainWrapperResult where
  MergeWorker 'Found b = 'Found
  MergeWorker a 'Found = 'Found
  MergeWorker 'NotFound 'NotFound = 'NotFound

type Merge (dom :: Domain) (n :: Type) (m :: Type) =
  MergeWorker (HasDomainWrapper dom n) (HasDomainWrapper dom m)

type family DomEqWorker (n :: Domain) (m :: Domain) :: HasDomainWrapperResult where
  DomEqWorker n n = 'Found
  DomEqWorker n m = 'NotFound

-- | Check domain for equality. Return @'Found@ if so, return @'NotFound@ if not.
-- The reason d'etre for this type family is that _open_ type families don't
-- allow overlapping types. We therefore defer equality checking to a closed
-- type family.
type DomEq (n :: Domain) (m :: Domain) =
  IfStuck (DomEqWorker n m) ('NotFound) (Pure (DomEqWorker n m))

-- | Type family that searches a type and checks whether a specific domain is
-- present. Will result in either "domain not found, and no others either",
-- "domain not found, but found another", or "found domain".
type family HasDomain (dom :: Domain) (n :: Type) :: HasDomainWrapperResult

type instance HasDomain dom1 (Proxy dom2)           = DomEq dom1 dom2
type instance HasDomain dom1 (Signal dom2 a)        = DomEq dom1 dom2
type instance HasDomain dom1 (DSignal dom2 delay a) = DomEq dom1 dom2
type instance HasDomain dom1 (Clock dom2)           = DomEq dom1 dom2
type instance HasDomain dom1 (Reset dom2)           = DomEq dom1 dom2
type instance HasDomain dom1 (Enable dom2)          = DomEq dom1 dom2
type instance HasDomain dom (Vec n a)               = HasDomain dom a
type instance HasDomain dom (a, b)                  = Merge dom a b
type instance HasDomain dom (a -> b)                = Merge dom a b

type family ErrOnNotFound (dom :: Domain) (n :: HasDomainWrapperResult) (t :: Type) :: Domain where
  ErrOnNotFound dom  'NotFound t = DelayError (NotFoundError dom t)
  ErrOnNotFound dom  'Found    t = dom

-- | Wrapper that checks for stuckness and returns @'NotFound@ if so
type family HasDomainWrapper (dom :: Domain) (n :: Type) :: HasDomainWrapperResult where
  HasDomainWrapper dom n =
    IfStuck
      (HasDomain dom n)
      ('NotFound)
      (Pure (HasDomain dom n))

-- | Helper function for HasSpecificDomain class (I don't really understand
-- why this one is necessary. HasDomainWrapper _should_ check for stuckness
-- and does so according to tests..
type family ResolveOrErr (dom :: Domain) (t :: Type) :: Domain where
  ResolveOrErr dom t =
    IfStuck
      (HasDomainWrapper dom t)
      (ErrOnNotFound dom 'NotFound t)
      (Pure (ErrOnNotFound dom (HasDomainWrapper dom t) t))

-- | Type class that specifies that a certain domain, /dom/, needs to be present
-- in some other type, /r/. This is used to disambiguate what hidden clock,
-- reset, and enable lines should be exposed in functions such as
-- 'Clash.Signal.withSpecificReset'.
--
-- Functions in need of this class should use 'WithSpecificDomain' though, to
-- force Clash to display an error instead of letting it silently pass.
class HasSpecificDomain (dom :: Domain) (r :: Type) where
  type GetDomain dom r :: Domain
  type GetDomain dom r = ResolveOrErr dom r

instance HasSpecificDomain dom a

mkHasDomainTuples ''HasDomain ''Merge
