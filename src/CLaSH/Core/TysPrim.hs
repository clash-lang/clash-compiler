module CLaSH.Core.TysPrim where

-- External Modules
import {-# SOURCE #-} CLaSH.Core.TyCon (TyCon,TyConName,PrimRep(..),pcPrimTyCon0)
import Unbound.LocallyNameless         (makeName)

-- GHC API

import PrelNames (intPrimTyConKey,addrPrimTyConKey)
import Unique    (getKey)

intPrimTyConName, addrPrimTyConName :: TyConName
intPrimTyConName  = makeName "#Int"  (toInteger . getKey $ intPrimTyConKey)
addrPrimTyConName = makeName "#Addr" (toInteger . getKey $ addrPrimTyConKey)

intPrimTyCon :: TyCon
intPrimTyCon  = pcPrimTyCon0 intPrimTyConName IntRep

addrPrimTyCon :: TyCon
addrPrimTyCon = pcPrimTyCon0 addrPrimTyConName AddrRep


