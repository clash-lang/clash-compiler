{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Core.DataCon where

import Control.DeepSeq                  (NFData)
import GHC.Generics                     (Generic)
import Unbound.Generics.LocallyNameless (Alpha,Subst)

import {-# SOURCE #-} CLaSH.Core.Term   (Term)
import {-# SOURCE #-} CLaSH.Core.Type   (Type)

data DataCon

instance Eq      DataCon
instance Ord     DataCon
instance Generic DataCon
instance Show    DataCon
instance Alpha   DataCon
instance Subst   Type DataCon
instance Subst   Term DataCon
instance NFData  DataCon
