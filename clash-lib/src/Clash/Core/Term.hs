{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Term representation in the CoreHW language: System F + LetRec + Case
-}

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Clash.Core.Term
  ( Term (..)
  , TmName
  , LetBinding
  , Pat (..)
  , Alt
  )
where

-- External Modules
import Control.DeepSeq
import Data.Binary                             (Binary)
import Data.Hashable                           (Hashable)
import Data.Text                               (Text)
import GHC.Generics

-- Internal Modules
import Clash.Core.DataCon                      (DataCon)
import Clash.Core.Literal                      (Literal)
import Clash.Core.Name                         (Name (..))
import {-# SOURCE #-} Clash.Core.Type          (Type)
import Clash.Core.Var                          (Id, TyVar)

-- | Term representation in the CoreHW language: System F + LetRec + Case
data Term
  = Var     !Id                             -- ^ Variable reference
  | Data    !DataCon                        -- ^ Datatype constructor
  | Literal !Literal                        -- ^ Literal
  | Prim    !Text !Type                     -- ^ Primitive
  | Lam     !Id Term                        -- ^ Term-abstraction
  | TyLam   !TyVar Term                     -- ^ Type-abstraction
  | App     !Term !Term                     -- ^ Application
  | TyApp   !Term !Type                     -- ^ Type-application
  | Letrec  [LetBinding] Term               -- ^ Recursive let-binding
  | Case    !Term !Type [Alt]               -- ^ Case-expression: subject, type of
                                            -- alternatives, list of alternatives
  | Cast    !Term !Type !Type               -- ^ Cast a term from one type to another
  deriving (Show,Generic,NFData,Hashable,Binary)

-- | Term reference
type TmName     = Name Term
-- | Binding in a LetRec construct
type LetBinding = (Id, Term)

-- | Patterns in the LHS of a case-decomposition
data Pat
  = DataPat !DataCon [TyVar] [Id]
  -- ^ Datatype pattern, '[TyVar]' bind existentially-quantified
  -- type-variables of a DataCon
  | LitPat  !Literal
  -- ^ Literal pattern
  | DefaultPat
  -- ^ Default pattern
  deriving (Eq,Ord,Show,Generic,NFData,Hashable,Binary)

type Alt = (Pat,Term)

