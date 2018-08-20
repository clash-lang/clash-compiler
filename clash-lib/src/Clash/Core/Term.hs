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
  , TmOccName
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
import Unbound.Generics.LocallyNameless        hiding (Name)
import Unbound.Generics.LocallyNameless.Extra  ()

-- Internal Modules
import Clash.Core.DataCon                      (DataCon)
import Clash.Core.Literal                      (Literal)
import Clash.Core.Name                         (Name (..), OccName)
import {-# SOURCE #-} Clash.Core.Type          (Type)
import Clash.Core.Var                          (Id, TyVar)

-- | Term representation in the CoreHW language: System F + LetRec + Case
data Term
  = Var     !Type !TmName                   -- ^ Variable reference
  | Data    !DataCon                        -- ^ Datatype constructor
  | Literal !Literal                        -- ^ Literal
  | Prim    !Text !Type                     -- ^ Primitive
  | Lam     !(Bind Id Term)                 -- ^ Term-abstraction
  | TyLam   !(Bind TyVar Term)              -- ^ Type-abstraction
  | App     !Term !Term                     -- ^ Application
  | TyApp   !Term !Type                     -- ^ Type-application
  | Letrec  !(Bind (Rec [LetBinding]) Term) -- ^ Recursive let-binding
  | Case    !Term !Type [Alt]               -- ^ Case-expression: subject, type of
                                            -- alternatives, list of alternatives
  | Cast    !Term !Type !Type               -- ^ Cast a term from one type to another
  deriving (Show,Generic,NFData,Hashable,Binary)

-- | Term reference
type TmName     = Name Term
type TmOccName  = OccName Term
-- | Binding in a LetRec construct
type LetBinding = (Id, Embed Term)

-- | Patterns in the LHS of a case-decomposition
data Pat
  = DataPat !(Embed DataCon) !(Rebind [TyVar] [Id])
  -- ^ Datatype pattern, '[TyVar]' bind existentially-quantified
  -- type-variables of a DataCon
  | LitPat  !(Embed Literal)
  -- ^ Literal pattern
  | DefaultPat
  -- ^ Default pattern
  deriving (Eq,Show,Generic,NFData,Alpha,Hashable,Binary)

type Alt = Bind Pat Term

instance Eq Term where
  (==) = aeq

instance Ord Term where
  compare = acompare

instance Alpha Term where
  aeq' c (Var _ n)   (Var _ m)   = aeq' c n m
  aeq' _ (Prim t1 _) (Prim t2 _) = t1 == t2
  aeq' c t1          t2          = gaeq c (from t1) (from t2)

  acompare' c (Var _ n)   (Var _ m)   = acompare' c n m
  acompare' _ (Prim t1 _) (Prim t2 _) = compare t1 t2
  acompare' c t1          t2          = gacompare c (from t1) (from t2)

instance Subst Type Pat
instance Subst Term Pat

instance Subst Term Term where
  isvar (Var _ x) = Just (SubstName (nameOcc x))
  isvar _         = Nothing

instance Subst Type Term
