{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Term representation in the CoreHW language: System F + LetRec + Case
module CLaSH.Core.Term
  ( Term (..)
  , TmName
  , LetBinding
  , Pat (..)
  )
where

-- External Modules
import Control.DeepSeq
import Data.Text                               (Text)
import Data.Typeable
import GHC.Generics
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Extra  ()

-- Internal Modules
import CLaSH.Core.DataCon                      (DataCon)
import CLaSH.Core.Literal                      (Literal)
import {-# SOURCE #-} CLaSH.Core.Type          (Type)
import CLaSH.Core.Var                          (Id, TyVar)

-- | Term representation in the CoreHW language: System F + LetRec + Case
data Term
  = Var     Type TmName                    -- ^ Variable reference
  | Data    DataCon                        -- ^ Datatype constructor
  | Literal Literal                        -- ^ Literal
  | Prim    Text Type                      -- ^ Primitive
  | Lam     (Bind Id Term)                 -- ^ Term-abstraction
  | TyLam   (Bind TyVar Term)              -- ^ Type-abstraction
  | App     Term Term                      -- ^ Application
  | TyApp   Term Type                      -- ^ Type-application
  | Letrec  (Bind (Rec [LetBinding]) Term) -- ^ Recursive let-binding
  | Case    Term Type [Bind Pat Term]      -- ^ Case-expression: subject, type of
                                           -- alternatives, list of alternatives
  deriving (Show,Typeable,Generic,NFData)

-- | Term reference
type TmName     = Name Term
-- | Binding in a LetRec construct
type LetBinding = (Id, Embed Term)

-- | Patterns in the LHS of a case-decomposition
data Pat
  = DataPat (Embed DataCon) (Rebind [TyVar] [Id])
  -- ^ Datatype pattern, '[TyVar]' bind existentially-quantified
  -- type-variables of a DataCon
  | LitPat  (Embed Literal)
  -- ^ Literal pattern
  | DefaultPat
  -- ^ Default pattern
  deriving (Show,Typeable,Generic,NFData)

instance Eq Term where
  (==) = aeq

instance Ord Term where
  compare = acompare

instance Alpha Term where
  fvAny' c nfn (Var t n)  = fmap (Var t) $ fvAny' c nfn n
  fvAny' c nfn t          = fmap to . gfvAny c nfn $ from t

  aeq' c (Var _ n)   (Var _ m)   = aeq' c n m
  aeq' _ (Prim t1 _) (Prim t2 _) = t1 == t2
  aeq' c t1          t2          = gaeq c (from t1) (from t2)

  acompare' c (Var _ n)   (Var _ m)   = acompare' c n m
  acompare' _ (Prim t1 _) (Prim t2 _) = compare t1 t2
  acompare' c t1          t2          = gacompare c (from t1) (from t2)

instance Alpha Pat

instance Subst Type Pat
instance Subst Term Pat

instance Subst Term Term where
  isvar (Var _ x) = Just (SubstName x)
  isvar _         = Nothing

instance Subst Type Term
