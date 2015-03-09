{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Term representation in the CoreHW language: System F + LetRec + Case
module CLaSH.Core.Term
  ( Term (..)
  , TmName
  , LetBinding
  , Pat (..)
  )
where

-- External Modules
import                Control.DeepSeq
import                Data.Text                     (Text)
import                Data.Typeable
import                GHC.Generics
import                Unbound.Generics.LocallyNameless
import                Unbound.Generics.LocallyNameless.Name   (Name(..))
import                Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

-- Internal Modules
import                CLaSH.Core.DataCon            (DataCon)
import                CLaSH.Core.Literal            (Literal)
import {-# SOURCE #-} CLaSH.Core.Type               (Type)
import                CLaSH.Core.Var                (Id, TyVar)
import                CLaSH.Util

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
  deriving (Show,Typeable,Generic)

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
  deriving (Show)

instance Alpha Text

instance Eq Term where
  (==) = aeq

-- instance Ord Term where
--   compare = acompare

instance Alpha Term where
  fvAny' c (Var _ n)  = fvAny' c n
  fvAny' c t          = fvR1 rep1 c t

  aeq' c (Var _ n)   (Var _ m)   = aeq' c n m
  aeq' _ (Prim t1 _) (Prim t2 _) = t1 == t2
  aeq' c t1          t2          = aeqR1 rep1 c t1 t2

instance Alpha Pat

instance Subst Term Pat
instance Subst Term Term where
  isvar (Var _ x) = Just (SubstName x)
  isvar _         = Nothing

instance Subst Type Pat
instance Subst Type Term where
  subst tvN u x | isFreeName tvN = case x of
    Lam    b         -> Lam    (subst tvN u b  )
    TyLam  b         -> TyLam  (subst tvN u b  )
    App    fun arg   -> App    (subst tvN u fun) (subst tvN u arg)
    TyApp  e   ty    -> TyApp  (subst tvN u e  ) (subst tvN u ty )
    Letrec b         -> Letrec (subst tvN u b  )
    Case   e ty alts -> Case   (subst tvN u e  )
                               (subst tvN u ty )
                               (subst tvN u alts )
    Var ty nm        -> Var    (subst tvN u ty ) nm
    Prim nm ty       -> Prim   nm (subst tvN u ty)
    e                -> e
  subst m _ _ = error $ $(curLoc) ++ "Cannot substitute for bound variable: " ++ show m

instance Subst Term Text
instance Subst Type Text

instance NFData Term where
  rnf tm = case tm of
    Var     ty nm -> rnf ty `seq` rnf nm
    Data    dc    -> rnf dc
    Literal l     -> rnf l
    Prim    nm ty -> rnf nm `seq` rnf ty
    Lam     b     -> case unsafeUnbind b of
                       (id_,tm) -> rnf id_ `seq` rnf tm
    TyLam   b       -> case unsafeUnbind b of
                         (tv,tm) -> rnf tv `seq` rnf tm
    App     tmL tmR -> rnf tmL `seq` rnf tmR
    TyApp   tm ty   -> rnf tm `seq` rnf ty
    Letrec  b       -> case unsafeUnbind b of
                        (bs,e) -> rnf (map (second unembed) (unrec bs)) `seq` rnf e
    Case    sc ty alts -> rnf sc `seq` rnf ty `seq` rnf (map unsafeUnbind alts)

instance NFData Pat where
  rnf p = case p of
    DataPat dcE xs -> rnf (unembed dcE) `seq` rnf (unrebind xs)
    LitPat  lE     -> rnf (unembed lE)
    DefaultPat     -> ()

instance NFData (Name Term) where
  rnf nm = case nm of
    (Fn s i) -> rnf s `seq` rnf i
    (Bn l r) -> rnf l `seq` rnf r
