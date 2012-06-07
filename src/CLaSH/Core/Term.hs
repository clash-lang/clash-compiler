{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.Term where

-- External Modules
import Unbound.LocallyNameless as Unbound hiding (Data)
import Unbound.LocallyNameless.Name (isFree)

-- Internal Modules
import CLaSH.Core.DataCon (DataCon)
import CLaSH.Core.Literal (Literal)
import CLaSH.Core.Prim    (Prim)
import CLaSH.Core.TypeRep (Type)
import CLaSH.Core.Var     (Id,TyVar)
import CLaSH.Util

data Term
  = Var     TmName
  | Data    DataCon
  | Literal Literal
  | Prim    Prim
  | Lam     (Bind Id Term)
  | TyLam   (Bind TyVar Term)
  | App     Term Term
  | TyApp   Term Type
  | Letrec  (Bind (Rec [LetBinding]) Term)
  | Case    Term Type [Bind Pat Term]
  deriving Show

type TmName     = Name Term
type LetBinding = (Id, Embed Term)

data Pat
  = DataPat (Embed DataCon) [Id]
  | LitPat  (Embed Literal)
  | DefaultPat
  deriving (Eq,Ord,Show)

Unbound.derive [''Term,''Pat]

instance Eq Term where
  e1 == e2 = aeq e1 e2

instance Ord Term where
  compare e1 e2 = acompare e1 e2

instance Alpha Term
instance Alpha Pat

instance Subst Term Pat
instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _       = Nothing

instance Subst Type Pat
instance Subst Type Term where
  subst tvN u x | isFree tvN = case x of
    Lam    b       -> Lam    (subst tvN u b  )
    TyLam  b       -> TyLam  (subst tvN u b  )
    App    fun arg -> App    (subst tvN u fun) (subst tvN u arg)
    TyApp  e   ty  -> TyApp  (subst tvN u e  ) (subst tvN u ty )
    Letrec b       -> Letrec (subst tvN u b  )
    Case   e ty  a -> Case   (subst tvN u e  )
                             (subst tvN u ty )
                             (subst tvN u a  )
    e              -> e
  subst m _ _ = error $ $(curLoc) ++ "Cannot substitute for bound variable: " ++ show m
