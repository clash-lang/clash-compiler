{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- | The correct versions of the negative tests, which must typecheck. This
-- checks that the negative tests fail for the reason they're meant to, not
-- because they use the API wrongly.
module Ok where

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C

import Clash.Core.Scoped.Alpha
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

occ :: C.Name C.Term
occ = C.mkUnsafeSystemName (error "occ") 0

-- N1
termVar :: IdBinder n l -> Type l -> Term l
termVar b ty = Var occ ty (tmNameOf b)

-- N2
extend :: Distinct n => Scope n -> Type n -> Term n -> Term n
extend sc ty e =
  withFreshIdBinder sc occ ty $ \sc1 b ->
    Let (NonRec b e) (useScope sc1 (idBinderVar b))
 where
  useScope :: Scope m -> Term m -> Term m
  useScope _ t = t

-- N3
commonBody :: [Alt n] -> Maybe (Term n)
commonBody = commonAltBody

-- N4
pushUnder :: Distinct n => Scope n -> Term n -> Term n -> Term n
pushUnder sc (Lam b e) arg = openIdBinder sc b e $ \_ b1 e1 -> Lam b1 (App e1 (sink arg))
pushUnder _ fun arg = App fun arg

-- N5
same :: Term n -> Term n
same = id

-- N6
noLeak :: Distinct n => Scope n -> Type n -> Term n
noLeak sc ty = withFreshIdBinder sc occ ty $ \_ b -> Lam b (idBinderVar b)

-- N7
keep :: IdBinder n l -> IdBinder n l
keep (IdBinder o ty nb) = IdBinder o ty nb

-- N8
lookupGlobal :: GlobalId -> GlobalEnv a -> Maybe a
lookupGlobal = lookupGlobalEnv

lookupLocal :: TmName n -> NameMap TmName n a -> Maybe a
lookupLocal = lookupNameMap

-- N9
strengthenPat :: Pat n l -> Type l -> Maybe (Type n)
strengthenPat = strengthenTyPat
