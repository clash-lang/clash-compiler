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

-- N10
nonRecToRec :: Distinct n => Scope n -> Term n -> Term n
nonRecToRec sc (Let bind body) = openBind sc bind body $ \_ bind1 body1 -> case bind1 of
  NonRec b rhs -> Let (Rec (TCons b TNil) [sink rhs]) body1
  Rec {} -> Let bind1 body1
nonRecToRec _ e = e

-- N11
appPropLet :: Distinct n => Scope n -> Term n -> [Term n] -> Term n
appPropLet sc fun args = case fun of
  Let bind e -> openBind sc bind e $ \sc1 bind1 e1 -> Let bind1 (appPropLet sc1 e1 (map sink args))
  _ -> foldl App fun args

-- N12
appPropLam :: Distinct n => Scope n -> Term n -> [Term n] -> Term n
appPropLam sc fun args = case (fun, args) of
  (Lam b e, arg : rest) ->
    openIdBinder sc b e $ \_ b1 e1 -> Let (NonRec b1 arg) (foldl App e1 (map sink rest))
  _ -> foldl App fun args

-- N13
appPropAlt :: Distinct n => Scope n -> Term n -> [Term n] -> Term n
appPropAlt sc fun args = case fun of
  Case scrut ty alts ->
    Case scrut ty [openAlt sc alt (\_ p e -> Alt p (foldl App e (map sink args))) | alt <- alts]
  _ -> foldl App fun args
