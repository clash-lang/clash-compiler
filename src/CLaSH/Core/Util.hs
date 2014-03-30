{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fcontext-stack=21 #-}

-- | Smart constructor and destructor functions for CoreHW
module CLaSH.Core.Util where

import           Data.HashMap.Lazy       (HashMap)
import           Unbound.LocallyNameless (Fresh, bind, embed, unbind, unembed,
                                          unrebind, unrec)
import           Unbound.LocallyNameless.Ops (unsafeUnbind)

import           CLaSH.Core.DataCon      (dcType)
import           CLaSH.Core.Literal      (literalType)
import           CLaSH.Core.Pretty       (showDoc)
import           CLaSH.Core.Term         (Pat (..), Term (..), TmName)
import           CLaSH.Core.Type         (Kind, TyName, Type (..), applyTy,
                                          isFunTy, isPolyFunCoreTy, mkFunTy,
                                          splitFunTy)
import           CLaSH.Core.TyCon        (TyCon, TyConName)
import           CLaSH.Core.Var          (Id, TyVar, Var (..), varType)
import           CLaSH.Util

-- | Type environment/context
type Gamma = HashMap TmName Type
-- | Kind environment/context
type Delta = HashMap TyName Kind

-- | Determine the type of a term
termType :: (Functor m, Fresh m)
         => HashMap TyConName TyCon
         -> Term
         -> m Type
termType m e = case e of
  Var t _        -> return t
  Data dc        -> return $ dcType dc
  Literal l      -> return $ literalType l
  Prim _ t       -> return t
  Lam b          -> do (v,e') <- unbind b
                       mkFunTy (unembed $ varType v) <$> termType m e'
  TyLam b        -> do (tv,e') <- unbind b
                       ForAllTy <$> bind tv <$> termType m e'
  App _ _        -> case collectArgs e of
                      (fun, args) -> termType m fun >>=
                                     (flip (applyTypeToArgs m) args)
  TyApp e' ty    -> termType m e' >>= (\f -> applyTy m f ty)
  Letrec b       -> do (_,e') <- unbind b
                       termType m e'
  Case _ (alt:_) -> do (_,e') <- unbind alt
                       termType m e'
  Case _ []      -> error $ $(curLoc) ++ "Empty case"

-- | Split a (Type)Application in the applied term and it arguments
collectArgs :: Term
            -> (Term, [Either Term Type])
collectArgs = go []
  where
    go args (App e1 e2) = go (Left e2:args) e1
    go args (TyApp e t) = go (Right t:args) e
    go args e           = (e, args)

-- | Split a (Type)Abstraction in the bound variables and the abstracted term
collectBndrs :: Fresh m
             => Term
             -> m ([Either Id TyVar], Term)
collectBndrs = go []
  where
    go bs (Lam b) = do
      (v,e') <- unbind b
      go (Left v:bs) e'
    go bs (TyLam b) = do
      (tv,e') <- unbind b
      go (Right tv:bs) e'
    go bs e' = return (reverse bs,e')

-- | Get the result type of a polymorphic function given a list of arguments
applyTypeToArgs :: Fresh m
                => HashMap TyConName TyCon
                -> Type
                -> [Either Term Type]
                -> m Type
applyTypeToArgs _ opTy []              = return opTy
applyTypeToArgs m opTy (Right ty:args) = applyTy m opTy ty >>=
                                          (flip (applyTypeToArgs m) args)
applyTypeToArgs m opTy (Left e:args)   = case splitFunTy m opTy of
  Just (_,resTy) -> applyTypeToArgs m resTy args
  Nothing        -> error $
                    concat [ $(curLoc)
                           , "applyTypeToArgs splitFunTy: not a funTy:\n"
                           , "opTy: "
                           , showDoc opTy
                           , "\nTerm: "
                           , showDoc e
                           , "\nOtherArgs: "
                           , unlines (map (either showDoc showDoc) args)
                           ]

-- | Get the list of term-binders out of a DataType pattern
patIds :: Pat -> [Id]
patIds (DataPat _ ids) = snd $ unrebind ids
patIds _               = []

-- | Make a type variable
mkTyVar :: Kind
        -> TyName
        -> TyVar
mkTyVar tyKind tyName = TyVar tyName (embed tyKind)

-- | Make a term variable
mkId :: Type
     -> TmName
     -> Id
mkId tmType tmName = Id tmName (embed tmType)

-- | Abstract a term over a list of term and type variables
mkAbstraction :: Term
              -> [Either Id TyVar]
              -> Term
mkAbstraction = foldr (either (Lam `dot` bind) (TyLam `dot` bind))

-- | Abstract a term over a list of term variables
mkTyLams :: Term
         -> [TyVar]
         -> Term
mkTyLams tm = mkAbstraction tm . map Right

-- | Abstract a term over a list of type variables
mkLams :: Term
       -> [Id]
       -> Term
mkLams tm = mkAbstraction tm . map Left

-- | Apply a list of types and terms to a term
mkApps :: Term
       -> [Either Term Type]
       -> Term
mkApps = foldl (\e a -> either (App e) (TyApp e) a)

-- | Apply a list of terms to a term
mkTmApps :: Term
         -> [Term]
         -> Term
mkTmApps = foldl App

-- | Apply a list of types to a term
mkTyApps :: Term
         -> [Type]
         -> Term
mkTyApps = foldl TyApp

-- | Does a term have a function type?
isFun :: (Functor m, Fresh m)
      => HashMap TyConName TyCon
      -> Term
      -> m Bool
isFun m t = fmap (isFunTy m) $ (termType m) t

-- | Does a term have a function or polymorphic type?
isPolyFun :: (Functor m, Fresh m)
          => HashMap TyConName TyCon
          -> Term
          -> m Bool
isPolyFun m t = isPolyFunCoreTy m <$> termType m t

-- | Is a term a term-abstraction?
isLam :: Term
      -> Bool
isLam (Lam _) = True
isLam _       = False

-- | Is a term a recursive let-binding?
isLet :: Term
      -> Bool
isLet (Letrec _) = True
isLet _          = False

-- | Is a term a variable reference?
isVar :: Term
      -> Bool
isVar (Var _ _) = True
isVar _         = False

-- | Is a term a datatype constructor?
isCon :: Term
      -> Bool
isCon (Data _) = True
isCon _        = False

-- | Is a term a primitive?
isPrim :: Term
       -> Bool
isPrim (Prim _ _) = True
isPrim _          = False

-- | Make variable reference out of term variable
idToVar :: Id
        -> Term
idToVar (Id nm tyE) = Var (unembed tyE) nm
idToVar tv          = error $ $(curLoc) ++ "idToVar: tyVar: " ++ showDoc tv

-- | Make a term variable out of a variable reference
varToId :: Term
        -> Id
varToId (Var ty nm) = Id nm (embed ty)
varToId e           = error $ $(curLoc) ++ "varToId: not a var: " ++ showDoc e

termSize :: Term
         -> Int
termSize (Var _ _)   = 1
termSize (Data _)    = 1
termSize (Literal _) = 1
termSize (Prim _ _)  = 1
termSize (Lam b)     = let (_,e) = unsafeUnbind b
                       in  termSize e + 1
termSize (TyLam b)   = let (_,e) = unsafeUnbind b
                       in  termSize e
termSize (App e1 e2) = termSize e1 + termSize e2
termSize (TyApp e _) = termSize e
termSize (Letrec b)  = let (bndrsR,body) = unsafeUnbind b
                           bndrSzs       = map (termSize . unembed . snd) (unrec bndrsR)
                           bodySz        = termSize body
                       in sum (bodySz:bndrSzs)
termSize (Case subj alts) = let subjSz = termSize subj
                                altSzs = map (termSize . snd . unsafeUnbind) alts
                            in  sum (subjSz:altSzs)
