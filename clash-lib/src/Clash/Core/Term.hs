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
{-# LANGUAGE ViewPatterns          #-}

module Clash.Core.Term
  ( Term (..)
  , TmName
  , LetBinding
  , Pat (..)
  , Alt
  , PrimInfo (..)
  , WorkInfo (..)
  , CoreContext (..), Context, isLambdaBodyCtx
  , collectArgs, primArg
  )
where

-- External Modules
import Control.DeepSeq
import Data.Binary                             (Binary)
import Data.Either                             (lefts, rights)
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
  | Prim    !Text !PrimInfo                 -- ^ Primitive
  | Lam     !Id Term                        -- ^ Term-abstraction
  | TyLam   !TyVar Term                     -- ^ Type-abstraction
  | App     !Term !Term                     -- ^ Application
  | TyApp   !Term !Type                     -- ^ Type-application
  | Letrec  [LetBinding] Term               -- ^ Recursive let-binding
  | Case    !Term !Type [Alt]               -- ^ Case-expression: subject, type of
                                            -- alternatives, list of alternatives
  | Cast    !Term !Type !Type               -- ^ Cast a term from one type to another
  deriving (Show,Generic,NFData,Hashable,Binary)

data PrimInfo
  = PrimInfo
  { primType     :: !Type
  , primWorkInfo :: !WorkInfo
  }
  deriving (Show,Generic,NFData,Hashable,Binary)

data WorkInfo
  = WorkConstant
  -- ^ Ignores its arguments, and outputs a constant
  | WorkNever
  -- ^ Never adds any work
  | WorkVariable
  -- ^ Does work when the arguments are variable
  | WorkAlways
  -- ^ Performs work regardless of whether the variables are constant or
  -- variable; these are things like clock or reset generators
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

-- | Context in which a term appears
data CoreContext
  = AppFun
  -- ^ Function position of an application
  | AppArg (Maybe (Text, Int, Int))
  -- ^ Argument position of an application. If this is an argument applied to
  -- a primitive, a tuple is defined containing (name of the primitive, #type
  -- args, #term args)
  | TyAppC
  -- ^ Function position of a type application
  | LetBinding Id [Id]
  -- ^ RHS of a Let-binder with the sibling LHS'
  | LetBody [Id]
  -- ^ Body of a Let-binding with the bound LHS'
  | LamBody Id
  -- ^ Body of a lambda-term with the abstracted variable
  | TyLamBody TyVar
  -- ^ Body of a TyLambda-term with the abstracted type-variable
  | CaseAlt Pat
  -- ^ RHS of a case-alternative with the bound pattern on the LHS
  | CaseScrut
  -- ^ Subject of a case-decomposition
  | CastBody
  -- ^ Body of a Cast
  deriving (Show, Generic, NFData, Hashable, Binary)

-- | A list of @CoreContext@ describes the complete navigation path from the
-- top-level to a specific sub-expression.
type Context = [CoreContext]

-- [Note] Custom @Eq@ instance for @CoreContext@
--
-- We need a manual equality instance here, due to the argument of `AppArg`.
-- Specifically, it is the only piece of information kept in `CoreContext`,
-- which references information about its children, breaking the invariant
-- that contexts represent a navigation to a specific sub-expression.
--
-- One would expect equal contexts to navigate to the same place, but if
-- these navigate to an argument position that contains different children,
-- we will get inequality from the derived `Eq`.
instance Eq CoreContext where
  c == c' = case (c, c') of
    (AppFun,          AppFun)            -> True
    (AppArg _,        AppArg _)          -> True
    -- NB: we do not see inside the argument here
    (TyAppC,          TyAppC)            -> True
    (LetBinding i is, LetBinding i' is') -> i == i' && is == is'
    (LetBody is,      LetBody is')       -> is == is'
    (LamBody i,       LamBody i')        -> i == i'
    (TyLamBody tv,    TyLamBody tv')     -> tv == tv'
    (CaseAlt p,       CaseAlt p')        -> p == p'
    (CaseScrut,       CaseScrut)         -> True
    (CastBody,        CastBody)          -> True
    (_,               _)                 -> False

-- | Is the Context a Lambda/Term-abstraction context?
isLambdaBodyCtx :: CoreContext -> Bool
isLambdaBodyCtx (LamBody _) = True
isLambdaBodyCtx _           = False

-- | Split a (Type)Application in the applied term and it arguments
collectArgs :: Term
            -> (Term, [Either Term Type])
collectArgs = go []
  where
    go args (App e1 e2) = go (Left e2:args) e1
    go args (TyApp e t) = go (Right t:args) e
    go args e           = (e, args)

-- | Given a function application, find the primitive it's applied. Yields
-- Nothing if given term is not an application or if it is not a primitive.
primArg
  :: Term
  -- ^ Function application
  -> Maybe (Text, Int, Int)
  -- ^ If @Term@ was a primitive: (name of primitive, #type args, #term args)
primArg (collectArgs -> t) =
  case t of
    (Prim nm _, args) ->
      Just (nm, length (rights args), length (lefts args))
    _ ->
      Nothing
