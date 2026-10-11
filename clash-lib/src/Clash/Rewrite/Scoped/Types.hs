{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The rewrite context and the transformation type of scoped Clash Core
  ("Clash.Core.Scoped.Syntax"). See "Clash.Rewrite.Scoped" for how to write a
  transformation.

  = The context

  A @'TransformContext' n@ is the context of a subterm in scope @n@: the
  'Scope' itself, to create fresh binders, and the 'Path' from the root of the
  term to the subterm. It mirrors 'Clash.Rewrite.Types.TransformContext' and
  its 'Clash.Core.Term.CoreContext' frames, except that frames that bind
  variables are scoped: the path is a snoc-list in which every frame lives in
  the scope of the subterms below it, and a frame that goes under binders
  ('Under') extends the scope. So, unlike in Clash Core, the binders on the
  path never shadow one another, and a let-binding on the path is valid at the
  subterm as it is (after sinking).

  Only traversals extend the context, and they do so while opening the binder
  they go under: see 'underLam', 'underTyLam', 'underAlt', 'underLet' and
  'underLetRec'.

  The context is minimal (decision D6 of the port): the evaluator port decides
  how it carries let-bindings for the evaluator. Until then, the root of a
  context may be a 'LegacyRoot': the context of a Clash Core subterm that
  "Clash.Rewrite.Scoped.ViaFoil" converted.

  = Inspecting the context

  Transformations inspect the context through 'ctxFrames', which lists the
  kinds of its frames from the subterm outwards, including those of a
  'LegacyRoot'. E.g. whether a subterm is the function of an application:

  @
  case 'ctxFrames' ctx of
    'KAppFun' : _ -> ...
  @
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Clash.Rewrite.Scoped.Types
  ( -- * Transformations
    ScopedTransform (..)
  , ScopedRewrite
    -- * The context
  , TransformContext (..)
  , Path (..)
  , Frame (..)
  , Under (..)
  , LegacyRoot (..)
  , rootContext
  , pushFrame
  , pushUnder
  , appArgFrame
    -- * Going under binders
  , underLam
  , underTyLam
  , underAlt
  , underLet
  , underLetRec
    -- * Inspecting the context
  , FrameKind (..)
  , ctxFrames
  , isLambdaBodyKind
  , isTickKind
  , closestLetBinder
  , mkDerivedName
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Var as C
import qualified Clash.Rewrite.Types as C

import Clash.Core.Scoped.Subst (openAlt, openBind, openIdBinder, openTyBinder)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (primArg)
import Clash.Rewrite.Types (RewriteMonad)

-- * Transformations

-- | A transformation of a term in any scope, in its context. A newtype rather
-- than a rank-2 type synonym, so that transformations can be stored, passed to
-- combinators and defined recursively without type signatures.
newtype ScopedTransform m = ScopedTransform
  { runScopedTransform
      :: forall n. Distinct n => TransformContext n -> Term n -> m (Term n)
  }

-- | A 'ScopedTransform' in the 'RewriteMonad'
type ScopedRewrite extra = ScopedTransform (RewriteMonad extra)

-- * The context

-- | The context of a subterm in scope @n@, see the module header
data TransformContext (n :: S) = TransformContext
  { tfScope :: !(Scope n)
  -- ^ The scope of the subterm
  , tfPath :: !(Path n)
  -- ^ The path from the root to the subterm
  }
type role TransformContext nominal

-- | The path from the root of a term to a subterm in scope @n@, innermost
-- frame first
data Path (n :: S) where
  -- | The root of a term
  PRoot :: Path n
  -- | The root of a Clash Core subterm converted by
  -- "Clash.Rewrite.Scoped.ViaFoil", see 'LegacyRoot'
  PLegacy :: !(LegacyRoot n) -> Path n
  -- | A frame that doesn't bind variables
  PFrame :: !(Frame n) -> !(Path n) -> Path n
  -- | A frame that goes under binders, extending scope @i@ to @n@
  PUnder :: (Distinct i, DExt i n) => !(Under i n) -> !(Path i) -> Path n
type role Path nominal

-- | A frame of the context that doesn't bind variables. See
-- 'Clash.Core.Term.CoreContext'.
data Frame (n :: S) where
  -- | Function position of an application
  AppFun :: Frame n
  -- | Argument position of an application. If the function is an applied
  -- primitive: the name of the primitive, and the numbers of type and term
  -- arguments it is applied to. See 'appArgFrame'.
  AppArg :: !(Maybe (Text, Int, Int)) -> Frame n
  -- | Function position of a type application
  TyAppC :: Frame n
  -- | Right-hand side of a non-recursive let-binding. Its binder is in scope
  -- of the body only, not of the right-hand side.
  LetBinding :: !(IdBinder n l) -> Frame n
  -- | Subject of a case-expression
  CaseScrut :: Frame n
  -- | Body of a cast
  CastBody :: Frame n
  -- | Body of a tick
  TickC :: !(TickInfo n) -> Frame n
type role Frame nominal

-- | A frame of the context that goes under binders, extending scope @i@ to
-- @n@. See 'Clash.Core.Term.CoreContext'.
data Under (i :: S) (n :: S) where
  -- | Body of a lambda
  LamBody :: !(IdBinder i n) -> Under i n
  -- | Body of a type lambda
  TyLamBody :: !(TyBinder i n) -> Under i n
  -- | Right-hand side of a case alternative
  CaseAlt :: !(Pat i n) -> Under i n
  -- | Body of a let-expression, with its bindings (as they were before the
  -- right-hand sides were transformed, like in Clash Core)
  LetBody :: !(Bind i n) -> Under i n
  -- | The right-hand side with the given index of a recursive let-binding
  LetBindingRec :: !(Tele IdBinder i n) -> !Int -> Under i n
type role Under nominal nominal

-- | The root of the context of a Clash Core subterm that
-- "Clash.Rewrite.Scoped.ViaFoil" converted into scope @n@. Scope @n@ has the
-- free variables of the subterm, and those of the let-bindings of its context
-- that the evaluator may use (see 'Clash.Rewrite.Scoped.ViaFoil.whnfRW').
--
-- This only exists during the port, see "Clash.Rewrite.Scoped.ViaFoil".
data LegacyRoot (n :: S) = LegacyRoot
  { legacyContext :: C.TransformContext
  -- ^ The Clash Core context of the subterm
  , legacyTyVars :: [(C.TyVar, Type n)]
  -- ^ The type variables of scope @n@, with their Clash type variables.
  -- Ordered such that the kind of a type variable only mentions the type
  -- variables before it.
  , legacyIds :: [(C.Id, Term n)]
  -- ^ The term variables of scope @n@, with their Clash variables
  }
type role LegacyRoot nominal

-- | The context of the root of a term in scope @n@
rootContext :: Scope n -> TransformContext n
rootContext sc = TransformContext sc PRoot

-- | Push a frame that doesn't bind variables
pushFrame :: Frame n -> TransformContext n -> TransformContext n
pushFrame f (TransformContext sc p) = TransformContext sc (PFrame f p)
{-# INLINE pushFrame #-}

-- | Push a frame that goes under binders. The scope is the one that opening
-- the binders gave, e.g. by 'Clash.Core.Scoped.Subst.openIdBinder'.
pushUnder
  :: (Distinct i, DExt i n)
  => Scope n -> Under i n -> TransformContext i -> TransformContext n
pushUnder sc u (TransformContext _ p) = TransformContext sc (PUnder u p)
{-# INLINE pushUnder #-}

-- | The frame of the argument of an application, given the (already
-- transformed) function. See 'Clash.Core.Term.primArg'.
appArgFrame :: Term n -> Frame n
appArgFrame = AppArg . primArg

-- * Going under binders

-- | Go under the binder of a lambda: open it (see
-- 'Clash.Core.Scoped.Subst.openIdBinder'), and give the context of the body.
underLam
  :: Distinct n
  => TransformContext n
  -> IdBinder n l
  -> Term l
  -> (forall l'. DExt n l' => TransformContext l' -> IdBinder n l' -> Term l' -> r)
  -> r
underLam ctx b e k =
  openIdBinder (tfScope ctx) b e $ \sc b' e' ->
    k (pushUnder sc (LamBody b') ctx) b' e'
{-# INLINE underLam #-}

-- | Go under the binder of a type lambda, see 'underLam'
underTyLam
  :: Distinct n
  => TransformContext n
  -> TyBinder n l
  -> Term l
  -> (forall l'. DExt n l' => TransformContext l' -> TyBinder n l' -> Term l' -> r)
  -> r
underTyLam ctx b e k =
  openTyBinder (tfScope ctx) b e $ \sc b' e' ->
    k (pushUnder sc (TyLamBody b') ctx) b' e'
{-# INLINE underTyLam #-}

-- | Go under the pattern of a case alternative, see 'underLam'
underAlt
  :: Distinct n
  => TransformContext n
  -> Alt n
  -> (forall l. DExt n l => TransformContext l -> Pat n l -> Term l -> r)
  -> r
underAlt ctx alt k =
  openAlt (tfScope ctx) alt $ \sc p e ->
    k (pushUnder sc (CaseAlt p) ctx) p e
{-# INLINE underAlt #-}

-- | Go under the binders of a let-expression, see 'underLam'. Gives the
-- context of the body. For the right-hand sides of a recursive let-binding,
-- see 'underLetRec'; that of a non-recursive one lives in the scope of the
-- let-expression, in the context @'pushFrame' ('LetBinding' b) ctx@.
underLet
  :: Distinct n
  => TransformContext n
  -> Bind n l
  -> Term l
  -> (forall l'. DExt n l' => TransformContext l' -> Bind n l' -> Term l' -> r)
  -> r
underLet ctx bind e k =
  openBind (tfScope ctx) bind e $ \sc bind' e' ->
    k (pushUnder sc (LetBody bind') ctx) bind' e'
{-# INLINE underLet #-}

-- | Go under the binders of a recursive let-expression. Gives the context of
-- the body, and those of the right-hand sides, by index.
underLetRec
  :: Distinct n
  => TransformContext n
  -> Tele IdBinder n l
  -> [Term l]
  -> Term l
  -> (forall l'. DExt n l'
        => TransformContext l'
        -> (Int -> TransformContext l')
        -> Tele IdBinder n l'
        -> [Term l']
        -> Term l'
        -> r)
  -> r
underLetRec ctx bs xs e k =
  openBind (tfScope ctx) (Rec bs xs) e $ \sc bind' e' -> case bind' of
    Rec bs' xs' ->
      k (pushUnder sc (LetBody bind') ctx)
        (\i -> pushUnder sc (LetBindingRec bs' i) ctx)
        bs' xs' e'
    NonRec {} -> error "underLetRec: opening a Rec gave a NonRec"
{-# INLINE underLetRec #-}

-- * Inspecting the context

-- | The kind of a frame of the context, without its binders. See
-- 'ctxFrames'.
data FrameKind
  = KAppFun
  | KAppArg !(Maybe (Text, Int, Int))
  -- ^ See 'AppArg'
  | KTyAppC
  | KLetBinding !(C.Name C.Term)
  -- ^ Right-hand side of a let-binding, with the name of its binder
  | KLetBody
  | KLamBody
  | KTyLamBody
  | KCaseAlt
  | KCaseScrut
  | KCastBody
  | KTickC
  deriving (Eq, Show)

-- | The kinds of the frames of the context, from the subterm outwards. Lazy:
-- only the frames that are looked at are computed.
ctxFrames :: TransformContext n -> [FrameKind]
ctxFrames = go . tfPath
 where
  go :: Path x -> [FrameKind]
  go = \case
    PRoot -> []
    PLegacy root -> map legacyKind (C.tfContext (legacyContext root))
    PFrame f p -> frameKind f : go p
    PUnder u p -> underKind u : go p

  frameKind :: Frame x -> FrameKind
  frameKind = \case
    AppFun -> KAppFun
    AppArg a -> KAppArg a
    TyAppC -> KTyAppC
    LetBinding b -> KLetBinding (idBinderOcc b)
    CaseScrut -> KCaseScrut
    CastBody -> KCastBody
    TickC _ -> KTickC

  underKind :: Under x y -> FrameKind
  underKind = \case
    LamBody _ -> KLamBody
    TyLamBody _ -> KTyLamBody
    CaseAlt _ -> KCaseAlt
    LetBody _ -> KLetBody
    LetBindingRec bs i -> KLetBinding (teleOcc bs i)

  teleOcc :: Tele IdBinder x y -> Int -> C.Name C.Term
  teleOcc TNil _ = error "ctxFrames: LetBindingRec index out of range"
  teleOcc (TCons b bs) i
    | i <= 0 = idBinderOcc b
    | otherwise = teleOcc bs (i - 1)

  legacyKind :: C.CoreContext -> FrameKind
  legacyKind = \case
    C.AppFun -> KAppFun
    C.AppArg a -> KAppArg a
    C.TyAppC -> KTyAppC
    C.LetBinding b _ -> KLetBinding (C.varName b)
    C.LetBody _ -> KLetBody
    C.LamBody _ -> KLamBody
    C.TyLamBody _ -> KTyLamBody
    C.CaseAlt _ -> KCaseAlt
    C.CaseScrut -> KCaseScrut
    C.CastBody -> KCastBody
    C.TickC _ -> KTickC

-- | See 'Clash.Core.Term.isLambdaBodyCtx'
isLambdaBodyKind :: FrameKind -> Bool
isLambdaBodyKind KLamBody = True
isLambdaBodyKind _ = False

-- | See 'Clash.Core.Term.isTickCtx'
isTickKind :: FrameKind -> Bool
isTickKind KTickC = True
isTickKind _ = False

-- | The name of the binder of the closest let-binding whose right-hand side
-- the subterm is in
closestLetBinder :: TransformContext n -> Maybe (C.Name C.Term)
closestLetBinder ctx = case [nm | KLetBinding nm <- ctxFrames ctx] of
  nm : _ -> Just nm
  [] -> Nothing

-- | A name for a new binder, derived from the closest let-binder, see
-- 'Clash.Rewrite.Util.mkDerivedName'. Its unique is not used: scoped binders
-- get fresh names from the scope.
mkDerivedName :: HasCallStack => TransformContext n -> C.OccName -> C.Name C.Term
mkDerivedName ctx sf = case closestLetBinder ctx of
  Just nm -> C.appendToName nm ('_' `Text.cons` sf)
  Nothing -> C.mkUnsafeInternalName sf 0
