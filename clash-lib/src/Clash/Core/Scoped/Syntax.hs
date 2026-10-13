{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Scoped Clash Core: Clash Core ("Clash.Core.Term") in the style of the foil
  (Maclaurin, Radul, Paszke, "The Foil: Capture-Avoiding Substitution With No
  Sharp Edges", IFL 2022, <https://doi.org/10.1145/3587216.3587224>).

  = Scopes

  Every type and term is indexed by its /scope/ @n :: 'S'@: a @'Term' n@ only
  mentions local variables of scope @n@. Scope indices are type variables,
  bound by rank-2 polymorphic functions, so the type checker keeps the scopes
  of different binders apart. For example, the body of a lambda
  @'Lam' (b :: t'IdBinder' n l) (e :: 'Term' l)@ lives in scope @l@, which
  extends @n@ with the variable that @b@ binds, and a term in scope @n@ can't
  be used in scope @l@ without moving it there:

  * 'sink' moves a value into an /extension/ of its scope (@'DExt' n l@), which
    is a coercion. Only functions that create fresh binders, such as
    'Clash.Core.Scoped.Subst.withFreshIdBinder' and
    'Clash.Core.Scoped.Subst.openIdBinder', provide 'DExt'. So a term can't be
    moved under a binder that might capture its free variables: such a binder
    first has to be /opened/, which renames it if needed.

  * Substitution ("Clash.Core.Scoped.Subst") moves a value from one scope into
    another, renaming binders where needed.

  * Moving a value out of the scope of a binder (/unsinking/, see
    'Clash.Core.Scoped.Subst.unsinkTerm') checks that the binder doesn't occur
    in it.

  To create fresh binders, you need the 'Scope' itself, which is a runtime
  value. Functions that go under binders take a @'Scope' n@ and pass the
  extended scope on.

  Note that pattern matching on the constructors that bind variables needs the
  @GADTs@ extension (or @ExistentialQuantification@ and @MonoLocalBinds@).

  = Design

  * __One scope, two sorts.__ Type variables and term variables live in a
    single scope, exactly like Clash's 'Clash.Core.VarEnv.InScopeSet' contains
    both. The sorts are kept apart by the newtypes 'TyName' and 'TmName'. A
    'TyName' can only be obtained from a t'TyBinder', a 'TmName' only from an
    t'IdBinder', so a type never mentions a term variable.

  * __Binders carry their payload in the outer scope.__ An t'IdBinder' @n l@
    carries the type of the bound variable in scope @n@; a t'TyBinder' carries
    its kind in scope @n@. Telescopes ('Tele') chain them, so in a 'DataPat'
    the types of the field binders live in the scope extended with the
    existential type variables.

  * __Occurrences carry their type__, so that analyses don't need a typing
    environment.

  * __The types of term variables are lazy__, in t'IdBinder' and 'Var', like
    the types of the variables of GHC Core: a type that nothing asks for is never computed. E.g. the
    evaluator ("Clash.Core.Scoped.Evaluator") let-binds the arguments it
    applies functions to, with the type of the argument, which often nothing
    needs. Everything else is strict.

  * __Globals are not scoped__: 'GlobalVar' holds a 'GlobalId', i.e. a
    name with a closed type. The same holds for data constructors and
    primitives, whose types are closed too. Local variables ('TmName') and
    global ones ('GlobalId') are different types, and so are the maps keyed by
    them (see "Clash.Core.Scoped.NameMap"), so they can't be confused.

  * __Names keep their Clash metadata__ (occurrence name, sort, source
    location) as a 'Clash.Core.Name.Name' whose unique is ignored: the scoped
    name is the raw name of the binder.

  * __A scope is a watermark__, not a set of names, see 'Scope'.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Clash.Core.Scoped.Syntax
  ( -- * Scopes
    S (..)
  , Scope
  , emptyScope
  , raiseWatermark
  , Distinct
  , Ext
  , DExt
  , withExtTrans
    -- * Sinking
  , Sinkable (sinkabilityProof)
  , sink
  , sink1
  , sinkClosed
  , Name
  , NameBinder
  , extendRenamingTyNameBinder
  , extendRenamingTmNameBinder
    -- * Local names
  , TyName
  , TmName
  , tyNameRaw
  , tmNameRaw
  , ScopedName (scopedNameRaw)
  , TyNameBinder
  , TmNameBinder
  , tyNameBinderName
  , tmNameBinderName
    -- * Global names
  , GlobalId (..)
  , mkGlobalId
  , globalIdUniq
    -- * Primitives
  , PrimInfo (..)
  , mkPrimInfo
  , PrimUnfolding (..)
    -- * Binders
  , TyBinder (..)
  , IdBinder (..)
  , tyNameOf
  , tmNameOf
  , tyBinderVar
  , idBinderVar
  , Tele (..)
  , teleLength
    -- * Types
  , Type (..)
  , Kind
    -- * Terms
  , Term (..)
  , Bind (..)
  , Alt (..)
  , Pat (..)
  , TickInfo (..)
    -- * Sinkability proofs
  , sinkTyBinder
  , sinkIdBinder
  , sinkTele
  , sinkPat
  , sinkBind
  ) where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import qualified Data.Kind as K
import Data.Text (Text)
import GHC.Types.SrcLoc (SrcSpan)

import Clash.Annotations.SynthesisAttributes (Attr)
import Clash.Core.Basic (ConstTy, IsMultiPrim, LitTy, NameMod, WorkInfo)
-- Through a boot file: data constructors store scoped types
import {-# SOURCE #-} Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal)
import qualified Clash.Core.Name as C
import Clash.Unique (Uniquable (..), Unique)

import Clash.Core.Scoped.Internal.Names
import Clash.Core.Scoped.Internal.Scope

-- * Global names

-- | A global variable: one that refers to a top-level binding. Its type is
-- closed.
data GlobalId = GlobalId
  { globalIdName :: !(C.Name Term)
  -- ^ Its name, whose unique identifies it
  , globalIdType :: Type 'VoidS
  -- ^ Its type. Lazy, like the types of local variables (see the module
  -- header).
  }

-- | Shows the name and the type
instance Show GlobalId where
  showsPrec d g =
    showParen (d > 10) $
      showString "GlobalId " . showsPrec 11 (globalIdName g)
        . showString " " . showsPrec 11 (globalIdType g)

-- | By unique
instance Eq GlobalId where
  g1 == g2 = globalIdUniq g1 == globalIdUniq g2

-- | By unique
instance Ord GlobalId where
  compare g1 g2 = compare (globalIdUniq g1) (globalIdUniq g2)

instance Hashable GlobalId where
  hashWithSalt salt = hashWithSalt salt . globalIdUniq

-- | Doesn't force the type, like the instances of terms don't force the types
-- of local variables (see the module header)
instance NFData GlobalId where
  rnf g = rnf (globalIdName g)

instance Uniquable GlobalId where
  getUnique = globalIdUniq
  setUnique g u = g {globalIdName = (globalIdName g) {C.nameUniq = u}}

-- | A global variable with the given name and closed type
mkGlobalId :: C.Name Term -> Type 'VoidS -> GlobalId
mkGlobalId = GlobalId

-- | The unique of a global variable
globalIdUniq :: GlobalId -> Unique
globalIdUniq = C.nameUniq . globalIdName
{-# INLINE globalIdUniq #-}

-- * Primitives

-- | A primitive: its name, closed type, what work it does, whether it
-- returns its results as one value, and the binding that implements it
data PrimInfo = PrimInfo
  { primName :: !Text
  , primType :: Type 'VoidS
  -- ^ Lazy, like the types of global variables. The type of a multi-result
  -- primitive is the type of the primitive that returns a tuple, see
  -- 'Clash.Core.Scoped.HasType.primInfoType'.
  , primWorkInfo :: !WorkInfo
  , primMultiResult :: !IsMultiPrim
  -- ^ Primitive with multiple return values. Useful for primitives that
  -- cannot return their results as a single product type, due to limitation
  -- of synthesis tooling. It will be applied to its normal arguments, followed
  -- by the variables it should assign its results to.
  --
  -- See: 'Clash.Normalize.Scoped.Transformations.MultiPrim.setupMultiResultPrim'
  , primUnfolding :: !PrimUnfolding
  }

instance Show PrimInfo where
  showsPrec d p =
    showParen (d > 10) $
      showString "PrimInfo " . showsPrec 11 (primName p)
        . showString " " . showsPrec 11 (primType p)
        . showString " " . showsPrec 11 (primWorkInfo p)
        . showString " " . showsPrec 11 (primMultiResult p)
        . showString " " . showsPrec 11 (primUnfolding p)

-- | Doesn't force the type, like the instance of 'GlobalId'
instance NFData PrimInfo where
  rnf p =
    rnf (primName p) `seq` rnf (primWorkInfo p) `seq` rnf (primMultiResult p)
      `seq` rnf (primUnfolding p)

-- | A primitive with the given name, closed type, work info, multi-result
-- info and unfolding
mkPrimInfo :: Text -> Type 'VoidS -> WorkInfo -> IsMultiPrim -> PrimUnfolding -> PrimInfo
mkPrimInfo = PrimInfo

-- | The binding that implements a primitive, if any
data PrimUnfolding
  = NoUnfolding
  | Unfolding !GlobalId
  deriving (Show, Eq)

instance NFData PrimUnfolding where
  rnf = \case
    NoUnfolding -> ()
    Unfolding g -> rnf g

-- * Binders

-- | A type variable binder, extending scope @n@ to @l@. Its kind lives in @n@.
data TyBinder (n :: S) (l :: S) = TyBinder
  { tyBinderOcc  :: !(C.Name Type)
  -- ^ Human readable name. Its unique is /not/ used.
  , tyBinderKind :: !(Kind n)
  , tyBinderName :: !(TyNameBinder n l)
  }
  deriving (Show)
type role TyBinder nominal nominal

-- | A term variable binder, extending scope @n@ to @l@. Its type lives in @n@.
data IdBinder (n :: S) (l :: S) = IdBinder
  { idBinderOcc  :: !(C.Name Term)
  -- ^ Human readable name. Its unique is /not/ used.
  , idBinderType :: Type n
  -- ^ Lazy, see the module header
  , idBinderName :: !(TmNameBinder n l)
  }
  deriving (Show)
type role IdBinder nominal nominal

-- | The type variable a binder binds
tyNameOf :: TyBinder n l -> TyName l
tyNameOf = tyNameBinderName . tyBinderName
{-# INLINE tyNameOf #-}

-- | The term variable a binder binds
tmNameOf :: IdBinder n l -> TmName l
tmNameOf = tmNameBinderName . idBinderName
{-# INLINE tmNameOf #-}

-- | The type variable a binder binds, as a type in the extended scope
tyBinderVar :: DExt n l => TyBinder n l -> Type l
tyBinderVar b = VarTy (tyBinderOcc b) (sink (tyBinderKind b)) (tyNameOf b)
{-# INLINE tyBinderVar #-}

-- | The term variable a binder binds, as a term in the extended scope
idBinderVar :: DExt n l => IdBinder n l -> Term l
idBinderVar b = Var (idBinderOcc b) (sink (idBinderType b)) (tmNameOf b)
{-# INLINE idBinderVar #-}

-- | A telescope of binders: each binder's payload lives in the scope extended
-- by the binders before it.
type Tele :: (S -> S -> K.Type) -> S -> S -> K.Type
data Tele b n l where
  TNil  :: Tele b n n
  TCons :: !(b n i) -> !(Tele b i l) -> Tele b n l
type role Tele nominal nominal nominal

deriving instance Show (Tele TyBinder n l)
deriving instance Show (Tele IdBinder n l)

-- | The number of binders in a telescope
teleLength :: Tele b n l -> Int
teleLength = go 0
 where
  go :: Int -> Tele b x y -> Int
  go !acc TNil = acc
  go !acc (TCons _ bs) = go (acc + 1) bs

-- * Types

-- | Types, with scoped type variables
data Type (n :: S) where
  -- | Type variable occurrence: human readable name, kind, and the name itself
  VarTy    :: !(C.Name Type) -> !(Kind n) -> !(TyName n) -> Type n
  ConstTy  :: !ConstTy -> Type n
  ForAllTy :: !(TyBinder n l) -> !(Type l) -> Type n
  AppTy    :: !(Type n) -> !(Type n) -> Type n
  LitTy    :: !LitTy -> Type n
  AnnType  :: [Attr Text] -> !(Type n) -> Type n
type role Type nominal

deriving instance Show (Type n)

-- | Kinds are types
type Kind = Type

-- * Terms

-- | Terms, with scoped local variables
data Term (n :: S) where
  -- | Local variable occurrence: human readable name, type (lazy, see the
  -- module header), and the name itself
  Var       :: !(C.Name Term) -> Type n -> !(TmName n) -> Term n
  -- | Global variable. Globals are not scoped, and have closed types.
  GlobalVar :: !GlobalId -> Term n
  Data      :: !DataCon -> Term n
  Literal   :: !Literal -> Term n
  Prim      :: !PrimInfo -> Term n
  Lam       :: !(IdBinder n l) -> !(Term l) -> Term n
  TyLam     :: !(TyBinder n l) -> !(Term l) -> Term n
  App       :: !(Term n) -> !(Term n) -> Term n
  TyApp     :: !(Term n) -> !(Type n) -> Term n
  Let       :: !(Bind n l) -> !(Term l) -> Term n
  -- | Subject, type of the alternatives, alternatives
  Case      :: !(Term n) -> !(Type n) -> [Alt n] -> Term n
  -- | Term, type it is cast from, type it is cast to
  Cast      :: !(Term n) -> !(Type n) -> !(Type n) -> Term n
  Tick      :: !(TickInfo n) -> !(Term n) -> Term n
type role Term nominal

deriving instance Show (Term n)

-- | Let-bindings, binding the variables of scope @l@ that are not in @n@
data Bind (n :: S) (l :: S) where
  -- | The right-hand side lives in the outer scope
  NonRec :: !(IdBinder n l) -> !(Term n) -> Bind n l
  -- | The right-hand sides live in the inner scope. There are exactly as many
  -- right-hand sides as binders.
  Rec    :: !(Tele IdBinder n l) -> [Term l] -> Bind n l
type role Bind nominal nominal

deriving instance Show (Bind n l)

-- | A case alternative: a pattern binding the variables of some scope @l@,
-- and a right-hand side in that scope
data Alt (n :: S) where
  Alt :: !(Pat n l) -> !(Term l) -> Alt n
type role Alt nominal

deriving instance Show (Alt n)

-- | A pattern, binding the variables of scope @l@ that are not in @n@
data Pat (n :: S) (l :: S) where
  -- | The existential type variables scope over the types of the field
  -- binders, and over the right-hand side of the alternative.
  DataPat    :: !DataCon -> !(Tele TyBinder n i) -> !(Tele IdBinder i l) -> Pat n l
  LitPat     :: !Literal -> Pat n n
  DefaultPat :: Pat n n
type role Pat nominal nominal

deriving instance Show (Pat n l)

-- | Ticks: source locations, name modifiers, deduplication hints and
-- synthesis attributes, with scoped types and terms
data TickInfo (n :: S)
  = SrcSpan !SrcSpan
  | NameMod !NameMod !(Type n)
  | DeDup
  | NoDeDup
  | Attributes !(Type n) !(Term n)
  deriving (Show)
type role TickInfo nominal

-- * Normal forms
--
-- These instances don't force the lazy types of term variables, in
-- t'IdBinder' and 'Var' (see the module header): forcing them would compute
-- types that nothing asks for.

instance NFData (TyBinder n l) where
  rnf (TyBinder o k b) = rnf o `seq` rnf k `seq` rnf b

-- | Doesn't force the type of the variable
instance NFData (IdBinder n l) where
  rnf (IdBinder o _ty b) = rnf o `seq` rnf b

instance NFData (Tele TyBinder n l) where
  rnf TNil = ()
  rnf (TCons b bs) = rnf b `seq` rnf bs

instance NFData (Tele IdBinder n l) where
  rnf TNil = ()
  rnf (TCons b bs) = rnf b `seq` rnf bs

instance NFData (Type n) where
  rnf = \case
    VarTy o k n -> rnf o `seq` rnf k `seq` rnf n
    ConstTy c -> rnf c
    ForAllTy b t -> rnf b `seq` rnf t
    AppTy t1 t2 -> rnf t1 `seq` rnf t2
    LitTy l -> rnf l
    AnnType a t -> rnf a `seq` rnf t

-- | Doesn't force the types of term variables
instance NFData (Term n) where
  rnf = \case
    Var o _ty n -> rnf o `seq` rnf n
    GlobalVar g -> rnf g
    Data dc -> rnf dc
    Literal l -> rnf l
    Prim p -> rnf p
    Lam b e -> rnf b `seq` rnf e
    TyLam b e -> rnf b `seq` rnf e
    App e1 e2 -> rnf e1 `seq` rnf e2
    TyApp e ty -> rnf e `seq` rnf ty
    Let bs e -> rnf bs `seq` rnf e
    Case e ty alts -> rnf e `seq` rnf ty `seq` rnf alts
    Cast e t1 t2 -> rnf e `seq` rnf t1 `seq` rnf t2
    Tick t e -> rnf t `seq` rnf e

instance NFData (Bind n l) where
  rnf = \case
    NonRec b e -> rnf b `seq` rnf e
    Rec bs es -> rnf bs `seq` rnf es

instance NFData (Alt n) where
  rnf (Alt p e) = rnf p `seq` rnf e

instance NFData (Pat n l) where
  rnf = \case
    DataPat dc tvs ids -> rnf dc `seq` rnf tvs `seq` rnf ids
    LitPat l -> rnf l
    DefaultPat -> ()

instance NFData (TickInfo n) where
  rnf = \case
    SrcSpan s -> rnf s
    NameMod m ty -> rnf m `seq` rnf ty
    DeDup -> ()
    NoDeDup -> ()
    Attributes ty e -> rnf ty `seq` rnf e

-- * Sinkability
--
-- These instances are only typechecked, never run: 'sink' is a coercion. They
-- prove that renaming the free names of a value through an injection yields a
-- value in the target scope.

instance Sinkable Type where
  sinkabilityProof r = \case
    VarTy o k n -> VarTy o (sinkabilityProof r k) (sinkabilityProof r n)
    ConstTy c -> ConstTy c
    ForAllTy b t -> sinkTyBinder r b $ \r' b' -> ForAllTy b' (sinkabilityProof r' t)
    AppTy t1 t2 -> AppTy (sinkabilityProof r t1) (sinkabilityProof r t2)
    LitTy l -> LitTy l
    AnnType a t -> AnnType a (sinkabilityProof r t)

instance Sinkable Term where
  sinkabilityProof r = \case
    Var o ty n -> Var o (sinkabilityProof r ty) (sinkabilityProof r n)
    GlobalVar i -> GlobalVar i
    Data dc -> Data dc
    Literal l -> Literal l
    Prim p -> Prim p
    Lam b e -> sinkIdBinder r b $ \r' b' -> Lam b' (sinkabilityProof r' e)
    TyLam b e -> sinkTyBinder r b $ \r' b' -> TyLam b' (sinkabilityProof r' e)
    App e1 e2 -> App (sinkabilityProof r e1) (sinkabilityProof r e2)
    TyApp e ty -> TyApp (sinkabilityProof r e) (sinkabilityProof r ty)
    Let bs e -> sinkBind r bs $ \r' bs' -> Let bs' (sinkabilityProof r' e)
    Case e ty alts ->
      Case (sinkabilityProof r e) (sinkabilityProof r ty) (map (sinkabilityProof r) alts)
    Cast e t1 t2 ->
      Cast (sinkabilityProof r e) (sinkabilityProof r t1) (sinkabilityProof r t2)
    Tick t e -> Tick (sinkabilityProof r t) (sinkabilityProof r e)

instance Sinkable Alt where
  sinkabilityProof r (Alt p e) = sinkPat r p $ \r' p' -> Alt p' (sinkabilityProof r' e)

instance Sinkable TickInfo where
  sinkabilityProof r = \case
    SrcSpan s -> SrcSpan s
    NameMod m ty -> NameMod m (sinkabilityProof r ty)
    DeDup -> DeDup
    NoDeDup -> NoDeDup
    Attributes ty e -> Attributes (sinkabilityProof r ty) (sinkabilityProof r e)

-- | Sinkability proof of a type variable binder
sinkTyBinder
  :: (Name n -> Name n')
  -> TyBinder n l
  -> (forall l'. (Name l -> Name l') -> TyBinder n' l' -> r)
  -> r
sinkTyBinder r (TyBinder o k b) cont =
  extendRenamingTyNameBinder r b $ \r' b' ->
    cont r' (TyBinder o (sinkabilityProof r k) b')

-- | Sinkability proof of a term variable binder
sinkIdBinder
  :: (Name n -> Name n')
  -> IdBinder n l
  -> (forall l'. (Name l -> Name l') -> IdBinder n' l' -> r)
  -> r
sinkIdBinder r (IdBinder o ty b) cont =
  extendRenamingTmNameBinder r b $ \r' b' ->
    cont r' (IdBinder o (sinkabilityProof r ty) b')

-- | Sinkability proof of a telescope, given the one of its binders
sinkTele
  :: (forall x x' y r'. (Name x -> Name x') -> b x y -> (forall y'. (Name y -> Name y') -> b x' y' -> r') -> r')
  -> (Name n -> Name n')
  -> Tele b n l
  -> (forall l'. (Name l -> Name l') -> Tele b n' l' -> r)
  -> r
sinkTele _ r TNil cont = cont r TNil
sinkTele sinkB r (TCons b bs) cont =
  sinkB r b $ \r' b' -> sinkTele sinkB r' bs $ \r'' bs' -> cont r'' (TCons b' bs')

-- | Sinkability proof of a pattern
sinkPat
  :: (Name n -> Name n')
  -> Pat n l
  -> (forall l'. (Name l -> Name l') -> Pat n' l' -> r)
  -> r
sinkPat r p cont = case p of
  DataPat dc tvs ids ->
    sinkTele sinkTyBinder r tvs $ \r' tvs' ->
      sinkTele sinkIdBinder r' ids $ \r'' ids' ->
        cont r'' (DataPat dc tvs' ids')
  LitPat l -> cont r (LitPat l)
  DefaultPat -> cont r DefaultPat

-- | Sinkability proof of let-bindings
sinkBind
  :: (Name n -> Name n')
  -> Bind n l
  -> (forall l'. (Name l -> Name l') -> Bind n' l' -> r)
  -> r
sinkBind r bs cont = case bs of
  NonRec b e ->
    sinkIdBinder r b $ \r' b' -> cont r' (NonRec b' (sinkabilityProof r e))
  Rec bndrs es ->
    sinkTele sinkIdBinder r bndrs $ \r' bndrs' ->
      cont r' (Rec bndrs' (map (sinkabilityProof r') es))
