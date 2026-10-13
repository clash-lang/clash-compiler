{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2021-2022, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  PrettyPrec printing class and instances for CoreHW

  Pretty printing of scoped Clash Core: the 'PrettyPrec' instances of its
  types, terms, binders, global variables and type constructors. Notably:

  * The unique of a local variable is its raw name.

  * An occurrence of a variable that is bound in the printed term shows the
    occurrence name of its binder, not its own.

  * Subterms are annotated with their position in their parent term
    ('Clash.Core.Pretty.AnnSubterm', see 'Clash.Core.Pretty.SubtermPos'), for
    navigation. The bodies of ticks are annotated too.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Core.Pretty
  ( PrettyPrec (..)
  , PrettyOptions (..)
  , ClashDoc
  , ClashAnnotation (..)
  , SubtermPos (..)
  , SyntaxElement (..)
  , ppr, ppr'
  , showPpr, showPpr'
  , showPprDiag
  , tracePprId
  , tracePpr
  , fromPpr
  , unsafeLookupEnvBool
  )
where

import Control.DeepSeq                  (NFData)
import Data.Binary                      (Binary)
import Data.Char                        (isSymbol, isUpper, ord)
import Data.Default                     (Default(..))
import Data.IntMap.Strict               (IntMap)
import qualified Data.IntMap.Strict     as IntMap
import Data.Text                        (Text)
import Control.Monad.Identity
import GHC.Float                        (castWord32ToFloat, castWord64ToDouble)
import GHC.Generics                     (Generic)
import qualified Data.Text              as T
import Data.Maybe                       (fromMaybe)
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Prettyprinter.Internal
#else
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal
#endif
import GHC.Show                         (showMultiLineString)
import GHC.Stack                        (HasCallStack)
import qualified GHC.Utils.Outputable   as GHC
import System.Environment               (lookupEnv)
import System.IO.Unsafe                 (unsafePerformIO)
import Text.Read                        (readMaybe)

import Clash.Core.Basic (ConstTy (..), LitTy (..), NameMod (..))
import Clash.Core.DataCon               (DataCon (..))
import Clash.Core.Literal               (Literal (..))
import Clash.Core.Name                  (Name (..))
import qualified Clash.Core.Name        as C
import Clash.Core.Sig            (TyFamEqn (..))
import Clash.Core.Syntax hiding (Name)
import Clash.Core.Term           (primArg)
import Clash.Core.TyCon
  (AlgTyConRhs (..), TyCon (..), TyConName, isTupleTyConLike)
import Clash.Debug                      (trace)
import Clash.Util
import qualified Clash.Util.Interpolate as I
import Clash.Pretty

unsafeLookupEnvBool :: HasCallStack =>  String -> Bool -> Bool
unsafeLookupEnvBool key dflt =
  case unsafePerformIO (lookupEnv key) of
    Nothing -> dflt
    Just a -> flip fromMaybe (readMaybe a) $ error [I.i|
      'unsafeLookupEnvBool' tried to lookup #{key} in the environment. It found
      it, but couldn't interpret it to as a Bool. Expected one of: True, False.
      But found:

        #{a}
    |]

-- | Options for the pretty-printer, controlling which elements to hide.
data PrettyOptions = PrettyOptions
  { displayUniques    :: Bool
  -- ^ whether to display unique identifiers
  , displayTypes      :: Bool
  -- ^ whether to display type information
  , displayQualifiers :: Bool
  -- ^ whether to display module qualifiers
  , displayTicks      :: Bool
  -- ^ whether to display ticks
  }

instance Default PrettyOptions where
  def = PrettyOptions
    { displayUniques    = unsafeLookupEnvBool "CLASH_PPR_UNIQUES" True
    , displayTypes      = unsafeLookupEnvBool "CLASH_PPR_TYPES" True
    , displayQualifiers = unsafeLookupEnvBool "CLASH_PPR_QUALIFIERS" True
    , displayTicks      = unsafeLookupEnvBool "CLASH_PPR_TICKS" True
    }

-- | Annotations carried on pretty-printed code.
data ClashAnnotation
  = AnnSubterm SubtermPos
  -- ^ marking navigation to a subterm, in the printed terms of scoped Core
  -- (see the module header)
  | AnnSyntax  SyntaxElement
  -- ^ marking a specific sort of syntax
  deriving Eq

-- | The position of a subterm in its parent term. It marks navigation to the
-- subterm in the printed terms of scoped Core (see the module header).
-- It holds no binders, which are scoped: the subterm is identified by its
-- position alone.
data SubtermPos
  = PosAppFun
  -- ^ Function of an application
  | PosAppArg !(Maybe (Text, Int, Int))
  -- ^ Argument of an application. If the argument is an applied primitive:
  -- the name of the primitive, and the numbers of type and term arguments it
  -- is applied to, see 'Clash.Core.Term.primArg'.
  | PosTyAppFun
  -- ^ Function of a type application
  | PosLetRhs !Int
  -- ^ The right-hand side of a let-binding with the given index, counting
  -- from 0. A non-recursive let-binding has one.
  | PosLetBody
  -- ^ Body of a let-expression
  | PosLamBody
  -- ^ Body of a lambda
  | PosTyLamBody
  -- ^ Body of a type lambda
  | PosCaseScrut
  -- ^ Subject of a case-expression
  | PosCaseAlt !Int
  -- ^ The right-hand side of the case alternative with the given index,
  -- counting from 0
  | PosCastBody
  -- ^ Body of a cast
  | PosTickBody
  -- ^ Body of a tick
  deriving (Eq, Show, Generic, NFData, Binary)

-- | Specific places in the program syntax.
data SyntaxElement = Keyword | LitS | Type | Unique | Qualifier | Ticky
  deriving (Eq, Show)

-- | Clash's specialized @Doc@ type holds metadata of type @ClashAnnotation@.
type ClashDoc = Doc ClashAnnotation

-- | PrettyPrec printing Show-like typeclass
class PrettyPrec p where

  -- default pretty-printing without hiding
  pprPrec :: Monad m => Rational -> p -> m ClashDoc

  -- pretty-printing with hiding options
  -- NB: we utilise the syntax annotations to hide the requested parts of syntax
  pprPrec' :: Monad m => PrettyOptions -> Rational -> p -> m ClashDoc
  pprPrec' opts p = fmap hide . pprPrec p
    where
      hide = \case
        FlatAlt d d'         -> FlatAlt (hide d) (hide d')
        Cat d d'             -> Cat (hide d) (hide d')
        Nest i d             -> Nest i (hide d)
        Union d d'           -> Union (hide d) (hide d')
        Column f             -> Column (hide . f)
        WithPageWidth f      -> WithPageWidth (hide . f)
        Nesting f            -> Nesting (hide . f)
        Annotated ann d'     ->
          if not (displayTypes opts)      && ann == AnnSyntax Type
          || not (displayUniques opts)    && ann == AnnSyntax Unique
          || not (displayQualifiers opts) && ann == AnnSyntax Qualifier
          || not (displayTicks opts)      && ann == AnnSyntax Ticky
            then Empty
            else Annotated ann (hide d')
        d -> d

pprM :: (Monad m, PrettyPrec p) => p -> m ClashDoc
pprM = pprPrec 0

pprM' :: (Monad m, PrettyPrec p) => PrettyOptions -> p -> m ClashDoc
pprM' opts = pprPrec' opts 0

ppr :: PrettyPrec p => p -> ClashDoc
ppr = runIdentity . pprM

ppr' :: PrettyPrec p => PrettyOptions -> p -> ClashDoc
ppr' opts = runIdentity . pprM' opts

fromPpr :: PrettyPrec a => a -> Doc ()
fromPpr = removeAnnotations . ppr

-- | Print a PrettyPrec thing to a String
showPpr :: PrettyPrec p => p -> String
showPpr = showPpr' def

showPpr' :: PrettyPrec p => PrettyOptions -> p -> String
showPpr' opts = showDoc . ppr' opts

-- | Print a 'PrettyPrec' thing for use in a one-line diagnostic: all runs of
-- whitespace (including newlines) are collapsed into a single space, and the
-- result is truncated to the given number of characters. Without this, a
-- warning quoting a term containing a @case@ or @let@ spills over many lines
-- and drowns out the rest of the message.
showPprDiag :: PrettyPrec p => Int -> p -> String
showPprDiag maxLen p =
  case splitAt maxLen (unwords (words (showPpr p))) of
    (shown, []) -> shown
    (shown, _)  -> shown <> "..."

tracePprId :: PrettyPrec p => p -> p
tracePprId p = trace (showPpr p) p

tracePpr :: PrettyPrec p => p -> a -> a
tracePpr p a = trace (showPpr p) a

parensIf :: Bool -> ClashDoc -> ClashDoc
parensIf False = id
parensIf True  = parens

viewName :: Name a -> (Text, Text, Text)
viewName n = (qual, occ, T.pack $ show $ nameUniq n)
  where (qual, occ) = T.breakOnEnd "." $ nameOcc n

instance PrettyPrec (Name a) where
  pprPrec p (viewName -> (qual, occ, uniq)) = do
    qual' <- annotate (AnnSyntax Qualifier) <$> pprPrec p qual
    occ'  <- pprPrec p occ
    uniq' <- annotate (AnnSyntax Unique) . brackets <$> (pprPrec p uniq)
    return $ qual' <> occ' <> uniq'

instance ClashPretty (Name a) where
  clashPretty = fromPpr

instance PrettyPrec a => PrettyPrec [a] where
  pprPrec prec = fmap vcat . mapM (pprPrec prec)

instance PrettyPrec Text where
  pprPrec _ = pure . pretty

instance Pretty LitTy where
  pretty (NumTy i) = pretty i
  pretty (SymTy s) = dquotes $ pretty s
  pretty (CharTy c) = squotes $ pretty c

instance PrettyPrec LitTy where
  pprPrec _ = return . annotate (AnnSyntax LitS) . pretty

instance PrettyPrec SrcSpan where
  pprPrec _ sp = return ("<src>"<>pretty (GHC.showSDocUnsafe (GHC.ppr sp)))

instance PrettyPrec DataCon where
  pprPrec _ = pprM . dcName

instance PrettyPrec Literal where
  pprPrec _ l = return $ annotate (AnnSyntax LitS) $ case l of
    IntegerLiteral i   -> parensIf (i < 0) (pretty i)
    IntLiteral i       -> parensIf (i < 0) (pretty i <> "#")
    Int64Literal i     -> parensIf (i < 0) (pretty i <> "#64")
    WordLiteral w      -> pretty w <> "##"
    Word64Literal w    -> pretty w <> "##64"
    Int8Literal i      -> parensIf (i < 0) (pretty i <> "#8")
    Int16Literal i     -> parensIf (i < 0) (pretty i <> "#16")
    Int32Literal i     -> parensIf (i < 0) (pretty i <> "#32")
    Word8Literal w     -> pretty w <> "##8"
    Word16Literal w    -> pretty w <> "##16"
    Word32Literal w    -> pretty w <> "##32"
    FloatLiteral w     -> pretty (castWord32ToFloat w) <> "#"
    DoubleLiteral w    -> pretty (castWord64ToDouble w) <> "##"
    CharLiteral c      -> pretty c <> "#"
    StringLiteral s    -> vcat $ map pretty $ showMultiLineString s
    NaturalLiteral n   -> pretty n
    ByteArrayLiteral s -> pretty $ show s

-- * Instances

instance PrettyPrec (Type n) where
  pprPrec _ = pure . pprType emptyEnv

instance ClashPretty (Type n) where
  clashPretty = fromPpr

instance PrettyPrec (Term n) where
  pprPrec prec = pure . pprTerm emptyEnv prec

instance ClashPretty (Term n) where
  clashPretty = fromPpr

-- | Its name and its type, like a local variable
instance PrettyPrec GlobalId where
  pprPrec _ g = pure (pprTyped (ppr (globalIdName g)) (pprType emptyEnv (globalIdType g)))

-- | Its name and its type
instance PrettyPrec (IdBinder n l) where
  pprPrec _ = pure . fst . pprIdBinder emptyEnv

-- | Its name
instance PrettyPrec (TyBinder n l) where
  pprPrec _ = pure . fst . pprTyBinder emptyEnv

instance PrettyPrec (TickInfo n) where
  pprPrec prec = pure . pprTick emptyEnv prec

-- | Its declaration: its kind, and its data constructors or equations. Their
-- types are closed, and the uniques of the variables they bind are their raw
-- names.
instance PrettyPrec TyCon where
  pprPrec _ = pure . pprTyCon

-- * Names

-- | The names of the variables bound in the printed term, by raw name, see
-- the module header
data Env = Env !(IntMap (C.Name Type)) !(IntMap (C.Name Term))

emptyEnv :: Env
emptyEnv = Env IntMap.empty IntMap.empty

-- | The name of a local variable: its occurrence name, with its raw name as
-- unique
localName :: C.Name a -> Int -> C.Name a
localName nm raw
  | C.nameUniq nm == u = nm
  | otherwise = nm {C.nameUniq = u}
 where
  u = fromIntegral raw
{-# INLINE localName #-}

tyVarName :: Env -> C.Name Type -> TyName n -> C.Name Type
tyVarName (Env tys _) occ n =
  let raw = tyNameRaw n
  in case IntMap.lookup raw tys of
       Just nm -> nm
       Nothing -> localName occ raw

tmVarName :: Env -> C.Name Term -> TmName n -> C.Name Term
tmVarName (Env _ tms) occ n =
  let raw = tmNameRaw n
  in case IntMap.lookup raw tms of
       Just nm -> nm
       Nothing -> localName occ raw

-- | The name of a type variable binder, and the environment of its scope
bindTy :: Env -> TyBinder n l -> (C.Name Type, Env)
bindTy (Env tys tms) b =
  let raw = tyNameRaw (tyNameOf b)
      nm = localName (tyBinderOcc b) raw
  in (nm, Env (IntMap.insert raw nm tys) tms)

-- | The name of a term variable binder, and the environment of its scope
bindId :: Env -> IdBinder n l -> (C.Name Term, Env)
bindId (Env tys tms) b =
  let raw = tmNameRaw (tmNameOf b)
      nm = localName (idBinderOcc b) raw
  in (nm, Env tys (IntMap.insert raw nm tms))

-- * Binders

-- | A name with a type
pprTyped :: ClashDoc -> ClashDoc -> ClashDoc
pprTyped nm ty = nm <> annotate (AnnSyntax Type) (align (space <> dcolon <+> ty))

-- | A type variable binder is printed by its name
pprTyBinder :: Env -> TyBinder n l -> (ClashDoc, Env)
pprTyBinder env b =
  let (nm, env') = bindTy env b
  in (ppr nm, env')

-- | A term variable binder is printed by its name and its type
pprIdBinder :: Env -> IdBinder n l -> (ClashDoc, Env)
pprIdBinder env b =
  let (nm, env') = bindId env b
  in (pprTyped (ppr nm) (pprType env (idBinderType b)), env')

-- | The binders of a telescope, and the environment of its scope
pprTele
  :: (forall x y. Env -> b x y -> (ClashDoc, Env))
  -> Env -> Tele b n l -> ([ClashDoc], Env)
pprTele _ env TNil = ([], env)
pprTele pprB env (TCons b bs) =
  let (d, env1) = pprB env b
      (ds, env2) = pprTele pprB env1 bs
  in (d : ds, env2)

-- * Terms

noPrec, opPrec, appPrec :: Rational
noPrec = 0
opPrec = 1
appPrec = 2

pprTerm :: Env -> Rational -> Term n -> ClashDoc
pprTerm env prec = \case
  Var occ _ n ->
    ppr (tmVarName env occ n) <> annotate (AnnSyntax Unique) (brackets "LocalId")
  GlobalVar g ->
    ppr (globalIdName g) <> annotate (AnnSyntax Unique) (brackets "GlobalId")
  Data dc -> ppr (dcName dc)
  Literal l -> ppr l
  Prim p -> pprPrim (primName p)
  Lam b e ->
    let (b', env') = pprIdBinder env b
    in annotate (AnnSubterm PosLamBody) $ parensIf (prec > noPrec) $
         lam <> tyParens b' <+> rarrow <> line <> pprTerm env' noPrec e
  TyLam b e ->
    let (b', env') = pprTyBinder env b
    in annotate (AnnSubterm PosTyLamBody) $ tyParensIf (prec > noPrec) $
         annotate (AnnSyntax Type) (tylam <> b' <+> rarrow <> line)
           <> pprTerm env' noPrec e
  App e1 e2 ->
    let e1' = annotate (AnnSubterm PosAppFun) (pprTerm env opPrec e1)
        e2' = annotate (AnnSubterm (PosAppArg (primArg e2))) (pprTerm env appPrec e2)
    in parensIf (prec >= appPrec) (hang 2 (sep [e1', e2']))
  TyApp e ty ->
    annotate (AnnSubterm PosTyAppFun) $ tyParensIf (prec >= appPrec) $
      hang 2 $ group $
        pprTerm env opPrec e
          <> annotate (AnnSyntax Type) (line <> at <> pprType' env TyConPrec ty)
  Let (NonRec b x) e ->
    let (b', env') = pprIdBinder env b
    in pprLet prec False [(b', pprTerm env noPrec x)] (pprTerm env' noPrec e)
  Let (Rec bs xs) e ->
    let (bs', env') = pprTele pprIdBinder env bs
    in pprLet prec True (zip bs' (map (pprTerm env' noPrec) xs)) (pprTerm env' noPrec e)
  Case e _ alts ->
    let e' = annotate (AnnSubterm PosCaseScrut) (pprTerm env prec e)
        alts' = zipWith (pprAlt env) [0 ..] alts
    in parensIf (prec > noPrec) $ hang 2 $ vsepHard $ (case_ <+> e' <+> of_) : alts'
  Cast e ty1 ty2 ->
    let e' = annotate (AnnSubterm PosCastBody) (pprTerm env appPrec e)
    in tyParensIf (prec >= appPrec) $
         e' <> annotate (AnnSyntax Type)
                 (softline <> nest 2 (vsep [cast, pprType env ty1, coerce, pprType env ty2]))
  Tick t e ->
    annotate (AnnSyntax Ticky) (pprTick env prec t <> line')
      <> annotate (AnnSubterm PosTickBody) (pprTerm env prec e)

pprPrim :: Text -> ClashDoc
pprPrim nm = annotate (AnnSyntax Qualifier) (pretty qual) <> pretty occ
 where
  (qual, occ) = T.breakOnEnd "." nm

-- | A let-expression, given whether it is recursive, its binders and
-- right-hand sides, and its body
pprLet :: Rational -> Bool -> [(ClashDoc, ClashDoc)] -> ClashDoc -> ClashDoc
pprLet prec isRec xes body =
  parensIf (prec > noPrec) $
    vsepHard [hang 2 (vsepHard (kw : xes'')), in_ <+> annotate (AnnSubterm PosLetBody) body]
 where
  xes' = zipWith (\i (x, e) -> annotate (AnnSubterm (PosLetRhs i)) (vsepHard [x, equals <+> e]))
           [0 ..] xes
  xes'' = case xes' of { [] -> ["EmptyLetrec"]; _ -> xes' }
  kw = if isRec then letrec else let_

pprAlt :: Env -> Int -> Alt n -> ClashDoc
pprAlt env i (Alt pat e) =
  let (pat', env') = pprPat env pat
  in annotate (AnnSubterm (PosCaseAlt i)) $
       hang 2 $ vsepHard [pat' <+> rarrow, pprTerm env' noPrec e]

-- | A pattern, and the environment of the right-hand side of its alternative
pprPat :: Env -> Pat n l -> (ClashDoc, Env)
pprPat env = \case
  DataPat dc tvs ids ->
    let (tvs', env1) = pprTele pprTyBinder env tvs
        (ids', env2) = pprTele pprIdBinder env1 ids
    in (sep [hsep (ppr (dcName dc) : tvs'), nest 2 (sep (map tyParens ids'))], env2)
  LitPat l -> (ppr l, env)
  DefaultPat -> ("_", env)

pprTick :: Env -> Rational -> TickInfo n -> ClashDoc
pprTick env prec = \case
  SrcSpan sp -> ppr sp
  NameMod m ty -> nameMod m <> pprType env ty
  DeDup -> "<deDup>"
  NoDeDup -> "<noDeDup>"
  Attributes _ attrs -> "<attributes>" <> pprTerm env prec attrs
 where
  nameMod = \case
    PrefixName -> "<prefixName>"
    SuffixName -> "<suffixName>"
    SuffixNameP -> "<suffixNameP>"
    SetName -> "<setName>"

-- * Types

data TypePrec = TopPrec | FunPrec | TyConPrec deriving (Eq, Ord)

maybeParen :: TypePrec -> TypePrec -> ClashDoc -> ClashDoc
maybeParen ctxt_prec inner_prec = parensIf (ctxt_prec >= inner_prec)

pprType :: Env -> Type n -> ClashDoc
pprType env = pprType' env TopPrec

pprType' :: Env -> TypePrec -> Type n -> ClashDoc
pprType' env p ty = case ty of
  VarTy occ _ n -> ppr (tyVarName env occ n)
  LitTy l -> ppr l
  ForAllTy {} -> maybeParen p FunPrec (pprForAllType env ty)
  ConstTy (TyCon tc) -> pprTcApp env p tc []
  AnnType _ t -> pprType' env p t
  _ | TyConAppView tc args <- tyView ty -> pprTcApp env p tc args
    | FunTyView arg res <- tyView ty -> pprFunTy env p arg res
  AppTy t1 t2 ->
    maybeParen p TyConPrec (pprType env t1 <+> pprType' env TyConPrec t2)
  ConstTy Arrow -> parens rarrow

-- | A chain of function types
pprFunTy :: Env -> TypePrec -> Type n -> Type n -> ClashDoc
pprFunTy env p arg0 res0 =
  maybeParen p FunPrec (sep [pprType' env FunPrec arg0, sep (map (rarrow <+>) (tails res0))])
 where
  tails res = case tyView res of
    FunTyView arg res' -> pprType' env FunPrec arg : tails res'
    _ -> [pprType env res]

-- | The type variables of consecutive foralls, and the type in their scope
pprForAllType :: Env -> Type n -> ClashDoc
pprForAllType env0 ty0 = go [] env0 ty0
 where
  go :: [ClashDoc] -> Env -> Type x -> ClashDoc
  go tvs env = \case
    ForAllTy b t ->
      let (nm, env') = bindTy env b
          tv = tyParens (ppr nm <> annotate (AnnSyntax Type)
                                     (space <> dcolon <+> pprType env (tyBinderKind b)))
      in go (tv : tvs) env' t
    rho -> sep [forall_ <+> sep (reverse tvs) <> dot, pprType env rho]

pprTcApp :: Env -> TypePrec -> TyConName -> [Type n] -> ClashDoc
pprTcApp env p tc tys
  | null tys
  = ppr tc

  | isTupleTyConLike tc
  = parens $ sep $ punctuate comma (map (pprType' env TopPrec) tys)

  | isSym
  , [ty1, ty2] <- tys
  = maybeParen p FunPrec $
      sep [pprType' env FunPrec ty1, enclose "`" "`" (ppr tc) <+> pprType' env FunPrec ty2]

  | otherwise
  = maybeParen p TyConPrec $
      hang 2 $ sep (parensIf isSym (ppr tc) : map (pprType' env TyConPrec) tys)
 where
  isSym = isSymName tc

-- | The view on types of 'Clash.Core.Type.tyView', which imports this
-- module
data TypeView n
  = FunTyView (Type n) (Type n)
  | TyConAppView TyConName [Type n]
  | OtherTypeView

tyView :: Type n -> TypeView n
tyView = go []
 where
  go args = \case
    ConstTy (TyCon tc) -> TyConAppView tc args
    ConstTy Arrow -> case args of
      [arg, res] -> FunTyView arg res
      -- With the kinds of the argument and the result, see Note [Arrow
      -- arguments] in "Clash.Core.Type"
      [_, _, arg, res] -> FunTyView arg res
      _ -> OtherTypeView
    AppTy l r -> go (r : args) l
    _ -> OtherTypeView

-- * Type constructors

pprTyCon :: TyCon -> ClashDoc
pprTyCon tc = case tc of
  AlgTyCon {algTcRhs = DataTyCon dcs} ->
    vsep (data_ <+> decl : map pprDataCon dcs)
  AlgTyCon {algTcRhs = NewTyCon dc _} ->
    vsep [newtype_ <+> decl, pprDataCon dc]
  PromotedDataCon {tyConData = dc} ->
    "promoted" <+> ppr (dcName dc)
  FunTyCon {tyConEqns = eqns} ->
    vsep (type_ <+> family_ <+> decl : map pprEqn eqns)
  PrimTyCon {} ->
    decl
 where
  decl = ppr (tyConName tc)
    <> annotate (AnnSyntax Type) (space <> dcolon <+> pprType emptyEnv (tyConKind tc))

  pprDataCon dc = ppr (dcName dc)
    <> annotate (AnnSyntax Type) (space <> dcolon <+> pprType emptyEnv (dcType dc))

  pprEqn (TyFamEqn tvs lhs rhs) =
    let (_, env) = pprTele pprTyBinder emptyEnv tvs
    in type_ <+> instance_ <+> pprTcApp env TopPrec (tyConName tc) lhs
         <+> "=" <+> pprType env rhs

-- * Syntax

dcolon, rarrow, lam, tylam, at, cast, coerce, let_, letrec, in_, case_, of_, forall_,
  data_, newtype_, type_, family_, instance_
  :: ClashDoc
dcolon = annotate (AnnSyntax Keyword) "::"
rarrow = annotate (AnnSyntax Keyword) "->"
lam = annotate (AnnSyntax Keyword) "λ"
tylam = annotate (AnnSyntax Keyword) "Λ"
at = annotate (AnnSyntax Keyword) "@"
cast = annotate (AnnSyntax Keyword) "▷"
coerce = annotate (AnnSyntax Keyword) "~"
let_ = annotate (AnnSyntax Keyword) "let"
letrec = annotate (AnnSyntax Keyword) "letrec"
in_ = annotate (AnnSyntax Keyword) "in"
case_ = annotate (AnnSyntax Keyword) "case"
of_ = annotate (AnnSyntax Keyword) "of"
forall_ = annotate (AnnSyntax Keyword) "forall"
data_ = annotate (AnnSyntax Keyword) "data"
newtype_ = annotate (AnnSyntax Keyword) "newtype"
type_ = annotate (AnnSyntax Keyword) "type"
family_ = annotate (AnnSyntax Keyword) "family"
instance_ = annotate (AnnSyntax Keyword) "instance"

tyParens :: ClashDoc -> ClashDoc
tyParens = enclose (annotate (AnnSyntax Type) lparen) (annotate (AnnSyntax Type) rparen)

tyParensIf :: Bool -> ClashDoc -> ClashDoc
tyParensIf False = id
tyParensIf True = tyParens

vsepHard :: [ClashDoc] -> ClashDoc
vsepHard = concatWith (\x y -> x <> hardline <> y)

isSymName :: C.Name a -> Bool
isSymName n = go (C.nameOcc n)
 where
  go s | T.null s = False
       | isUpper (T.head s) = isLexConSym s
       | otherwise = isLexSym s

isLexSym :: Text -> Bool
isLexSym cs = isLexConSym cs || isLexVarSym cs

isLexConSym :: Text -> Bool
isLexConSym "->" = True
isLexConSym cs = T.head cs == ':'

isLexVarSym :: Text -> Bool
isLexVarSym cs = startsVarSym (T.head cs)

startsVarSym :: Char -> Bool
startsVarSym c = isSymbolASCII c || (ord c > 0x7f && isSymbol c)

isSymbolASCII :: Char -> Bool
isSymbolASCII c = c `elem` ("!#$%&*+./<=>?@\\^|~-" :: String)
