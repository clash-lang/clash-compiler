{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Print terms and types of scoped Clash Core in the language of
  'Test.Clash.Rewrite.parseTestTerm', so that a result can be written into a
  test as the term it expects.

  Free variables print as their human readable names with their uniques
  (@x_3@), or without a unique if it is the one the parser derives from the
  name (@x@), so that the printed term refers to the variables of the scope of
  a test ('Test.Clash.Scoped.TestScope') that it was built in. Binders print
  as their human readable names if that refers to them unambiguously, and with
  their raw names as uniques otherwise. What the parser doesn't parse prints
  between @<<@ and @>>@.
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Clash.Scoped.Print
  ( showTestTerm
  , showTestType
  , globalIdent
  ) where

import Data.Char (isAlphaNum, isAscii, isLower, isUpper, toLower)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Clash.Core.DataCon as C (dcName)
import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Basic as C (ConstTy (..), LitTy (..), NameMod (..))
import Clash.Core.FreeVars (HasNameBinder (..))
import Clash.Core.Internal.Scope (nameId, nameOf)
import Clash.Core.Syntax
import qualified Clash.Core.TysPrim as P


-- | The names of the variables in scope: of the binders by raw name, and the
-- keys of all names that are taken
data Env = Env
  { envTm :: !(IntMap String)
  , envTy :: !(IntMap String)
  , envKeys :: !IntSet
  }

-- | Print a term, see the module header
showTestTerm :: Term n -> String
showTestTerm tm = ppTm (Env IntMap.empty IntMap.empty (occKeysTm tm)) 0 tm

-- | Print a type, see the module header
showTestType :: Type n -> String
showTestType ty = ppTy (Env IntMap.empty IntMap.empty (occKeysTy ty)) 0 ty

-- * Names

-- | The keys of the names of all variables of a term, so that binders don't
-- get names that refer to free variables
occKeysTm :: Term n -> IntSet
occKeysTm = \case
  Var o ty _ -> IntSet.insert (uniqInt o) (occKeysTy ty)
  Lam b e -> occKeysTy (idBinderType b) <> occKeysTm e
  TyLam b e -> occKeysTy (tyBinderKind b) <> occKeysTm e
  App e1 e2 -> occKeysTm e1 <> occKeysTm e2
  TyApp e ty -> occKeysTm e <> occKeysTy ty
  Let (NonRec b x) e -> occKeysTy (idBinderType b) <> occKeysTm x <> occKeysTm e
  Let (Rec bs xs) e -> teleKeys bs <> foldMap occKeysTm xs <> occKeysTm e
  Case e ty alts -> occKeysTm e <> occKeysTy ty <> foldMap altKeys alts
  Cast e t1 t2 -> occKeysTm e <> occKeysTy t1 <> occKeysTy t2
  Tick (NameMod _ ty) e -> occKeysTy ty <> occKeysTm e
  Tick (Attributes ty x) e -> occKeysTy ty <> occKeysTm x <> occKeysTm e
  Tick _ e -> occKeysTm e
  _ -> IntSet.empty
 where
  teleKeys :: Tele IdBinder x y -> IntSet
  teleKeys = \case
    TNil -> IntSet.empty
    TCons b bs -> occKeysTy (idBinderType b) <> teleKeys bs

  altKeys :: Alt x -> IntSet
  altKeys (Alt p e) = patKeys p <> occKeysTm e

  patKeys :: Pat x y -> IntSet
  patKeys = \case
    DataPat _ tvs ids -> tvKeys tvs <> teleKeys ids
    _ -> IntSet.empty

  tvKeys :: Tele TyBinder x y -> IntSet
  tvKeys = \case
    TNil -> IntSet.empty
    TCons b bs -> occKeysTy (tyBinderKind b) <> tvKeys bs

occKeysTy :: Type n -> IntSet
occKeysTy = \case
  VarTy o ki _ -> IntSet.insert (uniqInt o) (occKeysTy ki)
  ForAllTy b t -> occKeysTy (tyBinderKind b) <> occKeysTy t
  AppTy t1 t2 -> occKeysTy t1 <> occKeysTy t2
  AnnType _ t -> occKeysTy t
  _ -> IntSet.empty

-- | A human readable name as the start of an identifier: its letters and
-- digits, starting with a lower case letter
sanitize :: String -> String
sanitize s = case filter (\c -> isAscii c && (isAlphaNum c || c == '\'')) s of
  c : cs | isLower c -> c : cs
         | isUpper c -> toLower c : cs
  cs -> 'v' : cs

-- | Whether the parser derives a unique from an identifier (see
-- 'Test.Clash.Rewrite.nameToUnique'), and which
derivedKey :: String -> Maybe Int
derivedKey s
  | not (null s), length s <= 4, all (\c -> isAscii c && isAlphaNum c) s
  , s `notElem` keywords
  = Just (List.foldl' (\acc c -> acc * 256 + fromEnum c) 0 s)
  | otherwise = Nothing
 where
  keywords = ["case", "of", "let", "in", "data", "type", "if", "then", "else", "do"]

-- | The unique of a name, as a key
uniqInt :: C.Name a -> Int
uniqInt = fromIntegral . C.nameUniq

-- | The modifier of the sort of a name, see
-- 'Test.Clash.Rewrite.parseNameScope'
sortMod :: C.Name a -> String
sortMod o = case C.nameSort o of
  C.User -> ""
  C.System -> "S"
  C.Internal -> "I"

-- | The identifier of a free variable
freeName :: C.Name a -> String
freeName o = case derivedKey s of
  Just k | k == uniqInt o -> if null m then s else s <> "_" <> m
  _ -> s <> "_" <> m <> show (C.nameUniq o)
 where
  s = sanitize (Text.unpack (C.nameOcc o))
  m = sortMod o

-- | The identifier of a binder, given its human readable name and raw name,
-- and the keys of the names taken. Gives its key too.
binderName :: IntSet -> C.Name a -> Int -> (String, Int)
binderName taken o r = case derivedKey s of
  Just k | not (IntSet.member k taken) -> (if null m then s else s <> "_" <> m, k)
  _ | not (IntSet.member r taken) -> (s <> "_" <> m <> show r, r)
    | otherwise -> (s <> "_" <> m <> show (r + 1000000), r + 1000000)
 where
  s = sanitize (Text.unpack (C.nameOcc o))
  m = sortMod o

-- | The raw name of a binder
binderRaw :: HasNameBinder b => b n l -> Int
binderRaw = nameId . nameOf . nameBinderOf

bindTm :: Env -> IdBinder n l -> (String, Env)
bindTm env b = (nm, env {envTm = IntMap.insert (binderRaw b) nm (envTm env), envKeys = IntSet.insert k (envKeys env)})
 where
  (nm, k) = binderName (envKeys env) (idBinderOcc b) (binderRaw b)

bindTy :: Env -> TyBinder n l -> (String, Env)
bindTy env b = (nm, env {envTy = IntMap.insert (binderRaw b) nm (envTy env), envKeys = IntSet.insert k (envKeys env)})
 where
  (nm, k) = binderName (envKeys env) (tyBinderOcc b) (binderRaw b)

-- | The identifier of a global variable, without its unique: the last
-- component of its name, sanitized
globalIdent :: GlobalId -> String
globalIdent g = sanitize (lastComponent (Text.unpack (C.nameOcc (globalIdName g))))

-- | Whether the last component of a qualified name is an operator
isOperator :: String -> Bool
isOperator s = case lastComponent s of
  c : _ -> not (isAlphaNum c || c == '_')
  [] -> False

-- | The last component of a qualified name, e.g. @:>@ of
-- @Clash.Sized.Vector.:>@
lastComponent :: String -> String
lastComponent s = case span (\c -> isAlphaNum c || c == '_' || c == '\'') s of
  ((c : _), '.' : rest@(_ : _)) | isUpper c -> lastComponent rest
  _ -> s

-- | A name, in parentheses if it is an operator
qualName :: String -> String
qualName s
  | isOperator s = "(" <> s <> ")"
  | otherwise = s

-- | A primitive by its name: qualified by its module, or by @Prim@ if it has
-- none, or else @Core.prim \@"name"@
primSyntax :: String -> String
primSyntax nm
  | validVar (lastComponent nm) || isOperator nm
  , all validMod modules
  = qualName (if null modules then "Prim." <> nm else nm)
  | otherwise = "Core.prim @" <> show nm
 where
  modules = splitModules nm
  validMod m = case m of
    c : cs -> isUpper c && all (\x -> isAscii x && (isAlphaNum x || x == '_')) cs
    [] -> False
  validVar v = case v of
    c : cs -> (isLower c || c == '_') && all (\x -> isAscii x && (isAlphaNum x || x `elem` ("_'#" :: String))) cs
    [] -> False

-- | The modules that qualify a name, see 'lastComponent'
splitModules :: String -> [String]
splitModules s = case span (\c -> isAlphaNum c || c == '_' || c == '\'') s of
  (m@(c : _), '.' : rest@(_ : _)) | isUpper c -> m : splitModules rest
  _ -> []

parensIf :: Bool -> String -> String
parensIf True s = "(" <> s <> ")"
parensIf False s = s

-- * Terms

-- | Print a term at a precedence: 0 for anything, 1 for the function of an
-- application, 2 for an argument
ppTm :: Env -> Int -> Term n -> String
ppTm env p = \case
  Var o _ nm -> maybe (freeName o) id (IntMap.lookup (tmNameRaw nm) (envTm env))
  GlobalVar g -> globalIdent g <> "_G" <> show (C.nameUniq (globalIdName g))
  Data dc -> qualName (Text.unpack (C.nameOcc (C.dcName dc)))
  Literal l -> ppLit l
  Prim pr -> "(" <> primSyntax (Text.unpack (primName pr)) <> " :: " <> ppTy env 0 (primType pr) <> ")"
  e@Lam {} -> parensIf (p > 0) (ppLams env [] e)
  e@TyLam {} -> parensIf (p > 0) (ppLams env [] e)
  e@App {} -> parensIf (p > 1) (ppApp env e [])
  e@TyApp {} -> parensIf (p > 1) (ppApp env e [])
  e@Let {} -> parensIf (p > 0) (ppLet env [] e)
  Case e ty alts ->
    parensIf (p > 0) $
      "(case " <> ppTm env 0 e <> " of { "
        <> List.intercalate "; " (map (ppAlt env) alts)
        <> " }) :: " <> ppTy env 0 ty
  Cast e t1 t2 ->
    parensIf (p > 1) ("Core.cast @" <> ppTy env 2 t1 <> " @" <> ppTy env 2 t2 <> " " <> ppTm env 2 e)
  Tick t e -> parensIf (p > 1) $ case t of
    DeDup -> "Core.deDup " <> ppTm env 2 e
    NoDeDup -> "Core.noDeDup " <> ppTm env 2 e
    SrcSpan _ -> "Core.srcSpan " <> ppTm env 2 e
    NameMod m ty -> "Core." <> nameMod m <> " @" <> ppTy env 2 ty <> " " <> ppTm env 2 e
    Attributes {} -> "<<ATTRIBUTES>> " <> ppTm env 2 e
 where
  nameMod = \case
    C.PrefixName -> "prefixName"
    C.SuffixName -> "suffixName"
    C.SuffixNameP -> "suffixNameP"
    C.SetName -> "setName"

ppLit :: C.Literal -> String
ppLit = \case
  C.IntLiteral i -> if i < 0 then "(" <> show i <> ")" else show i
  C.IntegerLiteral i -> "(" <> show i <> " :: Integer)"
  C.NaturalLiteral i -> "(" <> show i <> " :: Natural)"
  C.WordLiteral i -> "(" <> show i <> " :: Word)"
  C.StringLiteral s -> show s
  C.CharLiteral c -> show c
  l -> "<<LITERAL " <> show l <> ">>"

-- | Lambdas, with their binders so far
ppLams :: Env -> [String] -> Term n -> String
ppLams env bs = \case
  Lam b e ->
    let (nm, env') = bindTm env b
    in  ppLams env' (bs <> ["(" <> nm <> " :: " <> ppTy env 0 (idBinderType b) <> ")"]) e
  TyLam b e ->
    let (nm, env') = bindTy env b
    in  ppLams env' (bs <> ["(" <> nm <> " :: " <> ppKindAnn env (tyBinderKind b) <> ")"]) e
  e -> "\\" <> unwords bs <> " -> " <> ppTm env 0 e

-- | An application, with its arguments so far
ppApp :: Env -> Term n -> [String] -> String
ppApp env e args = case e of
  App f a -> ppApp env f (ppTm env 2 a : args)
  TyApp f ty -> ppApp env f (("@" <> ppTy env 2 ty) : args)
  _ -> unwords (ppTm env 1 e : args)

-- | Let-expressions: consecutive non-recursive bindings print in a single
-- let-expression, with their bindings so far
ppLet :: Env -> [String] -> Term n -> String
ppLet env bs = \case
  Let (NonRec b x) e ->
    let (nm, env') = bindTm env b
        bnd = "!(" <> nm <> " :: " <> ppTy env 0 (idBinderType b) <> ") = " <> ppTm env 0 x
    in  ppLet env' (bs <> [bnd]) e
  e | not (null bs) -> "let { " <> List.intercalate "; " bs <> " } in " <> ppTm env 0 e
  Let (Rec tele xs) e ->
    let (env', bnds) = bindTele env tele
        rhss = map (ppTm env' 0) xs
    in  "let { "
          <> List.intercalate "; " [ "(" <> nm <> " :: " <> ty <> ") = " <> rhs | ((nm, ty), rhs) <- zip bnds rhss ]
          <> " } in " <> ppTm env' 0 e
  e -> ppTm env 0 e
 where
  bindTele :: Env -> Tele IdBinder x y -> (Env, [(String, String)])
  bindTele env0 = \case
    TNil -> (env0, [])
    TCons b rest ->
      let (nm, env1) = bindTm env0 b
          (env2, more) = bindTele env1 rest
      in  (env2, (nm, ppTy env2 0 (idBinderType b)) : more)

ppAlt :: Env -> Alt n -> String
ppAlt env (Alt pat e) = case pat of
  DefaultPat -> "_ -> " <> ppTm env 0 e
  LitPat (C.IntLiteral i) -> show i <> " -> " <> ppTm env 0 e
  LitPat l -> "<<LITPAT " <> show l <> ">> -> " <> ppTm env 0 e
  DataPat dc tvs ids ->
    let (env1, tvBs) = goTvs env tvs
        (env2, idBs) = goIds env1 ids
    in  unwords (qualName (Text.unpack (C.nameOcc (C.dcName dc))) : tvBs <> idBs) <> " -> " <> ppTm env2 0 e
 where
  goTvs :: Env -> Tele TyBinder x y -> (Env, [String])
  goTvs env0 = \case
    TNil -> (env0, [])
    TCons b rest ->
      let (nm, env1) = bindTy env0 b
          (env2, more) = goTvs env1 rest
      in  (env2, ("(" <> nm <> " :: " <> ppKindAnn env0 (tyBinderKind b) <> ")") : more)

  goIds :: Env -> Tele IdBinder x y -> (Env, [String])
  goIds env0 = \case
    TNil -> (env0, [])
    TCons b rest ->
      let (nm, env1) = bindTm env0 b
          (env2, more) = goIds env1 rest
      in  (env2, ("(" <> nm <> " :: " <> ppTy env0 0 (idBinderType b) <> ")") : more)

-- | The kind of the binder of a type variable in a term: as it is if the
-- parser recognizes it as a kind, and as @Core.Kind k@ otherwise, see
-- 'Test.Clash.Rewrite.coreKind'
ppKindAnn :: Env -> Kind n -> String
ppKindAnn env ki
  | isSimpleKind ki = ppTy env 0 ki
  | otherwise = "Core.Kind " <> ppTy env 2 ki
 where
  isSimpleKind :: Kind x -> Bool
  isSimpleKind = \case
    ConstTy (C.TyCon nm) -> ppConst (C.TyCon nm) `elem` ["Type", "Nat", "Symbol"]
    AppTy (AppTy (ConstTy C.Arrow) k1) k2 -> isSimpleKind k1 && isSimpleKind k2
    _ -> False

-- * Types

-- | Print a type at a precedence: 0 for anything, 1 for the argument of a
-- function type, 2 for the argument of an application
ppTy :: Env -> Int -> Type n -> String
ppTy env p = \case
  VarTy o _ nm -> maybe (freeName o) id (IntMap.lookup (tyNameRaw nm) (envTy env))
  ConstTy c -> ppConst c
  AppTy (AppTy (ConstTy C.Arrow) t1) t2 ->
    parensIf (p > 0) (ppTy env 1 t1 <> " -> " <> ppTy env 0 t2)
  AppTy (AppTy (AppTy (ConstTy (C.TyCon nm)) k) t1) t2
    | C.nameOcc nm == Text.pack "~", ppTy env 0 k == "Type" ->
        parensIf (p > 0) (ppTy env 1 t1 <> " ~ " <> ppTy env 1 t2)
  t@AppTy {} -> parensIf (p > 1) (ppTyApp t [])
  t@ForAllTy {} -> parensIf (p > 0) (ppForAll env [] t)
  LitTy (C.NumTy i) -> show i
  LitTy (C.SymTy s) -> show s
  LitTy l -> "<<LITTY " <> show l <> ">>"
  AnnType _ t -> "<<ANN>> " <> ppTy env 2 t
 where
  ppTyApp :: Type x -> [String] -> String
  ppTyApp t args = case t of
    AppTy t1 t2 -> ppTyApp t1 (ppTy env 2 t2 : args)
    _ -> unwords (ppTy env 2 t : args)

ppForAll :: Env -> [String] -> Type n -> String
ppForAll env bs = \case
  ForAllTy b t ->
    let (nm, env') = bindTy env b
    in  ppForAll env' (bs <> ["(" <> nm <> " :: " <> ppTy env 0 (tyBinderKind b) <> ")"]) t
  t -> "forall " <> unwords bs <> ". " <> ppTy env 0 t

ppConst :: C.ConstTy -> String
ppConst = \case
  C.Arrow -> "<<ARROW>>"
  C.TyCon nm
    | nm == P.liftedTypeKindTyConName -> "Type"
    | nm == P.typeNatKindTyConName -> "Nat"
    | nm == P.typeSymbolKindTyConName -> "Symbol"
    -- The parser finds the type constructors it knows by their names, see
    -- 'Test.Clash.Rewrite.resolveTyConName'. Others it gives the unique it
    -- derives from a short name.
    | otherwise -> case derivedKey occ of
        Just k | k /= uniqInt nm -> occ <> "_" <> show (C.nameUniq nm)
        _ -> qualName occ
   where
    occ = Text.unpack (C.nameOcc nm)
