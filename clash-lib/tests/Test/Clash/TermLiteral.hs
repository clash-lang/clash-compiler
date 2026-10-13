{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Helpers for testing "Clash.Core.Scoped.TermLiteral": encoding Haskell
  values as the terms of scoped Clash Core that the decoders expect, and
  generating values.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Clash.TermLiteral
  ( ToScopedTerm (..)
  , deriveToScopedTerm
  , deriveArbitrary
  , scopedDataCon
  , textByteArrayTerm
  ) where

import qualified Data.Hashable as Hashable
import qualified Data.Primitive.ByteArray as BA
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Array as Text
import qualified Data.Text.Internal as Text
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Test.Tasty.QuickCheck (Arbitrary (..), oneof, scale, sized)

import Clash.Promoted.Nat (natToInteger)
import Clash.Sized.Index (Index)
import Clash.Sized.Vector (Vec (Cons, Nil))

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C (IsMultiPrim (SingleResult), WorkInfo (WorkNever))
import qualified Clash.Core.Type as C (LitTy (NumTy))

import Clash.Core.Scoped.Builder (runDataConSig)
import Clash.Core.Scoped.Convert (mkDataCon, mkPrimInfo)
import qualified Clash.Core.Scoped.Syntax as S

import Test.Clash.Scoped (closedType)

-- | Encode a value as a closed term of scoped Clash Core, of the shape
-- "Clash.GHC.GHC2Core" makes for it, as far as the decoders of
-- 'Clash.Core.Scoped.TermLiteral.TermLiteral' look. Type arguments and data
-- constructor types are placeholders.
class ToScopedTerm a where
  toScopedTerm :: a -> S.Term 'S.VoidS

-- | The placeholder type
intTy :: S.Type 'S.VoidS
intTy = closedType "Int"

-- | A data constructor with the given fully qualified name, as the derived
-- decoders match on (see 'Clash.Core.Scoped.TermLiteral.TH')
scopedDataCon :: String -> C.DataCon
scopedDataCon nm =
  mkDataCon (C.mkUnsafeName C.User (Text.pack nm) (fromIntegral (Hashable.hash nm))) 1 intTy
    (runDataConSig [] [] []) [] []

-- | A primitive with the given name and the placeholder type
primInfo :: Text -> S.PrimInfo
primInfo nm = mkPrimInfo nm intTy C.WorkNever C.SingleResult S.NoUnfolding

-- | A data constructor application with type arguments and fields
dataApp :: String -> Int -> [S.Term 'S.VoidS] -> S.Term 'S.VoidS
dataApp nm nTys fields =
  foldl S.App (foldl S.TyApp (S.Data (scopedDataCon nm)) (replicate nTys intTy)) fields

-- | A boxed literal, like @I# 3#@
boxed :: String -> C.Literal -> S.Term 'S.VoidS
boxed nm l = dataApp nm 0 [S.Literal l]

-- | A coercion, as "Clash.GHC.GHC2Core" makes for GADT equalities
coercion :: S.Term 'S.VoidS
coercion = S.Prim (primInfo "_CO_")

instance ToScopedTerm Int where
  toScopedTerm = boxed "GHC.Types.I#" . C.IntLiteral . toInteger

instance ToScopedTerm Word where
  toScopedTerm = boxed "GHC.Types.W#" . C.WordLiteral . toInteger

instance ToScopedTerm Char where
  toScopedTerm = boxed "GHC.Types.C#" . C.CharLiteral

instance ToScopedTerm Integer where
  toScopedTerm = S.Literal . C.IntegerLiteral

instance ToScopedTerm Natural where
  toScopedTerm = S.Literal . C.NaturalLiteral . toInteger

instance ToScopedTerm String where
  toScopedTerm = S.App (S.Prim (primInfo "GHC.CString.unpackCString#")) . S.Literal . C.StringLiteral

-- | @Text@ as an unpacked string literal; see 'textByteArrayTerm' for its
-- other encoding
instance ToScopedTerm Text where
  toScopedTerm = toScopedTerm . Text.unpack

-- | @Text@ as its internal byte array, offset, and length
textByteArrayTerm :: Text -> S.Term 'S.VoidS
textByteArrayTerm (Text.Text (Text.ByteArray ba) off len) =
  dataApp "S.Data.Text.Internal.Text" 0
    [ S.Literal (C.ByteArrayLiteral (BA.ByteArray ba))
    , S.Literal (C.IntLiteral (toInteger off))
    , S.Literal (C.IntLiteral (toInteger len)) ]

instance KnownNat n => ToScopedTerm (Index n) where
  toScopedTerm i =
    S.App
      (S.App
        (S.TyApp (S.Prim (primInfo "Clash.Sized.Internal.Index.fromInteger#")) (S.LitTy (C.NumTy (natToInteger @n))))
        (S.Literal (C.NaturalLiteral (natToInteger @n))))
      (S.Literal (C.IntegerLiteral (toInteger i)))

instance (ToScopedTerm a, ToScopedTerm b) => ToScopedTerm (a, b) where
  toScopedTerm (a, b) = dataApp "GHC.Tuple.(,)" 2 [toScopedTerm a, toScopedTerm b]

instance ToScopedTerm a => ToScopedTerm (Vec n a) where
  toScopedTerm = \case
    Nil -> dataApp (show 'Nil) 2 [coercion]
    Cons x xs -> dataApp (show 'Cons) 3 [coercion, toScopedTerm x, toScopedTerm xs]

-- | Derive a 'ToScopedTerm' instance for a data type: a constructor @C@ with
-- fields @x1 .. xk@ of a type with @m@ parameters becomes the application of
-- the data constructor named @show 'C@ to @m@ type arguments and the
-- encoded fields.
deriveToScopedTerm :: Name -> Q [Dec]
deriveToScopedTerm typName = do
  (typeVars, constrs) <- reifyData typName
  clauses <- mapM (mkClause (length typeVars)) constrs
  let
    ty = foldl AppT (ConT typName) (map VarT typeVars)
    ctxt = [ConT ''ToScopedTerm `AppT` VarT tv | tv <- typeVars]
  pure [InstanceD Nothing ctxt (ConT ''ToScopedTerm `AppT` ty) [FunD 'toScopedTerm clauses]]
 where
  mkClause nTys (cName, nFields) = do
    xs <- mapM (\i -> newName ("x" <> show i)) [1 .. nFields]
    body <- [| dataApp $(lift (show cName)) nTys $(listE [ [| toScopedTerm $(varE x) |] | x <- xs ]) |]
    pure (Clause [ConP cName [] (map VarP xs)] (NormalB body) [])

-- | Derive an 'Arbitrary' instance for a data type, choosing a constructor
-- uniformly, and halving the size for its fields. At size zero it only
-- chooses constructors whose fields don't mention the type itself, if there
-- are any.
deriveArbitrary :: Name -> Q [Dec]
deriveArbitrary typName = do
  (typeVars, constrs) <- reifyData typName
  TyConI (DataD _ _ _ _ cons _) <- reify typName
  let
    ty = foldl AppT (ConT typName) (map VarT typeVars)
    ctxt = [ConT ''Arbitrary `AppT` VarT tv | tv <- typeVars]
    recursive = \case
      NormalC _ fields -> any (mentions . snd) fields
      RecC _ fields -> any (\(_, _, t) -> mentions t) fields
      _ -> False
    mentions = \case
      ConT nm -> nm == typName
      AppT t1 t2 -> mentions t1 || mentions t2
      _ -> False
    gen (cName, nFields) =
      foldl
        (\e _ -> [| $e <*> scale (`div` 2) arbitrary |])
        [| pure $(conE cName) |]
        [1 .. nFields]
    gens = listE (map gen constrs)
    baseGens = case [c | (c, con) <- zip constrs cons, not (recursive con)] of
      [] -> gens
      cs -> listE (map gen cs)
  body <- [| sized (\n -> oneof (if n <= 0 then $baseGens else $gens)) |]
  pure [InstanceD Nothing ctxt (ConT ''Arbitrary `AppT` ty) [ValD (VarP 'arbitrary) (NormalB body) []]]

-- | The type variables of a data type, and the names and arities of its
-- constructors
reifyData :: Name -> Q ([Name], [(Name, Int)])
reifyData typName = do
  TyConI (DataD _ _ typeVars _ constrs _) <- reify typName
  pure (map tvName typeVars, map conArity constrs)
 where
  tvName = \case
    PlainTV nm _ -> nm
    KindedTV nm _ _ -> nm
  conArity = \case
    NormalC cName fields -> (cName, length fields)
    RecC cName fields -> (cName, length fields)
    c -> error ("reifyData: unexpected constructor: " <> show c)
