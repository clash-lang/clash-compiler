{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Helpers for testing "Clash.Core.TermLiteral": encoding Haskell
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
  ( ToTerm (..)
  , deriveToTerm
  , deriveArbitrary
  , namedDataCon
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
import qualified Clash.Core.Basic as C (IsMultiPrim (SingleResult), LitTy (NumTy), WorkInfo (WorkNever))

import Clash.Core.Builder (runDataConSig)
import Clash.Core.DataCon (mkDataCon)
import Clash.Core.Syntax (mkPrimInfo)
import qualified Clash.Core.Syntax as S

import Test.Clash.Core (closedType)

-- | Encode a value as a closed term of scoped Clash Core, of the shape
-- "Clash.GHC.GHC2Core" makes for it, as far as the decoders of
-- 'Clash.Core.TermLiteral.TermLiteral' look. Type arguments and data
-- constructor types are placeholders.
class ToTerm a where
  toTerm :: a -> S.Term 'S.VoidS

-- | The placeholder type
intTy :: S.Type 'S.VoidS
intTy = closedType "Int"

-- | A data constructor with the given fully qualified name, as the derived
-- decoders match on (see 'Clash.Core.TermLiteral.TH')
namedDataCon :: String -> C.DataCon
namedDataCon nm =
  mkDataCon (C.mkUnsafeName C.User (Text.pack nm) (fromIntegral (Hashable.hash nm))) 1 intTy
    (runDataConSig [] [] []) [] []

-- | A primitive with the given name and the placeholder type
primInfo :: Text -> S.PrimInfo
primInfo nm = mkPrimInfo nm intTy C.WorkNever C.SingleResult S.NoUnfolding

-- | A data constructor application with type arguments and fields
dataApp :: String -> Int -> [S.Term 'S.VoidS] -> S.Term 'S.VoidS
dataApp nm nTys fields =
  foldl S.App (foldl S.TyApp (S.Data (namedDataCon nm)) (replicate nTys intTy)) fields

-- | A boxed literal, like @I# 3#@
boxed :: String -> C.Literal -> S.Term 'S.VoidS
boxed nm l = dataApp nm 0 [S.Literal l]

-- | A coercion, as "Clash.GHC.GHC2Core" makes for GADT equalities
coercion :: S.Term 'S.VoidS
coercion = S.Prim (primInfo "_CO_")

instance ToTerm Int where
  toTerm = boxed "GHC.Types.I#" . C.IntLiteral . toInteger

instance ToTerm Word where
  toTerm = boxed "GHC.Types.W#" . C.WordLiteral . toInteger

instance ToTerm Char where
  toTerm = boxed "GHC.Types.C#" . C.CharLiteral

instance ToTerm Integer where
  toTerm = S.Literal . C.IntegerLiteral

instance ToTerm Natural where
  toTerm = S.Literal . C.NaturalLiteral . toInteger

instance ToTerm String where
  toTerm = S.App (S.Prim (primInfo "GHC.CString.unpackCString#")) . S.Literal . C.StringLiteral

-- | @Text@ as an unpacked string literal; see 'textByteArrayTerm' for its
-- other encoding
instance ToTerm Text where
  toTerm = toTerm . Text.unpack

-- | @Text@ as its internal byte array, offset, and length
textByteArrayTerm :: Text -> S.Term 'S.VoidS
textByteArrayTerm (Text.Text (Text.ByteArray ba) off len) =
  dataApp "S.Data.Text.Internal.Text" 0
    [ S.Literal (C.ByteArrayLiteral (BA.ByteArray ba))
    , S.Literal (C.IntLiteral (toInteger off))
    , S.Literal (C.IntLiteral (toInteger len)) ]

instance KnownNat n => ToTerm (Index n) where
  toTerm i =
    S.App
      (S.App
        (S.TyApp (S.Prim (primInfo "Clash.Sized.Internal.Index.fromInteger#")) (S.LitTy (C.NumTy (natToInteger @n))))
        (S.Literal (C.NaturalLiteral (natToInteger @n))))
      (S.Literal (C.IntegerLiteral (toInteger i)))

instance (ToTerm a, ToTerm b) => ToTerm (a, b) where
  toTerm (a, b) = dataApp "GHC.Tuple.(,)" 2 [toTerm a, toTerm b]

instance ToTerm a => ToTerm (Vec n a) where
  toTerm = \case
    Nil -> dataApp (show 'Nil) 2 [coercion]
    Cons x xs -> dataApp (show 'Cons) 3 [coercion, toTerm x, toTerm xs]

-- | Derive a 'ToTerm' instance for a data type: a constructor @C@ with
-- fields @x1 .. xk@ of a type with @m@ parameters becomes the application of
-- the data constructor named @show 'C@ to @m@ type arguments and the
-- encoded fields.
deriveToTerm :: Name -> Q [Dec]
deriveToTerm typName = do
  (typeVars, constrs) <- reifyData typName
  clauses <- mapM (mkClause (length typeVars)) constrs
  let
    ty = foldl AppT (ConT typName) (map VarT typeVars)
    ctxt = [ConT ''ToTerm `AppT` VarT tv | tv <- typeVars]
  pure [InstanceD Nothing ctxt (ConT ''ToTerm `AppT` ty) [FunD 'toTerm clauses]]
 where
  mkClause nTys (cName, nFields) = do
    xs <- mapM (\i -> newName ("x" <> show i)) [1 .. nFields]
    body <- [| dataApp $(lift (show cName)) nTys $(listE [ [| toTerm $(varE x) |] | x <- xs ]) |]
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
