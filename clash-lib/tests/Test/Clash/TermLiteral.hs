{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Helpers for testing "Clash.Core.TermLiteral" and
  "Clash.Core.Scoped.TermLiteral": encoding Haskell values as the Clash Core
  terms the decoders expect, and generating values.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Clash.TermLiteral
  ( ToCoreTerm (..)
  , deriveToCoreTerm
  , deriveArbitrary
  , coreDataCon
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
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C

import Test.Clash.Rewrite (intTy, mkDataCon)

-- | Encode a value as a Clash Core term of the shape "Clash.GHC.GHC2Core"
-- makes for it, as far as the decoders of 'Clash.Core.TermLiteral.TermLiteral'
-- look. Type arguments and data constructor types are placeholders.
class ToCoreTerm a where
  toCoreTerm :: a -> C.Term

-- | A data constructor with the given fully qualified name, as the derived
-- decoders match on (see 'Clash.Core.TermLiteral.TH.deriveTermToData')
coreDataCon :: String -> C.DataCon
coreDataCon nm = mkDataCon nm (fromIntegral (Hashable.hash nm)) [] [] [] intTy

-- | A data constructor application with type arguments and fields
dataApp :: String -> Int -> [C.Term] -> C.Term
dataApp nm nTys fields =
  C.mkApps
    (C.Data (coreDataCon nm))
    (replicate nTys (Right intTy) <> map Left fields)

-- | A boxed literal, like @I# 3#@
boxed :: String -> C.Literal -> C.Term
boxed nm l = dataApp nm 0 [C.Literal l]

-- | A coercion, as "Clash.GHC.GHC2Core" makes for GADT equalities
coercion :: C.Term
coercion =
  C.Prim (C.PrimInfo "_CO_" (C.ConstTy C.Arrow) C.WorkNever C.SingleResult C.NoUnfolding)

instance ToCoreTerm Int where
  toCoreTerm = boxed "GHC.Types.I#" . C.IntLiteral . toInteger

instance ToCoreTerm Word where
  toCoreTerm = boxed "GHC.Types.W#" . C.WordLiteral . toInteger

instance ToCoreTerm Char where
  toCoreTerm = boxed "GHC.Types.C#" . C.CharLiteral

instance ToCoreTerm Integer where
  toCoreTerm = C.Literal . C.IntegerLiteral

instance ToCoreTerm Natural where
  toCoreTerm = C.Literal . C.NaturalLiteral . toInteger

instance ToCoreTerm String where
  toCoreTerm = C.App (C.Prim unpackCString) . C.Literal . C.StringLiteral
   where
    unpackCString =
      C.PrimInfo "GHC.CString.unpackCString#" intTy C.WorkNever C.SingleResult C.NoUnfolding

-- | @Text@ as an unpacked string literal; see 'textByteArrayTerm' for its
-- other encoding
instance ToCoreTerm Text where
  toCoreTerm = toCoreTerm . Text.unpack

-- | @Text@ as its internal byte array, offset, and length
textByteArrayTerm :: Text -> C.Term
textByteArrayTerm (Text.Text (Text.ByteArray ba) off len) =
  dataApp "Data.Text.Internal.Text" 0
    [ C.Literal (C.ByteArrayLiteral (BA.ByteArray ba))
    , C.Literal (C.IntLiteral (toInteger off))
    , C.Literal (C.IntLiteral (toInteger len)) ]

instance KnownNat n => ToCoreTerm (Index n) where
  toCoreTerm i =
    C.mkApps
      (C.Prim fromIntegerPrim)
      [ Right (C.LitTy (C.NumTy (natToInteger @n)))
      , Left (C.Literal (C.NaturalLiteral (natToInteger @n)))
      , Left (C.Literal (C.IntegerLiteral (toInteger i))) ]
   where
    fromIntegerPrim =
      C.PrimInfo "Clash.Sized.Internal.Index.fromInteger#" intTy C.WorkNever
        C.SingleResult C.NoUnfolding

instance (ToCoreTerm a, ToCoreTerm b) => ToCoreTerm (a, b) where
  toCoreTerm (a, b) = dataApp "GHC.Tuple.(,)" 2 [toCoreTerm a, toCoreTerm b]

instance ToCoreTerm a => ToCoreTerm (Vec n a) where
  toCoreTerm = \case
    Nil -> dataApp (show 'Nil) 2 [coercion]
    Cons x xs -> dataApp (show 'Cons) 3 [coercion, toCoreTerm x, toCoreTerm xs]

-- | Derive a 'ToCoreTerm' instance for a data type: a constructor @C@ with
-- fields @x1 .. xk@ of a type with @m@ parameters becomes the application of
-- the data constructor named @show 'C@ to @m@ type arguments and the
-- encoded fields.
deriveToCoreTerm :: Name -> Q [Dec]
deriveToCoreTerm typName = do
  (typeVars, constrs) <- reifyData typName
  clauses <- mapM (mkClause (length typeVars)) constrs
  let
    ty = foldl AppT (ConT typName) (map VarT typeVars)
    ctxt = [ConT ''ToCoreTerm `AppT` VarT tv | tv <- typeVars]
  pure [InstanceD Nothing ctxt (ConT ''ToCoreTerm `AppT` ty) [FunD 'toCoreTerm clauses]]
 where
  mkClause nTys (cName, nFields) = do
    xs <- mapM (\i -> newName ("x" <> show i)) [1 .. nFields]
    body <- [| dataApp $(lift (show cName)) nTys $(listE [ [| toCoreTerm $(varE x) |] | x <- xs ]) |]
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
