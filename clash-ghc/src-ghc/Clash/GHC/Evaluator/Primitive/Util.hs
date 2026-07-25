{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitive.Util where

import           Control.DeepSeq            (force)
import           Control.Exception          (ArithException(..), ErrorCall, Exception, tryJust, evaluate)
import           Control.Monad.State.Strict (State, MonadState)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans.Except (runExcept)
import           Data.Bits
import qualified Data.Either         as Either
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Primitive.ByteArray as BA
import           Data.Proxy          (Proxy)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Extra     (showt)
import           GHC.Float
import           GHC.Int
import GHC.Num.Integer (Integer (..))
import           GHC.Natural
import           GHC.Prim
import           GHC.TypeLits        (KnownNat)
import           GHC.Types           (IO (..))
import           GHC.Word
import           System.IO.Unsafe    (unsafeDupablePerformIO)

import           GHC.Types.Basic     (Boxity (..))
import           GHC.Types.Name      (getSrcSpan, nameOccName, occNameString)
import           GHC.Builtin.Names   (trueDataConKey, falseDataConKey)
import qualified GHC.Core.TyCon      as TyCon
import           GHC.Builtin.Types   (tupleTyCon)

import           Clash.Core.DataCon  (DataCon (..))
import           Clash.Core.Evaluator.Types
import Clash.Core.HasType (piResultTys)
import           Clash.Core.Literal  (Literal (..))
import           Clash.Core.Name
  (Name (..), NameSort (..), mkUnsafeSystemName)
import           Clash.Core.Pretty   (showPpr)
import Clash.Core.Term
  (IsMultiPrim (..),
   PrimInfo (..),
   Term (..),
   WorkInfo (..),
   mkApps,
   PrimUnfolding(..))
import Clash.Core.Type
  (Type (..),
   LitTy (..),
   TypeView (..),
   mkFunTy,
   mkTyConApp,
   splitFunForallTy,
   tyView)
import           Clash.Core.TyCon
  (TyConMap, TyConName, tyConDataCons)
import           Clash.Core.TysPrim
import Clash.Core.Util (tyNatSize)
import Clash.Core.Var (mkTyVar)
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Debug
import           Clash.GHC.GHC2Core  (modNameM)
import           Clash.Unique        (fromGhcUnique)
import Clash.Util (MonadUnique (..), curLoc)
import           Clash.Util.Supply   (Supply,freshId)
import Clash.Normalize.PrimitiveReductions (typeNatAdd)

import qualified Clash.Normalize.Primitives as NP
import Clash.Sized.Internal.BitVector(BitVector(..), Bit(..))
import Clash.Sized.Internal.Signed   (Signed   (..))
import Clash.Sized.Internal.Unsigned (Unsigned (..))
import Clash.XException (isX)

import {-# SOURCE #-} Clash.GHC.Evaluator

import qualified Clash.Sized.Internal.BitVector
import qualified Clash.Sized.Internal.Index
import qualified Clash.Sized.Internal.Signed
import qualified Clash.Sized.Internal.Unsigned
import qualified Clash.Sized.Vector


newtype PrimEvalMonad a = PEM (State Supply a)
  deriving (Functor, Applicative, Monad, MonadState Supply)

instance MonadUnique PrimEvalMonad where
  getUniqueM = PEM $ State.state (\s -> case freshId s of (!i,!s') -> (i,s'))

runPEM :: PrimEvalMonad a -> Supply -> (a, Supply)
runPEM (PEM m) = State.runState m

-- | Helpers from ghcPrimStep's pre-map implementation. This is mostly there to
-- have a way to do a machine-based conversion of the old situation (one gigantic
-- case expression) to the current one (HashMap based lookups).
--
-- TODO: Remove this in favor of a more Haskelly approach?
data PrimStepContext = PrimStepContext
  { ty :: Type
  , checkNaturalRange1 :: Type -> Integer -> (Natural -> Natural) -> Term
  , checkNaturalRange2 :: Type -> Integer -> Integer -> (Natural -> Natural -> Natural) -> Term
  , checkNaturalRange :: Type -> [Integer] -> ([Natural] -> Term) -> Term
  , reduce :: Term -> Maybe Machine
  , reduceWith :: Machine -> Term -> Maybe Machine
  , reduceWHNF :: Term -> Maybe Machine
  , reduceWHNF' :: Machine -> Term -> Maybe Machine
  , catchDivByZero :: Term -> Term
  , catchErrorCall :: Term -> Term
  }

mkPrimStepContext :: TyConMap -> Bool -> PrimInfo -> [Type] -> [Value] -> Machine
 -> PrimStepContext
mkPrimStepContext tcm isSubj pInfo tys args mach = PrimStepContext{..}
  where
    ty = primType pInfo

    checkNaturalRange1 nTy i f =
      checkNaturalRange nTy [i]
        (\[i'] -> naturalToNaturalLiteral (f i'))

    checkNaturalRange2 nTy i j f =
      checkNaturalRange nTy [i, j]
        (\[i', j'] -> naturalToNaturalLiteral (f i' j'))

    -- Check given integer's range. If any of them are less than zero, give up
    -- and return an undefined type.
    checkNaturalRange
      :: Type
      -- Type of GHC.Natural.Natural ^
      -> [Integer]
      -> ([Natural] -> Term)
      -> Term
    checkNaturalRange nTy natsAsInts f =
      if any (<0) natsAsInts then
        TyApp (Prim NP.undefined) nTy
      else
        f (map fromInteger natsAsInts)

    reduce :: Term -> Maybe Machine
    reduce = reduceWith mach

    -- Like 'reduceWith, but reduces in (the heap of) an explicitly given machine
    -- rather than the captured 'mach'. Use this when the reduced term refers to
    -- bindings freshly allocated with 'newLetBinding'.
    reduceWith :: Machine -> Term -> Maybe Machine
    reduceWith mach0 e = case isX e of
      Left msg ->
        let resTy = getResultTy tcm ty tys
            warning = unlines
              [ "Warning: caught XException: \"" ++ msg ++ "\" while trying to evaluate: "
              , showPpr (mkApps (Prim pInfo) (map (Left . valToTerm) args))
              ]
        in trace warning (Just (setTerm (TyApp (Prim NP.undefined) resTy) mach0))
      Right e' -> Just (setTerm e' mach0)

    reduceWHNF e =
      let eval = evaluator
          mach1@Machine{mStack=[]} = whnf eval tcm isSubj (setTerm e $ stackClear mach)
      in Just $ mach1 { mStack = mStack mach }

    reduceWHNF' mach1 e =
      let eval = evaluator
          mach2@Machine{mStack=[]} = whnf eval tcm isSubj (setTerm e $ stackClear mach1)
       in Just $ mach2 { mStack = mStack mach }

    makeUndefinedIf :: Exception e => (e -> Bool) -> Term -> Term
    makeUndefinedIf wantToHandle tm =
      case unsafeDupablePerformIO $ tryJust selectException (evaluate $ force tm) of
        Right b -> b
        Left e -> trace (msg e) (TyApp (Prim NP.undefined) resTy)
      where
        resTy = getResultTy tcm ty tys
        selectException e | wantToHandle e = Just e
                          | otherwise = Nothing
        msg e = unlines ["Warning: caught exception: \"" ++ show e ++ "\" while trying to evaluate: "
                        , showPpr (mkApps (Prim pInfo) (map (Left . valToTerm) args))
                        ]

    catchDivByZero = makeUndefinedIf (==DivideByZero)

    catchErrorCall = makeUndefinedIf (const True :: ErrorCall -> Bool)

-- Helper functions for literals

pairOf :: (Value -> Maybe a) -> [Value] -> Maybe (a, a)
pairOf f [x, y] = (,) <$> f x <*> f y
pairOf _ _ = Nothing

listOf :: (Value -> Maybe a) -> [Value] -> [a]
listOf = mapMaybe

wrapUnsigned :: Integer -> Integer -> Integer
wrapUnsigned n i = i `mod` sz
 where
  sz = 1 `shiftL` fromInteger n

wrapSigned :: Integer -> Integer -> Integer
wrapSigned n i = if n == 0 then 0 else res
 where
  mask = 1 `shiftL` fromInteger (n - 1)
  res  = case divMod i mask of
           (s,i1) | even s    -> i1
                  | otherwise -> i1 - mask

doubleLiterals' :: [Value] -> [Word64]
doubleLiterals' = listOf doubleLiteral

doubleLiteral :: Value -> Maybe Word64
doubleLiteral v = case v of
  Lit (DoubleLiteral i) -> Just i
  _ -> Nothing

floatLiterals' :: [Value] -> [Word32]
floatLiterals' = listOf floatLiteral

floatLiteral :: Value -> Maybe Word32
floatLiteral v = case v of
  Lit (FloatLiteral i) -> Just i
  _ -> Nothing

integerLiterals :: [Value] -> Maybe (Integer, Integer)
integerLiterals = pairOf integerLiteral

integerLiteral :: Value -> Maybe Integer
integerLiteral v =
  case v of
    Lit (IntegerLiteral i) -> Just i
    DC dc [Left (Literal (IntLiteral i))]
      | dcTag dc == 1
      -> Just i
    DC dc [Left (Literal (ByteArrayLiteral (BA.ByteArray ba)))]
      | dcTag dc == 2
      -> Just (IP ba)
      | dcTag dc == 3
      -> Just (IN ba)
    _ -> Nothing

naturalLiterals :: [Value] -> Maybe (Integer, Integer)
naturalLiterals = pairOf naturalLiteral

naturalLiteral :: Value -> Maybe Integer
naturalLiteral v =
  case v of
    Lit (NaturalLiteral i) -> Just i
    DC dc [Left (Literal (WordLiteral i))]
      | dcTag dc == 1
      -> Just i
    DC dc [Left (Literal (ByteArrayLiteral (BA.ByteArray ba)))]
      | dcTag dc == 2
      -> Just (IP ba)
    _ -> Nothing

integerLiterals' :: [Value] -> [Integer]
integerLiterals' = listOf integerLiteral

naturalLiterals' :: [Value] -> [Integer]
naturalLiterals' = listOf naturalLiteral

intLiterals :: [Value] -> Maybe (Integer,Integer)
intLiterals = pairOf intLiteral

intLiterals' :: [Value] -> [Integer]
intLiterals' = listOf intLiteral

intCLiterals' :: [Value] -> [Integer]
intCLiterals' = listOf intCLiteral

intLiteral :: Value -> Maybe Integer
intLiteral x = case x of
  Lit (IntLiteral i) -> Just i
  _ -> Nothing

int8Literals' :: [Value] -> [Integer]
int8Literals' = listOf int8Literal

int8Literal :: Value -> Maybe Integer
int8Literal x = case x of
  Lit (Int8Literal i) -> Just i
  _ -> Nothing

int16Literals' :: [Value] -> [Integer]
int16Literals' = listOf int16Literal

int16Literal :: Value -> Maybe Integer
int16Literal x = case x of
  Lit (Int16Literal i) -> Just i
  _ -> Nothing

int32Literals' :: [Value] -> [Integer]
int32Literals' = listOf int32Literal

int32Literal :: Value -> Maybe Integer
int32Literal x = case x of
  Lit (Int32Literal i) -> Just i
  _ -> Nothing

int64Literals' :: [Value] -> [Integer]
int64Literals' = listOf int64Literal

int64Literal :: Value -> Maybe Integer
int64Literal x = case x of
  Lit (Int64Literal i) -> Just i
  _ -> Nothing

intCLiteral :: Value -> Maybe Integer
intCLiteral v = case v of
  (DC _ [Left (Literal (IntLiteral i))]) -> Just i
  _ -> Nothing

intCLiterals :: [Value] -> Maybe (Integer, Integer)
intCLiterals = pairOf intCLiteral

wordLiterals :: [Value] -> Maybe (Integer,Integer)
wordLiterals = pairOf wordLiteral

wordLiterals' :: [Value] -> [Integer]
wordLiterals' = listOf wordLiteral

wordLiteral :: Value -> Maybe Integer
wordLiteral x = case x of
  Lit (WordLiteral i) -> Just i
  _ -> Nothing

word8Literals' :: [Value] -> [Integer]
word8Literals' = listOf word8Literal

word8Literal :: Value -> Maybe Integer
word8Literal x = case x of
  Lit (Word8Literal i) -> Just i
  _ -> Nothing

word16Literals' :: [Value] -> [Integer]
word16Literals' = listOf word16Literal

word16Literal :: Value -> Maybe Integer
word16Literal x = case x of
  Lit (Word16Literal i) -> Just i
  _ -> Nothing

word32Literals' :: [Value] -> [Integer]
word32Literals' = listOf word32Literal

word32Literal :: Value -> Maybe Integer
word32Literal x = case x of
  Lit (Word32Literal i) -> Just i
  _ -> Nothing

word64Literals' :: [Value] -> [Integer]
word64Literals' = listOf word64Literal

word64Literal :: Value -> Maybe Integer
word64Literal x = case x of
  Lit (Word64Literal i) -> Just i
  _ -> Nothing

charLiterals :: [Value] -> Maybe (Char,Char)
charLiterals = pairOf charLiteral

charLiterals' :: [Value] -> [Char]
charLiterals' = listOf charLiteral

charLiteral :: Value -> Maybe Char
charLiteral x = case x of
  Lit (CharLiteral c) -> Just c
  _ -> Nothing

sizedLiterals :: Text -> [Value] -> Maybe (Integer,Integer)
sizedLiterals szCon = pairOf (sizedLiteral szCon)

sizedLiterals' :: Text -> [Value] -> [Integer]
sizedLiterals' szCon = listOf (sizedLiteral szCon)

sizedLiteral :: Text -> Value -> Maybe Integer
sizedLiteral szCon val = case val of
  PrimVal p _ [_, Lit (IntegerLiteral i)]
    | primName p == szCon -> Just i
  _ -> Nothing

bitLiterals
  :: [Value]
  -> [(Integer,Integer)]
bitLiterals = map normalizeBit . mapMaybe go
 where
  normalizeBit (msk,v) = (msk .&. 1, v .&. 1)
  go val = case val of
    PrimVal p _ [Lit (WordLiteral m), Lit (IntegerLiteral i)]
      | primName p == showt 'Clash.Sized.Internal.BitVector.fromInteger##
      -> Just (m,i)
    _ -> Nothing

indexLiterals, signedLiterals, unsignedLiterals
  :: [Value] -> Maybe (Integer,Integer)
indexLiterals     = sizedLiterals (showt 'Clash.Sized.Internal.Index.fromInteger#)
signedLiterals    = sizedLiterals (showt 'Clash.Sized.Internal.Signed.fromInteger#)
unsignedLiterals  = sizedLiterals (showt 'Clash.Sized.Internal.Unsigned.fromInteger#)

indexLiterals', signedLiterals', unsignedLiterals'
  :: [Value] -> [Integer]
indexLiterals'     = sizedLiterals' (showt 'Clash.Sized.Internal.Index.fromInteger#)
signedLiterals'    = sizedLiterals' (showt 'Clash.Sized.Internal.Signed.fromInteger#)
unsignedLiterals'  = sizedLiterals' (showt 'Clash.Sized.Internal.Unsigned.fromInteger#)

bitVectorLiterals'
  :: [Value] -> [(Integer,Integer)]
bitVectorLiterals' = listOf bitVectorLiteral

bitVectorLiteral :: Value -> Maybe (Integer, Integer)
bitVectorLiteral val = case val of
  (PrimVal p _ [_, Lit (NaturalLiteral m), Lit (IntegerLiteral i)])
    | primName p == showt 'Clash.Sized.Internal.BitVector.fromInteger# -> Just (m, i)
  _ -> Nothing

toBV :: (Integer,Integer) -> BitVector n
toBV (mask,val) = BV (fromInteger mask) (fromInteger val)

splitBV :: BitVector n -> (Integer,Integer)
splitBV (BV msk val) = (toInteger msk, toInteger val)

toBit :: (Integer,Integer) -> Bit
toBit (mask,val) = Bit (fromInteger mask) (fromInteger val)

valArgs
  :: Value
  -> Maybe [Term]
valArgs v =
  case v of
    PrimVal _ _ vs -> Just (fmap valToTerm vs)
    DC _ args -> Just (Either.lefts args)
    _ -> Nothing

-- Tries to match literal arguments to a function like
--   (Unsigned.shiftL#  :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n)
sizedLitIntLit
  :: Text -> TyConMap -> [Type] -> [Value]
  -> Maybe (Type,Integer,Integer,Integer)
sizedLitIntLit szCon tcm tys args
  | Just (nTy,kn) <- extractKnownNat tcm tys
  , [_
    ,PrimVal p _ [_,Lit (IntegerLiteral i)]
    ,valArgs -> Just [Literal (IntLiteral j)]
    ] <- args
  , primName p == szCon
  = Just (nTy,kn,i,j)
  | otherwise
  = Nothing

signedLitIntLit, unsignedLitIntLit
  :: TyConMap -> [Type] -> [Value]
  -> Maybe (Type,Integer,Integer,Integer)
signedLitIntLit    = sizedLitIntLit (showt 'Clash.Sized.Internal.Signed.fromInteger#)
unsignedLitIntLit  = sizedLitIntLit (showt 'Clash.Sized.Internal.Unsigned.fromInteger#)

bitVectorLitIntLit
  :: TyConMap -> [Type] -> [Value]
  -> Maybe (Type,Integer,(Integer,Integer),Integer)
bitVectorLitIntLit tcm tys args
  | Just (nTy,kn) <- extractKnownNat tcm tys
  , [_
    ,PrimVal p _ [_,Lit (NaturalLiteral m),Lit (IntegerLiteral i)]
    ,valArgs -> Just [Literal (IntLiteral j)]
    ] <- args
  , primName p == showt 'Clash.Sized.Internal.BitVector.fromInteger#
  = Just (nTy,kn,(m,i),j)
  | otherwise
  = Nothing

mkIntCLit :: TyConMap -> (Integer -> Literal) -> Integer -> Type -> Term
mkIntCLit tcm proj lit resTy =
  App (Data intDc) (Literal (proj lit))
 where
  (_, tyView -> TyConApp intTcNm []) = splitFunForallTy resTy
  Just intTc = UniqMap.lookup intTcNm tcm
  [intDc] = tyConDataCons intTc

mkFloatCLit :: TyConMap -> Word32 -> Type -> Term
mkFloatCLit tcm lit resTy =
  App (Data floatDc) (Literal (FloatLiteral lit))
 where
  (_, tyView -> TyConApp floatTcNm []) = splitFunForallTy resTy
  (Just floatTc) = UniqMap.lookup floatTcNm tcm
  [floatDc] = tyConDataCons floatTc

mkDoubleCLit :: TyConMap -> Word64 -> Type -> Term
mkDoubleCLit tcm lit resTy =
  App (Data doubleDc) (Literal (DoubleLiteral lit))
 where
  (_, tyView -> TyConApp doubleTcNm []) = splitFunForallTy resTy
  (Just doubleTc) = UniqMap.lookup doubleTcNm tcm
  [doubleDc] = tyConDataCons doubleTc

mkSomeNat :: TyConMap -> Integer -> Type -> Term
mkSomeNat tcm lit resTy =
  mkApps (Data someNatDc)
         [ Right (LitTy (NumTy lit))
         , Left (Literal (NaturalLiteral lit))
         , Left proxy
         ]
 where
  -- Get the SomeNat data constructor
  TyConApp someNatTcNm [] = tyView resTy
  (Just someNatTc) = UniqMap.lookup someNatTcNm tcm
  [someNatDc] = tyConDataCons someNatTc

  -- Get the Proxy data constructor
  (_:_:Right (tyView -> TyConApp proxyTcNm [natTy,_]):_,_) =
    splitFunForallTy (dcType someNatDc)
  (Just proxyTc) = UniqMap.lookup proxyTcNm tcm
  [proxyDc] = tyConDataCons proxyTc

  -- Build the Proxy argument
  proxy = mkApps (Data proxyDc)
                 [ Right natTy
                 , Right (LitTy (NumTy lit))
                 ]

-- From an argument list to function of type
--   forall n. KnownNat n => ...
-- extract (nTy,nInt)
-- where nTy is the Type of n
-- and   nInt is its value as an Integer
extractKnownNat :: TyConMap -> [Type] -> Maybe (Type, Integer)
extractKnownNat tcm tys = case tys of
  nTy : _ | Right nInt <- runExcept (tyNatSize tcm nTy)
    -> Just (nTy, nInt)
  _ -> Nothing

-- From an argument list to function of type
--   forall n m o .. . (KnownNat n, KnownNat m, KnownNat o, ..) => ...
-- extract [(nTy,nInt), (mTy,mInt), (oTy,oInt)]
-- where nTy is the Type of n
-- and   nInt is its value as an Integer
extractKnownNats :: TyConMap -> [Type] -> [(Type, Integer)]
extractKnownNats tcm =
  mapMaybe (extractKnownNat tcm . pure)

-- Construct a constant term of a sized type
mkSizedLit
  :: (Type -> Term)
  -- ^ Type constructor?
  -> Type
  -- ^ Result type
  -> Type
  -- ^ forall n.
  -> Integer
  -- ^ KnownNat n
  -> Integer
  -- ^ Value to construct
  -> Term
mkSizedLit conPrim ty nTy kn val =
  mkApps
    (conPrim sTy)
    [ Right nTy
    , Left (Literal (NaturalLiteral kn))
    , Left (Literal (IntegerLiteral val)) ]
 where
    (_,sTy) = splitFunForallTy ty

mkBitLit
  :: Type
  -- ^ Result type
  -> Integer
  -- ^ Mask
  -> Integer
  -- ^ Value
  -> Term
mkBitLit ty msk val =
  mkApps (bConPrim sTy) [ Left (Literal (WordLiteral (msk .&. 1)))
                        , Left (Literal (IntegerLiteral (val .&. 1)))]
  where
    (_,sTy) = splitFunForallTy ty

mkSignedLit, mkUnsignedLit
  :: Type
  -- Result type
  -> Type
  -- forall n.
  -> Integer
  -- KnownNat n
  -> Integer
  -- Value
  -> Term
mkSignedLit    = mkSizedLit signedConPrim
mkUnsignedLit  = mkSizedLit unsignedConPrim

mkBitVectorLit
  :: Type
  -- ^ Result type
  -> Type
  -- ^ forall n.
  -> Integer
  -- ^ KnownNat n
  -> Integer
  -- ^ mask
  -> Integer
  -- ^ Value to construct
  -> Term
mkBitVectorLit ty nTy kn mask val
  = mkApps (bvConPrim sTy)
           [Right nTy
           ,Left (Literal (NaturalLiteral kn))
           ,Left (Literal (NaturalLiteral mask))
           ,Left (Literal (IntegerLiteral val))]
  where
    (_,sTy) = splitFunForallTy ty

mkIndexLitE
  :: Type
  -- ^ Result type
  -> Type
  -- ^ forall n.
  -> Integer
  -- ^ KnownNat n
  -> Integer
  -- ^ Value to construct
  -> Either Term Term
  -- ^ Either undefined (if given value is out of bounds of given type) or term
  -- representing literal
mkIndexLitE rTy nTy kn val
  | val >= 0
  , val < kn
  = Right (mkSizedLit indexConPrim rTy nTy kn val)
  | otherwise
  = Left (TyApp (Prim NP.undefined) (mkTyConApp indexTcNm [nTy]))
  where
    TyConApp indexTcNm _ = tyView (snd (splitFunForallTy rTy))

mkIndexLit
  :: Type
  -- ^ Result type
  -> Type
  -- ^ forall n.
  -> Integer
  -- ^ KnownNat n
  -> Integer
  -- ^ Value to construct
  -> Term
mkIndexLit rTy nTy kn val =
  either id id (mkIndexLitE rTy nTy kn val)

mkBitVectorLit'
  :: (Type, Type, Integer)
  -- ^ (result type, forall n., KnownNat n)
  -> Integer
  -- ^ Mask
  -> Integer
  -- ^ Value
  -> Term
mkBitVectorLit' (ty,nTy,kn) = mkBitVectorLit ty nTy kn

mkIndexLit'
  :: (Type, Type, Integer)
  -- ^ (result type, forall n., KnownNat n)
  -> Integer
  -- ^ value
  -> Term
mkIndexLit' (rTy,nTy,kn) = mkIndexLit rTy nTy kn

boolToIntLiteral :: Bool -> Term
boolToIntLiteral b = if b then Literal (IntLiteral 1) else Literal (IntLiteral 0)

boolToBoolLiteral :: TyConMap -> Type -> Bool -> Term
boolToBoolLiteral tcm ty b =
 let (_,tyView -> TyConApp boolTcNm []) = splitFunForallTy ty
     (Just boolTc) = UniqMap.lookup boolTcNm tcm
     [falseDc,trueDc] = tyConDataCons boolTc
     retDc = if b then trueDc else falseDc
 in  Data retDc

charToCharLiteral :: Char -> Term
charToCharLiteral = Literal . CharLiteral

integerToIntLiteral :: Integer -> Term
integerToIntLiteral = Literal . IntLiteral . toInteger . (fromInteger :: Integer -> Int) -- for overflow behavior

integerToWordLiteral :: Integer -> Term
integerToWordLiteral = Literal . WordLiteral . toInteger . (fromInteger :: Integer -> Word) -- for overflow behavior

integerToInt64Literal :: Integer -> Term
integerToInt64Literal = Literal . Int64Literal . toInteger . (fromInteger :: Integer -> Int64) -- for overflow behavior

integerToWord64Literal :: Integer -> Term
integerToWord64Literal = Literal . Word64Literal . toInteger . (fromInteger :: Integer -> Word64) -- for overflow behavior

integerToIntegerLiteral :: Integer -> Term
integerToIntegerLiteral = Literal . IntegerLiteral

naturalToNaturalLiteral :: Natural -> Term
naturalToNaturalLiteral = Literal . NaturalLiteral . toInteger

bConPrim :: Type -> Term
bConPrim (tyView -> TyConApp bTcNm _)
  = Prim (PrimInfo (showt 'Clash.Sized.Internal.BitVector.fromInteger##) funTy WorkNever SingleResult NoUnfolding)
  where
    funTy      = foldr1 mkFunTy [wordPrimTy,integerPrimTy,mkTyConApp bTcNm []]
bConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

bvConPrim :: Type -> Term
bvConPrim (tyView -> TyConApp bvTcNm _)
  = Prim (PrimInfo (showt 'Clash.Sized.Internal.BitVector.fromInteger#) (ForAllTy nTV funTy) WorkNever SingleResult NoUnfolding)
  where
    funTy = foldr1 mkFunTy [naturalPrimTy,naturalPrimTy,integerPrimTy,mkTyConApp bvTcNm [nVar]]
    nName = mkUnsafeSystemName "n" 0
    nVar  = VarTy nTV
    nTV   = mkTyVar typeNatKind nName
bvConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

indexConPrim :: Type -> Term
indexConPrim (tyView -> TyConApp indexTcNm _)
  = Prim (PrimInfo (showt 'Clash.Sized.Internal.Index.fromInteger#) (ForAllTy nTV funTy) WorkNever SingleResult NoUnfolding)
  where
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp indexTcNm [nVar]]
    nName      = mkUnsafeSystemName "n" 0
    nVar       = VarTy nTV
    nTV        = mkTyVar typeNatKind nName
indexConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

signedConPrim :: Type -> Term
signedConPrim (tyView -> TyConApp signedTcNm _)
  = Prim (PrimInfo (showt 'Clash.Sized.Internal.Signed.fromInteger#) (ForAllTy nTV funTy) WorkNever SingleResult NoUnfolding)
  where
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp signedTcNm [nVar]]
    nName      = mkUnsafeSystemName "n" 0
    nVar       = VarTy nTV
    nTV        = mkTyVar typeNatKind nName
signedConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

unsignedConPrim :: Type -> Term
unsignedConPrim (tyView -> TyConApp unsignedTcNm _)
  = Prim (PrimInfo (showt 'Clash.Sized.Internal.Unsigned.fromInteger#) (ForAllTy nTV funTy) WorkNever SingleResult NoUnfolding)
  where
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp unsignedTcNm [nVar]]
    nName        = mkUnsafeSystemName "n" 0
    nVar         = VarTy nTV
    nTV          = mkTyVar typeNatKind nName
unsignedConPrim _ = error $ $(curLoc) ++ "called with incorrect type"


-- |  Lift a binary function over 'Unsigned' values to be used as literal Evaluator
--
--
liftUnsigned2 :: KnownNat n
              => (Unsigned n -> Unsigned n -> Unsigned n)
              -> Type
              -> TyConMap
              -> [Type]
              -> [Value]
              -> (Proxy n -> Maybe Term)
liftUnsigned2 = liftSized2 unsignedLiterals' mkUnsignedLit

liftSigned2 :: KnownNat n
              => (Signed n -> Signed n -> Signed n)
              -> Type
              -> TyConMap
              -> [Type]
              -> [Value]
              -> (Proxy n -> Maybe Term)
liftSigned2 = liftSized2 signedLiterals' mkSignedLit

liftBitVector2 :: KnownNat n
              => (BitVector n -> BitVector n -> BitVector n)
              -> Type
              -> TyConMap
              -> [Type]
              -> [Value]
              -> (Proxy n -> Maybe Term)
liftBitVector2  f ty tcm tys args _p
  | Just (nTy, kn) <- extractKnownNat tcm tys
  , [i,j] <- bitVectorLiterals' args
  = let BV mask val = f (toBV i) (toBV j)
    in Just $ mkBitVectorLit ty nTy kn (toInteger mask) (toInteger val)
  | otherwise = Nothing

liftBitVector2Bool :: KnownNat n
              => (BitVector n -> BitVector n -> Bool)
              -> Type
              -> TyConMap
              -> [Value]
              -> (Proxy n -> Maybe Term)
liftBitVector2Bool  f ty tcm args _p
  | [i,j] <- bitVectorLiterals' args
  = let val = f (toBV i) (toBV j)
    in Just $ boolToBoolLiteral tcm ty val
  | otherwise = Nothing

liftInteger2BitVector
  :: KnownNat n
  => (Integer -> BitVector n)
  -> (Type, Type, Integer)
  -> [Value]
  -> (Proxy n -> Maybe Term)
liftInteger2BitVector f resTyInfo args _p
  | [i] <- intCLiterals' args
  = let BV msk val = f i
     in Just (mkBitVectorLit' resTyInfo (toInteger msk) (toInteger val))

  | otherwise
  = Nothing

liftBitVector2CInt
  :: KnownNat n
  => TyConMap
  -> Type
  -> (BitVector n -> Integer)
  -> [Value]
  -> (Proxy n -> Maybe Term)
liftBitVector2CInt tcm resTy f args _p
  | [i] <- bitVectorLiterals' args
  = let val = f (toBV i)
     in Just $ mkIntCLit tcm IntLiteral val resTy
  | otherwise
  = Nothing

liftSized2 :: (KnownNat n, Integral (sized n))
           => ([Value] -> [Integer])
              -- ^ literal argument extraction function
           -> (Type -> Type -> Integer -> Integer -> Term)
              -- ^ literal contruction function
           -> (sized n -> sized n -> sized n)
           -> Type
           -> TyConMap
           -> [Type]
           -> [Value]
           -> (Proxy n -> Maybe Term)
liftSized2 extractLitArgs mkLit f ty tcm tys args p
  | Just (nTy, kn) <- extractKnownNat tcm tys
  , [i,j] <- extractLitArgs args
  = let val = runSizedF f i j p
    in Just $ mkLit ty nTy kn val
  | otherwise = Nothing

-- | Helper to run a function over sized types on integers
--
-- This only works on function of type (sized n -> sized n -> sized n)
-- The resulting function must be executed with reifyNat
runSizedF
  :: (KnownNat n, Integral (sized n))
  => (sized n -> sized n -> sized n)
  -- ^ function to run
  -> Integer
  -- ^ first  argument
  -> Integer
  -- ^ second argument
  -> (Proxy n -> Integer)
runSizedF f i j _ = toInteger $ f (fromInteger i) (fromInteger j)

extractTySizeInfo :: TyConMap -> Type -> [Type] -> (Type, Type, Integer)
extractTySizeInfo tcm ty tys = (resTy,resSizeTy,resSize)
  where
    ty' = piResultTys tcm ty tys
    (_,resTy) = splitFunForallTy ty'
    TyConApp _ [resSizeTy] = tyView resTy
    Right resSize = runExcept (tyNatSize tcm resSizeTy)

getResultTy
  :: TyConMap
  -> Type
  -> [Type]
  -> Type
getResultTy tcm ty tys = resTy
 where
  ty' = piResultTys tcm ty tys
  (_,resTy) = splitFunForallTy ty'

liftDDI :: (Double# -> Double# -> Int#) -> [Value] -> Maybe Term
liftDDI f args = case doubleLiterals' args of
  [i,j] -> Just $ runDDI f i j
  _     -> Nothing
liftDDD :: (Double# -> Double# -> Double#) -> [Value] -> Maybe Term
liftDDD f args = case doubleLiterals' args of
  [i,j] -> Just $ runDDD f i j
  _     -> Nothing
liftDD  :: (Double# -> Double#) -> [Value] -> Maybe Term
liftDD  f args = case doubleLiterals' args of
  [i]   -> Just $ runDD f i
  _     -> Nothing
runDDI :: (Double# -> Double# -> Int#) -> Word64 -> Word64 -> Term
runDDI f i j
  = let !(D# a) = castWord64ToDouble i
        !(D# b) = castWord64ToDouble j
        r = f a b
    in  Literal . IntLiteral . toInteger $ I# r
runDDD :: (Double# -> Double# -> Double#) -> Word64 -> Word64 -> Term
runDDD f i j
  = let !(D# a) = castWord64ToDouble i
        !(D# b) = castWord64ToDouble j
        r = f a b
    in  Literal . DoubleLiteral . castDoubleToWord64 $ D# r
runDD :: (Double# -> Double#) -> Word64 -> Term
runDD f i
  = let !(D# a) = castWord64ToDouble i
        r = f a
    in  Literal . DoubleLiteral . castDoubleToWord64 $ D# r

liftFFI :: (Float# -> Float# -> Int#) -> [Value] -> Maybe Term
liftFFI f args = case floatLiterals' args of
  [i,j] -> Just $ runFFI f i j
  _     -> Nothing
liftFFF :: (Float# -> Float# -> Float#) -> [Value] -> Maybe Term
liftFFF f args = case floatLiterals' args of
  [i,j] -> Just $ runFFF f i j
  _     -> Nothing
liftFF  :: (Float# -> Float#) -> [Value] -> Maybe Term
liftFF  f args = case floatLiterals' args of
  [i]   -> Just $ runFF f i
  _     -> Nothing
runFFI :: (Float# -> Float# -> Int#) -> Word32 -> Word32 -> Term
runFFI f i j
  = let !(F# a) = castWord32ToFloat i
        !(F# b) = castWord32ToFloat j
        r = f a b
    in  Literal . IntLiteral . toInteger $ I# r
runFFF :: (Float# -> Float# -> Float#) -> Word32 -> Word32 -> Term
runFFF f i j
  = let !(F# a) = castWord32ToFloat i
        !(F# b) = castWord32ToFloat j
        r = f a b
    in  Literal . FloatLiteral . castFloatToWord32 $ F# r
runFF :: (Float# -> Float#) -> Word32 -> Term
runFF f i
  = let !(F# a) = castWord32ToFloat i
        r = f a
    in  Literal . FloatLiteral . castFloatToWord32 $ F# r

liftI8 :: (Int8# -> Int8# -> Int8#) -> [Value] -> Maybe Term
liftI8 f args = case int8Literals' args of
  [i,j] ->
    let !(I8# a) = fromInteger i
        !(I8# b) = fromInteger j
     in Just (Literal (Int8Literal (toInteger (I8# (f a b)))))
  _ -> Nothing

liftI8I :: (Int8# -> Int# -> Int8#) -> [Value] -> Maybe Term
liftI8I f args = case args of
  [Lit (Int8Literal i),Lit (IntLiteral j)] ->
    let !(I8# a) = fromInteger i
        !(I# b) = fromInteger j
     in Just (Literal (Int8Literal (toInteger (I8# (f a b)))))
  _ -> Nothing

liftI8RI :: (Int8# -> Int8# -> Int#) -> [Value] -> Maybe Term
liftI8RI f args = case int8Literals' args of
  [i,j] ->
    let !(I8# a) = fromInteger i
        !(I8# b) = fromInteger j
     in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftI16 :: (Int16# -> Int16# -> Int16#) -> [Value] -> Maybe Term
liftI16 f args = case int16Literals' args of
  [i,j] ->
    let !(I16# a) = fromInteger i
        !(I16# b) = fromInteger j
     in Just (Literal (Int16Literal (toInteger (I16# (f a b)))))
  _ -> Nothing

liftI16I :: (Int16# -> Int# -> Int16#) -> [Value] -> Maybe Term
liftI16I f args = case args of
  [Lit (Int16Literal i),Lit (IntLiteral j)] ->
    let !(I16# a) = fromInteger i
        !(I# b) = fromInteger j
     in Just (Literal (Int16Literal (toInteger (I16# (f a b)))))
  _ -> Nothing

liftI16RI :: (Int16# -> Int16# -> Int#) -> [Value] -> Maybe Term
liftI16RI f args = case int16Literals' args of
  [i,j] ->
    let !(I16# a) = fromInteger i
        !(I16# b) = fromInteger j
     in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftI32 :: (Int32# -> Int32# -> Int32#) -> [Value] -> Maybe Term
liftI32 f args = case int32Literals' args of
  [i,j] ->
    let !(I32# a) = fromInteger i
        !(I32# b) = fromInteger j
     in Just (Literal (Int32Literal (toInteger (I32# (f a b)))))
  _ -> Nothing

liftI32I :: (Int32# -> Int# -> Int32#) -> [Value] -> Maybe Term
liftI32I f args = case args of
  [Lit (Int32Literal i),Lit (IntLiteral j)] ->
    let !(I32# a) = fromInteger i
        !(I# b) = fromInteger j
     in Just (Literal (Int32Literal (toInteger (I32# (f a b)))))
  _ -> Nothing

liftI32RI :: (Int32# -> Int32# -> Int#) -> [Value] -> Maybe Term
liftI32RI f args = case int32Literals' args of
  [i,j] ->
    let !(I32# a) = fromInteger i
        !(I32# b) = fromInteger j
     in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftI64 :: (Int64# -> Int64# -> Int64#) -> [Value] -> Maybe Term
liftI64 f args = case int64Literals' args of
  [i,j] ->
    let !(I64# a) = fromInteger i
        !(I64# b) = fromInteger j
     in Just (Literal (Int64Literal (toInteger (I64# (f a b)))))
  _ -> Nothing

liftI64I :: (Int64# -> Int# -> Int64#) -> [Value] -> Maybe Term
liftI64I f args = case args of
  [Lit (Int64Literal i),Lit (IntLiteral j)] ->
    let !(I64# a) = fromInteger i
        !(I# b) = fromInteger j
     in Just (Literal (Int64Literal (toInteger (I64# (f a b)))))
  _ -> Nothing

liftI64RI :: (Int64# -> Int64# -> Int#) -> [Value] -> Maybe Term
liftI64RI f args = case int64Literals' args of
  [i,j] ->
    let !(I64# a) = fromInteger i
        !(I64# b) = fromInteger j
     in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftW8 :: (Word8# -> Word8# -> Word8#) -> [Value] -> Maybe Term
liftW8 f args = case word8Literals' args of
  [i,j] ->
    let !(W8# a) = fromInteger i
        !(W8# b) = fromInteger j
     in Just (Literal (Word8Literal (toInteger (W8# (f a b)))))
  _ -> Nothing

liftW8I :: (Word8# -> Int# -> Word8#) -> [Value] -> Maybe Term
liftW8I f args = case args of
  [Lit (Word8Literal i),Lit (IntLiteral j)] ->
    let !(W8# a) = fromInteger i
        !(I# b) = fromInteger j
     in Just (Literal (Word8Literal (toInteger (W8# (f a b)))))
  _ -> Nothing

liftW8RI :: (Word8# -> Word8# -> Int#) -> [Value] -> Maybe Term
liftW8RI f args = case word8Literals' args of
  [i,j] ->
    let !(W8# a) = fromInteger i
        !(W8# b) = fromInteger j
     in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftW16 :: (Word16# -> Word16# -> Word16#) -> [Value] -> Maybe Term
liftW16 f args = case word16Literals' args of
  [i,j] -> let !(W16# a) = fromInteger i
               !(W16# b) = fromInteger j
            in Just (Literal (Word16Literal (toInteger (W16# (f a b)))))
  _ -> Nothing

liftW16I :: (Word16# -> Int# -> Word16#) -> [Value] -> Maybe Term
liftW16I f args = case args of
  [Lit (Word16Literal i),Lit (IntLiteral j)] ->
    let !(W16# a) = fromInteger i
        !(I# b) = fromInteger j
     in Just (Literal (Word16Literal (toInteger (W16# (f a b)))))
  _ -> Nothing

liftW16RI :: (Word16# -> Word16# -> Int#) -> [Value] -> Maybe Term
liftW16RI f args = case word16Literals' args of
  [i,j] ->
    let !(W16# a) = fromInteger i
        !(W16# b) = fromInteger j
     in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftW32 :: (Word32# -> Word32# -> Word32#) -> [Value] -> Maybe Term
liftW32 f args = case word32Literals' args of
  [i,j] -> let !(W32# a) = fromInteger i
               !(W32# b) = fromInteger j
            in Just (Literal (Word32Literal (toInteger (W32# (f a b)))))
  _ -> Nothing

liftW32I :: (Word32# -> Int# -> Word32#) -> [Value] -> Maybe Term
liftW32I f args = case args of
  [Lit (Word32Literal i),Lit (IntLiteral j)] ->
    let !(W32# a) = fromInteger i
        !(I# b) = fromInteger j
     in Just (Literal (Word32Literal (toInteger (W32# (f a b)))))
  _ -> Nothing

liftW32RI :: (Word32# -> Word32# -> Int#) -> [Value] -> Maybe Term
liftW32RI f args = case word32Literals' args of
  [i,j] ->
    let !(W32# a) = fromInteger i
        !(W32# b) = fromInteger j
     in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftW64 :: (Word64# -> Word64# -> Word64#) -> [Value] -> Maybe Term
liftW64 f args = case word64Literals' args of
  [i,j] -> let !(W64# a) = fromInteger i
               !(W64# b) = fromInteger j
            in Just (Literal (Word64Literal (toInteger (W64# (f a b)))))
  _ -> Nothing

liftW64I :: (Word64# -> Int# -> Word64#) -> [Value] -> Maybe Term
liftW64I f args = case args of
  [Lit (Word64Literal i),Lit (IntLiteral j)] ->
    let !(W64# a) = fromInteger i
        !(I# b) = fromInteger j
     in Just (Literal (Word64Literal (toInteger (W64# (f a b)))))
  _ -> Nothing

liftW64RI :: (Word64# -> Word64# -> Int#) -> [Value] -> Maybe Term
liftW64RI f args = case word64Literals' args of
  [i,j] ->
    let !(W64# a) = fromInteger i
        !(W64# b) = fromInteger j
     in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

splitAtPrim
  :: TyConName
  -- ^ SNat TyCon name
  -> TyConName
  -- ^ Vec TyCon name
  -> Term
splitAtPrim snatTcNm vecTcNm =
  Prim (PrimInfo (showt 'Clash.Sized.Vector.splitAt) (splitAtTy snatTcNm vecTcNm) WorkNever SingleResult NoUnfolding)

splitAtTy
  :: TyConName
  -- ^ SNat TyCon name
  -> TyConName
  -- ^ Vec TyCon name
  -> Type
splitAtTy snatNm vecNm =
  ForAllTy mTV (
  ForAllTy nTV (
  ForAllTy aTV (
  mkFunTy
    (mkTyConApp snatNm [VarTy mTV])
    (mkFunTy
      (mkTyConApp vecNm
                  [mkTyConApp typeNatAdd
                    [VarTy mTV
                    ,VarTy nTV]
                  ,VarTy aTV])
      (mkTyConApp tupNm
                  [mkTyConApp vecNm
                              [VarTy mTV
                              ,VarTy aTV]
                  ,mkTyConApp vecNm
                              [VarTy nTV
                              ,VarTy aTV]])))))
  where
    mTV   = mkTyVar typeNatKind (mkUnsafeSystemName "m" 0)
    nTV   = mkTyVar typeNatKind (mkUnsafeSystemName "n" 1)
    aTV   = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 2)
    tupNm = ghcTyconToTyConName (tupleTyCon Boxed 2)

foldSplitAtTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
foldSplitAtTy vecNm =
  ForAllTy mTV (
  ForAllTy nTV (
  ForAllTy aTV (
  mkFunTy
    naturalPrimTy
    (mkFunTy
      (mkTyConApp vecNm
                  [mkTyConApp typeNatAdd
                    [VarTy mTV
                    ,VarTy nTV]
                  ,VarTy aTV])
      (mkTyConApp tupNm
                  [mkTyConApp vecNm
                              [VarTy mTV
                              ,VarTy aTV]
                  ,mkTyConApp vecNm
                              [VarTy nTV
                              ,VarTy aTV]])))))
  where
    mTV   = mkTyVar typeNatKind (mkUnsafeSystemName "m" 0)
    nTV   = mkTyVar typeNatKind (mkUnsafeSystemName "n" 1)
    aTV   = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 2)
    tupNm = ghcTyconToTyConName (tupleTyCon Boxed 2)

vecAppendPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecAppendPrim vecNm =
  Prim (PrimInfo (showt '(Clash.Sized.Vector.++)) (vecAppendTy vecNm) WorkNever SingleResult NoUnfolding)

vecAppendTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
vecAppendTy vecNm =
    ForAllTy nTV (
    ForAllTy aTV (
    ForAllTy mTV (
    mkFunTy
      (mkTyConApp vecNm [VarTy nTV
                        ,VarTy aTV
                        ])
      (mkFunTy
         (mkTyConApp vecNm [VarTy mTV
                           ,VarTy aTV
                           ])
         (mkTyConApp vecNm [mkTyConApp typeNatAdd
                              [VarTy nTV
                              ,VarTy mTV]
                           ,VarTy aTV
                           ])))))
  where
    nTV = mkTyVar typeNatKind (mkUnsafeSystemName "n" 0)
    aTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 1)
    mTV = mkTyVar typeNatKind (mkUnsafeSystemName "m" 2)

vecZipWithPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecZipWithPrim vecNm =
  Prim (PrimInfo (showt 'Clash.Sized.Vector.zipWith) (vecZipWithTy vecNm) WorkNever SingleResult NoUnfolding)

vecZipWithTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
vecZipWithTy vecNm =
  ForAllTy aTV (
  ForAllTy bTV (
  ForAllTy cTV (
  ForAllTy nTV (
  mkFunTy
    (mkFunTy aTy (mkFunTy bTy cTy))
    (mkFunTy
      (mkTyConApp vecNm [nTy,aTy])
      (mkFunTy
        (mkTyConApp vecNm [nTy,bTy])
        (mkTyConApp vecNm [nTy,cTy])))))))
  where
    aTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 0)
    bTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "b" 1)
    cTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "c" 2)
    nTV = mkTyVar typeNatKind (mkUnsafeSystemName "n" 3)
    aTy = VarTy aTV
    bTy = VarTy bTV
    cTy = VarTy cTV
    nTy = VarTy nTV

vecImapGoTy
  :: TyConName
  -- ^ Vec TyCon name
  -> TyConName
  -- ^ Index TyCon name
  -> Type
vecImapGoTy vecTcNm indexTcNm =
  ForAllTy nTV (
  ForAllTy mTV (
  ForAllTy aTV (
  ForAllTy bTV (
  mkFunTy fTy
       (mkFunTy vecATy (mkFunTy indexTy vecBTy))))))
  where
    nTV = mkTyVar typeNatKind (mkUnsafeSystemName "n" 0)
    mTV = mkTyVar typeNatKind (mkUnsafeSystemName "m" 1)
    aTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 2)
    bTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "b" 3)
    indexTy = mkTyConApp indexTcNm [nTy]
    nTy = VarTy nTV
    mTy = VarTy mTV
    fTy = mkFunTy indexTy (mkFunTy aTy bTy)
    aTy = VarTy aTV
    bTy = VarTy bTV
    vecATy = mkTyConApp vecTcNm [mTy,aTy]
    vecBTy = mkTyConApp vecTcNm [mTy,bTy]

indexAddTy
  :: TyConName
  -- ^ Index TyCon name
  -> Type
indexAddTy indexTcNm =
  ForAllTy nTV (
  mkFunTy naturalPrimTy (mkFunTy indexTy (mkFunTy indexTy indexTy)))
  where
    nTV     = mkTyVar typeNatKind (mkUnsafeSystemName "n" 0)
    indexTy = mkTyConApp indexTcNm [VarTy nTV]

bvAppendPrim
  :: TyConName
  -- ^ BitVector TyCon Name
  -> Term
bvAppendPrim bvTcNm =
  Prim (PrimInfo (showt '(Clash.Sized.Internal.BitVector.++#)) (bvAppendTy bvTcNm) WorkNever SingleResult NoUnfolding)

bvAppendTy
  :: TyConName
  -- ^ BitVector TyCon Name
  -> Type
bvAppendTy bvNm =
  ForAllTy mTV (
  ForAllTy nTV (
  mkFunTy naturalPrimTy (mkFunTy
    (mkTyConApp bvNm [VarTy nTV])
    (mkFunTy
      (mkTyConApp bvNm [VarTy mTV])
      (mkTyConApp bvNm [mkTyConApp typeNatAdd
                          [VarTy nTV
                          ,VarTy mTV]])))))
  where
    mTV = mkTyVar typeNatKind (mkUnsafeSystemName "m" 0)
    nTV = mkTyVar typeNatKind (mkUnsafeSystemName "n" 1)

bvSplitPrim
  :: TyConName
  -- ^ BitVector TyCon Name
  -> Term
bvSplitPrim bvTcNm =
  Prim (PrimInfo (showt 'Clash.Sized.Internal.BitVector.split#) (bvSplitTy bvTcNm) WorkNever SingleResult NoUnfolding)

bvSplitTy
  :: TyConName
  -- ^ BitVector TyCon Name
  -> Type
bvSplitTy bvNm =
  ForAllTy nTV (
  ForAllTy mTV (
  mkFunTy naturalPrimTy (mkFunTy
    (mkTyConApp bvNm [mkTyConApp typeNatAdd
                                 [VarTy mTV
                                 ,VarTy nTV]])
    (mkTyConApp tupNm [mkTyConApp bvNm [VarTy mTV]
                      ,mkTyConApp bvNm [VarTy nTV]]))))
  where
    nTV   = mkTyVar typeNatKind (mkUnsafeSystemName "n" 0)
    mTV   = mkTyVar typeNatKind (mkUnsafeSystemName "m" 1)
    tupNm = ghcTyconToTyConName (tupleTyCon Boxed 2)

ghcTyconToTyConName
  :: TyCon.TyCon
  -> TyConName
ghcTyconToTyConName tc =
    Name User n' (fromGhcUnique (TyCon.tyConUnique tc)) (getSrcSpan n)
  where
    n'      = fromMaybe "_INTERNAL_" (modNameM n) `Text.append`
              ('.' `Text.cons` Text.pack occName)
    occName = occNameString $ nameOccName n
    n       = TyCon.tyConName tc

svoid :: (State# RealWorld -> State# RealWorld) -> IO ()
svoid m0 = IO (\s -> case m0 s of s' -> (# s', () #))

isTrueDC,isFalseDC :: DataCon -> Bool
isTrueDC dc  = dcUniq dc == fromGhcUnique trueDataConKey
isFalseDC dc = dcUniq dc == fromGhcUnique falseDataConKey
