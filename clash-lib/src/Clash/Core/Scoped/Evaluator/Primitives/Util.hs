{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Shared helpers of the reductions of primitives of the scoped evaluator
  ("Clash.Core.Scoped.Evaluator"): ports of those of
  @Clash.GHC.Evaluator.Primitive.Util@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how reductions are registered,
  and how to port one.

  Names and argument orders are those of @Clash.GHC.Evaluator.Primitive.Util@,
  so that a reduction ports with as few changes as possible. The differences:

  * __Types are scoped.__ The type arguments of a primitive ('tys') and its
    type ('ty') live in the scope of the machine ('sc'). Helpers that look at
    types take the t'TypeEnv' ('te') and, if they need it, the scope, where
    the current ones take a 'Clash.Core.TyCon.TyConMap' (@tcm@).

  * __Results are scoped terms__, in any scope: helpers that build literals
    and values are polymorphic in their scope.

  * __Steps can extend the scope.__ A reduction gives a @'Next' o n@: the
    machine after the step, possibly in an extension of the scope of the
    primitive, e.g. after a nested evaluation ('whnfArg') or an allocation
    ('Clash.Core.Scoped.Evaluator.newLetBinding'). Use 'reduceWith' to
    continue with a term in such a machine.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.Core.Scoped.Evaluator.Primitives.Util
  ( -- * Entries
    PrimStepContext (..)
  , primStepEntry
  , mkPrimStepContext
    -- * Lifting functions on literals
  , liftLit1
  , liftLit2
    -- * Unpacking values
  , pairOf
  , listOf
  , integerLiteral
  , integerLiterals
  , integerLiterals'
  , naturalLiteral
  , naturalLiterals
  , naturalLiterals'
  , intLiteral
  , intLiterals
  , intLiterals'
  , intCLiteral
  , intCLiterals
  , intCLiterals'
  , int8Literal
  , int8Literals'
  , int16Literal
  , int16Literals'
  , int32Literal
  , int32Literals'
  , int64Literal
  , int64Literals'
  , wordLiteral
  , wordLiterals
  , wordLiterals'
  , word8Literal
  , word8Literals'
  , word16Literal
  , word16Literals'
  , word32Literal
  , word32Literals'
  , word64Literal
  , word64Literals'
  , charLiteral
  , charLiterals
  , charLiterals'
  , doubleLiteral
  , doubleLiterals'
  , floatLiteral
  , floatLiterals'
  , boolLiteral
  , sizedLiteral
  , sizedLiterals
  , sizedLiterals'
  , bitLiterals
  , indexLiterals
  , indexLiterals'
  , signedLiterals
  , signedLiterals'
  , unsignedLiterals
  , unsignedLiterals'
  , bitVectorLiteral
  , bitVectorLiterals'
  , valArgs
  , vecConsArgs
  , sizedLitIntLit
  , signedLitIntLit
  , unsignedLitIntLit
  , bitVectorLitIntLit
  , toBV
  , splitBV
  , toBit
  , wrapUnsigned
  , wrapSigned
    -- * Building literals and values
  , boolToIntLiteral
  , boolToBoolLiteral
  , orderingToOrderingLiteral
  , charToCharLiteral
  , integerToIntLiteral
  , integerToWordLiteral
  , integerToInt64Literal
  , integerToWord64Literal
  , integerToIntegerLiteral
  , naturalToNaturalLiteral
  , mkIntCLit
  , mkFloatCLit
  , mkDoubleCLit
  , mkSomeNat
  , mkSizedLit
  , mkBitLit
  , mkSignedLit
  , mkUnsignedLit
  , mkBitVectorLit
  , mkBitVectorLit'
  , mkIndexLitE
  , mkIndexLit
  , mkIndexLit'
  , mkResultCon
  , resultDataCon
  , mkVec
  , mkVecCons
  , mkVecNil
  , appendToVec
  , mkRTree
    -- * Types
  , getResultTy
  , extractKnownNat
  , extractKnownNats
  , extractTySizeInfo
  , tyNatSize
  , numTy
  , primArgResTys
  , SomeType (..)
  , funArgTys
  , tyConNameOf
  , tyConAppOf
    -- * Lifting operations on sized types
  , liftUnsigned2
  , liftSigned2
  , liftBitVector2
  , liftBitVector2Bool
  , liftInteger2BitVector
  , liftBitVector2CInt
  , liftSized2
  , runSizedF
    -- * Lifting operations on unboxed numbers
  , liftDDI
  , liftDDD
  , liftDD
  , runDDI
  , runDDD
  , runDD
  , liftFFI
  , liftFFF
  , liftFF
  , runFFI
  , runFFF
  , runFF
  , liftI8
  , liftI8I
  , liftI8RI
  , liftI16
  , liftI16I
  , liftI16RI
  , liftI32
  , liftI32I
  , liftI32RI
  , liftI64
  , liftI64I
  , liftI64RI
  , liftW8
  , liftW8I
  , liftW8RI
  , liftW16
  , liftW16I
  , liftW16RI
  , liftW32
  , liftW32I
  , liftW32RI
  , liftW64
  , liftW64I
  , liftW64RI
  , powImplWorker
    -- * Primitives in results
  , closedPrim
  , tyConB
  , forAllNatB
  , forAllTypeB
  , bConPrim
  , bvConPrim
  , indexConPrim
  , signedConPrim
  , unsignedConPrim
  , splitAtPrim
  , splitAtTy
  , foldSplitAtTy
  , vecAppendPrim
  , vecAppendTy
  , vecZipWithPrim
  , vecZipWithTy
  , vecImapGoTy
  , indexAddTy
  , bvAppendPrim
  , bvAppendTy
  , bvSplitPrim
  , bvSplitTy
  , vecHeadPrim
  , vecLastPrim
  , vecTailPrim
  , vecInitPrim
  , tupleTyConName
    -- * The machine
  , setTerm
  , stackClear
    -- ** The primitive heap
  , primCount
  , primLookup
  , primInsert
  , primUpdate
  , svoid
    -- * Data constructors
  , isTrueDC
  , isFalseDC
  ) where

import Control.DeepSeq (force)
import Control.Exception (ArithException (..), ErrorCall, Exception, evaluate, tryJust)
import Control.Monad.Trans.Except (runExcept)
import Data.Bits
import qualified Data.Either as Either
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (mapMaybe)
import qualified Data.Primitive.ByteArray as BA
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Extra (showt)
import GHC.Float
import GHC.Int
import GHC.Natural
import GHC.Num.Integer (Integer (..))
import GHC.Prim
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat)
import GHC.Types (IO (..))
import GHC.Word
import System.IO.Unsafe (unsafeDupablePerformIO)

import GHC.Builtin.Names (falseDataConKey, trueDataConKey)
import GHC.Builtin.Types (tupleTyCon)
import qualified GHC.Core.TyCon as GhcTyCon
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Name (getSrcSpan, nameModule_maybe, nameOccName, occNameString)
import GHC.Unit.Module (moduleName, moduleNameString)

import Clash.Core.DataCon (DataCon (..))
import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Term as C
import Clash.Core.TyCon (TyConName, tyConDataCons)
import qualified Clash.Core.Type as C
import Clash.Debug (trace)
import qualified Clash.Normalize.Scoped.Primitives as NP
import Clash.Normalize.PrimitiveReductions (typeNatAdd)
import Clash.Sized.Internal.BitVector (Bit (..), BitVector (..))
import Clash.Sized.Internal.Signed (Signed (..))
import Clash.Sized.Internal.Unsigned (Unsigned (..))
import Clash.Unique (fromGhcUnique)
import Clash.Util (curLoc)

import qualified Clash.Sized.Internal.BitVector
import qualified Clash.Sized.Internal.Index
import qualified Clash.Sized.Internal.Signed
import qualified Clash.Sized.Internal.Unsigned
import qualified Clash.Sized.Vector

import Clash.Core.Scoped.Builder
  (Key, TyB, forAllTyB, funTyB, litTyB, runClosedType, tyConAppB, varTyB)
import Clash.Core.Scoped.Convert (fromClashTermClosedIn, toClashTerm, mkPrimInfo)
import Clash.Core.Scoped.TysPrim
  (integerPrimTyConName, liftedTypeKindTyConName, naturalPrimTyConName,
   typeNatKindTyConName, wordPrimTyConName)
import Clash.Core.Scoped.Evaluator
  (integerLiteral, naturalLiteral, reduceIn, whnf, wrapSigned, wrapUnsigned)
import qualified Clash.Core.Scoped.Evaluator as E
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.HasType (piResultTys, primInfoType)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (mkApps)
import Clash.Core.Scoped.Type
  (PiTele (..), SplitFunForall (..), TypeView (..), mkTyConApp, splitFunForallTy, tyNatSize, tyView)
import Clash.Core.Scoped.TypeEnv (TypeEnv, lookupTyCon)
import Clash.Core.Scoped.Util (appendToVec, mkRTree, mkVec)

-- * Entries

-- | The arguments of a reduction, and helpers that use them. See
-- @Clash.GHC.Evaluator.Primitive.Util.PrimStepContext@, of which this has the
-- fields, except that @tcm@ is 'te' and 'sc'. Reductions match on it with
-- @RecordWildCards@.
data PrimStepContext o n = PrimStepContext
  { te :: TypeEnv
  , sc :: Scope n
  -- ^ The scope of the machine, of the types and arguments
  , evalEnv :: Env
  -- ^ For nested evaluation ('whnf') and allocation
  -- ('Clash.Core.Scoped.Evaluator.newLetBinding')
  , isSubj :: Bool
  -- ^ Whether the primitive is the subject of a case-expression, or an
  -- argument of a primitive, see 'forcePrims'
  , pInfo :: PrimInfo
  , tys :: [Type n]
  , args :: [Value n]
  , mach :: Machine o n
  , ty :: Type n
  -- ^ The type of the primitive, see 'primInfoType'
  , checkNaturalRange1 :: Type n -> Integer -> (Natural -> Natural) -> Term n
  , checkNaturalRange2 :: Type n -> Integer -> Integer -> (Natural -> Natural -> Natural) -> Term n
  , checkNaturalRange :: Type n -> [Integer] -> ([Natural] -> Term n) -> Term n
  -- ^ Build a term from naturals, or an undefined value of the given type if
  -- one of the integers is negative
  , reduce :: Term n -> Maybe (Next o n)
  -- ^ Continue with a term, see 'E.reduce'
  , reduceWith :: forall l. DExt n l => Machine o l -> Term l -> Maybe (Next o n)
  -- ^ Continue with a term in the given machine, an extension of 'mach', e.g.
  -- after an allocation. See 'reduceIn'.
  , reduceWHNF :: Term n -> Maybe (Next o n)
  -- ^ Continue with the WHNF of a term, see 'E.reduceWHNF'
  , reduceWHNF' :: forall l. DExt n l => Machine o l -> Term l -> Maybe (Next o n)
  -- ^ Continue with the WHNF of a term in the given machine, an extension of
  -- 'mach'
  , whnfArg :: forall r. Term n -> (forall l. DExt n l => Machine o l -> Term l -> r) -> r
  -- ^ Evaluate a term to WHNF with a nested machine, as the subject of a
  -- case-expression. The continuation gets that machine, with the stack of
  -- 'mach', and the WHNF. Continue in that machine, e.g. with 'reduceWith':
  -- the WHNF can mention the bindings it allocated.
  , whnfIn
      :: forall l r. DExt n l
      => Machine o l -> Term l
      -> (forall l'. (DExt l l', DExt n l') => Machine o l' -> Term l' -> r) -> r
  -- ^ 'whnfArg' in the given machine, an extension of 'mach', e.g. that of a
  -- previous 'whnfArg'
  , catchDivByZero :: Term n -> Term n
  -- ^ The term, or an undefined value of the result type if forcing it
  -- throws 'DivideByZero'
  , catchErrorCall :: Term n -> Term n
  -- ^ The term, or an undefined value of the result type if forcing it
  -- throws an 'ErrorCall'
  }

-- | The context of a reduction
mkPrimStepContext :: forall o n. Distinct n => PrimContext o n -> PrimStepContext o n
mkPrimStepContext pc = PrimStepContext{..}
 where
  evalEnv = pcEnv pc
  te = envTypeEnv evalEnv
  mach = pcMachine pc
  sc = mScope mach
  isSubj = pcIsSubj pc
  pInfo = pcInfo pc
  tys = pcTys pc
  args = pcArgs pc
  ty = primInfoType pInfo

  checkNaturalRange1 nTy i f =
    checkNaturalRange nTy [i] (\case [i'] -> naturalToNaturalLiteral (f i'); _ -> unreachable)

  checkNaturalRange2 nTy i j f =
    checkNaturalRange nTy [i, j]
      (\case [i', j'] -> naturalToNaturalLiteral (f i' j'); _ -> unreachable)

  checkNaturalRange nTy natsAsInts f
    | any (< 0) natsAsInts = TyApp (Prim NP.undefined) nTy
    | otherwise = f (map fromInteger natsAsInts)

  unreachable :: a
  unreachable = error ($(curLoc) ++ "unreachable")

  reduce = E.reduce pc

  reduceWith :: forall l. DExt n l => Machine o l -> Term l -> Maybe (Next o n)
  reduceWith = reduceIn pc

  reduceWHNF = E.reduceWHNF pc

  reduceWHNF' :: forall l. DExt n l => Machine o l -> Term l -> Maybe (Next o n)
  reduceWHNF' m1 e =
    whnf evalEnv isSubj m1 {mStack = [], mTerm = e} $ \(m2 :: Machine o l2) ->
      withExtTrans @n @l @l2 (Just (Next m2 {mStack = sink1 (mStack mach)}))

  whnfArg :: forall r. Term n -> (forall l. DExt n l => Machine o l -> Term l -> r) -> r
  whnfArg e k = whnfIn mach e k

  whnfIn
    :: forall l r. DExt n l
    => Machine o l -> Term l
    -> (forall l'. (DExt l l', DExt n l') => Machine o l' -> Term l' -> r) -> r
  whnfIn m1 e k =
    whnf evalEnv True m1 {mStack = [], mTerm = e} $ \(m2 :: Machine o l2) ->
      withExtTrans @n @l @l2 (k m2 {mStack = sink1 (mStack mach)} (mTerm m2))

  makeUndefinedIf :: Exception e => (e -> Bool) -> Term n -> Term n
  makeUndefinedIf wantToHandle tm =
    case unsafeDupablePerformIO $ tryJust selectException (evaluate $ force $ toClashTerm tm) of
      Right _ -> tm
      Left e -> trace (msg e) (TyApp (Prim NP.undefined) (getResultTy te sc ty tys))
   where
    selectException e
      | wantToHandle e = Just e
      | otherwise = Nothing
    msg e = unlines
      [ "Warning: caught exception: \"" ++ show e ++ "\" while trying to evaluate: "
      , showPpr (mkApps (Prim pInfo) (map (Left . valToTerm) args)) ]

  catchDivByZero = makeUndefinedIf (== DivideByZero)
  catchErrorCall = makeUndefinedIf (const True :: ErrorCall -> Bool)

-- | Associate the name of a primitive with its reduction. See
-- @Clash.GHC.Evaluator.Primitive.Util.primStepEntry@.
primStepEntry
  :: Text
  -> (forall o n. Distinct n => PrimStepContext o n -> Maybe (Next o n))
  -> (Text, PrimReduction)
primStepEntry name impl = (name, PrimReduction (impl . mkPrimStepContext))
{-# INLINE primStepEntry #-}

-- * Lifting functions on literals

-- | The reduction of a primitive of one argument that only looks at its
-- literal, e.g.
--
-- @
-- liftLit1 \$(textNameLit 'GHC.Prim.ord#) charLiteral (integerToIntLiteral . toInteger . ord)
-- @
--
-- It doesn't reduce if the argument is not a literal of the given kind.
liftLit1
  :: Text
  -> (forall n. Value n -> Maybe a)
  -- ^ The literal of an argument, e.g. 'intLiteral'
  -> (forall n. a -> Term n)
  -- ^ The result, e.g. @'integerToIntLiteral' . negate@
  -> (Text, PrimReduction)
liftLit1 name lit f = primStepEntry name $ \PrimStepContext{..} -> case args of
  [lit -> Just i] -> reduce (f i)
  _ -> Nothing

-- | The reduction of a primitive of two arguments that only looks at their
-- literals, e.g.
--
-- @
-- liftLit2 \$(textNameLit '(GHC.Prim.+#)) intLiteral (\\i j -> integerToIntLiteral (i + j))
-- @
--
-- It doesn't reduce if an argument is not a literal of the given kind. The
-- result is checked for 'DivideByZero', see 'catchDivByZero'.
liftLit2
  :: Text
  -> (forall n. Value n -> Maybe a)
  -- ^ The literal of an argument, e.g. 'intLiteral'
  -> (forall n. a -> a -> Term n)
  -- ^ The result, e.g. @\\i j -> 'integerToIntLiteral' (i + j)@
  -> (Text, PrimReduction)
liftLit2 name lit f = primStepEntry name $ \PrimStepContext{..} -> case args of
  [lit -> Just i, lit -> Just j] -> reduce (catchDivByZero (f i j))
  _ -> Nothing

-- * Unpacking values

pairOf :: (Value n -> Maybe a) -> [Value n] -> Maybe (a, a)
pairOf f [x, y] = (,) <$> f x <*> f y
pairOf _ _ = Nothing

listOf :: (Value n -> Maybe a) -> [Value n] -> [a]
listOf = mapMaybe

integerLiterals :: [Value n] -> Maybe (Integer, Integer)
integerLiterals = pairOf integerLiteral

integerLiterals' :: [Value n] -> [Integer]
integerLiterals' = listOf integerLiteral

naturalLiterals :: [Value n] -> Maybe (Integer, Integer)
naturalLiterals = pairOf naturalLiteral

naturalLiterals' :: [Value n] -> [Integer]
naturalLiterals' = listOf naturalLiteral

intLiteral :: Value n -> Maybe Integer
intLiteral = \case
  Lit (IntLiteral i) -> Just i
  _ -> Nothing

intLiterals :: [Value n] -> Maybe (Integer, Integer)
intLiterals = pairOf intLiteral

intLiterals' :: [Value n] -> [Integer]
intLiterals' = listOf intLiteral

-- | The literal of a boxed number, e.g. an 'Int'
intCLiteral :: Value n -> Maybe Integer
intCLiteral = \case
  DC _ [Left (Literal (IntLiteral i))] -> Just i
  _ -> Nothing

intCLiterals :: [Value n] -> Maybe (Integer, Integer)
intCLiterals = pairOf intCLiteral

intCLiterals' :: [Value n] -> [Integer]
intCLiterals' = listOf intCLiteral

int8Literal :: Value n -> Maybe Integer
int8Literal = \case
  Lit (Int8Literal i) -> Just i
  _ -> Nothing

int8Literals' :: [Value n] -> [Integer]
int8Literals' = listOf int8Literal

int16Literal :: Value n -> Maybe Integer
int16Literal = \case
  Lit (Int16Literal i) -> Just i
  _ -> Nothing

int16Literals' :: [Value n] -> [Integer]
int16Literals' = listOf int16Literal

int32Literal :: Value n -> Maybe Integer
int32Literal = \case
  Lit (Int32Literal i) -> Just i
  _ -> Nothing

int32Literals' :: [Value n] -> [Integer]
int32Literals' = listOf int32Literal

int64Literal :: Value n -> Maybe Integer
int64Literal = \case
  Lit (Int64Literal i) -> Just i
  _ -> Nothing

int64Literals' :: [Value n] -> [Integer]
int64Literals' = listOf int64Literal

wordLiteral :: Value n -> Maybe Integer
wordLiteral = \case
  Lit (WordLiteral i) -> Just i
  _ -> Nothing

wordLiterals :: [Value n] -> Maybe (Integer, Integer)
wordLiterals = pairOf wordLiteral

wordLiterals' :: [Value n] -> [Integer]
wordLiterals' = listOf wordLiteral

word8Literal :: Value n -> Maybe Integer
word8Literal = \case
  Lit (Word8Literal i) -> Just i
  _ -> Nothing

word8Literals' :: [Value n] -> [Integer]
word8Literals' = listOf word8Literal

word16Literal :: Value n -> Maybe Integer
word16Literal = \case
  Lit (Word16Literal i) -> Just i
  _ -> Nothing

word16Literals' :: [Value n] -> [Integer]
word16Literals' = listOf word16Literal

word32Literal :: Value n -> Maybe Integer
word32Literal = \case
  Lit (Word32Literal i) -> Just i
  _ -> Nothing

word32Literals' :: [Value n] -> [Integer]
word32Literals' = listOf word32Literal

word64Literal :: Value n -> Maybe Integer
word64Literal = \case
  Lit (Word64Literal i) -> Just i
  _ -> Nothing

word64Literals' :: [Value n] -> [Integer]
word64Literals' = listOf word64Literal

charLiteral :: Value n -> Maybe Char
charLiteral = \case
  Lit (CharLiteral c) -> Just c
  _ -> Nothing

charLiterals :: [Value n] -> Maybe (Char, Char)
charLiterals = pairOf charLiteral

charLiterals' :: [Value n] -> [Char]
charLiterals' = listOf charLiteral

doubleLiteral :: Value n -> Maybe Word64
doubleLiteral = \case
  Lit (DoubleLiteral i) -> Just i
  _ -> Nothing

doubleLiterals' :: [Value n] -> [Word64]
doubleLiterals' = listOf doubleLiteral

floatLiteral :: Value n -> Maybe Word32
floatLiteral = \case
  Lit (FloatLiteral i) -> Just i
  _ -> Nothing

floatLiterals' :: [Value n] -> [Word32]
floatLiterals' = listOf floatLiteral

-- | A constructor of 'Bool'
boolLiteral :: Value n -> Maybe Bool
boolLiteral = \case
  DC dc []
    | isTrueDC dc -> Just True
    | isFalseDC dc -> Just False
  _ -> Nothing

-- | The integer of a value of a sized type with the given @fromInteger#@
-- primitive, e.g. 'Clash.Sized.Internal.Signed.fromInteger#'
sizedLiteral :: Text -> Value n -> Maybe Integer
sizedLiteral szCon = \case
  PrimVal p _ [_, Lit (IntegerLiteral i)]
    | primName p == szCon -> Just i
  _ -> Nothing

sizedLiterals :: Text -> [Value n] -> Maybe (Integer, Integer)
sizedLiterals szCon = pairOf (sizedLiteral szCon)

sizedLiterals' :: Text -> [Value n] -> [Integer]
sizedLiterals' szCon = listOf (sizedLiteral szCon)

-- | The masks and values of the 'Bit's among the values
bitLiterals :: [Value n] -> [(Integer, Integer)]
bitLiterals = map normalizeBit . mapMaybe go
 where
  normalizeBit (msk, v) = (msk .&. 1, v .&. 1)
  go = \case
    PrimVal p _ [Lit (WordLiteral m), Lit (IntegerLiteral i)]
      | primName p == showt 'Clash.Sized.Internal.BitVector.fromInteger##
      -> Just (m, i)
    _ -> Nothing

indexLiterals, signedLiterals, unsignedLiterals :: [Value n] -> Maybe (Integer, Integer)
indexLiterals = sizedLiterals (showt 'Clash.Sized.Internal.Index.fromInteger#)
signedLiterals = sizedLiterals (showt 'Clash.Sized.Internal.Signed.fromInteger#)
unsignedLiterals = sizedLiterals (showt 'Clash.Sized.Internal.Unsigned.fromInteger#)

indexLiterals', signedLiterals', unsignedLiterals' :: [Value n] -> [Integer]
indexLiterals' = sizedLiterals' (showt 'Clash.Sized.Internal.Index.fromInteger#)
signedLiterals' = sizedLiterals' (showt 'Clash.Sized.Internal.Signed.fromInteger#)
unsignedLiterals' = sizedLiterals' (showt 'Clash.Sized.Internal.Unsigned.fromInteger#)

-- | The mask and value of a 'BitVector'
bitVectorLiteral :: Value n -> Maybe (Integer, Integer)
bitVectorLiteral = \case
  PrimVal p _ [_, Lit (NaturalLiteral m), Lit (IntegerLiteral i)]
    | primName p == showt 'Clash.Sized.Internal.BitVector.fromInteger# -> Just (m, i)
  _ -> Nothing

bitVectorLiterals' :: [Value n] -> [(Integer, Integer)]
bitVectorLiterals' = listOf bitVectorLiteral

-- | The term arguments of a primitive value or a constructor application
valArgs :: Value n -> Maybe [Term n]
valArgs = \case
  PrimVal _ _ vs -> Just (fmap valToTerm vs)
  DC _ as -> Just (Either.lefts as)
  _ -> Nothing

-- | The head and the tail of a vector built by @Cons@. The machine evaluates
-- a vector to a constructor application, whose fields are terms that need
-- not be values.
vecConsArgs :: Value n -> Maybe (Term n, Term n)
vecConsArgs = \case
  DC _ (Either.lefts -> [_co, x, xs]) -> Just (x, xs)
  _ -> Nothing

-- | Match the literal arguments of a function like
--
-- @
-- Unsigned.shiftL# :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
-- @
sizedLitIntLit
  :: Distinct n
  => Text -> TypeEnv -> Scope n -> [Type n] -> [Value n]
  -> Maybe (Type n, Integer, Integer, Integer)
sizedLitIntLit szCon tenv scope tyArgs vals
  | Just (nTy, kn) <- extractKnownNat tenv scope tyArgs
  , [ _
    , PrimVal p _ [_, Lit (IntegerLiteral i)]
    , valArgs -> Just [Literal (IntLiteral j)]
    ] <- vals
  , primName p == szCon
  = Just (nTy, kn, i, j)
  | otherwise
  = Nothing

signedLitIntLit, unsignedLitIntLit
  :: Distinct n
  => TypeEnv -> Scope n -> [Type n] -> [Value n]
  -> Maybe (Type n, Integer, Integer, Integer)
signedLitIntLit = sizedLitIntLit (showt 'Clash.Sized.Internal.Signed.fromInteger#)
unsignedLitIntLit = sizedLitIntLit (showt 'Clash.Sized.Internal.Unsigned.fromInteger#)

bitVectorLitIntLit
  :: Distinct n
  => TypeEnv -> Scope n -> [Type n] -> [Value n]
  -> Maybe (Type n, Integer, (Integer, Integer), Integer)
bitVectorLitIntLit tenv scope tyArgs vals
  | Just (nTy, kn) <- extractKnownNat tenv scope tyArgs
  , [ _
    , PrimVal p _ [_, Lit (NaturalLiteral m), Lit (IntegerLiteral i)]
    , valArgs -> Just [Literal (IntLiteral j)]
    ] <- vals
  , primName p == showt 'Clash.Sized.Internal.BitVector.fromInteger#
  = Just (nTy, kn, (m, i), j)
  | otherwise
  = Nothing

toBV :: (Integer, Integer) -> BitVector n
toBV (mask, val) = BV (fromInteger mask) (fromInteger val)

splitBV :: BitVector n -> (Integer, Integer)
splitBV (BV msk val) = (toInteger msk, toInteger val)

toBit :: (Integer, Integer) -> Bit
toBit (mask, val) = Bit (fromInteger mask) (fromInteger val)

-- * Building literals and values

boolToIntLiteral :: Bool -> Term n
boolToIntLiteral b = Literal (IntLiteral (if b then 1 else 0))

-- | A constructor of 'Bool', the result type of the given type of a
-- primitive. Only its type constructor is used, so the result can be in
-- another scope.
boolToBoolLiteral :: HasCallStack => TypeEnv -> Type m -> Bool -> Term n
boolToBoolLiteral tenv primTy b = case resultTyConName primTy of
  Just boolTcNm
    | Just boolTc <- lookupTyCon tenv boolTcNm
    , [falseDc, trueDc] <- tyConDataCons boolTc
    -> Data (if b then trueDc else falseDc)
  _ -> error ($(curLoc) ++ "the result type is not Bool: " ++ showPpr primTy)

-- | A constructor of 'Ordering', the result type of the given type of a
-- primitive, like 'boolToBoolLiteral'. Replaces
--
-- @
-- let (_, tyView -> TyConApp ordTcNm []) = splitFunForallTy ty
--     (Just ordTc) = UniqMap.lookup ordTcNm tcm
--     [ltDc, eqDc, gtDc] = tyConDataCons ordTc
-- @
--
-- of the reductions of Clash Core.
orderingToOrderingLiteral :: HasCallStack => TypeEnv -> Type m -> Ordering -> Term n
orderingToOrderingLiteral tenv primTy o = case resultTyConName primTy of
  Just ordTcNm
    | Just ordTc <- lookupTyCon tenv ordTcNm
    , [ltDc, eqDc, gtDc] <- tyConDataCons ordTc
    -> Data (case o of LT -> ltDc; EQ -> eqDc; GT -> gtDc)
  _ -> error ($(curLoc) ++ "the result type is not Ordering: " ++ showPpr primTy)

charToCharLiteral :: Char -> Term n
charToCharLiteral = Literal . CharLiteral

-- | An @Int#@ literal, with the overflow behaviour of 'Int'
integerToIntLiteral :: Integer -> Term n
integerToIntLiteral = Literal . IntLiteral . toInteger . (fromInteger :: Integer -> Int)

-- | A @Word#@ literal, with the overflow behaviour of 'Word'
integerToWordLiteral :: Integer -> Term n
integerToWordLiteral = Literal . WordLiteral . toInteger . (fromInteger :: Integer -> Word)

-- | An @Int64#@ literal, with the overflow behaviour of 'Int64'
integerToInt64Literal :: Integer -> Term n
integerToInt64Literal = Literal . Int64Literal . toInteger . (fromInteger :: Integer -> Int64)

-- | A @Word64#@ literal, with the overflow behaviour of 'Word64'
integerToWord64Literal :: Integer -> Term n
integerToWord64Literal = Literal . Word64Literal . toInteger . (fromInteger :: Integer -> Word64)

integerToIntegerLiteral :: Integer -> Term n
integerToIntegerLiteral = Literal . IntegerLiteral

naturalToNaturalLiteral :: Natural -> Term n
naturalToNaturalLiteral = Literal . NaturalLiteral . toInteger

-- | The only data constructor of the result type of the given type of a
-- primitive, e.g. 'Int' for @I#@
resultOnlyDataCon :: HasCallStack => TypeEnv -> Type m -> DataCon
resultOnlyDataCon tenv primTy = case resultTyConName primTy of
  Just tcNm
    | Just tc <- lookupTyCon tenv tcNm
    , [dc] <- tyConDataCons tc
    -> dc
  _ -> error ($(curLoc) ++ "the result type has more than one constructor: " ++ showPpr primTy)

-- | A boxed number, e.g. an 'Int', the result type of the given type of a
-- primitive
mkIntCLit :: HasCallStack => TypeEnv -> (Integer -> Literal) -> Integer -> Type m -> Term n
mkIntCLit tenv proj lit primTy = App (Data (resultOnlyDataCon tenv primTy)) (Literal (proj lit))

-- | A 'Float', the result type of the given type of a primitive
mkFloatCLit :: HasCallStack => TypeEnv -> Word32 -> Type m -> Term n
mkFloatCLit tenv lit primTy = App (Data (resultOnlyDataCon tenv primTy)) (Literal (FloatLiteral lit))

-- | A 'Double', the result type of the given type of a primitive
mkDoubleCLit :: HasCallStack => TypeEnv -> Word64 -> Type m -> Term n
mkDoubleCLit tenv lit primTy = App (Data (resultOnlyDataCon tenv primTy)) (Literal (DoubleLiteral lit))

-- | A @SomeNat@, the given result type
mkSomeNat :: HasCallStack => TypeEnv -> Integer -> Type n -> Term n
mkSomeNat tenv lit resTy = case tyView resTy of
  TyConApp someNatTcNm []
    | Just someNatTc <- lookupTyCon tenv someNatTcNm
    , [someNatDc] <- tyConDataCons someNatTc
    -- forall n. KnownNat n => Proxy n -> SomeNat
    , SplitFunForall (PiForall _ (PiArg _ (PiArg (tyView -> TyConApp proxyTcNm [natTy, _]) _))) _
        <- splitFunForallTy (dcType someNatDc)
    , Just proxyTc <- lookupTyCon tenv proxyTcNm
    , [proxyDc] <- tyConDataCons proxyTc
    -> mkApps (Data someNatDc)
         [ Right (numTy lit)
         , Left (Literal (NaturalLiteral lit))
         , Left (mkApps (Data proxyDc) [Right (closedType natTy), Right (numTy lit)]) ]
  _ -> error ($(curLoc) ++ "not SomeNat: " ++ showPpr resTy)
 where
  -- The kind of the Proxy, Natural, is closed
  closedType :: Type y -> Type x
  closedType = \case
    ConstTy c -> ConstTy c
    t -> error ($(curLoc) ++ "not a type constructor: " ++ showPpr t)

-- | A literal of a sized type, see
-- @Clash.GHC.Evaluator.Primitive.Util.mkSizedLit@
mkSizedLit
  :: HasCallStack
  => (TyConName -> Term n)
  -- ^ The @fromInteger#@ primitive of the sized type, given its type
  -- constructor
  -> Type n
  -- ^ The type of the primitive whose result it is
  -> Type n
  -- ^ The size
  -> Integer
  -- ^ The size, as a number
  -> Integer
  -- ^ The value
  -> Term n
mkSizedLit conPrim primTy nTy kn val =
  mkApps (conPrim (resultTyCon primTy))
    [ Right nTy
    , Left (Literal (NaturalLiteral kn))
    , Left (Literal (IntegerLiteral val)) ]

-- | A 'Bit', the result of the given type of a primitive
mkBitLit
  :: HasCallStack
  => Type n
  -- ^ The type of the primitive whose result it is
  -> Integer
  -- ^ Mask
  -> Integer
  -- ^ Value
  -> Term n
mkBitLit primTy msk val =
  mkApps (bConPrim (resultTyCon primTy))
    [ Left (Literal (WordLiteral (msk .&. 1)))
    , Left (Literal (IntegerLiteral (val .&. 1))) ]

mkSignedLit, mkUnsignedLit
  :: HasCallStack
  => Type n
  -- ^ The type of the primitive whose result it is
  -> Type n
  -- ^ The size
  -> Integer
  -- ^ The size, as a number
  -> Integer
  -- ^ The value
  -> Term n
mkSignedLit = mkSizedLit signedConPrim
mkUnsignedLit = mkSizedLit unsignedConPrim

-- | A 'BitVector'
mkBitVectorLit
  :: HasCallStack
  => Type n
  -- ^ The type of the primitive whose result it is
  -> Type n
  -- ^ The size
  -> Integer
  -- ^ The size, as a number
  -> Integer
  -- ^ Mask
  -> Integer
  -- ^ Value
  -> Term n
mkBitVectorLit primTy nTy kn mask val =
  mkApps (bvConPrim (resultTyCon primTy))
    [ Right nTy
    , Left (Literal (NaturalLiteral kn))
    , Left (Literal (NaturalLiteral mask))
    , Left (Literal (IntegerLiteral val)) ]

-- | An 'Clash.Sized.Internal.Index.Index', or an undefined value if the
-- value is out of its bounds
mkIndexLitE
  :: HasCallStack
  => Type n
  -- ^ The type of the primitive whose result it is
  -> Type n
  -- ^ The size
  -> Integer
  -- ^ The size, as a number
  -> Integer
  -- ^ The value
  -> Either (Term n) (Term n)
mkIndexLitE primTy nTy kn val
  | val >= 0
  , val < kn
  = Right (mkSizedLit indexConPrim primTy nTy kn val)
  | otherwise
  = Left (TyApp (Prim NP.undefined) (mkTyConApp (resultTyCon primTy) [nTy]))

mkIndexLit :: HasCallStack => Type n -> Type n -> Integer -> Integer -> Term n
mkIndexLit primTy nTy kn val = either id id (mkIndexLitE primTy nTy kn val)

-- | 'mkBitVectorLit', given the result of 'extractTySizeInfo'
mkBitVectorLit' :: HasCallStack => (Type n, Type n, Integer) -> Integer -> Integer -> Term n
mkBitVectorLit' (resTy, nTy, kn) = mkBitVectorLit resTy nTy kn

-- | 'mkIndexLit', given the result of 'extractTySizeInfo'
mkIndexLit' :: HasCallStack => (Type n, Type n, Integer) -> Integer -> Term n
mkIndexLit' (resTy, nTy, kn) = mkIndexLit resTy nTy kn

-- | The only data constructor of the result type of a primitive, e.g. that of
-- an unboxed tuple, and its type arguments. Its type arguments are those of
-- the result type: the result type of the type of the primitive applied to
-- its type arguments.
resultDataCon
  :: (HasCallStack, Distinct n)
  => TypeEnv -> Scope n -> Type n -> [Type n] -> (DataCon, [Type n])
resultDataCon tenv scope primTy tyArgs =
  case tyView (getResultTy tenv scope primTy tyArgs) of
    TyConApp tcNm tcArgs
      | Just tc <- lookupTyCon tenv tcNm
      , [dc] <- tyConDataCons tc
      -> (dc, tcArgs)
    _ -> error ($(curLoc) ++ "the result type has more than one constructor: " ++ showPpr primTy)

-- | The only data constructor of the result type of a primitive, e.g. that of
-- an unboxed tuple, applied to the type arguments of the result type and the
-- given fields. Replaces
--
-- @
-- let (_, tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
--     (Just tupTc) = UniqMap.lookup tupTcNm tcm
--     [tupDc] = tyConDataCons tupTc
-- in mkApps (Data tupDc) (map Right tyArgs ++ map Left fields)
-- @
--
-- of the reductions of Clash Core, which give type arguments that mention the
-- type variables of the type of a polymorphic primitive.
mkResultCon
  :: (HasCallStack, Distinct n)
  => TypeEnv -> Scope n -> Type n -> [Type n] -> [Term n] -> Term n
mkResultCon tenv scope primTy tyArgs fields =
  let (dc, tcArgs) = resultDataCon tenv scope primTy tyArgs
  in mkApps (Data dc) (map Right tcArgs ++ map Left fields)

-- | @Cons@ of a vector of the given length, see
-- 'Clash.Normalize.PrimitiveReductions.mkVecCons'
mkVecCons
  :: (HasCallStack, Distinct n)
  => TypeEnv -> Scope n -> DataCon -> Type n -> Integer -> Term n -> Term n -> Term n
mkVecCons tenv scope consCon elTy n h t
  | n <= 0 = error ($(curLoc) ++ "n <= 0")
  | otherwise = appendToVec tenv scope consCon elTy t n [h]

-- | An empty vector, see 'Clash.Normalize.PrimitiveReductions.mkVecNil'
mkVecNil :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> DataCon -> Type n -> Term n
mkVecNil tenv scope nilCon elTy = mkVec tenv scope nilCon nilCon elTy 0 []

-- * Types

-- | The type constructor at the head of the result type of a type of a
-- primitive, after its quantified type variables and arguments. Also takes a
-- result type itself.
resultTyConName :: Type m -> Maybe TyConName
resultTyConName primTy = case splitFunForallTy primTy of
  SplitFunForall _ res -> case tyView res of
    TyConApp tcNm _ -> Just tcNm
    _ -> Nothing

-- | 'resultTyConName', which must exist
resultTyCon :: HasCallStack => Type m -> TyConName
resultTyCon primTy = case resultTyConName primTy of
  Just tcNm -> tcNm
  Nothing -> error ($(curLoc) ++ "the result type is not a type constructor: " ++ showPpr primTy)

-- | The result type of the type of a primitive, applied to its type
-- arguments, after its arguments. See
-- @Clash.GHC.Evaluator.Primitive.Util.getResultTy@.
getResultTy :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Type n -> [Type n] -> Type n
getResultTy tenv scope primTy tyArgs = dropFuns (piResultTys tenv scope primTy tyArgs)
 where
  dropFuns t = case tyView t of
    FunTy _ r -> dropFuns r
    _ -> t

-- | The first type argument and its size, from the type arguments of a
-- function of type @forall n. KnownNat n => ...@
extractKnownNat :: Distinct n => TypeEnv -> Scope n -> [Type n] -> Maybe (Type n, Integer)
extractKnownNat tenv scope = \case
  nTy : _ | Right nInt <- runExcept (tyNatSize tenv scope nTy) -> Just (nTy, nInt)
  _ -> Nothing

-- | The type arguments and their sizes, from the type arguments of a function
-- of type @forall n m o .. . (KnownNat n, KnownNat m, KnownNat o, ..) => ...@
extractKnownNats :: Distinct n => TypeEnv -> Scope n -> [Type n] -> [(Type n, Integer)]
extractKnownNats tenv scope = mapMaybe (extractKnownNat tenv scope . pure)

-- | The result type of a primitive with a result of a sized type, its size,
-- and its size as a number
extractTySizeInfo
  :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Type n -> [Type n] -> (Type n, Type n, Integer)
extractTySizeInfo tenv scope primTy tyArgs = case tyView resTy of
  TyConApp _ [resSizeTy]
    | Right resSize <- runExcept (tyNatSize tenv scope resSizeTy)
    -> (resTy, resSizeTy, resSize)
  _ -> error ($(curLoc) ++ "not a sized type: " ++ showPpr resTy)
 where
  resTy = getResultTy tenv scope primTy tyArgs

-- | A natural number as a type
numTy :: Integer -> Type n
numTy = LitTy . C.NumTy

-- | A type in some scope, e.g. to look at the names of its type constructors
data SomeType where
  SomeType :: Type n -> SomeType

-- | The argument types and the result type of the type of a primitive, its
-- 'primType', each in its own scope. A reduction that only needs the names of
-- the type constructors in the type of its primitive (e.g. that of @Vec@) can
-- take them from here ('tyConNameOf'), rather than from its instantiated type
-- 'ty'.
primArgResTys :: PrimInfo -> ([SomeType], SomeType)
primArgResTys p = funArgResTys (SomeType (primType p))

-- | The argument types and the result type of a function type, e.g. of an
-- argument type from 'primArgResTys'
funArgResTys :: SomeType -> ([SomeType], SomeType)
funArgResTys (SomeType ty) = case splitFunForallTy ty of
  SplitFunForall tele res -> (teleArgs tele, SomeType res)
 where
  teleArgs :: PiTele x y -> [SomeType]
  teleArgs = \case
    PiNil -> []
    PiForall _ rest -> teleArgs rest
    PiArg a rest -> SomeType a : teleArgs rest

-- | The argument types of a function type, e.g. of an argument type from
-- 'primArgResTys'
funArgTys :: SomeType -> [SomeType]
funArgTys = fst . funArgResTys

-- | The name of the type constructor at the head of a type
tyConNameOf :: SomeType -> Maybe TyConName
tyConNameOf = fmap fst . tyConAppOf

-- | The type constructor at the head of a type, and its arguments
tyConAppOf :: SomeType -> Maybe (TyConName, [SomeType])
tyConAppOf (SomeType t) = case tyView t of
  TyConApp nm args -> Just (nm, map SomeType args)
  _ -> Nothing

-- * Lifting operations on sized types

-- | Lift a binary operation on 'Unsigned' values
liftUnsigned2
  :: (KnownNat m, Distinct n)
  => (Unsigned m -> Unsigned m -> Unsigned m)
  -> Type n -> TypeEnv -> Scope n -> [Type n] -> [Value n] -> (Proxy m -> Maybe (Term n))
liftUnsigned2 = liftSized2 unsignedLiterals' mkUnsignedLit

-- | Lift a binary operation on 'Signed' values
liftSigned2
  :: (KnownNat m, Distinct n)
  => (Signed m -> Signed m -> Signed m)
  -> Type n -> TypeEnv -> Scope n -> [Type n] -> [Value n] -> (Proxy m -> Maybe (Term n))
liftSigned2 = liftSized2 signedLiterals' mkSignedLit

-- | Lift a binary operation on 'BitVector' values
liftBitVector2
  :: (KnownNat m, Distinct n)
  => (BitVector m -> BitVector m -> BitVector m)
  -> Type n -> TypeEnv -> Scope n -> [Type n] -> [Value n] -> (Proxy m -> Maybe (Term n))
liftBitVector2 f primTy tenv scope tyArgs vals _p
  | Just (nTy, kn) <- extractKnownNat tenv scope tyArgs
  , [i, j] <- bitVectorLiterals' vals
  = let BV mask val = f (toBV i) (toBV j)
    in Just (mkBitVectorLit primTy nTy kn (toInteger mask) (toInteger val))
  | otherwise = Nothing

-- | Lift a binary predicate on 'BitVector' values
liftBitVector2Bool
  :: KnownNat m
  => (BitVector m -> BitVector m -> Bool)
  -> Type n -> TypeEnv -> [Value n] -> (Proxy m -> Maybe (Term n))
liftBitVector2Bool f primTy tenv vals _p
  | [i, j] <- bitVectorLiterals' vals
  = Just (boolToBoolLiteral tenv primTy (f (toBV i) (toBV j)))
  | otherwise = Nothing

-- | Lift a function from 'Int' to 'BitVector'
liftInteger2BitVector
  :: KnownNat m
  => (Integer -> BitVector m)
  -> (Type n, Type n, Integer)
  -- ^ See 'extractTySizeInfo'
  -> [Value n] -> (Proxy m -> Maybe (Term n))
liftInteger2BitVector f resTyInfo vals _p
  | [i] <- intCLiterals' vals
  = let BV msk val = f i
    in Just (mkBitVectorLit' resTyInfo (toInteger msk) (toInteger val))
  | otherwise
  = Nothing

-- | Lift a function from 'BitVector' to 'Int'
liftBitVector2CInt
  :: KnownNat m
  => TypeEnv -> Type n -> (BitVector m -> Integer) -> [Value n] -> (Proxy m -> Maybe (Term n))
liftBitVector2CInt tenv primTy f vals _p
  | [i] <- bitVectorLiterals' vals
  = Just (mkIntCLit tenv IntLiteral (f (toBV i)) primTy)
  | otherwise
  = Nothing

-- | Lift a binary operation on a sized type
liftSized2
  :: (KnownNat m, Integral (sized m), Distinct n)
  => ([Value n] -> [Integer])
  -- ^ The literals of the arguments
  -> (Type n -> Type n -> Integer -> Integer -> Term n)
  -- ^ Build a literal
  -> (sized m -> sized m -> sized m)
  -> Type n -> TypeEnv -> Scope n -> [Type n] -> [Value n] -> (Proxy m -> Maybe (Term n))
liftSized2 extractLitArgs mkLit f primTy tenv scope tyArgs vals p
  | Just (nTy, kn) <- extractKnownNat tenv scope tyArgs
  , [i, j] <- extractLitArgs vals
  = Just (mkLit primTy nTy kn (runSizedF f i j p))
  | otherwise = Nothing

-- | Run a binary operation of a sized type on integers. Run the result with
-- @reifyNat@.
runSizedF
  :: (KnownNat m, Integral (sized m))
  => (sized m -> sized m -> sized m) -> Integer -> Integer -> (Proxy m -> Integer)
runSizedF f i j _ = toInteger (f (fromInteger i) (fromInteger j))

-- * Lifting operations on unboxed numbers

liftDDI :: (Double# -> Double# -> Int#) -> [Value n] -> Maybe (Term n)
liftDDI f vals = case doubleLiterals' vals of
  [i, j] -> Just (runDDI f i j)
  _ -> Nothing

liftDDD :: (Double# -> Double# -> Double#) -> [Value n] -> Maybe (Term n)
liftDDD f vals = case doubleLiterals' vals of
  [i, j] -> Just (runDDD f i j)
  _ -> Nothing

liftDD :: (Double# -> Double#) -> [Value n] -> Maybe (Term n)
liftDD f vals = case doubleLiterals' vals of
  [i] -> Just (runDD f i)
  _ -> Nothing

runDDI :: (Double# -> Double# -> Int#) -> Word64 -> Word64 -> Term n
runDDI f i j =
  let !(D# a) = castWord64ToDouble i
      !(D# b) = castWord64ToDouble j
  in Literal (IntLiteral (toInteger (I# (f a b))))

runDDD :: (Double# -> Double# -> Double#) -> Word64 -> Word64 -> Term n
runDDD f i j =
  let !(D# a) = castWord64ToDouble i
      !(D# b) = castWord64ToDouble j
  in Literal (DoubleLiteral (castDoubleToWord64 (D# (f a b))))

runDD :: (Double# -> Double#) -> Word64 -> Term n
runDD f i =
  let !(D# a) = castWord64ToDouble i
  in Literal (DoubleLiteral (castDoubleToWord64 (D# (f a))))

liftFFI :: (Float# -> Float# -> Int#) -> [Value n] -> Maybe (Term n)
liftFFI f vals = case floatLiterals' vals of
  [i, j] -> Just (runFFI f i j)
  _ -> Nothing

liftFFF :: (Float# -> Float# -> Float#) -> [Value n] -> Maybe (Term n)
liftFFF f vals = case floatLiterals' vals of
  [i, j] -> Just (runFFF f i j)
  _ -> Nothing

liftFF :: (Float# -> Float#) -> [Value n] -> Maybe (Term n)
liftFF f vals = case floatLiterals' vals of
  [i] -> Just (runFF f i)
  _ -> Nothing

runFFI :: (Float# -> Float# -> Int#) -> Word32 -> Word32 -> Term n
runFFI f i j =
  let !(F# a) = castWord32ToFloat i
      !(F# b) = castWord32ToFloat j
  in Literal (IntLiteral (toInteger (I# (f a b))))

runFFF :: (Float# -> Float# -> Float#) -> Word32 -> Word32 -> Term n
runFFF f i j =
  let !(F# a) = castWord32ToFloat i
      !(F# b) = castWord32ToFloat j
  in Literal (FloatLiteral (castFloatToWord32 (F# (f a b))))

runFF :: (Float# -> Float#) -> Word32 -> Term n
runFF f i =
  let !(F# a) = castWord32ToFloat i
  in Literal (FloatLiteral (castFloatToWord32 (F# (f a))))

liftI8 :: (Int8# -> Int8# -> Int8#) -> [Value n] -> Maybe (Term n)
liftI8 f vals = case int8Literals' vals of
  [i, j] ->
    let !(I8# a) = fromInteger i
        !(I8# b) = fromInteger j
    in Just (Literal (Int8Literal (toInteger (I8# (f a b)))))
  _ -> Nothing

liftI8I :: (Int8# -> Int# -> Int8#) -> [Value n] -> Maybe (Term n)
liftI8I f vals = case vals of
  [Lit (Int8Literal i), Lit (IntLiteral j)] ->
    let !(I8# a) = fromInteger i
        !(I# b) = fromInteger j
    in Just (Literal (Int8Literal (toInteger (I8# (f a b)))))
  _ -> Nothing

liftI8RI :: (Int8# -> Int8# -> Int#) -> [Value n] -> Maybe (Term n)
liftI8RI f vals = case int8Literals' vals of
  [i, j] ->
    let !(I8# a) = fromInteger i
        !(I8# b) = fromInteger j
    in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftI16 :: (Int16# -> Int16# -> Int16#) -> [Value n] -> Maybe (Term n)
liftI16 f vals = case int16Literals' vals of
  [i, j] ->
    let !(I16# a) = fromInteger i
        !(I16# b) = fromInteger j
    in Just (Literal (Int16Literal (toInteger (I16# (f a b)))))
  _ -> Nothing

liftI16I :: (Int16# -> Int# -> Int16#) -> [Value n] -> Maybe (Term n)
liftI16I f vals = case vals of
  [Lit (Int16Literal i), Lit (IntLiteral j)] ->
    let !(I16# a) = fromInteger i
        !(I# b) = fromInteger j
    in Just (Literal (Int16Literal (toInteger (I16# (f a b)))))
  _ -> Nothing

liftI16RI :: (Int16# -> Int16# -> Int#) -> [Value n] -> Maybe (Term n)
liftI16RI f vals = case int16Literals' vals of
  [i, j] ->
    let !(I16# a) = fromInteger i
        !(I16# b) = fromInteger j
    in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftI32 :: (Int32# -> Int32# -> Int32#) -> [Value n] -> Maybe (Term n)
liftI32 f vals = case int32Literals' vals of
  [i, j] ->
    let !(I32# a) = fromInteger i
        !(I32# b) = fromInteger j
    in Just (Literal (Int32Literal (toInteger (I32# (f a b)))))
  _ -> Nothing

liftI32I :: (Int32# -> Int# -> Int32#) -> [Value n] -> Maybe (Term n)
liftI32I f vals = case vals of
  [Lit (Int32Literal i), Lit (IntLiteral j)] ->
    let !(I32# a) = fromInteger i
        !(I# b) = fromInteger j
    in Just (Literal (Int32Literal (toInteger (I32# (f a b)))))
  _ -> Nothing

liftI32RI :: (Int32# -> Int32# -> Int#) -> [Value n] -> Maybe (Term n)
liftI32RI f vals = case int32Literals' vals of
  [i, j] ->
    let !(I32# a) = fromInteger i
        !(I32# b) = fromInteger j
    in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftI64 :: (Int64# -> Int64# -> Int64#) -> [Value n] -> Maybe (Term n)
liftI64 f vals = case int64Literals' vals of
  [i, j] ->
    let !(I64# a) = fromInteger i
        !(I64# b) = fromInteger j
    in Just (Literal (Int64Literal (toInteger (I64# (f a b)))))
  _ -> Nothing

liftI64I :: (Int64# -> Int# -> Int64#) -> [Value n] -> Maybe (Term n)
liftI64I f vals = case vals of
  [Lit (Int64Literal i), Lit (IntLiteral j)] ->
    let !(I64# a) = fromInteger i
        !(I# b) = fromInteger j
    in Just (Literal (Int64Literal (toInteger (I64# (f a b)))))
  _ -> Nothing

liftI64RI :: (Int64# -> Int64# -> Int#) -> [Value n] -> Maybe (Term n)
liftI64RI f vals = case int64Literals' vals of
  [i, j] ->
    let !(I64# a) = fromInteger i
        !(I64# b) = fromInteger j
    in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftW8 :: (Word8# -> Word8# -> Word8#) -> [Value n] -> Maybe (Term n)
liftW8 f vals = case word8Literals' vals of
  [i, j] ->
    let !(W8# a) = fromInteger i
        !(W8# b) = fromInteger j
    in Just (Literal (Word8Literal (toInteger (W8# (f a b)))))
  _ -> Nothing

liftW8I :: (Word8# -> Int# -> Word8#) -> [Value n] -> Maybe (Term n)
liftW8I f vals = case vals of
  [Lit (Word8Literal i), Lit (IntLiteral j)] ->
    let !(W8# a) = fromInteger i
        !(I# b) = fromInteger j
    in Just (Literal (Word8Literal (toInteger (W8# (f a b)))))
  _ -> Nothing

liftW8RI :: (Word8# -> Word8# -> Int#) -> [Value n] -> Maybe (Term n)
liftW8RI f vals = case word8Literals' vals of
  [i, j] ->
    let !(W8# a) = fromInteger i
        !(W8# b) = fromInteger j
    in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftW16 :: (Word16# -> Word16# -> Word16#) -> [Value n] -> Maybe (Term n)
liftW16 f vals = case word16Literals' vals of
  [i, j] ->
    let !(W16# a) = fromInteger i
        !(W16# b) = fromInteger j
    in Just (Literal (Word16Literal (toInteger (W16# (f a b)))))
  _ -> Nothing

liftW16I :: (Word16# -> Int# -> Word16#) -> [Value n] -> Maybe (Term n)
liftW16I f vals = case vals of
  [Lit (Word16Literal i), Lit (IntLiteral j)] ->
    let !(W16# a) = fromInteger i
        !(I# b) = fromInteger j
    in Just (Literal (Word16Literal (toInteger (W16# (f a b)))))
  _ -> Nothing

liftW16RI :: (Word16# -> Word16# -> Int#) -> [Value n] -> Maybe (Term n)
liftW16RI f vals = case word16Literals' vals of
  [i, j] ->
    let !(W16# a) = fromInteger i
        !(W16# b) = fromInteger j
    in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftW32 :: (Word32# -> Word32# -> Word32#) -> [Value n] -> Maybe (Term n)
liftW32 f vals = case word32Literals' vals of
  [i, j] ->
    let !(W32# a) = fromInteger i
        !(W32# b) = fromInteger j
    in Just (Literal (Word32Literal (toInteger (W32# (f a b)))))
  _ -> Nothing

liftW32I :: (Word32# -> Int# -> Word32#) -> [Value n] -> Maybe (Term n)
liftW32I f vals = case vals of
  [Lit (Word32Literal i), Lit (IntLiteral j)] ->
    let !(W32# a) = fromInteger i
        !(I# b) = fromInteger j
    in Just (Literal (Word32Literal (toInteger (W32# (f a b)))))
  _ -> Nothing

liftW32RI :: (Word32# -> Word32# -> Int#) -> [Value n] -> Maybe (Term n)
liftW32RI f vals = case word32Literals' vals of
  [i, j] ->
    let !(W32# a) = fromInteger i
        !(W32# b) = fromInteger j
    in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

liftW64 :: (Word64# -> Word64# -> Word64#) -> [Value n] -> Maybe (Term n)
liftW64 f vals = case word64Literals' vals of
  [i, j] ->
    let !(W64# a) = fromInteger i
        !(W64# b) = fromInteger j
    in Just (Literal (Word64Literal (toInteger (W64# (f a b)))))
  _ -> Nothing

liftW64I :: (Word64# -> Int# -> Word64#) -> [Value n] -> Maybe (Term n)
liftW64I f vals = case vals of
  [Lit (Word64Literal i), Lit (IntLiteral j)] ->
    let !(W64# a) = fromInteger i
        !(I# b) = fromInteger j
    in Just (Literal (Word64Literal (toInteger (W64# (f a b)))))
  _ -> Nothing

liftW64RI :: (Word64# -> Word64# -> Int#) -> [Value n] -> Maybe (Term n)
liftW64RI f vals = case word64Literals' vals of
  [i, j] ->
    let !(W64# a) = fromInteger i
        !(W64# b) = fromInteger j
    in Just (Literal (IntLiteral (toInteger (I# (f a b)))))
  _ -> Nothing

-- | The reduction shared by every worker of GHC's @powImpl@, see
-- @Clash.GHC.Evaluator.Primitive.Util.powImplWorker@
powImplWorker :: PrimStepContext o n -> Maybe (Next o n)
powImplWorker PrimStepContext{..}
  | [intLiteral -> Just j, integerLiteral -> Just i] <- args
  = reduce (catchErrorCall (integerToIntegerLiteral (i ^ j)))
  | [integerLiteral -> Just i, intLiteral -> Just j] <- args
  = reduce (catchErrorCall (integerToIntegerLiteral (i ^ j)))
  | [intLiteral -> Just i, intLiteral -> Just j] <- args
  = reduce (catchErrorCall (integerToIntLiteral (i ^ j)))
  | [Lit (ByteArrayLiteral (BA.ByteArray ba)), integerLiteral -> Just i] <- args
  = reduce (catchErrorCall (integerToIntegerLiteral (i ^ IP ba)))
  | otherwise
  = Nothing

-- * Primitives in results

-- | A primitive with the given name, work info and closed type. The types of
-- the primitives below are built with "Clash.Core.Scoped.Builder": the keys
-- of their type variables only need to differ within a type.
closedPrim :: Text -> C.WorkInfo -> TyB -> Term n
closedPrim nm wi ty = Prim (mkPrimInfo nm (runClosedType ty) wi C.SingleResult NoUnfolding)

-- | A type constructor
tyConB :: TyConName -> TyB
tyConB tc = tyConAppB tc []

-- | A type quantified over a type variable of kind 'Natural', and of kind
-- 'Type': its key, its name, and the body
forAllNatB, forAllTypeB :: Key -> Text -> TyB -> TyB
forAllNatB k nm = forAllTyB k (C.mkUnsafeSystemName nm (fromIntegral k)) (tyConB typeNatKindTyConName)
forAllTypeB k nm = forAllTyB k (C.mkUnsafeSystemName nm (fromIntegral k)) (tyConB liftedTypeKindTyConName)

-- | The type-level addition of naturals
typeNatAddB :: TyB -> TyB -> TyB
typeNatAddB m n = tyConAppB typeNatAdd [m, n]

-- | 'Clash.Sized.Internal.BitVector.fromInteger##', given the type
-- constructor of 'Bit'
bConPrim :: TyConName -> Term n
bConPrim bTcNm =
  closedPrim (showt 'Clash.Sized.Internal.BitVector.fromInteger##) C.WorkNever
    (funTyB [tyConB wordPrimTyConName, tyConB integerPrimTyConName] (tyConB bTcNm))

-- | 'Clash.Sized.Internal.BitVector.fromInteger#', given the type
-- constructor of 'BitVector'
bvConPrim :: TyConName -> Term n
bvConPrim =
  sizedConPrim (showt 'Clash.Sized.Internal.BitVector.fromInteger#) [tyConB naturalPrimTyConName]

-- | 'Clash.Sized.Internal.Index.fromInteger#', given the type
-- constructor of 'Clash.Sized.Internal.Index.Index'
indexConPrim :: TyConName -> Term n
indexConPrim = sizedConPrim (showt 'Clash.Sized.Internal.Index.fromInteger#) []

-- | 'Clash.Sized.Internal.Signed.fromInteger#', given the type
-- constructor of 'Signed'
signedConPrim :: TyConName -> Term n
signedConPrim = sizedConPrim (showt 'Clash.Sized.Internal.Signed.fromInteger#) []

-- | 'Clash.Sized.Internal.Unsigned.fromInteger#', given the type
-- constructor of 'Unsigned'
unsignedConPrim :: TyConName -> Term n
unsignedConPrim = sizedConPrim (showt 'Clash.Sized.Internal.Unsigned.fromInteger#) []

-- | The @fromInteger#@ primitive of a sized type: @forall n. Natural# ->
-- extra args -> Integer -> sized n@
sizedConPrim :: Text -> [TyB] -> TyConName -> Term n
sizedConPrim nm extraArgs tcNm =
  closedPrim nm C.WorkNever $ forAllNatB 0 "n" $
    funTyB
      ([tyConB naturalPrimTyConName] ++ extraArgs ++ [tyConB integerPrimTyConName])
      (tyConAppB tcNm [varTyB 0])

-- | 'Clash.Sized.Vector.splitAt'
splitAtPrim
  :: TyConName
  -- ^ SNat TyCon name
  -> TyConName
  -- ^ Vec TyCon name
  -> Term n
splitAtPrim snatTcNm vecTcNm =
  closedPrim (showt 'Clash.Sized.Vector.splitAt) C.WorkNever (splitAtTy snatTcNm vecTcNm)

splitAtTy
  :: TyConName
  -- ^ SNat TyCon name
  -> TyConName
  -- ^ Vec TyCon name
  -> TyB
splitAtTy snatNm = splitAtTyWith (tyConAppB snatNm [varTyB 0])

-- | The type of @fold_split@: 'splitAt' with the length as a 'Natural'
foldSplitAtTy
  :: TyConName
  -- ^ Vec TyCon name
  -> TyB
foldSplitAtTy = splitAtTyWith (tyConB naturalPrimTyConName)

-- | The type of 'splitAt', given the type of the length, which may mention the
-- type variable @m@ (key 0)
splitAtTyWith :: TyB -> TyConName -> TyB
splitAtTyWith lenTy vecNm =
  forAllNatB 0 "m" $ forAllNatB 1 "n" $ forAllTypeB 2 "a" $
    funTyB
      [lenTy, tyConAppB vecNm [typeNatAddB m n, a]]
      (tyConAppB (tupleTyConName 2) [tyConAppB vecNm [m, a], tyConAppB vecNm [n, a]])
 where
  m = varTyB 0
  n = varTyB 1
  a = varTyB 2

-- | 'Clash.Sized.Vector.++'
vecAppendPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term n
vecAppendPrim vecNm =
  closedPrim (showt '(Clash.Sized.Vector.++)) C.WorkNever (vecAppendTy vecNm)

vecAppendTy
  :: TyConName
  -- ^ Vec TyCon name
  -> TyB
vecAppendTy vecNm =
  forAllNatB 0 "n" $ forAllTypeB 1 "a" $ forAllNatB 2 "m" $
    funTyB
      [tyConAppB vecNm [n, a], tyConAppB vecNm [m, a]]
      (tyConAppB vecNm [typeNatAddB n m, a])
 where
  n = varTyB 0
  a = varTyB 1
  m = varTyB 2

-- | 'Clash.Sized.Vector.zipWith'
vecZipWithPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term n
vecZipWithPrim vecNm =
  closedPrim (showt 'Clash.Sized.Vector.zipWith) C.WorkNever (vecZipWithTy vecNm)

vecZipWithTy
  :: TyConName
  -- ^ Vec TyCon name
  -> TyB
vecZipWithTy vecNm =
  forAllTypeB 0 "a" $ forAllTypeB 1 "b" $ forAllTypeB 2 "c" $ forAllNatB 3 "n" $
    funTyB
      [funTyB [a, b] c, tyConAppB vecNm [n, a], tyConAppB vecNm [n, b]]
      (tyConAppB vecNm [n, c])
 where
  a = varTyB 0
  b = varTyB 1
  c = varTyB 2
  n = varTyB 3

-- | The type of @Clash.Sized.Vector.imap_go@
vecImapGoTy
  :: TyConName
  -- ^ Vec TyCon name
  -> TyConName
  -- ^ Index TyCon name
  -> TyB
vecImapGoTy vecTcNm indexTcNm =
  forAllNatB 0 "n" $ forAllNatB 1 "m" $ forAllTypeB 2 "a" $ forAllTypeB 3 "b" $
    funTyB
      [funTyB [indexTy, a] b, tyConAppB vecTcNm [m, a], indexTy]
      (tyConAppB vecTcNm [m, b])
 where
  m = varTyB 1
  a = varTyB 2
  b = varTyB 3
  indexTy = tyConAppB indexTcNm [varTyB 0]

-- | The type of the addition of 'Clash.Sized.Internal.Index.Index'
indexAddTy
  :: TyConName
  -- ^ Index TyCon name
  -> TyB
indexAddTy indexTcNm =
  forAllNatB 0 "n" (funTyB [tyConB naturalPrimTyConName, indexTy, indexTy] indexTy)
 where
  indexTy = tyConAppB indexTcNm [varTyB 0]

-- | 'Clash.Sized.Internal.BitVector.++#'
bvAppendPrim
  :: TyConName
  -- ^ BitVector TyCon name
  -> Term n
bvAppendPrim bvTcNm =
  closedPrim (showt '(Clash.Sized.Internal.BitVector.++#)) C.WorkNever (bvAppendTy bvTcNm)

bvAppendTy
  :: TyConName
  -- ^ BitVector TyCon name
  -> TyB
bvAppendTy bvNm =
  forAllNatB 0 "m" $ forAllNatB 1 "n" $
    funTyB
      [tyConB naturalPrimTyConName, tyConAppB bvNm [n], tyConAppB bvNm [m]]
      (tyConAppB bvNm [typeNatAddB n m])
 where
  m = varTyB 0
  n = varTyB 1

-- | 'Clash.Sized.Internal.BitVector.split#'
bvSplitPrim
  :: TyConName
  -- ^ BitVector TyCon name
  -> Term n
bvSplitPrim bvTcNm =
  closedPrim (showt 'Clash.Sized.Internal.BitVector.split#) C.WorkNever (bvSplitTy bvTcNm)

bvSplitTy
  :: TyConName
  -- ^ BitVector TyCon name
  -> TyB
bvSplitTy bvNm =
  forAllNatB 0 "n" $ forAllNatB 1 "m" $
    funTyB
      [tyConB naturalPrimTyConName, tyConAppB bvNm [typeNatAddB m n]]
      (tyConAppB (tupleTyConName 2) [tyConAppB bvNm [m], tyConAppB bvNm [n]])
 where
  n = varTyB 0
  m = varTyB 1

-- | 'Clash.Sized.Vector.head' and 'Clash.Sized.Vector.last', given the type
-- constructor of 'Clash.Sized.Vector.Vec': @forall n a. Vec (n + 1) a -> a@.
-- See 'Clash.Normalize.PrimitiveReductions.vecHeadPrim'.
vecHeadPrim, vecLastPrim :: TyConName -> Term n
vecHeadPrim = closedPrim "Clash.Sized.Vector.head" C.WorkNever . vecHeadTy
vecLastPrim = closedPrim "Clash.Sized.Vector.last" C.WorkNever . vecHeadTy

vecHeadTy :: TyConName -> TyB
vecHeadTy vecNm =
  forAllNatB 0 "n" $ forAllTypeB 1 "a" $
    funTyB [tyConAppB vecNm [typeNatAddB (varTyB 0) (litTyB (C.NumTy 1)), varTyB 1]] (varTyB 1)

-- | 'Clash.Sized.Vector.tail' and 'Clash.Sized.Vector.init', given the type
-- constructor of 'Clash.Sized.Vector.Vec': @forall n a. Vec (n + 1) a -> Vec n
-- a@. See 'Clash.Normalize.PrimitiveReductions.vecTailPrim'.
vecTailPrim, vecInitPrim :: TyConName -> Term n
vecTailPrim = closedPrim "Clash.Sized.Vector.tail" C.WorkNever . vecTailTy
vecInitPrim = closedPrim "Clash.Sized.Vector.init" C.WorkNever . vecTailTy

vecTailTy :: TyConName -> TyB
vecTailTy vecNm =
  forAllNatB 0 "n" $ forAllTypeB 1 "a" $
    funTyB
      [tyConAppB vecNm [typeNatAddB (varTyB 0) (litTyB (C.NumTy 1)), varTyB 1]]
      (tyConAppB vecNm [varTyB 0, varTyB 1])

-- | The name of the type constructor of boxed tuples of the given arity, as
-- "Clash.GHC.GHC2Core" names it. See
-- @Clash.GHC.Evaluator.Primitive.Util.ghcTyconToTyConName@.
tupleTyConName :: Int -> TyConName
tupleTyConName arity =
  C.Name C.User qualName (fromGhcUnique (GhcTyCon.tyConUnique tc)) (getSrcSpan nm)
 where
  tc = tupleTyCon Boxed arity
  nm = GhcTyCon.tyConName tc
  modNm = maybe "_INTERNAL_" (Text.pack . moduleNameString . moduleName) (nameModule_maybe nm)
  qualName = modNm `Text.append` ('.' `Text.cons` Text.pack (occNameString (nameOccName nm)))

-- * The machine

-- | Set the term of a machine
setTerm :: Term n -> Machine o n -> Machine o n
setTerm e m = m {mTerm = e}

-- | Clear the stack of a machine
stackClear :: Machine o n -> Machine o n
stackClear m = m {mStack = []}

-- ** The primitive heap

-- | The number of the next byte array on the primitive heap
primCount :: Machine o n -> Int
primCount = snd . mHeapPrim

-- | A byte array on the primitive heap
primLookup :: Distinct n => Int -> Machine o n -> Maybe (Term n)
primLookup i m = fromClashTermClosedIn (mScope m) <$> IntMap.lookup i (fst (mHeapPrim m))

-- | Allocate a byte array on the primitive heap, with the given number, see
-- 'primCount'
primInsert :: Int -> Term n -> Machine o n -> Machine o n
primInsert i x m =
  let (heap, count) = mHeapPrim m
  in m {mHeapPrim = (IntMap.insert i (toClashTerm x) heap, count + 1)}

-- | Update a byte array on the primitive heap
primUpdate :: Int -> Term n -> Machine o n -> Machine o n
primUpdate i x m =
  let (heap, count) = mHeapPrim m
  in m {mHeapPrim = (IntMap.insert i (toClashTerm x) heap, count)}

-- | Run a state transformer of GHC for its effect
svoid :: (State# RealWorld -> State# RealWorld) -> IO ()
svoid m0 = IO (\s -> case m0 s of s' -> (# s', () #))

-- * Data constructors

isTrueDC, isFalseDC :: DataCon -> Bool
isTrueDC dc = dcUniq dc == fromGhcUnique trueDataConKey
isFalseDC dc = dcUniq dc == fromGhcUnique falseDataConKey
