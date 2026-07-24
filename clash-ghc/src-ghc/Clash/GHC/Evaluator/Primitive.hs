{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#if MIN_VERSION_ghc(9,12,0)
-- We'll need to support deprecated primitives too
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

#include "MachDeps.h"

module Clash.GHC.Evaluator.Primitive
  ( ghcPrimStep
  , ghcPrimUnwind
  , isUndefinedPrimVal
  , isUndefinedXPrimVal
  ) where

import           Control.DeepSeq            (force)
import           Control.Exception          (ArithException(..), ErrorCall, Exception, tryJust, evaluate)
import qualified Control.Lens               as Lens
import           Control.Monad.State.Strict (State, MonadState)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans.Except (runExcept)
import           Data.Bits
import qualified Data.ByteString.Internal as BS
import           Data.Char           (chr,ord)
import qualified Data.Either         as Either
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.List           as List
import qualified Data.Primitive.ByteArray as BA
import           Data.Proxy          (Proxy)
import           Data.Reflection     (reifyNat)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Extra     (showt)
import           GHC.Exts (IsList(..))
import           GHC.Float
import           GHC.Int
import           GHC.Integer
  (decodeDoubleInteger,encodeDoubleInteger,compareInteger,orInteger,andInteger,
   xorInteger,complementInteger,absInteger,signumInteger)
import           GHC.Num.BigNat      (bigNatEq#)
import           GHC.Num.Integer (Integer (..), integerEncodeFloat#)
import           GHC.Num.Natural     (naturalSubUnsafe)
import           GHC.Natural
import           GHC.ForeignPtr
import           GHC.Prim
import           GHC.Real            (Ratio (..))
import           GHC.TypeLits        (KnownNat)
import           GHC.Types           (IO (..))
import           GHC.Word
import           System.IO.Unsafe    (unsafeDupablePerformIO)
import           Data.Bifunctor      (first)
import qualified Data.Text.Array     as Text
import qualified Data.Text.Internal  as Text

import           GHC.Types.Basic     (Boxity (..))
import           GHC.Types.Name      (getSrcSpan, nameOccName, occNameString)
import           GHC.Builtin.Names   (trueDataConKey, falseDataConKey)
import qualified GHC.Core.TyCon      as TyCon
import           GHC.Builtin.Types   (tupleTyCon)

import           Clash.Class.BitPack (pack,unpack)
import           Clash.Core.DataCon  (DataCon (..))
import           Clash.Core.Evaluator.Types
import           Clash.Core.FreeVars (typeFreeVars)
import           Clash.Core.HasType  (piResultTys, applyTypeToArgs)
import           Clash.Core.Literal  (Literal (..))
import           Clash.Core.Name
  (Name (..), NameSort (..), mkUnsafeSystemName)
import           Clash.Core.Pretty   (showPpr)
import           Clash.Core.Subst    (extendTvSubst, mkSubst, substTy)
import           Clash.Core.Term
  (IsMultiPrim (..), Pat (..), PrimInfo (..), Term (..), WorkInfo (..), mkApps,
   PrimUnfolding(..), collectArgs)
import           Clash.Core.Type
  (Type (..), ConstTy (..), LitTy (..), TypeView (..), mkFunTy, mkTyConApp,
   normalizeType, splitFunForallTy, tyView)
import           Clash.Core.TyCon
  (TyConMap, TyConName, tyConDataCons)
import           Clash.Core.TysPrim
import           Clash.Core.Util
  (mkRTree,mkVec,tyNatSize,dataConInstArgTys,primCo, mkSelectorCase,undefinedPrims,
   undefinedXPrims)
import           Clash.Core.Var      (mkLocalId, mkTyVar)
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Debug
import           Clash.GHC.GHC2Core  (modNameM)
import           Clash.Unique        (fromGhcUnique)
import           Clash.Util
  (MonadUnique (..), clogBase, flogBase, curLoc, textNameLit)
import           Clash.Util.Supply   (Supply,freshId)
import           Clash.Normalize.PrimitiveReductions
  (typeNatMul, typeNatSub, typeNatAdd, vecLastPrim, vecInitPrim, vecHeadPrim,
   vecTailPrim, mkVecCons, mkVecNil)

import qualified Clash.Normalize.Primitives as NP
import Clash.Promoted.Nat.Unsafe (unsafeSNat)
import qualified Clash.Sized.Internal.BitVector as BitVector
import qualified Clash.Sized.Internal.Signed    as Signed
import qualified Clash.Sized.Internal.Unsigned  as Unsigned
import Clash.Sized.Internal.BitVector(BitVector(..), Bit(..))
import Clash.Sized.Internal.Signed   (Signed   (..))
import Clash.Sized.Internal.Unsigned (Unsigned (..))
import Clash.XException (isX)

import {-# SOURCE #-} Clash.GHC.Evaluator

import qualified Clash.Annotations.BitRepresentation.Deriving
import qualified Clash.Class.BitPack.Internal
import qualified Clash.Class.Exp
import qualified Clash.Promoted.Nat
import qualified Clash.Sized.Internal.BitVector
import qualified Clash.Sized.Internal.Index
import qualified Clash.Sized.Internal.Signed
import qualified Clash.Sized.Internal.Unsigned
import qualified Clash.Sized.RTree
import qualified Clash.Sized.Vector
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.CString
import qualified GHC.TypeLits
import qualified GHC.TypeNats
import qualified GHC.Types
#if MIN_VERSION_ghc(9,12,0)
import qualified GHC.Magic
#endif
import qualified GHC.Num
import qualified GHC.Num.Integer

#if MIN_VERSION_ghc_prim(0,12,0)
import qualified GHC.PrimopWrappers
#endif

isUndefinedPrimVal :: Value -> Bool
isUndefinedPrimVal (PrimVal (PrimInfo{primName}) _ _) =
  primName `elem` undefinedPrims
isUndefinedPrimVal _ = False

isUndefinedXPrimVal :: Value -> Bool
isUndefinedXPrimVal (PrimVal (PrimInfo{primName}) _ _) =
  primName `elem` undefinedXPrims
isUndefinedXPrimVal _ = False

-- | Evaluation of primitive operations.
ghcPrimUnwind :: PrimUnwind
ghcPrimUnwind tcm p tys vs v [] m
  | primName p `elem` [ showt 'Clash.Sized.Internal.Index.fromInteger#
                       , showt 'GHC.CString.unpackCString#
                       , showt 'NP.removedArg
                       , showt ''MutableByteArray#
                       , showt 'NP.undefined
                       , showt 'NP.undefinedX
                       ]
              -- The above primitives are actually values, and not operations.
  = ghcUnwind (PrimVal p tys (vs ++ [v])) m tcm
  | primName p == showt 'Clash.Sized.Internal.BitVector.fromInteger#
  = case (vs,v) of
    ([naturalLiteral -> Just n,mask], integerLiteral -> Just i) ->
      ghcUnwind (PrimVal p tys [Lit (NaturalLiteral n), mask, Lit (IntegerLiteral (wrapUnsigned n i))]) m tcm
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | primName p == showt 'Clash.Sized.Internal.BitVector.fromInteger##
  = case (vs,v) of
    ([mask], integerLiteral -> Just i) ->
      ghcUnwind (PrimVal p tys [mask, Lit (IntegerLiteral (wrapUnsigned 1 i))]) m tcm
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | primName p == showt 'Clash.Sized.Internal.Signed.fromInteger#
  = case (vs,v) of
    ([naturalLiteral -> Just n],integerLiteral -> Just i) ->
      ghcUnwind (PrimVal p tys [Lit (NaturalLiteral n), Lit (IntegerLiteral (wrapSigned n i))]) m tcm
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | primName p == showt 'Clash.Sized.Internal.Unsigned.fromInteger#
  = case (vs,v) of
    ([naturalLiteral -> Just n],integerLiteral -> Just i) ->
      ghcUnwind (PrimVal p tys [Lit (NaturalLiteral n), Lit (IntegerLiteral (wrapUnsigned n i))]) m tcm
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | isUndefinedPrimVal v
  = let tyArgs = map Right tys
        tmArgs = map (Left . valToTerm) (vs ++ [v])
    in  Just $ flip setTerm m $ TyApp (Prim NP.undefined) $
          applyTypeToArgs (Prim p) tcm (primType p) (tyArgs ++ tmArgs)
  | isUndefinedXPrimVal v
  = let tyArgs = map Right tys
        tmArgs = map (Left . valToTerm) (vs ++ [v])
    in  Just $ flip setTerm m $ TyApp (Prim NP.undefinedX) $
          applyTypeToArgs (Prim p) tcm (primType p) (tyArgs ++ tmArgs)
  | otherwise
  = ghcPrimStep tcm (forcePrims m) p tys (vs ++ [v]) m

ghcPrimUnwind tcm p tys vs v [e] m0
  -- Note [Lazy primitives]
  -- ~~~~~~~~~~~~~~~~~~~~~~
  --
  -- Primitives are usually considered undefined when one of their arguments is
  -- (unless they're unused). _Some_ primitives can still yield a result even
  -- though one of their arguments is undefined. It turns out that all primitives
  -- exhibiting this property happen to be "lazy" in their last argument. Thus,
  -- all the cases can be covered by a match on [e] and their names:
  | primName p `elem` [  showt 'Clash.Sized.Vector.lazyV
                       , showt 'Clash.Sized.Vector.replicate
                       , "Clash.Sized.Vector.replace_int"
                       , showt '(GHC.Classes.&&)
                       , showt '(GHC.Classes.||)
                       , showt 'BitVector.xToBV
                       , "Clash.Sized.Vector.imap_go"
                       ]
  = if isUndefinedPrimVal v then
      let tyArgs = map Right tys
          tmArgs = map (Left . valToTerm) (vs ++ [v]) ++ [Left e]
      in  Just $ flip setTerm m0 $ TyApp (Prim NP.undefined) $
            applyTypeToArgs (Prim p) tcm (primType p) (tyArgs ++ tmArgs)
    else
      let (m1,i) = newLetBinding tcm m0 e
      in  ghcPrimStep tcm (forcePrims m0) p tys (vs ++ [v,Suspend (Var i)]) m1

ghcPrimUnwind tcm p tys vs (collectValueTicks -> (v, ts)) (e:es) m
  | isUndefinedPrimVal v
  = let tyArgs = map Right tys
        tmArgs = map (Left . valToTerm) (vs ++ [v]) ++ map Left (e:es)
    in  Just $ flip setTerm m $ TyApp (Prim NP.undefined) $
          applyTypeToArgs (Prim p) tcm (primType p) (tyArgs ++ tmArgs)
  | otherwise
  = Just . setTerm e $ stackPush (PrimApply p tys (vs ++ [foldr TickValue v ts]) es) m

newtype PrimEvalMonad a = PEM (State Supply a)
  deriving (Functor, Applicative, Monad, MonadState Supply)

instance MonadUnique PrimEvalMonad where
  getUniqueM = PEM $ State.state (\s -> case freshId s of (!i,!s') -> (i,s'))

runPEM :: PrimEvalMonad a -> Supply -> (a, Supply)
runPEM (PEM m) = State.runState m

ghcPrimStep :: PrimStep
ghcPrimStep tcm isSubj pInfo tys args mach =
  case HashMap.lookup (primName pInfo) ghcPrimStepImpls of
    Just impl -> impl tcm isSubj pInfo tys args mach
    Nothing -> Nothing

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
      let eval = Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
          mach1@Machine{mStack=[]} = whnf eval tcm isSubj (setTerm e $ stackClear mach)
      in Just $ mach1 { mStack = mStack mach }

    reduceWHNF' mach1 e =
      let eval = Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
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

ghcPrimStepImpls :: HashMap.HashMap Text PrimStep
ghcPrimStepImpls = HashMap.fromList
-----------------
-- GHC.Prim.Char#
-----------------
  [ ( $(textNameLit 'GHC.Prim.gtChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- charLiterals' args
            -> reduce (integerToIntLiteral (toInteger $ ord i))
          _ -> Nothing
    )

----------------
-- GHC.Prim.Int#
----------------
  , ( $(textNameLit '(GHC.Prim.+#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i+j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.-#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i-j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.*#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i*j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.mulIntMayOflo#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals  args
            -> let !(I# a)  = fromInteger i
                   !(I# b)  = fromInteger j
                   c :: Int#
                   c = mulIntMayOflo# a b
               in  reduce (integerToIntLiteral (toInteger $ I# c))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.quotInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce $ catchDivByZero (integerToIntLiteral (i `quot` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce $ catchDivByZero (integerToIntLiteral (i `rem` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   (q,r)   = quotRem i j
                   ret     = mkApps (Data tupDc) (map Right tyArgs ++
                            [Left $ catchDivByZero (integerToIntLiteral q)
                            ,Left $ catchDivByZero (integerToIntLiteral r)])
               in  reduce ret
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.andI#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i .&. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.orI#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i .|. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xorI#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i `xor` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.notI#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> reduce (integerToIntLiteral (complement i))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.negateInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (IntLiteral i)] <- args
            -> reduce (integerToIntLiteral (negate i))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.addIntC#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(I# a)  = fromInteger i
                   !(I# b)  = fromInteger j
                   !(# d, c #) = addIntC# a b
               in  reduce $
                   mkApps (Data tupDc) (map Right tyArgs ++
                           [ Left (Literal . IntLiteral . toInteger $ I# d)
                           , Left (Literal . IntLiteral . toInteger $ I# c)])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subIntC#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(I# a)  = fromInteger i
                   !(I# b)  = fromInteger j
                   !(# d, c #) = subIntC# a b
               in  reduce $
                   mkApps (Data tupDc) (map Right tyArgs ++
                           [ Left (Literal . IntLiteral . toInteger $ I# d)
                           , Left (Literal . IntLiteral . toInteger $ I# c)])
          _ -> Nothing
    )

  , ( $(textNameLit '(GHC.Prim.>#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.>=#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.==#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim./=#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.<#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}| Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.<=#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.chr#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> reduce (catchErrorCall (charToCharLiteral (chr $ fromInteger i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.int2Word#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (IntLiteral i)] <- args
            -> reduce . Literal . WordLiteral . toInteger $ (fromInteger :: Integer -> Word) i -- for overflow behavior
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.int2Float#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (IntLiteral i)] <- args
            -> reduce . Literal . FloatLiteral  . castFloatToWord32 $ fromInteger i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int2Double#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (IntLiteral i)] <- args
            -> reduce . Literal . DoubleLiteral . castDoubleToWord64 $ fromInteger i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.word2Float#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral i)] <- args
            -> reduce . Literal . FloatLiteral  . castFloatToWord32 $ fromInteger i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word2Double#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral i)] <- args
            -> reduce . Literal . DoubleLiteral . castDoubleToWord64 $ fromInteger i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ Lit (IntLiteral i)
              , Lit (IntLiteral s)
              ] <- args
            -> reduce (integerToIntLiteral (i `shiftL` fromInteger s))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftRA#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ Lit (IntLiteral i)
              , Lit (IntLiteral s)
              ] <- args
            -> reduce (integerToIntLiteral (i `shiftR` fromInteger s))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftRL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> let !(I# a)  = fromInteger i
                   !(I# b)  = fromInteger j
                   c :: Int#
                   c = uncheckedIShiftRL# a b
               in  reduce (integerToIntLiteral (toInteger $ I# c))
          _ -> Nothing
    )

-----------------
-- GHC.Prim.Word#
-----------------
  , ( $(textNameLit 'GHC.Prim.plusWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i+j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.subWordC#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(W# a)  = fromInteger i
                   !(W# b)  = fromInteger j
                   !(# d, c #) = subWordC# a b
               in  reduce $
                   mkApps (Data tupDc) (map Right tyArgs ++
                           [ Left (Literal . WordLiteral . toInteger $ W# d)
                           , Left (Literal . IntLiteral . toInteger $ I# c)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.plusWord2#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(W# a)  = fromInteger i
                   !(W# b)  = fromInteger j
                   !(# h', l #) = plusWord2# a b
               in  reduce $
                   mkApps (Data tupDc) (map Right tyArgs ++
                           [ Left (Literal . WordLiteral . toInteger $ W# h')
                           , Left (Literal . WordLiteral . toInteger $ W# l)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.minusWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i-j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i*j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.timesWord2#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(W# a)  = fromInteger i
                   !(W# b)  = fromInteger j
                   !(# h', l #) = timesWord2# a b
               in  reduce $
                   mkApps (Data tupDc) (map Right tyArgs ++
                           [ Left (Literal . WordLiteral . toInteger $ W# h')
                           , Left (Literal . WordLiteral . toInteger $ W# l)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.quotWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce $ catchDivByZero (integerToWordLiteral (i `quot` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce $ catchDivByZero (integerToWordLiteral (i `rem` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   (q,r)   = quotRem i j
                   ret     = mkApps (Data tupDc) (map Right tyArgs ++
                            [Left $ catchDivByZero (integerToWordLiteral q)
                            ,Left $ catchDivByZero (integerToWordLiteral r)])
               in  reduce ret
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemWord2#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i,j,k'] <- wordLiterals' args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(W# a)  = fromInteger i
                   !(W# b)  = fromInteger j
                   !(W# c)  = fromInteger k'
                   !(# x, y #) = quotRemWord2# a b c
               in  reduce $
                   mkApps (Data tupDc) (map Right tyArgs ++
                           [ Left $ catchDivByZero (Literal . WordLiteral . toInteger $ W# x)
                           , Left $ catchDivByZero (Literal . WordLiteral . toInteger $ W# y)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.and#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i .&. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.or#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i .|. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xor#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i `xor` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.not#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce (integerToWordLiteral (complement i))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.uncheckedShiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ Lit (WordLiteral w)
              , Lit (IntLiteral  i)
              ] <- args
            -> reduce (Literal (WordLiteral (w `shiftL` fromInteger i)))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ Lit (WordLiteral w)
              , Lit (IntLiteral  i)
              ] <- args
            -> reduce (Literal (WordLiteral (w `shiftR` fromInteger i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.word2Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral i)] <- args
            -> reduce . Literal . IntLiteral . toInteger $ (fromInteger :: Integer -> Int) i -- for overflow behavior
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.gtWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.popCnt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word8) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.popCnt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word16) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.popCnt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word32) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.popCnt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word64) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.popCnt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word) $ i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.clz8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word8) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.clz16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word16) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.clz32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word32) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.clz64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word64) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.clz#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word) $ i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.ctz8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 8 - 1)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ctz16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 16 - 1)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ctz32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 32 - 1)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ctz64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word64) $ i .&. (bit 64 - 1)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ctz#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.byteSwap16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . byteSwap16 . (fromInteger :: Integer -> Word16) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.byteSwap32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . byteSwap32 . (fromInteger :: Integer -> Word32) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.byteSwap64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . integerToWordLiteral . toInteger . byteSwap64 . (fromInteger :: Integer -> Word64) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.byteSwap#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args -- assume 64bits
            -> reduce . integerToWordLiteral . toInteger . byteSwap64 . (fromInteger :: Integer -> Word64) $ i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.bitReverse#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . bitReverse64 . fromInteger $ i -- assume 64bits
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.bitReverse8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . bitReverse8 . fromInteger $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.bitReverse16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . bitReverse16 . fromInteger $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.bitReverse32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . bitReverse32 . fromInteger $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.bitReverse64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . integerToWordLiteral . toInteger . bitReverse64 . fromInteger $ i
          _ -> Nothing
    )
------------
-- Narrowing
------------
  , ( $(textNameLit 'GHC.Prim.narrow8Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow8Int# a
               in  reduce . Literal . IntLiteral . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.narrow16Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow16Int# a
               in  reduce . Literal . IntLiteral . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.narrow32Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow32Int# a
               in  reduce . Literal . IntLiteral . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.narrow8Word#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow8Word# a
               in  reduce . Literal . WordLiteral . toInteger $ W# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.narrow16Word#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow16Word# a
               in  reduce . Literal . WordLiteral . toInteger $ W# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.narrow32Word#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow32Word# a
               in  reduce . Literal . WordLiteral . toInteger $ W# b
          _ -> Nothing
    )

--------
-- Int8#
--------
  , ( $(textNameLit 'GHC.Prim.intToInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow8Int# a
               in  reduce . Literal . Int8Literal . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int8ToInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int8Literals' args
            -> reduce . Literal $ IntLiteral i
          _ -> Nothing
    )
  -- XXX: Primitive does not exist?
  , ( "GHC.Prim.negateInt8"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int8Literals' args
            -> let !(I8# a) = fromInteger i
                in reduce (Literal (Int8Literal (toInteger (I8# (negateInt8# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8 plusInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8 subInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8 timesInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int8Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int8Literal (toInteger (fromInteger i `quot` fromInteger j :: Int8))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int8Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int8Literal (toInteger (fromInteger i `rem` fromInteger j :: Int8))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- int8Literals' args
            , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
            , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
            , [tupDc] <- tyConDataCons tupTc
            -> let (q,r) = quotRem (fromInteger i :: Int8) (fromInteger j)
               in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                          [ Left $ catchDivByZero (Literal (Int8Literal (toInteger q)))
                          , Left $ catchDivByZero (Literal (Int8Literal (toInteger r)))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8I uncheckedShiftLInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRAInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8I uncheckedShiftRAInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8I uncheckedShiftRLInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int8ToWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int8Literals' args
            -> let !(I8# a) = fromInteger i
                in reduce (Literal (Word8Literal (toInteger (W8# (int8ToWord8# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI eqInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI geInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI gtInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI leInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI ltInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI neInt8# args
            -> reduce r
          _ -> Nothing
    )

---------
-- Int16#
---------
  , ( $(textNameLit 'GHC.Prim.intToInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow16Int# a
               in  reduce . Literal . Int16Literal . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int16ToInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int16Literals' args
            -> reduce . Literal $ IntLiteral i
          _ -> Nothing
    )
  -- XXX: Primitive does not exist?
  , ( "GHC.Prim.negateInt16"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int16Literals' args
            -> let !(I16# a) = fromInteger i
                in reduce (Literal (Int16Literal (toInteger (I16# (negateInt16# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16 plusInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16 subInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16 timesInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int16Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int16Literal (toInteger (fromInteger i `quot` fromInteger j :: Int16))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int16Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int16Literal (toInteger (fromInteger i `rem` fromInteger j :: Int16))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- int16Literals' args
            , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
            , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
            , [tupDc] <- tyConDataCons tupTc
            -> let (q,r) = quotRem (fromInteger i :: Int16) (fromInteger j)
               in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                          [ Left $ catchDivByZero (Literal (Int16Literal (toInteger q)))
                          , Left $ catchDivByZero (Literal (Int16Literal (toInteger r)))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16I uncheckedShiftLInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRAInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16I uncheckedShiftRAInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16I uncheckedShiftRLInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int16ToWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int16Literals' args
            -> let !(I16# a) = fromInteger i
                in reduce (Literal (Word16Literal (toInteger (W16# (int16ToWord16# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI eqInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI geInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI gtInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI leInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI ltInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI neInt16# args
            -> reduce r
          _ -> Nothing
    )

---------
-- Int32#
---------
  , ( $(textNameLit 'GHC.Prim.intToInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow32Int# a
               in  reduce . Literal . Int32Literal . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int32ToInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int32Literals' args
            -> reduce . Literal $ IntLiteral i
          _ -> Nothing
    )
  -- XXX: Primitive does not exist?
  , ( "GHC.Prim.negateInt32"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int32Literals' args
            -> let !(I32# a) = fromInteger i
                in reduce (Literal (Int32Literal (toInteger (I32# (negateInt32# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32 plusInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32 subInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32 timesInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int32Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int32Literal (toInteger (fromInteger i `quot` fromInteger j :: Int32))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int32Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int32Literal (toInteger (fromInteger i `rem` fromInteger j :: Int32))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- int32Literals' args
            , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
            , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
            , [tupDc] <- tyConDataCons tupTc
            -> let (q,r) = quotRem (fromInteger i :: Int32) (fromInteger j)
               in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                          [ Left $ catchDivByZero (Literal (Int32Literal (toInteger q)))
                          , Left $ catchDivByZero (Literal (Int32Literal (toInteger r)))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32I uncheckedShiftLInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRAInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32I uncheckedShiftRAInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32I uncheckedShiftRLInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int32ToWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int32Literals' args
            -> let !(I32# a) = fromInteger i
                in reduce (Literal (Word32Literal (toInteger (W32# (int32ToWord32# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI eqInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI geInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI gtInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI leInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI ltInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI neInt32# args
            -> reduce r
          _ -> Nothing
    )

---------
-- Int64#
---------
  , ( $(textNameLit 'GHC.Prim.intToInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> reduce (Literal (Int64Literal i))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int64ToInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int64Literals' args
            -> reduce . Literal $ IntLiteral i
          _ -> Nothing
    )
  -- XXX: Primitive does not exist?
  , ( "GHC.Prim.negateInt64"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int64Literals' args
            -> let !(I64# a) = fromInteger i
                in reduce (Literal (Int64Literal (toInteger (I64# (negateInt64# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64 plusInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64 subInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64 timesInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int64Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int64Literal (toInteger (fromInteger i `quot` fromInteger j :: Int64))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int64Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int64Literal (toInteger (fromInteger i `rem` fromInteger j :: Int64))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftL64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64I uncheckedIShiftL64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftRA64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64I uncheckedIShiftRA64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftRL64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64I uncheckedIShiftRL64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int64ToWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int64Literals' args
            -> let !(I64# a) = fromInteger i
                in reduce (Literal (Word64Literal (toInteger (W64# (int64ToWord64# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI eqInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI geInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI gtInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI leInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI ltInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI neInt64# args
            -> reduce r
          _ -> Nothing
    )

---------
-- Word8#
---------
  , ( $(textNameLit 'GHC.Prim.wordToWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow8Word# a
               in  reduce . Literal . Word8Literal . toInteger $ W# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word8ToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word8Literals' args
            -> reduce . Literal $ WordLiteral i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 plusWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 subWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 timesWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word8Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word8Literal (toInteger (fromInteger i `quot` fromInteger j :: Word8))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word8Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word8Literal (toInteger (fromInteger i `rem` fromInteger j :: Word8))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- word8Literals' args
            , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
            , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
            , [tupDc] <- tyConDataCons tupTc
            -> let (q,r) = quotRem (fromInteger i :: Word8) (fromInteger j)
               in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                          [ Left $ catchDivByZero (Literal (Word8Literal (toInteger q)))
                          , Left $ catchDivByZero (Literal (Word8Literal (toInteger r)))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.andWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 andWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.orWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 orWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xorWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 xorWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.notWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word8Literals' args
            -> let !(W8# a) = fromInteger i
                in reduce (Literal (Word8Literal (toInteger (W8# (notWord8# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8I uncheckedShiftLWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8I uncheckedShiftRLWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word8ToInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word8Literals' args
            -> let !(W8# a) = fromInteger i
                in reduce (Literal (Int8Literal (toInteger (I8# (word8ToInt8# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI eqWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI geWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI gtWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI leWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI ltWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI neWord8# args
            -> reduce r
          _ -> Nothing
    )

----------
-- Word16#
----------
  , ( $(textNameLit 'GHC.Prim.wordToWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow16Word# a
               in  reduce . Literal . Word16Literal . toInteger $ W# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word16ToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word16Literals' args
            -> reduce . Literal $ WordLiteral i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 plusWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 subWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 timesWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word16Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word16Literal (toInteger (fromInteger i `quot` fromInteger j :: Word16))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word16Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word16Literal (toInteger (fromInteger i `rem` fromInteger j :: Word16))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- word16Literals' args
            , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
            , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
            , [tupDc] <- tyConDataCons tupTc
            -> let (q,r) = quotRem (fromInteger i :: Word16) (fromInteger j)
               in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                          [ Left $ catchDivByZero (Literal (Word16Literal (toInteger q)))
                          , Left $ catchDivByZero (Literal (Word16Literal (toInteger r)))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.andWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 andWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.orWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 orWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xorWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 xorWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.notWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word16Literals' args
            -> let !(W16# a) = fromInteger i
                in reduce (Literal (Word16Literal (toInteger (W16# (notWord16# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16I uncheckedShiftLWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16I uncheckedShiftRLWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word16ToInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word16Literals' args
            -> let !(W16# a) = fromInteger i
                in reduce (Literal (Int16Literal (toInteger (I16# (word16ToInt16# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI eqWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI geWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI gtWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI leWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI ltWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI neWord16# args
            -> reduce r
          _ -> Nothing
    )

----------
-- Word32#
----------
  , ( $(textNameLit 'GHC.Prim.wordToWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow32Word# a
               in  reduce . Literal . Word32Literal . toInteger $ W# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word32ToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word32Literals' args
            -> reduce . Literal $ WordLiteral i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 plusWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 subWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 timesWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word32Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word32Literal (toInteger (fromInteger i `quot` fromInteger j :: Word32))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word32Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word32Literal (toInteger (fromInteger i `rem` fromInteger j :: Word32))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- word32Literals' args
            , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
            , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
            , [tupDc] <- tyConDataCons tupTc
            -> let (q,r) = quotRem (fromInteger i :: Word32) (fromInteger j)
               in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                          [ Left $ catchDivByZero (Literal (Word32Literal (toInteger q)))
                          , Left $ catchDivByZero (Literal (Word32Literal (toInteger r)))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.andWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 andWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.orWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 orWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xorWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 xorWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.notWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word32Literals' args
            -> let !(W32# a) = fromInteger i
                in reduce (Literal (Word32Literal (toInteger (W32# (notWord32# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32I uncheckedShiftLWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32I uncheckedShiftRLWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word32ToInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word32Literals' args
            -> let !(W32# a) = fromInteger i
                in reduce (Literal (Int32Literal (toInteger (I32# (word32ToInt32# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI eqWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI geWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI gtWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI leWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI ltWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI neWord32# args
            -> reduce r
          _ -> Nothing
    )

----------
-- Word64#
----------
  , ( $(textNameLit 'GHC.Prim.wordToWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce (Literal (Word64Literal i))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word64ToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . Literal $ WordLiteral i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 plusWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 subWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 timesWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word64Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word64Literal (toInteger (fromInteger i `quot` fromInteger j :: Word64))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word64Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word64Literal (toInteger (fromInteger i `rem` fromInteger j :: Word64))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.and64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 and64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.or64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 or64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xor64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 xor64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.not64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> let !(W64# a) = fromInteger i
                in reduce (Literal (Word64Literal (toInteger (W64# (not64# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftL64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64I uncheckedShiftL64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRL64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64I uncheckedShiftRL64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word64ToInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> let !(W64# a) = fromInteger i
                in reduce (Literal (Int64Literal (toInteger (I64# (word64ToInt64# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI eqWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI geWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI gtWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI leWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI ltWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI neWord64# args
            -> reduce r
          _ -> Nothing
    )

----------
-- Double#
----------
  , ( $(textNameLit '(GHC.Prim.>##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDI (>##)  args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.>=##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDDI (>=##) args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.==##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDDI (==##) args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim./=##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDDI (/=##) args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.<##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDI (<##)  args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.<=##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDDI (<=##) args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.+##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDD (+##)  args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.-##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDD (-##)  args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.*##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDD (*##)  args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim./##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDD (/##)  args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.negateDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD negateDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.fabsDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD fabsDouble# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.double2Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- doubleLiterals' args
            -> let !(D# a) = castWord64ToDouble i
                   r = double2Int# a
               in  reduce . Literal . IntLiteral . toInteger $ I# r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.double2Float#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (DoubleLiteral d)] <- args
            -> let !(D# a) = castWord64ToDouble d
                   r = double2Float# a
               in reduce . Literal . FloatLiteral . castFloatToWord32 $ F# r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.expDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD expDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.logDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD logDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sqrtDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD sqrtDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sinDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD sinDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.cosDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD cosDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.tanDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD tanDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.asinDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD asinDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.acosDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD acosDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.atanDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD atanDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sinhDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD sinhDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.coshDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD coshDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.tanhDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD tanhDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.asinhDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDD asinhDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.acoshDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDD acoshDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.atanhDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDD atanhDouble# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit '(GHC.Prim.**##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDDD (**##) args
            -> reduce r
          _ -> Nothing
    )
-- decodeDouble_2Int# :: Double# -> (#Int#, Word#, Word#, Int##)
  , ( $(textNameLit 'GHC.Prim.decodeDouble_2Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- doubleLiterals' args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(D# a) = castWord64ToDouble i
                   !(# p, q, r, s #) = decodeDouble_2Int# a
               in reduce $
                  mkApps (Data tupDc) (map Right tyArgs ++
                           [ Left (Literal . IntLiteral  . toInteger $ I# p)
                           , Left (Literal . WordLiteral . toInteger $ W# q)
                           , Left (Literal . WordLiteral . toInteger $ W# r)
                           , Left (Literal . IntLiteral  . toInteger $ I# s)])
          _ -> Nothing
    )
-- decodeDouble_Int64# :: Double# -> (# Int64#, Int# #)
  , ( $(textNameLit 'GHC.Prim.decodeDouble_Int64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- doubleLiterals' args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(D# a) = castWord64ToDouble i
                   !(# p, q #) = decodeDouble_Int64# a
               in reduce $
                  mkApps (Data tupDc) (map Right tyArgs ++
                           [ Left (Literal . Int64Literal  . toInteger $ I64# p)
                           , Left (Literal . IntLiteral  . toInteger $ I# q)])
          _ -> Nothing
    )

--------
-- Float
--------
  , ( $(textNameLit 'GHC.Prim.gtFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI gtFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI geFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI eqFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI neFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI ltFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI leFloat# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.plusFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFF plusFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.minusFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFF minusFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFF timesFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.divideFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFF divideFloat# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.negateFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF negateFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.fabsFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF fabsFloat# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.float2Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- floatLiterals' args
            -> let !(F# a) = castWord32ToFloat i
                   r = float2Int# a
               in  reduce . Literal . IntLiteral . toInteger $ I# r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.expFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF expFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.logFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF logFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sqrtFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF sqrtFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sinFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF sinFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.cosFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF cosFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.tanFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF tanFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.asinFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF asinFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.acosFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF acosFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.atanFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF atanFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sinhFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF sinhFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.coshFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF coshFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.tanhFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF tanhFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.powerFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFF powerFloat# args
            -> reduce r
          _ -> Nothing
    )

  -- GHC.Float.asinh  -- XXX: Very fragile
  --  $w$casinh is the Double specialisation of asinh
  --  $w$casinh1 is the Float specialisation of asinh
  , ( "GHC.Float.$w$casinh"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD go args
            -> reduce r
            where go f = case asinh (D# f) of
                           D# f' -> f'
          _ -> Nothing
    )
  , ( "GHC.Float.$w$casinh1"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftFF go args
            -> reduce r
            where go f = case asinh (F# f) of
                           F# f' -> f'
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.asinhFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF asinhFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.acoshFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF acoshFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.atanhFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF atanhFloat# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.float2Double#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- floatLiterals' args
            -> let !(F# a) = castWord32ToFloat i
                   r = float2Double# a
               in  reduce . Literal . DoubleLiteral . castDoubleToWord64 $ D# r
          _ -> Nothing
    )


  , ( $(textNameLit 'GHC.Prim.newByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV,PrimVal rwTy _ _] <- args
            , [i] <- intLiterals' [iV]
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   p = primCount mach
                   lit = Literal (ByteArrayLiteral (fromList (List.genericReplicate i 0)))
                   mbaTy = mkFunTy intPrimTy (last tyArgs)
                   newE = mkApps (Data tupDc) (map Right tyArgs ++
                            [Left (Prim rwTy)
                            ,Left (mkApps (Prim (PrimInfo (showt ''MutableByteArray#) mbaTy WorkNever SingleResult NoUnfolding))
                                          [Left (Literal . IntLiteral $ toInteger p)])
                            ])
               in Just . setTerm newE $ primInsert p lit mach
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.setByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
            | [PrimVal _mbaTy _ [baV]
              ,offV,lenV,cV
              ,PrimVal rwTy _ _
              ] <- args
            , [ba,off,len,c] <- intLiterals' [baV,offV,lenV,cV]
            -> let Just (Literal (ByteArrayLiteral ba1)) =
                      primLookup (fromInteger ba) mach
                   !(I# off') = fromInteger off
                   !(I# len') = fromInteger len
                   !(I# c')   = fromInteger c
                   ba2 = unsafeDupablePerformIO $ do
                          BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                          svoid (setByteArray# mba off' len' c')
                          BA.unsafeFreezeByteArray (BA.MutableByteArray mba)
                   ba3 = Literal (ByteArrayLiteral ba2)
               in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger ba) ba3 mach
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.writeWordArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
            | [PrimVal _mbaTy _  [baV]
              ,iV,wV
              ,PrimVal rwTy _ _
              ] <- args
            , [ba,i] <- intLiterals' [baV,iV]
            , [w] <- wordLiterals' [wV]
            -> let Just (Literal (ByteArrayLiteral ba1)) =
                      primLookup (fromInteger ba) mach
                   !(I# i') = fromInteger i
                   !(W# w') = fromIntegral w
                   ba2 = unsafeDupablePerformIO $ do
                          BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                          svoid (writeWordArray# mba i' w')
                          BA.unsafeFreezeByteArray (BA.MutableByteArray mba)
                   ba3 = Literal (ByteArrayLiteral ba2)
               in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger ba) ba3 mach
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.unsafeFreezeByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal _mbaTy _ [baV]
              ,PrimVal rwTy _ _
              ] <- args
            , [ba] <-  intLiterals' [baV]
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   Just ba' = primLookup (fromInteger ba) mach
               in  reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                              [Left (Prim rwTy)
                              ,Left ba'])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.sizeofByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral ba)] <- args
            -> reduce (Literal (IntLiteral (toInteger (BA.sizeofByteArray ba))))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.indexWordArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral (BA.ByteArray ba)),iV] <- args
            , [i] <- intLiterals' [iV]
            -> let !(I# i') = fromInteger i
                   !w       = indexWordArray# ba i'
               in  reduce (Literal (WordLiteral (toInteger (W# w))))
          _ -> Nothing
    )

  -- XXX: Primitive does not exist?
  , ( "GHC.Prim.getSizeofMutBigNat#"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal _mbaTy _ [baV]
              ,PrimVal rwTy _ _
              ] <- args
            , [ba] <- intLiterals' [baV]
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   Just (Literal (ByteArrayLiteral ba')) = primLookup (fromInteger ba) mach
                   lit = Literal (IntLiteral (toInteger (BA.sizeofByteArray ba')))
               in  reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                              [Left (Prim rwTy)
                              ,Left lit])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.resizeMutableByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal mbaTy _ [baV]
              ,iV
              ,PrimVal rwTy _ _
              ] <- args
            , [ba,i] <- intLiterals' [baV,iV]
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   p = primCount mach
                   Just (Literal (ByteArrayLiteral ba1))
                    = primLookup (fromInteger ba) mach
                   !(I# i') = fromInteger i
                   ba2 = unsafeDupablePerformIO $ do
                           BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                           mba' <- IO (\s -> case resizeMutableByteArray# mba i' s of
                                         (# s', mba' #) -> (# s', BA.MutableByteArray mba' #))
                           BA.unsafeFreezeByteArray mba'
                   ba3 = Literal (ByteArrayLiteral ba2)
                   newE = mkApps (Data tupDc) (map Right tyArgs ++
                            [Left (Prim rwTy)
                            ,Left (mkApps (Prim mbaTy)
                                          [Left (Literal . IntLiteral $ toInteger p)])
                            ])
               in Just . setTerm newE $ primInsert p ba3 mach
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.shrinkMutableByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
            | [PrimVal _mbaTy _ [baV]
              ,lenV
              ,PrimVal rwTy _ _
              ] <- args
            , [ba,len] <- intLiterals' [baV,lenV]
            -> let Just (Literal (ByteArrayLiteral ba1)) =
                      primLookup (fromInteger ba) mach
                   !(I# len') = fromInteger len
                   ba2 = unsafeDupablePerformIO $ do
                          BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                          svoid (shrinkMutableByteArray# mba len')
                          BA.unsafeFreezeByteArray (BA.MutableByteArray mba)
                   ba3 = Literal (ByteArrayLiteral ba2)
               in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger ba) ba3 mach
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.copyByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
            | [Lit (ByteArrayLiteral (BA.ByteArray src_ba))
              ,src_offV
              ,PrimVal _mbaTy _ [dst_mbaV]
              ,dst_offV, nV
              ,PrimVal rwTy _ _
              ] <- args
            , [src_off,dst_mba,dst_off,n] <- intLiterals' [src_offV,dst_mbaV,dst_offV,nV]
            -> let Just (Literal (ByteArrayLiteral dst_ba)) =
                      primLookup (fromInteger dst_mba) mach
                   !(I# src_off') = fromInteger src_off
                   !(I# dst_off') = fromInteger dst_off
                   !(I# n')       = fromInteger n
                   ba2 = unsafeDupablePerformIO $ do
                          BA.MutableByteArray dst_mba1 <- BA.unsafeThawByteArray dst_ba
                          svoid (copyByteArray# src_ba src_off' dst_mba1 dst_off' n')
                          BA.unsafeFreezeByteArray (BA.MutableByteArray dst_mba1)
                   ba3 = Literal (ByteArrayLiteral ba2)
               in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger dst_mba) ba3 mach
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.readWordArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal _mbaTy _  [baV]
              ,offV
              ,PrimVal rwTy _ _
              ] <- args
            , [ba,off] <- intLiterals' [baV,offV]
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   Just (Literal (ByteArrayLiteral ba1)) =
                      primLookup (fromInteger ba) mach
                   !(I# off') = fromInteger off
                   w = unsafeDupablePerformIO $ do
                          BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                          IO (\s -> case readWordArray# mba off' s of
                                (# s', w' #) -> (# s',  W# w' #))
                   newE = mkApps (Data tupDc) (map Right tyArgs ++
                            [Left (Prim rwTy)
                            ,Left (Literal (WordLiteral (toInteger w)))
                            ])
               in reduce newE
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.copyAddrToByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
            | [ Lit (StringLiteral addr)
              , PrimVal _mbaTy _ [dst_mbaV]
              , offV, lenV
              , PrimVal rwTy _ _
              ] <- args
            , [off,len,dst_mba] <- intLiterals' [offV, lenV, dst_mbaV]
            -> let Just (Literal (ByteArrayLiteral dst_ba)) =
                      primLookup (fromInteger dst_mba) mach
                   !(I# off') = fromInteger off
                   !(I# len') = fromInteger len
                   !(BS.PS (ForeignPtr addr' _) _ _) = BS.packChars addr
                   ba2 = unsafeDupablePerformIO $ do
                            BA.MutableByteArray dst_mba1 <- BA.unsafeThawByteArray dst_ba
                            svoid (copyAddrToByteArray# addr' dst_mba1 off' len')
                            BA.unsafeFreezeByteArray (BA.MutableByteArray dst_mba1)
                   ba3 = Literal (ByteArrayLiteral ba2)
                in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger dst_mba) ba3 mach
          _ -> Nothing
    )

-- decodeFloat_Int# :: Float# -> (#Int#, Int##)
  , ( $(textNameLit 'GHC.Prim.decodeFloat_Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- floatLiterals' args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(F# a) = castWord32ToFloat i
                   !(# p, q #) = decodeFloat_Int# a
               in reduce $
                  mkApps (Data tupDc) (map Right tyArgs ++
                           [ Left (Literal . IntLiteral  . toInteger $ I# p)
                           , Left (Literal . IntLiteral  . toInteger $ I# q)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.tagToEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
            | [ConstTy (TyCon tcN)] <- tys
            , [Lit (IntLiteral i)]  <- args
            -> let dc = do { tc <- UniqMap.lookup tcN tcm
                           ; let dcs = tyConDataCons tc
                           ; List.find ((== (i+1)) . toInteger . dcTag) dcs
                           }
               in (\e -> setTerm (Data e) mach) <$> dc
          _ -> Nothing
    )

#if MIN_VERSION_ghc_prim(0,12,0)
  , ( $(textNameLit 'GHC.PrimopWrappers.dataToTagSmall#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.PrimopWrappers.dataToTagLarge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
#endif
#if MIN_VERSION_ghc(9,12,0)
  , ( $(textNameLit 'GHC.Magic.dataToTag#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
#endif
#if MIN_VERSION_ghc(9,10,0)
  , ( $(textNameLit 'GHC.Prim.dataToTagSmall#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.dataToTagLarge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Base.dataToTag#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
#else
  , ( $(textNameLit 'GHC.Prim.dataToTag#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
#endif

  , ( $(textNameLit 'GHC.Classes.eqInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.neInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.leInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.ltInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i < j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.geInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.gtInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i > j))
          _ -> Nothing
    )

  , ( $(textNameLit '(GHC.Classes.&&))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ lArg , rArg ] <- args
            , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
            -- evaluation of the arguments is deferred until the evaluation of the ghcPrimUnwindWith
            -- to make `&&` lazy in both arguments
            , mach1@Machine{mStack=[],mTerm=lArgWHNF} <- whnf eval tcm True (setTerm (valToTerm lArg) $ stackClear mach)
            , mach2@Machine{mStack=[],mTerm=rArgWHNF} <- whnf eval tcm True (setTerm (valToTerm rArg) $ stackClear mach1)
            -> case [ lArgWHNF, rArgWHNF ] of
                 [ Data lCon, Data rCon ] ->
                   Just $ mach2
                     { mStack = mStack mach
                     , mTerm = boolToBoolLiteral tcm ty (isTrueDC lCon && isTrueDC rCon)
                     }

                 [ Data lCon, _ ]
                   | isTrueDC lCon -> reduce rArgWHNF
                   | otherwise     -> reduce (boolToBoolLiteral tcm ty False)

                 [ _, Data rCon ]
                   | isTrueDC rCon -> reduce lArgWHNF
                   | otherwise     -> reduce (boolToBoolLiteral tcm ty False)

                 _ -> Nothing
          _ -> Nothing
    )

  , ( $(textNameLit '(GHC.Classes.||))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ lArg , rArg ] <- args
            , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
            -- evaluation of the arguments is deferred until the evaluation of the ghcPrimUnwindWith
            -- to make `||` lazy in both arguments
            , mach1@Machine{mStack=[],mTerm=lArgWHNF} <- whnf eval tcm True (setTerm (valToTerm lArg) $ stackClear mach)
            , mach2@Machine{mStack=[],mTerm=rArgWHNF} <- whnf eval tcm True (setTerm (valToTerm rArg) $ stackClear mach1)
            -> case [ lArgWHNF, rArgWHNF ] of
                 [ Data lCon, Data rCon ] ->
                   Just $ mach2
                     { mStack = mStack mach
                     , mTerm = boolToBoolLiteral tcm ty (isTrueDC lCon || isTrueDC rCon)
                     }

                 [ Data lCon, _ ]
                   | isFalseDC lCon -> reduce rArgWHNF
                   | otherwise      -> reduce (boolToBoolLiteral tcm ty True)

                 [ _, Data rCon ]
                   | isFalseDC rCon -> reduce lArgWHNF
                   | otherwise      -> reduce (boolToBoolLiteral tcm ty True)

                 _ -> Nothing
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.divInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (catchDivByZero (integerToIntLiteral (i `div` j)))
          _ -> Nothing
    )

  -- modInt# :: Int# -> Int# -> Int#
  , ( $(textNameLit 'GHC.Classes.modInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [dividend, divisor] <- intLiterals' args
            ->
              if divisor == 0 then
                let iTy = snd (splitFunForallTy ty) in
                reduce (TyApp (Prim NP.undefined) iTy)
              else
                reduce (Literal (IntLiteral (dividend `mod` divisor)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.not)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC bCon _] <- args
            -> reduce (boolToBoolLiteral tcm ty (nameOcc (dcName bCon) == showt 'False))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerLogBase#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (a,b) <- integerLiterals args
            , Just c <- flogBase a b
            -> (reduce . Literal . WordLiteral . toInteger) c
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Float.integerToFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [v] <- args
            , Just i <- integerLiteral v
            -> reduce . Literal . FloatLiteral . castFloatToWord32 $ F# (integerToFloat# i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Float.integerToDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [v] <- args
            , Just i <- integerLiteral v
            -> reduce . Literal . DoubleLiteral . castDoubleToWord64 $ D# (integerToDouble# i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalLogBase#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (a,b) <- naturalLiterals args
            , Just c <- flogBase a b
            -> (reduce . Literal . WordLiteral . toInteger) c
          _ -> Nothing
    )


  , ( $(textNameLit 'GHC.Num.Integer.integerToInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (integerToIntLiteral i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerDecodeDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Double# -> (#Integer, Int##)
            | [Lit (DoubleLiteral i)] <- args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(D# a)  = castWord64ToDouble i
                   !(# b, c #) = decodeDoubleInteger a
            in reduce $
               mkApps (Data tupDc) (map Right tyArgs ++
                        [ Left (integerToIntegerLiteral b)
                        , Left (integerToIntLiteral . toInteger $ I# c)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerEncodeDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Integer -> Int# -> Double
            | [iV, Lit (IntLiteral j)] <- args
            , [i] <- integerLiterals' [iV]
            -> let !(I# k') = fromInteger j
                   r = encodeDoubleInteger i k'
            in  reduce . Literal . DoubleLiteral . castDoubleToWord64 $ D# r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerEncodeFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (IntLiteral j)] <- args
            , [i] <- integerLiterals' [iV]
            -> let !(I# k') = fromInteger j
                   r = integerEncodeFloat# i k'
                in reduce . Literal . FloatLiteral . castFloatToWord32 $ F# r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerQuotRem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Integer -> Integer -> (#Integer, Integer#)
            | [i, j] <- integerLiterals' args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   (q,r) = quotRem i j
            in reduce $
                 mkApps (Data tupDc) (map Right tyArgs ++
                        [ Left $ catchDivByZero (integerToIntegerLiteral q)
                        , Left $ catchDivByZero (integerToIntegerLiteral r)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerAdd)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (integerToIntegerLiteral (i+j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerSub)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (integerToIntegerLiteral (i-j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerMul)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (integerToIntegerLiteral (i*j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerNegate)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (integerToIntegerLiteral (negate i))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerDiv)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce $ catchDivByZero (integerToIntegerLiteral (i `div` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerMod)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce $ catchDivByZero (integerToIntegerLiteral (i `mod` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerQuot)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce $ catchDivByZero (integerToIntegerLiteral (i `quot` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerRem)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce $ catchDivByZero (integerToIntegerLiteral (i `rem` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerDivMod#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> let (_,tyView -> TyConApp ubTupTcNm [liftedKi,_,intTy,_]) = splitFunForallTy ty
                   (Just ubTupTc) = UniqMap.lookup ubTupTcNm tcm
                   [ubTupDc] = tyConDataCons ubTupTc
                   (d,m) = divMod i j
               in  reduce $
                   mkApps (Data ubTupDc) [ Right liftedKi, Right liftedKi
                                         , Right intTy,    Right intTy
                                         , Left $ catchDivByZero (Literal (IntegerLiteral d))
                                         , Left $ catchDivByZero (Literal (IntegerLiteral m))
                                         ]
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerGt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i > j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerGe)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerEq)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerNe)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerLt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i < j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerLe)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerGt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerGe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerEq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerNe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerLt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerLe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerCompare)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- integerLiterals' args
            -> let -- Get the required result type (viewed as an applied type constructor name)
                   (_,tyView -> TyConApp tupTcNm []) = splitFunForallTy ty
                   -- Find the type constructor from the name
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   -- Get the data constructors of that type
                   -- The type is 'Ordering', so they are: 'LT', 'EQ', 'GT'
                   [ltDc, eqDc, gtDc] = tyConDataCons tupTc
                   -- Do the actual compile-time evaluation
                   ordVal = compareInteger i j
                in reduce $ case ordVal of
                    LT -> Data ltDc
                    EQ -> Data eqDc
                    GT -> Data gtDc
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerShiftR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (WordLiteral j)] <- args
            , [i] <- integerLiterals' [iV]
            -> reduce (integerToIntegerLiteral (i `shiftR` fromInteger j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerShiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (WordLiteral j)] <- args
            , [i] <- integerLiterals' [iV]
            -> reduce (integerToIntegerLiteral (i `shiftL` fromInteger j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerFromWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral w)] <- args
            -> reduce (Literal (IntegerLiteral w))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (integerToWordLiteral i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerTestBit#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Integer -> Int# -> Int#
            | [Lit (IntegerLiteral i), Lit (WordLiteral j)] <- args
            -> reduce (boolToIntLiteral (testBit i (fromInteger j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.NS)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral w)] <- args
            -> reduce (Literal (NaturalLiteral w))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Num.NB)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral (BA.ByteArray ba))] <- args
            -> reduce (Literal (NaturalLiteral (IP ba)))
            | [Lit l] <- args
            -> error ("NB: " <> show l)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Num.Integer.IS)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (IntLiteral i)] <- args
            -> reduce (Literal (IntegerLiteral i))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Num.Integer.IP)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral (BA.ByteArray ba))] <- args
            -> reduce (Literal (IntegerLiteral (IP ba)))
            | [Lit l] <- args
            -> error ("IP: " <> show l)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Num.Integer.IN)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral (BA.ByteArray ba))] <- args
            -> reduce (Literal (IntegerLiteral (IN ba)))
            | [Lit l] <- args
            -> error ("IN: " <> show l)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerFromNatural)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- naturalLiterals' args
            -> reduce (Literal (IntegerLiteral (toInteger i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToNatural)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange1 nTy i id)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToNaturalClamp)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> if i < 0 then
                 reduce (naturalToNaturalLiteral 0)
               else
                 reduce (naturalToNaturalLiteral (fromInteger i))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToNaturalThrow)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> let nTy = snd (splitFunForallTy ty) in
               reduce (checkNaturalRange1 nTy i id)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (integerToInt64Literal i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (integerToWord64Literal i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerFromWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [w] <- word64Literals' args
            -> reduce (Literal (IntegerLiteral w))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalAdd)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j (+))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalMul)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j (*))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Natural.naturalSubUnsafe)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange nTy [i, j] (\[i', j'] ->
              naturalToNaturalLiteral (naturalSubUnsafe i' j')))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalSubThrow)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange nTy [i, j] (\[i', j'] ->
                        case minusNaturalMaybe i' j' of
                          Nothing -> checkNaturalRange1 nTy (-1) id
                          Just n -> naturalToNaturalLiteral n))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalFromWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral w)] <- args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange1 nTy w id)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- naturalLiterals' args
            -> reduce (integerToWordLiteral i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalQuot)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j quot)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalRem)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j rem)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalQuotRem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Natural -> Natural -> (#Natural, Natural#)
            | [i, j] <- naturalLiterals' args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   (q,r) = quotRem (fromInteger i) (fromInteger j)
            in reduce $
                 mkApps (Data tupDc) (map Right tyArgs ++
                        [ Left $ catchDivByZero (naturalToNaturalLiteral q)
                        , Left $ catchDivByZero (naturalToNaturalLiteral r)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalGcd)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j gcd)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalLcm)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j lcm)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalGt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalGe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalEq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalNe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalLt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalLe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalShiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (WordLiteral j)] <- args
            , [i] <- naturalLiterals' [iV]
            -> reduce (naturalToNaturalLiteral (fromInteger (i `shiftL` fromInteger j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalShiftR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (WordLiteral j)] <- args
            , [i] <- naturalLiterals' [iV]
            -> reduce (naturalToNaturalLiteral (fromInteger (i `shiftR` fromInteger j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalCompare)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- naturalLiterals' args
            -> let -- Get the required result type (viewed as an applied type constructor name)
                   (_,tyView -> TyConApp tupTcNm []) = splitFunForallTy ty
                   -- Find the type constructor from the name
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   -- Get the data constructors of that type
                   -- The type is 'Ordering', so they are: 'LT', 'EQ', 'GT'
                   [ltDc, eqDc, gtDc] = tyConDataCons tupTc
                   -- Do the actual compile-time evaluation
                   ordVal = compareInteger i j
                in reduce $ case ordVal of
                    LT -> Data ltDc
                    EQ -> Data eqDc
                    GT -> Data gtDc
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalSignum)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- naturalLiterals' args
            -> reduce (Literal (NaturalLiteral (signum i)))
          _ -> Nothing
    )

  , ( "GHC.Num.Natural.$wnaturalSignum"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- naturalLiterals' args
            -> reduce (Literal (WordLiteral (signum i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.BigNat.bigNatEq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ Lit (ByteArrayLiteral (BA.ByteArray i))
              , Lit (ByteArrayLiteral (BA.ByteArray j))] <- args
            -> reduce (Literal (IntLiteral (IS (bigNatEq# i j))))
          _ -> Nothing
    )

  -- GHC.Real.^  -- XXX: Very fragile
  --   ^_f, $wf, $wf1 are specialisations of the internal function f in the implementation of (^) in GHC.Real
  , ( "GHC.Real.^_f"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  -- :: Integer -> Integer -> Integer
            | [i,j] <- integerLiterals' args
            -> reduce (catchErrorCall (integerToIntegerLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Real.$wf"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  -- :: Integer -> Int# -> Integer
            | [iV, Lit (IntLiteral j)] <- args
            , [i] <- integerLiterals' [iV]
            -> reduce (catchErrorCall (integerToIntegerLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Real.$wf1"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int# -> Int# -> Int#
            | [Lit (IntLiteral i), Lit (IntLiteral j)] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Internal.Real.^_$s$spowImpl2"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int# -> Integer -> Integer
            | [intLiteral -> Just j, integerLiteral -> Just i] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Internal.Real.^_$s$spowImpl"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int -> Integer -> Integer
            | [intLiteral -> Just j, integerLiteral -> Just i] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Internal.Real.$w$spowImpl"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Integer -> Int# -> Integer
            | [integerLiteral -> Just i, intLiteral -> Just j] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Internal.Real.$w$spowImpl1"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int# -> Int# -> Integer
            | [intLiteral -> Just i, intLiteral -> Just j] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Real.^_$s$spowImpl2"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int# -> Integer -> Integer
            | [intLiteral -> Just j, integerLiteral -> Just i] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Real.$w$spowImpl"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Integer -> Int# -> Integer
            | [integerLiteral -> Just i, intLiteral -> Just j] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Real.$w$spowImpl1"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int# -> Int# -> Integer
            | [intLiteral -> Just i, intLiteral -> Just j] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Real.^_$sf2"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int# -> Integer -> Integer
            | [intLiteral -> Just j, integerLiteral -> Just i] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )

  -- Type level ^    -- XXX: Very fragile
  -- These is are specialized versions of ^_f, named by some combination of ghc and singletons.
  , ( "Data.Singletons.TypeLits.Internal.$fSingI->^@#@$_f"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- ghc-8.6.5, singletons-2.5.1
            | [i,j] <- naturalLiterals' args
            -> reduce (Literal (NaturalLiteral (i ^ j)))
          _ -> Nothing
    )
  , ( "Data.Singletons.TypeLits.Internal.%^_f"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}             -- ghc-8.8.1, singletons-2.6
            | [i,j] <- naturalLiterals' args
            -> reduce (Literal (NaturalLiteral (i ^ j)))
          _ -> Nothing
    )

  -- XXX: Does it make sense to match on a @NaturalLiteral@ here?
  , ( $(textNameLit 'GHC.TypeLits.natVal)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (NaturalLiteral n), _] <- args
            -> reduce (integerToIntegerLiteral n)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.TypeNats.natVal)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (NaturalLiteral n), _] <- args
            -> reduce (Literal (NaturalLiteral n))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.TypeNats.someNatVal)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (NaturalLiteral n)] <- args
            -> let resTy = getResultTy tcm ty tys
                in reduce (mkSomeNat tcm n resTy)
          _ -> Nothing
    )

  -- XXX: Does it make sense to match on a @NaturalLiteral@ here?
  , ( $(textNameLit 'GHC.TypeLits.someNatVal)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (NaturalLiteral n)] <- args
            -> let resTy = getResultTy tcm ty tys
                in reduce (mkSomeNat tcm n resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Types.I#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (IntLiteral i)] <- args
            ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                    (Just intTc) = UniqMap.lookup intTcNm tcm
                    [intDc] = tyConDataCons intTc
                in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Int.I8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (Int8Literal i)] <- args
            ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                    (Just intTc) = UniqMap.lookup intTcNm tcm
                    [intDc] = tyConDataCons intTc
                in  reduce (mkApps (Data intDc) [Left (Literal (Int8Literal i))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Int.I16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (Int16Literal i)] <- args
            ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                    (Just intTc) = UniqMap.lookup intTcNm tcm
                    [intDc] = tyConDataCons intTc
                in  reduce (mkApps (Data intDc) [Left (Literal (Int16Literal i))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Int.I32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (Int32Literal i)] <- args
            ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                    (Just intTc) = UniqMap.lookup intTcNm tcm
                    [intDc] = tyConDataCons intTc
                in  reduce (mkApps (Data intDc) [Left (Literal (Int32Literal i))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Int.I64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (Int64Literal i)] <- args
            ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                    (Just intTc) = UniqMap.lookup intTcNm tcm
                    [intDc] = tyConDataCons intTc
                in  reduce (mkApps (Data intDc) [Left (Literal (Int64Literal i))])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Word.W8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (Word8Literal c)] <- args
            ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                    (Just wordTc) = UniqMap.lookup wordTcNm tcm
                    [wordDc] = tyConDataCons wordTc
                in  reduce (mkApps (Data wordDc) [Left (Literal (Word8Literal c))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Word.W16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (Word16Literal c)] <- args
            ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                    (Just wordTc) = UniqMap.lookup wordTcNm tcm
                    [wordDc] = tyConDataCons wordTc
                in  reduce (mkApps (Data wordDc) [Left (Literal (Word16Literal c))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Word.W32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (Word32Literal c)] <- args
            ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                    (Just wordTc) = UniqMap.lookup wordTcNm tcm
                    [wordDc] = tyConDataCons wordTc
                in  reduce (mkApps (Data wordDc) [Left (Literal (Word32Literal c))])
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Word.W64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (Word64Literal c)] <- args
            ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                    (Just wordTc) = UniqMap.lookup wordTcNm tcm
                    [wordDc] = tyConDataCons wordTc
                in  reduce (mkApps (Data wordDc) [Left (Literal (Word64Literal c))])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Types.W#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (WordLiteral i)] <- args
            ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                    (Just intTc) = UniqMap.lookup intTcNm tcm
                    [intDc] = tyConDataCons intTc
                in  reduce (mkApps (Data intDc) [Left (Literal (WordLiteral i))])
          _ -> Nothing
    )

  , ( "GHC.Float.$w$sfromRat''"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- XXX: Very fragile
            | [Lit (IntLiteral _minEx)
              ,Lit (IntLiteral matDigs)
              ,nV
              ,dV] <- args
            , [n,d] <- integerLiterals' [nV,dV]
            -> case fromInteger matDigs of
                  matDigs'
                    | matDigs' == floatDigits (undefined :: Float)
                    -> reduce (Literal (FloatLiteral (castFloatToWord32 (fromRational (n :% d)))))
                    | matDigs' == floatDigits (undefined :: Double)
                    -> reduce (Literal (DoubleLiteral (castDoubleToWord64 (fromRational (n :% d)))))
                  _ -> error $ $(curLoc) ++ "GHC.Float.$w$sfromRat'': Not a Float or Double"
          _ -> Nothing
    )

  , ( "GHC.Float.$w$sfromRat''1"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- XXX: Very fragile
            | [Lit (IntLiteral _minEx)
              ,Lit (IntLiteral matDigs)
              ,nV
              ,dV] <- args
            , [n,d] <- integerLiterals' [nV,dV]
            -> case fromInteger matDigs of
                  matDigs'
                    | matDigs' == floatDigits (undefined :: Float)
                    -> reduce (Literal (FloatLiteral (castFloatToWord32 (fromRational (n :% d)))))
                    | matDigs' == floatDigits (undefined :: Double)
                    -> reduce (Literal (DoubleLiteral (castDoubleToWord64 (fromRational (n :% d)))))
                  _ -> error $ $(curLoc) ++ "GHC.Float.$w$sfromRat'': Not a Float or Double"
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerSignum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (Literal (IntLiteral (signum i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerSignum)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (signumInteger i)))
          _ -> Nothing
    )

  , ( "GHC.Num.Integer.$wintegerSignum"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (Literal (IntLiteral (signum i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerAbs)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (absInteger i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerBit#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- wordLiterals' args
            -> reduce (Literal (IntegerLiteral (bit (fromInteger i))))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerComplement)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (complementInteger i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerOr)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (orInteger i j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerXor)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (xorInteger i j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerAnd)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (andInteger i j)))
          _ -> Nothing
    )

  , ( "GHC.Num.Integer.$wintegerFromInt64#"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- int64Literals' args
            -> reduce . Literal $ IntLiteral i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Base.eqString)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal _ _ [Lit (StringLiteral s1)]
              ,PrimVal _ _ [Lit (StringLiteral s2)]
              ] <- args
            -> reduce (boolToBoolLiteral tcm ty (s1 == s2))
            | otherwise -> error (show args)
    )

  , ( $(textNameLit 'GHC.Base.quotInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ DC intDc [Left (Literal (IntLiteral i))]
              , DC _     [Left (Literal (IntLiteral j))]
              ] <- args
            -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `quot` j)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int8 -> BitVector 8
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Int8Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (IntLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int16 -> BitVector 16
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Int16Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (IntLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int32 -> BitVector 32
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Int32Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (IntLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int64 -> BitVector 64
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Int64Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (IntLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Word -> BitVector WORD_SIZE_IN_BITS
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Word8 -> BitVector 8
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Word8Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Word16 -> BitVector 16
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Word16Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Word32 -> BitVector 32
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Word32Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Word64 -> BitVector 64
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Word64Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Base.remInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ DC intDc [Left (Literal (IntLiteral i))]
              , DC _     [Left (Literal (IntLiteral j))]
              ] <- args
            -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `rem` j)))))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Base.divInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ DC intDc [Left (Literal (IntLiteral i))]
              , DC _     [Left (Literal (IntLiteral j))]
              ] <- args
            -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `div` j)))))
          _ -> Nothing
    )


  , ( $(textNameLit 'GHC.Base.modInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ DC intDc [Left (Literal (IntLiteral i))]
              , DC _     [Left (Literal (IntLiteral j))]
              ] <- args
            -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `mod` j)))))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Double -> BitVector 64
            | [DC _ [Left arg]] <- args
            , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
            , mach2@Machine{mStack=[],mTerm=Literal (DoubleLiteral i)} <- whnf eval tcm True (setTerm arg $ stackClear mach)
            -> let resTyInfo = extractTySizeInfo tcm ty tys
                in Just $ mach2
                     { mStack = mStack mach
                     , mTerm = mkBitVectorLit' resTyInfo 0 (toInteger $ (pack :: Word64 -> BitVector 64) i)
                     }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Float -> BitVector 32
            | [DC _ [Left arg]] <- args
            , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
            , mach2@Machine{mStack=[],mTerm=Literal (FloatLiteral i)} <- whnf eval tcm True (setTerm arg $ stackClear mach)
            -> let resTyInfo = extractTySizeInfo tcm ty tys
                in Just $ mach2
                     { mStack = mStack mach
                     , mTerm = mkBitVectorLit' resTyInfo 0 (toInteger $ (pack :: Word32 -> BitVector 32) i)
                     }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packCUShort#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: CUShort -> BitVector 16
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Word16Literal i)}
                  <- whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)}
                  <- whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 8 -> Int8
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Signed 8)
#if MIN_VERSION_base(4,16,0)
                   proj = Int8Literal
#else
                   proj = IntLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 16 -> Int16
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Signed 16)
#if MIN_VERSION_base(4,16,0)
                   proj = Int16Literal
#else
                   proj = IntLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 32 -> Int32
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Signed 32)
#if MIN_VERSION_base(4,16,0)
                   proj = Int32Literal
#else
                   proj = IntLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 64 -> Int64
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Signed 64)
#if MIN_VERSION_base(4,16,0)
                   proj = Int64Literal
#else
                   proj = IntLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector WORD_SIZE_IN_BITS -> Word
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 64)
                in reduce (mkIntCLit tcm WordLiteral val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 8 -> Word8
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 8)
#if MIN_VERSION_base(4,16,0)
                   proj = Word8Literal
#else
                   proj = WordLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 16 -> Word16
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 16)
#if MIN_VERSION_base(4,16,0)
                   proj = Word16Literal
#else
                   proj = WordLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 32 -> Word32
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 32)
#if MIN_VERSION_base(4,16,0)
                   proj = Word32Literal
#else
                   proj = WordLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 64 -> Word64
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 64)
#if MIN_VERSION_base(4,16,0)
                   proj = Word64Literal
#else
                   proj = WordLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = unpack (toBV i :: BitVector 32)
                in reduce (mkFloatCLit tcm val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = unpack (toBV i :: BitVector 64)
                in reduce (mkDoubleCLit tcm val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackCUShort#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 16)
#if MIN_VERSION_base(4,16,0)
                   proj = Word16Literal
#else
                   proj = WordLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.xToBV)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -- The second argument to `xToBV` is always going to be suspended.
            -- See Note [Lazy primitives]
            , [ _, (Suspend arg) ] <- args
            , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
            , mach1@Machine{mStack=[],mTerm=argWHNF} <-
                whnf eval tcm True (setTerm arg (stackClear mach))
            , let undefBitVector =
                    Just $ mach1
                         { mStack = mStack mach
                         , mTerm  = mkBitVectorLit ty nTy kn (bit (fromInteger kn)-1) 0
                         }
            -> case isX argWHNF of
                 Left _ -> undefBitVector
                 _ -> case collectArgs argWHNF of
                   (Prim p,_) | primName p `elem` undefinedXPrims -> undefBitVector
                   _ -> Just $ mach1
                             { mStack = mStack mach
                             , mTerm  = argWHNF
                             }
          _ -> Nothing
    )

  -- expIndex#
  --   :: KnownNat m
  --   => Index m
  --   -> SNat n
  --   -> Index (n^m)
  , ( $(textNameLit 'Clash.Class.Exp.expIndex#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [b] <- indexLiterals' args
            , [(_mTy, km), (_, e)] <- extractKnownNats tcm tys
            -> reduce (mkIndexLit ty (LitTy (NumTy (km^e))) (km^e) (b^e))
          _ -> Nothing
    )

  -- expSigned#
  --   :: KnownNat m
  --   => Signed m
  --   -> SNat n
  --   -> Signed (n*m)
  , ( $(textNameLit 'Clash.Class.Exp.expSigned#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [b] <- signedLiterals' args
            , [(_mTy, km), (_, e)] <- extractKnownNats tcm tys
            -> reduce (mkSignedLit ty (LitTy (NumTy (km*e))) (km*e) (b^e))
          _ -> Nothing
    )

  -- expUnsigned#
  --   :: KnownNat m
  --   => Unsigned m
  --   -> SNat n
  --   -> Unsigned m
  , ( $(textNameLit 'Clash.Class.Exp.expUnsigned#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [b] <- unsignedLiterals' args
            , [(_mTy, km), (_, e)] <- extractKnownNats tcm tys
            -> reduce (mkUnsignedLit ty (LitTy (NumTy (km*e))) (km*e) (b^e))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Promoted.Nat.powSNat)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
            -> let c = case a of
                         2 -> 1 `shiftL` (fromInteger b)
                         _ -> a ^ b
                   (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
                   (Just snatTc) = UniqMap.lookup snatTcNm tcm
                   [snatDc] = tyConDataCons snatTc
               in  reduce $
                   mkApps (Data snatDc) [ Right (LitTy (NumTy c))
                                        , Left (Literal (NaturalLiteral c))]
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Promoted.Nat.flogBaseSNat)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
            , Just c <- flogBase a b
            , let c' = toInteger c
            -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
                   (Just snatTc) = UniqMap.lookup snatTcNm tcm
                   [snatDc] = tyConDataCons snatTc
               in  reduce $
                   mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                        , Left (Literal (NaturalLiteral c'))]
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Promoted.Nat.clogBaseSNat)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
            , Just c <- clogBase a b
            , let c' = toInteger c
            -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
                   (Just snatTc) = UniqMap.lookup snatTcNm tcm
                   [snatDc] = tyConDataCons snatTc
               in  reduce $
                   mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                        , Left (Literal (NaturalLiteral c'))]
            | otherwise
            -> error ("clogBaseSNat: args = " <> show args <> ", tys = " <> show tys)
    )

  , ( $(textNameLit 'Clash.Promoted.Nat.logBaseSNat)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
            , Just c <- flogBase a b
            , let c' = toInteger c
            -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
                   (Just snatTc) = UniqMap.lookup snatTcNm tcm
                   [snatDc] = tyConDataCons snatTc
               in  reduce $
                   mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                        , Left (Literal (NaturalLiteral c'))]
          _ -> Nothing
    )

------------
-- BitVector
------------
-- Constructor
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.BV)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Right _] <- map (runExcept . tyNatSize tcm) tys
            , Just (m,i) <- integerLiterals args
            -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkBitVectorLit' resTyInfo m i)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.Bit)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (m,i) <- integerLiterals args
            -> reduce (mkBitLit ty m i)
          _ -> Nothing
    )

-- Initialization
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.size#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            -> let (_,tyView -> TyConApp intTcNm _) = splitFunForallTy ty
                   (Just intTc) = UniqMap.lookup intTcNm tcm
                   [intCon] = tyConDataCons intTc
               in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.maxIndex#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            -> let (_,tyView -> TyConApp intTcNm _) = splitFunForallTy ty
                   (Just intTc) = UniqMap.lookup intTcNm tcm
                   [intCon] = tyConDataCons intTc
               in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (kn-1)))])
          _ -> Nothing
    )

-- Construction
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.high)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            -> reduce (mkBitLit ty 0 1)
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.low)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            -> reduce (mkBitLit ty 0 0)
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.undefined##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            -> reduce (mkBitLit ty 1 0)
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.undefined#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            -> let resTyInfo = extractTySizeInfo tcm ty tys
                   mask = bit (fromInteger kn) - 1
               in reduce (mkBitVectorLit' resTyInfo mask 0)
          _ -> Nothing
    )

-- Eq
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.eq##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.neq##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

-- Ord
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.lt##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <  j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.ge##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.gt##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >  j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.le##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

-- Enum
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.toEnum##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- intCLiterals' args
            -> let Bit msk val = BitVector.toEnum## (fromInteger i)
               in reduce (mkBitLit ty (toInteger msk) (toInteger val))
          _ -> Nothing
    )

-- Bits
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.and##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- bitLiterals args
            -> let Bit msk val = BitVector.and## (toBit i) (toBit j)
               in reduce (mkBitLit ty (toInteger msk) (toInteger val))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.or##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- bitLiterals args
            -> let Bit msk val = BitVector.or## (toBit i) (toBit j)
               in reduce (mkBitLit ty (toInteger msk) (toInteger val))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.xor##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- bitLiterals args
            -> let Bit msk val = BitVector.xor## (toBit i) (toBit j)
               in reduce (mkBitLit ty (toInteger msk) (toInteger val))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.complement##)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- bitLiterals args
            -> let Bit msk val = BitVector.complement## (toBit i)
               in reduce (mkBitLit ty (toInteger msk) (toInteger val))
          _ -> Nothing
    )

-- Pack
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.pack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [(msk,i)] <- bitLiterals args
            -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkBitVectorLit' resTyInfo msk i)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.unpack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [(msk,i)] <- bitVectorLiterals' args
            -> reduce (mkBitLit ty msk i)
          _ -> Nothing
    )

-- Concatenation
  , ( $(textNameLit '(Clash.Sized.Internal.BitVector.++#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat m => BitVector n -> BitVector m -> BitVector (n + m)
            | Just (_,m) <- extractKnownNat tcm tys
            , [(mski,i),(mskj,j)] <- bitVectorLiterals' args
            -> let val = i `shiftL` fromInteger m .|. j
                   msk = mski `shiftL` fromInteger m .|. mskj
                   resTyInfo = extractTySizeInfo tcm ty tys
               in reduce (mkBitVectorLit' resTyInfo msk val)
          _ -> Nothing
    )

-- Reduction
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.reduceAnd#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => BitVector n -> Bit
            | [i] <- bitVectorLiterals' args
            , Just (_, kn) <- extractKnownNat tcm tys
            -> let resTy = getResultTy tcm ty tys
                   val = reifyNat kn (op (toBV i))
               in reduce (mkBitLit resTy 0 val)
            where
              op :: KnownNat n => BitVector n -> Proxy n -> Integer
              op u _ = toInteger (BitVector.reduceAnd# u)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.reduceOr#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => BitVector n -> Bit
            | [i] <- bitVectorLiterals' args
            , Just (_, kn) <- extractKnownNat tcm tys
            -> let resTy = getResultTy tcm ty tys
                   val = reifyNat kn (op (toBV i))
               in reduce (mkBitLit resTy 0 val)
            where
              op :: KnownNat n => BitVector n -> Proxy n -> Integer
              op u _ = toInteger (BitVector.reduceOr# u)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.reduceXor#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => BitVector n -> Bit
            | [i] <- bitVectorLiterals' args
            , Just (_, kn) <- extractKnownNat tcm tys
            -> let resTy = getResultTy tcm ty tys
                   val = reifyNat kn (op (toBV i))
               in reduce (mkBitLit resTy 0 val)
            where
              op :: KnownNat n => BitVector n -> Proxy n -> Integer
              op u _ = toInteger (BitVector.reduceXor# u)
          _ -> Nothing
    )


-- Indexing
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.index#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => BitVector n -> Int -> Bit
            | Just (_,kn,i,j) <- bitVectorLitIntLit tcm tys args
              -> let resTy = getResultTy tcm ty tys
                     (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
                 in reduce (mkBitLit resTy msk val)
              where
                op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
                op u i _ = (toInteger m, toInteger v)
                  where Bit m v = (BitVector.index# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.replaceBit#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: :: KnownNat n => BitVector n -> Int -> Bit -> BitVector n
            | Just (_, n) <- extractKnownNat tcm tys
            , [ _
              , PrimVal bvP _ [_, Lit (NaturalLiteral mskBv), Lit (IntegerLiteral bv)]
              , valArgs -> Just [Literal (IntLiteral i)]
              , PrimVal bP _ [Lit (WordLiteral mskB), Lit (IntegerLiteral b)]
              ] <- args
            , primName bvP == showt 'Clash.Sized.Internal.BitVector.fromInteger#
            , primName bP  == showt 'Clash.Sized.Internal.BitVector.fromInteger##
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                     (mskVal,val) = reifyNat n (op (BV (fromInteger mskBv) (fromInteger bv))
                                                   (fromInteger i)
                                                   (Bit (fromInteger mskB) (fromInteger b)))
              in reduce (mkBitVectorLit' resTyInfo mskVal val)
              where
                op :: KnownNat n => BitVector n -> Int -> Bit -> Proxy n -> (Integer,Integer)
                -- op bv i b _ = (BitVector.unsafeMask res, BitVector.unsafeToInteger res)
                op bv i b _ = splitBV (BitVector.replaceBit# bv i b)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.setSlice#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
          -- :: SNat (m+1+i) -> BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n) -> BitVector (m + 1 + i)
            | mTy : iTy : nTy : _ <- tys
            , Right m <- runExcept (tyNatSize tcm mTy)
            , Right iN <- runExcept (tyNatSize tcm iTy)
            , Right n <- runExcept (tyNatSize tcm nTy)
            , [i,j] <- bitVectorLiterals' args
            -> let BV msk val = BitVector.setSlice# (unsafeSNat (m+1+iN)) (toBV i) (unsafeSNat m) (unsafeSNat n) (toBV j)
                   resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkBitVectorLit' resTyInfo (toInteger msk) (toInteger val))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.slice#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
          -- :: BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n)
            | mTy : _ : nTy : _ <- tys
            , Right m <- runExcept (tyNatSize tcm mTy)
            , Right n <- runExcept (tyNatSize tcm nTy)
            , [i] <- bitVectorLiterals' args
            -> let BV msk val = BitVector.slice# (toBV i) (unsafeSNat m) (unsafeSNat n)
                   resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkBitVectorLit' resTyInfo (toInteger msk) (toInteger val))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.split#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n m. KnownNat n => BitVector (m + n) -> (BitVector m, BitVector n)
            | nTy : mTy : _ <- tys
            , Right n <-  runExcept (tyNatSize tcm nTy)
            , Right m <-  runExcept (tyNatSize tcm mTy)
            , [(mski,i)] <- bitVectorLiterals' args
            -> let ty' = piResultTys tcm ty tys
                   (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty'
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   bvTy : _ = tyArgs
                   valM = i `shiftR` fromInteger n
                   mskM = mski `shiftR` fromInteger n
                   valN = i .&. mask
                   mskN = mski .&. mask
                   mask = bit (fromInteger n) - 1
            in reduce $
               mkApps (Data tupDc) (map Right tyArgs ++
                        [ Left (mkBitVectorLit bvTy mTy m mskM valM)
                        , Left (mkBitVectorLit bvTy nTy n mskN valN)])
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.msb#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n. KnownNat n => BitVector n -> Bit
            | [i] <- bitVectorLiterals' args
            , Just (_, kn) <- extractKnownNat tcm tys
            -> let resTy = getResultTy tcm ty tys
                   (msk,val) = reifyNat kn (op (toBV i))
               in reduce (mkBitLit resTy (toInteger msk) (toInteger val))
            where
              op :: KnownNat n => BitVector n -> Proxy n -> (Word,Word)
              op u _ = (unsafeMask# res, BitVector.unsafeToInteger# res)
                where
                  res = BitVector.msb# u
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.lsb#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: BitVector n -> Bit
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   Bit msk val = BitVector.lsb# (toBV i)
            in reduce (mkBitLit resTy (toInteger msk) (toInteger val))
          _ -> Nothing
    )


-- Eq
  -- eq#, neq# :: KnownNat n => BitVector n -> BitVector n -> Bool
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.eq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | nTy : _ <- tys
            , Right 0 <- runExcept (tyNatSize tcm nTy)
            -> reduce (boolToBoolLiteral tcm ty True)
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2Bool BitVector.eq# ty tcm args)
            -> reduce val
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.neq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | nTy : _ <- tys
            , Right 0 <- runExcept (tyNatSize tcm nTy)
            -> reduce (boolToBoolLiteral tcm ty False)
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2Bool BitVector.neq# ty tcm args)
            -> reduce val
          _ -> Nothing
    )

-- Ord
  -- lt#,ge#,gt#,le# :: KnownNat n => BitVector n -> BitVector n -> Bool
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.lt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | nTy : _ <- tys
            , Right 0 <- runExcept (tyNatSize tcm nTy)
            -> reduce (boolToBoolLiteral tcm ty False)
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2Bool BitVector.lt# ty tcm args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.ge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | nTy : _ <- tys
            , Right 0 <- runExcept (tyNatSize tcm nTy)
            -> reduce (boolToBoolLiteral tcm ty True)
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2Bool BitVector.ge# ty tcm args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.gt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | nTy : _ <- tys
            , Right 0 <- runExcept (tyNatSize tcm nTy)
            -> reduce (boolToBoolLiteral tcm ty False)
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2Bool BitVector.gt# ty tcm args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.le#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | nTy : _ <- tys
            , Right 0 <- runExcept (tyNatSize tcm nTy)
            -> reduce (boolToBoolLiteral tcm ty True)
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2Bool BitVector.le# ty tcm args)
            -> reduce val
          _ -> Nothing
    )

-- Enum

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.toEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | let resTyInfo@(_,_,kn) = extractTySizeInfo tcm ty tys
            , Just val <- reifyNat kn (liftInteger2BitVector (BitVector.toEnum# . fromInteger) resTyInfo args)
            -> reduce val
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.fromEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , let resTy = getResultTy tcm ty tys
            , Just val <- reifyNat kn (liftBitVector2CInt tcm resTy (toInteger . BitVector.fromEnum#) args)
            -> reduce val
          _ -> Nothing
    )

-- Bounded
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.minBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,len) <- extractKnownNat tcm tys
            -> reduce (mkBitVectorLit ty nTy len 0 0)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.maxBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (litTy,mb) <- extractKnownNat tcm tys
            -> let maxB = (2 ^ mb) - 1
               in  reduce (mkBitVectorLit ty litTy mb 0 maxB)
          _ -> Nothing
    )

-- Num
  , ( $(textNameLit '(Clash.Sized.Internal.BitVector.+#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2 (BitVector.+#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.BitVector.-#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2 (BitVector.-#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.BitVector.*#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2 (BitVector.*#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.negate#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- bitVectorLiterals' args
            -> let (msk,val) = reifyNat kn (op (toBV i))
            in reduce (mkBitVectorLit ty nTy kn msk val)
            where
              op :: KnownNat n => BitVector n -> Proxy n -> (Integer,Integer)
              op u _ = splitBV (BitVector.negate# u)
          _ -> Nothing
    )

-- ExtendingNum
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.plus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: (KnownNat n, KnownNat m) => BitVector m -> BitVector n -> BitVector (Max m n + 1)
            | [(0,i),(0,j)] <- bitVectorLiterals' args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 (i+j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.minus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [(0,i),(0,j)] <- bitVectorLiterals' args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
                   val = reifyNat resSizeInt (runSizedF (BitVector.-#) i j)
              in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 val)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.times#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [(0,i),(0,j)] <- bitVectorLiterals' args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 (i*j))
          _ -> Nothing
    )

-- Integral
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.quot#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2 (BitVector.quot#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.rem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2 (BitVector.rem#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.toInteger#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , [i] <- bitVectorLiterals' args
            -> let val = reifyNat kn (op (toBV i))
            in reduce (integerToIntegerLiteral val)
            where
              op :: KnownNat n => BitVector n -> Proxy n -> Integer
              op u _ = BitVector.toInteger# u
          _ -> Nothing
    )

-- Bits
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.and#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2 (BitVector.and#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.or#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2 (BitVector.or#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.xor#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftBitVector2 (BitVector.xor#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.complement#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- bitVectorLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> let (msk,val) = reifyNat kn (op (toBV i))
            in reduce (mkBitVectorLit ty nTy kn msk val)
            where
              op :: KnownNat n => BitVector n -> Proxy n -> (Integer,Integer)
              op u _ = splitBV $ BitVector.complement# u
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.shiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- bitVectorLitIntLit tcm tys args
              -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
              in reduce (mkBitVectorLit ty nTy kn msk val)
              where
                op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
                op u i _ = splitBV (BitVector.shiftL# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.shiftR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- bitVectorLitIntLit tcm tys args
              -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
              in reduce (mkBitVectorLit ty nTy kn msk val)
              where
                op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
                op u i _ = splitBV (BitVector.shiftR# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.rotateL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- bitVectorLitIntLit tcm tys args
              -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
              in reduce (mkBitVectorLit ty nTy kn msk val)
              where
                op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
                op u i _ = splitBV (BitVector.rotateL# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.rotateR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- bitVectorLitIntLit tcm tys args
              -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
              in reduce (mkBitVectorLit ty nTy kn msk val)
              where
                op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
                op u i _ = splitBV (BitVector.rotateR# u i)
          _ -> Nothing
    )

-- truncateB
  , ( $(textNameLit 'Clash.Sized.Internal.BitVector.truncateB#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- forall a b . KnownNat a => BitVector (a + b) -> BitVector a
            | aTy  : _ <- tys
            , Right ka <- runExcept (tyNatSize tcm aTy)
            , [(mski,i)] <- bitVectorLiterals' args
            -> let bitsKeep = (bit (fromInteger ka)) - 1
                   val = i .&. bitsKeep
                   msk = mski .&. bitsKeep
            in reduce (mkBitVectorLit ty aTy ka msk val)
          _ -> Nothing
    )

--------
-- Index
--------
-- BitPack
  , ( $(textNameLit 'Clash.Sized.Internal.Index.pack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | nTy : _ <- tys
            , Right _ <- runExcept (tyNatSize tcm nTy)
            , [i] <- indexLiterals' args
            -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkBitVectorLit' resTyInfo 0 i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.unpack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , [(0,i)] <- bitVectorLiterals' args
            -> reduce (mkIndexLit ty nTy kn i)
          _ -> Nothing
    )

-- Eq
  , ( $(textNameLit 'Clash.Sized.Internal.Index.eq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.neq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

-- Ord
  , ( $(textNameLit 'Clash.Sized.Internal.Index.lt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i < j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.ge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.gt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i > j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.le#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

-- Enum
  , ( $(textNameLit 'Clash.Sized.Internal.Index.toEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- intCLiterals' args
            , Just (nTy, mb) <- extractKnownNat tcm tys
            -> reduce (mkIndexLit ty nTy mb i)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Index.fromEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- indexLiterals' args
            -> let resTy = getResultTy tcm ty tys
                in reduce (mkIntCLit tcm IntLiteral i resTy)
          _ -> Nothing
    )

-- Bounded
  , ( $(textNameLit 'Clash.Sized.Internal.Index.maxBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,mb) <- extractKnownNat tcm tys
            -> reduce (mkIndexLit ty nTy mb (mb - 1))
          _ -> Nothing
    )

-- Num
  , ( $(textNameLit '(Clash.Sized.Internal.Index.+#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , [i,j] <- indexLiterals' args
            -> reduce (mkIndexLit ty nTy kn (i + j))
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Index.-#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , [i,j] <- indexLiterals' args
            -> reduce (mkIndexLit ty nTy kn (i - j))
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Index.*#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , [i,j] <- indexLiterals' args
            -> reduce (mkIndexLit ty nTy kn (i * j))
          _ -> Nothing
    )

-- ExtendingNum
  , ( $(textNameLit 'Clash.Sized.Internal.Index.plus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | mTy : nTy : _ <- tys
            , Right _ <- runExcept (tyNatSize tcm mTy)
            , Right _ <- runExcept (tyNatSize tcm nTy)
            , Just (i,j) <- indexLiterals args
            -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkIndexLit' resTyInfo (i + j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.minus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | mTy : nTy : _ <- tys
            , Right _ <- runExcept (tyNatSize tcm mTy)
            , Right _ <- runExcept (tyNatSize tcm nTy)
            , Just (i,j) <- indexLiterals args
            -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkIndexLit' resTyInfo (i - j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.times#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | mTy : nTy : _ <- tys
            , Right _ <- runExcept (tyNatSize tcm mTy)
            , Right _ <- runExcept (tyNatSize tcm nTy)
            , Just (i,j) <- indexLiterals args
            -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkIndexLit' resTyInfo (i * j))
          _ -> Nothing
    )

-- Integral
  , ( $(textNameLit 'Clash.Sized.Internal.Index.quot#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , Just (i,j) <- indexLiterals args
            -> reduce $ catchDivByZero (mkIndexLit ty nTy kn (i `quot` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.rem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , Just (i,j) <- indexLiterals args
            -> reduce $ catchDivByZero (mkIndexLit ty nTy kn (i `rem` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.toInteger#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
            , primName p == showt 'Clash.Sized.Internal.Index.fromInteger#
            -> reduce (integerToIntegerLiteral i)
          _ -> Nothing
    )

-- Resize
  , ( $(textNameLit 'Clash.Sized.Internal.Index.resize#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (mTy,m) <- extractKnownNat tcm tys
            , [i] <- indexLiterals' args
            -> reduce (mkIndexLit ty mTy m i)
          _ -> Nothing
    )

---------
-- Signed
---------
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.size#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            -> let (_,tyView -> TyConApp intTcNm _) = splitFunForallTy ty
                   (Just intTc) = UniqMap.lookup intTcNm tcm
                   [intCon] = tyConDataCons intTc
               in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])
          _ -> Nothing
    )

-- BitPack
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.pack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- signedLiterals' args
            -> let val = reifyNat kn (op (fromInteger i))
               in reduce (mkBitVectorLit ty nTy kn 0 val)
            where
                op :: KnownNat n => Signed n -> Proxy n -> Integer
                op s _ = toInteger (Signed.pack# s)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.unpack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [(0,i)] <- bitVectorLiterals' args
            -> let val = reifyNat kn (op (fromInteger i))
               in reduce (mkSignedLit ty nTy kn val)
            where
                op :: KnownNat n => BitVector n -> Proxy n -> Integer
                op s _ = toInteger (Signed.unpack# s)
          _ -> Nothing
    )

-- Eq
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.eq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.neq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

-- Ord
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.lt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <  j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.ge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.gt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >  j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.le#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

-- Enum
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.toEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- intCLiterals' args
            , Just (litTy, mb) <- extractKnownNat tcm tys
            -> reduce (mkSignedLit ty litTy mb i)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Signed.fromEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- signedLiterals' args
            -> let resTy = getResultTy tcm ty tys
                in reduce (mkIntCLit tcm IntLiteral i resTy)
          _ -> Nothing
    )

-- Bounded
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.minBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (litTy,mb) <- extractKnownNat tcm tys
            -> let minB = negate (2 ^ (mb - 1))
               in  reduce (mkSignedLit ty litTy mb minB)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.maxBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (litTy,mb) <- extractKnownNat tcm tys
            -> let maxB = (2 ^ (mb - 1)) - 1
               in reduce (mkSignedLit ty litTy mb maxB)
          _ -> Nothing
    )

-- Num
  , ( $(textNameLit '(Clash.Sized.Internal.Signed.+#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.+#) ty tcm tys args)
            -> reduce (val)
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Signed.-#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.-#) ty tcm tys args)
            -> reduce (val)
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Signed.*#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.*#) ty tcm tys args)
            -> reduce (val)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.negate#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- signedLiterals' args
            -> let val = reifyNat kn (op (fromInteger i))
            in reduce (mkSignedLit ty nTy kn val)
            where
              op :: KnownNat n => Signed n -> Proxy n -> Integer
              op s _ = toInteger (Signed.negate# s)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.abs#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- signedLiterals' args
            -> let val = reifyNat kn (op (fromInteger i))
            in reduce (mkSignedLit ty nTy kn val)
            where
              op :: KnownNat n => Signed n -> Proxy n -> Integer
              op s _ = toInteger (Signed.abs# s)
          _ -> Nothing
    )

-- ExtendingNum
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.plus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- signedLiterals args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i+j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Signed.minus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- signedLiterals args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i-j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Signed.times#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- signedLiterals args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i*j))
          _ -> Nothing
    )

-- Integral
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.quot#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.quot#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.rem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.rem#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.div#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.div#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.mod#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.mod#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.toInteger#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
            , primName p == showt 'Clash.Sized.Internal.Signed.fromInteger#
            -> reduce (integerToIntegerLiteral i)
          _ -> Nothing
    )

-- Bits
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.and#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- signedLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkSignedLit ty nTy kn (i .&. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.or#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- signedLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkSignedLit ty nTy kn (i .|. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.xor#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- signedLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkSignedLit ty nTy kn (i `xor` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Signed.complement#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- signedLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> let val = reifyNat kn (op (fromInteger i))
            in reduce (mkSignedLit ty nTy kn val)
            where
              op :: KnownNat n => Signed n -> Proxy n -> Integer
              op u _ = toInteger (Signed.complement# u)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Signed.shiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkSignedLit ty nTy kn val)
              where
                op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Signed.shiftL# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.shiftR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkSignedLit ty nTy kn val)
              where
                op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Signed.shiftR# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.rotateL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkSignedLit ty nTy kn val)
              where
                op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Signed.rotateL# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.rotateR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkSignedLit ty nTy kn val)
              where
                op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Signed.rotateR# u i)
          _ -> Nothing
    )

-- Resize
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.resize#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- forall m n. (KnownNat n, KnownNat m) => Signed n -> Signed m
            | mTy : nTy : _ <- tys
            , Right mInt <- runExcept (tyNatSize tcm mTy)
            , Right nInt <- runExcept (tyNatSize tcm nTy)
            , [i] <- signedLiterals' args
            -> let val | nInt <= mInt = extended
                       | otherwise    = truncated
                   extended  = i
                   mask      = 1 `shiftL` fromInteger (mInt - 1)
                   i'        = i `mod` mask
                   truncated = if testBit i (fromInteger nInt - 1)
                                  then (i' - mask)
                                  else i'
               in reduce (mkSignedLit ty mTy mInt val)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.truncateB#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- KnownNat m => Signed (m + n) -> Signed m
            | Just (mTy, km) <- extractKnownNat tcm tys
            , [i] <- signedLiterals' args
            -> let bitsKeep = (bit (fromInteger km)) - 1
                   val = i .&. bitsKeep
            in reduce (mkSignedLit ty mTy km val)
          _ -> Nothing
    )

-- SaturatingNum
-- No need to manually evaluate Clash.Sized.Internal.Signed.minBoundSym#
-- It is just implemented in terms of other primitives.


-----------
-- Unsigned
-----------
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.size#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            -> let (_,ty') = splitFunForallTy ty
                   (TyConApp intTcNm _) = tyView ty'
                   (Just intTc) = UniqMap.lookup intTcNm tcm
                   [intCon] = tyConDataCons intTc
               in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])
          _ -> Nothing
    )

-- BitPack
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.pack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- unsignedLiterals' args
            -> reduce (mkBitVectorLit ty nTy kn 0 i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.unpack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- bitVectorLiterals' args
            -> let val = reifyNat kn (op (toBV i))
            in reduce (mkUnsignedLit ty nTy kn val)
            where
              op :: KnownNat n => BitVector n -> Proxy n -> Integer
              op u _ = toInteger (Unsigned.unpack# u)
          _ -> Nothing
    )

-- Eq
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.eq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.neq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

-- Ord
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.lt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <  j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.ge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.gt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >  j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.le#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

-- Enum
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.toEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- intCLiterals' args
            , Just (litTy, mb) <- extractKnownNat tcm tys
            -> reduce (mkUnsignedLit ty litTy mb i)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.fromEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- unsignedLiterals' args
            -> let resTy = getResultTy tcm ty tys
                in reduce (mkIntCLit tcm IntLiteral i resTy)
          _ -> Nothing
    )

-- Bounded
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.minBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,len) <- extractKnownNat tcm tys
            -> reduce (mkUnsignedLit ty nTy len 0)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.maxBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (litTy,mb) <- extractKnownNat tcm tys
            -> let maxB = (2 ^ mb) - 1
               in  reduce (mkUnsignedLit ty litTy mb maxB)
          _ -> Nothing
    )

-- Num
  , ( $(textNameLit '(Clash.Sized.Internal.Unsigned.+#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.+#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Unsigned.-#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.-#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Unsigned.*#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.*#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.negate#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- unsignedLiterals' args
            -> let val = reifyNat kn (op (fromInteger i))
            in reduce (mkUnsignedLit ty nTy kn val)
            where
              op :: KnownNat n => Unsigned n -> Proxy n -> Integer
              op u _ = toInteger (Unsigned.negate# u)
          _ -> Nothing
    )

-- ExtendingNum
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.plus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Unsigned m -> Unsigned n -> Unsigned (Max m n + 1)
            | Just (i,j) <- unsignedLiterals args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkUnsignedLit resTy resSizeTy resSizeInt (i+j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.minus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- unsignedLiterals' args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
                   val = reifyNat resSizeInt (runSizedF (Unsigned.-#) i j)
              in   reduce (mkUnsignedLit resTy resSizeTy resSizeInt val)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.times#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- unsignedLiterals args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkUnsignedLit resTy resSizeTy resSizeInt (i*j))
          _ -> Nothing
    )

-- Integral
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.quot#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.quot#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.rem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.rem#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.toInteger#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
            , primName p == showt 'Clash.Sized.Internal.Unsigned.fromInteger#
            -> reduce (integerToIntegerLiteral i)
          _ -> Nothing
    )

-- Bits
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.and#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- unsignedLiterals args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkUnsignedLit ty nTy kn (i .&. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.or#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- unsignedLiterals args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkUnsignedLit ty nTy kn (i .|. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.xor#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- unsignedLiterals args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkUnsignedLit ty nTy kn (i `xor` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.complement#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- unsignedLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> let val = reifyNat kn (op (fromInteger i))
            in reduce (mkUnsignedLit ty nTy kn val)
            where
              op :: KnownNat n => Unsigned n -> Proxy n -> Integer
              op u _ = toInteger (Unsigned.complement# u)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.shiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
            | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkUnsignedLit ty nTy kn val)
              where
                op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Unsigned.shiftL# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.shiftR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
            | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkUnsignedLit ty nTy kn val)
              where
                op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Unsigned.shiftR# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.rotateL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
            | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkUnsignedLit ty nTy kn val)
              where
                op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Unsigned.rotateL# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.rotateR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
            | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkUnsignedLit ty nTy kn val)
              where
                op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Unsigned.rotateR# u i)
          _ -> Nothing
    )

-- Resize
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.resize#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- forall n m . KnownNat m => Unsigned n -> Unsigned m
            | _ : mTy : _ <- tys
            , Right km <- runExcept (tyNatSize tcm mTy)
            , [i] <- unsignedLiterals' args
            -> let bitsKeep = (bit (fromInteger km)) - 1
                   val = i .&. bitsKeep
            in reduce (mkUnsignedLit ty mTy km val)
          _ -> Nothing
    )

-- Conversions
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.unsignedToWord)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [a] <- unsignedLiterals' args
            -> let b = Unsigned.unsignedToWord (U (fromInteger a))
                   (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                   (Just wordTc) = UniqMap.lookup wordTcNm tcm
                   [wordDc] = tyConDataCons wordTc
               in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.unsigned8toWord8)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [a] <- unsignedLiterals' args
            -> let b = Unsigned.unsigned8toWord8 (U (fromInteger a))
                   (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                   (Just wordTc) = UniqMap.lookup wordTcNm tcm
                   [wordDc] = tyConDataCons wordTc
               in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.unsigned16toWord16)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [a] <- unsignedLiterals' args
            -> let b = Unsigned.unsigned16toWord16 (U (fromInteger a))
                   (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                   (Just wordTc) = UniqMap.lookup wordTcNm tcm
                   [wordDc] = tyConDataCons wordTc
               in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.unsigned32toWord32)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [a] <- unsignedLiterals' args
            -> let b = Unsigned.unsigned32toWord32 (U (fromInteger a))
                   (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                   (Just wordTc) = UniqMap.lookup wordTcNm tcm
                   [wordDc] = tyConDataCons wordTc
               in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Annotations.BitRepresentation.Deriving.dontApplyInHDL)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , f : a : _ <- args
            -> reduceWHNF (mkApps (valToTerm f) [Left (valToTerm a)])
          _ -> Nothing
    )

--------
-- RTree
--------
  , ( $(textNameLit 'Clash.Sized.RTree.textract)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [DC _ tArgs] <- args
            -> reduceWHNF (Either.lefts tArgs !! 1)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.RTree.tsplit)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , dTy : aTy : _ <- tys
            , [DC _ tArgs] <- args
            , (tyArgs,tyView -> TyConApp tupTcNm _) <- splitFunForallTy ty
            , TyConApp treeTcNm _ <- tyView (Either.rights tyArgs !! 0)
            -> let (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc]      = tyConDataCons tupTc
               in  reduce $
                   mkApps (Data tupDc)
                          [Right (mkTyConApp treeTcNm [dTy,aTy])
                          ,Right (mkTyConApp treeTcNm [dTy,aTy])
                          ,Left (Either.lefts tArgs !! 1)
                          ,Left (Either.lefts tArgs !! 2)
                          ]
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.RTree.tdfold)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , pTy : kTy : aTy : _ <- tys
            , _ : p : f : g : ts : _ <- args
            , DC _ tArgs <- ts
            , Right k' <- runExcept (tyNatSize tcm kTy)
            -> case k' of
                 0 -> reduceWHNF (mkApps (valToTerm f) [Left (Either.lefts tArgs !! 1)])
                 _ -> let k'ty = LitTy (NumTy (k'-1))
                          (tyArgs,_)  = splitFunForallTy ty
                          (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 3)
                          TyConApp snatTcNm _ = tyView (Either.rights tyArgs' !! 0)
                          Just snatTc = UniqMap.lookup snatTcNm tcm
                          [snatDc]    = tyConDataCons snatTc
                      in  reduceWHNF $
                          mkApps (valToTerm g)
                                 [Right k'ty
                                 ,Left (mkApps (Data snatDc)
                                               [Right k'ty
                                               ,Left (Literal (NaturalLiteral (k'-1)))])
                                 ,Left (mkApps (Prim pInfo)
                                               [Right pTy
                                               ,Right k'ty
                                               ,Right aTy
                                               ,Left (Literal (NaturalLiteral (k'-1)))
                                               ,Left (valToTerm p)
                                               ,Left (valToTerm f)
                                               ,Left (valToTerm g)
                                               ,Left (Either.lefts tArgs !! 1)
                                               ])
                                 ,Left (mkApps (Prim pInfo)
                                               [Right pTy
                                               ,Right k'ty
                                               ,Right aTy
                                               ,Left (Literal (NaturalLiteral (k'-1)))
                                               ,Left (valToTerm p)
                                               ,Left (valToTerm f)
                                               ,Left (valToTerm g)
                                               ,Left (Either.lefts tArgs !! 2)
                                               ])
                                 ]
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.RTree.treplicate)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , let ty' = piResultTys tcm ty tys
            , (_,tyView -> TyConApp treeTcNm [lenTy,argTy]) <- splitFunForallTy ty'
            , Right len <- runExcept (tyNatSize tcm lenTy)
            -> let (Just treeTc) = UniqMap.lookup treeTcNm tcm
                   [lrCon,brCon] = tyConDataCons treeTc
               in  reduce (mkRTree lrCon brCon argTy len (replicate (2^len) (valToTerm (last args))))
          _ -> Nothing
    )

---------
-- Vector
---------
  , ( $(textNameLit 'Clash.Sized.Vector.length)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => Vec n a -> Int
            | isSubj
            , [nTy, _] <- tys
            , Right n <-runExcept (tyNatSize tcm nTy)
            -> let (_, tyView -> TyConApp intTcNm _) = splitFunForallTy ty
                   (Just intTc) = UniqMap.lookup intTcNm tcm
                   [intCon] = tyConDataCons intTc
               in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (toInteger n)))])
          _ -> Nothing
    )

  -- XXX: Not a thing anymore?
  , ( "Clash.Sized.Vector.maxIndex"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [nTy, _] <- tys
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> let (_, tyView -> TyConApp intTcNm _) = splitFunForallTy ty
                   (Just intTc) = UniqMap.lookup intTcNm tcm
                   [intCon] = tyConDataCons intTc
               in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (toInteger (n - 1))))])
          _ -> Nothing
    )

-- Indexing
  -- XXX: Not exported
  , ( "Clash.Sized.Vector.index_int"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => Vec n a -> Int
            | nTy : aTy : _  <- tys
            , _ : xs : i : _ <- args
            , DC intDc [Left (Literal (IntLiteral i'))] <- i
            -> if i' < 0
                  then Nothing
                  else case xs of
                         DC _ vArgs  -> case runExcept (tyNatSize tcm nTy) of
                            Right 0  -> Nothing
                            Right n' ->
                              if i' == 0
                                 then reduceWHNF (Either.lefts vArgs !! 1)
                                 else reduceWHNF $
                                      mkApps (Prim pInfo)
                                             [Right (LitTy (NumTy (n'-1)))
                                             ,Right aTy
                                             ,Left (Literal (NaturalLiteral (n'-1)))
                                             ,Left (Either.lefts vArgs !! 2)
                                             ,Left (mkApps (Data intDc)
                                                           [Left (Literal (IntLiteral (i'-1)))])
                                             ]
                            _ -> Nothing
                         _ -> Nothing
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Vector.head)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Vec (n+1) a -> a
            | isSubj
            , [DC _ vArgs] <- args
            -> reduceWHNF (Either.lefts vArgs !! 1)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Vector.last)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Vec (n+1) a -> a
            | isSubj
            , [DC _ vArgs] <- args
            , (Right _ : Right aTy : Right nTy : _) <- vArgs
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> if n == 0
                  then reduceWHNF (Either.lefts vArgs !! 1)
                  else reduceWHNF
                        (mkApps (Prim pInfo)
                                             [Right (LitTy (NumTy (n-1)))
                                             ,Right aTy
                                             ,Left (Either.lefts vArgs !! 2)
                                             ])
          _ -> Nothing
    )
-- - Sub-vectors
  , ( $(textNameLit 'Clash.Sized.Vector.tail)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Vec (n+1) a -> Vec n a
            | isSubj
            , [DC _ vArgs] <- args
            -> reduceWHNF (Either.lefts vArgs !! 2)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Vector.init)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Vec (n+1) a -> Vec n a
            | isSubj
            , [DC consCon vArgs] <- args
            , (Right _ : Right aTy : Right nTy : _) <- vArgs
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> if n == 0
                  then reduceWHNF (Either.lefts vArgs !! 2)
                  else reduce $
                       mkVecCons consCon aTy n
                          (Either.lefts vArgs !! 1)
                          (mkApps (Prim pInfo)
                                               [Right (LitTy (NumTy (n-1)))
                                               ,Right aTy
                                               ,Left (Either.lefts vArgs !! 2)])
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Vector.select)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: (CmpNat (i+s) (s*n) ~ GT) => SNat f -> SNat s -> SNat n -> Vec (f + i) a -> Vec n a
            | isSubj
            , iTy : sTy : nTy : fTy : aTy : _ <- tys
            , eq : f : s : n : xs : _ <- args
            , Right n' <- runExcept (tyNatSize tcm nTy)
            , Right f' <- runExcept (tyNatSize tcm fTy)
            , Right i' <- runExcept (tyNatSize tcm iTy)
            , Right s' <- runExcept (tyNatSize tcm sTy)
            , DC _ vArgs <- xs
            -> case n' of
                 0 -> reduce (mkVecNil nilCon aTy)
                 _ -> case f' of
                  0 -> let splitAtCall =
                            mkApps (splitAtPrim snatTcNm vecTcNm)
                                   [Right sTy
                                   ,Right (LitTy (NumTy (i'-s')))
                                   ,Right aTy
                                   ,Left (valToTerm s)
                                   ,Left (valToTerm xs)
                                   ]
                           fVecTy = mkTyConApp vecTcNm [sTy,aTy]
                           iVecTy = mkTyConApp vecTcNm [LitTy (NumTy (i'-s')),aTy]
                           -- Guaranteed no capture, so okay to use unsafe name generation
                           fNm    = mkUnsafeSystemName "fxs" 0
                           iNm    = mkUnsafeSystemName "ixs" 1
                           fId    = mkLocalId fVecTy fNm
                           iId    = mkLocalId iVecTy iNm
                           tupPat = DataPat tupDc [] [fId,iId]
                           iAlt   = (tupPat, (Var iId))
                       in  reduce $
                           mkVecCons consCon aTy n' (Either.lefts vArgs !! 1) $
                           mkApps (Prim pInfo)
                                  [Right (LitTy (NumTy (i'-s')))
                                  ,Right sTy
                                  ,Right (LitTy (NumTy (n'-1)))
                                  ,Right (LitTy (NumTy 0))
                                  ,Right aTy
                                  ,Left (valToTerm eq)
                                  ,Left (Literal (NaturalLiteral 0))
                                  ,Left (valToTerm s)
                                  ,Left (Literal (NaturalLiteral (n'-1)))
                                  ,Left (Case splitAtCall iVecTy [iAlt])
                                  ]
                  _ -> let splitAtCall =
                            mkApps (splitAtPrim snatTcNm vecTcNm)
                                   [Right fTy
                                   ,Right iTy
                                   ,Right aTy
                                   ,Left (valToTerm f)
                                   ,Left (valToTerm xs)
                                   ]
                           fVecTy = mkTyConApp vecTcNm [fTy,aTy]
                           iVecTy = mkTyConApp vecTcNm [iTy,aTy]
                           -- Guaranteed no capture, so okay to use unsafe name generation
                           fNm    = mkUnsafeSystemName "fxs" 0
                           iNm    = mkUnsafeSystemName "ixs" 1
                           fId    = mkLocalId fVecTy fNm
                           iId    = mkLocalId iVecTy iNm
                           tupPat = DataPat tupDc [] [fId,iId]
                           iAlt   = (tupPat, (Var iId))
                       in  reduceWHNF $
                           mkApps (Prim pInfo)
                             [Right iTy
                             ,Right sTy
                             ,Right nTy
                             ,Right (LitTy (NumTy 0))
                             ,Right aTy
                             ,Left (valToTerm eq)
                             ,Left (Literal (NaturalLiteral 0))
                             ,Left (valToTerm s)
                             ,Left (valToTerm n)
                             ,Left (Case splitAtCall iVecTy [iAlt])
                             ]
            where
              (tyArgs,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
              Just vecTc          = UniqMap.lookup vecTcNm tcm
              [nilCon,consCon]    = tyConDataCons vecTc
              TyConApp snatTcNm _ = tyView (Either.rights tyArgs !! 1)
              tupTcNm            = ghcTyconToTyConName (tupleTyCon Boxed 2)
              (Just tupTc)       = UniqMap.lookup tupTcNm tcm
              [tupDc]            = tyConDataCons tupTc
          _ -> Nothing
    )
-- - Splitting
  , ( $(textNameLit 'Clash.Sized.Vector.splitAt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
            | isSubj
            , (DC snatDc (Right mTy:_)):_ <- args
            , Right m <- runExcept (tyNatSize tcm mTy)
            -> let _:nTy:aTy:_ = tys
                   -- Get the tuple data-constructor
                   ty1 = piResultTys tcm ty tys
                   (_,tyView -> TyConApp tupTcNm tyArgs@(tyArg:_)) = splitFunForallTy ty1
                   (Just tupTc)       = UniqMap.lookup tupTcNm tcm
                   [tupDc]            = tyConDataCons tupTc
                   -- Get the vector data-constructors
                   TyConApp vecTcNm _ = tyView tyArg
                   Just vecTc         = UniqMap.lookup vecTcNm tcm
                   [nilCon,consCon]   = tyConDataCons vecTc
                   -- Recursive call to @splitAt@
                   splitAtRec v =
                    mkApps (Prim pInfo)
                           [Right (LitTy (NumTy (m-1)))
                           ,Right nTy
                           ,Right aTy
                           ,Left (mkApps (Data snatDc)
                                         [ Right (LitTy (NumTy (m-1)))
                                         , Left  (Literal (NaturalLiteral (m-1)))])
                           ,Left v
                           ]
                   m1VecTy = mkTyConApp vecTcNm [LitTy (NumTy (m-1)),aTy]
                   nVecTy  = mkTyConApp vecTcNm [nTy,aTy]
                   -- Guaranteed no capture, so okay to use unsafe name generation
                   lNm     = mkUnsafeSystemName "l" 0
                   rNm     = mkUnsafeSystemName "r" 1
                   lId     = mkLocalId m1VecTy lNm
                   rId     = mkLocalId nVecTy rNm
                   tupPat  = DataPat tupDc [] [lId,rId]
                   lAlt    = (tupPat, (Var lId))
                   rAlt    = (tupPat, (Var rId))

               in case m of
                 -- (Nil,v)
                 0 -> reduce $
                      mkApps (Data tupDc) $ (map Right tyArgs) ++
                        [ Left (mkVecNil nilCon aTy)
                        , Left (valToTerm (last args))
                        ]
                 -- (x:xs) <- v
                 m' | DC _ vArgs <- last args
                    -- (x:fst (splitAt (m-1) xs),snd (splitAt (m-1) xs))
                    -> case Either.lefts vArgs of
                        (_ : x : xs : _) ->
                          let (mach1, recId) = newLetBinding tcm mach (splitAtRec xs)
                          in reduceWith mach1 $
                            mkApps (Data tupDc) $ (map Right tyArgs) ++
                              [ Left (mkVecCons consCon aTy m' x
                                        (Case (Var recId) m1VecTy [lAlt]))
                              , Left (Case (Var recId) nVecTy [rAlt])
                              ]
                        _ ->
                          -- v actually reduces to Nil and not Cons, this only happens
                          -- when 'n' would reduce to a negative number; the complement
                          -- of 'm'.
                          --
                          -- See Clash issue: https://github.com/clash-lang/clash-compiler/issues/2831
                          let resTy = getResultTy tcm ty tys
                           in reduce (TyApp (Prim NP.undefined) resTy)

                 -- v doesn't reduce to a data-constructor
                 _  -> Nothing
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Vector.unconcat)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => SNamt m -> Vec (n * m) a -> Vec n (Vec m a)
            | isSubj
            , kn : snat : v : _  <- args
            , nTy : mTy : aTy :_ <- tys
            , Lit (NaturalLiteral n) <- kn
            -> let ( Either.rights -> argTys, tyView -> TyConApp vecTcNm _) =
                      splitFunForallTy ty
                   Just vecTc = UniqMap.lookup vecTcNm tcm
                   [nilCon,consCon]   = tyConDataCons vecTc
                   tupTcNm            = ghcTyconToTyConName (tupleTyCon Boxed 2)
                   (Just tupTc)       = UniqMap.lookup tupTcNm tcm
                   [tupDc]            = tyConDataCons tupTc
                   TyConApp snatTcNm _ = tyView (argTys !! 1)
                   n1mTy  = mkTyConApp typeNatMul
                                [mkTyConApp typeNatSub [nTy,LitTy (NumTy 1)]
                                ,mTy]
                   splitAtCall =
                    mkApps (splitAtPrim snatTcNm vecTcNm)
                           [Right mTy
                           ,Right n1mTy
                           ,Right aTy
                           ,Left (valToTerm snat)
                           ,Left (valToTerm v)
                           ]
                   mVecTy   = mkTyConApp vecTcNm [mTy,aTy]
                   n1mVecTy = mkTyConApp vecTcNm [n1mTy,aTy]
                   -- Guaranteed no capture, so okay to use unsafe name generation
                   asNm     = mkUnsafeSystemName "as" 0
                   bsNm     = mkUnsafeSystemName "bs" 1
                   asId     = mkLocalId mVecTy asNm
                   bsId     = mkLocalId n1mVecTy bsNm
                   tupPat   = DataPat tupDc [] [asId,bsId]
                   asAlt    = (tupPat, (Var asId))
                   bsAlt    = (tupPat, (Var bsId))

               in  case n of
                 0 -> reduce (mkVecNil nilCon mVecTy)
                 _ -> reduce $
                      mkVecCons consCon mVecTy n
                        (Case splitAtCall mVecTy [asAlt])
                        (mkApps (Prim pInfo)
                            [Right (LitTy (NumTy (n-1)))
                            ,Right mTy
                            ,Right aTy
                            ,Left (Literal (NaturalLiteral (n-1)))
                            ,Left (valToTerm snat)
                            ,Left (Case splitAtCall n1mVecTy [bsAlt])])
          _ -> Nothing
    )
-- Construction
-- - initialisation
  , ( $(textNameLit 'Clash.Sized.Vector.replicate)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: SNat n -> a -> Vec n a
            | isSubj
            , let ty' = piResultTys tcm ty tys
            , let (_,resTy) = splitFunForallTy ty'
            , (TyConApp vecTcNm [lenTy,argTy]) <- tyView resTy
            , Right len <- runExcept (tyNatSize tcm lenTy)
            -> let (Just vecTc) = UniqMap.lookup vecTcNm tcm
                   [nilCon,consCon] = tyConDataCons vecTc
               in  reduce $
                   mkVec nilCon consCon argTy len
                         (replicate (fromInteger len) (valToTerm (last args)))
          _ -> Nothing
    )
-- - Concatenation
  , ( $(textNameLit '(Clash.Sized.Vector.++))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Vec n a -> Vec m a -> Vec (n + m) a
            | isSubj
            , (DC dc vArgs):_ <- args
            , Right nTy : Right aTy : _ <- vArgs
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0  -> reduce (valToTerm (last args))
                 n' | (_ : _ : mTy : _) <- tys
                    , Right m <- runExcept (tyNatSize tcm mTy)
                    -> -- x : (xs ++ ys)
                       reduce $
                       mkVecCons dc aTy (n' + m) (Either.lefts vArgs !! 1)
                         (mkApps (Prim pInfo)
                                              [Right (LitTy (NumTy (n'-1)))
                                              ,Right aTy
                                              ,Right mTy
                                              ,Left (Either.lefts vArgs !! 2)
                                              ,Left (valToTerm (last args))
                                              ])
                 _ -> Nothing
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Vector.concat)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Vec n (Vec m a) -> Vec (n * m) a
            | isSubj
            , (nTy : mTy : aTy : _)  <- tys
            , (xs : _)               <- args
            , DC dc vArgs <- xs
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                0 -> reduce (mkVecNil dc aTy)
                _ | _ : h' : t : _ <- Either.lefts  vArgs
                  , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
                  -> reduceWHNF $
                     mkApps (vecAppendPrim vecTcNm)
                            [Right mTy
                            ,Right aTy
                            ,Right $ mkTyConApp typeNatMul
                              [mkTyConApp typeNatSub [nTy,LitTy (NumTy 1)], mTy]
                            ,Left h'
                            ,Left $ mkApps (Prim pInfo)
                              [ Right (LitTy (NumTy (n-1)))
                              , Right mTy
                              , Right aTy
                              , Left t
                              ]
                            ]
                _ -> Nothing
          _ -> Nothing
    )

-- Modifying vectors
  , ( "Clash.Sized.Vector.replace_int"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => Vec n a -> Int -> a -> Vec n a
            | nTy : aTy : _  <- tys
            , _ : xs : i : a : _ <- args
            , DC intDc [Left (Literal (IntLiteral i'))] <- i
            -> if i' < 0
                  then Nothing
                  else case xs of
                         DC vecTcNm vArgs -> case runExcept (tyNatSize tcm nTy) of
                            Right 0  -> Nothing
                            Right n' ->
                              if i' == 0
                                 then reduce (mkVecCons vecTcNm aTy n' (valToTerm a) (Either.lefts vArgs !! 2))
                                 else reduce $
                                      mkVecCons vecTcNm aTy n' (Either.lefts vArgs !! 1)
                                        (mkApps (Prim pInfo)
                                                [Right (LitTy (NumTy (n'-1)))
                                                ,Right aTy
                                                ,Left (Literal (NaturalLiteral (n'-1)))
                                                ,Left (Either.lefts vArgs !! 2)
                                                ,Left (mkApps (Data intDc)
                                                              [Left (Literal (IntLiteral (i'-1)))])
                                                ,Left (valToTerm a)
                                                ])
                            _ -> Nothing
                         _ -> Nothing
          _ -> Nothing
    )

-- - specialized permutations
  , ( $(textNameLit 'Clash.Sized.Vector.reverse)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Vec n a -> Vec n a
            | isSubj
            , nTy : aTy : _  <- tys
            , [DC vecDc vArgs] <- args
            -> case runExcept (tyNatSize tcm nTy) of
                 Right 0 -> reduce (mkVecNil vecDc aTy)
                 Right n
                   | (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
                   , let (Just vecTc) = UniqMap.lookup vecTcNm tcm
                   , let [nilCon,consCon] = tyConDataCons vecTc
                   -> reduceWHNF $
                      mkApps (vecAppendPrim vecTcNm)
                        [Right (LitTy (NumTy (n-1)))
                        ,Right aTy
                        ,Right (LitTy (NumTy 1))
                        ,Left (mkApps (Prim pInfo)
                                      [Right (LitTy (NumTy (n-1)))
                                      ,Right aTy
                                      ,Left (Either.lefts vArgs !! 2)
                                      ])
                        ,Left (mkVec nilCon consCon aTy 1 [Either.lefts vArgs !! 1])
                        ]
                 _ -> Nothing
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Vector.transpose)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => Vec m (Vec n a) -> Vec n (Vec m a)
            | isSubj
            , nTy : mTy : aTy : _ <- tys
            , kn : xss : _ <- args
            , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
            , DC _ vArgs <- xss
            , Right n <- runExcept (tyNatSize tcm nTy)
            , Right m <- runExcept (tyNatSize tcm mTy)
            -> case m of
              0 -> let (Just vecTc)     = UniqMap.lookup vecTcNm tcm
                       [nilCon,consCon] = tyConDataCons vecTc
                   in  reduce $
                       mkVec nilCon consCon (mkTyConApp vecTcNm [mTy,aTy]) n
                        (replicate (fromInteger n) (mkVec nilCon consCon aTy 0 []))
              m' -> let (Just vecTc)     = UniqMap.lookup vecTcNm tcm
                        [_,consCon] = tyConDataCons vecTc
                        Just (consCoTy : _) = dataConInstArgTys consCon
                                                [mTy,aTy,LitTy (NumTy (m'-1))]
                    in  reduceWHNF $
                        mkApps (vecZipWithPrim vecTcNm)
                               [ Right aTy
                               , Right (mkTyConApp vecTcNm [LitTy (NumTy (m'-1)),aTy])
                               , Right (mkTyConApp vecTcNm [mTy,aTy])
                               , Right nTy
                               , Left  (mkApps (Data consCon)
                                               [Right mTy
                                               ,Right aTy
                                               ,Right (LitTy (NumTy (m'-1)))
                                               ,Left (primCo consCoTy)
                                               ])
                               , Left  (Either.lefts vArgs !! 1)
                               , Left  (mkApps (Prim pInfo)
                                               [ Right nTy
                                               , Right (LitTy (NumTy (m'-1)))
                                               , Right aTy
                                               , Left  (valToTerm kn)
                                               , Left  (Either.lefts vArgs !! 2)
                                               ])
                               ]
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Vector.rotateLeftS)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => Vec n a -> SNat d -> Vec n a
            | nTy : aTy : _ : _ <- tys
            , kn : xs : d : _ <- args
            , DC dc vArgs <- xs
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0  -> reduce (mkVecNil dc aTy)
                 n' | DC snatDc [_,Left d'] <- d
                    , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
                    , mach2@Machine{mStack=[],mTerm=Literal (NaturalLiteral d2)} <- whnf eval tcm isSubj (setTerm d' $ stackClear mach)
                    -> case (d2 `mod` n) of
                         0  -> reduce (valToTerm xs)
                         d3 -> let (_,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
                                   (Just vecTc)     = UniqMap.lookup vecTcNm tcm
                                   [nilCon,consCon] = tyConDataCons vecTc
                               in  reduceWHNF' mach2 $
                                   mkApps (Prim pInfo)
                                          [Right nTy
                                          ,Right aTy
                                          ,Right (LitTy (NumTy (d3-1)))
                                          ,Left (valToTerm kn)
                                          ,Left (mkApps (vecAppendPrim vecTcNm)
                                                        [Right (LitTy (NumTy (n'-1)))
                                                        ,Right aTy
                                                        ,Right (LitTy (NumTy 1))
                                                        ,Left  (Either.lefts vArgs !! 2)
                                                        ,Left  (mkVec nilCon consCon aTy 1 [Either.lefts vArgs !! 1])])
                                          ,Left (mkApps (Data snatDc)
                                                        [Right (LitTy (NumTy (d3-1)))
                                                        ,Left  (Literal (NaturalLiteral (d3-1)))])
                                          ]
                 _  -> Nothing
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Vector.rotateRightS)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: KnownNat n => Vec n a -> SNat d -> Vec n a
            | isSubj
            , nTy : aTy : _ : _ <- tys
            , kn : xs : d : _ <- args
            , DC dc _ <- xs
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0  -> reduce (mkVecNil dc aTy)
                 n' | DC snatDc [_,Left d'] <- d
                    , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
                    , mach2@Machine{mStack=[],mTerm=Literal (NaturalLiteral d2)} <- whnf eval tcm isSubj (setTerm d' $ stackClear mach)
                    -> case (d2 `mod` n) of
                         0  -> reduce (valToTerm xs)
                         d3 -> let (_,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
                               in  reduceWHNF' mach2 $
                                   mkApps (Prim pInfo)
                                          [Right nTy
                                          ,Right aTy
                                          ,Right (LitTy (NumTy (d3-1)))
                                          ,Left (valToTerm kn)
                                          ,Left (mkVecCons dc aTy n
                                                  (mkApps (vecLastPrim vecTcNm)
                                                          [Right (LitTy (NumTy (n'-1)))
                                                          ,Right aTy
                                                          ,Left  (valToTerm xs)])
                                                  (mkApps (vecInitPrim vecTcNm)
                                                          [Right (LitTy (NumTy (n'-1)))
                                                          ,Right aTy
                                                          ,Left (valToTerm xs)]))
                                          ,Left (mkApps (Data snatDc)
                                                        [Right (LitTy (NumTy (d3-1)))
                                                        ,Left  (Literal (NaturalLiteral (d3-1)))])
                                          ]
                 _  -> Nothing
          _ -> Nothing
    )
-- Element-wise operations
-- - mapping
  , ( $(textNameLit 'Clash.Sized.Vector.map)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: (a -> b) -> Vec n a -> Vec n b
            | isSubj
            , DC dc vArgs <- args !! 1
            , aTy : bTy : nTy : _ <- tys
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0  -> reduce (mkVecNil dc bTy)
                 n' -> reduce $
                       mkVecCons dc bTy n'
                         (mkApps (valToTerm (args !! 0)) [Left (Either.lefts vArgs !! 1)])
                         (mkApps (Prim pInfo)
                                              [Right aTy
                                              ,Right bTy
                                              ,Right (LitTy (NumTy (n' - 1)))
                                              ,Left (valToTerm (args !! 0))
                                              ,Left (Either.lefts vArgs !! 2)])
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Vector.imap)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n a b . KnownNat n => (Index n -> a -> b) -> Vec n a -> Vec n b
            | isSubj
            , nTy : aTy : bTy : _ <- tys
            , (tyArgs,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
            , let (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 1)
            , TyConApp indexTcNm _ <- tyView (Either.rights tyArgs' !! 0)
            , Right n <- runExcept (tyNatSize tcm nTy)
            , let iLit = mkIndexLit (Either.rights tyArgs' !! 0) nTy n 0
            -> reduceWHNF $
               mkApps (Prim (PrimInfo "Clash.Sized.Vector.imap_go" (vecImapGoTy vecTcNm indexTcNm) WorkNever SingleResult NoUnfolding))
                      [Right nTy
                      ,Right nTy
                      ,Right aTy
                      ,Right bTy
                      ,Left (valToTerm (args !! 1))
                      ,Left (valToTerm (args !! 2))
                      ,Left iLit
                      ]
          _ -> Nothing
    )

  , ( "Clash.Sized.Vector.imap_go"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , nTy : mTy : aTy : bTy : _ <- tys
            , f : xs : (Suspend nArg) : _ <- args
            , DC dc vArgs <- xs
            , Right n' <- runExcept (tyNatSize tcm nTy)
            , Right m <- runExcept (tyNatSize tcm mTy)
            -> case m of
                 0  -> reduce (mkVecNil dc bTy)
                 m'
                  | eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
                  , mach1@Machine{mStack=[],mTerm=n} <-
                      whnf eval tcm True (setTerm nArg (stackClear mach))
                  ->  let (tyArgs,_) = splitFunForallTy ty
                          TyConApp indexTcNm _ = tyView (Either.rights tyArgs !! 2)
                          iLit = mkIndexLit (Either.rights tyArgs !! 2) nTy n' 1
                       in Just $ flip setTerm (mach1 {mStack = mStack mach}) $ mkVecCons dc bTy m'
                         (mkApps (valToTerm f) [Left n,Left (Either.lefts vArgs !! 1)])
                         (mkApps (Prim pInfo)
                                 [Right nTy
                                 ,Right (LitTy (NumTy (m'-1)))
                                 ,Right aTy
                                 ,Right bTy
                                 ,Left (valToTerm f)
                                 ,Left (Either.lefts vArgs !! 2)
                                 ,Left (mkApps (Prim (PrimInfo (showt '(Clash.Sized.Internal.Index.+#)) (indexAddTy indexTcNm) WorkVariable SingleResult NoUnfolding))
                                               [Right nTy
                                               ,Left (Literal (NaturalLiteral n'))
                                               ,Left n
                                               ,Left iLit
                                               ])
                                 ])
                  | otherwise
                  -> Nothing
          _ -> Nothing
    )

  -- :: forall n a. KnownNat n => (a -> a) -> a -> Vec n a
  , ( $(textNameLit 'Clash.Sized.Vector.iterateI)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [nTy, aTy] <- tys
            , [_n, f, a] <- args
            , Right n <- runExcept (tyNatSize tcm nTy)
            ->
              let
                TyConApp vecTcNm _ = tyView (getResultTy tcm ty tys)
                Just vecTc = UniqMap.lookup vecTcNm tcm
                [nilCon, consCon] = tyConDataCons vecTc
              in case n of
                 0 -> reduce (mkVecNil nilCon aTy)
                 _ -> reduce $
                  mkVecCons consCon aTy n
                    (valToTerm a)
                    (mkApps
                      (Prim pInfo)
                      [ Right (LitTy (NumTy (n - 1)))
                      , Right aTy
                      , Left (valToTerm (Lit (NaturalLiteral (n - 1))))
                      , Left (valToTerm f)
                      , Left (mkApps (valToTerm f) [Left (valToTerm a)])
                      ])
          _ -> Nothing
    )

-- - Zipping
  , ( $(textNameLit 'Clash.Sized.Vector.zipWith)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
            | isSubj
            , aTy : bTy : cTy : nTy : _ <- tys
            , f : xs : ys : _   <- args
            , DC dc vArgs <- xs
            , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0  -> reduce (mkVecNil dc cTy)
                 -- We share the function 'f' and the second vector 'ys' via heap
                 -- let-bindings instead of inlining 'valToTerm f' / 'valToTerm ys'
                 -- twice. See #3308.
                 n' ->
                   let (mach1, fId)  = newLetBinding tcm mach  (valToTerm f)
                       (mach2, ysId) = newLetBinding tcm mach1 (valToTerm ys)
                   in reduceWith mach2 $ mkVecCons dc cTy n'
                         (mkApps (Var fId)
                                    [Left (Either.lefts vArgs !! 1)
                                    ,Left (mkApps (vecHeadPrim vecTcNm)
                                            [Right (LitTy (NumTy (n'-1)))
                                            ,Right bTy
                                            ,Left  (Var ysId)
                                            ])
                                    ])
                         (mkApps (Prim pInfo)
                                              [Right aTy
                                              ,Right bTy
                                              ,Right cTy
                                              ,Right (LitTy (NumTy (n' - 1)))
                                              ,Left (Var fId)
                                              ,Left (Either.lefts vArgs !! 2)
                                              ,Left (mkApps (vecTailPrim vecTcNm)
                                                            [Right (LitTy (NumTy (n'-1)))
                                                            ,Right bTy
                                                            ,Left (Var ysId)
                                                            ])])
          _ -> Nothing
    )

-- Folding
  , ( $(textNameLit 'Clash.Sized.Vector.foldr)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: (a -> b -> b) -> b -> Vec n a -> b
            | isSubj
            , aTy : bTy : nTy : _ <- tys
            , f : z : xs : _ <- args
            , DC _ vArgs <- xs
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0 -> reduce (valToTerm z)
                 _ -> reduceWHNF $
                      mkApps (valToTerm f)
                             [Left (Either.lefts vArgs !! 1)
                             ,Left (mkApps (Prim pInfo)
                                           [Right aTy
                                           ,Right bTy
                                           ,Right (LitTy (NumTy (n-1)))
                                           ,Left  (valToTerm f)
                                           ,Left  (valToTerm z)
                                           ,Left  (Either.lefts vArgs !! 2)
                                           ])
                             ]
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Vector.fold)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: (a -> a -> a) -> Vec (n + 1) a -> a
            | isSubj
            , nTy : aTy :  _ <- tys
            , f : vs : _ <- args
            , DC _ vArgs <- vs
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0 -> reduceWHNF (Either.lefts vArgs !! 1)
                 _ -> let (tyArgs,_)         = splitFunForallTy ty
                          TyConApp vecTcNm _ = tyView (Either.rights tyArgs !! 1)
                          tupTcNm      = ghcTyconToTyConName (tupleTyCon Boxed 2)
                          (Just tupTc) = UniqMap.lookup tupTcNm tcm
                          [tupDc]      = tyConDataCons tupTc
                          n'     = n+1
                          m      = n' `div` 2
                          n1     = n' - m
                          mTy    = LitTy (NumTy m)
                          m'ty   = LitTy (NumTy (m-1))
                          n1mTy  = LitTy (NumTy n1)
                          n1m'ty = LitTy (NumTy (n1-1))
                          splitAtCall =
                           mkApps (Prim (PrimInfo "Clash.Sized.Vector.fold_split" (foldSplitAtTy vecTcNm) WorkNever SingleResult NoUnfolding))
                                  [Right mTy
                                  ,Right n1mTy
                                  ,Right aTy
                                  ,Left (Literal (NaturalLiteral m))
                                  ,Left (valToTerm vs)
                                  ]
                          mVecTy   = mkTyConApp vecTcNm [mTy,aTy]
                          n1mVecTy = mkTyConApp vecTcNm [n1mTy,aTy]
                          -- Guaranteed no capture, so okay to use unsafe name generation
                          asNm     = mkUnsafeSystemName "as" 0
                          bsNm     = mkUnsafeSystemName "bs" 1
                          asId     = mkLocalId mVecTy asNm
                          bsId     = mkLocalId n1mVecTy bsNm
                          tupPat   = DataPat tupDc [] [asId,bsId]
                          asAlt    = (tupPat, (Var asId))
                          bsAlt    = (tupPat, (Var bsId))
                      in  reduceWHNF $
                          mkApps (valToTerm f)
                                 [Left (mkApps (Prim pInfo)
                                               [Right m'ty
                                               ,Right aTy
                                               ,Left (valToTerm f)
                                               ,Left (Case splitAtCall mVecTy [asAlt])
                                               ])
                                 ,Left (mkApps (Prim pInfo)
                                               [Right n1m'ty
                                               ,Right aTy
                                               ,Left  (valToTerm f)
                                               ,Left  (Case splitAtCall n1mVecTy [bsAlt])
                                               ])
                                 ]
          _ -> Nothing
    )


  , ( "Clash.Sized.Vector.fold_split"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Natural -> Vec (m + n) a -> (Vec m a, Vec n a)
            | isSubj
            , mTy : nTy : aTy : _ <- tys
            , Right m <- runExcept (tyNatSize tcm mTy)
            -> let -- Get the tuple data-constructor
                   ty1 = piResultTys tcm ty tys
                   (_,tyView -> TyConApp tupTcNm tyArgs@(tyArg:_)) = splitFunForallTy ty1
                   (Just tupTc)       = UniqMap.lookup tupTcNm tcm
                   [tupDc]            = tyConDataCons tupTc
                   -- Get the vector data-constructors
                   TyConApp vecTcNm _ = tyView tyArg
                   Just vecTc         = UniqMap.lookup vecTcNm tcm
                   [nilCon,consCon]   = tyConDataCons vecTc
                   -- Recursive call to @splitAt@
                   splitAtRec v =
                    mkApps (Prim pInfo)
                           [Right (LitTy (NumTy (m-1)))
                           ,Right nTy
                           ,Right aTy
                           ,Left (Literal (NaturalLiteral (m-1)))
                           ,Left v
                           ]
                   -- Projection either the first or second field of the recursive
                   -- call to @splitAt@
                   splitAtSelR v = Case (splitAtRec v)
                   m1VecTy = mkTyConApp vecTcNm [LitTy (NumTy (m-1)),aTy]
                   nVecTy  = mkTyConApp vecTcNm [nTy,aTy]
                   -- Guaranteed no capture, so okay to use unsafe name generation
                   lNm     = mkUnsafeSystemName "l" 0
                   rNm     = mkUnsafeSystemName "r" 1
                   lId     = mkLocalId m1VecTy lNm
                   rId     = mkLocalId nVecTy rNm
                   tupPat  = DataPat tupDc [] [lId,rId]
                   lAlt    = (tupPat, (Var lId))
                   rAlt    = (tupPat, (Var rId))
               in case m of
                 -- (Nil,v)
                 0 -> reduce $
                      mkApps (Data tupDc) $ (map Right tyArgs) ++
                        [ Left (mkVecNil nilCon aTy)
                        , Left (valToTerm (last args))
                        ]
                 -- (x:xs) <- v
                 m' | DC _ vArgs <- last args
                    -- (x:fst (splitAt (m-1) xs),snd (splitAt (m-1) xs))
                    -> reduce $
                       mkApps (Data tupDc) $ (map Right tyArgs) ++
                         [ Left (mkVecCons consCon aTy m' (Either.lefts vArgs !! 1)
                                   (splitAtSelR (Either.lefts vArgs !! 2) m1VecTy [lAlt]))
                         , Left (splitAtSelR (Either.lefts vArgs !! 2) nVecTy [rAlt])
                         ]
                 -- v doesn't reduce to a data-constructor
                 _  -> Nothing
          _ -> Nothing
    )
-- - Specialised folds
  , ( $(textNameLit 'Clash.Sized.Vector.dfold)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , pTy : kTy : aTy : _ <- tys
            , _ : p : f : z : xs : _ <- args
            , DC _ vArgs <- xs
            , Right k' <- runExcept (tyNatSize tcm kTy)
            -> case k'  of
                 0 -> reduce (valToTerm z)
                 _ -> let (tyArgs,_)  = splitFunForallTy ty
                          (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 2)
                          Just (tvN, _) = List.uncons $ Either.lefts tyArgs'
                          ubpT = Either.rights tyArgs' !! 0
                          fTVs = Lens.toListOf typeFreeVars ubpT
                          Just tvK = List.find (/= tvN) fTVs
                          subst0 = extendTvSubst (mkSubst is0) tvN k'ty
                          subst1 = extendTvSubst subst0 tvK (LitTy (NumTy k'))
                          witness = normalizeType tcm (substTy subst1 ubpT)
                          TyConApp tupTcNm _ = tyView witness
                          Just witnessTc = UniqMap.lookup tupTcNm tcm
                          ubp : _ = tyConDataCons witnessTc
                          TyConApp snatTcNm _ = tyView (Either.rights tyArgs' !! 1)
                          Just snatTc = UniqMap.lookup snatTcNm tcm
                          [snatDc]    = tyConDataCons snatTc
                          k'ty        = LitTy (NumTy (k'-1))
                      in  reduceWHNF $
                          mkApps (valToTerm f)
                                 [Right k'ty
                                 ,Left (Data ubp)
                                 ,Left (mkApps (Data snatDc)
                                               [Right k'ty
                                               ,Left (Literal (NaturalLiteral (k'-1)))])
                                 ,Left (Either.lefts vArgs !! 1)
                                 ,Left (mkApps (Prim pInfo)
                                               [Right pTy
                                               ,Right k'ty
                                               ,Right aTy
                                               ,Left (Literal (NaturalLiteral (k'-1)))
                                               ,Left (valToTerm p)
                                               ,Left (valToTerm f)
                                               ,Left (valToTerm z)
                                               ,Left (Either.lefts vArgs !! 2)
                                               ])
                                 ]
            where
              is0 = mScopeNames mach
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Vector.dtfold)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , pTy : kTy : aTy : _ <- tys
            , _ : p : f : g : xs : _ <- args
            , DC _ vArgs <- xs
            , Right k' <- runExcept (tyNatSize tcm kTy)
            -> case k' of
                 0 -> reduceWHNF (mkApps (valToTerm f) [Left (Either.lefts vArgs !! 1)])
                 _ -> let (tyArgs,_)  = splitFunForallTy ty
                          TyConApp vecTcNm _ = tyView (Either.rights tyArgs !! 4)
                          (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 3)
                          TyConApp snatTcNm _ = tyView (Either.rights tyArgs' !! 0)
                          Just snatTc = UniqMap.lookup snatTcNm tcm
                          [snatDc]    = tyConDataCons snatTc
                          tupTcNm     = ghcTyconToTyConName (tupleTyCon Boxed 2)
                          (Just tupTc) = UniqMap.lookup tupTcNm tcm
                          [tupDc]     = tyConDataCons tupTc
                          k'ty        = LitTy (NumTy (k'-1))
                          k2ty        = LitTy (NumTy (2^(k'-1)))
                          splitAtCall =
                           mkApps (splitAtPrim snatTcNm vecTcNm)
                                  [Right k2ty
                                  ,Right k2ty
                                  ,Right aTy
                                  ,Left (mkApps (Data snatDc)
                                                [Right k2ty
                                                ,Left (Literal (NaturalLiteral (2^(k'-1))))])
                                  ,Left (valToTerm xs)
                                  ]
                          xsSVecTy = mkTyConApp vecTcNm [k2ty,aTy]
                          -- Guaranteed no capture, so okay to use unsafe name generation
                          xsLNm    = mkUnsafeSystemName "xsL" 0
                          xsRNm    = mkUnsafeSystemName "xsR" 1
                          xsLId    = mkLocalId k2ty xsLNm
                          xsRId    = mkLocalId k2ty xsRNm
                          tupPat   = DataPat tupDc [] [xsLId,xsRId]
                          asAlt    = (tupPat, (Var xsLId))
                          bsAlt    = (tupPat, (Var xsRId))
                      in  reduceWHNF $
                          mkApps (valToTerm g)
                                 [Right k'ty
                                 ,Left (mkApps (Data snatDc)
                                               [Right k'ty
                                               ,Left (Literal (NaturalLiteral (k'-1)))])
                                 ,Left (mkApps (Prim pInfo)
                                               [Right pTy
                                               ,Right k'ty
                                               ,Right aTy
                                               ,Left (Literal (NaturalLiteral (k'-1)))
                                               ,Left (valToTerm p)
                                               ,Left (valToTerm f)
                                               ,Left (valToTerm g)
                                               ,Left (Case splitAtCall xsSVecTy [asAlt])])
                                 ,Left (mkApps (Prim pInfo)
                                               [Right pTy
                                               ,Right k'ty
                                               ,Right aTy
                                               ,Left (Literal (NaturalLiteral (k'-1)))
                                               ,Left (valToTerm p)
                                               ,Left (valToTerm f)
                                               ,Left (valToTerm g)
                                               ,Left (Case splitAtCall xsSVecTy [bsAlt])])
                                 ]
          _ -> Nothing
    )
-- Misc
  , ( $(textNameLit 'Clash.Sized.Vector.lazyV)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , nTy : aTy : _ <- tys
            , _ : xs : _ <- args
            , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0  -> let (Just vecTc) = UniqMap.lookup vecTcNm tcm
                           [nilCon,_]   = tyConDataCons vecTc
                       in  reduce (mkVecNil nilCon aTy)
                 n' -> let (Just vecTc) = UniqMap.lookup vecTcNm tcm
                           [_,consCon]  = tyConDataCons vecTc
                       in  reduce $ mkVecCons consCon aTy n'
                             (mkApps (vecHeadPrim vecTcNm)
                                     [ Right (LitTy (NumTy (n' - 1)))
                                     , Right aTy
                                     , Left  (valToTerm xs)
                                     ])
                             (mkApps (Prim pInfo)
                                     [ Right (LitTy (NumTy (n' - 1)))
                                     , Right aTy
                                     , Left  (Literal (NaturalLiteral (n'-1)))
                                     , Left  (mkApps (vecTailPrim vecTcNm)
                                                     [ Right (LitTy (NumTy (n'-1)))
                                                     , Right aTy
                                                     , Left  (valToTerm xs)
                                                     ])
                                     ])
          _ -> Nothing
    )
-- Traversable
  , ( $(textNameLit 'Clash.Sized.Vector.traverse#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , aTy : fTy : bTy : nTy : _ <- tys
            , apDict : f : xs : _ <- args
            , DC dc vArgs <- xs
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0 -> let (pureF,ids') = runPEM (mkSelectorCase $(curLoc) is0 tcm (valToTerm apDict) 1 1) ids
                      in  reduceWHNF' (mach { mSupply = ids' }) $
                          mkApps pureF
                                 [Right (mkTyConApp (vecTcNm) [nTy,bTy])
                                 ,Left  (mkVecNil dc bTy)]
                 _ -> let ((fmapF,apF),ids') = flip runPEM ids $ do
                            fDict  <- mkSelectorCase $(curLoc) is0 tcm (valToTerm apDict) 1 0
                            fmapF' <- mkSelectorCase $(curLoc) is0 tcm fDict 1 0
                            apF'   <- mkSelectorCase $(curLoc) is0 tcm (valToTerm apDict) 1 2
                            return (fmapF',apF')
                          n'ty = LitTy (NumTy (n-1))
                          Just (consCoTy : _) = dataConInstArgTys dc [nTy,bTy,n'ty]
                      in  reduceWHNF' (mach { mSupply = ids' }) $
                          mkApps apF
                                 [Right (mkTyConApp vecTcNm [n'ty,bTy])
                                 ,Right (mkTyConApp vecTcNm [nTy,bTy])
                                 ,Left (mkApps fmapF
                                               [Right bTy
                                               ,Right (mkFunTy (mkTyConApp vecTcNm [n'ty,bTy])
                                                               (mkTyConApp vecTcNm [nTy,bTy]))
                                               ,Left (mkApps (Data dc)
                                                             [Right nTy
                                                             ,Right bTy
                                                             ,Right n'ty
                                                             ,Left (primCo consCoTy)])
                                               ,Left (mkApps (valToTerm f)
                                                             [Left (Either.lefts vArgs !! 1)])
                                               ])
                                 ,Left (mkApps (Prim pInfo)
                                               [Right aTy
                                               ,Right fTy
                                               ,Right bTy
                                               ,Right n'ty
                                               ,Left (valToTerm apDict)
                                               ,Left (valToTerm f)
                                               ,Left (Either.lefts vArgs !! 2)
                                               ])
                                 ]
            where
              (tyArgs,_)         = splitFunForallTy ty
              TyConApp vecTcNm _ = tyView (Either.rights tyArgs !! 2)
              (ids, is0) = (mSupply mach, mScopeNames mach)
          _ -> Nothing
    )

-- BitPack
  , ( $(textNameLit 'Clash.Sized.Vector.concatBitVector#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , nTy : mTy : _ <- tys
            , _  : km  : v : _ <- args
            , DC _ vArgs <- v
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0  -> let resTyInfo = extractTySizeInfo tcm ty tys
                       in  reduce (mkBitVectorLit' resTyInfo 0 0)
                 n' | Right m <- runExcept (tyNatSize tcm mTy)
                    , (_,tyView -> TyConApp bvTcNm _) <- splitFunForallTy ty
                    -> reduceWHNF $
                       mkApps (bvAppendPrim bvTcNm)
                         [ Right (mkTyConApp typeNatMul [LitTy (NumTy (n'-1)),mTy])
                         , Right mTy
                         , Left (Literal (NaturalLiteral ((n'-1)*m)))
                         , Left (Either.lefts vArgs !! 1)
                         , Left (mkApps (Prim pInfo)
                                        [ Right (LitTy (NumTy (n'-1)))
                                        , Right mTy
                                        , Left (Literal (NaturalLiteral (n'-1)))
                                        , Left (valToTerm km)
                                        , Left (Either.lefts vArgs !! 2)
                                        ])
                         ]
                 _ -> Nothing
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Vector.unconcatBitVector#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , nTy : mTy : _  <- tys
            , _  : km  : bv : _ <- args
            , (_,tyView -> TyConApp vecTcNm [_,bvMTy]) <- splitFunForallTy ty
            , TyConApp bvTcNm _ <- tyView bvMTy
            , Right n <- runExcept (tyNatSize tcm nTy)
            -> case n of
                 0 ->
                  let (Just vecTc) = UniqMap.lookup vecTcNm tcm
                      [nilCon,_] = tyConDataCons vecTc
                  in  reduce (mkVecNil nilCon (mkTyConApp bvTcNm [mTy]))
                 n' | Right m <- runExcept (tyNatSize tcm mTy) ->
                  let Just vecTc  = UniqMap.lookup vecTcNm tcm
                      [_,consCon] = tyConDataCons vecTc
                      mBVTy       = mkTyConApp bvTcNm [mTy]
                      n1MTy       = mkTyConApp typeNatMul [LitTy (NumTy (n'-1)),mTy]
                      n1BVTy      = mkTyConApp bvTcNm [n1MTy]

                      (hd, tl) = case bitVectorLiteral bv of
                        -- Fast path for literals
                        Just (mski, i) ->
                          let sh     = fromInteger ((n'-1) * m)
                              loMask = bit sh - 1
                          in  ( mkBitVectorLit mBVTy mTy m
                                  (mski `shiftR` sh) (i `shiftR` sh)
                              , mkBitVectorLit n1BVTy n1MTy ((n'-1)*m)
                                  (mski .&. loMask) (i .&. loMask)
                              )
                        Nothing ->
                          let tupTcNm     = ghcTyconToTyConName (tupleTyCon Boxed 2)
                              Just tupTc  = UniqMap.lookup tupTcNm tcm
                              [tupDc]     = tyConDataCons tupTc
                              splitCall   =
                                mkApps (bvSplitPrim bvTcNm)
                                       [ Right n1MTy
                                       , Right mTy
                                       , Left (Literal (NaturalLiteral ((n'-1)*m)))
                                       , Left (valToTerm bv)
                                       ]
                              -- Guaranteed no capture, so okay to use unsafe name
                              -- generation
                              xNm         = mkUnsafeSystemName "x" 0
                              bvNm        = mkUnsafeSystemName "bv'" 1
                              xId         = mkLocalId mBVTy xNm
                              bvId        = mkLocalId n1BVTy bvNm
                              tupPat      = DataPat tupDc [] [xId,bvId]
                              xAlt        = (tupPat, (Var xId))
                              bvAlt       = (tupPat, (Var bvId))
                          in  ( Case splitCall mBVTy [xAlt]
                              , Case splitCall n1BVTy [bvAlt]
                              )

                  in  reduce $ mkVecCons consCon mBVTy n' hd
                        (mkApps (Prim pInfo)
                                [ Right (LitTy (NumTy (n'-1)))
                                , Right mTy
                                , Left (Literal (NaturalLiteral (n'-1)))
                                , Left (valToTerm km)
                                , Left tl
                                ])
                 _ -> Nothing
          _ -> Nothing
    )
  , ( "Data.Text.Show.$wunpackCStringAscii#"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (StringLiteral addr)] <- args
            , Text.Text (Text.ByteArray ba) _off len <- Text.pack addr
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   ret     = mkApps (Data tupDc) (map Right tyArgs ++
                            [ Left (Literal (ByteArrayLiteral (BA.ByteArray ba)))
                            , Left (Literal (IntLiteral 0))
                            , Left (Literal (IntLiteral (toInteger len)))])
                in reduce ret
          _ -> Nothing
    )
  -- XXX: Does not seem to exist?
  , ( "GHC.Magic.noinlineConstraint"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [arg] <- args
            -> reduce (valToTerm arg)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.TypeNats.withSomeSNat)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Lit (NaturalLiteral n) : fun : _ <- args
            , _ : funTy : _ <- Either.rights (fst (splitFunForallTy ty))
            , (tyView -> TyConApp snatTcNm _) : _ <- Either.rights (fst (splitFunForallTy funTy))
            , Just snatTc <- UniqMap.lookup snatTcNm tcm
            , [snatDc] <- tyConDataCons snatTc
            -> let nTy = LitTy (NumTy n)
                   snat = mkApps (Data snatDc) [Right nTy, Left (Literal (NaturalLiteral n))]
                   ret = mkApps (valToTerm fun) [Right nTy, Left snat]
                in reduce ret
          _ -> Nothing
    )
  -- XXX: Does not seem to exist?
  , ( "GHC.Magic.nospec"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [arg] <- args
            -> reduce (valToTerm arg)
          _ -> Nothing
    )
  , ( "GHC.Float.$wproperFractionDouble"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | _ : Lit (DoubleLiteral d) : _ <- args
            , [sty@(tyView -> TyConApp signedTcNm [nTy@(LitTy (NumTy kn))])] <- tys
            , nameOcc signedTcNm == showt ''Signed
            , (_, tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
            , Just tupTc <- UniqMap.lookup tupTcNm tcm
            , [tupDc] <- tyConDataCons tupTc
            -> let (sn, d1) = reifyNat kn (\p -> first toInteger (op p (castWord64ToDouble d)))
                   ret = mkApps (Data tupDc) (map Right tyArgs ++
                          [ Left (mkSignedLit sty nTy kn sn)
                          , Left (mkDoubleCLit tcm (castDoubleToWord64 d1) (last tyArgs))
                          ])
                in reduce ret
            where
              op :: KnownNat n => Proxy n -> Double -> (Signed n, Double)
              op _ = properFraction
          _ -> Nothing
    )
  , ( "GHC.Internal.Float.$wproperFractionDouble"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | _ : Lit (DoubleLiteral d) : _ <- args
            , [sty@(tyView -> TyConApp signedTcNm [nTy@(LitTy (NumTy kn))])] <- tys
            , nameOcc signedTcNm == showt ''Clash.Sized.Internal.Signed.Signed
            , (_, tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
            , Just tupTc <- UniqMap.lookup tupTcNm tcm
            , [tupDc] <- tyConDataCons tupTc
            -> let (sn, d1) = reifyNat kn (\p -> first toInteger (op p (castWord64ToDouble d)))
                   ret = mkApps (Data tupDc) (map Right tyArgs ++
                          [ Left (mkSignedLit sty nTy kn sn)
                          , Left (mkDoubleCLit tcm (castDoubleToWord64 d1) (last tyArgs))
                          ])
                in reduce ret
            where
              op :: KnownNat n => Proxy n -> Double -> (Signed n, Double)
              op _ = properFraction
          _ -> Nothing
    )

  ]


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
