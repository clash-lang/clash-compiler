{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.Evaluator.Primitive
  ( ghcPrimStep
  , ghcPrimUnwind
  , isUndefinedPrimVal
  , isUndefinedXPrimVal
  ) where

import qualified Data.HashMap.Strict as HashMap
import           Data.Text.Extra     (showt)
import           GHC.Prim

import           Clash.Core.Evaluator.Types
import Clash.Core.HasType (applyTypeToArgs)
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (PrimInfo (..), Term (..))
import Clash.Core.Util (undefinedPrims, undefinedXPrims)
import Clash.Util (curLoc)

import qualified Clash.Normalize.Primitives as NP
import qualified Clash.Sized.Internal.BitVector as BitVector

import {-# SOURCE #-} Clash.GHC.Evaluator

import qualified Clash.Sized.Internal.BitVector
import qualified Clash.Sized.Internal.Index
import qualified Clash.Sized.Internal.Signed
import qualified Clash.Sized.Internal.Unsigned
import qualified Clash.Sized.Vector
import qualified GHC.Classes
import qualified GHC.CString

import Clash.GHC.Evaluator.Primitive.Util
import Clash.GHC.Evaluator.Primitives (ghcPrimStepImpls)

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

ghcPrimStep :: PrimStep
ghcPrimStep tcm isSubj pInfo tys args mach =
  case HashMap.lookup (primName pInfo) ghcPrimStepImpls of
    Just impl -> impl tcm isSubj pInfo tys args mach
    Nothing -> Nothing
