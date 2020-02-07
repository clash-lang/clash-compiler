{-# LANGUAGE OverloadedStrings #-}

{-|
  Copyright     : (C) 2020, QBayLogic B.V.
  License       : BSD2 (see the file LICENSE)
  Maintainer    : Christiaan Baaij <christiaan.baaij@gmail.com>

  Types for the Partial Evaluator
-}
module Clash.Core.Evaluator.Types where

import Control.Concurrent.Supply (Supply)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap (insert, lookup)
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Text.Prettyprint.Doc (hsep)

import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal(CharLiteral))
import Clash.Core.Pretty (fromPpr)
import Clash.Core.Term (Term(..), PrimInfo(..), TickInfo, Alt)
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type (Type)
import Clash.Core.Var (Id, IdScope(..), TyVar)
import Clash.Core.VarEnv
import Clash.Pretty (ClashPretty(..), fromPretty)

{- [Note: forcing special primitives]
Clash uses the `whnf` function in two places (for now):

  1. The case-of-known-constructor transformation
  2. The reduceConstant transformation

The first transformation is needed to reach the required normal form. The
second transformation is more of cleanup transformation, so non-essential.

Normally, `whnf` would force the evaluation of all primitives, which is needed
in the `case-of-known-constructor` transformation. However, there are some
primitives which we want to leave unevaluated in the `reduceConstant`
transformation. Such primitives are:

  - Primitives such as `Clash.Sized.Vector.transpose`, `Clash.Sized.Vector.map`,
    etc. that do not reduce to an expression in normal form. Where the
    `reduceConstant` transformation is supposed to be normal-form preserving.
  - Primitives such as `GHC.Int.I8#`, `GHC.Word.W32#`, etc. which seem like
    wrappers around a 64-bit literal, but actually perform truncation to the
    desired bit-size.

This is why the Primitive Evaluator gets a flag telling whether it should
evaluate these special primitives.
-}

type PrimStep
  =  TyConMap
  -> Bool
  -> PrimInfo
  -> [Type]
  -> [Value]
  -> Machine
  -> Maybe Machine

type PrimUnwind
  =  TyConMap
  -> PrimInfo
  -> [Type]
  -> [Value]
  -> Value
  -> [Term]
  -> Machine
  -> Maybe Machine

type PrimEvaluator = (PrimStep, PrimUnwind)

data Machine = Machine
  { mPrimStep   :: PrimStep
  , mPrimUnwind :: PrimUnwind
  , mHeapPrim   :: PrimHeap
  , mHeapGlobal :: PureHeap
  , mHeapLocal  :: PureHeap
  , mStack      :: Stack
  , mSupply     :: Supply
  , mScopeNames :: InScopeSet
  , mTerm       :: Term
  }

instance Show Machine where
  show (Machine _ _ ph gh lh s _ _ x) =
    unlines
      [ "Machine:"
      , ""
      , "Heap (Prim):"
      , show ph
      , ""
      , "Heap (Globals):"
      , show gh
      , ""
      , "Heap (Locals):"
      , show lh
      , ""
      , "Stack:"
      , show (fmap clashPretty s)
      , ""
      , "Term:"
      , show x
      ]

type PrimHeap = (IntMap Term, Int)
type PureHeap = VarEnv Term

type Stack = [StackFrame]

data StackFrame
  = Update IdScope Id
  | Apply  Id
  | Instantiate Type
  | PrimApply  PrimInfo [Type] [Value] [Term]
  | Scrutinise Type [Alt]
  | Tickish TickInfo
  | Castish Type Type
  deriving Show

instance ClashPretty StackFrame where
  clashPretty (Update GlobalId i) = hsep ["Update(Global)", fromPpr i]
  clashPretty (Update LocalId i)  = hsep ["Update(Local)", fromPpr i]
  clashPretty (Apply i) = hsep ["Apply", fromPpr i]
  clashPretty (Instantiate t) = hsep ["Instantiate", fromPpr t]
  clashPretty (PrimApply p tys vs ts) =
    hsep ["PrimApply", fromPretty (primName p), "::", fromPpr (primType p),
          "; type args=", fromPpr tys,
          "; val args=", fromPpr (map valToTerm vs),
          "term args=", fromPpr ts]
  clashPretty (Scrutinise a b) =
    hsep ["Scrutinise ", fromPpr a,
          fromPpr (Case (Literal (CharLiteral '_')) a b)]
  clashPretty (Tickish sp) =
    hsep ["Tick", fromPpr sp]
  clashPretty (Castish ty1 ty2) =
    hsep ["Cast", fromPpr ty1, fromPpr ty2]

-- Values
data Value
  = Lambda Id Term
  -- ^ Functions
  | TyLambda TyVar Term
  -- ^ Type abstractions
  | DC DataCon [Either Term Type]
  -- ^ Data constructors
  | Lit Literal
  -- ^ Literals
  | PrimVal  PrimInfo [Type] [Value]
  -- ^ Clash's number types are represented by their "fromInteger#" primitive
  -- function. So some primitives are values.
  | Suspend Term
  -- ^ Used by lazy primitives
  | TickValue TickInfo Value
  -- ^ Preserve ticks from Terms in Values
  | CastValue Value Type Type
  -- ^ Preserve casts from Terms in Values
  deriving Show

valToTerm :: Value -> Term
valToTerm v = case v of
  Lambda x e           -> Lam x e
  TyLambda x e         -> TyLam x e
  DC dc pxs            -> foldl' (\e a -> either (App e) (TyApp e) a)
                                 (Data dc) pxs
  Lit l                -> Literal l
  PrimVal ty tys vs    -> foldl' App (foldl' TyApp (Prim ty) tys)
                                 (map valToTerm vs)
  Suspend e            -> e
  TickValue t x        -> Tick t (valToTerm x)
  CastValue x t1 t2    -> Cast (valToTerm x) t1 t2

-- Collect all the ticks from a value, exposing the ticked value.
--
collectValueTicks
  :: Value
  -> (Value, [TickInfo])
collectValueTicks = go []
 where
  go ticks (TickValue t v) = go (t:ticks) v
  go ticks v = (v, ticks)

-- | Are we in a context where special primitives must be forced.
--
-- See [Note: forcing special primitives]
forcePrims :: Machine -> Bool
forcePrims = go . mStack
 where
  go (Scrutinise{}:_) = True
  go (PrimApply{}:_)  = True
  go (Tickish{}:xs)   = go xs
  go _                = False

primCount :: Machine -> Int
primCount = snd . mHeapPrim

primLookup :: Int -> Machine -> Maybe Term
primLookup i = IntMap.lookup i . fst . mHeapPrim

primInsert :: Int -> Term -> Machine -> Machine
primInsert i x m =
  let (gh, c) = mHeapPrim m
   in m { mHeapPrim = (IntMap.insert i x gh, c + 1) }

primUpdate :: Int -> Term -> Machine -> Machine
primUpdate i x m =
  let (gh, c) = mHeapPrim m
   in m { mHeapPrim = (IntMap.insert i x gh, c) }

heapLookup :: IdScope -> Id -> Machine -> Maybe Term
heapLookup GlobalId i m =
  lookupVarEnv i $ mHeapGlobal m
heapLookup LocalId i m =
  lookupVarEnv i $ mHeapLocal m

heapContains :: IdScope -> Id -> Machine -> Bool
heapContains scope i = isJust . heapLookup scope i

heapInsert :: IdScope -> Id -> Term -> Machine -> Machine
heapInsert GlobalId i x m =
  m { mHeapGlobal = extendVarEnv i x (mHeapGlobal m) }
heapInsert LocalId i x m =
  m { mHeapLocal = extendVarEnv i x (mHeapLocal m) }

heapDelete :: IdScope -> Id -> Machine -> Machine
heapDelete GlobalId i m =
  m { mHeapGlobal = delVarEnv (mHeapGlobal m) i }
heapDelete LocalId i m =
  m { mHeapLocal = delVarEnv (mHeapLocal m) i }

stackPush :: StackFrame -> Machine -> Machine
stackPush f m = m { mStack = f : mStack m }

stackPop :: Machine -> Maybe (Machine, StackFrame)
stackPop m = case mStack m of
  [] -> Nothing
  (x:xs) -> Just (m { mStack = xs }, x)

stackClear :: Machine -> Machine
stackClear m = m { mStack = [] }

stackNull :: Machine -> Bool
stackNull = null . mStack

getTerm :: Machine -> Term
getTerm = mTerm

setTerm :: Term -> Machine -> Machine
setTerm x m = m { mTerm = x }

