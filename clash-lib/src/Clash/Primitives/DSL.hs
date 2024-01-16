{-|
  Copyright   :  (C) 2019,      Myrtle Software Ltd.
                     2020-2023, QBayLogic B.V.
                     2021,      Myrtle.ai
                     2022-2023, Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

This module contains a mini dsl for creating haskell blackbox
instantiations.
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Primitives.DSL
  (
  -- * Annotations
    BlackBoxHaskellOpts(..)
  , blackBoxHaskell

  -- * Declarations
  , BlockState (..)
  , TExpr(..)
  , addDeclaration
  , assign
  , compInBlock
  , declaration
  , declarationReturn
  , declare
  , declareN
  , instDecl
  , instHO
  , viaAnnotatedSignal

  -- ** Literals
  , bvLit
  , LitHDL (..)
  , pattern High
  , pattern Low
  , constructProduct
  , tuple
  , vec

  -- ** Extraction
  , tInputs
  , tResults
  , getStr
  , getBool
  , getVec
  , exprToInteger
  , tExprToInteger
  , deconstructProduct
  , untuple
  , unvec
  , deconstructMaybe

  -- ** Conversion
  , bitCoerce
  , toBV
  , toBvWithAttrs
  , fromBV
  , enableToBit
  , boolToBit
  , boolFromBit
  , boolFromBitVector
  , unsignedFromBitVector
  , boolFromBits

  , unsafeToActiveHigh
  , unsafeToActiveLow

  -- ** Operations
  , andExpr
  , notExpr
  , pureToBV
  , pureToBVResized
  , open

  -- ** Utilities
  , clog2
  , litTExpr
  , toIdentifier
  , tySize
  ) where

import           Control.Lens                    hiding (Indexed, assign)
#if MIN_VERSION_mtl(2,3,0)
import           Control.Monad                   (forM, forM_, zipWithM)
#endif
import           Control.Monad.State
import           Data.Default                    (Default(def))
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as IntMap
import           Data.List                       (intersperse)
import           Data.List.Extra                 (zipEqual)
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     (Ap(getAp))
import           Data.Semigroup                  hiding (Product)
import           Data.String
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Text.Extra                 (showt)
import           Data.Text.Prettyprint.Doc.Extra
import           GHC.Stack                       (HasCallStack)

import           Clash.Annotations.Primitive     (HDL (..), Primitive (..))
import           Clash.Annotations.SynthesisAttributes (Attr)
import           Clash.Backend                   hiding (Usage, fromBV, toBV)
import           Clash.Backend.VHDL              (VHDLState)
import           Clash.Explicit.Signal           (ResetPolarity(..), vResetPolarity)
import           Clash.Netlist.BlackBox.Util     (exprToString, getDomainConf, renderElem)
import           Clash.Netlist.BlackBox.Types
  (BlackBoxTemplate, Element(Component, Text), Decl(..))
import qualified Clash.Netlist.Id                as Id
import           Clash.Netlist.Types             hiding (Component, toBit)
import           Clash.Netlist.Util
import           Clash.Util                      (clogBase)
import qualified Data.String.Interpolate         as I
import           Language.Haskell.TH             (Name)
import           Prelude

-- | Options for 'blackBoxHaskell' function. Use 'def' from package
-- 'data-default' for a set of default options.
data BlackBoxHaskellOpts = BlackBoxHaskellOpts
  { -- | Arguments to ignore (i.e., remove during normalization)
    --
    -- Default: []
    bo_ignoredArguments :: [Int]

    -- | HDLs to use the blackbox for
    --
    -- Default: all
  , bo_supportedHdls :: [HDL]

    -- | Does this blackbox assign its results to multiple binders?
    --
    -- Default: False.
  , bo_multiResult :: Bool
  }

instance Default BlackBoxHaskellOpts where
  def = BlackBoxHaskellOpts
    { bo_ignoredArguments = []
    , bo_supportedHdls = [minBound..maxBound]
    , bo_multiResult = False
    }

-- | Create a blackBoxHaskell primitive. To be used as part of an annotation:
--
-- @
-- {-\# ANN myFunction (blackBoxHaskell 'myFunction 'myBBF def{bo_ignoredArguments=[1,2]}) \#-}
-- @
--
-- @[1,2]@ would mean this blackbox __ignores__ its second and third argument.
blackBoxHaskell
  :: Name
  -- ^ blackbox name
  -> Name
  -- ^ template function name
  -> BlackBoxHaskellOpts
  -- ^ Options, see data structure for more information
  -> Primitive
blackBoxHaskell bb tf BlackBoxHaskellOpts{..} =
  InlineYamlPrimitive bo_supportedHdls [I.__i|
    BlackBoxHaskell:
      name: #{bb}
      templateFunction: #{tf}
      ignoredArguments : #{bo_ignoredArguments}
      multiResult : #{toYamlBool bo_multiResult}
    |]
 where
  toYamlBool :: Bool -> String
  toYamlBool True = "true"
  toYamlBool False = "false"

-- | The state of a block. Contains a list of declarations and a the
--   backend state.
data BlockState backend = BlockState
  { _bsDeclarations :: [Declaration]
    -- ^ Declarations store
  , _bsHigherOrderCalls :: IntMap Int
    -- ^ Tracks how many times a higher order function has been instantiated.
    -- Needed to fill in the second field of 'Clash.Netlist.BlackBox.Types.Decl'
  , _bsBackend :: backend
    -- ^ Backend state
  }
makeLenses ''BlockState

instance Backend backend => HasIdentifierSet (BlockState backend) where
  identifierSet :: Lens' (BlockState backend) IdentifierSet
  identifierSet = bsBackend . identifierSet

instance HasUsageMap backend => HasUsageMap (BlockState backend) where
  usageMap = bsBackend.usageMap

liftToBlockState
  :: forall backend a. Backend backend
   => State backend a -> State (BlockState backend) a
liftToBlockState (StateT f) = StateT g
 where
  g :: BlockState backend -> Identity (a, BlockState backend)
  g sbsIn = do
    let sIn = _bsBackend sbsIn
    (res,sOut) <- f sIn
    pure (res, sbsIn{_bsBackend = sOut})

-- | A typed expression.
data TExpr = TExpr
  { ety :: HWType
  , eex :: Expr
  } deriving Show
makeLenses ''TExpr

-- | Run a block declaration. Assign the result of the block builder to the
-- result variable in the given blackbox context.
declarationReturn
  :: Backend backend
  => BlackBoxContext
  -> Text.Text
  -- ^ block name
  -> State (BlockState backend) [TExpr]
  -- ^ block builder yielding an expression that should be assigned to the
  -- result variable in the blackbox context
  -> State backend Doc
  -- ^ pretty printed block
declarationReturn bbCtx blockName blockBuilder =
  declaration blockName $ do
    res <- blockBuilder
    forM_ (zip (bbResults bbCtx) res) $ \(rNm, r) -> case rNm of
      (Identifier resultNm Nothing, _) ->
        addDeclaration (Assignment resultNm Cont (eex r))
      (t,_) -> error ("declarationReturn expected an Identifier, but got: " <> show t)


emptyBlockState :: backend -> BlockState backend
emptyBlockState bck = BlockState
  { _bsDeclarations = []
  , _bsHigherOrderCalls = IntMap.empty
  , _bsBackend = bck
  }

-- | Run a block declaration.
declaration
  :: Backend backend
  => Text.Text
  -- ^ block name
  -> State (BlockState backend) ()
  -- ^ block builder
  -> State backend Doc
  -- ^ pretty printed block
declaration blockName c = do
  backend0 <- get
  let initState = emptyBlockState backend0
      (BlockState {..}) = execState c initState
  put _bsBackend
  blockNameUnique <- Id.makeBasic blockName
  getAp $ blockDecl blockNameUnique (reverse _bsDeclarations)

-- | Add a declaration to the state.
addDeclaration :: Declaration -> State (BlockState backend) ()
addDeclaration dec = bsDeclarations %= cons dec

-- | Declare a new signal with the given name and type.
declare'
  :: Backend backend
  => Text
  -- ^ Name hint
  -> HWType
  -- ^ Type of new signal
  -> State (BlockState backend) Identifier
  -- ^ Expression pointing the the new signal
declare' decName ty = do
  uniqueName <- Id.makeBasic decName
  addDeclaration (NetDecl' Nothing uniqueName ty Nothing)
  pure uniqueName

-- | Declare a new signal with the given name and type.
declare
  :: Backend backend
  => Text
  -- ^ Name hint
  -> HWType
  -- ^ Type of new signal
  -> State (BlockState backend) TExpr
  -- ^ Expression pointing the the new signal
declare decName ty = do
  uniqueName <- declare' decName ty
  pure (TExpr ty (Identifier uniqueName Nothing))

-- | Declare /n/ new signals with the given type and based on the given name
declareN
  :: Backend backend
  => Text
  -- ^ Name hint
  -> [HWType]
  -- ^ Types of the signals
  -> State (BlockState backend) [TExpr]
  -- ^ Expressions pointing the the new signals
declareN decName tys = do
  firstName <- Id.makeBasic decName
  nextNames <- Id.nextN (length tys - 1) firstName
  let uniqueNames = firstName : nextNames
  zipWithM
    (\uniqueName ty -> do
      addDeclaration $ NetDecl' Nothing uniqueName ty Nothing
      pure $ TExpr ty (Identifier uniqueName Nothing)
    ) uniqueNames tys

-- | Assign an expression to an identifier, returns the new typed
--   identifier expression.
assign
  :: Backend backend
  => Text
  -- ^ Name hint for assignment
  -> TExpr
  -- ^ expression to be assigned to freshly generated identifier
  -> State (BlockState backend) TExpr
  -- ^ the identifier of the expression that actually got assigned
assign aName (TExpr ty aExpr) = do
  texp <- declare aName ty
  let uniqueName = case texp of
        TExpr _ (Identifier x Nothing) -> x
        t' -> error ("assign expected an Identifier, but got: " <> show t')

  addDeclaration (Assignment uniqueName Cont aExpr)
  pure texp

-- | Extract the elements of a vector expression and return expressions
-- to them. If given expression is not an identifier, an intermediate variable
-- will be used to assign the given expression to which is subsequently indexed.
unvec
  :: (HasCallStack, Backend backend)
  => Text
  -- ^ Name hint for intermediate signal
  -> TExpr
  -- ^ Vector expression
  -> State (BlockState backend) [TExpr]
  -- ^ Vector elements
unvec vName v@(ety -> Vector vSize eType) = do
  texp <- toIdentifier vName v
  let vUniqueName = case texp of
        TExpr _ (Identifier x Nothing) -> x
        t' -> error ("unvec expected an Identifier, but got: " <> show t')

  let vIndex i = Identifier vUniqueName (Just (Indexed (ety v, 10, i)))
  pure (map (TExpr eType . vIndex) [0..vSize-1])
unvec _ e = error $ "unvec: cannot be called on non-vector: " <> show (ety e)

-- | Deconstruct a 'Maybe' into its constructor 'Bit' and contents of its 'Just'
-- field. Note that the contents might be undefined, if the constructor bit is
-- set to 'Nothing'.
deconstructMaybe ::
  (HasCallStack, Backend backend) =>
  -- | Maybe expression
  TExpr ->
  -- | Name hint for constructor bit, data
  (Text, Text) ->
  -- | Constructor represented as a Bit, contents of Just
  State (BlockState backend) (TExpr, TExpr)
deconstructMaybe e@TExpr{ety} (bitName, contentName)
  | SP tyName [(_nothing, []),(_just, [aTy])] <- ety
  , tyName == fromString (show ''Maybe)
  = do
    eBv <- toBV (bitName <> "_and_" <> contentName <> "_bv") e
    eId <- toIdentifier' (bitName <> "_and_" <> contentName) eBv
    let eSize = typeSize ety

    bitExpr <- fromBV bitName Bit TExpr
      { eex = Identifier eId (Just (Sliced (BitVector eSize, eSize - 1, eSize - 1)))
      , ety = BitVector 1
      }

    contentExpr <- fromBV contentName aTy TExpr
      { eex = Identifier eId (Just (Sliced (BitVector eSize, eSize - 1 - 1, 0)))
      , ety = BitVector (eSize - 1)
      }

    pure (bitExpr, contentExpr)

deconstructMaybe e _ =
  error $ "deconstructMaybe: cannot be called on non-Maybe: " <> show (ety e)

-- | Extract the fields of a product type and return expressions
--   to them. These new expressions are given unique names and get
--   declared in the block scope.
deconstructProduct
  :: (HasCallStack, Backend backend)
  => TExpr
  -- ^ Product expression
  -> [Text]
  -- ^ Name hints for element assignments
  -> State (BlockState backend) [TExpr]
deconstructProduct (TExpr ty@(Product _ _ fieldTys) (Identifier resName Nothing)) nameHints =
  forM (zip3 [0..] nameHints fieldTys) $ \(fieldIndex, nameHint, fieldTy) ->
    assign nameHint $
      TExpr fieldTy (Identifier resName (Just (Indexed (ty, 0, fieldIndex))))

deconstructProduct t0@(TExpr (Product {}) _) nameHints = do
  t1 <- toIdentifier "product" t0
  deconstructProduct t1 nameHints

deconstructProduct e i =
  error $ "deconstructProduct: " <> show e <> " " <> show i

-- | Extract the elements of a tuple expression and return expressions
--   to them. These new expressions are given unique names and get
--   declared in the block scope.
untuple
  :: (HasCallStack, Backend backend)
  => TExpr
  -- ^ Tuple expression
  -> [Text]
  -- ^ Name hints for element assignments
  -> State (BlockState backend) [TExpr]
untuple = deconstructProduct

-- | The high literal bit.
pattern High :: TExpr
pattern High <- TExpr Bit (Literal _ (BitLit H))
  where High = TExpr Bit (Literal (Just (Bit,1)) (BitLit H))

-- | The low literal bit.
pattern Low :: TExpr
pattern Low <- TExpr Bit (Literal _ (BitLit L))
  where Low = TExpr Bit (Literal (Just (Bit,1)) (BitLit L))

-- | The true literal bool.
pattern T :: TExpr
pattern T <- TExpr Bool (Literal _ (BoolLit True))
  where T = TExpr Bool (Literal (Just (Bool,1)) (BoolLit True))

-- | The false literal bool.
pattern F :: TExpr
pattern F <- TExpr Bool (Literal _ (BoolLit False))
  where F = TExpr Bool (Literal (Just (Bool,1)) (BoolLit False))

-- | Construct a fully defined BitVector literal
bvLit
  :: Int
  -- ^ BitVector size
  -> Integer
  -- ^ Literal
  -> TExpr
bvLit sz n =
  TExpr
    (BitVector sz)
    (Literal (Just (BitVector sz, sz)) (BitVecLit 0 n))

-- | Convert a bool to a bit.
boolToBit
  :: (HasCallStack, Backend backend)
  => Text
  -- ^ Name hint for intermediate signal
  -> TExpr
  -> State (BlockState backend) TExpr
boolToBit bitName = \case
  T -> pure High
  F -> pure Low
  TExpr Bool boolExpr -> do
    texp <- declare bitName Bit
    let uniqueBitName = case texp of
          TExpr _ (Identifier x Nothing) -> x
          t' -> error ("boolFromBit expected an Identifier, but got: " <> show t')
    addDeclaration $
      CondAssignment uniqueBitName Bit boolExpr Bool
        [ (Just (BoolLit True), Literal Nothing (BitLit H))
        , (Nothing            , Literal Nothing (BitLit L))
        ]
    declareUseOnce (Proc NonBlocking) uniqueBitName
    pure texp
  tExpr -> error $ "boolToBit: Got \"" <> show tExpr <> "\" expected Bool"

-- | Convert an enable to a bit.
enableToBit
  :: (HasCallStack, Backend backend)
  => Text
  -- ^ Name hint for intermediate signal
  -> TExpr
  -> State (BlockState backend) TExpr
enableToBit bitName = \case
  TExpr ena@(Enable _) enableExpr -> do
    texp <- declare bitName Bit
    let uniqueBitName = case texp of
          TExpr _ (Identifier x Nothing) -> x
          t' -> error ("boolFromBit expected an Identifier, but got: " <> show t')
    addDeclaration $
      CondAssignment uniqueBitName Bit enableExpr ena
        -- Enable normalizes to Bool for all current backends
        [ (Just (BoolLit True), Literal Nothing (BitLit H))
        , (Nothing            , Literal Nothing (BitLit L))
        ]
    declareUseOnce (Proc NonBlocking) uniqueBitName
    pure texp
  tExpr -> error $ "enableToBit: Got \"" <> show tExpr <> "\" expected Enable"

-- | Use to create an output `Bool` from a `Bit`. The expression given
--   must be the identifier of the bool you wish to get assigned.
--   Returns a reference to a declared `Bit` that should get assigned
--   by something (usually the output port of an entity).
boolFromBit
  :: (HasCallStack, Backend backend)
  => Text
  -- ^ Name hint for intermediate signal
  -> TExpr
  -> State (BlockState backend) TExpr
boolFromBit boolName = \case
  High -> pure T
  Low -> pure F
  TExpr Bit bitExpr -> do
    texp <- declare boolName Bool
    let uniqueBoolName = case texp of
          TExpr _ (Identifier x Nothing) -> x
          t' -> error ("boolFromBit expected an Identifier, but got: " <> show t')
    addDeclaration $
      CondAssignment uniqueBoolName Bool bitExpr Bit
        [ (Just (BitLit H), Literal Nothing (BoolLit True))
        , (Nothing        , Literal Nothing (BoolLit False))
        ]
    declareUseOnce (Proc NonBlocking) uniqueBoolName
    pure texp
  tExpr -> error $ "boolFromBit: Got \"" <> show tExpr <> "\" expected Bit"

-- | Used to create an output `Bool` from a `BitVector` of given size.
-- Works in a similar way to `boolFromBit` above.
--
-- TODO: Implement for (System)Verilog
boolFromBitVector
  :: Size
  -> Text
  -- ^ Name hint for intermediate signal
  -> TExpr
  -> State (BlockState VHDLState) TExpr
boolFromBitVector n =
  outputCoerce (BitVector n) Bool (\i -> "unsigned(" <> i <> ") > 0")

-- | Used to create an output `Unsigned` from a `BitVector` of given
-- size. Works in a similar way to `boolFromBit` above.
unsignedFromBitVector ::
  (HasCallStack, Backend backend) =>
  -- | Name hint for intermediate signal
  Text ->
  -- | BitVector expression
  TExpr ->
  -- | Unsigned expression
  State (BlockState backend) TExpr
unsignedFromBitVector nameHint e@TExpr{ety=BitVector n} =
  fromBV nameHint (Unsigned n) e
unsignedFromBitVector _nameHint TExpr{ety} =
  error $ "unsignedFromBitVector: Expected BitVector, got: " <> show ety

-- | Used to create an output `Bool` from a number of `Bit`s, using
-- conjunction. Similarly to `untuple`, it returns a list of
-- references to declared values (the inputs to the function) which
-- should get assigned by something---usually output ports of an
-- entity.
--
-- TODO: Implement for (System)Verilog
boolFromBits
  :: [Text]
  -> TExpr
  -> State (BlockState VHDLState) [TExpr]
boolFromBits inNames = outputFn (map (const Bit) inNames) Bool
  (foldl (<>) "" . intersperse " and " . map (\i -> "(" <> i <> " = '1')")) inNames

-- | Used to create an output value with an arbitrary VHDL coercion.
-- The expression given should be the identifier of the output value
-- you wish to get assigned. Returns a reference to a declared value
-- of the input type that should get assigned by something (usually
-- the output port of an entity).
outputCoerce
  :: (HasCallStack, Backend backend)
  => HWType
  -> HWType
  -> (Text -> Text)
  -> Text
  -> TExpr
  -> State (BlockState backend) TExpr
outputCoerce fromType toType exprStringFn inName0 expr_
  | TExpr outType (Identifier outName Nothing) <- expr_
  , outType == toType = do
      inName1 <- Id.makeBasic inName0
      let inName2 = Id.unsafeMake (exprStringFn (Id.toText inName1))
          exprIdent = Identifier inName2 Nothing
      addDeclaration (NetDecl Nothing inName1 fromType)
      addDeclaration (Assignment outName Cont exprIdent)
      pure (TExpr fromType (Identifier inName1 Nothing))
outputCoerce _ toType _ _ texpr = error $ "outputCoerce: the expression " <> show texpr
                                  <> " must be an Identifier with type " <> show toType

-- | Used to create an output value that is an arbitrary function (as
-- VHDL) of existing values. The expression given should be the
-- identifier of the output value you wish to get assigned. Similarly
-- to `untuple`, it returns a list of references to declared values
-- (the inputs to the function) which should get assigned by
-- something---usually output ports of an entity.
outputFn
  :: (HasCallStack, Backend backend)
  => [HWType]
  -> HWType
  -> ([Text] -> Text)
  -> [Text]
  -> TExpr
  -> State (BlockState backend) [TExpr]
outputFn fromTypes toType exprFn inNames0 (TExpr outType (Identifier outName Nothing))
  | outType == toType = do
      inNames1 <- mapM Id.makeBasic inNames0
      let idExpr = Id.unsafeMake (exprFn (map Id.toText inNames1))
          exprIdent = Identifier idExpr Nothing
      sequenceOf_ each [ addDeclaration (NetDecl Nothing nm t)
                       | (nm, t) <- zip inNames1 fromTypes ]
      addDeclaration (Assignment outName Cont exprIdent)
      pure [ TExpr t (Identifier nm Nothing)
           | (nm,t) <- zipEqual inNames1 fromTypes ]
outputFn _ outType _ _ texpr =
  error $ "outputFn: the expression " <> show texpr
  <> " must be an Identifier with type " <> show outType

-- | Create a vector of 'TExpr's
vec
  :: (HasCallStack, Backend backend)
  => [TExpr]
  -- ^ Elements of vector
  -> State (BlockState backend) TExpr
  -- ^ Vector elements
vec els@(el:_)
  | all (\e -> ety e == ety el) els
  = pure (TExpr (Vector (length els) (ety el)) theVec)
  | otherwise
  = error $ "vec: elements not of same type: " ++ show els
 where
  theVec = mkVectorChain (length els) (ety el) (map eex els)
vec [] = error "vec: can't be used on empty lists"

-- | Construct a product type given its type and fields
constructProduct :: HWType -> [TExpr] -> TExpr
constructProduct ty els =
  TExpr ty (DataCon ty (DC (ty,0)) (map eex els))

-- | Create an n-tuple of 'TExpr'
tuple :: HasCallStack => [TExpr] -> TExpr
tuple [] = error $ "nTuple: Cannot create empty tuple"
tuple [_] =
  -- If we don't put this in: tuple . untuple /= id
  error $ "nTuple: Cannot create 1-tuple"
tuple els = constructProduct tupTy els
 where
  commas = Text.replicate (length els - 1) ","
  tupTy = Product ("GHC.Tuple.(" <> commas <> ")") Nothing (map ety els)

-- | Try to get the literal string value of an expression.
getStr :: TExpr -> Maybe String
getStr (TExpr _ e) = exprToString e

-- | Try to get the literal bool value of an expression.
getBool :: TExpr -> Maybe Bool
getBool (TExpr _ (Literal _ (BoolLit b))) = Just b
getBool _ = Nothing

-- | Try to get a Vector of expressions.
getVec :: TExpr -> Maybe [TExpr]
getVec (TExpr (Void (Just (Vector 0 _) )) _) =
  pure []
getVec (TExpr (Vector 1 elementTy) (DataCon _ VecAppend [e])) =
  pure [TExpr elementTy e]
getVec (TExpr (Vector n elementTy) (DataCon _ VecAppend [e, es0])) = do
  es1 <- getVec (TExpr (Vector (n-1) elementTy) es0)
  pure (TExpr elementTy e:es1)
getVec _ = Nothing

-- | Try to get the literal nat value of an expression.
tExprToInteger :: TExpr -> Maybe Integer
tExprToInteger (TExpr _ e) = exprToInteger e

exprToInteger :: Expr -> Maybe Integer
exprToInteger (DataCon _ _ [n]) = exprToInteger n
exprToInteger (Literal _ (NumLit n)) = Just n
exprToInteger _ = Nothing

-- | Convert an expression from one type to another. Errors if result type and
-- given expression are sized differently.
bitCoerce ::
  (HasCallStack, Backend backend) =>
  -- | Name hints for intermediate variables
  Text ->
  -- | Type to convert to
  HWType ->
  -- | Expression to convert
  TExpr ->
  -- | Converted expression
  State (BlockState backend) TExpr
bitCoerce nameHint destType e@(TExpr ety _)
  | tySize ety /= tySize @Int destType = error "Size mismatch"
  | ety == destType = pure e
  | BitVector _ <- ety = fromBV nameHint destType e
  | otherwise = bitCoerce nameHint destType =<< toBV nameHint e

-- | Convert an expression to a BitVector
toBV ::
  Backend backend =>
  -- | BitVector name hint
  Text ->
  -- | Expression to convert to BitVector
  TExpr ->
  -- | BitVector expression
  State (BlockState backend) TExpr
toBV = toBvWithAttrs []

-- | Convert an expression to a BitVector and add the given HDL attributes
toBvWithAttrs ::
  Backend backend =>
  [Attr Text] ->
  -- | BitVector name hint
  Text ->
  -- | Expression to convert to BitVector
  TExpr ->
  -- | BitVector expression
  State (BlockState backend) TExpr
toBvWithAttrs attrs bvName (TExpr aTy aExpr) =
  assign bvName $
    TExpr
      (annotated attrs (BitVector (tySize aTy)))
      (ToBv Nothing aTy aExpr)

-- | Convert an expression from a 'BitVector' into some type. If the expression
-- is 'Annotated', only convert the expression within.
fromBV
  :: (HasCallStack, Backend backend) =>
  -- | Result name hint
  Text ->
  -- | Type to convert to
  HWType ->
  -- | 'BitVector' expression
  TExpr ->
  -- | Converted 'BitVector' expression
  State (BlockState backend) TExpr
fromBV resultName resultType e@TExpr{eex, ety = BitVector _} =
  case resultType of
    BitVector{} -> pure e
    _ -> assign resultName (TExpr resultType (FromBv Nothing resultType eex))
fromBV resultName resultType e@TExpr{ety = Annotated _ bv@(BitVector _)} =
  case resultType of
    BitVector{} -> pure (TExpr bv (eex e))
    _ -> assign resultName (TExpr resultType (FromBv Nothing resultType (eex e)))
fromBV _ _ TExpr{ety} = error $ "fromBV: expected BitVector, got: " <> show ety

clog2 :: Num i => Integer -> i
clog2 = fromIntegral . fromMaybe 0 . clogBase 2

tySize :: Num i => HWType -> i
tySize = fromIntegral . typeSize

-- | A literal that can be used for hdl attributes. It has a `Num` and
--   `IsString` instances for convenience.
data LitHDL
  = B Bool
  | S String
  | I Integer
  deriving Show

instance Num LitHDL where
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined
  fromInteger = I

instance IsString LitHDL where
  fromString = S

-- | Instantiate/call a higher-order function.
instHO
  :: Backend backend
  => BlackBoxContext
  -- ^ BlackBoxContext, used for rendering higher-order function and error
  -- reporting
  -> Int
  -- ^ Position of HO-argument. For example:
  --
  -- > fold :: forall n a . (a -> a -> a) -> Vec (n + 1) a -> a
  --
  -- would have its HO-argument at position 0, while
  --
  -- > iterateI :: forall n a. KnownNat n => (a -> a) -> a -> Vec n a
  --
  -- would have it at position 1.
  -> (HWType, BlackBoxTemplate)
  -- ^ Result type of HO function
  -> [(TExpr, BlackBoxTemplate)]
  -- ^ Arguments and their types
  -> State (BlockState backend) TExpr
  -- ^ Result of the function
instHO bbCtx fPos (resTy, bbResTy) argsWithTypes = do
  let (args0, argTypes) = unzip argsWithTypes
  fSubPos <- fromMaybe 0 . IntMap.lookup fPos <$> use bsHigherOrderCalls
  bsHigherOrderCalls %= IntMap.insert fPos (succ fSubPos)

  -- Create argument identifiers, example: fold_ho3_0_arg0
  let
    ctxName = last (Text.split (=='.') (bbName bbCtx))
    baseArgName = ctxName <> "_" <> "ho" <> showt fPos <> "_" <> showt fSubPos
    argName n = baseArgName <> "_arg" <> showt n
  args1 <- zipWithM (\argN -> toIdentifier' (argName argN)) [(0::Int)..] args0

  let
    args2 = map (pure . Text . Id.toLazyText) args1

  resName <- declare' (ctxName <> "_" <> "ho" <> showt fPos <> "_"
                               <> showt fSubPos <> "_res") resTy
  let res = ([Text (Id.toLazyText resName)], bbResTy)

  -- Render HO argument to plain text
  let component = Component (Decl fPos fSubPos (res:zip args2 argTypes))
  rendered0 <-
    zoom bsBackend (string =<< (renderElem bbCtx component <*> pure 0))

  let
    layout = LayoutOptions (AvailablePerLine 120 0.4)
    rendered1 = renderLazy (layoutPretty layout rendered0)

  addDeclaration $
    BlackBoxD
      ("__INST_" <> bbName bbCtx <> "_BB_INTERNAL__") [] [] []
      (BBTemplate [Text rendered1])
      (emptyBBContext ("__INST_" <> bbName bbCtx <> "_BB_INTERNAL__"))

  pure (TExpr resTy (Identifier resName Nothing))

-- | This creates a component declaration (for VHDL) given in and out port
-- names, updating the 'BlockState backend' stored in the 'State' monad.
--
-- A typical result is that a
--
-- > component fifo port
-- >    ( rst : in std_logic
-- >    ...
-- >    ; full : out std_logic
-- >    ; empty : out std_logic );
-- >  end component;
--
-- declaration would be added in the appropriate place.
compInBlock
  :: forall backend
   . Backend backend
  => Text
  -- ^ Component name
  -> [(Text, HWType)]
  -- ^ in ports
  -> [(Text, HWType)]
  -- ^ out ports
  -> State (BlockState backend) ()
compInBlock compName inPorts0 outPorts0 =
  addDeclaration (CompDecl compName (inPorts1 ++ outPorts1))
 where
  mkPort inOut (nm, ty) = (nm, inOut, ty)
  inPorts1 = mkPort In <$> inPorts0
  outPorts1 = mkPort Out <$> outPorts0

-- | Convert a 'LitHDL' to a 'TExpr'
--
-- __N.B.__: Clash 1.8 changed 'instDecl'\'s type signature. Where it would
--           previously accept 'LitHDL' in its generics/parameters argument, it
--           now accepts a 'TExpr'. This function is mostly there to ease this
--           transition.
litTExpr :: LitHDL -> TExpr
litTExpr (B b) = TExpr Bool    (Literal Nothing (BoolLit b))
litTExpr (S s) = TExpr String  (Literal Nothing (StringLit s))
litTExpr (I i) = TExpr Integer (Literal Nothing (NumLit i))

-- | Instantiate a component/entity in a block state
instDecl
  :: forall backend
   . Backend backend
  => EntityOrComponent
  -- ^ Type of instantiation
  -> Identifier
  -- ^ Component/entity name
  -> Identifier
  -- ^ Instantiation label
  -> [(Text, TExpr)]
  -- ^ Generics / parameters
  -> [(Text, TExpr)]
  -- ^ In ports
  -> [(Text, TExpr)]
  -- ^ Out ports
  -> State (BlockState backend) ()
instDecl entOrComp compName instLbl params inPorts outPorts = do

  inPorts' <- mapM (mkPort In) inPorts
  outPorts' <- mapM (mkPort Out) outPorts

  addDeclaration $
    InstDecl
      entOrComp Nothing [] compName instLbl (mkParams params)
      (NamedPortMap (inPorts' ++ outPorts'))
   where
    mkPort
      :: PortDirection
      -> (Text, TExpr)
      -> StateT (BlockState backend) Identity (Expr, PortDirection, HWType, Expr)
    mkPort inOrOut (nmText, pExpr) = do
      TExpr ty pExpr' <- toIdentifier (nmText <> "_port")  pExpr
      pure (Identifier (Id.unsafeMake nmText) Nothing, inOrOut, ty, pExpr')

    -- Convert a list of name generics / parameters to the form clash wants
    mkParams :: [(Text.Text, TExpr)] -> [(Expr, HWType, Expr)]
    mkParams = map $ \(paramName, texpr) ->
      ( Identifier (Id.unsafeMake paramName) Nothing
      , ety texpr
      , eex texpr )

-- | Wires the two given `TExpr`s together using a newly declared
-- signal with (exactly) the given name `sigNm`. The new signal has an
-- annotated type, using the given attributes.
viaAnnotatedSignal
  :: (HasCallStack, Backend backend)
  => Identifier
  -- ^ Name given to signal
  -> TExpr
  -- ^ expression the signal is assigned to
  -> TExpr
  -- ^ expression (must be identifier) to which the signal is assigned
  -> [Attr Text]
  -- ^ the attributes to annotate the signal with
  -> State (BlockState backend) ()
viaAnnotatedSignal sigNm (TExpr fromTy fromExpr) (TExpr toTy (Identifier outNm Nothing)) attrs
  | fromTy == toTy = do
      addDeclaration (NetDecl Nothing sigNm (Annotated attrs fromTy))
      addDeclaration (Assignment sigNm Cont fromExpr)
      addDeclaration (Assignment outNm Cont (Identifier sigNm Nothing))
viaAnnotatedSignal _ inTExpr outTExpr@(TExpr _ (Identifier _ _)) _ =
  error $ "viaAnnotatedSignal: The in and out expressions \"" <> show inTExpr <>
  "\" and \"" <> show outTExpr <> "\" have non-matching types."
viaAnnotatedSignal _ _ outTExpr _ =
  error $ "viaAnnotatedSignal: The out expression \"" <> show outTExpr <>
  "\" must be an Identifier."

-- | The TExp inputs from a blackbox context.
tInputs :: BlackBoxContext -> [(TExpr, HWType)]
tInputs = map (\(x, t, _) -> (TExpr t x, t)) . bbInputs

-- | The TExp result of a blackbox context.
tResults :: BlackBoxContext -> [TExpr]
tResults = map (\(x,t) -> TExpr t x) . bbResults

-- | Get an identifier to an expression, creating a new assignment if
--   necessary.
toIdentifier'
  :: Backend backend
  => Text
  -- ^ desired new identifier name, will be made unique
  -> TExpr
  -- ^ expression to get identifier of
  -> State (BlockState backend) Identifier
  -- ^ identifier to expression
toIdentifier' _ (TExpr _ (Identifier aExpr Nothing)) = pure aExpr
toIdentifier' nm texp = do
  t <- assign nm texp
  let nm' = case t of
              TExpr _ (Identifier x Nothing) -> x
              t' -> error ("toIdentifier' expected an Identifier, but got: " <> show t')
  pure nm'

-- | Get an identifier to an expression, creating a new assignment if
--   necessary.
toIdentifier
  :: Backend backend
  => Text
  -- ^ desired new identifier name, will be made unique
  -> TExpr
  -- ^ expression to get identifier of
  -> State (BlockState backend) TExpr
  -- ^ identifier to expression
toIdentifier nm texp = do
  id' <- toIdentifier' nm texp
  pure (TExpr (ety texp) (Identifier id' Nothing))

-- | And together @(&&)@ two expressions, assigning it to a new identifier.
andExpr
  :: Backend backend
  => Text
  -- ^ name hint
  -> TExpr
  -- ^ a
  -> TExpr
  -- ^ a
  -> State (BlockState backend) TExpr
  -- ^ a && b
andExpr _ T bExpr = pure bExpr
andExpr _ F _     = pure F
andExpr _ aExpr T = pure aExpr
andExpr _ _ F     = pure F
andExpr nm a b = do
  aIdent <- Id.toText <$> toIdentifier' (nm <> "_a") a
  bIdent <- Id.toText <$> toIdentifier' (nm <> "_b") b
  -- This is somewhat hacky and relies on the fact that clash doesn't
  -- postprocess the text in Identifier. The alternative is to run
  -- this as a fully fledged @BlackBoxE@ but that involves a lot of
  -- faffing. It should be reasonably safe because we assign each side
  -- to an identifier if it isn't already.
  andTxt <-
    uses bsBackend hdlKind <&> \case
      VHDL          -> aIdent <> " and " <> bIdent
      Verilog       -> aIdent <> " && " <> bIdent
      SystemVerilog -> aIdent <> " && " <> bIdent
  assign nm $ TExpr Bool (Identifier (Id.unsafeMake andTxt) Nothing)

-- | Massage a reset to work as active-high reset.
unsafeToActiveHigh
  :: Backend backend
  => Text
  -- ^ Name hint
  -> TExpr
  -- ^ Reset signal
  -> State (BlockState backend) TExpr
unsafeToActiveHigh nm rExpr = do
  resetLevel <- vResetPolarity <$> liftToBlockState (getDomainConf (ety rExpr))
  case resetLevel of
    ActiveHigh -> pure rExpr
    ActiveLow -> notExpr nm rExpr

-- | Massage a reset to work as active-low reset.
unsafeToActiveLow
  :: Backend backend
  => Text
  -- ^ Name hint
  -> TExpr
  -- ^ Reset signal
  -> State (BlockState backend) TExpr
unsafeToActiveLow nm rExpr = do
  resetLevel <- vResetPolarity <$> liftToBlockState (getDomainConf (ety rExpr))
  case resetLevel of
    ActiveLow -> pure rExpr
    ActiveHigh -> notExpr nm rExpr

-- | Negate @(not)@ an expression, assigning it to a new identifier.
notExpr
  :: Backend backend
  => Text
  -- ^ name hint
  -> TExpr
  -- ^ @a@
  -> State (BlockState backend) TExpr
  -- ^ @not a@
notExpr _ T = pure F
notExpr _ F = pure T
notExpr nm aExpr = do
  aIdent <- Id.toText <$> toIdentifier' (nm <> "_a") aExpr
  -- See disclaimer in `andExpr` above.
  notTxt <- uses bsBackend hdlKind <&> \case
    VHDL          -> "not " <> aIdent
    Verilog       -> "! " <> aIdent
    SystemVerilog -> "! " <> aIdent
  assign nm $ TExpr Bit (Identifier (Id.unsafeMake notTxt) Nothing)

-- | Creates a BV that produces the following vhdl:
--
-- > (0 to n => ARG)
--
-- TODO: Implement for (System)Verilog
pureToBV
  :: Text
  -- ^ name hint
  -> Int
  -- ^ Size (n)
  -> TExpr
  -- ^ @ARG@
  -> State (BlockState VHDLState) TExpr
  -- ^ @(0 to n => ARG)@
pureToBV nm n arg = do
  arg' <- Id.toText <$> toIdentifier' nm arg
  -- This is very hard coded and hacky
  let text = "(0 to " <> showt n <> " => " <> arg' <> ")"
  assign nm $ TExpr (BitVector (n+1)) (Identifier (Id.unsafeMake text) Nothing)

-- | Creates a BV that produces the following vhdl:
--
-- > std_logic_vector(resize(ARG, n))
--
-- TODO: Implement for (System)Verilog
pureToBVResized
  :: Text
  -- ^ name hint
  -> Int
  -- ^ Size (n)
  -> TExpr
  -- ^ ARG
  -> State (BlockState VHDLState) TExpr
  -- ^ @std_logic_vector(resize(ARG, n))@
pureToBVResized nm n arg = do
  arg' <- Id.toText <$> toIdentifier' nm arg
  -- This is very hard coded and hacky
  let text = "std_logic_vector(resize(" <> arg' <> ", " <> showt n <> "))"
  assign nm $ TExpr (BitVector n) (Identifier (Id.unsafeMake text) Nothing)

-- | Allows assignment of a port to be "open"
open
  :: Backend backend
  => HWType
  -> State (BlockState backend) TExpr
open hwType = pure $ TExpr hwType (Identifier (Id.unsafeMake "open") Nothing)
