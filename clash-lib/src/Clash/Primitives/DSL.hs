{-|
  Copyright   :  (C) 2019, Myrtle Software Ltd.
                     2020, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

This module contains a mini dsl for creating haskell blackbox
instantiations.
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

  -- * declarations
  , BlockState (..)
  , TExpr
  , declaration
  , declarationReturn
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
  , exprToInteger
  , tExprToInteger
  , deconstructProduct
  , untuple
  , unvec

  -- ** Conversion
  , toBV
  , fromBV
  , boolToBit
  , boolFromBit
  , boolFromBitVector
  , unsignedFromBitVector
  , boolFromBits

  -- ** Operations
  , andExpr
  , notExpr
  , pureToBV
  , pureToBVResized
  , open

  -- ** Utilities
  , toIdentifier
  , tySize
  , clog2
  ) where

import           Clash.Util                      (HasCallStack, clogBase)
import           Control.Lens                    hiding (Indexed, assign)
import           Control.Monad.State
import           Data.Default                    (Default(def))
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as IntMap
import           Data.List                       (intersperse)
import           Data.List.Extra                 (zipEqual)
import           Data.Maybe                      (fromMaybe)
import           Data.Semigroup                  hiding (Product)
import           Data.Semigroup.Monad
import           Data.String
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Text.Prettyprint.Doc.Extra
import           TextShow                        (showt)

import           Clash.Annotations.Primitive     (HDL (..), Primitive (..))
import           Clash.Backend                   hiding (fromBV, toBV)
import           Clash.Backend.VHDL              (VHDLState)
import           Clash.Core.Var                  (Attr')
import           Clash.Netlist.BlackBox.Util     (exprToString, renderElem)
import           Clash.Netlist.BlackBox.Types
  (BlackBoxTemplate, Element(Component, Text), Decl(..))
import qualified Clash.Netlist.Id                as Id
import           Clash.Netlist.Types             hiding (Component, toBit)
import           Clash.Netlist.Util
import qualified Data.String.Interpolate         as I
import           Data.String.Interpolate.Util    (unindent)
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
-- {-# ANN myFunction (blackBoxHaskell 'myFunction 'myBBF def{_ignoredArguments=[2,3]}) #-}
-- @
--
-- [2,3] would mean this blackbox __ignores__ its second and third argument.
blackBoxHaskell
  :: Name
  -- ^ blackbox name
  -> Name
  -- ^ template function name
  -> BlackBoxHaskellOpts
  -- ^ Options, see data structure for more information
  -> Primitive
blackBoxHaskell bb tf BlackBoxHaskellOpts{..} =
  InlinePrimitive bo_supportedHdls $ unindent [I.i|
  [ { "BlackBoxHaskell" :
      { "name" : "#{bb}"
      , "templateFunction" : "#{tf}"
      , "ignoredArguments" : #{show bo_ignoredArguments}
      , "multiResult" : #{toJsonBool bo_multiResult}
      }
    }
  ] |]
 where
  toJsonBool :: Bool -> String
  toJsonBool True = "true"
  toJsonBool False = "false"

-- | The state of a block. Contains a list of declarations and a the
--   backend state.
data BlockState backend = BlockState
  { _bsDeclarations :: [Declaration]
    -- ^ Declarations store
  , _bsHigherOrderCalls :: IntMap Int
    -- ^ Tracks how many times a higher order function has been instantiated.
    -- Needed to fill in the second field of "Clash.Netlist.BlackBox.Types.Decl"
  , _bsBackend :: backend
    -- ^ Backend state
  }
makeLenses ''BlockState

instance Backend backend => HasIdentifierSet (BlockState backend) where
  identifierSet :: Lens' (BlockState backend) IdentifierSet
  identifierSet = bsBackend . identifierSet

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
    forM_ (zip (bbResults bbCtx) res) $ \(rNm, r) -> do
      let (Identifier resultNm Nothing, _) = rNm
      addDeclaration (Assignment resultNm (eex r))

-- | Run a block declaration.
declaration
  :: Backend backend
  => Text.Text
  -- ^ block name
  -> State (BlockState backend) ()
  -- ^ block builder
  -> State backend Doc
  -- ^ pretty printed block
declaration blockName s = do
  backend0 <- get
  let initState = BlockState [] IntMap.empty backend0
      BlockState decs _hoCalls backend1 = execState s initState
  put backend1
  blockNameUnique <- Id.makeBasic blockName
  getMon $ blockDecl blockNameUnique (reverse decs)

-- | Add a declaration to the state.
addDeclaration :: Declaration -> State (BlockState backend) ()
addDeclaration dec = bsDeclarations %= cons dec

-- | Declare a new signal with the given name and type.
declare'
  :: Backend backend
  => Text
  -- ^ Name hint
  -> WireOrReg
  -- ^ Should signal be declared as a wire or a reg
  -> HWType
  -- ^ Type of new signal
  -> State (BlockState backend) Identifier
  -- ^ Expression pointing the the new signal
declare' decName wireOrReg ty = do
  uniqueName <- Id.makeBasic decName
  addDeclaration (NetDecl' Nothing wireOrReg uniqueName (Right ty) Nothing)
  pure uniqueName

-- | Declare a new signal with the given name and type.
declare
  :: Backend backend
  => Text
  -- ^ Name hint
  -> WireOrReg
  -- ^ Should signal be declared as a wire or a reg
  -> HWType
  -- ^ Type of new signal
  -> State (BlockState backend) TExpr
  -- ^ Expression pointing the the new signal
declare decName wireOrReg ty = do
  uniqueName <- declare' decName wireOrReg ty
  pure (TExpr ty (Identifier uniqueName Nothing))

-- | Assign an expression to an identifier, returns the new typed
--   identifier expression.
assign
  :: Backend backend
  => Text
  -- ^ Name hint for assignment
  -> TExpr
  -- ^ expression to be assigned to
  -> State (BlockState backend) TExpr
  -- ^ the identifier of the expression that actually got assigned
assign aName (TExpr ty aExpr) = do
  texp@(~(TExpr _ (Identifier uniqueName Nothing))) <- declare aName Wire ty
  addDeclaration (Assignment uniqueName aExpr)
  pure texp

-- | Extract the elements of a vector expression and return expressions
-- to them. If given expression is not an identifier, an intermediate variable
-- will be used to assign the given expression to which is subsequently indexed.
unvec
  :: Backend backend
  => Text
  -- ^ Name hint for intermediate signal
  -> TExpr
  -- ^ Vector expression
  -> State (BlockState backend) [TExpr]
  -- ^ Vector elements
unvec vName v@(ety -> Vector vSize eType) = do
  ~(TExpr _ (Identifier vUniqueName Nothing)) <- toIdentifier vName v
  let vIndex i = Identifier vUniqueName (Just (Indexed (ety v, 10, i)))
  pure (map (TExpr eType . vIndex) [0..vSize-1])
unvec _ e = error $ "unvec: cannot be called on non-vector: " <> show (ety e)

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
deconstructProduct (TExpr ty@(Product _ _ tys) (Identifier resName _)) vals = do
  newNames <- zipWithM (flip declare Wire) vals tys
  addDeclaration $ Assignment resName $ DataCon ty (DC (ty, 0)) (fmap eex newNames)
  pure newNames
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
    texp@(~(TExpr _ (Identifier uniqueBitName Nothing))) <- declare bitName Wire Bit
    addDeclaration $
      CondAssignment uniqueBitName Bit boolExpr Bool
        [ (Just (BoolLit True), Literal Nothing (BitLit H))
        , (Nothing            , Literal Nothing (BitLit L))
        ]
    pure texp
  tExpr -> error $ "boolToBit: Got \"" <> show tExpr <> "\" expected Bool"

-- | Use to create an output `Bool` from a `Bit`. The expression given
--   must be the identifier of the bool you wish to get assigned.
--   Returns a reference to a declared `Bit` that should get assigned
--   by something (usually the output port of an entity).
boolFromBit
  :: Text
  -- ^ Name hint for intermediate signal
  -> TExpr
  -> State (BlockState VHDLState) TExpr
boolFromBit = outputCoerce Bit Bool (<> " = '1'")

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
--
-- TODO: Implement for (System)Verilog
unsignedFromBitVector
  :: Size
  -> Text
  -- ^ Name hint for intermediate signal
  -> TExpr
  -> State (BlockState VHDLState) TExpr
unsignedFromBitVector n =
  outputCoerce (BitVector n) (Unsigned n) (\i -> "unsigned(" <> i <> ")")

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
      addDeclaration (Assignment outName exprIdent)
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
      addDeclaration (Assignment outName exprIdent)
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
tuple :: [TExpr] -> TExpr
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

-- | Try to get the literal nat value of an expression.
tExprToInteger :: TExpr -> Maybe Integer
tExprToInteger (TExpr _ e) = exprToInteger e

exprToInteger :: Expr -> Maybe Integer
exprToInteger (DataCon _ _ [n]) = exprToInteger n
exprToInteger (Literal _ (NumLit n)) = Just n
exprToInteger _ = Nothing

-- | Assign an input bitvector to an expression. Declares a new
--   bitvector if the expression is not already a bitvector.
toBV
  :: Backend backend
  => Text
  -- ^ BitVector name hint
  -> TExpr
  -- ^ expression
  -> State (BlockState backend) TExpr
  -- ^ BitVector expression
toBV bvName a = case a of
  TExpr BitVector{} _ -> pure a
  TExpr aTy aExpr     -> assign bvName $
    TExpr (BitVector (typeSize aTy)) (ToBv Nothing aTy aExpr)

-- | Assign an output bitvector to an expression. Declares a new
--   bitvector if the expression is not already a bitvector.
fromBV
  :: (HasCallStack, Backend backend)
  => Text
  -- ^ BitVector name hint
  -> TExpr
  -- ^ expression
  -> State (BlockState backend) TExpr
  -- ^ bv expression
fromBV _ a@(TExpr BitVector{} _) = pure a
fromBV bvName (TExpr aTy (Identifier aName Nothing)) = do
  bvName' <- Id.makeBasic bvName
  let bvExpr = FromBv Nothing aTy (Identifier bvName' Nothing)
      bvTy   = BitVector (typeSize aTy)
  addDeclaration (NetDecl Nothing bvName' bvTy)
  addDeclaration (Assignment aName bvExpr)
  pure (TExpr bvTy (Identifier bvName' Nothing))
fromBV _ texpr = error $
  "fromBV: the expression " <> show texpr <> "must be an Identifier"

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
  --   fold :: forall n a . (a -> a -> a) -> Vec (n + 1) a -> a
  --
  -- would have its HO-argument at position 0, while
  --
  --  iterateI :: forall n a. KnownNat n => (a -> a) -> a -> Vec n a
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

    -- Create result identifier
    -- See https://github.com/clash-lang/clash-compiler/issues/919 for info on
    -- logic of 'resWireOrReg'
    resWireOrReg =
      case IntMap.lookup fPos (bbFunctions bbCtx) of
        Just ((_,rw,_,_,_,_):_) -> rw
        _ -> error "internal error"
  resName <- declare' (ctxName <> "_" <> "ho" <> showt fPos <> "_"
                               <> showt fSubPos <> "_res") resWireOrReg resTy
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

-- | Instantiate a component/entity in a block state.
instDecl
  :: forall backend
   . Backend backend
  => EntityOrComponent
  -- ^ Type of instantiation
  -> Identifier
  -- ^ component/entity name
  -> Identifier
  -- ^ instantiation label
  -> [(Text, LitHDL)]
  -- ^ attributes
  -> [(Text, TExpr)]
  -- ^ in ports
  -> [(Text, TExpr)]
  -- ^ out ports
  -> State (BlockState backend) ()
instDecl entOrComp compName instLbl attrs inPorts outPorts = do

  inPorts' <- mapM (mkPort In) inPorts
  outPorts' <- mapM (mkPort Out) outPorts

  addDeclaration $
    InstDecl
      entOrComp Nothing [] compName instLbl (mkAttrs attrs)
      (NamedPortMap (inPorts' ++ outPorts'))
    where
    mkPort
      :: PortDirection
      -> (Text, TExpr)
      -> StateT (BlockState backend) Identity (Expr, PortDirection, HWType, Expr)
    mkPort inOrOut (nmText, pExpr) = do
      TExpr ty pExpr' <- toIdentifier (nmText <> "_port")  pExpr
      pure (Identifier (Id.unsafeMake nmText) Nothing, inOrOut, ty, pExpr')

    -- Convert a list of name attributes to the form clash wants
    mkAttrs :: [(Text.Text, LitHDL)] -> [(Expr, HWType, Expr)]
    mkAttrs = map (\(s, ty) -> ( Identifier (Id.unsafeMake s) Nothing
                               , hdlTy ty, litExpr ty) )

    litExpr :: LitHDL -> Expr
    litExpr (B b) = Literal Nothing (BoolLit b)
    litExpr (S s) = Literal Nothing (StringLit s)
    litExpr (I i) = Literal Nothing (NumLit i)

    hdlTy :: LitHDL -> HWType
    hdlTy = \case
      B{} -> Bool
      S{} -> String
      I{} -> Integer

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
  -> [Attr']
  -- ^ the attributes to annotate the signal with
  -> State (BlockState backend) ()
viaAnnotatedSignal sigNm (TExpr fromTy fromExpr) (TExpr toTy (Identifier outNm Nothing)) attrs
  | fromTy == toTy = do
      addDeclaration (NetDecl Nothing sigNm (Annotated attrs fromTy))
      addDeclaration (Assignment sigNm fromExpr)
      addDeclaration (Assignment outNm (Identifier sigNm Nothing))
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
  ~(TExpr _ (Identifier nm' Nothing)) <- assign nm texp
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

-- | Negate @(not)@ an expression, assigning it to a new identifier.
notExpr
  :: Backend backend
  => Text
  -- ^ name hint
  -> TExpr
  -- ^ a
  -> State (BlockState backend) TExpr
  -- ^ not a
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
-- @
--    (0 to n => ARG)
-- @
--
-- TODO: Implement for (System)Verilog
pureToBV
  :: Text
  -- ^ name hint
  -> Int
  -- ^ Size (n)
  -> TExpr
  -- ^ ARG
  -> State (BlockState VHDLState) TExpr
  -- ^ (0 to n => ARG)
pureToBV nm n arg = do
  arg' <- Id.toText <$> toIdentifier' nm arg
  -- This is very hard coded and hacky
  let text = "(0 to " <> showt n <> " => " <> arg' <> ")"
  assign nm $ TExpr (BitVector (n+1)) (Identifier (Id.unsafeMake text) Nothing)

-- | Creates a BV that produces the following vhdl:
--
-- @
--    std_logic_vector(resize(ARG, Size))
-- @
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
  -- ^ std_logic_vector(resize(ARG, Size))
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
