{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Xpm.Cdc.Internal where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Control.Monad (when)
import Control.Monad.State (State)
import Data.Maybe (maybeToList)
import Data.Either (lefts, partitionEithers, rights)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)

import qualified Control.Lens as Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

import Clash.Annotations.Primitive
import Clash.Core.TermLiteral (TermLiteral(..), termToDataError, deriveTermLiteral)
import Clash.Backend (Backend, hdlKind)
import Clash.Driver.Types (DomainMap, envDomains)
import Clash.Netlist.BlackBox.Types
  ( BlackBoxFunction, BlackBoxMeta(bbKind), TemplateKind(TDecl), emptyBlackBoxMeta
  , bbLibrary, bbImports )
import Clash.Netlist.Types
import Clash.Core.Term ( Term(Data), collectArgs )
import Clash.Core.Name ( Name(Name, nameOcc) )
import Clash.Core.DataCon ( DataCon(MkData, dcName) )
import Clash.Core.Type (Type(..), LitTy (SymTy), splitTyConAppM, isIntegerTy)

import qualified Clash.Netlist.BlackBox.Types as BlackBox
import qualified Clash.Netlist.Id as Id
import qualified Clash.Primitives.DSL as DSL

-- | VHDL generic or Verilog parameter. Contents should be able to render to an
-- HDL constant.
data Param (name :: Symbol) a = Param a

-- | Port in a port mapping
data ClockPort (name :: Symbol) dom = ClockPort (Clock dom)
data ResetPort (name :: Symbol) dom (polarity :: ResetPolarity) = ResetPort (Reset dom)
data EnablePort (name :: Symbol) dom = EnablePort (Enable dom)
data Port (name :: Symbol) dom a = Port (Signal dom a)

data PrimParam a = PrimParam
  { paramName :: Text
  , paramAsInteger :: Maybe Integer
  , paramMeta :: a
  }
  deriving (Show, Functor)

data PrimPort a
  = PrimSignalPort { name :: Text, dom :: Text, meta :: a }
  | PrimClockPort  { name :: Text, dom :: Text, meta :: a }
  | PrimResetPort  { name :: Text, dom :: Text, meta :: a, polarity :: ResetPolarity }
  | PrimEnablePort { name :: Text, dom :: Text, meta :: a }
  deriving (Show, Functor)

addPrimPortMeta :: PrimPort () -> a -> PrimPort a
addPrimPortMeta p a = const a <$> p

data PrimPortOrParam a
  = MkPrimParam (PrimParam a)
  | MkPrimPort (PrimPort a)
  deriving (Show, Functor)

addPrimPortOrParamMeta :: PrimPortOrParam () -> a -> PrimPortOrParam a
addPrimPortOrParamMeta p a = const a <$> p

partitionPortOrPrims :: [PrimPortOrParam a] -> ([PrimParam a], [PrimPort a])
partitionPortOrPrims = partitionEithers . P.map go
 where
  go :: PrimPortOrParam a -> Either (PrimParam a) (PrimPort a)
  go (MkPrimParam p) = Left p
  go (MkPrimPort p) = Right p

primPortOrParmName :: PrimPortOrParam a -> Text
primPortOrParmName = \case
  MkPrimParam p -> paramName p
  MkPrimPort p -> name p

collectDataArgs :: Term -> Maybe (String, [Either Term Type])
collectDataArgs (collectArgs -> (f, args))
  | Data (MkData{dcName=Name{nameOcc}}) <- f
  = Just (Text.unpack nameOcc, args)
  | otherwise
  = Nothing

-- | XXX: Grimy, but helpful instance
instance TermLiteral (PrimPortOrParam ()) where
  termToData (collectDataArgs -> Just (constrName, args))
    | constrName == show 'Param
    , [Right (LitTy (SymTy nm)), Right ty, Left arg] <- args
    = do
      integerParam <-
        if isIntegerTy ty
        then Just <$> termToData arg
        else pure Nothing

      pure (MkPrimParam (PrimParam
        { paramName = Text.pack nm
        , paramMeta = ()
        , paramAsInteger = integerParam
        }))

    | constrName == show 'Port
    , (LitTy (SymTy nm) : LitTy (SymTy domNm) : _) <- rights args
    = pure (MkPrimPort (PrimSignalPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}))

    | constrName == show 'ClockPort
    , (LitTy (SymTy nm) : LitTy (SymTy domNm) : _) <- rights args
    = pure (MkPrimPort (PrimClockPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}))

    | constrName == show 'ResetPort
    = error (constrName <> ppShow args) -- TODO

    | constrName == show 'EnablePort
    = error (constrName <> ppShow args) -- TODO

  termToData t = Left t

-- | Config for 'inst'
data InstConfig = InstConfig
  { compName :: String
  , library :: Maybe String
  , libraryImport :: Maybe String
  }
deriveTermLiteral ''InstConfig

instConfig :: String -> InstConfig
instConfig nm = InstConfig
  { compName = nm
  , library = Nothing
  , libraryImport = Nothing
  }

class Inst a where
  instX :: a

  default instX :: a
  instX = errorX "Internal error: this is not supposed to end up in simulation"

class IsPort a where
  type PortType a
  unPort :: a -> PortType a

instance KnownDomain dom => IsPort (ClockPort name dom) where
  type PortType (ClockPort name dom) = Clock dom
  unPort (ClockPort clk) = clk

instance KnownDomain dom => IsPort (ResetPort name dom polarity) where
  type PortType (ResetPort name dom polarity) = Reset dom
  unPort (ResetPort rst) = rst

instance KnownDomain dom => IsPort (EnablePort name dom) where
  type PortType (EnablePort name dom) = Enable dom
  unPort (EnablePort ena) = ena

instance KnownDomain dom => IsPort (Port name dom a) where
  type PortType (Port name dom a) = Signal dom a
  unPort (Port sig) = sig

instance Inst ()
instance KnownDomain dom => Inst (Port s dom a)
instance KnownDomain dom => Inst (ClockPort s dom)
instance KnownDomain dom => Inst (ResetPort s dom polarity)
instance KnownDomain dom => Inst (EnablePort s dom)
instance (IsPort p0, IsPort p1) => Inst (p0, p1)
instance (IsPort p0, IsPort p1, IsPort p2) => Inst (p0, p1, p2)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3) => Inst (p0, p1, p2, p3)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4) => Inst (p0, p1, p2, p3, p4)

instance Inst a => Inst (Port s dom i -> a) where
  instX !_i = instX @a

instance Inst a => Inst (ClockPort s dom -> a) where
  instX !_i = instX @a

instance Inst a => Inst (ResetPort s polarity dom -> a) where
  instX !_i = instX @a

instance Inst a => Inst (EnablePort s dom -> a) where
  instX !_i = instX @a

instance Inst a => Inst (Param s const -> a) where
  instX !_i = instX @a

inst :: Inst a => InstConfig -> a
inst !_ = instX
{-# CLASH_OPAQUE inst #-}
{-# ANN inst (
    let
      primName = show 'inst
      tfName = show 'instBBF
    in
      InlineYamlPrimitive [minBound..] [__i|
        BlackBoxHaskell:
          name: #{primName}
          templateFunction: #{tfName}
          workInfo: Always
      |]) #-}

argsToPrimPortOrParams :: [Term] -> Either String [PrimPortOrParam ()]
argsToPrimPortOrParams [] = pure []
argsToPrimPortOrParams (t:ts) = do
  arg <- termToDataError t
  args <- argsToPrimPortOrParams ts
  pure (arg:args)

tyToPrimPort :: Type -> Either String [PrimPort ()]
tyToPrimPort (splitTyConAppM -> Just (Name{nameOcc=Text.unpack -> tyConName}, args))
    | tyConName == show 'Param
    , LitTy (SymTy nm) : _ <- args
    = Left ("Can't translate Param " <> nm <> " to PrimPort. Did you define a Param in a result type?")

    | tyConName == show 'Port
    , LitTy (SymTy nm) : LitTy (SymTy domNm) : _ <- args
    = Right [PrimSignalPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}]

    | tyConName == show 'ClockPort
    , LitTy (SymTy nm) : LitTy (SymTy domNm) : _ <- args
    = Right [PrimClockPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}]

    | tyConName == show 'ResetPort
    = error (tyConName <> ppShow args) -- TODO

    | tyConName == show 'EnablePort
    = error (tyConName <> ppShow args) -- TODO
tyToPrimPort ty = error (ppShow ty)

hwtyToPortTypes :: HWType -> [HWType]
hwtyToPortTypes (Annotated _ ty) = hwtyToPortTypes ty
hwtyToPortTypes ty@(Product {}) = error (ppShow ty)
hwtyToPortTypes hwty = [hwty]

instBBF :: HasCallStack => BlackBoxFunction
instBBF _isD _primName args [resTy]
  | _:config:userArgs <- lefts args
  = do
      doms <- Lens.view (clashEnv . Lens.to envDomains)
      SomeBackend b <- Lens.use backend
      let hdl = hdlKind b
      case go config userArgs of
        Left s -> error ("instBBF, bad context:\n\n" <> s)
        Right (c, a, r) -> pure $ Right (bbMeta hdl c, bb doms c a r)
 where
  go :: Term -> [Term] -> Either String (InstConfig, [PrimPortOrParam ()], [PrimPort ()])
  go config0 userArgs = do
    config1 <- termToDataError config0
    argPorts <- argsToPrimPortOrParams userArgs
    resPorts <- tyToPrimPort resTy
    pure (config1, argPorts, resPorts)

  bbMeta :: HDL -> InstConfig -> BlackBoxMeta
  bbMeta hdl InstConfig{..} = emptyBlackBoxMeta
    { bbKind = TDecl
    , bbLibrary = libToBBTemplate hdl library
    , bbImports = libToBBTemplate hdl libraryImport
    }

  libToBBTemplate hdl
    | hdl == VHDL = P.map (pure . BlackBox.Text . LazyText.pack) . maybeToList
    | otherwise = const []

  bb :: DomainMap -> InstConfig -> [PrimPortOrParam ()] -> [PrimPort ()] -> BlackBox
  bb doms config primArgs primRes =
    BBFunction (show 'instTF) 0 (instTF doms config primArgs primRes)

instBBF _ _ args resTys = error $
    "instBBF, bad args:\n\n"
  <> ppShow args
  <> "\n\nor result types:\n\n"
  <> ppShow resTys

instTF ::
  HasCallStack =>
  DomainMap ->
  InstConfig ->
  [PrimPortOrParam ()] ->
  [PrimPort ()] ->
  TemplateFunction
instTF doms config primArgs primRes =
  TemplateFunction
    (usedArguments primArgs)
    (const True)
    (instBBTF doms config primArgs primRes)

usedArguments :: [PrimPortOrParam a] -> [Int]
usedArguments (P.length -> nUserArgs) = 1 : [2..nUserArgs+2]

instBBTF ::
  forall s .
  (Backend s, HasCallStack) =>
  DomainMap ->
  InstConfig ->
  [PrimPortOrParam ()] ->
  [PrimPort ()] ->
  BlackBoxContext ->
  State s Doc
instBBTF doms config primArgs0 primResults0 bbCtx
  | (   _instConstraint
      : _instConfig
      : userArgs
      ) <- P.map fst $ DSL.tInputs bbCtx
  , [resultTy] <- DSL.tResults bbCtx
  , outPortTys <- hwtyToPortTypes (DSL.ety resultTy)
  = do
      DSL.declarationReturn bbCtx "inst_block" $ do
        when (P.length primResults0 /= P.length outPortTys) $
          error "noes"

        when (P.length primArgs0 /= P.length userArgs) $
          error "noes"

        outPorts <- mapM mkOutPort (P.zipWith addPrimPortMeta primResults0 outPortTys)
        let
          primArgs1 = P.zipWith addPrimPortOrParamMeta primArgs0 userArgs
          (params0, inPorts0) = partitionPortOrPrims primArgs1
          inPorts1 = P.map mkInPort inPorts0
          params1 = P.map mkParam params0

        instLabel <- Id.make (Text.pack (compName config) <> "_inst")

        DSL.instDecl
          Empty
          (Id.unsafeMake (Text.pack (compName config)))
          instLabel
          params1
          inPorts1
          outPorts

        case outPorts of
          []  -> pure []
          [p] -> pure [snd p]
          ps  -> pure [DSL.tuple (snd <$> ps)]
 where
  mkParam :: PrimParam DSL.TExpr -> (Text, DSL.TExpr)
  mkParam (PrimParam{paramName, paramMeta, paramAsInteger}) =
    case paramAsInteger of
      Just i -> (paramName, DSL.TExpr Integer (Literal Nothing (NumLit i)))
      Nothing -> (paramName, paramMeta)

  mkInPort :: PrimPort DSL.TExpr -> (Text, DSL.TExpr)
  mkInPort p = (name p, meta p)

  mkOutPort :: PrimPort HWType -> State (DSL.BlockState s) (Text, DSL.TExpr)
  mkOutPort = \case
    PrimResetPort{name, polarity, dom, meta=ty} -> do
      var <- DSL.declare name ty
      rst <-
        case HashMap.lookup dom doms of
          Just (vResetPolarity -> domPolarity)
            | polarity == domPolarity -> pure var
            | otherwise -> DSL.notExpr name var
          Nothing ->
            error ("Internal error: could not find domain " <> Text.unpack dom)
      pure (name, rst)

    port -> do
      let ty = meta port
      var <- DSL.declare (name port) ty
      pure (name port, var)

instBBTF _doms _config _primArgs _primRes ctx =
  error ("instBBTF, bad context:\n\n" <> ppShow ctx)
