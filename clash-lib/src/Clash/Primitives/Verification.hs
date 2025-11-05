{-|
  Copyright  :  (C) 2019     , Myrtle Software Ltd
                    2021     , Ellie Hermaszewska
                    2020-2025, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.Verification (checkBBF) where

import Data.Either


import qualified Control.Lens                    as Lens
import           Control.Monad.State             (State)
import           Data.List.Infinite              (Infinite(..), (...))
import           Data.Maybe                      (listToMaybe)
import           Data.Monoid                     (Ap(getAp))
import           Data.Text.Prettyprint.Doc.Extra (Doc)
import           Data.Text                       (Text)
import           GHC.Stack                       (HasCallStack)

import           Clash.Annotations.Primitive     (HDL(..))
import           Clash.Backend
  (Backend, blockDecl, hdlKind)
import           Clash.Core.HasType
import           Clash.Core.Term                 (Term(Var))
import           Clash.Core.TermLiteral          (termToDataError)
import           Clash.Util                      (indexNote)
import           Clash.Netlist                   (mkExpr)
import           Clash.Netlist.Util              (stripVoid, contAssign)
import qualified Clash.Netlist.Id                as Id
import           Clash.Netlist.Types
  (BlackBox(BBFunction), TemplateFunction(..), BlackBoxContext, Identifier,
   NetlistMonad, Declaration(NetDecl),
   HWType(Bool, KnownDomain), NetlistId(..),
   DeclarationType(Concurrent), tcCache, bbInputs, Expr(Identifier))
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(TDecl), RenderVoid(..),
   emptyBlackBoxMeta)

import           Clash.Verification.Internal
import           Clash.Verification.Pretty

checkBBF :: BlackBoxFunction
checkBBF _isD _primName args _ty =
  case litArgs of
    Left err -> pure (Left err)
    Right (propName, renderAs, cvProperty0) -> do
      clkBind <- bindMaybe (Just "clk") (indexNote "clk" (lefts args) clkArg)
      cvProperty1 <- mapM (uncurry bindMaybe) cvProperty0
      let decls = concatMap snd cvProperty1
          cvProperty2 = fmap fst cvProperty1
      pure (Right (meta, bb (checkTF decls clkBind propName renderAs cvProperty2)))
 where
  -- TODO: Improve error handling; currently errors don't indicate what
  -- TODO: blackbox generated them.
  _knownDomainArg
    :< clkArg
    :< _rstArg
    :< propNameArg
    :< renderAsArg
    :< propArg
    :< _ = ((0 :: Int)...)

  litArgs = do
    propName <- termToDataError (indexNote "propName" (lefts args) propNameArg)
    renderAs <- termToDataError (indexNote "renderAs" (lefts args) renderAsArg)
    cvProperty <- termToDataError (indexNote "propArg" (lefts args) propArg)
    Right (propName, renderAs, cvProperty)

  bb = BBFunction "Clash.Primitives.Verification.checkTF" 0
  meta = emptyBlackBoxMeta {bbKind=TDecl, bbRenderVoid=RenderVoid}

  bindMaybe
    :: Maybe Text
    -- ^ Hint for new identifier
    -> Term
    -- ^ Term to bind. Does not bind if it's already a reference to a signal
    -> NetlistMonad (Identifier, [Declaration])
    -- ^ ([new] reference to signal, [declarations need to get it in scope])
  bindMaybe _ (Var vId) = pure (Id.unsafeFromCoreId vId, [])
  bindMaybe Nothing t = bindMaybe (Just "s") t
  bindMaybe (Just nm) t = do
    tcm <- Lens.view tcCache
    newId <- Id.make nm
    (expr0, decls) <- mkExpr False Concurrent (NetlistId newId (inferCoreTypeOf tcm t)) t
    assn <- contAssign newId expr0
    pure
      ( newId
      , decls ++ [sigDecl Bool newId, assn] )

  -- Simple wire without comment
  sigDecl :: HWType -> Identifier -> Declaration
  sigDecl typ nm = NetDecl Nothing nm typ

checkTF
  :: [Declaration]
  -> (Identifier, [Declaration])
  -> Text
  -> RenderAs
  -> Property' Identifier
  -> TemplateFunction
checkTF decls clk propName renderAs prop =
  TemplateFunction [] (const True) (checkTF' decls clk propName renderAs prop)

checkTF'
  :: forall s
   . (HasCallStack, Backend s)
  => [Declaration]
  -- ^ Extra decls needed
  -> (Identifier, [Declaration])
  -- ^ Clock
  -> Text
  -- ^ Prop name
  -> RenderAs
  -> Property' Identifier
  -> BlackBoxContext
  -> State s Doc
checkTF' decls (clkId, clkDecls) propName renderAs prop bbCtx = do
  blockName <- Id.makeBasic (propName <> "_block")
  getAp (blockDecl blockName (clkDecls <> (renderedPslProperty : decls)))

 where
  hdl = hdlKind (undefined :: s)
  clk = Identifier clkId Nothing

  edge =
    case bbInputs bbCtx of
      (_, stripVoid -> KnownDomain _nm _period e _rst _init _polarity, _):_ -> e
      _ -> error $ "Unexpected first argument: " ++ show (listToMaybe (bbInputs bbCtx))

  renderedPslProperty = case renderAs of
    PSL          -> psl
    SVA          -> sva
    AutoRenderAs -> case hdl of
      SystemVerilog -> sva
      _             -> psl
    YosysFormal -> case hdl of
      VHDL -> psl
      _    -> ysva

   where
    sva = pprSvaProperty propName (Id.toText clkId) edge (fmap Id.toText prop)
    ysva = pprYosysSvaProperty propName clk edge (fmap Id.toText prop)
    psl = pprPslProperty hdl propName (Id.toText clkId) edge (fmap Id.toText prop)
