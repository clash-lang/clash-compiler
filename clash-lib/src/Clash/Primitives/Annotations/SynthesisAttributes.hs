{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.Annotations.SynthesisAttributes where

import Prelude

import Control.Monad.State (State)
import Data.Either (lefts, rights)
import Data.List.Infinite((...), Infinite((:<)))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (someNatVal)
import GHC.TypeNats (KnownNat, SomeNat(..))
import Text.Show.Pretty (ppShow)

import qualified Control.Lens as Lens
import qualified Data.Text as T

import Clash.Annotations.SynthesisAttributes
import Clash.Backend (Backend)
import Clash.Core.TermLiteral (termToDataErrorM)
import Clash.Core.Type (Type(LitTy), LitTy(NumTy), coreView)
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Types
import Clash.Sized.Vector (Vec, toList)

import qualified Clash.Primitives.DSL as DSL

usedArguments :: [Int]
usedArguments = [attrs, signal]
 where
  attrs :< signal :< _ = (0...)

annotateBBF :: HasCallStack => BlackBoxFunction
annotateBBF _isD _primName args _resTys = do
  tcm <- Lens.view tcCache
  go tcm
 where
  go tcm
    | ((coreView tcm -> LitTy (NumTy n)) : _) <- rights args
    , Just (SomeNat (Proxy :: Proxy n)) <- someNatVal n
    , (attrs0 : _) <- lefts args
    = do
        dataOrError <- termToDataErrorM attrs0
        case dataOrError of
          Left msg -> error msg
          Right attrs1 -> pure $ Right (bbMeta, bb @n (fmap T.pack <$> attrs1))
  go _ = error $ "Unexpected args:\n " <> ppShow args

  bbMeta :: BlackBoxMeta
  bbMeta = emptyBlackBoxMeta{bbKind = TDecl}

  bb :: KnownNat n => Vec n (Attr Text) -> BlackBox
  bb attrs = BBFunction (show 'annotateTF) 0 (annotateTF attrs)

annotateTF :: HasCallStack => KnownNat n => Vec n (Attr Text) -> TemplateFunction
annotateTF attrs = TemplateFunction usedArguments (const True) (annotateBBTF attrs)

annotateBBTF ::
  (Backend s, KnownNat n, HasCallStack) =>
  Vec n (Attr Text) ->
  BlackBoxContext ->
  State s Doc
annotateBBTF attrs0 bbCtx
  | (_attrs : signal0 : _) <- map fst $ DSL.tInputs bbCtx
  = DSL.declarationReturn bbCtx "annotate_block" $ do
      let
        attrs1 = toList attrs0
        signal1ty = Annotated attrs1 (DSL.ety signal0)
        signal1 = DSL.TExpr{DSL.eex=DSL.eex signal0, DSL.ety=signal1ty}
      resultExpr <- DSL.assign (getSignalName (bbCtxName bbCtx)) signal1
      pure [resultExpr]
 where
  -- Return user-friendly name given a context name hint.
  getSignalName :: Maybe T.Text -> T.Text
  getSignalName Nothing = "result"
  getSignalName (Just "__VOID_TDECL_NOOP__") = getSignalName Nothing
  getSignalName (Just s) = s

annotateBBTF _attrs bbCtx = error $ "Unexpected context:\n " <> ppShow bbCtx
