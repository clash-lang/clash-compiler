{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2021-2022, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  PrettyPrec printing class and instances for CoreHW
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Core.Pretty
  ( PrettyPrec (..)
  , PrettyOptions (..)
  , ClashDoc
  , ClashAnnotation (..)
  , SubtermPos (..)
  , SyntaxElement (..)
  , ppr, ppr'
  , showPpr, showPpr'
  , showPprDiag
  , tracePprId
  , tracePpr
  , fromPpr
  , unsafeLookupEnvBool
  )
where

import Control.DeepSeq                  (NFData)
import Data.Binary                      (Binary)
import Data.Default                     (Default(..))
import Data.Text                        (Text)
import Control.Monad.Identity
import GHC.Float                        (castWord32ToFloat, castWord64ToDouble)
import GHC.Generics                     (Generic)
import qualified Data.Text              as T
import Data.Maybe                       (fromMaybe)
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Prettyprinter.Internal
#else
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal
#endif
import GHC.Show                         (showMultiLineString)
import GHC.Stack                        (HasCallStack)
import qualified GHC.Utils.Outputable   as GHC
import System.Environment               (lookupEnv)
import System.IO.Unsafe                 (unsafePerformIO)
import Text.Read                        (readMaybe)

import Clash.Core.Basic (LitTy (..))
import Clash.Core.DataCon               (DataCon (..))
import Clash.Core.Literal               (Literal (..))
import Clash.Core.Name                  (Name (..))
import Clash.Debug                      (trace)
import Clash.Util
import qualified Clash.Util.Interpolate as I
import Clash.Pretty

unsafeLookupEnvBool :: HasCallStack =>  String -> Bool -> Bool
unsafeLookupEnvBool key dflt =
  case unsafePerformIO (lookupEnv key) of
    Nothing -> dflt
    Just a -> flip fromMaybe (readMaybe a) $ error [I.i|
      'unsafeLookupEnvBool' tried to lookup #{key} in the environment. It found
      it, but couldn't interpret it to as a Bool. Expected one of: True, False.
      But found:

        #{a}
    |]

-- | Options for the pretty-printer, controlling which elements to hide.
data PrettyOptions = PrettyOptions
  { displayUniques    :: Bool
  -- ^ whether to display unique identifiers
  , displayTypes      :: Bool
  -- ^ whether to display type information
  , displayQualifiers :: Bool
  -- ^ whether to display module qualifiers
  , displayTicks      :: Bool
  -- ^ whether to display ticks
  }

instance Default PrettyOptions where
  def = PrettyOptions
    { displayUniques    = unsafeLookupEnvBool "CLASH_PPR_UNIQUES" True
    , displayTypes      = unsafeLookupEnvBool "CLASH_PPR_TYPES" True
    , displayQualifiers = unsafeLookupEnvBool "CLASH_PPR_QUALIFIERS" True
    , displayTicks      = unsafeLookupEnvBool "CLASH_PPR_TICKS" True
    }

-- | Annotations carried on pretty-printed code.
data ClashAnnotation
  = AnnSubterm SubtermPos
  -- ^ marking navigation to a subterm, in the printed terms of scoped Core
  -- (see "Clash.Core.Scoped.Pretty")
  | AnnSyntax  SyntaxElement
  -- ^ marking a specific sort of syntax
  deriving Eq

-- | The position of a subterm in its parent term. It marks navigation to the
-- subterm in the printed terms of scoped Core ("Clash.Core.Scoped.Pretty").
-- It holds no binders, which are scoped: the subterm is identified by its
-- position alone.
data SubtermPos
  = PosAppFun
  -- ^ Function of an application
  | PosAppArg !(Maybe (Text, Int, Int))
  -- ^ Argument of an application. If the argument is an applied primitive:
  -- the name of the primitive, and the numbers of type and term arguments it
  -- is applied to, see 'Clash.Core.Scoped.Term.primArg'.
  | PosTyAppFun
  -- ^ Function of a type application
  | PosLetRhs !Int
  -- ^ The right-hand side of a let-binding with the given index, counting
  -- from 0. A non-recursive let-binding has one.
  | PosLetBody
  -- ^ Body of a let-expression
  | PosLamBody
  -- ^ Body of a lambda
  | PosTyLamBody
  -- ^ Body of a type lambda
  | PosCaseScrut
  -- ^ Subject of a case-expression
  | PosCaseAlt !Int
  -- ^ The right-hand side of the case alternative with the given index,
  -- counting from 0
  | PosCastBody
  -- ^ Body of a cast
  | PosTickBody
  -- ^ Body of a tick
  deriving (Eq, Show, Generic, NFData, Binary)

-- | Specific places in the program syntax.
data SyntaxElement = Keyword | LitS | Type | Unique | Qualifier | Ticky
  deriving (Eq, Show)

-- | Clash's specialized @Doc@ type holds metadata of type @ClashAnnotation@.
type ClashDoc = Doc ClashAnnotation

-- | PrettyPrec printing Show-like typeclass
class PrettyPrec p where

  -- default pretty-printing without hiding
  pprPrec :: Monad m => Rational -> p -> m ClashDoc

  -- pretty-printing with hiding options
  -- NB: we utilise the syntax annotations to hide the requested parts of syntax
  pprPrec' :: Monad m => PrettyOptions -> Rational -> p -> m ClashDoc
  pprPrec' opts p = fmap hide . pprPrec p
    where
      hide = \case
        FlatAlt d d'         -> FlatAlt (hide d) (hide d')
        Cat d d'             -> Cat (hide d) (hide d')
        Nest i d             -> Nest i (hide d)
        Union d d'           -> Union (hide d) (hide d')
        Column f             -> Column (hide . f)
        WithPageWidth f      -> WithPageWidth (hide . f)
        Nesting f            -> Nesting (hide . f)
        Annotated ann d'     ->
          if not (displayTypes opts)      && ann == AnnSyntax Type
          || not (displayUniques opts)    && ann == AnnSyntax Unique
          || not (displayQualifiers opts) && ann == AnnSyntax Qualifier
          || not (displayTicks opts)      && ann == AnnSyntax Ticky
            then Empty
            else Annotated ann (hide d')
        d -> d

pprM :: (Monad m, PrettyPrec p) => p -> m ClashDoc
pprM = pprPrec 0

pprM' :: (Monad m, PrettyPrec p) => PrettyOptions -> p -> m ClashDoc
pprM' opts = pprPrec' opts 0

ppr :: PrettyPrec p => p -> ClashDoc
ppr = runIdentity . pprM

ppr' :: PrettyPrec p => PrettyOptions -> p -> ClashDoc
ppr' opts = runIdentity . pprM' opts

fromPpr :: PrettyPrec a => a -> Doc ()
fromPpr = removeAnnotations . ppr

-- | Print a PrettyPrec thing to a String
showPpr :: PrettyPrec p => p -> String
showPpr = showPpr' def

showPpr' :: PrettyPrec p => PrettyOptions -> p -> String
showPpr' opts = showDoc . ppr' opts

-- | Print a 'PrettyPrec' thing for use in a one-line diagnostic: all runs of
-- whitespace (including newlines) are collapsed into a single space, and the
-- result is truncated to the given number of characters. Without this, a
-- warning quoting a term containing a @case@ or @let@ spills over many lines
-- and drowns out the rest of the message.
showPprDiag :: PrettyPrec p => Int -> p -> String
showPprDiag maxLen p =
  case splitAt maxLen (unwords (words (showPpr p))) of
    (shown, []) -> shown
    (shown, _)  -> shown <> "..."

tracePprId :: PrettyPrec p => p -> p
tracePprId p = trace (showPpr p) p

tracePpr :: PrettyPrec p => p -> a -> a
tracePpr p a = trace (showPpr p) a

parensIf :: Bool -> ClashDoc -> ClashDoc
parensIf False = id
parensIf True  = parens

viewName :: Name a -> (Text, Text, Text)
viewName n = (qual, occ, T.pack $ show $ nameUniq n)
  where (qual, occ) = T.breakOnEnd "." $ nameOcc n

instance PrettyPrec (Name a) where
  pprPrec p (viewName -> (qual, occ, uniq)) = do
    qual' <- annotate (AnnSyntax Qualifier) <$> pprPrec p qual
    occ'  <- pprPrec p occ
    uniq' <- annotate (AnnSyntax Unique) . brackets <$> (pprPrec p uniq)
    return $ qual' <> occ' <> uniq'

instance ClashPretty (Name a) where
  clashPretty = fromPpr

instance PrettyPrec a => PrettyPrec [a] where
  pprPrec prec = fmap vcat . mapM (pprPrec prec)

instance PrettyPrec Text where
  pprPrec _ = pure . pretty

instance Pretty LitTy where
  pretty (NumTy i) = pretty i
  pretty (SymTy s) = dquotes $ pretty s
  pretty (CharTy c) = squotes $ pretty c

instance PrettyPrec LitTy where
  pprPrec _ = return . annotate (AnnSyntax LitS) . pretty

instance PrettyPrec SrcSpan where
  pprPrec _ sp = return ("<src>"<>pretty (GHC.showSDocUnsafe (GHC.ppr sp)))

instance PrettyPrec DataCon where
  pprPrec _ = pprM . dcName

instance PrettyPrec Literal where
  pprPrec _ l = return $ annotate (AnnSyntax LitS) $ case l of
    IntegerLiteral i   -> parensIf (i < 0) (pretty i)
    IntLiteral i       -> parensIf (i < 0) (pretty i <> "#")
    Int64Literal i     -> parensIf (i < 0) (pretty i <> "#64")
    WordLiteral w      -> pretty w <> "##"
    Word64Literal w    -> pretty w <> "##64"
    Int8Literal i      -> parensIf (i < 0) (pretty i <> "#8")
    Int16Literal i     -> parensIf (i < 0) (pretty i <> "#16")
    Int32Literal i     -> parensIf (i < 0) (pretty i <> "#32")
    Word8Literal w     -> pretty w <> "##8"
    Word16Literal w    -> pretty w <> "##16"
    Word32Literal w    -> pretty w <> "##32"
    FloatLiteral w     -> pretty (castWord32ToFloat w) <> "#"
    DoubleLiteral w    -> pretty (castWord64ToDouble w) <> "##"
    CharLiteral c      -> pretty c <> "#"
    StringLiteral s    -> vcat $ map pretty $ showMultiLineString s
    NaturalLiteral n   -> pretty n
    ByteArrayLiteral s -> pretty $ show s
