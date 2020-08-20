{-|
Copyright   :  (C) 2019, Myrtle Software Ltd
License     :  BSD2 (see the file LICENSE)
Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

Tools to convert a 'Term' into its "real" representation
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

--{-# OPTIONS_GHC -ddump-splices #-}

module Clash.Core.TermLiteral
  ( TermLiteral
  , termToData
  , termToDataError
  , uncheckedTermToData
  ) where

import           Data.Bifunctor                  (bimap)
import           Data.Either                     (lefts)
import           Data.Proxy                      (Proxy(..))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Typeable                   (Typeable, typeRep)
import           GHC.Natural
import           GHC.Stack

import           Clash.Core.Term
  (AppArg (TermArg), Term(Literal), collectAppArgs, termArgs, stripTickX)
import           Clash.Core.Literal
import           Clash.Core.Pretty               (showPpr)
import qualified Clash.Util.Interpolate          as I
import qualified Clash.Verification.Internal     as Cv

import           Clash.Core.TermLiteral.TH

-- | Tools to deal with literals encoded as a "Term".
class Typeable a => TermLiteral a where
  -- | Convert 'Term' to the constant it represents. Will return an error if
  -- (one of the subterms) fail to translate.
  termToData
    :: HasCallStack
    => Term
    -- ^ Term to convert
    -> Either Term a
    -- ^ 'Left' indicates a failure, containing the (sub)term that failed to
    -- translate. 'Right' indicates a success.

instance TermLiteral Term where
  termToData = pure

instance TermLiteral String where
  termToData (collectAppArgs -> (_, stripTickX -> [TermArg (Literal (StringLiteral s))])) = Right s
  termToData t = Left t

instance TermLiteral Text where
  termToData t = Text.pack <$> termToData t

instance TermLiteral Int where
  termToData (collectAppArgs -> (_, stripTickX -> [TermArg (Literal (IntLiteral n))])) =
    Right (fromInteger n)
  termToData t = Left t

instance TermLiteral Word where
  termToData (collectAppArgs -> (_, stripTickX -> [TermArg (Literal (WordLiteral n))])) =
    Right (fromInteger n)
  termToData t = Left t

instance TermLiteral Integer where
  termToData (collectAppArgs -> (_, stripTickX -> [TermArg (Literal (IntegerLiteral n))])) = Right n
  termToData t = Left t

instance TermLiteral Char where
  termToData (collectAppArgs -> (_, stripTickX -> [TermArg (Literal (CharLiteral c))])) = Right c
  termToData t = Left t

instance TermLiteral Natural where
  termToData (collectAppArgs -> (_, stripTickX -> [TermArg (Literal (NaturalLiteral n))])) =
    Right (fromInteger n)
  termToData t = Left t

instance (TermLiteral a, TermLiteral b) => TermLiteral (a, b) where
  termToData (collectAppArgs -> (_, termArgs -> [a, b])) = do
    a' <- termToData a
    b' <- termToData b
    pure (a', b')
  termToData t = Left t

instance TermLiteral a => TermLiteral (Maybe a) where
  termToData = $(deriveTermToData ''Maybe)

instance TermLiteral Bool where
  termToData = $(deriveTermToData ''Bool)

instance TermLiteral Cv.RenderAs where
  termToData = $(deriveTermToData ''Cv.RenderAs)

instance TermLiteral a => TermLiteral (Cv.Assertion' a) where
  termToData = $(deriveTermToData ''Cv.Assertion')

instance TermLiteral a => TermLiteral (Cv.Property' a) where
  termToData = $(deriveTermToData ''Cv.Property')

-- | Same as 'termToData', but returns printable error message if it couldn't
-- translate a term.
termToDataError :: forall a. TermLiteral a => Term -> Either String a
termToDataError term = bimap err id (termToData term)
 where
  typ = show (typeRep (Proxy @a))

  err failedTerm = [I.i|
    Failed to translate term to literal. Term that failed to translate:

      #{showPpr failedTerm}

    In the full term:

      #{showPpr term}

    While trying to interpret something to type:

      #{typ}
  |]

-- | Same as 'termToData', but errors hard if it can't translate a given term
-- to data.
uncheckedTermToData :: TermLiteral a => Term -> a
uncheckedTermToData = either error id . termToDataError
