{-|
Copyright   :  (C) 2019, Myrtle Software Ltd
License     :  BSD2 (see the file LICENSE)
Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

Tools to convert a 'Term' into its "real" representation
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Clash.Core.TermLiteral
  ( TermLiteral
  , termToData
  , termToDataError
  , uncheckedTermToData
  ) where

import qualified Data.Text                       as Text
import           Data.Text                       (Text)
import           Data.Bifunctor                  (bimap)
import           Data.Either                     (lefts)
import           GHC.Natural
import           GHC.Stack

import           Clash.Core.Term                 (Term(Literal), collectArgs)
import           Clash.Core.Literal
import           Clash.Core.Pretty               (showPpr)

import           Clash.Core.TermLiteral.TH

-- | Tools to deal with literals encoded as a "Term".
class TermLiteral a where
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
  termToData (collectArgs -> (_, [Left (Literal (StringLiteral s))])) = Right s
  termToData t = Left t

instance TermLiteral Text where
  termToData t = Text.pack <$> termToData t

instance TermLiteral Int where
  termToData (collectArgs -> (_, [Left (Literal (IntLiteral n))])) =
    Right (fromInteger n)
  termToData t = Left t

instance TermLiteral Integer where
  termToData (collectArgs -> (_, [Left (Literal (IntegerLiteral n))])) = Right n
  termToData t = Left t

instance TermLiteral Char where
  termToData (collectArgs -> (_, [Left (Literal (CharLiteral c))])) = Right c
  termToData t = Left t

instance TermLiteral Natural where
  termToData (collectArgs -> (_, [Left (Literal (NaturalLiteral n))])) =
    Right (fromInteger n)
  termToData t = Left t

instance (TermLiteral a, TermLiteral b) => TermLiteral (a, b) where
  termToData (collectArgs -> (_, lefts -> [a, b])) = do
    a' <- termToData a
    b' <- termToData b
    pure (a', b')
  termToData t = Left t

instance TermLiteral a => TermLiteral (Maybe a) where
  termToData = $(deriveTermToData ''Maybe)

instance TermLiteral Bool where
  termToData = $(deriveTermToData ''Bool)

-- | Same as 'termToData', but returns printable error message if it couldn't
-- translate a term.
termToDataError :: TermLiteral a => Term -> Either String a
termToDataError term = bimap err id (termToData term)
 where
  err failedTerm =
    "Failed to translate term to literal. Term that failed to translate:\n\n"
    ++ showPpr failedTerm ++ "\n\nIn the full term:\n\n" ++ showPpr term

-- | Same as 'termToData', but errors hard if it can't translate a given term
-- to data.
uncheckedTermToData :: TermLiteral a => Term -> a
uncheckedTermToData = either error id . termToDataError
