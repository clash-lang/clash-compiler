{-|
Copyright   :  (C) 2019, Myrtle Software Ltd,
                   2021, QBayLogic B.V.
                   2022, Google Inc.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Tools to convert a 'Term' into its "real" representation
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

--{-# OPTIONS_GHC -ddump-splices #-}

module Clash.Core.TermLiteral
  ( TermLiteral
  , showsTypePrec
  , showType
  , termToData
  , termToDataError
  , deriveTermLiteral
  ) where

import           Data.Bifunctor                  (bimap)
import           Data.Either                     (lefts)
import           Data.Proxy                      (Proxy(..))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Text.Extra                 (showt)
#if MIN_VERSION_ghc(9,4,0)
import qualified Data.Text.Internal              as Text
import qualified Data.Text.Array                 as Text
import qualified Data.Primitive.ByteArray        as BA
#endif
import           Data.Typeable                   (Typeable, typeRep)
import           GHC.Natural
import           GHC.Stack
import           GHC.TypeNats (KnownNat)
import           Text.Show.Pretty                (ppShow)

import           Clash.Annotations.SynthesisAttributes (Attr)
import           Clash.Core.DataCon              (DataCon(..))
import           Clash.Core.Literal
import           Clash.Core.Name                 (Name(..))
import           Clash.Core.Pretty               (showPpr)
import           Clash.Core.Term                 (Term(Literal, Data, Tick), collectArgs)
import           Clash.Promoted.Nat
import           Clash.Promoted.Nat.Unsafe
import           Clash.Sized.Index               (Index)
import           Clash.Sized.Vector              (Vec (Nil, Cons), fromList)
import qualified Clash.Util.Interpolate          as I
import qualified Clash.Verification.Internal     as Cv

import           Clash.Core.TermLiteral.TH

-- | Pretty print type @a@
showType :: TermLiteral a => Proxy a -> String
showType proxy = showsTypePrec 0 proxy ""

-- | Tools to deal with literals encoded as a 'Term'.
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

  -- | Pretty print the type of a term (for error messages). Its default implementation
  -- uses 'Typeable' to print the type. Note that this method is there to allow
  -- an instance for 'SNat' to exist (and other GADTs imposing
  -- t'GHC.TypeNats.KnownNat'). Without it, GHC would ask for a @KnownNat@
  -- constraint on the instance, which would defeat the purpose of it.
  showsTypePrec ::
    -- | The operator precedence of the enclosing context (a number from @0@ to
    -- @11@). Function application has precedence @10@. Used to determine whether
    -- the result should be wrapped in parentheses.
    Int ->
    -- | Proxy for a term whose type needs to be pretty printed
    Proxy a ->
    ShowS

  default showsTypePrec :: Typeable a => Int -> Proxy a -> ShowS
  showsTypePrec n _ = showsPrec n (typeRep (Proxy @a))

instance TermLiteral Term where
  termToData = pure

instance TermLiteral String where
  termToData (collectArgs -> (_, [Left (Literal (StringLiteral s))])) = Right s
  termToData t = Left t

instance TermLiteral Text where
  termToData (collectArgs -> (_, [Left (Literal (StringLiteral s))])) =
    Right (Text.pack s)
#if MIN_VERSION_ghc(9,4,0)
  termToData (collectArgs -> (_, [ Left (Literal (ByteArrayLiteral (BA.ByteArray ba)))
                                 , Left (Literal (IntLiteral off))
                                 , Left (Literal (IntLiteral len))])) =
    Right (Text.Text (Text.ByteArray ba) (fromInteger off) (fromInteger len))
#endif
  termToData t = Left t

instance KnownNat n => TermLiteral (Index n) where
  termToData t@(collectArgs -> (_, [_, _, Left (Literal (IntegerLiteral n))]))
    | n < 0 = Left t
    | n >= natToNum @n = Left t
    | otherwise = Right (fromInteger n)
  termToData t = Left t

instance TermLiteral Int where
  termToData (collectArgs -> (_, [Left (Literal (IntLiteral n))])) =
    Right (fromInteger n)
  termToData t = Left t

instance TermLiteral Word where
  termToData (collectArgs -> (_, [Left (Literal (WordLiteral n))])) =
    Right (fromInteger n)
  termToData t = Left t

instance TermLiteral Integer where
  termToData (Tick _ e) = termToData e
  termToData (Literal (IntegerLiteral n)) = Right n
  termToData (collectArgs -> (_, [Left (Literal (IntLiteral n))])) = Right n
  termToData (collectArgs -> (_, [Left (Literal (IntegerLiteral n))])) = Right n
  termToData t = Left t

instance TermLiteral Char where
  termToData (collectArgs -> (_, [Left (Literal (CharLiteral c))])) = Right c
  termToData t = Left t

instance TermLiteral Natural where
  termToData (Tick _ e) = termToData e
  termToData t@(Literal (NaturalLiteral n))
    | n < 0 = Left t
    | otherwise = Right (fromIntegral n)
  termToData (collectArgs -> (_, [Left (Literal (NaturalLiteral n))])) =
    Right (fromInteger n)
  termToData t = Left t

-- | Unsafe warning: If you use this instance in a monomorphic context (e.g.,
-- @TermLiteral (SNat 5)@), you need to make very sure that the term corresponds
-- to the literal. If you don't, there will be a mismatch between type level
-- variables and the proof carried in 'SNat's 'KnownNat'. Typical usage of this
-- instance will therefore leave the /n/ polymorphic.
--
instance TermLiteral (SNat n) where
  termToData = \case
    Tick _ e                   -> termToData e
    Literal (NaturalLiteral n) -> Right (unsafeSNat n)
    t                          -> Left t

  showsTypePrec n _
    -- We don't know the literal /n/ at this point. However, we can't simply put
    -- and /n/ here either, as it might collide with other type variables. To
    -- prevent confusion, we put an underscore. This is obviously "wrong", but
    -- good enough for error messages - the main purpose of this function.
    = showParen (n > 10) $ showString "SNat _"

instance (TermLiteral a, TermLiteral b) => TermLiteral (a, b) where
  termToData (collectArgs -> (_, lefts -> [a, b])) = do
    a' <- termToData a
    b' <- termToData b
    pure (a', b')
  termToData t = Left t

  showsTypePrec _ _ =
    -- XXX: We pass in 11 here, but should really be passing in 0. We never want
    --      any parentheses for fields in tuples. However, Typeable's show
    --      implementation does put parentheses around tuple fields - so we
    --      replicate that behavior here for ease of testing.
      showChar '('
    . showsTypePrec 11 (Proxy @a)
    . showString ","
    . showsTypePrec 11 (Proxy @b)
    . showChar ')'

instance (TermLiteral a, KnownNat n) => TermLiteral (Vec n a) where
  termToData term = do
    res <- fromList <$> go term
    -- Check whether length of list constructed in 'go' corresponds to the
    -- @KnownNat n@ we've been given
    case res of
      Nothing -> Left term
      Just v -> Right v
   where
    -- Construct a list from given term
    go t@(collectArgs -> (constr, args)) =
      case constr of
        Data (MkData{dcName=Name{nameOcc}})
          | nameOcc == showt 'Nil -> Right []
          | nameOcc == showt 'Cons ->
            case lefts args of
              [_gadtProof, c0, cs0] -> do
                c1 <- termToData @a c0
                cs1 <- go cs0
                Right (c1:cs1)
              _ -> Left t
        _ -> Left t

  showsTypePrec n _ =
    showParen (n > 10) $
        showString "Vec"
      . showChar ' '
      . showString (show (natToInteger @n))
      . showChar ' '
      . showsTypePrec 11 (Proxy @a)

deriveTermLiteral ''Bool
deriveTermLiteral ''Maybe
deriveTermLiteral ''Either
deriveTermLiteral ''Cv.RenderAs
deriveTermLiteral ''Cv.Assertion'
deriveTermLiteral ''Cv.Property'
deriveTermLiteral ''Attr

-- | Same as 'termToData', but returns printable error message if it couldn't
-- translate a term.
termToDataError :: forall a. TermLiteral a => Term -> Either String a
termToDataError term = bimap err id (termToData term)
 where
  -- XXX: If we put this construct in the quasiquoted part, it yields a parse
  --      error on some platforms. This is likely related to some older version
  --      of dependencies. In the interested of time yours truly just moved it
  --      outside of the quasiquoter.
  shownType = showType (Proxy @a)

  err failedTerm = [I.i|
    Failed to translate term to literal. Term that failed to translate:

      #{showPpr failedTerm}

    In its non-pretty-printed form:

      #{ppShow failedTerm}

    In the full term:

      #{showPpr term}

    While trying to interpret something to type:

      #{shownType}
  |]
