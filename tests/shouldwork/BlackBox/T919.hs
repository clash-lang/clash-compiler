module T919 where

import Clash.Prelude

type U8  = BitVector 8
type U16 = BitVector 16
type U32 = BitVector 32
type U64 = BitVector 64

data SearchTarget = None |
                    S16 U16 | S32 U32
  deriving (Generic, Eq, Show, ShowX, NFDataX)
type SearchState = (SearchTarget, BitVector 24, BitVector 32, Bool)
type SearchResult = BitVector 32

searchT
  :: SearchState
  -> (Maybe U64)
  -> (Maybe SearchResult)

searchT (S32 t, buff, offset, first) (Just x) = m
 where
  m = fmap collate
    $ fold (<|>)
    $ imap match
    $ take d2
    $ id @(Vec 8 (Vec 4 U8))
    $ windows1d d4 $ bitCoerce @_ @(Vec _ U8)
    $ x ++# buff
  match i v = Just i
  collate i = resize (pack i)

searchT (S16 t, buff, offset, first) (Just x) = m
 where
  m = fmap collate
    $ fold (<|>)
    $ imap match
    $ take d2
    $ id @(Vec 8 (Vec 2 U8))
    $ windows1d d2 $ bitCoerce @_ @(Vec _ U8)
    $ x ++# slice d23 d16 buff
  match i v = Just i
  collate i = resize (pack i)

searchT _ _ = Nothing


topEntity = searchT
