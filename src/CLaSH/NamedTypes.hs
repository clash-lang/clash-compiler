{- |
Copyright  :  (C) 2017, QBayLogic
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Add inline documentation to types:

@
fifo
  :: SClock clk
  -> SNat addrSize
  -> "read request" ::: Signal' clk Bool
  -> "write request" ::: Signal' clk (Maybe (BitVector dataSize))
  -> ( "q"     ::: Signal' clk (BitVector dataSize)
     , "full"  ::: Signal' clk Bool
     , "empty" ::: Signal' clk Bool
     )
@

which can subsequently be inspected in the interactive environment:

>>> :t fifo systemClock
fifo systemClock
  :: SNat addrSize
     -> "read request" ::: Signal' SystemClock Bool
     -> "write request"
        ::: Signal' SystemClock (Maybe (BitVector dataSize))
     -> ("q" ::: Signal' SystemClock (BitVector dataSize),
         "full" ::: Signal' SystemClock Bool,
         "empty" ::: Signal' SystemClock Bool)

-}

{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.NamedTypes
  ((:::))
where

type (name :: k) ::: a = a
-- ^ Annotate a type with a name

{- $setup
>>> :set -XDataKinds -XTypeOperators -XNoImplicitPrelude
>>> import CLaSH.Prelude
>>> import CLaSH.Prelude.Explicit
>>> :{
let fifo
      :: SClock clk
      -> SNat addrSize
      -> "read request" ::: Signal' clk Bool
      -> "write request" ::: Signal' clk (Maybe (BitVector dataSize))
      -> ( "q"     ::: Signal' clk (BitVector dataSize)
         , "full"  ::: Signal' clk Bool
         , "empty" ::: Signal' clk Bool
         )
    fifo = CLaSH.Prelude.undefined
:}

-}
