{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Copyright  :  (C) 2015-2016, University of Twente,
                  2017-2019, Myrtle Software Ltd
                  2017     , Google Inc.,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

RAM primitives with a combinational read port
-}
module Clash.Prelude.RAM (
  -- * RAM synchronized to an arbitrary clock
  asyncRam,
  asyncRamPow2,
)
where

import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (KnownNat)

import qualified Clash.Explicit.RAM as E
import Clash.Promoted.Nat (SNat)
import Clash.Signal
import Clash.Sized.Unsigned (Unsigned)
import Clash.XException (NFDataX)

{- | Create a RAM with space for @n@ elements

* __NB__: Initial content of the RAM is /undefined/, reading it will throw an
'Clash.XException.XException'

=== See also:

* See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
RAM.
-}
asyncRam ::
  ( Enum addr
  , NFDataX addr
  , HiddenClock dom
  , HiddenEnable dom
  , HasCallStack
  , NFDataX a
  ) =>
  -- | Size @n@ of the RAM
  SNat n ->
  -- | Read address @r@
  Signal dom addr ->
  -- | (write address @w@, value to write)
  Signal dom (Maybe (addr, a)) ->
  -- | Value of the RAM at address @r@
  Signal dom a
asyncRam = \sz rd wrM ->
  withFrozenCallStack
    (hideEnable (\en -> hideClock (\clk -> E.asyncRam clk clk en sz rd wrM)))
{-# INLINE asyncRam #-}

{- | Create a RAM with space for 2^@n@ elements

* __NB__: Initial content of the RAM is /undefined/, reading it will throw an
'Clash.XException.XException'

=== See also:

* See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
RAM.
-}
asyncRamPow2 ::
  ( KnownNat n
  , HiddenClock dom
  , HiddenEnable dom
  , HasCallStack
  , NFDataX a
  ) =>
  -- | Read address @r@
  Signal dom (Unsigned n) ->
  -- | (write address @w@, value to write)
  Signal dom (Maybe (Unsigned n, a)) ->
  -- | Value of the RAM at address @r@
  Signal dom a
asyncRamPow2 = \rd wrM ->
  withFrozenCallStack
    (hideEnable (\en -> (hideClock (\clk -> E.asyncRamPow2 clk clk en rd wrM))))
{-# INLINE asyncRamPow2 #-}
