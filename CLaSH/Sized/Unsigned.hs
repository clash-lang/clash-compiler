{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
module CLaSH.Sized.Unsigned where

import GHC.TypeLits

newtype Unsigned (n :: Nat) = U Integer
