{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}

module T1340 where

import Clash.Prelude

type Lanes = 3

data SigFn a b = SigFn { runSigFn :: SigType a -> SigType b }

class Sig a where
  type SigType a

instance Sig Bit where
  type SigType Bit = Signal System Bit

instance Sig a => Sig (Vec n a) where
  type SigType (Vec n a) = Vec n (SigType a)

instance (Sig a, Sig b) => Sig (a, b) where
  type SigType (a, b) = (SigType a, SigType b)

singletonSigFn :: SigFn a (Vec 1 a)
singletonSigFn = SigFn singleton

headSigFn :: SigFn (Vec 1 a) a
headSigFn = SigFn head

defSigFn :: Default (SigType t) => SigFn s t
defSigFn = SigFn (const def)

(>>>) :: SigFn a b -> SigFn b c -> SigFn a c
SigFn f >>> SigFn g = SigFn (g . f)

(<=>) :: SigFn a b -> SigFn c d -> SigFn (a, c) (b, d)
SigFn f <=> SigFn g = SigFn (\(x, y) -> (f x, g y))

splitSigFn :: KnownNat n => SigFn (Vec (n + m) a) (Vec n a, Vec m a)
splitSigFn = SigFn splitAtI

catSigFn :: SigFn (Vec n a, Vec m a) (Vec (n + m) a)
catSigFn = SigFn (\(x, y) -> x ++ y)

data BitLayout t where
  BitLayout :: forall bitCount t
            .  ( KnownNat bitCount )
            => { bitCount :: SNat bitCount
               , inFn :: SigFn t (Vec bitCount Bit)
               , outFn :: SigFn (Vec bitCount Bit) t
               }
            -> BitLayout t

unitLayout :: BitLayout Bit
unitLayout = BitLayout SNat singletonSigFn headSigFn

voidLayout :: forall t . Default (SigType t) => BitLayout t
voidLayout = BitLayout d0 defSigFn defSigFn

catLayout
  :: forall n m s
  .  ( KnownNat n
     , KnownNat m
     , 1 <= n )
  => BitLayout (Vec n s)
  -> BitLayout (Vec m s)
  -> BitLayout (Vec (n + m) s)
catLayout (BitLayout aBitCount aInFn aOutFn) (BitLayout bBitCount bInFn bOutFn) = let
  inFn = splitSigFn >>> (aInFn <=> bInFn) >>> catSigFn
  outFn = splitSigFn >>> (aOutFn <=> bOutFn) >>> catSigFn
  in BitLayout SNat inFn outFn

vecLayout
  :: forall n n' t
  .  ( n' ~ (n - 1)
     , 1 <= n
     , KnownNat n
     , KnownNat n' )
  => Vec n (BitLayout t)
  -> BitLayout (Vec n t)
vecLayout (BitLayout{..} :> others) = let
  headLayout = BitLayout SNat (headSigFn >>> inFn) (outFn >>> singletonSigFn)
  in case others of
       Nil -> headLayout
       _ -> headLayout `catLayout` vecLayout @n' others

layout :: BitLayout (Vec Lanes Bit)
layout = mkPos (0 :: Index (Lanes + 1))

mkPos
  :: forall rp
  .  ( KnownNat rp
     , 1 <= rp
     )
  => Index (rp + 1)
  -> BitLayout (Vec rp Bit)
mkPos n = let
  lanes = map (\i -> if i < n
                     then unitLayout
                     else voidLayout) $ iterateI succ 0
  in vecLayout lanes

runBitLayout
  :: Default (SigType t)
  => BitLayout t
  -> SigType t
  -> SigType t
runBitLayout BitLayout{..} = runSigFn ( inFn >>> outFn )

topEntity :: Vec Lanes (Signal System Bit) -> Vec Lanes (Signal System Bit)
topEntity = runBitLayout layout