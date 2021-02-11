{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
-- {-# OPTIONS_GHC -ddump-simpl #-}
module T1663 (topEntity) where

import Clash.Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.RWS

newtype Component addr = Component Int
    deriving newtype (Eq, Ord)

newtype FanIn a = FanIn{ getFanIn :: First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap = AddrMap{ addrMap :: [(Int,(FanIn (Index 32768)))] }
    deriving newtype (Monoid)

instance Semigroup (AddrMap) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ unionWithKeyList (const mappend) map1 map2

unionWithKeyList :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> [(k,a)] -> [(k,a)]
unionWithKeyList f l r = foldList (insertWithKeyList f) r l

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ z []     = z
foldList f z (x:xs) = foldList f (f x z) xs

insertWithKeyList :: Ord k => (k -> a -> a -> a) -> (k,a) -> [(k,a)] -> [(k,a)]
insertWithKeyList _ k [] = [k]
insertWithKeyList f (k,a) ((k1,b):xs) = case compare k k1 of
  LT -> (k,a):(k1,b):xs
  EQ -> (k,f k a b) : xs
  GT -> (k1,b):insertWithKeyList f (k,a) xs

lookupList :: Eq k => k -> [(k,a)] -> Maybe a
lookupList _ [] = Nothing
lookupList k ((k1,a):xs) = if k == k1 then Just a else lookupList k xs

singletonList :: k -> a -> [(k,a)]
singletonList k a = [(k,a)]

newtype Addressing dat addr a = Addressing
    { unAddressing :: RWS
          (FanIn addr, AddrMap)
          (FanIn (Maybe dat), AddrMap)
          Int
          a
    }
    deriving newtype (Functor, Applicative, Monad)

memoryMap
    :: Maybe addr
    -> (Addressing dat addr a)
    -> (Maybe dat, a)
memoryMap addr body = (join (firstIn read), x)
  where
    (x, (read, conns)) = evalRWS (unAddressing body) (fanInMaybe addr, conns) 0

readWrite_
    :: ((Maybe (Index 32768)) -> (Maybe dat))
    -> Addressing dat addr (Component (Index 32768))
readWrite_ mkComponent = Addressing $ do
    component@(Component i) <- Component <$> get <* modify succ
    (_, addrs) <- ask
    let addr = firstIn . fromMaybe mempty $ lookupList i (addrMap addrs)
        read = mkComponent addr
    tell (fanIn read, mempty)
    return component

ram0
    :: (1 <= n)
    => SNat n
    -> Addressing (Index 32768) addr (Component (Index 32768))
ram0 size@SNat = readWrite_ $ \addr -> Nothing
    -- Just $ negate (fromMaybe 2 addr)

connect
    :: Component (Index 32768)
    -> Addressing dat (Index 32768) ()
connect component@(Component i) = Addressing $ do
    (addr, _) <- ask
    tell (mempty, AddrMap $ singletonList i $ addr)

firstIn :: FanIn a -> Maybe a
firstIn = getFirst . getFanIn

fanInMaybe :: Maybe a -> FanIn a
fanInMaybe = FanIn . First

fanIn :: a -> FanIn a
fanIn = fanInMaybe . pure

matchAddr
    :: (addr -> Maybe addr')
    -> Addressing dat addr' a
    -> Addressing dat addr a
matchAddr match body = Addressing $ rws $ \(addr, addrs) s ->
  let addr' = fanInMaybe . (match =<<) . firstIn $ addr
  in runRWS (unAddressing body) (addr', addrs) s

from
    :: (Num addr, Integral addr, Num addr')
    => addr
    -> Addressing dat addr' a
    -> Addressing dat addr a
from base = matchAddr $ \addr -> pure (fromIntegral base)

topEntity
    :: Maybe (Index 0x8000)
    -> ((Maybe (Index 0x8000)),( Signal System (Maybe (Index 7168)), Signal System (Maybe (Unsigned 8))))
topEntity addr = memoryMap addr $ do
    ram <- ram0 (SNat @0x0400)
    ram1 <- ram0 (SNat @0x0400)
    -- ram2 <- ram0 (SNat @0x0400)
    -- ram3 <- ram0 (SNat @0x0400)
    -- ram4 <- ram0 (SNat @0x0400)

    let vidAddr  = pure Nothing :: Signal System (Maybe (Index 7168))
        vidWrite = pure Nothing :: Signal System (Maybe (Unsigned 8))

    -- return ()
    from 0x0000 $ connect ram
    from 0x2000 $ connect ram1
    -- from 0x2400 $ connect ram2
    -- from 0x4000 $ connect ram3
    -- from 0x6000 $ connect ram4

    return (vidAddr, vidWrite)
