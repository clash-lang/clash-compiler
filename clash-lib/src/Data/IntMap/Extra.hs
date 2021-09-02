{-# LANGUAGE CPP #-}

module Data.IntMap.Extra where

#if !MIN_VERSION_containers(0,6,2)
import Data.IntMap.Internal

-- TODO We can remove this when support for GHC 8.6 is dropped.

-- | /O(n+m)/. Check whether the key sets of two maps are disjoint
-- (i.e. their 'intersection' is empty).
--
-- > disjoint (fromList [(2,'a')]) (fromList [(1,()), (3,())])   == True
-- > disjoint (fromList [(2,'a')]) (fromList [(1,'a'), (2,'b')]) == False
-- > disjoint (fromList [])        (fromList [])                 == True
--
-- > disjoint a b == null (intersection a b)
--
disjoint :: IntMap a -> IntMap b -> Bool
disjoint Nil _ = True
disjoint _ Nil = True
disjoint (Tip kx _) ys = notMember kx ys
disjoint xs (Tip ky _) = notMember ky xs
disjoint t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = disjoint1
  | shorter m2 m1 = disjoint2
  | p1 == p2      = disjoint l1 l2 && disjoint r1 r2
  | otherwise     = True
  where
    disjoint1 | nomatch p2 p1 m1 = True
              | zero p2 m1       = disjoint l1 t2
              | otherwise        = disjoint r1 t2
    disjoint2 | nomatch p1 p2 m2 = True
              | zero p1 m2       = disjoint t1 l2
              | otherwise        = disjoint t1 r2
#endif
