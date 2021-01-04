{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-} -- Needed for `TypeError` only

module T1536 where

import Clash.Prelude
import GHC.Types hiding (One)

topEntity = stepsOf mysteps

step1 = Step INothing (IJust ())
step2 = Step INothing INothing

mysteps :: Steps 2 'False 'False
mysteps = step1 `More` (One $ step2)

data IMaybe (isJust :: Bool) a where
    INothing :: IMaybe 'False a
    IJust :: a -> IMaybe 'True a

class Impossible where
    impossible :: a

type family Compat post1 pre2 :: Constraint where
    Compat 'True 'True = (TypeError ('Text "Conflict between postamble and next preamble"), Impossible)
    Compat post1 pre2 = ()

data Step pre post where
    Step :: IMaybe pre () -> IMaybe post () -> Step pre post

data Steps (l :: Nat) pre post where
    One :: Step pre post -> Steps 1 pre post
    More :: (Compat post1 pre2) => Step pre1 post1 -> Steps n pre2 post2 -> Steps (1 + n) pre1 post2

from :: IMaybe free m -> Maybe m
from INothing = Nothing
from (IJust x) = Just x

stepsOf :: Steps n pre post -> Vec n (Maybe ())
stepsOf xs0 = case go xs0 of (_pre, ys) -> ys

go :: Steps n pre post -> (IMaybe pre (), Vec n (Maybe ()))
go (One (Step pre post)) = (pre, singleton (from post))
go (More (Step pre post) xs) = case go xs of (pre', ys) -> (pre, (combine post pre') :> ys)

combine :: (Compat post1 pre2) => IMaybe post1 b -> IMaybe pre2 b -> Maybe b
combine IJust{} IJust{} = impossible
combine INothing post = from post
combine pre INothing = from pre
