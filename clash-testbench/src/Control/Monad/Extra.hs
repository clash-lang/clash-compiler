{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

Some extra monad operations.
-}
module Control.Monad.Extra
  ( (<?>)
  , (<:>)
  ) where

-- | A fully monadic "if-then-else", which is recommended to be used
-- with '<:>'.
infixr 0 <?>
(<?>) :: Monad m => m Bool -> (m a, m a) -> m a
c <?> (a, b) = c >>= \case True -> a
                           False -> b

-- | Some type restricted syntactic sugar for the pair constructor
-- @(,)@ (to make the usage of'<?>' look nice).
infixr 0 <:>
(<:>) :: Monad m => m a -> m b -> (m a, m b)
(<:>) = (,)
