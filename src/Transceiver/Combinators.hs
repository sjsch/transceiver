{-|
module      : Transceiver.Combinators
description : Pure combinators that work for all exponential functors.
-}
module Transceiver.Combinators
  ( untilE
  , until1E
  ) where

import           Data.List.NonEmpty

import           Data.Functor.Exp

-- | Syntax combinator similar to 'Control.Applicative.many'.
--
-- @
-- [a a .. a] b
-- @
untilE :: Pickable f => f a -> f b -> f ([a], b)
untilE a b = emap f g $ pick b (combine a (untilE a b))
  where
    f (Left y)             = ([], y)
    f (Right (x, (xs, y))) = (x : xs, y)
    g ([], y)   = Left y
    g (x:xs, y) = Right (x, (xs, y))

-- | Syntax combinator similar to 'Control.Applicative.some'.
--
-- @
-- a [a a .. a] b
-- @
until1E :: Pickable f => f a -> f b -> f (NonEmpty a, b)
until1E a b = emap f g $ combine a (pick b (untilE a b))
  where
    f (x, Left y)        = (x :| [], y)
    f (x, Right (xs, y)) = (x :| xs, y)
    g (x :| [], y) = (x, Left y)
    g (x :| xs, y) = (x, Right (xs, y))
