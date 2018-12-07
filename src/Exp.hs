module Exp where

import Control.Applicative
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Product
import Data.List

-- | Exponential/invariant functors.
class Exp f where
  emap :: (a -> b) -> (b -> a) -> f a -> f b

-- | Exponential/invariant profunctors.
class ExpProfunctor p where
  elmap :: (a -> b) -> (b -> a) -> p a c -> p b c
  ermap :: (a -> b) -> (b -> a) -> p c a -> p c b

-- | The exponential analogue to 'Applicative' covariant functors or
-- 'Divisible' contravariant functors.
class Exp f => Combinable f where
  combineId :: a -> f a
  combine :: f a -> f b -> f (a, b)

-- | The exponential analogue to 'Alternative' and 'Divisble'.
class Combinable f => Pickable f where
  pick :: f a -> f b -> f (Either a b)

-- | The product of a contravariant and covariant functor is an
-- exponential functor.
instance (Contravariant c, Functor f) => Exp (Product c f) where
  emap f g (Pair a b) = Pair (contramap g a) (fmap f b)

-- | Exponential functor products.
instance (Divisible c, Applicative f) => Combinable (Product c f) where
  combineId = \x -> Pair conquer (pure x)
  combine (Pair a b) (Pair c d) = Pair (divide id a c) ((,) <$> b <*> d)

-- | Exponential functor sums.
instance (Decidable c, Alternative f) => Pickable (Product c f) where
  pick (Pair a b) (Pair c d) = Pair (choose id a c) (Left <$> b <|> Right <$> d)
