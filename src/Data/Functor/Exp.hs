{-|
module      : Data.Functor.Exp
description : Exponential functors, and exponential analogues for
              Applicative and Alternative functors.
-}
module Data.Functor.Exp
  ( Exp(..)
  , ExpProfunctor(..)
  , Combinable(..)
  , Pickable(..)
  ) where

import           Control.Applicative
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Functor.Product
import           Data.Void

-- | Exponential/invariant functors.
class Exp f where
  emap :: (a -> b) -> (b -> a) -> f a -> f b

-- | Exponential/invariant profunctors.
class ExpProfunctor p where
  elmap :: (a -> b) -> (b -> a) -> p a c -> p b c
  ermap :: (a -> b) -> (b -> a) -> p c a -> p c b

-- | The exponential analogue to 'Applicative' covariant functors or
-- 'Divisible' contravariant functors.
class Exp f =>
      Combinable f
  where
  combineId :: a -> f a
  combine :: f a -> f b -> f (a, b)

-- | The exponential analogue to 'Alternative' and 'Divisble'.
class Combinable f =>
      Pickable f
  where
  pickId :: f Void
  pick :: f a -> f b -> f (Either a b)

-- | The product of a contravariant and covariant functor is an
-- exponential functor.
instance (Functor f, Contravariant c) => Exp (Product f c) where
  emap f g ~(Pair a b) = Pair (fmap f a) (contramap g b)

-- | Exponential functor products.
instance (Applicative f, Divisible c) => Combinable (Product f c) where
  combineId x = Pair (pure x) conquer
  combine ~(Pair a b) ~(Pair c d) = Pair ((,) <$> a <*> c) (divide id b d)

-- | Exponential functor sums.
instance (Alternative f, Decidable c) => Pickable (Product f c) where
  pickId = Pair empty (lose absurd)
  pick ~(Pair a b) ~(Pair c d) =
    Pair (Left <$> a <|> Right <$> c) (choose id b d)
