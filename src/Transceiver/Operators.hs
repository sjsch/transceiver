{-|
module      : Transceiver.Operators
description : Various infix synonyms for exponential combinators
-}
module Transceiver.Operators
  ( (|*|)
  , (|+|)
  , (|<|)
  , (|>|)
  ) where

import           Data.Functor.Exp
import           Transceiver.Combinators

-- | Synonym for 'combine'.
infixr 4 |*|

(|*|) :: Combinable f => f a -> f b -> f (a, b)
(|*|) = combine

-- | Synonym for 'pick'.
infixr 3 |+|

(|+|) :: Pickable f => f a -> f b -> f (Either a b)
(|+|) = pick

-- | Synonym for 'sequenceL'.
infixr 4 |<|

(|<|) :: Combinable f => f a -> f () -> f a
(|<|) = sequenceL

-- | Synonym for 'sequenceR'.
infixr 4 |>|

(|>|) :: Combinable f => f () -> f a -> f a
(|>|) = sequenceR
