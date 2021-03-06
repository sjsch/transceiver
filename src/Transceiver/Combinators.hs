{-# LANGUAGE TupleSections #-}

{-|
module      : Transceiver.Combinators
description : Pure combinators that work for all exponential functors.
-}
module Transceiver.Combinators
  ( manyE
  , someE
  , untilE
  , until1E
  , optional
  , repeatN
  , constant
  , padding
  , sequenceL
  , sequenceR
  , sepBy
  , sepBy1
  , between
  ) where

import           Data.List.NonEmpty (NonEmpty (..))

import           Data.Functor.Exp

-- | Syntax combinator similar to 'Control.Applicative.many', for zero
-- or more @a@s, until the usable IO is exhausted.
--
-- @
-- [a a .. a]
-- @
manyE :: Pickable f => f a -> f [a]
manyE a = emap f g $ pick (combine a (manyE a)) (combineId ())
  where
    f (Right ())     = []
    f (Left (x, xs)) = x : xs
    g []     = Right ()
    g (x:xs) = Left (x, xs)

-- | Syntax combinator similar to 'Control.Applicative.some', for one
-- or more @a@s, until the usable IO is exhausted.
--
-- @
-- a [a a .. a]
-- @
someE :: Pickable f => f a -> f (NonEmpty a)
someE a = emap f g $ combine a (pick (manyE a) (combineId ()))
  where
    f (x, Left xs)  = x :| xs
    f (x, Right ()) = x :| []
    g (x :| []) = (x, Right ())
    g (x :| xs) = (x, Left xs)

-- | Syntax combinator for zero or more @a@s, until the first @b@.
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

-- | Syntax combinator for one or more @a@s, until the first @b@.
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

-- | Syntax combinator for an optional element.  When parsing, will
-- return 'Nothing' when @a@ fails.
--
-- @
-- [a]
-- @
optional :: Pickable f => f a -> f (Maybe a)
optional a = emap f g $ pick a (combineId ())
  where
    f (Left x)   = Just x
    f (Right ()) = Nothing
    g (Just x) = Left x
    g Nothing  = Right ()

-- | Syntax for @n@ repeated instances of @a@.
--
-- @
-- a1 a2 ... an
-- @
repeatN :: Combinable f => Int -> f a -> f [a]
repeatN 0 _ = combineId []
repeatN n a = emap f g $ combine a (repeatN (n - 1) a)
  where
    f = uncurry (:)
    g []     = error "repeatN ran out of items"
    g (x:xs) = (x, xs)

-- | Disregard the information from the covariant functor, and replace
-- the information in the contravariaint functor with @x@.
--
-- For syntax combinators, this ignores the parsed token and prints
-- the same thing every time.
constant :: Exp f => a -> f a -> f ()
constant x = emap (const ()) (const x)

-- | Syntax combinator that disregards @n@ @t@s, and prints @n@ @f@s.
--
-- Parsing:
--
-- @
-- _1 _2 ... _n
-- @
--
-- Printing:
--
-- @
-- f1 f2 ... f3
-- @
padding :: Combinable f => Int -> a -> f a -> f ()
padding n f t = constant (replicate n ()) $ repeatN n (constant f t)

-- | Assuming @a@ carries no information, sequence @a@ and @b@, like '<*'
--
-- @
-- a b
-- @
sequenceL :: Combinable f => f a -> f () -> f a
sequenceL a b = emap fst (, ()) $ combine a b

-- | Assuming @b@ carries no information, sequence @a@ and @b@, like '*>'
--
-- @
-- a b
-- @
sequenceR :: Combinable f => f () -> f a -> f a
sequenceR b a = emap snd ((), ) $ combine b a

sepBy1 :: Pickable f => f a -> f () -> f (NonEmpty a)
sepBy1 p s = emap f g $ combine p (manyE $ sequenceR s p)
  where
    f = uncurry (:|)
    g (x :| y) = (x, y)

sepBy :: Pickable f => f a -> f () -> f [a]
sepBy p s = emap f g $ pick (sepBy1 p s) (combineId ())
  where
    f (Right ())      = []
    f (Left (x :| y)) = x : y
    g []    = Right ()
    g (x:y) = Left (x :| y)

between :: Combinable f => f () -> f () -> f a -> f a
between l r p = sequenceR l (sequenceL p r)
