{-|
module      : Transceiver.Printer
description : Contravariant printer combinators.
-}
module Transceiver.Printer
  ( Printer(..)
  , printToken
  , printEof
  ) where

import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible

import           Data.Stream

-- | Since it's assumed that all values of the type being printed are
-- valid, it's assumed they all have a valid representation in the
-- output stream.
newtype Printer s a =
  Printer (a -> s -> s)

instance Contravariant (Printer s) where
  contramap f (Printer p) = Printer $ \a s -> p (f a) s

instance Divisible (Printer s) where
  conquer = Printer $ \_ s -> s
  divide f (Printer a) (Printer b) =
    Printer $ \x s ->
      let (y, z) = f x
          s' = a y s
       in b z s'

instance Decidable (Printer s) where
  lose _ = Printer $ \_ s -> s
  choose f (Printer a) (Printer b) = Printer $ \x s -> either a b (f x) s

-- | Print a single token by appending it to the output stream.
printToken :: Stream s => Printer s (Token s)
printToken = Printer $ \x s -> appendStream s x

-- | Do nothing at all to the output stream.
printEof :: Stream s => Printer s ()
printEof = Printer (const id)
