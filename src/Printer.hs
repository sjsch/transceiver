module Printer where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

import Stream

newtype Printer s a
  = Printer { pprint :: a -> s -> s
            }

instance Contravariant (Printer s) where
  contramap f p = Printer $ \a s -> pprint p (f a) s

instance Divisible (Printer s) where
  conquer = Printer $ \_ s -> s
  divide f a b = Printer $ \x s ->
                             let (y, z) = f x
                                 s' = pprint a y s
                             in pprint b z s'

instance Decidable (Printer s) where
  lose _ = Printer $ \_ s -> s
  choose f a b = Printer $ \x s -> case f x of
    Left x' -> pprint a x' s
    Right x' -> pprint b x' s

putToken :: Stream s => Printer s (Token s)
putToken = Printer $ \x s -> appendStream s x
