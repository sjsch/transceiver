{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Transceiver.Iso
  ( gmap
  ) where

import           Data.Functor.Exp
import           Data.Void

import           GHC.Generics

data A
  = A Int
      Bool
  | B Int
  deriving (Show, Generic)

class Generalizable' f where
  type Generalized f :: *
  generalize' :: f a -> Generalized f
  specialize' :: Generalized f -> f a

instance Generalizable' V1 where
  type Generalized V1 = Void
  generalize' _ = error "unreachable"
  specialize' _ = error "unreachable"

instance Generalizable' U1 where
  type Generalized U1 = ()
  generalize' U1 = ()
  specialize' () = U1

instance (Generalizable' f, Generalizable' g) => Generalizable' (f :+: g) where
  type Generalized (f :+: g) = Either (Generalized f) (Generalized g)
  generalize' (L1 x) = Left (generalize' x)
  generalize' (R1 x) = Right (generalize' x)
  specialize' (Left x)  = L1 (specialize' x)
  specialize' (Right x) = R1 (specialize' x)

instance (Generalizable' f, Generalizable' g) => Generalizable' (f :*: g) where
  type Generalized (f :*: g) = (Generalized f, Generalized g)
  generalize' (x :*: y) = (generalize' x, generalize' y)
  specialize' (x, y) = specialize' x :*: specialize' y

instance Generalizable' (K1 i c) where
  type Generalized (K1 i c) = c
  generalize' (K1 x) = x
  specialize' x = K1 x

instance Generalizable' f => Generalizable' (M1 i t f) where
  type Generalized (M1 i t f) = Generalized f
  generalize' (M1 x) = generalize' x
  specialize' x = M1 (specialize' x)

generalize :: (Generic a, Generalizable' (Rep a)) => a -> Generalized (Rep a)
generalize = generalize' . from

specialize :: (Generic a, Generalizable' (Rep a)) => Generalized (Rep a) -> a
specialize = to . specialize'

gmap ::
     (Exp f, Generic a, Generalizable' (Rep a))
  => f (Generalized (Rep a))
  -> f a
gmap = emap specialize generalize
