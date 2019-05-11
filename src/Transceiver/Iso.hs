{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
module      : Transceiver.Iso
description : Generic isomorphisms
-}
module Transceiver.Iso
  ( gmap
  ) where

import           Data.Functor.Exp
import           Data.Void

import           GHC.Generics

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
  specialize' = K1

instance Generalizable' f => Generalizable' (M1 i t f) where
  type Generalized (M1 i t f) = Generalized f
  generalize' (M1 x) = generalize' x
  specialize' x = M1 (specialize' x)

generalize :: (Generic a, Generalizable' (Rep a)) => a -> Generalized (Rep a)
generalize = generalize' . from

specialize :: (Generic a, Generalizable' (Rep a)) => Generalized (Rep a) -> a
specialize = to . specialize'

-- | Maps an exponential functor from its generic version to its
-- specialized version.
--
-- The generic versions of datatypes are the ones parsed natively by
-- the 'Transceiver.Operators.|*|' and 'Transceiver.Operators.|+|'
-- combinators.  That is, product types like @A B C D@ are represented
-- as @(A, (B, (C, D)))@, and coproduct types like @A | B | C@ are
-- represented as @'Either' A ('Either' B C)@.
--
-- If your datatype has a 'Generic' instance, and your syntax
-- combinators follow the same order as the constructors, inserting a
-- 'gmap' in front should be all that's needed.
gmap ::
     (Exp f, Generic a, Generalizable' (Rep a))
  => f (Generalized (Rep a))
  -> f a
gmap = emap specialize generalize
