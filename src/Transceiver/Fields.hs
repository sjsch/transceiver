{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-|
module      : Transceiver.Fields
description : Utilities for constructing records and sums from named
              fields
-}
module Transceiver.Fields
  ( fld, con
  , (|&|), (|/|)
  , (=&), (=/)
  , pmap, smap
  , Generic
  ) where

import           Data.NamedSOP.Generic
import           Data.NamedSOP.Map
import           Data.NamedSOP.Sum
import           Data.Proxy
import           GHC.Generics
import           GHC.OverloadedLabels
import           GHC.TypeLits

import           Data.Functor.Exp

-- | Isomorphism from 'a' to an 'NMap' with a single, named field.
-- Use @-XTypeApplications@ to set the field name.
--
-- >>> :t fld @"hello" token
-- Syntax s (NMap '["hello" ':-> Token s])
fld :: forall (k :: Symbol) v f. Exp f => f v -> f (NMap '[ k ':-> v ])
fld = emap f g
  where
    f x = NMapExt x NMapEmpty

    g :: NMap '[ k ':-> v ] -> v
    g (NMapExt x NMapEmpty) = x

-- | Isomorphism from 'a' to an 'NSum' with a single, named
-- constructor.  Use @-XTypeApplications@ to set the field name.
--
-- >>> :t con @"hello" token
-- Syntax s (NSum '["hello" ':-> Token s])
con :: forall k v f. Exp f => f v -> f (NSum '[ k ':-> v ])
con = emap f g
  where
    f = NSumThis

    g :: NSum '[ k ':-> v ] -> v
    g (NSumThis x) = x
    g (NSumThat _) = error "unreachable"

-- | 'combine' two exponential functors of 'NMap's.  (The order of the
-- arguments determines the order of the serialized or parsed result,
-- but not the order of the fields in the type.)
infixr 5 |&|
(|&|) :: forall (xs :: [Mapping Symbol *]) (ys :: [Mapping Symbol *]) f.
  (SingI xs, SingI ys, Combinable f) =>
  f (NMap xs) -> f (NMap ys) -> f (NMap (Union xs ys))
x |&| y = emap unionMap ununionMap (combine x y)

-- | 'pick' two exponential functors of 'NSum's.  (The order of the
-- arguments determines the order of the parsed branches are tried,
-- but not the order of the fields in the type.)
infixr 3 |/|
(|/|):: forall (xs :: [Mapping Symbol *]) (ys :: [Mapping Symbol *]) f.
  (SingI xs, SingI ys, Pickable f) =>
  f (NSum xs) -> f (NSum ys) -> f (NSum (Union xs ys))
x |/| y = emap unionSum ununionSum (pick x y)

data FieldLabel (n :: Symbol) = FieldLabel
  deriving (Eq, Ord)

instance KnownSymbol n => Show (FieldLabel n) where
  show FieldLabel = "FieldLabel @\"" ++ symbolVal (Proxy :: Proxy n) ++ "\""

instance (n ~ m) => IsLabel n (FieldLabel m) where
  fromLabel = FieldLabel @n

infixl 6 =&
(=&) :: forall n a f. Exp f => f a -> FieldLabel n -> f (NMap '[n ':-> a])
f =& FieldLabel = fld @n f

infixl 4 =/
(=/) :: forall n a f. Exp f => f a -> FieldLabel n -> f (NSum '[n ':-> a])
f =/ FieldLabel = con @n f

pmap :: (Exp f, Generic a, GenProduct (Rep a)) =>
  f (NMap (GProduct (Rep a))) -> f a
pmap = emap specProduct genProduct

smap :: (Exp f, Generic a, GenSum (Rep a)) =>
  f (NSum (GSum (Rep a))) -> f a
smap = emap specSum genSum
