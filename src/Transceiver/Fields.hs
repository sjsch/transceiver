{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Transceiver.Fields
  ( fld, con
  , (|&|), (|/|)
  , (=&), (=/)
  ) where

import           Data.NamedSOP.Map
import           Data.NamedSOP.Sum
import           Data.Proxy
import           GHC.OverloadedLabels
import           GHC.TypeLits

import           Data.Functor.Exp

fld :: forall k v f. Exp f => f v -> f (NMap '[ k ':-> v ])
fld v = emap f g v
  where
    f x = NMapExt x NMapEmpty

    g :: NMap '[ k ':-> v ] -> v
    g (NMapExt x NMapEmpty) = x

con :: forall k v f. Exp f => f v -> f (NSum '[ k ':-> v])
con v = emap f g v
  where
    f x = NSumThis x

    g :: NSum '[ k ':-> v ] -> v
    g (NSumThis x) = x
    g (NSumThat _) = error "unreachable"

infixr 5 |&|
(|&|) :: forall xs ys f. (SingI xs, SingI ys, Combinable f) =>
  f (NMap xs) -> f (NMap ys) -> f (NMap (Union xs ys))
x |&| y = emap unionMap ununionMap (combine x y)

infixr 3 |/|
(|/|):: forall xs ys f. (SingI xs, SingI ys, Pickable f) =>
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
