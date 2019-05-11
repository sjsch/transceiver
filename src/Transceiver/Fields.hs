{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Transceiver.Fields where

import           Data.Functor.Exp

import           Data.Type.Map         hiding (Combinable)
import           GHC.Generics
import           GHC.TypeLits

data A =
  A
    { aOne :: Int
    , aTwo :: Bool
    }
  deriving (Eq, Ord, Show, Generic)

newtype Fields m e =
  Fields (e (Map m))

fld ::
     forall n a e. Exp e
  => e a
  -> Fields '[ n ':-> a] e
fld x = Fields $ emap f g x
  where
    f y = Ext (Var @n) y Empty
    g = lookp (Var @n)

class Con (f :: * -> *) where
  type ConFields f :: [Mapping Symbol *]
  toCon :: f a -> Map (ConFields f)
  fromCon :: Map (ConFields f) -> f a

instance Con (M1 S ('MetaSel ('Just s) _a _b _c) (Rec0 t)) where
  type ConFields (M1 S ('MetaSel ('Just s) _a _b _c) (Rec0 t)) = '[ s ':-> t]
  toCon (M1 (K1 x)) = Ext Var x Empty
  fromCon m = M1 (K1 (lookp (Var @s) m))

instance ( Unionable (ConFields a) (ConFields b)
         , Split (ConFields a) (ConFields b) (Union (ConFields a) (ConFields b))
         , Con a
         , Con b
         ) =>
         Con (a :*: b) where
  type ConFields (a :*: b) = Union (ConFields a) (ConFields b)
  toCon (x :*: y) = toCon x `union` toCon y
  fromCon m =
    let (m1, m2) = split m
     in fromCon m1 :*: fromCon m2
