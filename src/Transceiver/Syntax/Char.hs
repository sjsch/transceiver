{-# LANGUAGE TypeFamilies #-}

module Transceiver.Syntax.Char
  ( natural
  , signed
  , float
  , escapedChar
  , stringLit
  ) where

import           Data.Char
import           Data.Functor.Exp
import           Data.List.NonEmpty
import           Data.Stream

import           Transceiver.Combinators
import           Transceiver.Operators
import           Transceiver.Syntax

natural :: (Stream s, Token s ~ Char, Read n, Show n, Integral n) => Syntax s n
natural = emap f g $ someE (satisfy isNumber token)
  where
    f = read . toList
    g = fromList . show

signed :: (Stream s, Token s ~ Char, Num a, Ord a) => Syntax s a -> Syntax s a
signed p = emap f g $ optional (exactToken '-') |*| p
  where
    f (Just (), x) = negate x
    f (Nothing, x) = x
    g x
      | x < 0 = (Just (), negate x)
      | otherwise = (Nothing, x)

float :: (Stream s, Token s ~ Char) => Syntax s Double
float = emap f g $ digits |*| optional (exactToken '.') |*| optional digits
  where
    digits = someE (satisfy isNumber token)
    -- this really needs to be cleaned up
    f (x1, (_, x2)) = read $ toList x1 ++ maybe "" ("." ++) (toList <$> x2)
    g x = (fromList (show x), (Nothing, Nothing))

escapedChar :: (Stream s, Token s ~ Char) => Syntax s Char
escapedChar = emap f g $
  exactToken '\\' |>| token |+| satisfy (not . (`elem` esc)) token
  where
    f (Left c) = c
    f (Right c) = c

    g c = if c `elem` esc then Left c else Right c

    esc = "\\\"'"

stringLit :: (Stream s, Token s ~ Char) => Syntax s String
stringLit = between q q (manyE escapedChar)
  where
    q = exactToken '"'
