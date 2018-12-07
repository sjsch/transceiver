{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Syntax where

import Data.Functor.Product
import Data.Char
import Data.Functor.Foldable

import Stream
import Printer
import Parser
import Exp

type Syntax s a = Product (Printer s) (Parser s) a

token :: Stream s => Syntax s (Token s)
token = Pair putToken takeToken

runParser :: Stream s => Syntax s a -> s -> Maybe a
runParser (Pair _ p) s = case parse p s of
  Just (a, _) -> Just a
  Nothing -> Nothing

runPrinter :: Stream s => Syntax s a -> a -> s
runPrinter (Pair p _) a = pprint p a emptyStream

type Syn a = Syntax String a

digit :: Syn Int
digit = emap
  ((+ (-ord '0')) . ord)
  (chr . (+ (ord '0')))
  token

-- newtype List a = List (Either () (Int, a))

-- lol :: Fix List 
-- lol = Fix (List (Right (1, Fix (List (Left ())))))

digitComb p = combine digit p

-- list = emap Fix unfix $ pick digit