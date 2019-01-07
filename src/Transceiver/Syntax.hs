module Transceiver.Syntax where

import           Data.Char
import           Data.Functor.Product

import           Data.Functor.Exp
import           Data.Stream
import           Transceiver.Parser
import           Transceiver.Printer

type Syntax s a = Product (Printer s) (Parser s) a

token :: Stream s => Syntax s (Token s)
token = Pair putToken takeToken

runParser :: Stream s => Syntax s a -> s -> Maybe a
runParser (Pair _ p) s =
  case parse p s of
    Just (a, _) -> Just a
    Nothing     -> Nothing

runPrinter :: Stream s => Syntax s a -> a -> s
runPrinter (Pair p _) a = pprint p a emptyStream

type Syn a = Syntax String a

digit :: Syn Int
digit = emap ((+ (-ord '0')) . ord) (chr . (+ ord '0')) token

eof :: Stream s => Syntax s ()
eof = Pair putNothing endStream

takeMany :: Stream s => Syntax s a -> Syntax s [a]
takeMany e = emap f g $ pick eof (takeSome e)
  where
    f (Left ())  = []
    f (Right xs) = xs
    g [] = Left ()
    g xs = Right xs

takeSome :: Stream s => Syntax s a -> Syntax s [a]
takeSome e = emap f g $ combine e (takeMany e)
  where
    f = uncurry (:)
    g (x:xs) = (x, xs)
    g _      = error "some only works for nonempty lists"
