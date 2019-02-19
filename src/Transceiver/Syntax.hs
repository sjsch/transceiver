{-# LANGUAGE FlexibleContexts #-}

{-|
module      : Transceiver.Syntax
description : Syntax combinators.
-}
module Transceiver.Syntax
  ( Syntax
  , token
  , runParser
  , runPrinter
  , eof
  , exactLiteral
  , exactToken
  ) where

import           Data.Functor
import           Data.Functor.Exp
import           Data.Functor.Product

import           Data.Stream
import           Transceiver.Parser
import           Transceiver.Printer

-- | The main type representing a syntax combinator: it's just a pair
-- with a parser and a printer.  This isn't a newtype so users can
-- make them manually from components with 'Pair' or split them apart
-- with pattern matching.
type Syntax s a = Product (Parser s) (Printer s) a

-- | Syntax for a single token in the IO stream.
token :: Stream s => Syntax s (Token s)
token = Pair parseToken printToken

-- | Succeeds in parsing /only/ the empty stream, prints nothing.
eof :: Stream s => Syntax s ()
eof = Pair parseEof printEof

-- | Syntax for an exact match of a single token.  Produces/consumes
-- an '()' because an exact match carries no information.
exactToken :: (Stream s, Eq (Token s)) => Token s -> Syntax s ()
exactToken x = Pair (void $ parseSatisfy parseToken (== x)) (printInsert x)

-- | Syntax for an exact match of a sequence of tokens, like 'exactToken'.
exactLiteral :: (Stream s, Eq (Token s)) => [Token s] -> Syntax s ()
exactLiteral [] = combineId ()
exactLiteral (x:xs) =
  emap (const ()) (const ((), ())) $ combine (exactToken x) (exactLiteral xs)

-- | Parse the input stream @s@, and if it succeeds, return the
-- resulting @a@ and the unconsumed input.
runParser :: Syntax s a -> s -> Maybe (a, s)
runParser (Pair (Parser p) _) = p

-- | Print an @a@, appending the results of the print with the given
-- stream.
runPrinter :: Syntax s a -> a -> s -> s
runPrinter (Pair _ (Printer p)) = p
