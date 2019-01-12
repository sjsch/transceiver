{-|
module      : Transceiver.Parser
description : Covariant parser combinators
-}
module Transceiver.Parser
  ( Parser(..)
  , parseToken
  , parseEof
  , parseSatisfy
  ) where

import           Control.Applicative
import           Control.Monad

import           Data.Stream

-- | Since not every input string is a valid representation of the
-- output type, parsers must return a 'Maybe'.  This is a functor
-- covariant in its argmuent.
newtype Parser s a =
  Parser (s -> Maybe (a, s))

instance Functor (Parser s) where
  fmap f (Parser p) =
    Parser $ \s -> do
      (a, s') <- p s
      pure (f a, s')

instance Applicative (Parser s) where
  pure x = Parser $ \s -> pure (x, s)
  (Parser f) <*> (Parser p) =
    Parser $ \s -> do
      (f', s') <- f s
      (a, s'') <- p s'
      pure (f' a, s'')

instance Alternative (Parser s) where
  empty = Parser (const Nothing)
  (Parser a) <|> (Parser b) = Parser $ \s -> a s <|> b s

instance Monad (Parser s) where
  (Parser a) >>= f = Parser $ \s -> do
    (a', s') <- a s
    let Parser b = f a'
    b s'

-- | Parse one token from the input stream, failing only if the input
-- is empty.
parseToken :: Stream s => Parser s (Token s)
parseToken = Parser $ \s -> unconsStream s

-- | Succeed if the input stream is empty, fail otherwise.
parseEof :: Stream s => Parser s ()
parseEof =
  Parser $ \s ->
    case unconsStream s of
      Just _  -> Nothing
      Nothing -> Just ((), emptyStream)

-- | Succeed if the result of the parse matches the predicate.
parseSatisfy :: Stream s => Parser s a -> (a -> Bool) -> Parser s a
parseSatisfy (Parser p) f =
  Parser $ \s -> do
    (a, s') <- p s
    guard (f a)
    pure (a, s')
