module Parser where

import Control.Applicative

import Stream

newtype Parser s a
  = Parser { parse :: s -> Maybe (a, s)
           }

instance Functor (Parser s) where
  fmap f p = Parser $ \s -> case parse p s of
    Just (a, s') -> Just (f a, s')
    Nothing -> Nothing

instance Applicative (Parser s) where
  pure x = Parser $ \s -> Just (x, s)
  f <*> p = Parser $ \s -> case parse f s of
    Just (f', s') -> case parse p s' of
      Just (p', s'') -> Just (f' p', s'')
      Nothing -> Nothing
    Nothing -> Nothing

instance Alternative (Parser s) where
  empty = Parser (const Nothing)
  a <|> b = Parser $ \s -> case parse a s of
    Just x -> Just x
    Nothing -> parse b s

takeToken :: Stream s => Parser s (Token s)
takeToken = Parser $ \s -> unconsStream s

endStream :: Stream s => Parser s ()
endStream = Parser $ \s -> case unconsStream s of
  Just _ -> Nothing
  Nothing -> Just ((), emptyStream)

satisfies :: Stream s => Parser s a -> (a -> Bool) -> Parser s a
satisfies p f = Parser $ \s -> case parse p s of
  Just r@(a, _) -> if f a then Just r else Nothing
  Nothing -> Nothing
