{-
module      : Data.Stream
description : Typeclass for list-like types that can be split into a
              head and a tail, and append to.
-}
{-# LANGUAGE TypeFamilies #-}

module Data.Stream where

import           Data.List

class Stream s where
  type Token s
  emptyStream :: s
  appendStream :: s -> Token s -> s
  unconsStream :: s -> Maybe (Token s, s)

instance Stream [a] where
  type Token [a] = a
  emptyStream = []
  appendStream s c = s ++ [c]
  unconsStream = uncons
