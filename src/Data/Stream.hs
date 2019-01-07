{-|
module      : Data.Stream
description : List-like types that can be split into a head and a
              tail, and appended to.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Stream where

import qualified Data.List as L
import qualified Data.Text as T

-- | Anything that can be @ucons@ed or @append@ed to, and that has an
-- empty element.  You can think of this as a monoid that can have the
-- first element split off.
class Stream s where
  type Token s
  emptyStream :: s
  appendStream :: s -> Token s -> s
  unconsStream :: s -> Maybe (Token s, s)

-- | Lists are trivially a 'Stream', which means 'String's are too.
instance Stream [a] where
  type Token [a] = a
  emptyStream = []
  appendStream s c = s ++ [c]
  unconsStream = L.uncons

instance Stream T.Text where
  type Token T.Text = Char
  emptyStream = ""
  appendStream = T.snoc
  unconsStream = T.uncons
