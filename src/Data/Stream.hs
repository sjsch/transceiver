{-|
module      : Data.Stream
description : List-like types that can be split into a head and a
              tail, and appended to.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Stream
  ( Stream(..)
  ) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List            as L
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.Word

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

instance Stream TL.Text where
  type Token TL.Text = Char
  emptyStream = ""
  appendStream = TL.snoc
  unconsStream = TL.uncons

instance Stream B.ByteString where
  type Token B.ByteString = Word8
  emptyStream = ""
  appendStream = B.snoc
  unconsStream = B.uncons

instance Stream BL.ByteString where
  type Token BL.ByteString = Word8
  emptyStream = ""
  appendStream = BL.snoc
  unconsStream = BL.uncons
