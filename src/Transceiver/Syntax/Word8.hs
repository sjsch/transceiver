{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-|
module      : Transceiver.Syntax.Word8
description : Syntax combinators for dealing with bytestrings.
-}
module Transceiver.Syntax.Word8
  ( syntaxLE
  ) where

import           Data.Bits
import           Data.Word

import           Data.Functor.Exp
import           Data.Stream
import           Transceiver.Combinators
import           Transceiver.Syntax

unfoldLE :: (FiniteBits n, Integral n, Num n) => n -> [Word8]
unfoldLE x = go (finiteBitSize x `div` 8) x
  where
    go 0 _ = []
    go n y = fromIntegral (y .&. 0xff) : go (n - 1) (y `shiftR` 8)

foldLE :: (Bits n, Num n) => [Word8] -> n
foldLE = foldr f 0
  where
    f x a = a `shiftL` 8 + fromIntegral x

-- | Syntax for a little-endian 'Word' of arbitrary length.
--
-- >>> runPrinter syntaxLE (1234 :: Word32) ("" :: ByteString)
-- "\210\EOT\NUL\NUL"
syntaxLE ::
     forall n s. (Stream s, Token s ~ Word8, FiniteBits n, Integral n, Num n)
  => Syntax s n
syntaxLE = emap foldLE unfoldLE $ repeatN (finiteBitSize (0 :: n) `div` 8) token
