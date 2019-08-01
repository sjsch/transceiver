{-# LANGUAGE DeriveGeneric #-}

module Csv (CsvValue(..), Csv, csv) where

import Transceiver
import Transceiver.Syntax.Char

data CsvValue = CsvNum Double
              | CsvString String
              deriving (Eq, Ord, Show, Generic)

type Csv = [[CsvValue]]

csvValue :: Syntax String CsvValue
csvValue = gmap $
  signed float |+| stringLit

csv :: Syntax String Csv
csv = (csvValue `sepBy` exactToken ',') `sepBy` exactToken '\n'
