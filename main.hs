module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

type Groups = ([Int])

main :: IO ()

main = do
  csvData <- BL.readFile "butterfly.txt"
  let csv = decode NoHeader csvData :: Either String (V.Vector Groups)

  print "Hello"
