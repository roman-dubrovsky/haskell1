module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

-- ===== ranges =====

hammingRange :: Floating a => [a] -> [a] -> a
hammingRange x y = foldl (+) 0 $ map abs $ zipWith (-) x y

evclideRange :: Floating a => [a] -> [a] -> a
evclideRange x y = sqrt $ foldl (+) 0 $ map (** 2) $ zipWith (-) x y

type Groups = ([Int])

main :: IO ()

main = do
  csvData <- BL.readFile "butterfly.txt"
  let csv = decode NoHeader csvData :: Either String (V.Vector Groups)

  print "Hello"
