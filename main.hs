module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

-- ===== constants =====

m = 2

-- ===== ranges =====

hammingRange :: [Double] -> [Double] -> Double
hammingRange x y = foldl (+) 0 $ map abs $ zipWith (-) x y

evclideRange :: [Double] -> [Double] -> Double
evclideRange x y = sqrt $ foldl (+) 0 $ map (** 2) $ zipWith (-) x y

type RangeFunction = ([Double] -> [Double] -> Double)

-- =====  supplies matrix =====

suppliesCof :: RangeFunction -> [Double] -> [Double] -> [Double] -> Double
suppliesCof func x v y = (cof1 / cof2) ** exp
  where cof1 = func x v
        cof2 = func x y
        exp =  2 / (m - 1)

suppliesValue :: RangeFunction -> [Double] -> [Double] -> [[Double]] -> Double
suppliesValue func x v vs = cof ** (-1)
  where cof = foldl (+) 0 $ map (\y -> suppliesCof func x v y) vs

suppliesRow :: RangeFunction -> [Double] -> [[Double]] -> [Double]
suppliesRow func x vs = map (\v -> suppliesValue func x v vs) vs

suppliesMatrix :: RangeFunction -> [[Double]] -> [[Double]] -> [[Double]]
suppliesMatrix func xs vs = map (\x -> suppliesRow func x vs) xs

-- =====  parsing =====

type Groups = ([Int])

main :: IO ()

main = do
  csvData <- BL.readFile "butterfly.txt"
  let csv = decode NoHeader csvData :: Either String (V.Vector Groups)

  print "Hello"
