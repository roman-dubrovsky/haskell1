{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception
import Prelude
import Data.List
import Data.List.Split
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Maybe
import System.Console.CmdArgs

import Data.CSV.Conduit
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

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

validSuppliesValue :: RangeFunction -> [Double] -> [Double] -> [[Double]] -> Double
validSuppliesValue func x v vs = if isNaN value then 1 else value
  where value = suppliesValue func x v vs

suppliesRow :: RangeFunction -> [Double] -> [[Double]] -> [Double]
suppliesRow func x vs = map (\v -> validSuppliesValue func x v vs) vs

suppliesMatrix :: RangeFunction -> [[Double]] -> [[Double]] -> [[Double]]
suppliesMatrix func xs vs = map (\x -> suppliesRow func x vs) xs

-- =====  clasters find  =====

cordinatClasterFind :: [Double] -> [Double] -> Double
cordinatClasterFind xs ms = sum1 / sum2
  where sum1 = sum $ zipWith (*) xs $ map (**exp) ms
        sum2 = sum ms
        exp = m

oneClasterFind :: [[Double]] -> [Double] -> [Double]
oneClasterFind xs ms = map (\x -> cordinatClasterFind x ms) $ transpose xs

clastersFind :: [[Double]] -> [[Double]] -> [[Double]]
clastersFind xs ms = map (\m -> oneClasterFind xs m) $ transpose ms

-- =====  clasterization  =====

isClasterizationFinished :: [[Double]] -> [[Double]] -> Double -> Bool
isClasterizationFinished old new eps = if eps > cof then True else False
  where cof = maximum $ map abs $ zipWith (-) old_c new_c
        old_c = concat old
        new_c = concat new

clasterization :: RangeFunction -> [[Double]] -> [[Double]] -> Double -> [[Double]]
clasterization func xs ms eps
  | isClasterizationFinished ms ms_new eps = ms_new
  | otherwise = clasterization func xs ms_new eps
  where ms_new = suppliesMatrix func xs $ clastersFind xs ms

-- =====  clasterization starter  =====

clasterizationStart :: RangeFunction -> [[Double]] -> InputConfigs -> [[Double]]
clasterizationStart func xs configs = clasterization func xs ms eps
  where ms = suppliesMatrix func xs $ take n xs
        n = number configs
        eps = epsilon configs

-- =====  parsing =====

handleAll :: (SomeException -> IO a) -> IO a -> IO a
handleAll = handle

convertFromCsv :: V.Vector (Row String) -> [[Double]]
convertFromCsv = processCsv . V.toList
    where processCsv = filter (not . null) . map processRow
          processRow = map (fromMaybe 0.0) . filter isJust . map maybeRead . filterEmpty
          filterEmpty = filter (\s -> T.strip (T.pack s) /= T.pack "")
          maybeRead x = (fmap fst . listToMaybe . (reads :: String -> [(Double, String)])) x

-- =====  arguments =====

data InputConfigs = InputConfigs {
  delemiter :: String
  ,inputFile :: FilePath
  ,number :: Int
  ,epsilon :: Double
  ,metrick :: Int
  } deriving (Show, Data, Typeable)

defaultInputConfigs = InputConfigs {
  delemiter = ","                         &= help "Csv delemiter"
  ,inputFile = "butterfly.txt"            &= help "Input file name"
  ,number = 3                             &= help "Clusters number"
  ,epsilon = 0.001                        &= help "Epsilon value"
  ,metrick = 0                            &= help "Metrick: 0 - Hamming, 1 - Evclide"
}

currentMetrick :: InputConfigs -> RangeFunction
currentMetrick configs
  | m == 1 = evclideRange
  | otherwise = hammingRange
  where m = metrick configs

-- =====  main  =====

main :: IO ()
main = do
  configs <- cmdArgs defaultInputConfigs
  let csvOpts = defCSVSettings {csvSep = (head (delemiter configs)), csvQuoteChar = Nothing}

  input <- handleAll (\e -> error $ "Cannot read input file: " ++ show e ) $ runResourceT $ readCSVFile csvOpts $ inputFile configs

  let result =  clasterizationStart (currentMetrick configs) (convertFromCsv input) configs

  print result
