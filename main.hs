{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception
import Prelude
import Numeric
import System.Random
import Data.List
import Data.List.Split
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
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
cordinatClasterFind xs ms = (sum $  zipWith (*) xs $ map (**2) ms) / (sum $ map (**2) ms)

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

randomCenters :: [[Double]] -> Int -> StdGen -> [[Double]]
randomCenters xs n gen = take n xs

clasterizationStart :: RangeFunction -> [[Double]] -> InputConfigs -> StdGen -> [[Double]]
clasterizationStart func xs configs gen = clasterization func xs ms eps
  where ms = suppliesMatrix func xs $ randomCenters xs n gen
        n = number configs
        eps = epsilon configs

-- =====  parsing =====

handleAll :: (SomeException -> IO a) -> IO a -> IO a
handleAll = handle

convertFromCsv :: InputConfigs -> V.Vector (Row String) -> [[Double]]
convertFromCsv configs = processCsv . V.toList
    where processCsv = filter (not . null) . map processRow . withoutHeader
          processRow = map (fromMaybe 0.0) . filter isJust . map maybeRead . clearRow
          maybeRead = fmap fst . listToMaybe . (reads :: String -> [(Double, String)])
          withoutHeader x = if header configs then tail x else x
          clearRow x = withoutNumber $ withoutLabel x
          withoutNumber x = if rowNumber configs then tail x else x
          withoutLabel x = if label configs then init x else x

-- =====  output  =====

convertToCsv :: InputConfigs -> [[Double]] -> [B.ByteString]
convertToCsv configs = intersperse (BS.pack [13, 10]) . map (B.pack . intercalate (delemiter configs) . map ((\f -> f "") . showFFloat (Just 6)))

buildOutputHandle :: InputConfigs -> IO Handle
buildOutputHandle configs
    | outputFile configs /= ""  = handleAll (\e -> do { putStrLn $ "Cannot open file for output, using screen: " ++ show e; return stdout }) (openFile (outputFile configs) WriteMode)
    | otherwise            = return stdout

-- =====  arguments =====

data InputConfigs = InputConfigs {
  delemiter :: String
  ,inputFile :: FilePath
  ,outputFile :: FilePath
  ,number :: Int
  ,epsilon :: Double
  ,metrick :: Int
  ,header :: Bool
  ,rowNumber :: Bool
  ,label :: Bool
  } deriving (Show, Data, Typeable)

defaultInputConfigs = InputConfigs {
  delemiter = ","                         &= help "Csv delemiter"
  ,inputFile = "butterfly.txt"            &= help "Input file name"
  ,outputFile = ""                        &= help "Output file name (default console)"
  ,number = 3                             &= help "Clusters number"
  ,epsilon = 0.00000001                   &= help "Epsilon value"
  ,metrick = 0                            &= help "Metrick: 0 - Hamming, 1 - Evclide"
  ,header = False                         &= help "Have csv header?"
  ,rowNumber = False                      &= help "Have csv number (row's head)?"
  ,label = False                          &= help "Have csv class label (row's last)"
}  &= summary "Lab1 FCM 2015" &= program "lab1"

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

  rand <- getStdGen

  input <- handleAll (\e -> error $ "Cannot read input file: " ++ show e ) $ runResourceT $ readCSVFile csvOpts $ inputFile configs

  let result =  clasterizationStart (currentMetrick configs) (convertFromCsv configs input) configs rand

  runResourceT $ CL.sourceList (convertToCsv configs result) $$ CB.sinkIOHandle (buildOutputHandle configs)
