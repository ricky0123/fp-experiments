{-# LANGUAGE BlockArguments #-}

module Main where

import qualified System.Environment as SE

import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, forM, replicateM)
import Text.Read (readMaybe)
import Numeric.Sampling (sampleIO)
import Data.Maybe (fromMaybe)
import Data.List ((!?))

helpString :: String
helpString = "Usage: x13-german-tank <sample-size> <max-value>"

parseArgs :: [String] -> Either String (Int, Int)
parseArgs args
    | length args /= 2 = Left "We need two args"
    | otherwise = do
        parsed <- forM args \x -> do
            case readMaybe x of
                Nothing -> Left $ x <> " is not an integer"
                Just n -> Right n
        let (sampleSize, maxValue) = (parsed !! 0, parsed !! 1)
        when (sampleSize > maxValue) $ Left "sample size cannot be greater than observed max"
        return (sampleSize, maxValue)

returnError :: String -> IO ()
returnError errString = do
    putStrLn errString
    putStrLn helpString

app :: IO ()
app = do
    args <- SE.getArgs
    case parseArgs args of
        Left err -> returnError err
        Right (sampleSize, maxValue) -> simulation sampleSize maxValue

defaultNRuns :: Int
defaultNRuns = 50

simulation :: Int -> Int -> IO ()
simulation sampleSize maxValue = do
    let estimator = 2 * maxValue
    (bootstrapMean, bootstrapStddev) <- runTrials defaultNRuns sampleSize estimator
    putStrLn $ "Mean   " <> show bootstrapMean
    putStrLn $ "Stddev " <> show bootstrapStddev

runTrials :: Int -> Int -> Int -> IO (Float, Float)
runTrials nRuns sampleSize estimator = do
    let nRuns' = fromIntegral nRuns
    bootstrapSample <- replicateM nRuns do
        maybeSample <- sampleIO sampleSize [1..estimator]
        let sample = fromMaybe [-1] maybeSample
        let sampleMax = maximum sample
        return (fromIntegral sampleMax :: Float)
    let
        bootstrapMean = sum bootstrapSample / nRuns'
        deviations = map (\x -> (x - bootstrapMean) ** 2) bootstrapSample
        bootstrapStddev = sqrt (sum deviations / nRuns')
    return (bootstrapMean, bootstrapStddev)

main :: IO ()
main = app
