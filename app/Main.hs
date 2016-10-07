module Main where

import Data.Random

import Control.Monad

import GSGP.Data
import GSGP.Language


data Arith =
    C Double
  | I Int
  | Add Arith Arith
  | Sub Arith Arith
  | Mul Arith Arith
  deriving (Show)

instance Language Arith where
  randomTerminal = do
    k  <- uniform 0 10
    let ts = [C k, I 0]
    n <- uniform 0 (length ts - 1)

    return (ts !! n)

  randomFunction randArg = do
    [arg1, arg2] <- forM [1..2] $ \_ -> randArg
    let fs = [Add, Sub, Mul]
    n <- uniform 0 (length fs - 1)

    return $ (fs !! n) arg1 arg2

runArith :: Arith -> [Double] -> Double
runArith (C k)       _      = k
runArith (I ix)      inputs = inputs !! ix
runArith (Add a1 a2) inputs = runArith a1 inputs + runArith a2 inputs
runArith (Sub a1 a2) inputs = runArith a1 inputs - runArith a2 inputs
runArith (Mul a1 a2) inputs = runArith a1 inputs * runArith a2 inputs


main :: IO ()
main = do
  ds <- loadTxt "/Users/felipe/Desktop/housing.data.txt" :: IO (Dataset Double)

  let firstLine = reshape ds (0, 13, 0, 0)

  putStrLn $ show (reshape ds (0, 13, 0, 1))
  putStrLn ""
  putStrLn $ show firstLine
  putStrLn $ concat ["Count: ", show (count firstLine)]
  putStrLn $ concat ["Average: ", show (sum firstLine / (fromIntegral $ count firstLine))]
  putStrLn ""

  --
  arithP <- sample (randomProgram (GrowStrat 3))

  putStrLn $ show (runArith arithP [1.7])

  return ()
