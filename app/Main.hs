{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Random
import Data.List
import Data.Function

import Control.Monad
import Control.Monad.Loops

import Debug.Trace (trace)

import GSGP.Data
import GSGP.Language
import GSGP.Metrics
import GSGP.Selection
import GSGP.World
import GSGP.World.Seq


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
    ix <- uniform 0 12
    let ts = [C k, I ix]
    n <- uniform 0 (length ts - 1)

    return (ts !! n)

  randomFunction randArg = do
    [arg1, arg2] <- forM [1..2] $ \_ -> randArg
    let fs = [Add, Sub, Mul]
    n <- uniform 0 (length fs - 1)

    return $ (fs !! n) arg1 arg2

instance LanguageConstant Arith Double where
  languageConstant = C

instance GeneticLanguage Arith where
  generateMutationFunction = do
    t1 <- randomProgram (GrowStrat 2)
    t2 <- randomProgram (GrowStrat 2)
    return $ \[t] -> Add t (Mul (C 0.1) (Sub t1 t2))

  generateCrossoverFunction = do
    a <- uniform 0 1
    let b = 1 - a
    return $ \[t1, t2] -> Add (Mul (C a) t1) (Mul (C b) t2)

runArith :: Dataset Double -> Arith -> Double
runArith _      (C k)       = k
runArith inputs (I ix)      = elementAt inputs (ix, 0)
runArith inputs (Add a1 a2) = runArith inputs a1 + runArith inputs a2
runArith inputs (Sub a1 a2) = runArith inputs a1 - runArith inputs a2
runArith inputs (Mul a1 a2) = runArith inputs a1 * runArith inputs a2


main :: IO ()
main = do
  ds <- loadTxt "/Users/felipe/Desktop/housing.data.txt" :: IO (Dataset Double)

  let trainInput  = slice ds (0, 12, 0, 300)
      trainOutput = slice ds (13, 13, 0, 300)

      testInput  = slice ds (0, 12, 301, 504)
      testOutput = slice ds (13, 13, 301, 504)

      fitnessFn p = 1 / (1 + mae trainOutput p)
      selectionFn = tournamentSelection 7

  initialPopulation <- sample $ forM [1..1000] $ \_ -> do
    l <- randomProgram (GrowStrat 2)
    let p = mapR (flip runArith l) trainInput
        f = fitnessFn p
    return $ Individual l p f

  let firstWorld = SeqWorld {
      swParams = SeqWorldParams 0.4 0.3
    , swTrainInput = trainInput
    , swPopulation = initialPopulation
    , swEvalFn = runArith
    , swFitnessFn = fitnessFn
    , swSelectionFn = selectionFn
    }

  (lastWorld, fitnesses, _) <- sample $ flip (iterateUntilM (\(_, _, g) -> g > 5)) (firstWorld, [], 1) $ \(w, fs, g) -> do
    w' <- worldNextGeneration w
    let best = maximumBy (compare `on` indFitness) (swPopulation w')
    return (w, indFitness best : fs, g + 1)

  let best = maximumBy (compare `on` indFitness) (swPopulation lastWorld)
      bestP = mapR (flip runArith (indProgram best)) testInput

  putStrLn $ show fitnesses
  putStrLn $ show (rmse testOutput bestP)

  saveTxt "/Users/felipe/Desktop/haskell-pred.txt" (fromList2 (transpose [fmap fromIntegral [0..count bestP - 1], toList bestP]))

  return ()
