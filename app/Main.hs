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
import GSGP.BloatControl


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

    return $ Program (ts !! n) 1

  randomFunction randArg = do
    [arg1, arg2] <- forM [1..2] $ \_ -> randArg
    let fs = [Add, Sub, Mul]
    n <- uniform 0 (length fs - 1)

    let code     = (fs !! n) (programCode arg1) (programCode arg2)
        codeSize = programSize arg1 + programSize arg2 + 1

    return $ Program code codeSize

instance LanguageConstant Arith Double where
  languageConstant = flip Program 1 . C

instance GeneticLanguage Arith where
  generateMutationFunction = do
    t1 <- randomProgram (GrowStrat 2)
    t2 <- randomProgram (GrowStrat 2)
    return $ \[t] ->
      let code     = Add (programCode t) (Mul (C 0.1) (Sub (programCode t1) (programCode t2)))
          codeSize = programSize t + programSize t1 + programSize t2 + 4
      in
        Program code codeSize

  generateCrossoverFunction = do
    a <- uniform 0 1
    let b = 1 - a
    return $ \[t1, t2] ->
      let code     = Add (Mul (C a) (programCode t1)) (Mul (C b) (programCode t2))
          codeSize = programSize t1 + programSize t2 + 5
      in
        Program code codeSize

runArith :: Dataset Double -> Arith -> Double
runArith _      (C k)       = k
runArith inputs (I ix)      = elementAt inputs (ix, 0)
runArith inputs (Add a1 a2) = runArith inputs a1 + runArith inputs a2
runArith inputs (Sub a1 a2) = runArith inputs a1 - runArith inputs a2
runArith inputs (Mul a1 a2) = runArith inputs a1 * runArith inputs a2


main :: IO ()
main = do
  ds <- loadTxt "/Users/felipe/Desktop/faculdade/POC/housing.data.txt" :: IO (Dataset Double)

  -- let trainInput  = slice ds (0, 12, 204, 503)
  --     trainOutput = slice ds (13, 13, 204, 503)
  --
  --     testInput  = slice ds (0, 12, 0, 203)
  --     testOutput = slice ds (13, 13, 0, 203)
  let (testInput, testOutput, trainInput, trainOutput) = splitRC ds 204 13

      fitnessFn p = 1 / (1 + mae trainOutput p)
      selectionFn = tournamentSelection 7

  initialPopulation <- sample $ forM [1..1000] $ \_ -> do
    l <- randomProgram (GrowStrat 2)
    let p = mapR (flip runArith (programCode l)) trainInput
        f = fitnessFn p
    return $ Individual l p f

  let firstWorld = SeqWorld {
      swParams = SeqWorldParams {
        swpMutRate    = 0.4
      , swpCrossRate  = 0.3
      , swpTrainInput = trainInput
      , swpEvalFn      = runArith
      , swpFitnessFn   = fitnessFn
      , swpSelectionFn = selectionFn
      , swpBloatControlFn = parsimonyPressure
      }
    , swPopulation = initialPopulation
    }

  (lastWorld, fitnesses, _) <- sample $ flip (iterateUntilM (\(_, _, g) -> g > 50)) (firstWorld, [], 1) $ \(w, fs, g) -> do
    w' <- worldNextGeneration w
    let best = maximumBy (compare `on` indFitness) (swPopulation w')
    return (w', indFitness best : fs, g + 1)

  let best = maximumBy (compare `on` indFitness) (swPopulation lastWorld)
      bestP = mapR (flip runArith (programCode . indProgram $ best)) testInput
      maxProgramSize = maximum (fmap (programSize . indProgram) (swPopulation lastWorld))

  putStrLn $ show fitnesses
  putStrLn $ show (indProgram best)
  putStrLn $ show (rmse testOutput bestP)
  putStrLn $ show maxProgramSize

  saveTxt "/Users/felipe/Desktop/faculdade/POC/haskell-pred.txt" (fromList2 (transpose [fmap fromIntegral [0..count bestP - 1], toList bestP]))

  return ()
