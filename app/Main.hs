module Main where

import Data.Random
import Data.List
import Data.Function

import Control.Monad
import Control.Monad.Loops

import Debug.Trace (trace)

import GSGP.Data
import GSGP.Language
import GSGP.Language.Arith
import GSGP.Metrics
import GSGP.Selection
import GSGP.World
import GSGP.World.Seq
import GSGP.BloatControl


main :: IO ()
main = do
  ds <- loadTxt "/Users/felipe/Desktop/faculdade/POC/housing.data.txt" :: IO (Dataset Double)

  let (testInput, testOutput, trainInput, trainOutput) = splitRC ds 204 13

      fitnessFn p = 1 / (1 + mae trainOutput p)
      selectionFn = tournamentSelection 7

  initialPopulation <- sample $ forM [1..1000] $ \_ -> do
    l <- randomProgram (GrowStrat 2)
    let p = mapR (flip evalArith (programCode l)) trainInput
        f = fitnessFn p
    return $ Individual l p f

  let firstWorld = SeqWorld {
      swParams = SeqWorldParams {
        swpMutRate    = 0.4
      , swpCrossRate  = 0.3
      , swpTrainInput = trainInput
      , swpEvalFn      = evalArith
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
      bestP = mapR (flip evalArith (programCode . indProgram $ best)) testInput
      maxProgramSize = maximum (fmap (programSize . indProgram) (swPopulation lastWorld))

  putStrLn $ show fitnesses
  putStrLn $ show (indProgram best)
  putStrLn $ show (rmse testOutput bestP)
  putStrLn $ show maxProgramSize

  saveTxt "/Users/felipe/Desktop/faculdade/POC/haskell-pred.txt" (fromList2 (transpose [fmap fromIntegral [0..count bestP - 1], toList bestP]))

  return ()
