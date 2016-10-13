module GSGP.World.Seq (
  SeqWorldParams (..)
, SeqWorld (..)
) where

import Debug.Trace

import Control.Monad (forM)

import Data.Random (RVar, shuffleNofM)

import GSGP.Data (Dataset)
import GSGP.Language (LanguageConstant, programSize)
import GSGP.BloatControl (BloatControlFunction)
import GSGP.World


data SeqWorldParams l i o f =
  SeqWorldParams {
    swpMutRate        :: Double
  , swpCrossRate      :: Double
  , swpTrainInput     :: Dataset i
  , swpEvalFn         :: EvalFunction l i o
  , swpFitnessFn      :: FitnessFunction o f
  , swpSelectionFn    :: (Individual l o f -> f) -> SelectionFunction l o f
  , swpBloatControlFn :: BloatControlFunction l o f
  }

data SeqWorld l i o f =
  SeqWorld {
    swParams         :: SeqWorldParams l i o f
  , swPopulation     :: Population l o f
  }

instance (LanguageConstant l o, GeneticLanguage l) => World (SeqWorld l i o f) where
  worldNextGeneration w = do
    let population = swPopulation w
        pN = length population
        params = swParams w

        mutN   = truncate (swpMutRate params * fromIntegral pN)
        crossN = truncate (swpCrossRate params * fromIntegral pN)

        adjustedFitnessFn = swpBloatControlFn params population

    selectedForMut   <- shuffleNofM mutN pN population
    selectedForCross <- do
      inds1 <- forM [1..crossN] $ \_ -> (swpSelectionFn params adjustedFitnessFn) population
      inds2 <- forM [1..crossN] $ \_ -> (swpSelectionFn params adjustedFitnessFn) population
      return $ zipWith (\a b -> [a, b]) inds1 inds2

    mutResults <- forM selectedForMut $ \ind -> do
      mutFn <- generateMutationFunction
      return $ applyGeneticOperator mutFn (swpEvalFn params) (swpFitnessFn params) (swpTrainInput params) [ind]

    crossResults <- forM selectedForCross $ \inds -> do
      crossFn <- generateCrossoverFunction
      return $ applyGeneticOperator crossFn (swpEvalFn params) (swpFitnessFn params) (swpTrainInput params) inds

    let iPopulation = concat [population, mutResults, crossResults]
        iAdjustedFitnessFn = swpBloatControlFn params iPopulation

    nextPopulation <- forM [1..pN] $ \_ -> (swpSelectionFn params iAdjustedFitnessFn) iPopulation

    return $ w { swPopulation = nextPopulation }
