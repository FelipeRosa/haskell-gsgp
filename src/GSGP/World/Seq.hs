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


data SeqWorldParams =
  SeqWorldParams {
    swpMutRate   :: Double
  , swpCrossRate :: Double
  } deriving (Eq, Show)

data SeqWorld l i o f =
  SeqWorld {
    swParams         :: SeqWorldParams
  , swTrainInput     :: Dataset i
  , swPopulation     :: Population l o f
  , swEvalFn         :: EvalFunction l i o
  , swFitnessFn      :: FitnessFunction o f
  , swSelectionFn    :: (Individual l o f -> f) -> SelectionFunction l o f
  , swBloatControlFn :: BloatControlFunction l o f
  }

instance (LanguageConstant l o, GeneticLanguage l) => World (SeqWorld l i o f) where
  worldNextGeneration w = do
    let population = swPopulation w
        pN = length population

        mutN   = truncate ((swpMutRate . swParams $ w) * fromIntegral pN)
        crossN = truncate ((swpCrossRate . swParams $ w) * fromIntegral pN)

        adjustedFitnessFn = (swBloatControlFn w) population

    selectedForMut   <- shuffleNofM mutN pN population
    selectedForCross <- do
      inds1 <- forM [1..crossN] $ \_ -> (swSelectionFn w adjustedFitnessFn) population
      inds2 <- forM [1..crossN] $ \_ -> (swSelectionFn w adjustedFitnessFn) population
      return $ zipWith (\a b -> [a, b]) inds1 inds2

    mutResults <- forM selectedForMut $ \ind -> do
      mutFn <- generateMutationFunction
      return $ applyGeneticOperator mutFn (swEvalFn w) (swFitnessFn w) (swTrainInput w) [ind]

    crossResults <- forM selectedForCross $ \inds -> do
      crossFn <- generateCrossoverFunction
      return $ applyGeneticOperator crossFn (swEvalFn w) (swFitnessFn w) (swTrainInput w) inds

    let iPopulation = concat [population, mutResults, crossResults]
        iAdjustedFitnessFn = (swBloatControlFn w) iPopulation

    nextPopulation <- forM [1..pN] $ \_ -> (swSelectionFn w iAdjustedFitnessFn) iPopulation

    return $ w { swPopulation = nextPopulation }
