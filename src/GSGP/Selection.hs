module GSGP.Selection (
  tournamentSelection
) where

import Data.Random (RVar, shuffleNofM)
import Data.List (maximumBy)
import Data.Function (on)

import GSGP.World (Individual, SelectionFunction)


tournamentSelection :: (Ord f) => Int -> (Individual l o f -> f) -> SelectionFunction l o f
tournamentSelection tournamentSize adjustedFitnessFn population = do
  participants <- shuffleNofM tournamentSize (length population) population
  return $ maximumBy (compare `on` adjustedFitnessFn) participants
