module GSGP.Selection (
  tournamentSelection
) where

import Data.Random (RVar, shuffleNofM)
import Data.List (maximumBy)
import Data.Function (on)

import GSGP.World (SelectionFunction, indFitness)


tournamentSelection :: (Ord f) => Int -> SelectionFunction l o f
tournamentSelection tournamentSize population = do
  participants <- shuffleNofM tournamentSize (length population) population
  return $ maximumBy (compare `on` indFitness) participants
