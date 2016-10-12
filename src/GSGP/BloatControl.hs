module GSGP.BloatControl (
  BloatControlFunction
, parsimonyPressure
) where

import qualified GSGP.Data as D
import GSGP.Statistics (covariance, variance)
import GSGP.Language (programSize)
import GSGP.World (Population, Individual, indProgram, indFitness)


type BloatControlFunction l o f = Population l o f -> Individual l o f -> f


parsimonyPressure :: (Fractional f) => BloatControlFunction l o f
parsimonyPressure population =
  let indSizes     = D.fromList2 [fmap (fromIntegral . programSize . indProgram) population]
      indFitnesses = D.fromList2 [fmap indFitness population]

      pCoeff = covariance indSizes indFitnesses / (1 + variance indSizes)
  in \i -> indFitness i - pCoeff * fromIntegral (programSize (indProgram i))
