module GSGP.World (
  GeneticOperator
, EvalFunction
, FitnessFunction
, Individual (..)
, GeneticLanguage (..)
, World (..)
) where

import Data.Random (RVar)

import GSGP.Data (Dataset)


type GeneticOperator l     = [l] -> l
type EvalFunction    l i o = Dataset i -> l -> Dataset o
type FitnessFunction o f   = Dataset o -> f

data Individual l o f =
  Individual {
    indProgram   :: l
  , indPhenotype :: Dataset o
  , indFitness   :: f
  }


class GeneticLanguage l where
  generateMutationFunction  :: RVar (GeneticOperator l)
  generateCrossoverFunction :: RVar (GeneticOperator l)

class World w where
  worldNextGeneration :: w -> RVar w


applyGeneticOperator :: EvalFunction l i o -> FitnessFunction o f -> GeneticOperator l -> Dataset i -> Individual l o f -> Individual l o f
applyGeneticOperator evalFn fitnessFn opFn inputs ind = ind
