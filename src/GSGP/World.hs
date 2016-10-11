module GSGP.World (
  GeneticOperator
, EvalFunction
, FitnessFunction
, Individual (..)
, GeneticLanguage (..)
, World (..)
, applyGeneticOperator
) where

import Data.Random (RVar)
import Data.List (transpose)

import GSGP.Data (Dataset)
import qualified GSGP.Data as D
import GSGP.Language (LanguageConstant, languageConstant)


type GeneticOperator l     = [l] -> l
type EvalFunction    l i o = Dataset i -> l -> o
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


applyGeneticOperator ::
  (LanguageConstant l o) =>
  GeneticOperator l -> EvalFunction l i o -> FitnessFunction o f -> Dataset i -> [Individual l o f] -> Individual l o f
applyGeneticOperator opFn evalFn fitnessFn inputs inds =
  let childProgram = opFn (fmap indProgram inds)
      childPhenotype = (flip D.fromList (length inds, 1)) . fmap ((evalFn inputs) . opFn . fmap languageConstant) . transpose . fmap (D.toList . indPhenotype) $ inds
      childFitness = fitnessFn childPhenotype
  in
    Individual childProgram childPhenotype childFitness
