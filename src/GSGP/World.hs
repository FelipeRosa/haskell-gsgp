module GSGP.World (
  GeneticOperator
, EvalFunction
, FitnessFunction
, SelectionFunction
, Individual (..)
, Population
, GeneticLanguage (..)
, World (..)
, applyGeneticOperator
) where

import Control.Monad (forM)

import Data.Random (RVar)
import Data.List (transpose)

import GSGP.Data (Dataset)
import qualified GSGP.Data as D
import GSGP.Language (Program, programCode, LanguageConstant, languageConstant)


type GeneticOperator l       = [Program l] -> Program l
type EvalFunction    l i o   = Dataset i -> l -> o
type FitnessFunction o f     = Dataset o -> f
type SelectionFunction l o f = Population l o f -> RVar (Individual l o f)

data Individual l o f =
  Individual {
    indProgram   :: Program l
  , indPhenotype :: Dataset o
  , indFitness   :: f
  }

instance (Show l, Show f) => Show (Individual l o f) where
  show ind = concat ["Program: ", show (indProgram ind), "\nFitness: ", show (indFitness ind)]

type Population l o f = [Individual l o f]


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
      childPhenotype = (flip D.fromList (length inds, 1)) . fmap ((evalFn inputs . programCode) . opFn . fmap languageConstant) . transpose . fmap (D.toList . indPhenotype) $ inds
      childFitness = fitnessFn childPhenotype
  in
    Individual childProgram childPhenotype childFitness
