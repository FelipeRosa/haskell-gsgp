module GSGP.Language (
  Language (..)
, CreationStrategy (..)
, FullStrat (..)
, GrowStrat (..)
) where

import Data.Random (RVar, uniform, sample)


class Language l where
  randomTerminal :: RVar l
  randomFunction :: RVar l -> RVar l


class CreationStrategy s where
  randomProgram :: (Language l) => s -> RVar l


data FullStrat = FullStrat Int deriving (Eq, Show)

instance CreationStrategy FullStrat where
  randomProgram (FullStrat n)
    | n <= 0    = randomTerminal
    | otherwise = randomFunction (randomProgram (FullStrat (n - 1)))


data GrowStrat = GrowStrat Int deriving (Eq, Show)

instance CreationStrategy GrowStrat where
  randomProgram (GrowStrat n)
    | n <= 0    = randomTerminal
    | otherwise = do
      k <- uniform 0 1 :: RVar Int

      if k == 0
        then randomTerminal
        else randomFunction (randomProgram (GrowStrat (n - 1)))
