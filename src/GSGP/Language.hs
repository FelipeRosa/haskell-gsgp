{-# LANGUAGE MultiParamTypeClasses #-}

module GSGP.Language (
  Program (..)
, Language (..)
, LanguageConstant (..)
, CreationStrategy (..)
, FullStrat (..)
, GrowStrat (..)
) where

import Data.Function (on)
import Data.Random (RVar, uniform, sample)


data Program l =
  Program {
    programCode :: l
  , programSize :: Int
  }

instance (Eq l) => Eq (Program l) where
  (==) = (==) `on` programCode

instance (Show l) => Show (Program l) where
  show p = concat ["[Program code=", show (programCode p), ", Program Size=", show (programSize p), "]"]


class Language l where
  randomTerminal :: RVar (Program l)
  randomFunction :: RVar (Program l) -> RVar (Program l)

class LanguageConstant l e where
  languageConstant :: e -> Program l


class CreationStrategy s where
  randomProgram :: (Language l) => s -> RVar (Program l)


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
