{-# LANGUAGE MultiParamTypeClasses #-}

module GSGP.Language.Arith (
  Arith (..)
, evalArith
) where

import Data.Random (uniform)

import Control.Monad (forM)

import GSGP.Language
import GSGP.Data (Dataset, elementAt)
import GSGP.World (GeneticLanguage (..))


data Arith =
    C Double
  | I Int
  | Add Arith Arith
  | Sub Arith Arith
  | Mul Arith Arith

instance Show Arith where
  show (C e) = show e
  show (I i) = concat ["x", show i]
  show (Add a1 a2) = concat ["(", show a1, " + ", show a2, ")"]
  show (Sub a1 a2) = concat ["(", show a1, " - ", show a2, ")"]
  show (Mul a1 a2) = concat ["(", show a1, " * ", show a2, ")"]

instance Language Arith where
  randomTerminal = do
    k  <- uniform 0 10
    ix <- uniform 0 12
    let ts = [C k, I ix]
    n <- uniform 0 (length ts - 1)

    return $ Program (ts !! n) 1

  randomFunction randArg = do
    [arg1, arg2] <- forM [1..2] $ \_ -> randArg
    let fs = [Add, Sub, Mul]
    n <- uniform 0 (length fs - 1)

    let code     = (fs !! n) (programCode arg1) (programCode arg2)
        codeSize = programSize arg1 + programSize arg2 + 1

    return $ Program code codeSize

instance LanguageConstant Arith Double where
  languageConstant = flip Program 1 . C

instance GeneticLanguage Arith where
  generateMutationFunction = do
    t1 <- randomProgram (GrowStrat 2)
    t2 <- randomProgram (GrowStrat 2)
    return $ \[t] ->
      let code     = Add (programCode t) (Mul (C 0.1) (Sub (programCode t1) (programCode t2)))
          codeSize = programSize t + programSize t1 + programSize t2 + 4
      in
        Program code codeSize

  generateCrossoverFunction = do
    a <- uniform 0 1
    let b = 1 - a
    return $ \[t1, t2] ->
      let code     = Add (Mul (C a) (programCode t1)) (Mul (C b) (programCode t2))
          codeSize = programSize t1 + programSize t2 + 5
      in
        Program code codeSize


evalArith :: Dataset Double -> Arith -> Double
evalArith _      (C k)       = k
evalArith inputs (I ix)      = elementAt inputs (ix, 0)
evalArith inputs (Add a1 a2) = evalArith inputs a1 + evalArith inputs a2
evalArith inputs (Sub a1 a2) = evalArith inputs a1 - evalArith inputs a2
evalArith inputs (Mul a1 a2) = evalArith inputs a1 * evalArith inputs a2
