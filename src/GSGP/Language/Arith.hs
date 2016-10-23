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
  | Div Arith Arith
  | Log Arith
  | Exp Arith

instance Show Arith where
  show (C e) = show e
  show (I i) = concat ["x", show i]
  show (Add a1 a2) = concat ["(", show a1, " + ", show a2, ")"]
  show (Sub a1 a2) = concat ["(", show a1, " - ", show a2, ")"]
  show (Mul a1 a2) = concat ["(", show a1, " * ", show a2, ")"]
  show (Div a1 a2) = concat ["(", show a1, " / ", show a2, ")"]
  show (Log a)     = concat ["log(", show a, ")"]
  show (Exp a)     = concat ["exp(", show a, ")"]

instance Language Arith where
  randomTerminal = do
    k  <- uniform 0 5
    ix <- uniform 0 12
    let ts = [C k, I ix]
    n <- uniform 0 (length ts - 1)

    return $ Program (ts !! n) 1

  randomFunction randArg = do
    let binaryOps = [Add, Sub, Mul, Div]
        unaryOps  = [Log, Exp]

    p <- uniform False True

    (code, codeSize) <-
      if p
        then do
          n <- uniform 0 (length binaryOps - 1)
          [arg1, arg2] <- forM [1..2] $ \_ -> randArg

          let code     = (binaryOps !! n) (programCode arg1) (programCode arg2)
              codeSize = programSize arg1 + programSize arg2 + 1

          return (code, codeSize)
        else do
          n   <- uniform 0 (length unaryOps - 1)
          arg <- randArg

          let code     = (unaryOps !! n) (programCode arg)
              codeSize = programSize arg + 1

          return (code, codeSize)

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
evalArith inputs (Div a1 a2) =
  let r2 = evalArith inputs a2
  in
    if r2 == 0
      then 0
      else evalArith inputs a1 / r2
evalArith inputs (Log a) =
  let r = evalArith inputs a
  in
    if r > 0
      then log r
      else 0
evalArith inputs (Exp a) = exp (evalArith inputs a)
