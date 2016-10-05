module Main where

import GSGP.Data

main :: IO ()
main = do
  ds <- loadTxt "/Users/felipe/Desktop/housing.data.txt" :: IO (Dataset Double)

  let trainingInput  = reshape ds (0, 13, 0, 300)
      trainingOutput = reshape ds (13, 14, 0, 300)

  print $ shape trainingInput
  print $ shape trainingOutput

  print $ elementAt trainingInput (0, 0)
  print $ elementAt trainingOutput (0, 0)
