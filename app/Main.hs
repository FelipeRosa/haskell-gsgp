module Main where

import GSGP.Data

main :: IO ()
main = do
  ds <- loadTxt "/Users/felipe/Desktop/housing.data.txt" :: IO (Dataset Double)

  let firstLine = reshape ds (0, 13, 0, 0)

  putStrLn $ show (reshape ds (0, 13, 0, 1))
  putStrLn ""
  putStrLn $ show firstLine
  putStrLn $ concat ["Count: ", show (count firstLine)]
  putStrLn $ concat ["Average: ", show (sum firstLine / (fromIntegral $ count firstLine))]

  return ()
