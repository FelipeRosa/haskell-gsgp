{-# LANGUAGE OverloadedLists #-}

module GSGP.Data (
  Shape
, Dataset
, shape
, view
, reshape
, elementAt
, loadTxt
) where

import System.IO (hGetContents, withFile, IOMode (ReadMode))

import Data.String (lines, words)
import Data.Vector (Vector)
import qualified Data.Vector as V


type View  = (Int, Int, Int, Int)
type Shape = (Int, Int)

data Dataset e =
  Dataset {
    dsContents :: Vector e
  , dsShape    :: Shape
  , dsView     :: View
  } deriving (Eq, Show)


shape :: Dataset e -> Shape
shape ds =
  let
    (ox, ow, oy, oh) = dsView ds
  in
    (ow - ox, oh - oy)

view :: Dataset e -> View
view = dsView


reshape :: Dataset e -> View -> Dataset e
reshape ds newView = ds { dsView = newView }

elementAt :: Dataset e -> (Int, Int) -> e
elementAt ds (x, y) =
  let
    (vx, vw, vy, _) = dsView ds
    v = dsContents ds
  in
    v V.! ((vx + x) + (vy + y) * vw)


loadTxt :: (Read e) => String -> IO (Dataset e)
loadTxt fileName = do
  ls <- fmap lines (readFile fileName)
  let y = length ls
      x = length (words (head ls))
      es = map read . concatMap words $ ls

  return (Dataset (V.fromList es) (x, y) (0, x, 0, y))
