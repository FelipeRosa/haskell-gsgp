module GSGP.Data (
  Shape
, Dataset
, shape
, count
, reshape
, elementAt
, loadTxt
) where

import System.IO (hGetContents, withFile, IOMode (ReadMode))

import Data.Functor
import Data.Foldable
import Data.String (lines, words)
import Data.List (intercalate)
import Data.Vector (Vector)
import qualified Data.Vector as V


type View  = (Int, Int, Int, Int)
type Shape = (Int, Int)

data Dataset e =
  Dataset {
    dsContents :: Vector e
  , dsShape    :: Shape
  }

instance (Show e) => Show (Dataset e) where
  show ds =
    let (w, h)  = dsShape ds
        dsLines = fmap (\i -> intercalate " " . map show . foldr (:) [] . reshape ds $ (0, w - 1, i, i)) [0..(h - 1)]
    in
      intercalate "\n" dsLines

instance Functor Dataset where
  fmap f ds = ds { dsContents = fmap f (dsContents ds) }

instance Foldable Dataset where
  foldr f b = foldr f b . dsContents


shape :: Dataset e -> Shape
shape = dsShape

count :: Dataset e -> Int
count ds =
  let (w, h) = dsShape ds
  in
    w * h


reshape :: Dataset e -> View -> Dataset e
reshape ds (vx, vx', vy, vy') =
  let newShape = (vx' - vx + 1, vy' - vy + 1)
      newContents = V.fromList . fmap (elementAt ds) . fmap (\[a, b] -> (b, a)) . sequence $ [[vy..vy'], [vx..vx']]
  in
    Dataset newContents newShape


elementAt :: Dataset e -> (Int, Int) -> e
elementAt ds (x, y) =
  let (vw, _) = dsShape ds
      v = dsContents ds
  in
    v V.! (x + y * vw)


loadTxt :: (Read e) => String -> IO (Dataset e)
loadTxt fileName = do
  ls <- fmap lines (readFile fileName)
  let y = length ls
      x = length (words (head ls))
      es = map read . concatMap words $ ls

  return (Dataset (V.fromList es) (x, y))
