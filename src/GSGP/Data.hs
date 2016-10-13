module GSGP.Data (
  Shape
, Dataset
, fromList
, fromList2
, toList
, zipWith
, mapR
, shape
, count
, slice
, elementAt
, loadTxt
, saveTxt
) where

import Prelude hiding (zipWith)

import System.IO (hGetContents, withFile, IOMode (ReadMode))

import Data.Functor (Functor, fmap)
import Data.Foldable (Foldable, foldr)
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
        dsLines = fmap (\i -> intercalate " " . map show . foldr (:) [] . slice ds $ (0, w - 1, i, i)) [0..(h - 1)]
    in
      intercalate "\n" dsLines

instance Functor Dataset where
  fmap f ds = ds { dsContents = fmap f (dsContents ds) }

instance Foldable Dataset where
  foldr f b = foldr f b . dsContents


fromList :: [a] -> Shape -> Dataset a
fromList es shape = Dataset (V.fromList es) shape

fromList2 :: [[a]] -> Dataset a
fromList2 es =
  let h = length es
      w = length . head $ es
  in
    Dataset (V.fromList . concat $ es) (w, h)

toList :: Dataset a -> [a]
toList = V.toList . dsContents


zipWith :: (a -> b -> c) -> Dataset a -> Dataset b -> Dataset c
zipWith f dsA dsB = dsA { dsContents = V.zipWith f (dsContents dsA) (dsContents dsB) }


mapR :: (Dataset a -> b) -> Dataset a -> Dataset b
mapR fR ds =
  let (w, h) = dsShape ds
      contents = fmap fR . fmap (\i -> slice ds (0, w - 1, i, i)) $ [0..h - 1]
  in
    Dataset (V.fromList contents) (h, 1)


shape :: Dataset e -> Shape
shape = dsShape

count :: Dataset e -> Int
count ds =
  let (w, h) = dsShape ds
  in
    w * h


slice :: Dataset e -> View -> Dataset e
slice ds (vx, vx', vy, vy') =
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
  ls <- fmap (filter ((> 0) . length) . lines) (readFile fileName)
  let y = length ls
      x = length (words (head ls))
      es = map read . concatMap words $ ls

  return (Dataset (V.fromList es) (x, y))

saveTxt :: (Show e) => String -> Dataset e -> IO ()
saveTxt fileName ds = writeFile fileName (show ds)
