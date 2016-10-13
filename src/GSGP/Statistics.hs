module GSGP.Statistics (
  mean
, covariance
, variance
) where

import GSGP.Data (Dataset)
import qualified GSGP.Data as D


mean :: (Fractional e) => Dataset e -> e
mean xs = sum xs / fromIntegral (D.count xs)

covariance :: (Fractional e) => Dataset e -> Dataset e -> e
covariance xs ys =
  let meanX = mean xs
      meanY = mean ys
  in
    sum (D.zipWith (\x y -> (x - meanX) * (y - meanY)) xs ys) / fromIntegral (D.count xs)

variance :: (Fractional e) => Dataset e -> e
variance xs = covariance xs xs
