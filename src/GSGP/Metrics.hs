module GSGP.Metrics (
  mae
, mse
, rmse
) where

import GSGP.Data (Dataset)
import qualified GSGP.Data as D


mae :: (Fractional e) => Dataset e -> Dataset e -> e
mae xs ys =
  let diff = fmap abs . D.zipWith (-) xs $ ys
      n = D.count xs
  in
    sum diff / fromIntegral n

mse :: (Fractional e) => Dataset e -> Dataset e -> e
mse xs ys =
  let sqrDiff = fmap (^2) . D.zipWith (-) xs $ ys
      n = D.count xs
  in
    sum sqrDiff / fromIntegral n

rmse :: (Floating e) => Dataset e -> Dataset e -> e
rmse xs ys = sqrt (mse xs ys)
