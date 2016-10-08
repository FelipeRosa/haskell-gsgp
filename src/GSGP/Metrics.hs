module GSGP.Metrics (
  mae
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

rmse :: (Floating e) => Dataset e -> Dataset e -> e
rmse xs ys =
  let sqrDiff = fmap (^2) . D.zipWith (-) xs $ ys
      n = D.count xs
  in
    sqrt (sum sqrDiff) / fromIntegral n
