module HaskellWorks.Playground.Linear where

import Numeric.LinearAlgebra
import Prelude               hiding ((<>))

variance :: Matrix Double -> Matrix Double
variance m = x <> ((m - asRow rdMean) ** 2)
  where (rdMean, _) = meanCov m
        r = rows m
        x = konst (1 / fromIntegral r) (1, r)

errors :: Matrix Double -> Matrix Double
errors m = (m - asRow rdMean)
  where (rdMean, _) = meanCov m
        r           = rows m

correlation :: Matrix Double -> Matrix Double
correlation m = build (c, c) mkCorrelation
  where stddev  = flatten (sqrt (variance m)) :: Vector Double
        (_, c)  = size m
        err     = errors m
        mkCorrelation :: Double -> Double -> Double
        mkCorrelation di dj = sumElements (x * y) / fromIntegral r / (stddev ! i) / (stddev ! j)
          where i = round di :: Int
                j = round dj :: Int
                (r, _) = size x
                x = err ¿ [i]
                y = err ¿ [j]
