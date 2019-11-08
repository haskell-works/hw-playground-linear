module HaskellWorks.Playground.Linear where

import Numeric.LinearAlgebra
import Prelude               hiding ((<>))

variance :: Matrix Double -> Matrix Double
variance m = x <> ((m - asRow rdMean) ** 2)
  where (rdMean, _) = meanCov m
        r = rows m
        x = konst (1 / fromIntegral r) (1, r)
