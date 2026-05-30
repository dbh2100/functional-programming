module ComplexFunctions
( mandelbrotSet
) where

import Data.Complex
import Data.List

mandelbrotSet :: Complex Float -> [Complex Float]
mandelbrotSet c = iterate (\z -> z^2 + c) 0