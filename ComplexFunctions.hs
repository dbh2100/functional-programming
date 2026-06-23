-- This module defines functions related to complex numbers.

module ComplexFunctions
( mandelbrotSet
) where

import Data.Complex ( Complex )
import Data.List()

-- This function generates the Mandelbrot set for a given complex number c.
-- It returns an infinite list of complex numbers representing the iterations
-- of the Mandelbrot function starting from 0. Each element in the list is computed by squaring the previous element and adding c.
mandelbrotSet :: Complex Float -> [Complex Float]
mandelbrotSet c = iterate (\z -> z^2 + c) 0