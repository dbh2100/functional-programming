module Polynomials
( quadratic
, polynomial
) where

quadratic :: (Num a) => a -> a -> a -> (a -> a)
quadratic a b c = applyQuadratic
    where applyQuadratic x = a*x^2 + b*x + c

polynomial :: (Num a) => [a] -> a -> a
polynomial [] _ = 0
polynomial (n:ns) x = n * x^(length ns) + polynomial ns x