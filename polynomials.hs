quadratic :: (Num a) => a -> a -> a -> (a -> a)
quadratic a b c = applyQuadratic
    where applyQuadratic x = a*x^2 + b*x + c