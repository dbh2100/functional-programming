churchNumeral :: (Integral a) => a -> ((b -> b) -> b -> b)
churchNumeral 0 = indentity
    where indentity f x = x
churchNumeral n = applyFunction
    where applyFunction f x = f ((churchNumeral (n-1)) f x)