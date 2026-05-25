churchNumeral :: (Integral a) => a -> ((b -> b) -> b -> b)
churchNumeral 0 f x = x
churchNumeral n f x = f (churchNumeral (n-1) f x)