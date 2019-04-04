module Complexnumber where

    data Complex = C Float Float deriving (Show,Eq)
    instance Num Complex where
        fromInteger n = C (fromIntegral n) 0.0
        (C x y) * (C z t) = C (z*x - y*t) (y*z + x*t)
        (C x y) + (C z t) = C (x+z) (y+t)
        
    (<^>) :: Complex -> Int -> Complex
    z <^> n = integerExponentiation z n

    integerExponentiation :: Complex -> Int -> Complex
    integerExponentiation z n 
        | n <= 0 = 1
        | otherwise = z * integerExponentiation z (n-1)