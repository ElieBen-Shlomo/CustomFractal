module Maths where

    data Complex = C Float Float deriving (Show,Eq)
    instance Num Complex where
        (C a b) * (C c d) = C (c*a - b*d) (b*c + a*d)
        (C a b) + (C c d) = C (a+c) (b+d)
    
    real:: Complex -> Float
    real (C a b) = a

    imag:: Complex -> Float
    imag (C a b) = b

    (<^>) :: Complex -> Int -> Complex
    z <^> n = integerExponentiation z n

    (</>) :: Complex -> Int -> Complex
    (C a b) </> r = C (a/(fromIntegral r)) (b/(fromIntegral r))

    sumC:: [Complex] -> Complex
    sumC list = C realSum imagSum where
        realSum = sum $ map real list
        imagSum = sum $ map imag list
    
    integerExponentiation :: Complex -> Int -> Complex
    integerExponentiation z n   
        | n <= 0 = (C 1 0)
        | otherwise = z * integerExponentiation z (n-1)

    factorial :: Int -> Int
    factorial 0 = 1
    factorial n = product [1..n]

    infiniteSum:: (Int -> Complex) -> Complex -- should be able to replace Complex with generic Numeric typeclass
    infiniteSum terms = sumC $ take numberOfTerms $ series where
        series = map terms [0,1..]
        numberOfTerms = 20  

    exponentiation :: Complex -> Complex
    exponentiation z = infiniteSum f where
        f n = (z <^> n) </> (factorial n)

    arctan:: Complex -> Complex
    arctan z = infiniteSum f where
        f n = ((z <^> (2*n+1)) * (C ((-1)^n) 0)) </> (2*n+1)

    