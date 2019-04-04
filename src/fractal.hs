module Fractal where
    import Complexnumber
    import Graphics.Rendering.OpenGL
    import Graphics.UI.GLUT
    import Graphics.UI.GLUT.Window
    
    magnitude :: Complex -> Float
    magnitude (C x y) = sqrt(x * x + y * y)
    
    type CustomFunction = Complex -> Complex -> Complex
    functionIterates :: CustomFunction -> Complex -> [Complex]
    functionIterates f c = iterate (\z -> f z c) c
    
    type TruthCondition = Complex -> Bool 
    firstIndexOfElementSatisfyingCondition :: [Complex] -> TruthCondition -> Int
    firstIndexOfElementSatisfyingCondition [] _ = 0
    firstIndexOfElementSatisfyingCondition (x:xs) condition = if condition x then 1 else 1 + firstIndexOfElementSatisfyingCondition xs condition
    
    numberOfIterations :: CustomFunction -> Complex -> TruthCondition -> Int -> Int
    numberOfIterations f z condition maxIterations = firstIndexOfElementSatisfyingCondition (take maxIterations $ functionIterates f z) condition
    
    iterations :: CustomFunction -> Float -> Float -> Int -> (GLfloat, GLfloat) -> Int          
    iterations f x y n (widthDensity, heightDensity) = numberOfIterations f (C real imag) condition n
                        where 
                            condition z = (magnitude z) > 2
                            real = 2.0 * x / widthDensity
                            imag = 2.0 * y / heightDensity
                            
    colouredPoints :: CustomFunction -> (Float, Float, Float, GLfloat, GLfloat) -> [(GLfloat,GLfloat,Color3 GLfloat)]
    colouredPoints f (scaleFactor, shiftX, shiftY, widthDensity, heightDensity) = 
        [(shiftX +scaleFactor*x/widthDensity, shiftY + scaleFactor*y/heightDensity, intToColour $ iterations f x y 100 (widthDensity, heightDensity))|
                        x <- [-widthDensity..widthDensity],
                        y <- [-heightDensity..heightDensity]]
                            where 
                            intToColour n = Color3 (colour n) (colour 1) (colour 1)
                                where 
                                    colour :: Int -> GLfloat    
                                    colour n = fromIntegral n/50    