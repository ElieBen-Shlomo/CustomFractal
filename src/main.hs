import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Window
import Data.IORef

widthDensity = 300:: GLfloat
heightDensity = 300 :: GLfloat 

data Complex = C Float Float deriving (Show,Eq)
instance Num Complex where
    fromInteger n = C (fromIntegral n) 0.0
    (C x y) * (C z t) = C (z*x - y*t) (y*z + x*t)
    (C x y) + (C z t) = C (x+z) (y+t)

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

iterations ::  Float -> Float -> Int -> Int          
iterations x y n = numberOfIterations f (C real imag) condition n
                    where 
                        f z c = z * z + c
                        condition z = (magnitude z) > 2
                        real = 2.0 * x / widthDensity
                        imag = 2.0 * y / heightDensity

factor = 0.7
shiftX = -0.3
shiftY = -0.5
colouredPoints :: [(GLfloat,GLfloat,Color3 GLfloat)]
colouredPoints = [ (shiftX +factor*x/widthDensity, shiftY + factor*y/heightDensity, intToColour $ iterations x y 100)|
                    x <- [-widthDensity..widthDensity],
                    y <- [-heightDensity..heightDensity]]
                        where 
                        intToColour n = Color3 (colour n) (colour 1) (colour 1)
                            where 
                                colour :: Int -> GLfloat    
                                colour n = fromIntegral n/50

drawFractal :: Float ->  IO ()
drawFractal exponent = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Haskelbrot"
  windowSize $= Size 600 600
  displayCallback $= display
  mainLoop where
    display = do
        clear [ColorBuffer] 
        loadIdentity 
        preservingMatrix drawMandelbrot
        swapBuffers where
            drawMandelbrot =
                renderPrimitive Points $ do
                    mapM_ drawColoredPoint colouredPoints
                where
                    f z c = z * z + c
                    drawColoredPoint (x,y,c) = do
                        color c 
                        vertex $ Vertex3 x y 0


main:: IO()
main = do
    putStrLn "Enter an integer exponent n for the formula z -> z^n + c"
    exponent <- getLine
    drawFractal (read exponent :: Int)