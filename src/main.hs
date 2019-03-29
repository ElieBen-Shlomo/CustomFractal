import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Window
import Data.IORef

screenWidth = 700
screenHeight = 700
widthDensity = 700 :: GLfloat
heightDensity = 700 :: GLfloat 

data Complex = C Float Float deriving (Show,Eq)
instance Num Complex where
    fromInteger n = C (fromIntegral n) 0.0
    (C x y) * (C z t) = C (z*x - y*t) (y*z + x*t)
    (C x y) + (C z t) = C (x+z) (y+t)
-- complex x y = C x y
-- re (C x y)  = x
-- im   (C x y)    = y
magnitude (C x y) = sqrt(x * x + y * y)

type CustomFunction = Complex -> Complex -> Complex
functionIterates :: CustomFunction -> Complex -> [Complex]
functionIterates f c = iterate (\z -> f z c) c

type TruthCondition = Complex -> Bool
firstIndexOfElementSatisfyingCondition :: [Complex] -> TruthCondition -> Int -> Int
firstIndexOfElementSatisfyingCondition list condition n = snd $ head $ filter (\(z, n) -> condition z) enumeratedList where
    enumeratedList = take n $ zip list [1,2..]

numberOfIterations :: CustomFunction -> Complex -> TruthCondition -> Int -> Int
numberOfIterations f z condition maxIterations = firstIndexOfElementSatisfyingCondition (functionIterates f z) condition maxIterations


-- iteratedFunction :: Complex -> Complex -> CustomFunction -> Int -> Int
-- iteratedFunction z c f n = if (magnitude z > 2 )
--           then n
--           else iteratedFunction (f z c) c f (n+1)

-- iterations x y n = numberOfIterations f (C real imag) n
--                     where 
--                         f z c = z*z+c
--                         real = 2.0 * x / widthDensity
--                         imag = 2.0 * y / heightDensity

-- factor = 0.5
-- shiftX = -0.5
-- shiftY = -0.5
-- colouredPoints :: [(GLfloat,GLfloat,Color3 GLfloat)]
-- colouredPoints = [ (shiftX +factor*x/widthDensity, shiftY + factor*y/heightDensity, intToColour $ iterations x y)|
--                     x <- [-widthDensity..widthDensity],
--                     y <- [-heightDensity..heightDensity]]
--                     where intToColour x =
--                             let
--                                 colour :: GLfloat -> GLfloat    
--                                 colour x =  x 
--                             in
--                                 if x >= 50 then Color3 (colour 0) (colour 0) (colour 0) else Color3 (colour 100) (colour 50) (colour 200)
--                                 --Color3 (colour x) (colour 20) (colour 50)

condition :: TruthCondition                
condition = (\z -> (magnitude z) > 2)  
f z c =z*z + c
main :: IO ()
main = print(numberOfIterations f (C (-1.5) 0.001) condition 100)

-- main = do
--   (progname,_) <- getArgsAndInitialize
--   initialDisplayMode $= [DoubleBuffered]
--   createWindow "Haskelbrot"
--   windowSize $= Size screenWidth screenHeight
--   displayCallback $= display
--   mainLoop where
--     display = do
--         clear [ColorBuffer] 
--         loadIdentity 
--         preservingMatrix drawMandelbrot
--         swapBuffers where
--             drawMandelbrot =
--                 renderPrimitive Points $ do
--                     mapM_ drawColoredPoint colouredPoints
--                 where
--                     drawColoredPoint (x,y,c) = do
--                         color c 
--                         vertex $ Vertex3 x y 0

