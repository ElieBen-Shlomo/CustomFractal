import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

width = 320 :: GLfloat
height = 320 :: GLfloat

allPoints :: [(GLfloat,GLfloat,Color3 GLfloat)]
allPoints = [ (x/width,y/height, if mandel x y == 0 then Color3 0 0 0  else Color3 100 0 100 )|
                  x <- [-width..width],
                  y <- [-height..height]]

colorFromValue n =
    let
        t :: Int -> GLfloat
        t i = 0.5 + 0.5*cos( fromIntegral i / 10 )
    in
        Color3 (t n) (t (n+5)) (t (n+10))

data Complex = C Float Float deriving (Show,Eq)
instance Num Complex where
    fromInteger n = C (fromIntegral n) 0.0
    (C x y) * (C z t) = C (z*x - y*t) (y*z + x*t)
    (C x y) + (C z t) = C (x+z) (y+t)
    abs (C x y)     = C (sqrt (x*x + y*y)) 0.0
    signum (C x y)  = C (signum x) (0.0)

complex :: Float -> Float -> Complex
complex x y = C x y

real :: Complex -> Float
real (C x y)    = x

im :: Complex -> Float
im   (C x y)    = y

magnitude :: Complex -> Float
magnitude z = real $ abs z

f :: Complex -> Complex -> Int -> Int
f c z 0 = 0
f c z n = if (magnitude z > 2 )
          then n
          else f c ((z*z)+c) (n-1)

mandel x y =
    let r = 2.0 * x / width
        i = 2.0 * y / height
    in
        f (complex r i) 0 64

display = do
    clear [ColorBuffer] -- make the window black
    loadIdentity -- reset any transformation
    preservingMatrix drawMandelbrot
    swapBuffers -- refresh screen

drawMandelbrot =
    -- We will print Points (not triangles for example)
    renderPrimitive Points $ do
        mapM_ drawColoredPoint allPoints
    where
        drawColoredPoint (x,y,c) = do
            color c -- set the current color to c
            -- then draw the point at position (x,y,0)
            -- remember we're in 3D
            vertex $ Vertex3 x y 0

main :: IO ()
main = do
  -- GLUT need to be initialized
  (progname,_) <- getArgsAndInitialize
  -- We will use the double buffered mode (GL constraint)
  initialDisplayMode $= [DoubleBuffered]
  -- We create a window with some title
  createWindow "Mandelbrot Set with Haskell and OpenGL"
  -- Each time we will need to update the display
  -- we will call the function 'display'
  displayCallback $= display
  -- We enter the main loop
  mainLoop