import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

width = 500 :: GLfloat
height = 500 :: GLfloat

colouredPoints :: [(GLfloat,GLfloat,Color3 GLfloat)]
colouredPoints = [ (x/width,y/height, intToColour $ iterations x y)|
                  x <- [-width..width],
                  y <- [-height..height]]
                  where intToColour x =
                            let
                                colour :: Int -> GLfloat    
                                colour x = 0.5 + 0.5*cos( fromIntegral x / 10 )
                            in
                                Color3 (colour x) (colour 20) (colour 50)

data Complex = C Float Float deriving (Show,Eq)
instance Num Complex where
    fromInteger n = C (fromIntegral n) 0.0
    (C x y) * (C z t) = C (z*x - y*t) (y*z + x*t)
    (C x y) + (C z t) = C (x+z) (y+t)

complex x y = C x y
re (C x y)    = x
im   (C x y)    = y
magnitude z = sqrt(x * x + y * y) where
            x = re z
            y = im z

customFunction :: Complex -> Complex -> Complex
customFunction z c = (z * z) + c

iteratedFunction :: Complex -> Complex -> Int -> Int -- we can actually pass the custom function as a parameter here. But we need to figure out how to create custom types or else the signature will be too long
iteratedFunction z c 0 = 0
iteratedFunction z c n = if (magnitude z > 2 )
          then n
          else iteratedFunction (customFunction z c) c (n-1)

iterations x y =
    let r = 2.0 * x / width
        i = 2.0 * y / height
    in
        iteratedFunction 0 (complex r i) 64

display = do
    clear [ColorBuffer] -- make the window black
    loadIdentity -- reset any transformation
    preservingMatrix drawMandelbrot
    swapBuffers -- refresh screen

drawMandelbrot =
    -- We will print Points (not triangles for example)
    renderPrimitive Points $ do
        mapM_ drawColoredPoint colouredPoints
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