import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

width = 500 :: GLfloat
height = 500 :: GLfloat

data Complex = C Float Float deriving (Show,Eq)
instance Num Complex where
    fromInteger n = C (fromIntegral n) 0.0
    (C x y) * (C z t) = C (z*x - y*t) (y*z + x*t)
    (C x y) + (C z t) = C (x+z) (y+t)
complex x y = C x y
re (C x y)  = x
im   (C x y)    = y
magnitude (C x y) = sqrt(x * x + y * y)

type CustomFunction = Complex -> Complex -> Complex

iteratedFunction :: Complex -> Complex -> CustomFunction -> Int -> Int
iteratedFunction z c f 0 = 0
iteratedFunction z c f n = if (magnitude z > 2 )
          then n
          else iteratedFunction (f z c) c f (n-1)

iterations x y = iteratedFunction 0 (complex real imag) f 64
                    where 
                        f z c = z*z+c
                        real = 2.0 * x / width
                        imag = 2.0 * y / height

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

main :: IO ()
main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Mandelbrot Set with Haskell and OpenGL"
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
                    drawColoredPoint (x,y,c) = do
                        color c
                        vertex $ Vertex3 x y 0