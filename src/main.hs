import Fractal

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Window
import Data.IORef

drawFractal :: Int ->  IO ()
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
                    mapM_ drawColoredPoint $ colouredPoints f (0.7, (-0.3), (-0.5), (300::GLfloat), (300::GLfloat))
                where
                    f  = (\z c -> z * z + c) :: CustomFunction
                    drawColoredPoint (x,y,c) = do
                        color c 
                        vertex $ Vertex3 x y 0


main:: IO()
main = do
    putStrLn "Enter an integer exponent n for the formula z -> z^n + c"
    exponent <- getLine
    drawFractal (read exponent :: Int)