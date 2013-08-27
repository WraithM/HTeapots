{- Author: Matthew Wraith - 4/9/13
 - NetId: wraith1
 - CS 418 - Prof. John C. Hart
 - Credits: Three (3) Credits
 -
 - Compilation instructions: I've included a Makefile. Simply, make && ./Main 
 - to run this. If for some reason the Makefile does not work, this should:
 -
 - ghc --make -O2 Main.hs
 -
 - GHC will take care of the rest.
 -
 - Teapot Contest - This program makes a teapot with a sphere environment mapping
 -}

module Main where

import Vec
import Camera
import ObjFile
import Teapot

import TGA

import System.Exit (exitSuccess)
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Applicative
import qualified Data.Vector as V

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- Draw a green grid
drawGrid = do
    lighting $= Disabled
    color $ (Color4 (0.0 :: GLdouble) 0.7 0.2 1.0)
    renderPrimitive Lines $ do
        forM_ [-500,-495..500] $ \i -> do
            vertex $ Vertex3 (-500 :: GLdouble) 0 i
            vertex $ Vertex3 (500  :: GLdouble) 0 i
            vertex $ Vertex3 (i    :: GLdouble) 0 (-500)
            vertex $ Vertex3 (i    :: GLdouble) 0 500
    lighting $= Enabled

-- Main display function
display cam angle pos teapot texs = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    drawCamera cam
    drawGrid
    preservingMatrix $ do
        a <- get angle
        (x,y,z) <- get pos
        let [kitchen, campus] = texs

        position light0 $= lightPosition
        drawModel teapot

        texture Texture2D $= Enabled
        translate $ Vector3 (0.0 :: GLdouble) 5.0 0.0
        textureBinding Texture2D $= Just kitchen
        drawModel teapot
        translate $ Vector3 (10.0 :: GLdouble) 0.0 0.0
        translate $ Vector3 x y z
        rotate a $ Vector3 0.0 1.0 0.0
        textureBinding Texture2D $= Just campus 
        drawModel teapot
        texture Texture2D $= Disabled
    swapBuffers
    flush

-- Q to quit,
-- R to rotate the teapot
keyboard _ (Char 'q') Down = exitSuccess
keyboard angle (Char 'r') Down = do
    a <- get angle
    angle $=! (a + 1.0)
keyboard _ _ _ = return ()

-- This handles the input for the camera, teapot, and misc.
keyboardMouse cam angle tpos key state mod pos = do
    keyboard angle key state
    keyboardTeapot tpos key state
    keyboardCamera cam key state

-- Window reshape
reshape s@(Size w h) = do
    viewport $= (Position 0 0, s)
    postRedisplay Nothing

-- Animation function
idle t = do 
    threadDelay 10000
    milliseconds <- fromIntegral <$> get elapsedTime
    t $=! milliseconds
    postRedisplay Nothing

-- Light properties
light0        = Light 0 

lightPosition = Vertex4 0.0 5.0 3.0 1.0

lightWhite    = Color4 1.0 1.0 1.0 1.0
lightBlue     = Color4 0.0 0.0 1.0 1.0
lightSpecular = Color4 0.8 0.8 0.8 1.0
lightAmbient  = Color4 0.2 0.2 0.2 1.0

shininess     = 60.0

initFunc = do
    -- Enable light
    lighting $= Enabled
    light light0 $= Enabled

    -- Black background and depth
    clearColor $= Color4 0.0 0.0 0.0 0.0
    clearDepth $= 1
    shadeModel $= Smooth

    -- Material properties
    materialSpecular  Front $= lightSpecular
    materialShininess Front $= shininess

    -- Light
    position light0 $= lightPosition
    diffuse  light0 $= lightWhite
    specular light0 $= lightAmbient
    ambient  light0 $= lightAmbient

    lightModelAmbient $= lightAmbient

    
    hint PerspectiveCorrection $= Nicest
    depthFunc $= Just Less
    pointSize $= 3.0

    -- Set to full screen, disable cursor
    fullScreenToggle
    --cursor $= None

    -- Set perspective and load modelview
    matrixMode $= Projection
    loadIdentity

    (Size w h) <- get windowSize
    perspective 80.0 (realToFrac w / realToFrac h) 0.1 300.0

    matrixMode $= Modelview 0
    loadIdentity

applyTexture (Just (Size w h, pix)) tex = do
    textureBinding  Texture2D $= Just tex
    texImage2D
        Nothing
        NoProxy
        0
        RGBA'
        (TextureSize2D w h)
        0
        pix
    textureFilter   Texture2D $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Repeated, Clamp)
    textureWrapMode Texture2D T $= (Repeated, Clamp)
    textureGenMode  S $= Just SphereMap
    textureGenMode  T $= Just SphereMap
    textureFunction $= Modulate

main :: IO ()
main = do
    -- Read teapot from file
    x <- readObjFile "teapot_0.obj"

    -- Get the vertecies, faces, and vertex normals
    let (vs', fs') = commandsToVsFs x
    let ns'  = calcVertexNormals vs' fs'

    -- Convert everything to vectors
    -- Calculate texture coordinates
    let fs = V.fromList $ map faceIndecies fs'

    let vs = V.fromList vs'
    let ns = V.fromList ns'
    let ts = V.map teapotTexCoord vs

    let teapot = Model vs ns ts

    -- Load the textures
    let texFiles = [ "kitchen_probe.tga", "campus_probe.tga" ]
    pixs <- mapM readTga texFiles
    texs <- genObjectNames 2

    -- Initialize globals
    -- The Camera, the angle and position of the teapot, time
    camera <- newIORef $ setCamera 0.0 2.5 5.0 0.0 2.5 0.0 0.0 1.0 0.0
    angle  <- newIORef (0.0 :: GLdouble)
    pos    <- newIORef ((0.0, 0.0, 0.0) :: (GLdouble, GLdouble, GLdouble))
    time   <- newIORef (0.0 :: Double)

    -- Start OpenGL/GLUT
    (progname, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
    createWindow "OpenGL"

    -- Declare callback functions
    displayCallback $= (display camera angle pos (teapot, fs) texs)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse camera angle pos)
    idleCallback $= Just (idle time)

    -- Run
    initFunc
    zipWithM applyTexture pixs texs

    -- Main loop
    mainLoop

