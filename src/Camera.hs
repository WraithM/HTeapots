-- Author: Matthew Wraith

module Camera where

import Vec
import VecGL

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- A Camera has a position, view, and up
data Camera = Camera
    { pos  :: !Vec3
    , view :: !Vec3
    , up   :: !Vec3
    } deriving (Show, Eq)

-- Setter for Camera
setCamera px py pz vx vy vz ux uy uz = Camera 
    (Vec3 px py pz)
    (Vec3 vx vy vz)
    (Vec3 ux uy uz)

-- Move forward by amount speed in the x-z plane
cameraMoveForward (Camera p v u) speed =
    let fwdv = normalizeV $ v -. p

        px = x p + x fwdv * speed
        pz = z p + z fwdv * speed

        vx = x v + x fwdv * speed
        vz = z v + z fwdv * speed
    in Camera (Vec3 px (y p) pz) (Vec3 vx (y v) vz) u

cameraRoll (Camera p v u) speed =
    let fwdv = normalizeV $ v -. p in
    Camera p v (rotateV u fwdv speed)

cameraPitch (Camera p v u) speed =
    let fwdv = v -. p
        left = u `cross` fwdv
    in Camera p (rotateV fwdv left speed +. p) (rotateV u left speed)

-- Rotate the camera in the x-z plane
cameraRotate (Camera p v u) speed =
    let fwdv = normalizeV $ v -. p

        vz = z p + sin speed * x fwdv + cos speed * z fwdv
        vx = x p + cos speed * x fwdv - sin speed * z fwdv
    in Camera p (Vec3 vx (y v) vz) u

-- Strafe in the x-z plane
cameraStrafe (Camera p v u) speed =
    let fwdv = normalizeV $ v -. p

        px = x p - z fwdv * speed
        pz = z p + x fwdv * speed

        vx = x v - z fwdv * speed
        vz = z v + x fwdv * speed
    in Camera (Vec3 px (y p) pz) (Vec3 vx (y v) vz) u

-- Simply go up in the y-direction
cameraUp (Camera p v u) speed =
    let py = y p + speed
        vy = y v + speed
    in Camera (Vec3 (x p) py (z p)) (Vec3 (x v) vy (z v)) u

cameraLookAt (Camera p v u) = 
    lookAt (toVertex3 p)
           (toVertex3 v)
           (toVector3 u)

-- Draw the camera with a crosshair
drawCamera cam = do
    c <- get cam
    cameraLookAt c
    drawCross c

-- Simply place a red dot at the view position
drawCross c = do
    lighting $= Disabled
    color $ Color3 (1.0 :: GLdouble) 0.0 0.0
    renderPrimitive Points $ do
        vertex $ toVertex3 (view c)
    lighting $= Enabled

-- Constants for motion
movespeed = 0.3
rollspeed = 0.1
pitchspeed = 0.1 
rotatespeed = 0.2

-- Keyboard commands for the camera
--  W and S move forward
--  D and A rotate
--  E and C move up and down vertically
--  X and Z roll the camera
--  Up and down arrow keys pitch the camera
--  Left and right strafe
keyboardCommands =
    [ ('w', \c -> cameraMoveForward c movespeed)
    , ('s', \c -> cameraMoveForward c (-movespeed))
    , ('d', \c -> cameraRotate c rotatespeed)
    , ('a', \c -> cameraRotate c (-rotatespeed))
    , ('e', \c -> cameraUp c movespeed)
    , ('c', \c -> cameraUp c (-movespeed))
    , ('x', \c -> cameraRoll c rollspeed)
    , ('z', \c -> cameraRoll c (-rollspeed))
    ]

specialKeyboardCommands =
    [ (KeyDown,  \c -> cameraPitch c pitchspeed)
    , (KeyUp,    \c -> cameraPitch c (-pitchspeed))
    , (KeyRight, \c -> cameraStrafe c movespeed)
    , (KeyLeft,  \c -> cameraStrafe c (-movespeed))
    ]

-- Execute the keyboard commands for the camera
keyboardCamera cam (Char k) Down = do
    c <- get cam
    case lookup k keyboardCommands of
        Just camf -> cam $=! camf c
        Nothing -> return ()

keyboardCamera cam (SpecialKey k) Down = do
    c <- get cam
    case lookup k specialKeyboardCommands of
        Just camf -> cam $=! camf c
        Nothing -> return ()

keyboardCamera _ _ _ = return ()
