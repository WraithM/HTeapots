-- Author: Matthew Wraith
-- NetId: wraith1

module Teapot where

import qualified Data.Vector as V
import Control.Monad

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- Data structure for holding the model
-- We have vertex, normal, and texture data
data Model = Model
    { vertices :: !(V.Vector (Vertex3   GLdouble))
    , normals  :: !(V.Vector (Normal3   GLdouble))
    , texcoord :: !(V.Vector (TexCoord2 GLdouble))
    } deriving (Show)

-- Calculate the texture coordinates for each vertex
teapotTexCoord (Vertex3 x y z) =
    let theta = atan2 z x
        s = (theta + pi) / (2 * pi)
        t = y
    in TexCoord2 s t

-- Render the vertices, normals, and textures
renderVNT vs ns ts = do
    let vnts = zip3 vs ns ts
    forM_ vnts $ \(v,n,t) -> do
        normal n
        texCoord t
        vertex v

-- Draw the teapot model
drawModel (Model vs ns ts,fs) = do
    V.forM_ fs $ \is -> do
        let vs' = fromIndecies vs is
        let ns' = fromIndecies ns is
        let ts' = fromIndecies ts is
        renderPrimitive Triangles $ do
            renderVNT vs' ns' ts'
    where fromIndecies xs = map (\i -> xs V.! (i - 1))

-- Speed of the teapot
tpspeed = 1.0

-- A dictionary of keys and functions to change the position of the teapot
--   K, I, J, L make the teapot move in the x-z plane
--   U and N make the teapot move vertically
teapotCommands =
    [ ('k', \(x,y,z) -> (x,y,z + tpspeed))
    , ('i', \(x,y,z) -> (x,y,z - tpspeed))
    , ('l', \(x,y,z) -> (x + tpspeed,y,z))
    , ('j', \(x,y,z) -> (x - tpspeed,y,z))
    , ('u', \(x,y,z) -> (x,y + tpspeed,z))
    , ('n', \(x,y,z) -> (x,y - tpspeed,z))
    ]

-- Acquire the teapot position functions from teapotCommands
keyboardTeapot :: (HasSetter s, HasGetter s) =>
    s (GLdouble, GLdouble, GLdouble) -> Key -> KeyState -> IO ()
keyboardTeapot pos (Char k) Down = do
    p <- get pos
    case lookup k teapotCommands of
        Just posf -> pos $=! posf p
        Nothing -> return ()

keyboardTeapot _ _ _ = return ()
