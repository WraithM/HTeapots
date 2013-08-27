-- Author: Matthew Wraith

module VecGL where

-- Here we simply convert to and from the Vec3 type that's defined in Vec
-- The type signatures tell the story

import Vec
import Graphics.Rendering.OpenGL

toVector3 :: Vec3 -> Vector3 GLdouble
toVector3 (Vec3 x y z) = Vector3 (realToFrac x) (realToFrac y) (realToFrac z)

toVertex3 :: Vec3 -> Vertex3 GLdouble
toVertex3 (Vec3 x y z) = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

toNormal3 :: Vec3 -> Normal3 GLdouble
toNormal3 (Vec3 x y z) = Normal3 (realToFrac x) (realToFrac y) (realToFrac z)

toVec3 :: Vertex3 GLdouble -> Vec3
toVec3 (Vertex3 x y z) = Vec3 (realToFrac x) (realToFrac y) (realToFrac z)

