module Math.LinAlg
  ( project
  , lookAtLH
  , perspectiveLH
  , rotYawPitchRoll
  , translate
  ) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Types (width, height)
import Linear.V2
import Linear.V3
import Linear.V4 
import Linear.Metric (dot, normalize)
import Linear.Matrix ((!*!), M44)
import Linear.Epsilon (Epsilon)

translate :: Floating a => a -> a -> a -> M44 a
translate x y z
  = V4 (V4 1 0 0 0)
       (V4 0 1 0 0)
       (V4 0 0 1 0)
       (V4 x y z 1)

rotX :: Floating a => a -> M44 a
rotX angle
  = V4 (V4 1 0    0 0)
       (V4 0 c    s 0)
       (V4 0 (-s) c 0)
       (V4 0 0    0 1)
  where c = cos angle
        s = sin angle

rotY :: Floating a => a -> M44 a
rotY angle
  = V4 (V4 c 0 (-s) 0)
       (V4 0 1 0    0)
       (V4 s 0 c    0)
       (V4 0 0 0    1)
  where c = cos angle
        s = sin angle

rotZ :: Floating a => a -> M44 a
rotZ angle
  = V4 (V4 c    s 0 0)
       (V4 (-s) c 0 0)
       (V4 0    0 1 0)
       (V4 0    0 0 1)
  where c = cos angle
        s = sin angle

project :: Floating a => M44 a -> V3 a -> V2 a
project tran coord
  = V2 x y
  where x = vX * w + w / 2.0
        y = -vY * h + h / 2.0
        h = fromIntegral height
        w = fromIntegral width
        (V3 vX vY vZ) = transCoord tran coord

transCoord :: Floating a => M44 a -> V3 a -> V3 a 
transCoord (V4 (V4 x1 y1 z1 w1)
               (V4 x2 y2 z2 w2)
               (V4 x3 y3 z3 w3)
               (V4 x4 y4 z4 w4)) (V3 x y z) 
  = V3 (vX / vW) (vY / vW) (vZ / vW)
  where vX = (x * x1) + (y * x2) + (z * x3) + x4 
        vY = (x * y1) + (y * y2) + (z * y3) + y4 
        vZ = (x * z1) + (y * z2) + (z * z3) + z4 
        vW = (x * w1) + (y * w2) + (z * w3) + w4

-- | View matrix builder for row-major matrices
-- and left-handed coordinate-systems.
-- Based on Linear.Projection.lookAt
lookAtLH :: (Epsilon a, Floating a) => V3 a -> V3 a
                                            -> V3 a 
                                            -> M44 a
lookAtLH eye center up 
  = V4 (V4 (xa^._x) (ya^._x) (za^._x) 0)
       (V4 (xa^._y) (ya^._y) (za^._y) 0)
       (V4 (xa^._z) (ya^._z) (za^._z) 0)
       (V4 xd       yd       zd       1)
  where za = normalize $ center - eye
        xa = normalize $ cross up za
        ya = cross za xa
        xd = -dot xa eye
        yd = -dot ya eye
        zd = -dot za eye

perspectiveLH :: Floating a => a -> a -> a -> a -> M44 a
perspectiveLH fov aspect near far 
  = V4 (V4 x 0 0 0)
       (V4 0 y 0 0)
       (V4 0 0 z 1)
       (V4 0 0 w 0)
  where tanHalfFov = 1 / tan (fov / 2)
        x = tanHalfFov / aspect
        y = tanHalfFov
        z = -far / (near - far)
        w = (near * far) / (near - far)

rotYawPitchRoll :: Floating a => a -> a -> a -> M44 a
rotYawPitchRoll yaw pitch roll 
  = (rotZ roll) !*! (rotX pitch) !*! (rotY yaw)
