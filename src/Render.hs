{-# LANGUAGE RecordWildCards #-}

module Render 
  ( clear
  , clearWithCol
  , render
  , transform
  ) where

import Control.Monad (when, forM_)
import Control.Monad.Reader (ask, asks, liftIO)
import Data.IORef (readIORef, writeIORef)
import Data.Types hiding (scrnW, scrnH)
import qualified Data.Vector.Storable as VS (fromList, unsafeWith)
import qualified Data.Vector.Storable.Mutable as VM
  ( unsafeToForeignPtr0
  , write
  , unsafeWrite
  , set
  , unsafeCast
  )
import Foreign (copyArray)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Linear.V2
import Linear.V3
import Linear.Matrix ((!*!), M44)
import Math.LinAlg
import Render.Camera
import Render.Engine
import Render.Mesh
import Data.Word
import Data.Bits (shiftL, (.|.))
import Data.List (foldl')

clear :: Engine ()
clear
  = asks bBuff >>= liftIO . flip VM.set 0

fromRGBA :: RGBA -> Word32
fromRGBA (r, g, b, a)
  = foldl' (\a -> (a `shiftL` 8 .|.)) 0 (map fromIntegral [r, g, b, a])

-- | Clear the screen with a specific color
clearWithCol :: RGBA -> Engine ()
clearWithCol colour
  = asks bBuff >>= liftIO . flip VM.set (fromRGBA colour) . VM.unsafeCast

putPixel :: RGBA -> Coordinate -> Engine ()
putPixel colour (x, y) = do
  buf <- asks bBuff
  liftIO $ VM.unsafeWrite (VM.unsafeCast buf) index (fromRGBA colour)
  where index = (x + y * width)

drawPoint :: V2 Int -> Engine ()
drawPoint (V2 x y)
  = when (x >= 0 && y >= 0 && x < width && y < height) $
      putPixel (255,0,255,255) (x, y)

-- | Bresenham’s line algorithm
drawLine :: V2 Int -> V2 Int -> Engine ()
drawLine (V2 x0 y0) (V2 x1 y1) = do
  let dx   = abs (x1 - x0)
      dy   = abs (y1 - y0)
      sx   = if x0 < x1 then 1 else -1
      sy   = if y0 < y1 then 1 else -1
      err = dx - dy

  go (x0, y0) (x1, y1) dx dy sx sy err

  where 
    go (x0, y0) (x1, y1) dx dy sx sy err
      = when ((x0 /= x1) || (y0 /= y1)) $ do
          drawPoint (V2 x0 y0)

          let (x0', err')  = if e2 > -dy 
                               then (x0 + sx, err - dy) 
                               else (x0, err)
              (y0', err'') = if e2 < dx 
                               then (y0 + sy, err' + dx) 
                               else (y0, err')
              
          go (x0', y0') (x1, y1) dx dy sx sy err''
      where e2 = 2 * err

transform :: Mesh -> Engine ()
transform Mesh {..} = liftIO $ do 
  (V3 rx ry rz) <- readIORef rotation

  writeIORef rotation $ V3 (rx + 0.01) (ry + 0.01) rz

renderMesh :: Mesh -> Engine ()
renderMesh Mesh {..} = do
  EngineState {..} <- ask
  Camera {..}      <- liftIO $ readIORef camera
  vs               <- liftIO $ readIORef vertices
  fs               <- liftIO $ readIORef faces
  (V3 rx ry rz)    <- liftIO $ readIORef rotation
  (V3 px py pz)    <- liftIO $ readIORef meshPos
  cPos             <- liftIO $ readIORef camPos
  cT               <- liftIO $ readIORef target

  let viewM         = lookAtLH cPos cT (V3 0 1 0)
      projM         = perspectiveLH 0.78 (w / h) 0.01 1.0
      worldM        = (rotYawPitchRoll ry rx rz) 
                    !*! (translate px py pz)
      transM        = worldM !*! viewM !*! projM
  
  mapM_ (drawPoint . fmap floor . project transM) vs
  mapM_ (\ (V3 a b c) -> do
             let vA = vs !! a
                 vB = vs !! b
                 vC = vs !! c 
                 pA = floor <$> project transM vA
                 pB = floor <$> project transM vB
                 pC = floor <$> project transM vC
             drawLine pA pB
             drawLine pB pC
             drawLine pC pA
        ) 
        fs

  where (w, h) = (fromIntegral width, fromIntegral height)  

render :: Engine ()
render = do
  EngineState {..} <- ask
  ms               <- liftIO $ readIORef meshes

  mapM_ renderMesh ms
