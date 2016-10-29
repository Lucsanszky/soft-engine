{-# LANGUAGE RecordWildCards #-}

module Render 
  ( clear
  , clearWithCol
  , render
  ) where

import Control.Monad.Reader (ask, liftIO)
import Data.IORef (readIORef)
import Data.Types hiding (scrnW, scrnH)
import qualified Data.Vector.Storable as VS (fromList, unsafeWith)
import qualified Data.Vector.Storable.Mutable as VM 
  ( unsafeToForeignPtr0
  , write
  , set
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

clear :: Engine ()
clear = do
  EngineState {..} <- ask 

  liftIO $ VM.set bBuff 0

clearWithCol :: RGBA -> Engine ()
clearWithCol (r,g,b,a) = do
  EngineState {..} <- ask 
  (dest, _)        <- return $ VM.unsafeToForeignPtr0 bBuff

  liftIO $ VS.unsafeWith (VS.fromList lRGBA)
         (\ src -> copyArray 
                   (unsafeForeignPtrToPtr dest)
                   src bufLen
         )
         where lRGBA = take bufLen $ cycle [r,g,b,a]

putPixel :: RGBA -> Coordinate -> Engine ()
putPixel (r,g,b,a) (x, y) = do
  EngineState {..} <- ask

  liftIO $ do 
    VM.write bBuff index r
    VM.write bBuff (index + 1) g
    VM.write bBuff (index + 2) b
    VM.write bBuff (index + 3) a
    where index = (x + y * width) * 4

drawPoint :: (RealFrac a, Floating a) => V2 a -> Engine ()
drawPoint (V2 x y)
  | x' >= 0 && y' >= 0 && x' < width && y' < height
    = putPixel (255,0,255,255) (x', y') 
  | otherwise 
    = return ()
    where (x', y') = (floor x, floor y)

renderMesh :: Mesh -> Engine ()
renderMesh (Mesh {..}) = do
  EngineState {..} <- ask
  Camera {..}      <- liftIO $ readIORef camera
  vs               <- liftIO $ readIORef vertices
  (V3 rx ry rz)    <- liftIO $ readIORef rotation
  (V3 px py pz)    <- liftIO $ readIORef meshPos
  cPos             <- liftIO $ readIORef camPos
  cT               <- liftIO $ readIORef target
  
  viewM            <- return $ lookAtLH cPos cT (V3 0 1 0)
  projM            <- return $ perspectiveLH 0.78 (w / h) 0.01 1.0
  worldM           <- return $ (rotYawPitchRoll ry rx rz) 
                             !*! (translate px py pz)
  transM           <- return $ worldM !*! viewM !*! projM
  
  mapM_ (drawPoint . project transM) vs
  where (w, h) = (fromIntegral width, fromIntegral height)

render :: Engine ()
render = do
  EngineState {..} <- ask
  ms               <- liftIO $ readIORef meshes

  mapM_ renderMesh ms