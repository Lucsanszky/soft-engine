{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Engine
  ( engine
  ) where

import Control.Monad.Loops (whileM_)
import Control.Monad.Reader (ask, liftIO)
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, writeIORef)
import Data.Types (scrnW, scrnH)
import Linear.V2
import Linear.V3
import Render
import Render.Engine
import Render.Mesh
import qualified SDL 
  ( PixelFormat (BGR888)
  , Surface
  , Window
  , InitFlag (InitVideo)
  , windowInitialSize
  , getWindowSurface
  , defaultWindow
  , createWindow
  , initialize
  , createRGBSurfaceFrom
  , surfaceBlit
  , freeSurface
  , quit
  , destroyWindow
  , updateWindowSurface
  , showWindow
  )

initWindow :: IO SDL.Window
initWindow = do 
  SDL.initialize [SDL.InitVideo]
  SDL.createWindow "Hobo" SDL.defaultWindow 
                   {SDL.windowInitialSize = V2 scrnW scrnH}

destroy :: SDL.Window -> SDL.Surface -> Engine ()
destroy wind screen = liftIO $ do
  SDL.destroyWindow wind
  SDL.freeSurface screen
  SDL.quit

present :: SDL.Window -> SDL.Surface -> Engine ()
present wind screen = do 
  EngineState {..} <- ask
  surf             <- SDL.createRGBSurfaceFrom 
                      bBuff 
                      (V2 scrnW scrnH) 
                      (scrnW * 4) 
                      SDL.BGR888

  SDL.surfaceBlit surf Nothing screen Nothing
  SDL.updateWindowSurface wind

engine :: Engine ()
engine = do
  EngineState {..}   <- ask
  ((Mesh {..}) : ms) <- liftIO $ readIORef meshes 
  window             <- liftIO $ initWindow
  screen             <- liftIO $ SDL.getWindowSurface window

  liftIO $ SDL.showWindow window

  whileM_ (return True) $ do
    (V3 rx ry rz) <- liftIO $ readIORef rotation
    clear
    liftIO $ writeIORef rotation $ V3 (rx + 0.01) (ry + 0.01) rz
    render
    present window screen
    liftIO $ threadDelay (floor $ 1000000.0 / 60.0)
