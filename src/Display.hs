{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Display
  ( initWindow
  , destroy
  , present
  ) where

import Control.Monad.Reader (ask, liftIO)
import Data.Types (scrnW, scrnH)
import Linear.V2
import Render.Engine
import qualified SDL 
  ( PixelFormat (BGR888)
  , Surface
  , Window
  , InitFlag (InitVideo)
  , windowInitialSize
  , defaultWindow
  , createWindow
  , initialize
  , createRGBSurfaceFrom
  , surfaceBlit
  , freeSurface
  , quit
  , destroyWindow
  , updateWindowSurface
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
  