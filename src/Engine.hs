{-# LANGUAGE RecordWildCards #-}

module Engine
  ( engine
  ) where

import Control.Monad (forever)
import Control.Monad.Reader (ask, liftIO)
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef)
import Display
import Linear.V3
import Render
import Render.Engine
import Render.Mesh
import qualified SDL (getWindowSurface, showWindow)

engine :: Engine ()
engine = do
  EngineState {..} <- ask
  ms               <- liftIO $ readIORef meshes
  window           <- liftIO initWindow
  screen           <- liftIO $ SDL.getWindowSurface window

  liftIO $ SDL.showWindow window

  forever $ do
    clearWithCol (255, 0, 255, 100)
    mapM_ transform ms
    render
    present window screen
    liftIO $ threadDelay (floor $ 1000000.0 / 60.0)
