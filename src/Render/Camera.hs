module Render.Camera where

import Data.IORef (IORef)
import Linear.V3

data Camera = Camera
  { camPos :: IORef (V3 Double)
  , target :: IORef (V3 Double)
  }