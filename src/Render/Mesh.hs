module Render.Mesh where

import Data.IORef (IORef)
import Linear.V3

data Mesh = Mesh
  { name     :: String
  , vertices :: IORef [V3 Double]
  , faces    :: IORef [V3 Int]
  , meshPos  :: IORef (V3 Double)
  , rotation :: IORef (V3 Double)
  } 