module Main where

import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef)
import Data.Types (bufLen)
import qualified Data.Vector.Storable.Mutable as VM (replicate)
import Engine
import Linear.V3
import Render
import Render.Camera
import Render.Engine
import Render.Mesh

main :: IO ()
main = do
  camera <- Camera <$> newIORef (V3 0 0 10)
                   <*> newIORef (V3 0 0 0)

  mesh <- Mesh <$> return "Cube"
               <*> newIORef [ V3 (-1) 1 1
                            , V3 1 1 1
                            , V3 (-1) (-1) 1
                            , V3 1 (-1) 1
                            , V3 (-1) 1 (-1)
                            , V3 1 1 (-1)
                            , V3 1 (-1) (-1)
                            , V3 (-1) (-1) (-1)
                            ]
               <*> newIORef [ V3 0 1 2
                            , V3 1 2 3 
                            , V3 1 3 6 
                            , V3 1 5 6 
                            , V3 0 1 4 
                            , V3 1 4 5 
                            , V3 2 3 7 
                            , V3 3 6 7 
                            , V3 0 2 7 
                            , V3 0 4 7 
                            , V3 4 5 6 
                            , V3 4 6 7 
                            ]
               <*> newIORef (V3 0 0 0)
               <*> newIORef (V3 0 0 0)
               
  buf <- VM.replicate bufLen 0

  initState <- EngineState <$> newIORef camera 
                           <*> newIORef [mesh] 
                           <*> return buf

  runReaderT (start engine) initState