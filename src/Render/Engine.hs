{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Render.Engine where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.IORef (IORef)
import qualified Data.Vector.Storable.Mutable as VM (MVector)
import Data.Word8 (Word8)
import GHC.Prim (RealWorld)
import Render.Camera
import Render.Mesh

data EngineState = EngineState
  { camera :: IORef Camera 
  , meshes :: IORef [Mesh]
  , bBuff  :: VM.MVector RealWorld Word8
  } 

newtype Engine a = Engine
  { start :: (ReaderT EngineState IO) a 
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadReader EngineState
             )