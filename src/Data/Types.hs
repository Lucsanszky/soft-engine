module Data.Types
  ( RGBA
  , Coordinate
  , width, height
  , scrnW, scrnH
  , bufLen
  ) where

import Data.Word8 (Word8)
import Foreign.C.Types (CInt)

type RGBA = (Word8, Word8, Word8, Word8)
type Coordinate = (Int, Int)

width, height :: Int
(width, height) = (640, 480)

scrnW, scrnH :: CInt
(scrnW, scrnH) = (640, 480)

bufLen :: Int
bufLen = width * height * 4 - 1