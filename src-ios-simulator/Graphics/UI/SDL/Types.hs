{-# LINE 1 "Graphics/UI/SDL/Types.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Types.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Types
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Types
  ( SDL_bool
  , WindowStruct, Window
  , RendererStruct, Renderer
  , GLContextStruct, GLContext
  , SurfaceStruct, Surface
  , RWopsStruct, RWops
  , TextureStruct, Texture
  , Size(..), mkSize
  , Position(..), mkPosition
  , WindowFlag(..)
  , windowFlagToC
  , RenderingDevice(..)
  , RendererFlag(..)
  , rendererFlagToC
  , PixelFormatStruct
  , PixelFormat
  , CursorStruct
  , Cursor
  , PixelFormatEnum(..)
  , pixelFormatEnumFromC
  , pixelFormatEnumToC
  , TextureAccess(..)
  , textureAccessToC
  ) where

import Foreign.C (CInt, CUInt)
import Foreign
--import System.IO.Unsafe (unsafePerformIO)

--import Graphics.UI.SDL.Utilities (Enum(..), fromBitmask)
--import Graphics.UI.SDL.Color (Pixel(..))

type SDL_bool = Word32
{-# LINE 47 "Graphics/UI/SDL/Types.hsc" #-}

data WindowStruct
type Window = ForeignPtr WindowStruct

data RendererStruct
type Renderer = ForeignPtr RendererStruct

data SurfaceStruct
type Surface = ForeignPtr SurfaceStruct

data RWopsStruct
type RWops = ForeignPtr RWopsStruct

data TextureStruct
type Texture = ForeignPtr TextureStruct

data GLContextStruct
type GLContext = ForeignPtr GLContextStruct

data PixelFormatStruct
type PixelFormat = ForeignPtr PixelFormatStruct

data CursorStruct
type Cursor = ForeignPtr CursorStruct

data Size = Size { sizeWidth :: Int, sizeHeight :: Int }
  deriving ( Read, Show, Eq, Ord )

-- smart constructor
mkSize :: CInt -> CInt -> Size
mkSize width height = Size (fromIntegral width) (fromIntegral height)

data Position = Position { positionX :: Int, positionY :: Int }
  deriving ( Read, Show, Eq, Ord )

-- smart constructor
mkPosition :: CInt -> CInt -> Position
mkPosition x y = Position (fromIntegral x) (fromIntegral y)

data PixelFormatEnum
  = PixelFormatUnknown
  | PixelFormatIndex1LSB
  | PixelFormatIndex1MSB
  | PixelFormatIndex4LSB
  | PixelFormatIndex4MSB
  | PixelFormatIndex8
  | PixelFormatRGB332
  | PixelFormatRGB444
  | PixelFormatRGB555
  | PixelFormatBGR555
  | PixelFormatARGB4444
  | PixelFormatRGBA4444
  | PixelFormatABGR4444
  | PixelFormatBGRA4444
  | PixelFormatARGB1555
  | PixelFormatRGBA5551
  | PixelFormatBGRA5551
  | PixelFormatRGB565
  | PixelFormatBGR565
  | PixelFormatRGB24
  | PixelFormatBGR24
  | PixelFormatRGB888
  | PixelFormatRGBX8888
  | PixelFormatBGR888
  | PixelFormatBGRX8888
  | PixelFormatARGB8888
  | PixelFormatRGBA8888
  | PixelFormatABGR8888
  | PixelFormatBGRA8888
  | PixelFormatARGB2101010
  | PixelFormatYV12
  | PixelFormatIYUV
  | PixelFormatYUY2
  | PixelFormatUYVY
  | PixelFormatYVYU
  deriving (Eq, Show)

pixelFormatEnumFromC :: Word32 -> PixelFormatEnum
pixelFormatEnumFromC 0 = PixelFormatUnknown
{-# LINE 126 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 286261504 = PixelFormatIndex1LSB
{-# LINE 127 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 287310080 = PixelFormatIndex1MSB
{-# LINE 128 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 303039488 = PixelFormatIndex4LSB
{-# LINE 129 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 304088064 = PixelFormatIndex4MSB
{-# LINE 130 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 318769153 = PixelFormatIndex8
{-# LINE 131 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 336660481 = PixelFormatRGB332
{-# LINE 132 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 353504258 = PixelFormatRGB444
{-# LINE 133 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 353570562 = PixelFormatRGB555
{-# LINE 134 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 357764866 = PixelFormatBGR555
{-# LINE 135 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 355602434 = PixelFormatARGB4444
{-# LINE 136 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 356651010 = PixelFormatRGBA4444
{-# LINE 137 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 359796738 = PixelFormatABGR4444
{-# LINE 138 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 360845314 = PixelFormatBGRA4444
{-# LINE 139 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 355667970 = PixelFormatARGB1555
{-# LINE 140 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 356782082 = PixelFormatRGBA5551
{-# LINE 141 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 359862274 = PixelFormatBGRA5551
{-# LINE 142 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 353701890 = PixelFormatRGB565
{-# LINE 143 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 357896194 = PixelFormatBGR565
{-# LINE 144 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 386930691 = PixelFormatRGB24
{-# LINE 145 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 390076419 = PixelFormatBGR24
{-# LINE 146 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 370546692 = PixelFormatRGB888
{-# LINE 147 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 371595268 = PixelFormatRGBX8888
{-# LINE 148 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 374740996 = PixelFormatBGR888
{-# LINE 149 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 375789572 = PixelFormatBGRX8888
{-# LINE 150 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 372645892 = PixelFormatARGB8888
{-# LINE 151 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 373694468 = PixelFormatRGBA8888
{-# LINE 152 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 376840196 = PixelFormatABGR8888
{-# LINE 153 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 377888772 = PixelFormatBGRA8888
{-# LINE 154 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 372711428 = PixelFormatARGB2101010
{-# LINE 155 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 842094169 = PixelFormatYV12
{-# LINE 156 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 1448433993 = PixelFormatIYUV
{-# LINE 157 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 844715353 = PixelFormatYUY2
{-# LINE 158 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 1498831189 = PixelFormatUYVY
{-# LINE 159 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC 1431918169 = PixelFormatYVYU
{-# LINE 160 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumFromC unknown = error $ "Graphics.UI.SDL.Types.pixelFormatEnumFromC: unknown PixelFormat: " ++ show unknown

pixelFormatEnumToC :: PixelFormatEnum -> Word32
pixelFormatEnumToC PixelFormatUnknown = 0
{-# LINE 164 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatIndex1LSB = 286261504
{-# LINE 165 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatIndex1MSB = 287310080
{-# LINE 166 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatIndex4LSB = 303039488
{-# LINE 167 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatIndex4MSB = 304088064
{-# LINE 168 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatIndex8 = 318769153
{-# LINE 169 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatRGB332 = 336660481
{-# LINE 170 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatRGB444 = 353504258
{-# LINE 171 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatRGB555 = 353570562
{-# LINE 172 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatBGR555 = 357764866
{-# LINE 173 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatARGB4444 = 355602434
{-# LINE 174 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatRGBA4444 = 356651010
{-# LINE 175 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatABGR4444 = 359796738
{-# LINE 176 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatBGRA4444 = 360845314
{-# LINE 177 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatARGB1555 = 355667970
{-# LINE 178 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatRGBA5551 = 356782082
{-# LINE 179 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatBGRA5551 = 359862274
{-# LINE 180 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatRGB565 = 353701890
{-# LINE 181 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatBGR565 = 357896194
{-# LINE 182 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatRGB24 = 386930691
{-# LINE 183 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatBGR24 = 390076419
{-# LINE 184 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatRGB888 = 370546692
{-# LINE 185 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatRGBX8888 = 371595268
{-# LINE 186 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatBGR888 = 374740996
{-# LINE 187 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatBGRX8888 = 375789572
{-# LINE 188 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatARGB8888 = 372645892
{-# LINE 189 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatRGBA8888 = 373694468
{-# LINE 190 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatABGR8888 = 376840196
{-# LINE 191 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatBGRA8888 = 377888772
{-# LINE 192 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatARGB2101010 = 372711428
{-# LINE 193 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatYV12 = 842094169
{-# LINE 194 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatIYUV = 1448433993
{-# LINE 195 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatYUY2 = 844715353
{-# LINE 196 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatUYVY = 1498831189
{-# LINE 197 "Graphics/UI/SDL/Types.hsc" #-}
pixelFormatEnumToC PixelFormatYVYU = 1431918169
{-# LINE 198 "Graphics/UI/SDL/Types.hsc" #-}

data TextureAccess
  = TextureAccessStatic
  | TextureAccessStreaming
  | TextureAccessTarget

textureAccessToC :: TextureAccess -> CInt
textureAccessToC TextureAccessStatic = 0
{-# LINE 206 "Graphics/UI/SDL/Types.hsc" #-}
textureAccessToC TextureAccessStreaming = 1
{-# LINE 207 "Graphics/UI/SDL/Types.hsc" #-}
textureAccessToC TextureAccessTarget = 2
{-# LINE 208 "Graphics/UI/SDL/Types.hsc" #-}

data WindowFlag
  = WindowFullscreen         -- ^ fullscreen window
  | WindowFullscreenDesktop  -- ^ fullscreen window at the current desktop resolution
  | WindowOpengl             -- ^ window usable with OpenGL context
  | WindowShown              -- ^ window is visible
  | WindowHidden             -- ^ window is not visible
  | WindowBorderless         -- ^ no window decoration
  | WindowResizable          -- ^ window can be resized
  | WindowMinimized          -- ^ window is minimized
  | WindowMaximized          -- ^ window is maximized
  | WindowInputGrabbed       -- ^ window has grabbed input focus
  | WindowInputFocus         -- ^ window has input focus
  | WindowMouseFocus         -- ^ window has mouse focus
  | WindowForeign            -- ^ window not created by SDL
    deriving ( Eq, Ord, Read, Show, Bounded, Enum )

windowFlagToC :: WindowFlag -> Word32
{-# LINE 226 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowFullscreen        = 1
{-# LINE 227 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowFullscreenDesktop = 4097
{-# LINE 228 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowOpengl            = 2
{-# LINE 229 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowShown             = 4
{-# LINE 230 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowHidden            = 8
{-# LINE 231 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowBorderless        = 16
{-# LINE 232 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowResizable         = 32
{-# LINE 233 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowMinimized         = 64
{-# LINE 234 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowMaximized         = 128
{-# LINE 235 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowInputGrabbed      = 256
{-# LINE 236 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowInputFocus        = 512
{-# LINE 237 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowMouseFocus        = 1024
{-# LINE 238 "Graphics/UI/SDL/Types.hsc" #-}
windowFlagToC WindowForeign           = 2048
{-# LINE 239 "Graphics/UI/SDL/Types.hsc" #-}

data RenderingDevice = Device Int | FirstSupported
  deriving ( Eq, Ord, Read, Show )

data RendererFlag = Software | Accelerated | PresentVSync | TargetTexture
  deriving ( Eq, Ord, Read, Show, Bounded, Enum )

rendererFlagToC :: RendererFlag -> CUInt
rendererFlagToC Software = 1
{-# LINE 248 "Graphics/UI/SDL/Types.hsc" #-}
rendererFlagToC Accelerated = 2
{-# LINE 249 "Graphics/UI/SDL/Types.hsc" #-}
rendererFlagToC PresentVSync = 4
{-# LINE 250 "Graphics/UI/SDL/Types.hsc" #-}
rendererFlagToC TargetTexture = 8
{-# LINE 251 "Graphics/UI/SDL/Types.hsc" #-}

