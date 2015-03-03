{-# LINE 1 "Graphics/UI/SDL/Surface.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Surface.hsc" #-}
module Graphics.UI.SDL.Surface
    ( blitScaled
    , blitSurface
    , createRGBSurface
    , fillRect
    , fillRects
    , freeSurface
    , getSurfaceAlphaMod
    , loadBMP
    , lockSurface
    , setColorKey
    , setSurfaceAlphaMod
    , unlockSurface
    ) where

import Data.Vector.Storable (Vector)
import Foreign
import Foreign.C
import Graphics.UI.SDL.Color (Color)
import Graphics.UI.SDL.Rect (Rect)
import Graphics.UI.SDL.Utilities (fatalSDLBool)
import Graphics.UI.SDL.Types (RWopsStruct, Surface, SurfaceStruct)
import Graphics.UI.SDL.Raw

import qualified Data.Vector.Storable as V
import qualified Graphics.UI.SDL.RWOps as RWOps

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_FillRect"
  sdlFillRect :: Ptr SurfaceStruct -> Ptr Rect -> Word32 -> IO Int32
{-# LINE 32 "Graphics/UI/SDL/Surface.hsc" #-}

fillRect :: Surface -> Rect -> Color -> IO ()
fillRect s r color =
  withForeignPtr s $ \cS ->
  with r $ \cR ->
    colorToInt color >>= fatalSDLBool "SDL_FillRect" . sdlFillRect cS cR

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_FillRects"
  sdlFillRects :: Ptr SurfaceStruct -> Ptr Rect -> Int32 -> Word32 -> IO Int32
{-# LINE 42 "Graphics/UI/SDL/Surface.hsc" #-}

fillRects :: Surface -> Vector Rect -> Color -> IO ()
fillRects s rects color =
  withForeignPtr s $ \cS ->
  V.unsafeWith rects $ \cR ->
    colorToInt color >>=
      fatalSDLBool "SDL_FillRect" . sdlFillRects cS cR (fromIntegral $ V.length rects)

--------------------------------------------------------------------------------
colorToInt :: Color -> IO Word32
{-# LINE 52 "Graphics/UI/SDL/Surface.hsc" #-}
colorToInt color = alloca $ \colorPtr ->
  poke (castPtr colorPtr) color >> peek colorPtr

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_UnlockSurface"
  sdlUnlockSurface :: Ptr SurfaceStruct -> IO ()

unlockSurface :: Surface -> IO ()
unlockSurface s = withForeignPtr s sdlUnlockSurface

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_LockSurface"
  sdlLockSurface :: Ptr SurfaceStruct -> IO ()

lockSurface :: Surface -> IO ()
lockSurface s = withForeignPtr s sdlLockSurface

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_CreateRGBSurface"
  sdlCreateRGBSurface :: Word32 -> Int32 -> Int32 -> Int32
{-# LINE 72 "Graphics/UI/SDL/Surface.hsc" #-}
                      -> Word32 -> Word32 -> Word32
{-# LINE 73 "Graphics/UI/SDL/Surface.hsc" #-}
                      -> Word32 -> IO (Ptr SurfaceStruct)
{-# LINE 74 "Graphics/UI/SDL/Surface.hsc" #-}

createRGBSurface
  :: Int32 -> Int32 -> Int32 -> Word32
{-# LINE 77 "Graphics/UI/SDL/Surface.hsc" #-}
  -> Word32-> Word32 -> Word32 -> IO Surface
{-# LINE 78 "Graphics/UI/SDL/Surface.hsc" #-}
createRGBSurface w h depth rMask gMask bMask aMask =
  sdlCreateRGBSurface 0 w h depth rMask gMask bMask aMask
    >>= mkFinalizedSurface

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_LoadBMP_RW"
  sdlLoadBMP :: Ptr RWopsStruct -> CInt -> IO (Ptr SurfaceStruct)

-- TODO Decide if this should be partial or return Maybe/Either
loadBMP :: FilePath -> IO Surface
loadBMP path =
  RWOps.withFile path "r" $ \rwops ->
  withForeignPtr rwops $ \crwops -> do
    bmp <- sdlLoadBMP crwops 0
    if bmp == nullPtr
      then error "loadBMP: failed to load BMP"
      else mkFinalizedSurface bmp

--------------------------------------------------------------------------------
freeSurface :: Surface -> IO ()
freeSurface = finalizeForeignPtr

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_SetColorKey"
  sdlSetColorKey :: Ptr SurfaceStruct -> CInt -> Word32 -> IO Int32
{-# LINE 103 "Graphics/UI/SDL/Surface.hsc" #-}

setColorKey :: Surface -> Bool -> Word32 -> IO ()
setColorKey s enabled pixel = withForeignPtr s $ \cs ->
  fatalSDLBool "SDL_SetColorKey" $
    sdlSetColorKey cs (if enabled then 1 else 0) pixel

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_SetSurfaceAlphaMod"
  sdlSetSurfaceAlphaMod :: Ptr SurfaceStruct -> Word8 -> IO Int32
{-# LINE 112 "Graphics/UI/SDL/Surface.hsc" #-}

setSurfaceAlphaMod :: Surface -> Word8 -> IO ()
{-# LINE 114 "Graphics/UI/SDL/Surface.hsc" #-}
setSurfaceAlphaMod s alphaMod = withForeignPtr s $ \cS ->
  fatalSDLBool "SDL_SetSurfaceAlphaMod" $ sdlSetSurfaceAlphaMod cS alphaMod

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetSurfaceAlphaMod"
  sdlGetSurfaceAlphaMod :: Ptr SurfaceStruct -> Ptr Word8 -> IO Int32
{-# LINE 120 "Graphics/UI/SDL/Surface.hsc" #-}

getSurfaceAlphaMod :: Surface -> IO Word8
{-# LINE 122 "Graphics/UI/SDL/Surface.hsc" #-}
getSurfaceAlphaMod s =
  withForeignPtr s $ \cS ->
  alloca $ \alphaModPtr -> do
    fatalSDLBool "SDL_GetSurfaceAlphaMod" $ sdlGetSurfaceAlphaMod cS alphaModPtr
    peek alphaModPtr

--------------------------------------------------------------------------------
type SDLBlitF = Ptr SurfaceStruct -> Ptr Rect -> Ptr SurfaceStruct -> Ptr Rect -> IO Int32
{-# LINE 130 "Graphics/UI/SDL/Surface.hsc" #-}

foreign import ccall safe "SDL_UpperBlit"
  sdlUpperBlit :: SDLBlitF

foreign import ccall safe "SDL_UpperBlitScaled"
  sdlUpperBlitScaled :: SDLBlitF

-- mirror the defines in SDL_surface.h
sdlBlitSurface :: SDLBlitF
sdlBlitSurface = sdlUpperBlit
sdlBlitScaled :: SDLBlitF
sdlBlitScaled = sdlUpperBlitScaled

blitSurface :: Surface -> Maybe Rect -> Surface -> Maybe Rect -> IO ()
blitSurface = doBlit "SDL_BlitSurface" sdlBlitSurface

blitScaled :: Surface -> Maybe Rect -> Surface -> Maybe Rect -> IO ()
blitScaled = doBlit "SDL_BlitScaled" sdlBlitScaled

doBlit :: String -> SDLBlitF -> Surface -> Maybe Rect -> Surface -> Maybe Rect -> IO ()
doBlit name f srcSurface srcRect dstSurface dstRect =
  withForeignPtr srcSurface $ \srcSurfacePtr ->
  withForeignPtr dstSurface $ \dstSurfacePtr ->
  maybeWith with srcRect $ \srcRectPtr ->
  maybeWith with dstRect $ \dstRectPtr ->
  fatalSDLBool name $
    f srcSurfacePtr srcRectPtr dstSurfacePtr dstRectPtr
