{-# LINE 1 "Graphics/UI/SDL/Mouse.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Mouse.hsc" #-}
module Graphics.UI.SDL.Mouse
  ( createColorCursor
  , setCursor
  , createSystemCursor
  , systemCursorArrow
  , systemCursorIBeam
  , systemCursorWait
  , systemCursorCrosshair
  , systemCursorWaitArrow
  , systemCursorSizeNWSE
  , systemCursorSizeNESW
  , systemCursorSizeWE
  , systemCursorSizeNS
  , systemCursorSizeAll
  , systemCursorNo
  , systemCursorHand
  , numSystemCursors
  , freeCursor
  , getCursor
  , getDefaultCursor
  , getMouseFocus
  , getRelativeMouseMode
  , setRelativeMouseMode
  , CursorToggle(..)
  , showCursor
  , warpMouseInWindow
  ) where

import Foreign
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Error
import Graphics.UI.SDL.Utilities


foreign import ccall unsafe "SDL_CreateColorCursor"
  sdlCreateColorCursor :: Ptr SurfaceStruct -> Int32 -> Int32 -> IO (Ptr CursorStruct)
{-# LINE 41 "Graphics/UI/SDL/Mouse.hsc" #-}

createColorCursor :: Surface -> Int32 -> Int32 -> IO (Ptr CursorStruct)
{-# LINE 43 "Graphics/UI/SDL/Mouse.hsc" #-}
createColorCursor s x y =
  withForeignPtr s $ \sptr ->
    fatalSDLNull "SDL_CreateColorCursor" (sdlCreateColorCursor sptr x y)

newtype SystemCursor = SystemCursor { unwrapSystemCursor :: Int32 }
{-# LINE 48 "Graphics/UI/SDL/Mouse.hsc" #-}
systemCursorArrow      :: SystemCursor
systemCursorArrow      = SystemCursor 0
systemCursorIBeam      :: SystemCursor
systemCursorIBeam      = SystemCursor 1
systemCursorWait       :: SystemCursor
systemCursorWait       = SystemCursor 2
systemCursorCrosshair  :: SystemCursor
systemCursorCrosshair  = SystemCursor 3
systemCursorWaitArrow  :: SystemCursor
systemCursorWaitArrow  = SystemCursor 4
systemCursorSizeNWSE   :: SystemCursor
systemCursorSizeNWSE   = SystemCursor 5
systemCursorSizeNESW   :: SystemCursor
systemCursorSizeNESW   = SystemCursor 6
systemCursorSizeWE     :: SystemCursor
systemCursorSizeWE     = SystemCursor 7
systemCursorSizeNS     :: SystemCursor
systemCursorSizeNS     = SystemCursor 8
systemCursorSizeAll    :: SystemCursor
systemCursorSizeAll    = SystemCursor 9
systemCursorNo         :: SystemCursor
systemCursorNo         = SystemCursor 10
systemCursorHand       :: SystemCursor
systemCursorHand       = SystemCursor 11
numSystemCursors       :: SystemCursor
numSystemCursors       = SystemCursor 12

{-# LINE 63 "Graphics/UI/SDL/Mouse.hsc" #-}

foreign import ccall unsafe "SDL_CreateSystemCursor"
  sdlCreateSystemCursor :: Int32-> IO (Ptr CursorStruct)
{-# LINE 66 "Graphics/UI/SDL/Mouse.hsc" #-}

createSystemCursor :: SystemCursor -> IO (Ptr CursorStruct)
createSystemCursor sc =
  fatalSDLNull "SDL_CreateSystemCursor" $ sdlCreateSystemCursor (unwrapSystemCursor sc)

foreign import ccall unsafe "SDL_FreeCursor"
  freeCursor :: Ptr CursorStruct -> IO ()

foreign import ccall unsafe "SDL_GetCursor"
  sdlGetCursor :: IO (Ptr CursorStruct)

getCursor :: IO (Ptr CursorStruct)
getCursor =
  fatalSDLNull "SDL_GetCursor" $ sdlGetCursor

foreign import ccall unsafe "SDL_GetDefaultCursor"
  sdlGetDefaultCursor :: IO (Ptr CursorStruct)

getDefaultCursor :: IO (Ptr CursorStruct)
getDefaultCursor =
  fatalSDLNull "SDL_GetDefaultCursor" sdlGetDefaultCursor

foreign import ccall unsafe "SDL_GetMouseFocus"
  getMouseFocus :: IO (Ptr WindowStruct)

foreign import ccall unsafe "SDL_GetRelativeMouseMode"
  sdlGetRelativeMouseMode :: IO SDL_bool

getRelativeMouseMode :: IO Bool
getRelativeMouseMode = fmap sdlBoolToBool sdlGetRelativeMouseMode

foreign import ccall unsafe "SDL_SetCursor"
  setCursor :: Ptr CursorStruct -> IO ()

foreign import ccall unsafe "SDL_SetRelativeMouseMode"
  sdlSetRelativeMouseMode :: SDL_bool -> IO Int32
{-# LINE 102 "Graphics/UI/SDL/Mouse.hsc" #-}

setRelativeMouseMode :: Bool -> IO ()
setRelativeMouseMode toggle =
  fatalSDLBool "SDL_SetRelativeMouseMode" $ sdlSetRelativeMouseMode sdl_toggle
    where sdl_toggle = boolToSdlBool toggle

data CursorToggle
  = Show
  | Hide
  | Query
  deriving (Eq, Show)

foreign import ccall unsafe "SDL_ShowCursor"
  sdlShowCursor :: Int32 -> IO Int32
{-# LINE 116 "Graphics/UI/SDL/Mouse.hsc" #-}

showCursor :: CursorToggle -> IO CursorToggle
showCursor curshow = do
  ret <- sdlShowCursor $ cursorToggleToC curshow
  case ret of
    0 -> return Hide
    1 -> return Show
    _ -> (fromMaybe "(no error message)" <$> getError) >>= \msg ->
           error $ "SDL_ShowCursor failed: " ++ msg
  where cursorToggleToC Show  = 1
        cursorToggleToC Hide  = 0
        cursorToggleToC Query = -1

foreign import ccall unsafe "SDL_WarpMouseInWindow"
  warpMouseInWindow :: Ptr WindowStruct -> Int32 -> Int32 -> IO ()
{-# LINE 131 "Graphics/UI/SDL/Mouse.hsc" #-}

