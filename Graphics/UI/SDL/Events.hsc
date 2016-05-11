#include "SDL.h"
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Events
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Events where

import Control.Applicative
import Control.Monad ((>=>), void)
import Data.Int
import Data.Maybe
import Data.Word
import Foreign hiding (void)
import Foreign.C
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.Types (Position, Size, mkPosition, mkSize)

data Event = Event { eventTimestamp :: Word32, eventData :: EventData }
  deriving (Eq, Show)

data MouseButton = LeftButton | RightButton | MiddleButton | MouseX1 | MouseX2 | UnknownButton !Word8
  deriving (Eq, Show)

data MouseButtonState = Pressed | Released
  deriving (Eq, Show)

data Finger = Finger { fingerId       :: CLong
                     , fingerX        :: CFloat
                     , fingerY        :: CFloat
                     , fingerPressure :: CFloat
                     } deriving (Eq, Show)

instance Enum MouseButton where
  toEnum #{const SDL_BUTTON_LEFT} = LeftButton
  toEnum #{const SDL_BUTTON_MIDDLE} = MiddleButton
  toEnum #{const SDL_BUTTON_RIGHT} = RightButton
  toEnum #{const SDL_BUTTON_X1} = MouseX1
  toEnum #{const SDL_BUTTON_X2} = MouseX2
  toEnum k = UnknownButton (fromIntegral k)

  fromEnum LeftButton = #{const SDL_BUTTON_LEFT}
  fromEnum MiddleButton = #{const SDL_BUTTON_MIDDLE}
  fromEnum RightButton = #{const SDL_BUTTON_RIGHT}
  fromEnum MouseX1 = #{const SDL_BUTTON_X1}
  fromEnum MouseX2 = #{const SDL_BUTTON_X2}
  fromEnum (UnknownButton k) = fromIntegral k

mouseButtonToMask :: MouseButton -> Word32
mouseButtonToMask b = case b of
  LeftButton   -> #{const SDL_BUTTON_LMASK}
  MiddleButton -> #{const SDL_BUTTON_MMASK}
  RightButton  -> #{const SDL_BUTTON_RMASK}
  MouseX1      -> #{const SDL_BUTTON_X1MASK}
  MouseX2      -> #{const SDL_BUTTON_X2MASK}
  _            -> 0

data EventData
  = Keyboard { keyMovement :: KeyMovement
             , keyWindowID :: Word32
             , keyRepeat :: Bool
             , keySym :: Keysym
             }
  | Window { windowID :: Word32
           , windowEvent :: WindowEvent
           }
  | TextEditing -- TODO
  | TextInput { textInputWindowID :: Word32
              , textInput :: String
              }
  | MouseMotion { mouseMotionWindowID :: Word32
                , mouseMotionMouseID :: Word32
                , mouseMotionState :: [MouseButton]
                , mouseMotionPosition :: Position
                , mouseMotionXRelMotion :: Int32
                , mouseMotionYRelMotion :: Int32 -- TODO Tuple up?
                }
  | MouseButton { mouseButtonWindowID :: Word32
                , mouseButtonMouseID :: Word32
                , mouseButton :: MouseButton
                , mouseButtonState :: MouseButtonState
                , mouseButtonAt :: Position
                }
  | MouseWheel { mouseWheelWindowID :: Word32
               , mouseWheelMouseID :: Word32
               , mouseWheelHorizontalScroll :: Int32
               , mouseWheelVerticalScroll :: Int32
               }
  | JoyAxis -- TODO
  | JoyBall -- TODO
  | JoyHat -- TODO
  | JoyButton -- TODO
  | JoyDevice -- TODO
  | ControllerAxis -- TODO
  | ControllerButton -- TODO
  | ControllerDevice -- TODO
  | TouchFinger { touchFingerEvent :: TouchFingerEvent
                , touchTimestamp :: Word32
                , touchID :: CLong
                , touchFingerID :: CLong
                , touchX :: CFloat
                , touchY :: CFloat
                , touchDx :: CFloat
                , touchDy :: CFloat
                , touchPressure :: CFloat
                }
  | MultiGesture -- TODO
  | DollarGesture -- TODO
  | Drop -- TODO
  | Terminating -- TODO
  | LowMemory -- TODO
  | AppWillEnterBackground -- TODO
  | AppDidEnterBackground -- TODO
  | AppWillEnterForeground -- TODO
  | AppDidEnterForeground -- TODO
  | AudioDeviceAdded -- TODO
  | AudioDeviceRemoved -- TODO
  | ClipboardUpdate -- TODO
  | RenderTargetsReset -- TODO
  | UserEvent -- TODO

  | Quit
  deriving (Eq, Show)

data WindowEvent
  = Shown
  | Hidden
  | Exposed
  | Moved { windowMovedTo :: Position }
  | Resized { windowResizedTo :: Size }
  | SizeChanged
  | Minimized
  | Maximized
  | Restored
  | GainedMouseFocus
  | LostMouseFocus
  | GainedKeyboardFocus
  | LostKeyboardFocus
  | Closing
  deriving (Eq, Show)

data KeyMovement = KeyUp | KeyDown
  deriving (Eq, Show)

data TouchFingerEvent = TouchFingerMotion | TouchFingerDown | TouchFingerUp
  deriving (Eq, Show)

instance Storable Event where
  sizeOf = const #{size SDL_Event}

  alignment = const 4

  poke ptr (Event timestamp body) = do
    #{poke SDL_CommonEvent, type} ptr (sdlEventType body)
    #{poke SDL_CommonEvent, timestamp} ptr timestamp

    case body of
      Keyboard m w r s -> do
        #{poke SDL_KeyboardEvent, windowID} ptr w
        #{poke SDL_KeyboardEvent, state} ptr (sdlKeyState m)
        #{poke SDL_KeyboardEvent, repeat} ptr
          (if r then 1 else 0 :: Word8)
        #{poke SDL_KeyboardEvent, padding2} ptr padding8
        #{poke SDL_KeyboardEvent, padding3} ptr padding8
        #{poke SDL_KeyboardEvent, keysym} ptr s
      _ -> error "poke: unhandled event type"

   where padding8 = 0 :: Word8

  peek ptr = do
    evType <- #{peek SDL_CommonEvent, type} ptr
    Event <$> #{peek SDL_CommonEvent, timestamp} ptr <*> peekEvent evType

   where

    peekEvent :: Word32 -> IO EventData
    peekEvent e
      | isKeyboard e =
          Keyboard <$> case e of
                        #{const SDL_KEYDOWN} -> pure KeyDown
                        #{const SDL_KEYUP} -> pure KeyUp
                        _ -> error "Unknown key movement when parsing SDL_KeybordEvent"
                   <*> #{peek SDL_KeyboardEvent, windowID} ptr
                   <*> (uint8Bool <$> #{peek SDL_KeyboardEvent, repeat} ptr)
                   <*> #{peek SDL_KeyboardEvent, keysym} ptr

      | isWindow e =
          Window <$> #{peek SDL_WindowEvent, windowID} ptr
                 <*> (#{peek SDL_WindowEvent, event} ptr >>= peekWindowEvent)

      | isTextInput e =
          TextInput <$> #{peek SDL_TextInputEvent, windowID} ptr
                    <*> peekCString (ptr `plusPtr` #{offset SDL_TextInputEvent, text})

      | isTextEditing e = pure TextEditing -- TODO

      | isMouseMotion e =
          MouseMotion <$> #{peek SDL_MouseMotionEvent, windowID} ptr
                      <*> #{peek SDL_MouseMotionEvent, which} ptr
                      <*> (mouseStateToButtons <$> #{peek SDL_MouseMotionEvent, state} ptr)
                      <*> (mkPosition <$> #{peek SDL_MouseMotionEvent, x} ptr
                                      <*> #{peek SDL_MouseMotionEvent, y} ptr)
                      <*> #{peek SDL_MouseMotionEvent, xrel} ptr
                      <*> #{peek SDL_MouseMotionEvent, yrel} ptr

      | isMouseButton e = do
          btnState <- #{peek SDL_MouseButtonEvent, state} ptr :: IO Word8
          MouseButton <$> #{peek SDL_MouseButtonEvent, windowID} ptr
                      <*> #{peek SDL_MouseButtonEvent, which} ptr
                      <*> (mkButton <$> #{peek SDL_MouseButtonEvent, button} ptr)
                      <*> return (case btnState of
                                    #{const SDL_PRESSED} -> Pressed
                                    #{const SDL_RELEASED} -> Released
                                    _ -> error "isMouseButton: unhandled mouse button state")
                      <*> (mkPosition <$> #{peek SDL_MouseButtonEvent, x} ptr
                                      <*> #{peek SDL_MouseButtonEvent, y} ptr)

      | isMouseWheel e =
          MouseWheel <$> #{peek SDL_MouseWheelEvent, windowID} ptr
                     <*> #{peek SDL_MouseWheelEvent, which} ptr
                     <*> #{peek SDL_MouseWheelEvent, x} ptr
                     <*> #{peek SDL_MouseWheelEvent, y} ptr

      | isJoyAxis e = pure JoyAxis
      | isJoyBall e = pure JoyBall
      | isJoyHat e = pure JoyHat
      | isJoyButton e = pure JoyButton
      | isJoyDevice e = pure JoyDevice
      | isControllerAxis e = pure ControllerAxis
      | isControllerButton e = pure ControllerButton
      | isTouchFinger e =
          TouchFinger <$> case e of
                        #{const SDL_FINGERMOTION} -> pure TouchFingerMotion
                        #{const SDL_FINGERDOWN} -> pure TouchFingerDown
                        #{const SDL_FINGERUP} -> pure TouchFingerUp
                        _ -> error "isTouchFinger: unhandled finger constant"
                      <*> #{peek SDL_TouchFingerEvent, timestamp} ptr
                      <*> #{peek SDL_TouchFingerEvent, touchId} ptr
                      <*> #{peek SDL_TouchFingerEvent, fingerId} ptr
                      <*> #{peek SDL_TouchFingerEvent, x} ptr
                      <*> #{peek SDL_TouchFingerEvent, y} ptr
                      <*> #{peek SDL_TouchFingerEvent, dx} ptr
                      <*> #{peek SDL_TouchFingerEvent, dy} ptr
                      <*> #{peek SDL_TouchFingerEvent, pressure} ptr

      | isMultiGesture e           = pure MultiGesture
      | isDollarGesture e          = pure DollarGesture
      | isTerminating e            = pure Terminating
      | isLowMemory e              = pure LowMemory
      | isAppWillEnterBackground e = pure AppWillEnterBackground
      | isAppDidEnterBackground e  = pure  AppDidEnterBackground
      | isAppWillEnterForeground e = pure AppWillEnterForeground
      | isAppDidEnterForeground e  = pure AppDidEnterForeground
      | isAudioDeviceAdded e       = pure AudioDeviceAdded
      | isAudioDeviceRemoved e     = pure AudioDeviceRemoved
      | isClipboardUpdate e        = pure ClipboardUpdate
      | isRenderTargetsReset e     = pure RenderTargetsReset
      | isUserEvent          e     = pure UserEvent
      | isDrop e                   = pure Drop
      | isQuit e                   = pure Quit
      | otherwise = error $ "Unknown event type: " ++ show e

    peekWindowEvent :: Word8 -> IO WindowEvent
    peekWindowEvent e = case e of
      #{const SDL_WINDOWEVENT_SHOWN} -> pure Shown
      #{const SDL_WINDOWEVENT_HIDDEN} -> pure Hidden
      #{const SDL_WINDOWEVENT_EXPOSED} -> pure Exposed
      #{const SDL_WINDOWEVENT_MOVED} ->
        Moved <$> (mkPosition <$> #{peek SDL_WindowEvent, data1} ptr
                              <*> #{peek SDL_WindowEvent, data2} ptr)
      #{const SDL_WINDOWEVENT_RESIZED} ->
        Resized <$> (mkSize <$> #{peek SDL_WindowEvent, data1} ptr
                            <*> #{peek SDL_WindowEvent, data2} ptr)
      #{const SDL_WINDOWEVENT_SIZE_CHANGED} -> pure SizeChanged
      #{const SDL_WINDOWEVENT_MINIMIZED} -> pure Minimized
      #{const SDL_WINDOWEVENT_MAXIMIZED} -> pure Maximized
      #{const SDL_WINDOWEVENT_RESTORED} -> pure Restored
      #{const SDL_WINDOWEVENT_ENTER} -> pure GainedMouseFocus
      #{const SDL_WINDOWEVENT_LEAVE} -> pure LostMouseFocus
      #{const SDL_WINDOWEVENT_FOCUS_GAINED} -> pure GainedKeyboardFocus
      #{const SDL_WINDOWEVENT_FOCUS_LOST} -> pure LostKeyboardFocus
      #{const SDL_WINDOWEVENT_CLOSE} -> pure Closing
      unknown -> error $ "Unknown SDL_WINDOWEVENT: " ++ show unknown

    isKeyboard = (`elem` [ #{const SDL_KEYUP}, #{ const SDL_KEYDOWN } ])
    isWindow = (== #{const SDL_WINDOWEVENT})
    isTextInput = (== #{const SDL_TEXTINPUT})
    isTextEditing = (== #{const SDL_TEXTEDITING})
    isMouseMotion = (== #{const SDL_MOUSEMOTION})
    isMouseButton = (`elem` [#{const SDL_MOUSEBUTTONDOWN}, #{const SDL_MOUSEBUTTONUP}])
    isMouseWheel = (== #{const SDL_MOUSEWHEEL})
    isJoyAxis = (== #{const SDL_JOYAXISMOTION})
    isJoyBall = (== #{const SDL_JOYBALLMOTION})
    isJoyHat = (== #{const SDL_JOYHATMOTION})
    isJoyButton = (`elem` [#{const SDL_JOYBUTTONDOWN}, #{const SDL_JOYBUTTONUP}])
    isJoyDevice = (`elem` [#{const SDL_JOYDEVICEADDED}, #{const SDL_JOYDEVICEREMOVED}])
    isControllerAxis = (== #{const SDL_CONTROLLERAXISMOTION})
    isControllerButton = (`elem` [#{const SDL_CONTROLLERBUTTONDOWN}, #{const SDL_CONTROLLERBUTTONUP}])
    isTouchFinger = (`elem` [ #{const SDL_FINGERMOTION}, #{const SDL_FINGERDOWN}, #{const SDL_FINGERUP}])
    isMultiGesture = (== #{const SDL_MULTIGESTURE})
    isDollarGesture = (== #{const SDL_DOLLARGESTURE})
    isDrop = (== #{const SDL_DROPFILE})
    isQuit = (== #{const SDL_QUIT})

    isTerminating            = (== #{const SDL_APP_TERMINATING})
    isLowMemory              = (== #{const SDL_APP_LOWMEMORY})
    isAppWillEnterBackground = (== #{const SDL_APP_WILLENTERBACKGROUND})
    isAppDidEnterBackground  = (== #{const SDL_APP_DIDENTERBACKGROUND})
    isAppWillEnterForeground = (== #{const SDL_APP_WILLENTERFOREGROUND})
    isAppDidEnterForeground  = (== #{const SDL_APP_DIDENTERFOREGROUND})

    isAudioDeviceAdded       = (== #{const SDL_AUDIODEVICEADDED})
    isAudioDeviceRemoved     = (== #{const SDL_AUDIODEVICEREMOVED})
    isClipboardUpdate    = (== #{const SDL_CLIPBOARDUPDATE})
    isRenderTargetsReset = (== #{const SDL_RENDER_TARGETS_RESET})
    isUserEvent          = (== #{const SDL_USEREVENT})

    uint8Bool :: Word8 -> Bool
    uint8Bool = (== 0)

    mkButton :: Word8 -> MouseButton
    mkButton = toEnum . fromIntegral

sdlEventType :: EventData -> Word32
sdlEventType (Keyboard KeyUp _ _ _) = #{const SDL_KEYUP}
sdlEventType (Keyboard KeyDown _ _ _) = #{const SDL_KEYDOWN}
sdlEventType _ = error "sdlEventType: unhandled event data"

sdlKeyState :: KeyMovement -> Word8
sdlKeyState KeyUp = #{const SDL_RELEASED}
sdlKeyState KeyDown = #{const SDL_PRESSED}

foreign import ccall "SDL_PollEvent" sdlPollEvent :: Ptr Event -> IO Int

-- | Polls for currently pending events.
pollEvent :: IO (Maybe Event)
pollEvent = alloca $ \ptr -> do
  ret <- sdlPollEvent ptr
  case ret of
    0 -> return Nothing
    _ -> maybePeek peek ptr

foreign import ccall "SDL_WaitEvent"
  sdlWaitEvent :: Ptr Event -> IO Int

-- | waits indefinitely for next available event
waitEvent :: IO (Maybe Event)
waitEvent = alloca $ \ptr -> do
  ret <- sdlWaitEvent ptr
  case ret of
    0 -> return Nothing
    _ -> maybePeek peek ptr

foreign import ccall "wrapper"
  mkEventFilter :: (Ptr () -> Ptr Event -> IO ()) -> IO (FunPtr (Ptr () -> Ptr Event -> IO ()))

foreign import ccall "SDL_AddEventWatch"
  sdlAddEventWatch :: FunPtr (Ptr () -> Ptr Event -> IO ()) -> Ptr () -> IO ()

-- TODO Adding an event watch seems to stop ^C terminating the program
addEventWatch :: (Event -> IO a) -> IO ()
addEventWatch callback = do
  cb <- mkEventFilter $ \_ -> peek >=> void . callback
  sdlAddEventWatch cb nullPtr

-- Uint32 SDL_GetMouseState(int *x, int *y);
foreign import ccall "SDL_GetMouseState" sdlGetMouseState :: Ptr CInt -> Ptr CInt -> IO Word32
foreign import ccall "SDL_GetRelativeMouseState" sdlGetRelativeMouseState :: Ptr CInt -> Ptr CInt -> IO Word32

mousePressed :: Word32 -> MouseButton -> Bool
mousePressed mask b = mask .&. (mouseButtonToMask b) /= 0

-- | Retrieves the current state of the mouse. Returns (X position, Y position, pressed buttons).
getMouseState :: IO (Int, Int, [MouseButton])
getMouseState = mouseStateGetter sdlGetMouseState

-- | Retrieve the current state of the mouse. Like 'getMouseState' except that X and Y are
--   set to the change since last call to getRelativeMouseState.
getRelativeMouseState :: IO (Int, Int, [MouseButton])
getRelativeMouseState = mouseStateGetter sdlGetRelativeMouseState

mouseStateGetter :: (Ptr CInt -> Ptr CInt -> IO Word32) -> IO  (Int, Int, [MouseButton])
mouseStateGetter getter
  = alloca $ \xPtr ->
    alloca $ \yPtr ->

    do ret <- getter xPtr yPtr
       [x, y] <- mapM peek [xPtr, yPtr]
       return (fromIntegral x, fromIntegral y, mouseStateToButtons ret)

mouseStateToButtons :: Word32 -> [MouseButton]
mouseStateToButtons s = filter (mousePressed s) allButtons
 where allButtons = [LeftButton, MiddleButton, RightButton, MouseX1, MouseX2]

peekFinger :: Ptr Finger -> IO Finger
peekFinger ptr = do
      Finger
  <$> #{peek SDL_Finger, id} ptr
  <*> #{peek SDL_Finger, x}  ptr
  <*> #{peek SDL_Finger, y}  ptr
  <*> #{peek SDL_Finger, pressure} ptr

type TouchId = CLong

foreign import ccall "SDL_GetTouchFinger" sdlGetTouchFinger :: TouchId -> CInt -> IO (Ptr Finger)
foreign import ccall "SDL_GetTouchDevice" sdlGetTouchDevice :: CInt -> IO TouchId
foreign import ccall "SDL_GetNumTouchFingers" sdlGetNumTouchFingers :: TouchId -> IO CInt

--
-- FIXME: sseefried: How do I avoid getting the touch device index each time
--
getTouchFingers :: IO [Finger]
getTouchFingers  = do
  touchDevIndex <- sdlGetTouchDevice 0
  n <- sdlGetNumTouchFingers touchDevIndex
  catMaybes <$> mapM (getTouchFinger touchDevIndex) [0..n-1]
  where
    getTouchFinger :: TouchId -> CInt -> IO (Maybe Finger)
    getTouchFinger touchDevIndex fid = do
      ptr <- sdlGetTouchFinger touchDevIndex fid
      if ptr == nullPtr
       then return Nothing
       else Just <$> peekFinger ptr

-- FIXME: sseefried: How do I avoid getting the touch device index each time
getNumTouchFingers :: IO Int
getNumTouchFingers = do
  touchDevIndex <- sdlGetTouchDevice 0
  fromIntegral <$> sdlGetNumTouchFingers touchDevIndex
