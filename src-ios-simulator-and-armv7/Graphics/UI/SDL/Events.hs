{-# LINE 1 "Graphics/UI/SDL/Events.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Events.hsc" #-}
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
import Data.Word
import Data.Int
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
  toEnum 1 = LeftButton
{-# LINE 41 "Graphics/UI/SDL/Events.hsc" #-}
  toEnum 2 = MiddleButton
{-# LINE 42 "Graphics/UI/SDL/Events.hsc" #-}
  toEnum 3 = RightButton
{-# LINE 43 "Graphics/UI/SDL/Events.hsc" #-}
  toEnum 4 = MouseX1
{-# LINE 44 "Graphics/UI/SDL/Events.hsc" #-}
  toEnum 5 = MouseX2
{-# LINE 45 "Graphics/UI/SDL/Events.hsc" #-}
  toEnum k = UnknownButton (fromIntegral k)

  fromEnum LeftButton = 1
{-# LINE 48 "Graphics/UI/SDL/Events.hsc" #-}
  fromEnum MiddleButton = 2
{-# LINE 49 "Graphics/UI/SDL/Events.hsc" #-}
  fromEnum RightButton = 3
{-# LINE 50 "Graphics/UI/SDL/Events.hsc" #-}
  fromEnum MouseX1 = 4
{-# LINE 51 "Graphics/UI/SDL/Events.hsc" #-}
  fromEnum MouseX2 = 5
{-# LINE 52 "Graphics/UI/SDL/Events.hsc" #-}
  fromEnum (UnknownButton k) = fromIntegral k

mouseButtonToMask :: MouseButton -> Word32
mouseButtonToMask b = case b of
  LeftButton   -> 1
{-# LINE 57 "Graphics/UI/SDL/Events.hsc" #-}
  MiddleButton -> 2
{-# LINE 58 "Graphics/UI/SDL/Events.hsc" #-}
  RightButton  -> 4
{-# LINE 59 "Graphics/UI/SDL/Events.hsc" #-}
  MouseX1      -> 8
{-# LINE 60 "Graphics/UI/SDL/Events.hsc" #-}
  MouseX2      -> 16
{-# LINE 61 "Graphics/UI/SDL/Events.hsc" #-}
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
  sizeOf = const (56)
{-# LINE 153 "Graphics/UI/SDL/Events.hsc" #-}

  alignment = const 4

  poke ptr (Event timestamp body) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (sdlEventType body)
{-# LINE 158 "Graphics/UI/SDL/Events.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr timestamp
{-# LINE 159 "Graphics/UI/SDL/Events.hsc" #-}

    case body of
      Keyboard m w r s -> do
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr w
{-# LINE 163 "Graphics/UI/SDL/Events.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr (sdlKeyState m)
{-# LINE 164 "Graphics/UI/SDL/Events.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 13) ptr
{-# LINE 165 "Graphics/UI/SDL/Events.hsc" #-}
          (if r then 1 else 0 :: Word8)
        (\hsc_ptr -> pokeByteOff hsc_ptr 14) ptr padding8
{-# LINE 167 "Graphics/UI/SDL/Events.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 15) ptr padding8
{-# LINE 168 "Graphics/UI/SDL/Events.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr s
{-# LINE 169 "Graphics/UI/SDL/Events.hsc" #-}
      _ -> error "poke: unhandled event type"

   where padding8 = 0 :: Word8

  peek ptr = do
    evType <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 175 "Graphics/UI/SDL/Events.hsc" #-}
    Event <$> (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr <*> peekEvent evType
{-# LINE 176 "Graphics/UI/SDL/Events.hsc" #-}

   where

    peekEvent :: Word32 -> IO EventData
    peekEvent e
      | isKeyboard e =
          Keyboard <$> case e of
                        768 -> pure KeyDown
{-# LINE 184 "Graphics/UI/SDL/Events.hsc" #-}
                        769 -> pure KeyUp
{-# LINE 185 "Graphics/UI/SDL/Events.hsc" #-}
                        _ -> error "Unknown key movement when parsing SDL_KeybordEvent"
                   <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 187 "Graphics/UI/SDL/Events.hsc" #-}
                   <*> (uint8Bool <$> (\hsc_ptr -> peekByteOff hsc_ptr 13) ptr)
{-# LINE 188 "Graphics/UI/SDL/Events.hsc" #-}
                   <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 189 "Graphics/UI/SDL/Events.hsc" #-}

      | isWindow e =
          Window <$> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 192 "Graphics/UI/SDL/Events.hsc" #-}
                 <*> ((\hsc_ptr -> peekByteOff hsc_ptr 12) ptr >>= peekWindowEvent)
{-# LINE 193 "Graphics/UI/SDL/Events.hsc" #-}

      | isTextInput e =
          TextInput <$> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 196 "Graphics/UI/SDL/Events.hsc" #-}
                    <*> peekCString (ptr `plusPtr` (12))
{-# LINE 197 "Graphics/UI/SDL/Events.hsc" #-}

      | isTextEditing e = pure TextEditing -- TODO

      | isMouseMotion e =
          MouseMotion <$> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 202 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 203 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (mouseStateToButtons <$> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr)
{-# LINE 204 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (mkPosition <$> (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 205 "Graphics/UI/SDL/Events.hsc" #-}
                                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr)
{-# LINE 206 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 207 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 208 "Graphics/UI/SDL/Events.hsc" #-}

      | isMouseButton e = do
          btnState <- (\hsc_ptr -> peekByteOff hsc_ptr 17) ptr :: IO Word8
{-# LINE 211 "Graphics/UI/SDL/Events.hsc" #-}
          MouseButton <$> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 212 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 213 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (mkButton <$> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr)
{-# LINE 214 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> return (case btnState of
                                    1 -> Pressed
{-# LINE 216 "Graphics/UI/SDL/Events.hsc" #-}
                                    0 -> Released
{-# LINE 217 "Graphics/UI/SDL/Events.hsc" #-}
                                    _ -> error "isMouseButton: unhandled mouse button state")
                      <*> (mkPosition <$> (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 219 "Graphics/UI/SDL/Events.hsc" #-}
                                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr)
{-# LINE 220 "Graphics/UI/SDL/Events.hsc" #-}

      | isMouseWheel e =
          MouseWheel <$> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 223 "Graphics/UI/SDL/Events.hsc" #-}
                     <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 224 "Graphics/UI/SDL/Events.hsc" #-}
                     <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 225 "Graphics/UI/SDL/Events.hsc" #-}
                     <*> (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 226 "Graphics/UI/SDL/Events.hsc" #-}

      | isJoyAxis e = pure JoyAxis
      | isJoyBall e = pure JoyBall
      | isJoyHat e = pure JoyHat
      | isJoyButton e = pure JoyButton
      | isJoyDevice e = pure JoyDevice
      | isControllerAxis e = pure ControllerAxis
      | isControllerButton e = pure ControllerButton
      | isTouchFinger e =
          TouchFinger <$> case e of
                        1794 -> pure TouchFingerMotion
{-# LINE 237 "Graphics/UI/SDL/Events.hsc" #-}
                        1792 -> pure TouchFingerDown
{-# LINE 238 "Graphics/UI/SDL/Events.hsc" #-}
                        1793 -> pure TouchFingerUp
{-# LINE 239 "Graphics/UI/SDL/Events.hsc" #-}
                        _ -> error "isTouchFinger: unhandled finger constant"
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 241 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 242 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 243 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 244 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 245 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 246 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 36) ptr
{-# LINE 247 "Graphics/UI/SDL/Events.hsc" #-}
                      <*> (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 248 "Graphics/UI/SDL/Events.hsc" #-}

      | isMultiGesture e           = pure MultiGesture
      | isDollarGesture e          = pure DollarGesture
      | isTerminating e            = pure Terminating
      | isLowMemory e              = pure LowMemory
      | isAppWillEnterBackground e = pure AppWillEnterBackground
      | isAppDidEnterBackground e  = pure  AppDidEnterBackground
      | isAppWillEnterForeground e = pure AppWillEnterForeground
      | isAppDidEnterForeground e  = pure AppDidEnterForeground
      | isClipboardUpdate e        = pure ClipboardUpdate
      | isRenderTargetsReset e     = pure RenderTargetsReset
      | isUserEvent          e     = pure UserEvent
      | isDrop e                   = pure Drop
      | isQuit e                   = pure Quit
      | otherwise = error $ "Unknown event type: " ++ show e

    peekWindowEvent :: Word8 -> IO WindowEvent
    peekWindowEvent e = case e of
      1 -> pure Shown
{-# LINE 267 "Graphics/UI/SDL/Events.hsc" #-}
      2 -> pure Hidden
{-# LINE 268 "Graphics/UI/SDL/Events.hsc" #-}
      3 -> pure Exposed
{-# LINE 269 "Graphics/UI/SDL/Events.hsc" #-}
      4 ->
{-# LINE 270 "Graphics/UI/SDL/Events.hsc" #-}
        Moved <$> (mkPosition <$> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 271 "Graphics/UI/SDL/Events.hsc" #-}
                              <*> (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr)
{-# LINE 272 "Graphics/UI/SDL/Events.hsc" #-}
      5 ->
{-# LINE 273 "Graphics/UI/SDL/Events.hsc" #-}
        Resized <$> (mkSize <$> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 274 "Graphics/UI/SDL/Events.hsc" #-}
                            <*> (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr)
{-# LINE 275 "Graphics/UI/SDL/Events.hsc" #-}
      6 -> pure SizeChanged
{-# LINE 276 "Graphics/UI/SDL/Events.hsc" #-}
      7 -> pure Minimized
{-# LINE 277 "Graphics/UI/SDL/Events.hsc" #-}
      8 -> pure Maximized
{-# LINE 278 "Graphics/UI/SDL/Events.hsc" #-}
      9 -> pure Restored
{-# LINE 279 "Graphics/UI/SDL/Events.hsc" #-}
      10 -> pure GainedMouseFocus
{-# LINE 280 "Graphics/UI/SDL/Events.hsc" #-}
      11 -> pure LostMouseFocus
{-# LINE 281 "Graphics/UI/SDL/Events.hsc" #-}
      12 -> pure GainedKeyboardFocus
{-# LINE 282 "Graphics/UI/SDL/Events.hsc" #-}
      13 -> pure LostKeyboardFocus
{-# LINE 283 "Graphics/UI/SDL/Events.hsc" #-}
      14 -> pure Closing
{-# LINE 284 "Graphics/UI/SDL/Events.hsc" #-}
      unknown -> error $ "Unknown SDL_WINDOWEVENT: " ++ show unknown

    isKeyboard = (`elem` [ 769, 768 ])
{-# LINE 287 "Graphics/UI/SDL/Events.hsc" #-}
    isWindow = (== 512)
{-# LINE 288 "Graphics/UI/SDL/Events.hsc" #-}
    isTextInput = (== 771)
{-# LINE 289 "Graphics/UI/SDL/Events.hsc" #-}
    isTextEditing = (== 770)
{-# LINE 290 "Graphics/UI/SDL/Events.hsc" #-}
    isMouseMotion = (== 1024)
{-# LINE 291 "Graphics/UI/SDL/Events.hsc" #-}
    isMouseButton = (`elem` [1025, 1026])
{-# LINE 292 "Graphics/UI/SDL/Events.hsc" #-}
    isMouseWheel = (== 1027)
{-# LINE 293 "Graphics/UI/SDL/Events.hsc" #-}
    isJoyAxis = (== 1536)
{-# LINE 294 "Graphics/UI/SDL/Events.hsc" #-}
    isJoyBall = (== 1537)
{-# LINE 295 "Graphics/UI/SDL/Events.hsc" #-}
    isJoyHat = (== 1538)
{-# LINE 296 "Graphics/UI/SDL/Events.hsc" #-}
    isJoyButton = (`elem` [1539, 1540])
{-# LINE 297 "Graphics/UI/SDL/Events.hsc" #-}
    isJoyDevice = (`elem` [1541, 1542])
{-# LINE 298 "Graphics/UI/SDL/Events.hsc" #-}
    isControllerAxis = (== 1616)
{-# LINE 299 "Graphics/UI/SDL/Events.hsc" #-}
    isControllerButton = (`elem` [1617, 1618])
{-# LINE 300 "Graphics/UI/SDL/Events.hsc" #-}
    isTouchFinger = (`elem` [ 1794, 1792, 1793])
{-# LINE 301 "Graphics/UI/SDL/Events.hsc" #-}
    isMultiGesture = (== 2050)
{-# LINE 302 "Graphics/UI/SDL/Events.hsc" #-}
    isDollarGesture = (== 2048)
{-# LINE 303 "Graphics/UI/SDL/Events.hsc" #-}
    isDrop = (== 4096)
{-# LINE 304 "Graphics/UI/SDL/Events.hsc" #-}
    isQuit = (== 256)
{-# LINE 305 "Graphics/UI/SDL/Events.hsc" #-}

    isTerminating            = (== 257)
{-# LINE 307 "Graphics/UI/SDL/Events.hsc" #-}
    isLowMemory              = (== 258)
{-# LINE 308 "Graphics/UI/SDL/Events.hsc" #-}
    isAppWillEnterBackground = (== 259)
{-# LINE 309 "Graphics/UI/SDL/Events.hsc" #-}
    isAppDidEnterBackground  = (== 260)
{-# LINE 310 "Graphics/UI/SDL/Events.hsc" #-}
    isAppWillEnterForeground = (== 261)
{-# LINE 311 "Graphics/UI/SDL/Events.hsc" #-}
    isAppDidEnterForeground  = (== 262)
{-# LINE 312 "Graphics/UI/SDL/Events.hsc" #-}

    isClipboardUpdate    = (== 2304)
{-# LINE 314 "Graphics/UI/SDL/Events.hsc" #-}
    isRenderTargetsReset = (== 8192)
{-# LINE 315 "Graphics/UI/SDL/Events.hsc" #-}
    isUserEvent          = (== 32768)
{-# LINE 316 "Graphics/UI/SDL/Events.hsc" #-}

    uint8Bool :: Word8 -> Bool
    uint8Bool = (== 0)

    mkButton :: Word8 -> MouseButton
    mkButton = toEnum . fromIntegral

sdlEventType :: EventData -> Word32
sdlEventType (Keyboard KeyUp _ _ _) = 769
{-# LINE 325 "Graphics/UI/SDL/Events.hsc" #-}
sdlEventType (Keyboard KeyDown _ _ _) = 768
{-# LINE 326 "Graphics/UI/SDL/Events.hsc" #-}
sdlEventType _ = error "sdlEventType: unhandled event data"

sdlKeyState :: KeyMovement -> Word8
sdlKeyState KeyUp = 0
{-# LINE 330 "Graphics/UI/SDL/Events.hsc" #-}
sdlKeyState KeyDown = 1
{-# LINE 331 "Graphics/UI/SDL/Events.hsc" #-}

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
  <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 398 "Graphics/UI/SDL/Events.hsc" #-}
  <*> (\hsc_ptr -> peekByteOff hsc_ptr 8)  ptr
{-# LINE 399 "Graphics/UI/SDL/Events.hsc" #-}
  <*> (\hsc_ptr -> peekByteOff hsc_ptr 12)  ptr
{-# LINE 400 "Graphics/UI/SDL/Events.hsc" #-}
  <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 401 "Graphics/UI/SDL/Events.hsc" #-}

foreign import ccall "SDL_GetTouchFinger" sdlGetTouchFinger :: CLong -> CInt -> IO (Ptr Finger)

getTouchFinger :: CInt -> IO (Maybe Finger)
getTouchFinger fid = do
   ptr <- sdlGetTouchFinger 0 fid
   if ptr == nullPtr
    then return Nothing
    else Just <$> peekFinger ptr

