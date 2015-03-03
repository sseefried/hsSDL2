{-# LINE 1 "Graphics/UI/SDL/Timer.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Timer.hsc" #-}
module Graphics.UI.SDL.Timer
    ( addTimer
    , removeTimer
    , delay
    , getPerformanceCounter
    , getPerformanceFrequency
    , getTicks
    , ticksPassed
    , TimerCallback
    ) where

import Foreign
import Control.Applicative ((<$>))

import Graphics.UI.SDL.General (failWithError)
import Graphics.UI.SDL.Utilities (sdlBoolToBool)

foreign import ccall "wrapper"
  mkTimerCallback :: (Word32 -> Ptr () -> IO Word32) -> IO (FunPtr (Word32 -> Ptr () -> IO Word32))

newtype TimerCallback = TimerCallback Int32
{-# LINE 23 "Graphics/UI/SDL/Timer.hsc" #-}

foreign import ccall "SDL_AddTimer"
  sdlAddTimer :: Word32 -> FunPtr (Word32 -> Ptr () -> IO Word32) -> Ptr () -> IO Int32
{-# LINE 26 "Graphics/UI/SDL/Timer.hsc" #-}

data TimerTermination = CancelTimer | ContinueTimer

addTimer :: Word32 -> (Word32 -> IO TimerTermination) -> IO TimerCallback
addTimer interval callback = do
  cb <- mkTimerCallback $ \passed _ -> do
    termination <- callback passed
    case termination of
      CancelTimer -> return 0
      ContinueTimer -> return 1
  tId <- sdlAddTimer interval cb nullPtr
  case tId of
    0 -> failWithError "addTimer"
    _ -> return $ TimerCallback tId

foreign import ccall "SDL_RemoveTimer"
  sdlRemoveTimer :: Int32 -> IO Word32
{-# LINE 43 "Graphics/UI/SDL/Timer.hsc" #-}

removeTimer :: TimerCallback -> IO Bool
removeTimer (TimerCallback tId) = do
  sdlBoolToBool <$> sdlRemoveTimer tId

foreign import ccall safe "SDL_Delay"
  delay :: Word32 -> IO ()

foreign import ccall safe "SDL_GetPerformanceCounter"
  getPerformanceCounter :: IO (Word64)
{-# LINE 53 "Graphics/UI/SDL/Timer.hsc" #-}

foreign import ccall safe "SDL_GetPerformanceFrequency"
  getPerformanceFrequency :: IO (Word64)
{-# LINE 56 "Graphics/UI/SDL/Timer.hsc" #-}

foreign import ccall safe "SDL_GetTicks"
  getTicks :: IO (Word32)
{-# LINE 59 "Graphics/UI/SDL/Timer.hsc" #-}

ticksPassed :: Word32 -> Word32 -> Bool
{-# LINE 61 "Graphics/UI/SDL/Timer.hsc" #-}
ticksPassed a b = a - b <= 0

