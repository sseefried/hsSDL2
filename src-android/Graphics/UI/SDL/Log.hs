{-# LINE 1 "Graphics/UI/SDL/Log.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Log.hsc" #-}
module Graphics.UI.SDL.Log
    ( -- * SDL Routines
      log
    , logCritical
    , logDebug
    , logError
    , logGetOutputFunction
    , logGetPriority
    , logInfo
    , logMessage
    , logResetPriorities
    , logSetAllPriority
    , logSetOutputFunction
    , logSetPriority
    , logVerbose
    , logWarn
    , LogCategory (..)
    ) where

import Prelude hiding (log)

import Control.Applicative
import Data.Int
import Foreign
import Foreign.C
import Graphics.UI.SDL.StringUtilities (escapePrintf)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_Log"
  sdlLog :: CString -> IO ()

log :: String -> IO ()
log message = withCString (escapePrintf message) sdlLog

--------------------------------------------------------------------------------
data LogCategory
  = LogCategoryApplication
  | LogCategoryError
  | LogCategorySystem
  | LogCategoryAudio
  | LogCategoryVideo
  | LogCategoryRender
  | LogCategoryInput
  | LogCategoryCustom
  deriving (Bounded, Enum, Eq, Read, Show)

logCategoryToInt :: LogCategory -> Int32
{-# LINE 49 "Graphics/UI/SDL/Log.hsc" #-}
logCategoryToInt c = case c of
  LogCategoryApplication -> 0
{-# LINE 51 "Graphics/UI/SDL/Log.hsc" #-}
  LogCategoryError -> 1
{-# LINE 52 "Graphics/UI/SDL/Log.hsc" #-}
  LogCategorySystem -> 3
{-# LINE 53 "Graphics/UI/SDL/Log.hsc" #-}
  LogCategoryAudio -> 4
{-# LINE 54 "Graphics/UI/SDL/Log.hsc" #-}
  LogCategoryVideo -> 5
{-# LINE 55 "Graphics/UI/SDL/Log.hsc" #-}
  LogCategoryRender -> 6
{-# LINE 56 "Graphics/UI/SDL/Log.hsc" #-}
  LogCategoryInput -> 7
{-# LINE 57 "Graphics/UI/SDL/Log.hsc" #-}
  LogCategoryCustom -> 19
{-# LINE 58 "Graphics/UI/SDL/Log.hsc" #-}

logCategoryFromInt :: Int32 -> LogCategory
{-# LINE 60 "Graphics/UI/SDL/Log.hsc" #-}
logCategoryFromInt c = case c of
  0 -> LogCategoryApplication
{-# LINE 62 "Graphics/UI/SDL/Log.hsc" #-}
  1 -> LogCategoryError
{-# LINE 63 "Graphics/UI/SDL/Log.hsc" #-}
  3 -> LogCategorySystem
{-# LINE 64 "Graphics/UI/SDL/Log.hsc" #-}
  4 -> LogCategoryAudio
{-# LINE 65 "Graphics/UI/SDL/Log.hsc" #-}
  5 -> LogCategoryVideo
{-# LINE 66 "Graphics/UI/SDL/Log.hsc" #-}
  6 -> LogCategoryRender
{-# LINE 67 "Graphics/UI/SDL/Log.hsc" #-}
  7 -> LogCategoryInput
{-# LINE 68 "Graphics/UI/SDL/Log.hsc" #-}
  19 -> LogCategoryCustom
{-# LINE 69 "Graphics/UI/SDL/Log.hsc" #-}
  i -> error $ "Unexpected SDL_LogCategory: " ++ show i

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogCritical"
  sdlLogCritical :: Int32 -> CString -> IO ()
{-# LINE 74 "Graphics/UI/SDL/Log.hsc" #-}

logCritical :: LogCategory -> String -> IO ()
logCritical category message = withCString (escapePrintf message) $
  sdlLogCritical (logCategoryToInt category)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogDebug"
  sdlLogDebug :: Int32 -> CString -> IO ()
{-# LINE 82 "Graphics/UI/SDL/Log.hsc" #-}

logDebug :: LogCategory -> String -> IO ()
logDebug category message = withCString (escapePrintf message) $
  sdlLogDebug (logCategoryToInt category)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogError"
  sdlLogError :: Int32 -> CString -> IO ()
{-# LINE 90 "Graphics/UI/SDL/Log.hsc" #-}

logError :: LogCategory -> String -> IO ()
logError category message = withCString (escapePrintf message) $
  sdlLogError (logCategoryToInt category)

--------------------------------------------------------------------------------
data LogPriority
  = LogPriorityVerbose
  | LogPriorityDebug
  | LogPriorityInfo
  | LogPriorityWarn
  | LogPriorityError
  | LogPriorityCritical
  deriving (Bounded, Enum, Eq, Read, Show)
  
logPriorityToInt :: LogPriority -> Word32
{-# LINE 106 "Graphics/UI/SDL/Log.hsc" #-}
logPriorityToInt p = case p of
  LogPriorityVerbose -> 1
{-# LINE 108 "Graphics/UI/SDL/Log.hsc" #-}
  LogPriorityDebug -> 2
{-# LINE 109 "Graphics/UI/SDL/Log.hsc" #-}
  LogPriorityInfo -> 3
{-# LINE 110 "Graphics/UI/SDL/Log.hsc" #-}
  LogPriorityWarn -> 4
{-# LINE 111 "Graphics/UI/SDL/Log.hsc" #-}
  LogPriorityError -> 5
{-# LINE 112 "Graphics/UI/SDL/Log.hsc" #-}
  LogPriorityCritical -> 6
{-# LINE 113 "Graphics/UI/SDL/Log.hsc" #-}

logPriorityFromInt :: Word32 -> LogPriority
{-# LINE 115 "Graphics/UI/SDL/Log.hsc" #-}
logPriorityFromInt p = case p of
  1 -> LogPriorityVerbose
{-# LINE 117 "Graphics/UI/SDL/Log.hsc" #-}
  2 -> LogPriorityDebug
{-# LINE 118 "Graphics/UI/SDL/Log.hsc" #-}
  3 -> LogPriorityInfo
{-# LINE 119 "Graphics/UI/SDL/Log.hsc" #-}
  4 -> LogPriorityWarn
{-# LINE 120 "Graphics/UI/SDL/Log.hsc" #-}
  5 -> LogPriorityError
{-# LINE 121 "Graphics/UI/SDL/Log.hsc" #-}
  6 -> LogPriorityCritical
{-# LINE 122 "Graphics/UI/SDL/Log.hsc" #-}
  i -> error $ "Unexpected SDL_LogPriority: " ++ show i

--------------------------------------------------------------------------------
type SDLOutputFunction = Ptr () -> Int32 -> Word32 -> CString -> IO ()
{-# LINE 126 "Graphics/UI/SDL/Log.hsc" #-}

foreign import ccall unsafe "SDL_LogGetOutputFunction"
  sdlLogGetOutputFunction :: Ptr (FunPtr SDLOutputFunction) -> Ptr (Ptr ()) -> IO ()

foreign import ccall "dynamic"
  mkLogOutputFunction :: FunPtr SDLOutputFunction -> SDLOutputFunction

type OutputFunction = LogCategory -> LogPriority -> String -> IO ()

logGetOutputFunction :: IO OutputFunction
logGetOutputFunction =
  alloca $ \ofPtrLoc ->
  alloca $ \userDataPtr -> do
    sdlLogGetOutputFunction ofPtrLoc userDataPtr
    f <- mkLogOutputFunction <$> peek ofPtrLoc
    userData <- peek userDataPtr
    return $ \category priority message ->
      withCString message $ f userData (logCategoryToInt category) (logPriorityToInt priority)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogGetPriority"
  sdlLogGetPriority :: Int32 -> IO Word32
{-# LINE 148 "Graphics/UI/SDL/Log.hsc" #-}

logGetPriority :: LogCategory -> IO LogPriority
logGetPriority = fmap logPriorityFromInt . sdlLogGetPriority . logCategoryToInt

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogInfo"
  sdlLogInfo :: Int32 -> CString -> IO ()
{-# LINE 155 "Graphics/UI/SDL/Log.hsc" #-}

logInfo :: LogCategory -> String -> IO ()
logInfo category message = withCString (escapePrintf message) $
  sdlLogInfo (logCategoryToInt category)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogMessage"
  sdlLogMessage :: Int32 -> Word32 -> CString -> IO ()
{-# LINE 163 "Graphics/UI/SDL/Log.hsc" #-}

logMessage :: LogCategory -> LogPriority -> String -> IO ()
logMessage category priority message = withCString (escapePrintf message) $
  sdlLogMessage (logCategoryToInt category) (logPriorityToInt priority)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogResetPriorities"
  logResetPriorities :: IO ()
  
--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogSetAllPriority"
  sdlLogSetAllPriority :: Word32 -> IO ()
{-# LINE 175 "Graphics/UI/SDL/Log.hsc" #-}

logSetAllPriority :: LogPriority -> IO ()
logSetAllPriority = sdlLogSetAllPriority . logPriorityToInt

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogSetOutputFunction"
  sdlLogSetOutputFunction :: FunPtr (SDLOutputFunction) -> Ptr () -> IO ()
  
foreign import ccall "wrapper"
   mkSDLLogOutputFunction :: SDLOutputFunction -> IO (FunPtr SDLOutputFunction)
   
logSetOutputFunction :: OutputFunction -> IO ()
logSetOutputFunction f = do
  f' <- mkSDLLogOutputFunction $ \_ c p m ->
          peekCString m >>= f (logCategoryFromInt c) (logPriorityFromInt p)
  sdlLogSetOutputFunction f' nullPtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogSetPriority"
  sdlLogSetPriority :: Int32 -> Word32 -> IO ()
{-# LINE 195 "Graphics/UI/SDL/Log.hsc" #-}

logSetPriority :: LogCategory -> LogPriority -> IO ()
logSetPriority c p = sdlLogSetPriority (logCategoryToInt c) (logPriorityToInt p)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogVerbose"
  sdlLogVerbose :: Int32 -> CString -> IO ()
{-# LINE 202 "Graphics/UI/SDL/Log.hsc" #-}

logVerbose :: LogCategory -> String -> IO ()
logVerbose category message = withCString (escapePrintf message) $
  sdlLogVerbose (logCategoryToInt category)
  
--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogWarn"
  sdlLogWarn :: Int32 -> CString -> IO ()
{-# LINE 210 "Graphics/UI/SDL/Log.hsc" #-}

logWarn :: LogCategory -> String -> IO ()
logWarn category message = withCString (escapePrintf message) $
  sdlLogWarn (logCategoryToInt category)
