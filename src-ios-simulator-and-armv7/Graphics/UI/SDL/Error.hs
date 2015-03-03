{-# LINE 1 "Graphics/UI/SDL/Error.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Error.hsc" #-}
module Graphics.UI.SDL.Error
    ( -- * SDL Routines
      clearError
    , getError
    , setError
    )  where

import Control.Applicative
import Control.Monad (mfilter, void)
import Foreign hiding (void)
import Foreign.C
import Graphics.UI.SDL.StringUtilities (escapePrintf)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_ClearError"
  clearError :: IO ()
  
--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetError"
  sdlGetError :: IO CString
  
getError :: IO (Maybe String)
getError = mfilter (not . null) <$> (sdlGetError >>= maybePeek peekCString)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_SetError"
  sdlSetError :: CString -> IO Int32
{-# LINE 29 "Graphics/UI/SDL/Error.hsc" #-}

setError :: String -> IO ()
setError errorMessage = void $ withCString (escapePrintf errorMessage) sdlSetError
