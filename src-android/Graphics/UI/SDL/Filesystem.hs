{-# LINE 1 "Graphics/UI/SDL/Filesystem.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Filesystem.hsc" #-}
module Graphics.UI.SDL.Filesystem
  ( getBasePath
  , getPrefPath
  ) where

import Foreign.C.String

foreign import ccall unsafe "SDL_GetBasePath"
  sdlGetBasePath :: IO CString

getBasePath :: IO String
getBasePath = sdlGetBasePath >>= peekCString

foreign import ccall unsafe "SDL_GetPrefPath"
  sdlGetPrefPath:: CString -> CString -> IO CString

getPrefPath :: String -> String -> IO String
getPrefPath org app =
  withCString org $ \org' ->
  withCString app $ \app' ->
    sdlGetPrefPath org' app' >>= peekCString 
  
