{-# LINE 1 "Graphics/UI/SDL/Init.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Init.hsc" #-}
module Graphics.UI.SDL.Init
    ( -- * Haskell Wrappers
      withInit

      -- * SDL Routines
    , InitFlag(..)
    , init
    , initSubSystem
    , quit
    , quitSubSystem
    , wasInit
    ) where

import Prelude hiding (init)

import Control.Applicative
import Control.Exception (bracket_)
import Data.Int
import Data.Word
import Graphics.UI.SDL.Utilities (fatalSDLBool, toBitmask, fromBitmask)

--------------------------------------------------------------------------------
withInit :: [InitFlag] -> IO a -> IO a
withInit flags = bracket_ (init flags) quit

--------------------------------------------------------------------------------
data InitFlag
   = InitTimer
   | InitAudio
   | InitVideo
   | InitJoystick
   | InitHaptic
   | InitGameController
   | InitEvents
   | InitEverything
   | InitNoParachute
   deriving (Eq, Ord, Show, Read, Bounded, Enum)

initFlagToC :: InitFlag -> Word32
{-# LINE 41 "Graphics/UI/SDL/Init.hsc" #-}
initFlagToC InitTimer          = 1
{-# LINE 42 "Graphics/UI/SDL/Init.hsc" #-}
initFlagToC InitAudio          = 16
{-# LINE 43 "Graphics/UI/SDL/Init.hsc" #-}
initFlagToC InitVideo          = 32
{-# LINE 44 "Graphics/UI/SDL/Init.hsc" #-}
initFlagToC InitJoystick       = 512
{-# LINE 45 "Graphics/UI/SDL/Init.hsc" #-}
initFlagToC InitHaptic         = 4096
{-# LINE 46 "Graphics/UI/SDL/Init.hsc" #-}
initFlagToC InitGameController = 8192
{-# LINE 47 "Graphics/UI/SDL/Init.hsc" #-}
initFlagToC InitEvents         = 16384
{-# LINE 48 "Graphics/UI/SDL/Init.hsc" #-}
initFlagToC InitEverything     = 29233
{-# LINE 49 "Graphics/UI/SDL/Init.hsc" #-}
initFlagToC InitNoParachute    = 1048576
{-# LINE 50 "Graphics/UI/SDL/Init.hsc" #-}

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_Init"
  sdlInit :: Word32 -> IO Int32
{-# LINE 54 "Graphics/UI/SDL/Init.hsc" #-}

init :: [InitFlag] -> IO ()
init = fatalSDLBool "SDL_Init" .
  sdlInit . toBitmask initFlagToC

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_Quit"
  quit :: IO ()

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_InitSubSystem"
  sdlInitSubSystem :: Word32 -> IO Int32
{-# LINE 66 "Graphics/UI/SDL/Init.hsc" #-}

initSubSystem :: [InitFlag] -> IO ()
initSubSystem = fatalSDLBool "SDL_InitSubSystem" .
  sdlInitSubSystem . toBitmask initFlagToC

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_QuitSubSystem"
  sdlQuitSubSystem :: Word32 -> IO ()
{-# LINE 74 "Graphics/UI/SDL/Init.hsc" #-}

quitSubSystem :: [InitFlag] -> IO ()
quitSubSystem = sdlQuitSubSystem . toBitmask initFlagToC

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_WasInit"
  sdlWasInit :: Word32 -> IO Word32
{-# LINE 81 "Graphics/UI/SDL/Init.hsc" #-}

wasInit :: [InitFlag] -> IO [InitFlag]
wasInit flags = 
  fromBitmask initFlagToC <$> sdlWasInit (toBitmask initFlagToC flags)
