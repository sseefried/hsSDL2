{-# LINE 1 "Graphics/UI/SDL/Version.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Version.hsc" #-}
module Graphics.UI.SDL.Version
    ( compiledVersion
    , getRevision
    , getRevisionNumber
    , getVersion
    ) where


{-# LINE 10 "Graphics/UI/SDL/Version.hsc" #-}

import Control.Applicative
import Data.Int
import Data.Version (Version(Version))
import Foreign (Word8, Ptr , Storable (..), alloca)
import Foreign.C.String

--------------------------------------------------------------------------------
data SDLVersion = SDLVersion Word8 Word8 Word8
{-# LINE 19 "Graphics/UI/SDL/Version.hsc" #-}
  deriving (Eq, Show)

instance Storable SDLVersion where
  sizeOf = const (3)
{-# LINE 23 "Graphics/UI/SDL/Version.hsc" #-}

  alignment = const 1
{-# LINE 25 "Graphics/UI/SDL/Version.hsc" #-}

  peek ptr = SDLVersion
    <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 28 "Graphics/UI/SDL/Version.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 1) ptr
{-# LINE 29 "Graphics/UI/SDL/Version.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr
{-# LINE 30 "Graphics/UI/SDL/Version.hsc" #-}

  poke ptr (SDLVersion major minor patch) = do
     (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr major
{-# LINE 33 "Graphics/UI/SDL/Version.hsc" #-}
     (\hsc_ptr -> pokeByteOff hsc_ptr 1) ptr minor
{-# LINE 34 "Graphics/UI/SDL/Version.hsc" #-}
     (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr patch
{-# LINE 35 "Graphics/UI/SDL/Version.hsc" #-}

--------------------------------------------------------------------------------
-- | The SDL version your program was compiled against.
compiledVersion :: Version
compiledVersion = Version
    [ 2
{-# LINE 41 "Graphics/UI/SDL/Version.hsc" #-}
    , 0
{-# LINE 42 "Graphics/UI/SDL/Version.hsc" #-}
    , 3
{-# LINE 43 "Graphics/UI/SDL/Version.hsc" #-}
    ] []

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetVersion"
  sdlGetVersion :: Ptr SDLVersion -> IO ()

-- | The version of SDL that is linked against your program.
getVersion :: IO Version
getVersion = alloca $ \versionPtr -> do
  sdlGetVersion versionPtr
  SDLVersion major minor patch <- peek versionPtr
  return (Version (map fromIntegral [major,minor,patch]) [])

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetRevision"
  sdlGetRevision :: IO CString

-- | The revision of SDL your program was linked against
--
-- This returns a hash uniquely identifying the exact revision of SDL in
-- use, not an incrementing number. This is only useful in comparing
-- against other revisions.
getRevision :: IO String
getRevision = sdlGetRevision >>= peekCString

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetRevisionNumber"
  sdlGetRevisionNumber :: IO Int32
{-# LINE 71 "Graphics/UI/SDL/Version.hsc" #-}

-- | The revision number of SDL your program was linked against
--
-- This returns a number uniquely identifying the exact revision of SDL in
-- use. This is an incrementing number based on commits to hg.libsdl.org.
getRevisionNumber :: IO Int32
{-# LINE 77 "Graphics/UI/SDL/Version.hsc" #-}
getRevisionNumber = sdlGetRevisionNumber
