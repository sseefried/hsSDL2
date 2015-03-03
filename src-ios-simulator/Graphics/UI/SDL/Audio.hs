{-# LINE 1 "Graphics/UI/SDL/Audio.hsc" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LINE 2 "Graphics/UI/SDL/Audio.hsc" #-}

{-# LINE 3 "Graphics/UI/SDL/Audio.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Audio
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Audio
    ( AudioFormat (..)
    , fromAudioFormat
    , toAudioFormat
    
      -- * AudioSpec
    , AudioSpec (..)

      -- * WAV files
    , loadWAV
    
      -- * Audio Devices
    , AudioDevice
    , AudioStatus(..)
    , AudioDeviceUsage (..)
    , getAudioDeviceName
    , getAudioDeviceStatus
    , getAudioDriver
    , getAudioStatus
    , getCurrentAudioDriver
    , getNumAudioDevices
    , getNumAudioDrivers
    , lockAudio
    , lockAudioDevice
    , openAudioDevice 
    , pauseAudioDevice
    , unlockAudio
    , unlockAudioDevice

    , mixAudio
    , mixAudioFormat
    , audioInit
    , audioQuit
    , closeAudio
    , closeAudioDevice
    , openAudio
    , pauseAudio
    ) where

import Control.Applicative
import Control.Monad ((>=>))
import Foreign
import Foreign.C
import Data.ByteString hiding (index)
import Data.Maybe (fromMaybe)
import Data.Vector.Storable (Vector)
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Utilities (fatalSDLNull, fatalSDLBool)

import qualified Data.ByteString.Internal as BI
import qualified Data.Vector.Storable as V
import qualified Graphics.UI.SDL.RWOps as RWOps

data AudioFormat
    = AudioU8
    | AudioS8
    | AudioU16LSB
    | AudioS16LSB
    | AudioU16MSB
    | AudioS16MSB
    | AudioU16Sys
    | AudioS16Sys
      deriving (Show,Eq,Ord,Enum)

fromAudioFormat :: AudioFormat -> Word16
fromAudioFormat AudioU8 = 8
{-# LINE 80 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioS8 = 32776
{-# LINE 81 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioU16LSB = 16
{-# LINE 82 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioS16LSB = 32784
{-# LINE 83 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioU16MSB = 4112
{-# LINE 84 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioS16MSB = 36880
{-# LINE 85 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioU16Sys = 16
{-# LINE 86 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioS16Sys = 32784
{-# LINE 87 "Graphics/UI/SDL/Audio.hsc" #-}

toAudioFormat :: Word16 -> AudioFormat
toAudioFormat 8 = AudioU8
{-# LINE 90 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat 32776 = AudioS8
{-# LINE 91 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat 16 = AudioU16LSB
{-# LINE 92 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat 32784 = AudioS16LSB
{-# LINE 93 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat 4112 = AudioU16MSB
{-# LINE 94 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat 36880 = AudioS16MSB
{-# LINE 95 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat _ = error "Graphics.UI.SDL.Audio.toAudioFormat: bad argument"

data AudioSpec = AudioSpec { audioSpecFreq :: Int32
{-# LINE 98 "Graphics/UI/SDL/Audio.hsc" #-}
                           , audioSpecFormat :: AudioFormat
                           , audioSpecChannels :: Word8
{-# LINE 100 "Graphics/UI/SDL/Audio.hsc" #-}
                           , audioSpecSilence :: Word8
{-# LINE 101 "Graphics/UI/SDL/Audio.hsc" #-}
                           , audioSpecSamples :: Word16
{-# LINE 102 "Graphics/UI/SDL/Audio.hsc" #-}
                           , audioSpecSize :: Word32
{-# LINE 103 "Graphics/UI/SDL/Audio.hsc" #-}
                           , audioSpecCallback :: Maybe (Int32 -> IO (Vector Word8))
{-# LINE 104 "Graphics/UI/SDL/Audio.hsc" #-}
                           }

instance Show AudioSpec where
  show AudioSpec {..} = unlines [ show audioSpecFreq, show audioSpecFormat, show audioSpecChannels, show audioSpecSilence, show audioSpecSamples, show audioSpecSize ]

foreign import ccall "wrapper"
  mkAudioCallback :: (Ptr () -> Ptr (Word8) -> Int32 -> IO ())
{-# LINE 111 "Graphics/UI/SDL/Audio.hsc" #-}
     -> IO (FunPtr (Ptr () -> Ptr (Word8) -> Int32 -> IO ()))
{-# LINE 112 "Graphics/UI/SDL/Audio.hsc" #-}

instance Storable AudioSpec where
  sizeOf = const (24)
{-# LINE 115 "Graphics/UI/SDL/Audio.hsc" #-}
  
  alignment = const 4
  
  poke ptr AudioSpec{..} = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr audioSpecFreq
{-# LINE 120 "Graphics/UI/SDL/Audio.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr (fromAudioFormat audioSpecFormat)
{-# LINE 121 "Graphics/UI/SDL/Audio.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr audioSpecChannels
{-# LINE 122 "Graphics/UI/SDL/Audio.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 7) ptr audioSpecSilence
{-# LINE 123 "Graphics/UI/SDL/Audio.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr audioSpecSamples
{-# LINE 124 "Graphics/UI/SDL/Audio.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr audioSpecSize
{-# LINE 125 "Graphics/UI/SDL/Audio.hsc" #-}
    
    cb <- mkAudioCallback $ \_ buffer len -> do
      v <- fromMaybe (return . flip V.replicate 0 . fromIntegral) audioSpecCallback $ len
      let (vForeignPtr, len') = V.unsafeToForeignPtr0 v
      withForeignPtr vForeignPtr $ \vPtr ->
        copyBytes buffer vPtr (min (fromIntegral len) (fromIntegral len'))

    (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr cb
{-# LINE 133 "Graphics/UI/SDL/Audio.hsc" #-}

  peek ptr = AudioSpec
    <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 136 "Graphics/UI/SDL/Audio.hsc" #-}
    <*> (toAudioFormat <$> (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr)
{-# LINE 137 "Graphics/UI/SDL/Audio.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 138 "Graphics/UI/SDL/Audio.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 7) ptr
{-# LINE 139 "Graphics/UI/SDL/Audio.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 140 "Graphics/UI/SDL/Audio.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 141 "Graphics/UI/SDL/Audio.hsc" #-}
    <*> pure Nothing

foreign import ccall safe "&SDL_FreeWAV"
  sdlFreeWAV_finalizer :: FunPtr (Ptr (Word8) -> IO ())
{-# LINE 145 "Graphics/UI/SDL/Audio.hsc" #-}

foreign import ccall safe "SDL_LoadWAV_RW"
  sdlLoadWAV :: Ptr RWopsStruct -> Int32 -> Ptr AudioSpec -> Ptr (Ptr Word8) -> Ptr (Word32) -> IO (Ptr AudioSpec)
{-# LINE 148 "Graphics/UI/SDL/Audio.hsc" #-}

loadWAV :: FilePath -> AudioSpec -> IO (Vector Word8, AudioSpec)
loadWAV filePath desiredSpec =
  RWOps.withFile filePath "r" $ \rwops ->
  withForeignPtr rwops $ \cRwops ->
  with desiredSpec $ \desiredSpecPtr ->
  alloca $ \outputBufferSizePtr -> do
    outputBufferPtr <- malloc
    actualSpecBuffer <-  throwIfNull "loadWAV: failed to load WAV" $
      sdlLoadWAV cRwops 0 desiredSpecPtr outputBufferPtr outputBufferSizePtr

    peek actualSpecBuffer >>= \actualSpec -> do
      outputBuffer <- peek outputBufferPtr
      foreignAudioBuffer <- newForeignPtr sdlFreeWAV_finalizer outputBuffer
      outputBufferV <- V.unsafeFromForeignPtr0 foreignAudioBuffer . fromIntegral <$> peek outputBufferSizePtr
      return (outputBufferV, actualSpec)

foreign import ccall safe "SDL_OpenAudioDevice"
  sdlOpenAudioDevice :: CString -> Int32 -> Ptr AudioSpec -> Ptr AudioSpec -> Int32
{-# LINE 167 "Graphics/UI/SDL/Audio.hsc" #-}
                     -> IO (Word32)
{-# LINE 168 "Graphics/UI/SDL/Audio.hsc" #-}

data AudioDeviceUsage = ForPlayback | ForCapture

newtype AudioDevice = AudioDevice (Word32)
{-# LINE 172 "Graphics/UI/SDL/Audio.hsc" #-}

openAudioDevice :: Maybe String -> AudioDeviceUsage -> AudioSpec -> [a] -> IO (AudioDevice, AudioSpec)
openAudioDevice deviceName usage desiredSpec _ =
  maybeWith withCString deviceName $ \cDevName ->
  with desiredSpec $ \desiredSpecPtr ->
  alloca $ \actualSpecPtr -> do
    devId <- sdlOpenAudioDevice cDevName (encodeUsage usage) desiredSpecPtr actualSpecPtr
               (7)
{-# LINE 180 "Graphics/UI/SDL/Audio.hsc" #-}
    actualSpec <- peek actualSpecPtr
    return (AudioDevice devId, actualSpec)

foreign import ccall safe "SDL_OpenAudio"
  sdlOpenAudio :: Ptr AudioSpec -> Ptr AudioSpec -> IO Int32
{-# LINE 185 "Graphics/UI/SDL/Audio.hsc" #-}

openAudio :: AudioSpec -> IO (Maybe AudioSpec)
openAudio desiredSpec =
   with desiredSpec $ \desiredSpecPtr ->
   alloca $ \obtainedSpec -> do
     fatalSDLBool "SDL_OpenAudio" (sdlOpenAudio desiredSpecPtr obtainedSpec)
     maybePeek peek obtainedSpec

encodeUsage :: AudioDeviceUsage -> Int32
{-# LINE 194 "Graphics/UI/SDL/Audio.hsc" #-}
encodeUsage ForPlayback = 0
encodeUsage ForCapture = 1

foreign import ccall safe "SDL_PauseAudioDevice"
  sdlPauseAudioDevice :: Word32 -> Int32 -> IO ()
{-# LINE 199 "Graphics/UI/SDL/Audio.hsc" #-}

pauseAudioDevice :: AudioDevice -> Bool -> IO ()
pauseAudioDevice (AudioDevice dId) paused =
  sdlPauseAudioDevice dId (if paused then 1 else 0)
 
--------------------------------------------------------------------------------
foreign import ccall safe "SDL_LockAudio"
  lockAudio :: IO ()
  
foreign import ccall safe "SDL_UnlockAudio"
  unlockAudio :: IO ()
  
--------------------------------------------------------------------------------
foreign import ccall safe "SDL_LockAudioDevice"
  sdlLockAudioDevice :: Word32 -> IO ()
{-# LINE 214 "Graphics/UI/SDL/Audio.hsc" #-}

lockAudioDevice :: AudioDevice -> IO ()
lockAudioDevice (AudioDevice dId) = sdlLockAudioDevice dId

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_UnlockAudioDevice"
  sdlUnlockAudioDevice :: Word32 -> IO ()
{-# LINE 221 "Graphics/UI/SDL/Audio.hsc" #-}

unlockAudioDevice :: AudioDevice -> IO ()
unlockAudioDevice (AudioDevice dId) = sdlUnlockAudioDevice dId

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetNumAudioDrivers"
  getNumAudioDrivers :: IO Int32
{-# LINE 228 "Graphics/UI/SDL/Audio.hsc" #-}

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetAudioDriver"
  sdlGetAudioDriver :: Int32 -> IO CString
{-# LINE 232 "Graphics/UI/SDL/Audio.hsc" #-}

getAudioDriver :: Int32 -> IO String
{-# LINE 234 "Graphics/UI/SDL/Audio.hsc" #-}
getAudioDriver = sdlGetAudioDriver >=> peekCString

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetCurrentAudioDriver"
  sdlGetCurrentAudioDriver :: IO CString

getCurrentAudioDriver :: IO (Maybe String)
getCurrentAudioDriver = sdlGetCurrentAudioDriver >>= maybePeek peekCString

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetNumAudioDevices"
  sdlGetNumAudioDevices :: Int32 -> IO Int32
{-# LINE 246 "Graphics/UI/SDL/Audio.hsc" #-}

getNumAudioDevices :: AudioDeviceUsage -> IO Int32
{-# LINE 248 "Graphics/UI/SDL/Audio.hsc" #-}
getNumAudioDevices = sdlGetNumAudioDevices . encodeUsage

--------------------------------------------------------------------------------
data AudioStatus = AudioStopped | AudioPlaying | AudioPaused

decodeAudioStatus :: Word32 -> AudioStatus
{-# LINE 254 "Graphics/UI/SDL/Audio.hsc" #-}
decodeAudioStatus 0 = AudioStopped
{-# LINE 255 "Graphics/UI/SDL/Audio.hsc" #-}
decodeAudioStatus 1 = AudioPlaying
{-# LINE 256 "Graphics/UI/SDL/Audio.hsc" #-}
decodeAudioStatus 2 = AudioPaused
{-# LINE 257 "Graphics/UI/SDL/Audio.hsc" #-}
decodeAudioStatus i = error $ "Unexpected SDL_AudioStatus: " ++ show i

foreign import ccall safe "SDL_GetAudioStatus"
  sdlGetAudioStatus :: IO Word32
{-# LINE 261 "Graphics/UI/SDL/Audio.hsc" #-}

getAudioStatus :: IO AudioStatus
getAudioStatus = decodeAudioStatus <$> sdlGetAudioStatus

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetAudioDeviceName"
  sdlGetAudioDeviceName :: Int32 -> Int32 -> IO CString
{-# LINE 268 "Graphics/UI/SDL/Audio.hsc" #-}

getAudioDeviceName :: AudioDeviceUsage -> Int32 -> IO String
{-# LINE 270 "Graphics/UI/SDL/Audio.hsc" #-}
getAudioDeviceName usage index =
  fatalSDLNull "SDL_GetAudioDeviceName"
    (sdlGetAudioDeviceName (encodeUsage usage) index) >>= peekCString

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetAudioDeviceStatus"
  sdlGetAudioDeviceStatus :: Word32 -> IO Word32
{-# LINE 277 "Graphics/UI/SDL/Audio.hsc" #-}

getAudioDeviceStatus :: AudioDevice -> IO AudioStatus
getAudioDeviceStatus (AudioDevice dId) =
  decodeAudioStatus <$> sdlGetAudioDeviceStatus dId

foreign import ccall safe "SDL_MixAudio"
  sdlMixAudio :: Ptr Word8 -> Ptr Word8 -> Word32 -> Int32 -> IO ()
{-# LINE 284 "Graphics/UI/SDL/Audio.hsc" #-}

mixAudio :: ByteString -> ByteString -> Int -> IO ()
mixAudio xbs ybs volume =
  let (ybs', _, _)   = BI.toForeignPtr ybs
      (xbs', _, len) = BI.toForeignPtr xbs
  in withForeignPtr ybs' $ \ybs'' ->
     withForeignPtr xbs' $ \xbs'' ->
       let len' = fromIntegral $ len
           vol' = fromIntegral volume
       in sdlMixAudio xbs'' ybs'' len' vol'

foreign import ccall safe "SDL_MixAudioFormat"
  sdlMixAudioFormat :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word32 -> Int32 -> IO ()
{-# LINE 297 "Graphics/UI/SDL/Audio.hsc" #-}

mixAudioFormat :: ByteString -> ByteString -> AudioFormat -> Int -> IO ()
mixAudioFormat xbs ybs aufmt volume =
  let (ybs', _, _)   = BI.toForeignPtr ybs
      (xbs', _, len) = BI.toForeignPtr xbs
  in withForeignPtr ybs' $ \ybs'' ->
     withForeignPtr xbs' $ \xbs'' ->
       let len' = fromIntegral $ len
           vol' = fromIntegral volume
           fmt' = fromAudioFormat aufmt
       in sdlMixAudioFormat xbs'' ybs'' fmt' len' vol'

foreign import ccall safe "SDL_AudioInit"
  sdlAudioInit :: CString -> IO Int32
{-# LINE 311 "Graphics/UI/SDL/Audio.hsc" #-}

audioInit :: String -> IO ()
audioInit driver_name =
  withCString driver_name $ \cstr ->
    fatalSDLBool "SDL_AudioInit" (sdlAudioInit cstr)

foreign import ccall safe "SDL_AudioQuit"
  audioQuit :: IO ()

foreign import ccall safe "SDL_CloseAudio"
  closeAudio :: IO ()

foreign import ccall safe "SDL_CloseAudioDevice"
  sdlCloseAudioDevice :: Word32 -> IO ()
{-# LINE 325 "Graphics/UI/SDL/Audio.hsc" #-}

closeAudioDevice :: AudioDevice -> IO ()
closeAudioDevice (AudioDevice dev) = sdlCloseAudioDevice dev

foreign import ccall safe "SDL_PauseAudio"
  sdlPauseAudio :: Int32 -> IO ()
{-# LINE 331 "Graphics/UI/SDL/Audio.hsc" #-}

-- | True to pause. False to unpause.
pauseAudio :: Bool -> IO ()
pauseAudio True  = sdlPauseAudio 1
pauseAudio False = sdlPauseAudio 0

