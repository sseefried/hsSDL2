{-# LINE 1 "Graphics/UI/SDL/Rect.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Rect.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Video
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Rect
    ( Rect(..)
    , Point(..)
    , enclosePoints
    , rectEquals
    , rectEmpty
    , hasIntersection
    , intersectRect
    , intersectRectAndLine
    , unionRect
    ) where

import Foreign
import Foreign.C
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Utilities

data Rect
    = Rect
    { rectX, rectY :: Int,
      rectW, rectH :: Int }
    deriving (Show,Eq,Ord)

instance Storable Rect where
    sizeOf = const (16)
{-# LINE 38 "Graphics/UI/SDL/Rect.hsc" #-}
    alignment = const 2
    peek ptr
        = do x <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr :: IO CInt
{-# LINE 41 "Graphics/UI/SDL/Rect.hsc" #-}
             y <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr :: IO CInt
{-# LINE 42 "Graphics/UI/SDL/Rect.hsc" #-}
             w <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr :: IO CInt
{-# LINE 43 "Graphics/UI/SDL/Rect.hsc" #-}
             h <- (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr :: IO CInt
{-# LINE 44 "Graphics/UI/SDL/Rect.hsc" #-}
             return $! Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
    poke ptr (Rect x y w h)
        = do (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (fromIntegral x :: CInt)
{-# LINE 47 "Graphics/UI/SDL/Rect.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr (fromIntegral y :: CInt)
{-# LINE 48 "Graphics/UI/SDL/Rect.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr (fromIntegral w :: CInt)
{-# LINE 49 "Graphics/UI/SDL/Rect.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr (fromIntegral h :: CInt)
{-# LINE 50 "Graphics/UI/SDL/Rect.hsc" #-}

data Point = Point { pX :: Int, pY :: Int }

instance Storable Point where
  sizeOf = const (8)
{-# LINE 55 "Graphics/UI/SDL/Rect.hsc" #-}
  alignment = const 2
  peek ptr = do
    x <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr :: IO CInt
{-# LINE 58 "Graphics/UI/SDL/Rect.hsc" #-}
    y <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr :: IO CInt
{-# LINE 59 "Graphics/UI/SDL/Rect.hsc" #-}
    return $! Point (fromIntegral x) (fromIntegral y)
  poke ptr (Point x y) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (fromIntegral x :: CInt)
{-# LINE 62 "Graphics/UI/SDL/Rect.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr (fromIntegral y :: CInt)
{-# LINE 63 "Graphics/UI/SDL/Rect.hsc" #-}

foreign import ccall unsafe "SDL_EnclosePoints"
  sdlEnclosePoints :: Ptr Point -> Int32 -> Ptr Rect -> Ptr Rect -> IO Word32
{-# LINE 66 "Graphics/UI/SDL/Rect.hsc" #-}

enclosePoints :: [Point] -> Maybe Rect -> IO Rect
enclosePoints points clip =
  alloca $ \rect' ->
  allocaArray (length points) $ \points' -> do
    pokeArray points' points
    let count = fromIntegral (length points)
    r <- maybeWith with clip $ \clip' -> sdlEnclosePoints points' count clip' rect'
    rect <- peek rect'
    handleErrorI "enclosePoints" r $ const $ return rect

foreign import ccall unsafe "SDL_RectEmpty_Wrapper"
  sdlRectEmpty :: Ptr Rect -> IO Word32
{-# LINE 79 "Graphics/UI/SDL/Rect.hsc" #-}

rectEmpty :: Rect -> IO Bool
rectEmpty rect =
  (with rect $ sdlRectEmpty) >>= return . sdlBoolToBool

foreign import ccall unsafe "SDL_RectEquals_Wrapper"
  sdlRectEquals :: Ptr Rect -> Ptr Rect -> IO Word32
{-# LINE 86 "Graphics/UI/SDL/Rect.hsc" #-}

rectEquals :: Rect -> Rect -> IO Bool
rectEquals a b =
  with a $ \a' ->
  with b $ \b' ->
    sdlRectEquals a' b' >>= return . sdlBoolToBool

foreign import ccall unsafe "SDL_HasIntersection"
  sdlHasIntersection :: Ptr Rect -> Ptr Rect -> IO Word32
{-# LINE 95 "Graphics/UI/SDL/Rect.hsc" #-}

hasIntersection :: Rect -> Rect -> IO Bool
hasIntersection a b =
  with a $ \a' ->
  with b $ \b' ->
    sdlHasIntersection a' b' >>= return . sdlBoolToBool

foreign import ccall unsafe "SDL_IntersectRect"
  sdlIntersectRect :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO Word32
{-# LINE 104 "Graphics/UI/SDL/Rect.hsc" #-}

intersectRect :: Rect -> Rect -> IO Rect
intersectRect a b =
  with a $ \a' ->
  with b $ \b' -> do
    alloca $ \c' -> do
      r <- sdlIntersectRect a' b' c'
      c <- peek c'
      handleErrorI "intersectRect" r $ const $ return c

foreign import ccall unsafe "SDL_IntersectRectAndLine"
  sdlIntersectRectAndLine :: Ptr Rect -> Ptr Int32 -> Ptr Int32
{-# LINE 116 "Graphics/UI/SDL/Rect.hsc" #-}
                             -> Ptr Int32 -> Ptr Int32 -> IO Word32
{-# LINE 117 "Graphics/UI/SDL/Rect.hsc" #-}

intersectRectAndLine :: Rect -> Int -> Int -> Int -> Int -> IO Bool
intersectRectAndLine r x1 y1 x2 y2 =
  with r $ \r' ->
  with (fromIntegral x1) $ \x1' ->
  with (fromIntegral y1) $ \y1' ->
  with (fromIntegral x2) $ \x2' ->
  with (fromIntegral y2) $ \y2' ->
    sdlIntersectRectAndLine r' x1' y1' x2' y2' >>= return . sdlBoolToBool

foreign import ccall unsafe "SDL_UnionRect"
  sdlUnionRect :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO ()

unionRect :: Rect -> Rect -> IO Rect
unionRect a b =
  with a $ \a' ->
  with b $ \b' ->
    alloca $ \c' -> do
      sdlUnionRect a' b' c'
      handleError "unionRect" c' peek

