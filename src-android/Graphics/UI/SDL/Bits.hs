{-# LINE 1 "Graphics/UI/SDL/Bits.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Bits.hsc" #-}
module Graphics.UI.SDL.Bits
  ( mostSignificantBitIndex32
  ) where

import Foreign

foreign import ccall safe "SDL_MostSignificantBitIndex32_Wrapper"
  mostSignificantBitIndex32 :: Word32 -> Int32
{-# LINE 10 "Graphics/UI/SDL/Bits.hsc" #-}

