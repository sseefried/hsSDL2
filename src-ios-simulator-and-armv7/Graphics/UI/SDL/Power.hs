{-# LINE 1 "Graphics/UI/SDL/Power.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Power.hsc" #-}
module Graphics.UI.SDL.Power
    ( getPowerInfo
    , PowerState(..)
    , PowerInfo(..)
    ) where

import Control.Applicative
import Data.Time (DiffTime)
import Foreign

--------------------------------------------------------------------------------
data PowerState
  = PowerStateUnknown
  | PowerStateOnBattery
  | PowerStateNoBattery
  | PowerStateCharging
  | PowerStateCharged

decodePowerState :: Word32 -> PowerState
{-# LINE 21 "Graphics/UI/SDL/Power.hsc" #-}
decodePowerState 0 = PowerStateUnknown
{-# LINE 22 "Graphics/UI/SDL/Power.hsc" #-}
decodePowerState 1 = PowerStateOnBattery
{-# LINE 23 "Graphics/UI/SDL/Power.hsc" #-}
decodePowerState 2 = PowerStateNoBattery
{-# LINE 24 "Graphics/UI/SDL/Power.hsc" #-}
decodePowerState 3 = PowerStateCharging
{-# LINE 25 "Graphics/UI/SDL/Power.hsc" #-}
decodePowerState 4 = PowerStateCharged
{-# LINE 26 "Graphics/UI/SDL/Power.hsc" #-}
decodePowerState i = error $ "Unknown SDL_PowerState: " ++ show i

--------------------------------------------------------------------------------
data PowerInfo = PowerInfo { powerInfoState :: !PowerState
                           , powerInfoDurationLeft :: !(Maybe DiffTime)
                           , powerInfoPercentLeft :: !(Maybe Int32)
{-# LINE 32 "Graphics/UI/SDL/Power.hsc" #-}
                           }

foreign import ccall unsafe "SDL_GetPowerInfo"
  sdlGetPowerInfo :: Ptr Int32 -> Ptr Int32 -> IO Word32
{-# LINE 36 "Graphics/UI/SDL/Power.hsc" #-}

getPowerInfo :: IO PowerInfo
getPowerInfo =
  alloca $ \secPtr ->
  alloca $ \pctPtr ->
  PowerInfo <$> (decodePowerState <$> sdlGetPowerInfo secPtr pctPtr)
            <*> (whenPositive fromIntegral <$> peek secPtr)
            <*> (whenPositive id <$> peek pctPtr)
  where whenPositive f x | x < 0 = Nothing
                         | otherwise = Just (f x)
