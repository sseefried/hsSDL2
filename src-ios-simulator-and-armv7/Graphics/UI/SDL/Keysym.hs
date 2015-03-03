{-# LINE 1 "Graphics/UI/SDL/Keysym.hsc" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LINE 2 "Graphics/UI/SDL/Keysym.hsc" #-}

{-# LINE 3 "Graphics/UI/SDL/Keysym.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Keysym
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Keysym where

import Prelude hiding (Either(Left, Right))
import Control.Applicative
import Foreign
import Graphics.UI.SDL.Keycode (Keycode)

data Keysym = Keysym { keyScancode :: Scancode
                     , keyKeycode :: Keycode
                     , keyModifiers :: Word16
                     }
  deriving (Eq, Show)

instance Storable Keysym where
  sizeOf = const (16)
{-# LINE 29 "Graphics/UI/SDL/Keysym.hsc" #-}

  alignment = const 4

  poke ptr (Keysym _s _k m) = do
    -- TODO? Do we care about poking keysyms?
    -- #{poke SDL_Keysym, scancode} ptr s
    -- #{poke SDL_Keysym, sym} ptr k
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr m
{-# LINE 37 "Graphics/UI/SDL/Keysym.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr (0 :: Word32)
{-# LINE 38 "Graphics/UI/SDL/Keysym.hsc" #-}

  peek ptr = Keysym <$> ((toEnum . fromIntegral :: Int32 -> Scancode) <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
{-# LINE 40 "Graphics/UI/SDL/Keysym.hsc" #-}
                    <*> ((toEnum . fromIntegral :: Int32 -> Keycode) <$> (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr)
{-# LINE 41 "Graphics/UI/SDL/Keysym.hsc" #-}
                    <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 42 "Graphics/UI/SDL/Keysym.hsc" #-}

data Scancode
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  | Number1
  | Number2
  | Number3
  | Number4
  | Number5
  | Number6
  | Number7
  | Number8
  | Number9
  | Number0
  | Return
  | Escape
  | Backspace
  | Tab
  | Space
  | Minus
  | Equals
  | LeftBracket
  | RightBracket
  | Backslash
  | NonUSHash
  | Semicolon
  | Apostrophe
  | Grave
  | Comma
  | Period
  | Slash
  | Capslock
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | PrintScreen
  | ScrollLock
  | Pause
  | Insert
  | Home
  | PageUp
  | Delete
  | End
  | PageDown
  | Right
  | Left
  | Down
  | Up
  | NumLockClear
  | KeypadDivide
  | KeypadMultiply
  | KeypadMinus
  | KeypadPlus
  | KeypadEnter
  | Keypad1
  | Keypad2
  | Keypad3
  | Keypad4
  | Keypad5
  | Keypad6
  | Keypad7
  | Keypad8
  | Keypad9
  | Keypad0
  | KeypadPeriod
  | NonUSBackslash
  | Application
  | Power
  | KeypadEquals
  | F13
  | F14
  | F15
  | F16
  | F17
  | F18
  | F19
  | F20
  | F21
  | F22
  | F23
  | F24
  | Execute
  | Help
  | Menu
  | Select
  | Stop
  | Again
  | Undo
  | Cut
  | Copy
  | Paste
  | Find
  | Mute
  | VolumeUp
  | VolumeDown
  | KeypadComma
  | KeyPadEqualsAs400
  | International1
  | International2
  | International3
  | International4
  | International5
  | International6
  | International7
  | International8
  | International9
  | Lang1
  | Lang2
  | Lang3
  | Lang4
  | Lang5
  | Lang6
  | Lang7
  | Lang8
  | Lang9
  | AltErase
  | SysReq
  | Cancel
  | Clear
  | Prior
  | Return2
  | Separator
  | Out
  | Oper
  | ClearAgain
  | CrSel
  | ExSel
  | Keypad00
  | Keypad000
  | ThousandSeparator
  | DecimalSeparator
  | CurrencyUnit
  | CurrencySubunit
  | KeypadLeftParen
  | KeypadRightParen
  | KeypadLeftBrace
  | KeypadRightBrace
  | KeypadTab
  | KeypadBackspace
  | KeypadA
  | KeypadB
  | KeypadC
  | KeypadD
  | KeypadE
  | KeypadF
  | KeypadXOR
  | KeypadPower
  | KeypadPercent
  | KeypadLess
  | KeypadGreater
  | KeypadAmpersand
  | KeypadDoubleAmpersand
  | KeypadVerticalBar
  | KeypadDoubleVerticalBar
  | KeypadColon
  | KeypadHash
  | KeypadSpace
  | KeypadAt
  | KeypadExclamation
  | KeypadMemStore
  | KeypadMemRecall
  | KeypadMemClear
  | KeypadMemAdd
  | KeypadMemSubstract
  | KeypadMemMultiply
  | KeypadMemDivide
  | KeypadPlusMinus
  | KeypadClear
  | KeypadClearEntry
  | KeypadBinary
  | KeypadOctal
  | KeypadDecimal
  | KeypadHexadecimal
  | LeftControl
  | LeftShift
  | LeftAlt
  | LeftGUI
  | RightControl
  | RightShift
  | RightAlt
  | RightGUI
  | Mode
  | AudioNext
  | AudioPrevious
  | AudioStop
  | AudioPlay
  | AudioMute
  | MediaSelect
  | WWW
  | Mail
  | Calculator
  | Computer
  | ACSearch
  | ACHome
  | ACBack
  | ACForward
  | ACStop
  | ACRefresh
  | ACBookmarks
  | BrightnessDown
  | BrightnessUp
  | DisplaySwitch
  | KBIllumToggle
  | KBIllumDown
  | KBIllumUp
  | Eject
  | Sleep
  | App1
  | App2
  deriving (Eq, Ord, Show)

instance Enum Scancode where
  toEnum 4 = A
{-# LINE 288 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 5 = B
{-# LINE 289 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 6 = C
{-# LINE 290 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 7 = D
{-# LINE 291 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 8 = E
{-# LINE 292 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 9 = F
{-# LINE 293 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 10 = G
{-# LINE 294 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 11 = H
{-# LINE 295 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 12 = I
{-# LINE 296 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 13 = J
{-# LINE 297 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 14 = K
{-# LINE 298 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 15 = L
{-# LINE 299 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 16 = M
{-# LINE 300 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 17 = N
{-# LINE 301 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 18 = O
{-# LINE 302 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 19 = P
{-# LINE 303 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 20 = Q
{-# LINE 304 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 21 = R
{-# LINE 305 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 22 = S
{-# LINE 306 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 23 = T
{-# LINE 307 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 24 = U
{-# LINE 308 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 25 = V
{-# LINE 309 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 26 = W
{-# LINE 310 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 27 = X
{-# LINE 311 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 28 = Y
{-# LINE 312 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 29 = Z
{-# LINE 313 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 30 = Number1
{-# LINE 314 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 31 = Number2
{-# LINE 315 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 32 = Number3
{-# LINE 316 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 33 = Number4
{-# LINE 317 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 34 = Number5
{-# LINE 318 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 35 = Number6
{-# LINE 319 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 36 = Number7
{-# LINE 320 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 37 = Number8
{-# LINE 321 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 38 = Number9
{-# LINE 322 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 39 = Number0
{-# LINE 323 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 40 = Return
{-# LINE 324 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 41 = Escape
{-# LINE 325 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 42 = Backspace
{-# LINE 326 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 43 = Tab
{-# LINE 327 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 44 = Space
{-# LINE 328 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 45 = Minus
{-# LINE 329 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 46 = Equals
{-# LINE 330 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 47 = LeftBracket
{-# LINE 331 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 48 = RightBracket
{-# LINE 332 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 49 = Backslash
{-# LINE 333 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 50 = NonUSHash
{-# LINE 334 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 51 = Semicolon
{-# LINE 335 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 52 = Apostrophe
{-# LINE 336 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 53 = Grave
{-# LINE 337 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 54 = Comma
{-# LINE 338 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 55 = Period
{-# LINE 339 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 56 = Slash
{-# LINE 340 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 57 = Capslock
{-# LINE 341 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 58 = F1
{-# LINE 342 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 59 = F2
{-# LINE 343 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 60 = F3
{-# LINE 344 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 61 = F4
{-# LINE 345 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 62 = F5
{-# LINE 346 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 63 = F6
{-# LINE 347 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 64 = F7
{-# LINE 348 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 65 = F8
{-# LINE 349 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 66 = F9
{-# LINE 350 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 67 = F10
{-# LINE 351 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 68 = F11
{-# LINE 352 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 69 = F12
{-# LINE 353 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 70 = PrintScreen
{-# LINE 354 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 71 = ScrollLock
{-# LINE 355 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 72 = Pause
{-# LINE 356 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 73 = Insert
{-# LINE 357 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 74 = Home
{-# LINE 358 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 75 = PageUp
{-# LINE 359 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 76 = Delete
{-# LINE 360 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 77 = End
{-# LINE 361 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 78 = PageDown
{-# LINE 362 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 79 = Right
{-# LINE 363 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 80 = Left
{-# LINE 364 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 81 = Down
{-# LINE 365 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 82 = Up
{-# LINE 366 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 83 = NumLockClear
{-# LINE 367 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 84 = KeypadDivide
{-# LINE 368 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 85 = KeypadMultiply
{-# LINE 369 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 86 = KeypadMinus
{-# LINE 370 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 87 = KeypadPlus
{-# LINE 371 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 88 = KeypadEnter
{-# LINE 372 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 89 = Keypad1
{-# LINE 373 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 90 = Keypad2
{-# LINE 374 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 91 = Keypad3
{-# LINE 375 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 92 = Keypad4
{-# LINE 376 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 93 = Keypad5
{-# LINE 377 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 94 = Keypad6
{-# LINE 378 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 95 = Keypad7
{-# LINE 379 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 96 = Keypad8
{-# LINE 380 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 97 = Keypad9
{-# LINE 381 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 98 = Keypad0
{-# LINE 382 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 99 = KeypadPeriod
{-# LINE 383 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 100 = NonUSBackslash
{-# LINE 384 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 101 = Application
{-# LINE 385 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 102 = Power
{-# LINE 386 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 103 = KeypadEquals
{-# LINE 387 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 104 = F13
{-# LINE 388 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 105 = F14
{-# LINE 389 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 106 = F15
{-# LINE 390 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 107 = F16
{-# LINE 391 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 108 = F17
{-# LINE 392 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 109 = F18
{-# LINE 393 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 110 = F19
{-# LINE 394 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 111 = F20
{-# LINE 395 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 112 = F21
{-# LINE 396 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 113 = F22
{-# LINE 397 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 114 = F23
{-# LINE 398 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 115 = F24
{-# LINE 399 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 116 = Execute
{-# LINE 400 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 117 = Help
{-# LINE 401 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 118 = Menu
{-# LINE 402 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 119 = Select
{-# LINE 403 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 120 = Stop
{-# LINE 404 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 121 = Again
{-# LINE 405 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 122 = Undo
{-# LINE 406 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 123 = Cut
{-# LINE 407 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 124 = Copy
{-# LINE 408 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 125 = Paste
{-# LINE 409 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 126 = Find
{-# LINE 410 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 127 = Mute
{-# LINE 411 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 128 = VolumeUp
{-# LINE 412 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 129 = VolumeDown
{-# LINE 413 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 133 = KeypadComma
{-# LINE 414 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 134 = KeyPadEqualsAs400
{-# LINE 415 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 135 = International1
{-# LINE 416 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 136 = International2
{-# LINE 417 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 137 = International3
{-# LINE 418 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 138 = International4
{-# LINE 419 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 139 = International5
{-# LINE 420 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 140 = International6
{-# LINE 421 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 141 = International7
{-# LINE 422 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 142 = International8
{-# LINE 423 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 143 = International9
{-# LINE 424 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 144 = Lang1
{-# LINE 425 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 145 = Lang2
{-# LINE 426 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 146 = Lang3
{-# LINE 427 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 147 = Lang4
{-# LINE 428 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 148 = Lang5
{-# LINE 429 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 149 = Lang6
{-# LINE 430 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 150 = Lang7
{-# LINE 431 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 151 = Lang8
{-# LINE 432 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 152 = Lang9
{-# LINE 433 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 153 = AltErase
{-# LINE 434 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 154 = SysReq
{-# LINE 435 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 155 = Cancel
{-# LINE 436 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 156 = Clear
{-# LINE 437 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 157 = Prior
{-# LINE 438 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 158 = Return2
{-# LINE 439 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 159 = Separator
{-# LINE 440 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 160 = Out
{-# LINE 441 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 161 = Oper
{-# LINE 442 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 162 = ClearAgain
{-# LINE 443 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 163 = CrSel
{-# LINE 444 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 164 = ExSel
{-# LINE 445 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 176 = Keypad00
{-# LINE 446 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 177 = Keypad000
{-# LINE 447 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 178 = ThousandSeparator
{-# LINE 448 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 179 = DecimalSeparator
{-# LINE 449 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 180 = CurrencyUnit
{-# LINE 450 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 181 = CurrencySubunit
{-# LINE 451 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 182 = KeypadLeftParen
{-# LINE 452 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 183 = KeypadRightParen
{-# LINE 453 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 184 = KeypadLeftBrace
{-# LINE 454 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 185 = KeypadRightBrace
{-# LINE 455 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 186 = KeypadTab
{-# LINE 456 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 187 = KeypadBackspace
{-# LINE 457 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 188 = KeypadA
{-# LINE 458 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 189 = KeypadB
{-# LINE 459 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 190 = KeypadC
{-# LINE 460 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 191 = KeypadD
{-# LINE 461 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 192 = KeypadE
{-# LINE 462 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 193 = KeypadF
{-# LINE 463 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 194 = KeypadXOR
{-# LINE 464 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 195 = KeypadPower
{-# LINE 465 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 196 = KeypadPercent
{-# LINE 466 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 197 = KeypadLess
{-# LINE 467 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 198 = KeypadGreater
{-# LINE 468 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 199 = KeypadAmpersand
{-# LINE 469 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 200 = KeypadDoubleAmpersand
{-# LINE 470 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 201 = KeypadVerticalBar
{-# LINE 471 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 202 = KeypadDoubleVerticalBar
{-# LINE 472 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 203 = KeypadColon
{-# LINE 473 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 204 = KeypadHash
{-# LINE 474 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 205 = KeypadSpace
{-# LINE 475 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 206 = KeypadAt
{-# LINE 476 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 207 = KeypadExclamation
{-# LINE 477 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 208 = KeypadMemStore
{-# LINE 478 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 209 = KeypadMemRecall
{-# LINE 479 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 210 = KeypadMemClear
{-# LINE 480 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 211 = KeypadMemAdd
{-# LINE 481 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 212 = KeypadMemSubstract
{-# LINE 482 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 213 = KeypadMemMultiply
{-# LINE 483 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 214 = KeypadMemDivide
{-# LINE 484 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 215 = KeypadPlusMinus
{-# LINE 485 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 216 = KeypadClear
{-# LINE 486 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 217 = KeypadClearEntry
{-# LINE 487 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 218 = KeypadBinary
{-# LINE 488 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 219 = KeypadOctal
{-# LINE 489 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 220 = KeypadDecimal
{-# LINE 490 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 221 = KeypadHexadecimal
{-# LINE 491 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 224 = LeftControl
{-# LINE 492 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 225 = LeftShift
{-# LINE 493 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 226 = LeftAlt
{-# LINE 494 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 227 = LeftGUI
{-# LINE 495 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 228 = RightControl
{-# LINE 496 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 229 = RightShift
{-# LINE 497 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 230 = RightAlt
{-# LINE 498 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 231 = RightGUI
{-# LINE 499 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 257 = Mode
{-# LINE 500 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 258 = AudioNext
{-# LINE 501 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 259 = AudioPrevious
{-# LINE 502 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 260 = AudioStop
{-# LINE 503 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 261 = AudioPlay
{-# LINE 504 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 262 = AudioMute
{-# LINE 505 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 263 = MediaSelect
{-# LINE 506 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 264 = WWW
{-# LINE 507 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 265 = Mail
{-# LINE 508 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 266 = Calculator
{-# LINE 509 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 267 = Computer
{-# LINE 510 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 268 = ACSearch
{-# LINE 511 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 269 = ACHome
{-# LINE 512 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 270 = ACBack
{-# LINE 513 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 271 = ACForward
{-# LINE 514 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 272 = ACStop
{-# LINE 515 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 273 = ACRefresh
{-# LINE 516 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 274 = ACBookmarks
{-# LINE 517 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 275 = BrightnessDown
{-# LINE 518 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 276 = BrightnessUp
{-# LINE 519 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 277 = DisplaySwitch
{-# LINE 520 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 278 = KBIllumToggle
{-# LINE 521 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 279 = KBIllumDown
{-# LINE 522 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 280 = KBIllumUp
{-# LINE 523 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 281 = Eject
{-# LINE 524 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 282 = Sleep
{-# LINE 525 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 283 = App1
{-# LINE 526 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum 284 = App2
{-# LINE 527 "Graphics/UI/SDL/Keysym.hsc" #-}
  toEnum _ = error "Scancode.toEnum: Invalid argument."

  fromEnum A = 4
{-# LINE 530 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum B = 5
{-# LINE 531 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum C = 6
{-# LINE 532 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum D = 7
{-# LINE 533 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum E = 8
{-# LINE 534 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F = 9
{-# LINE 535 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum G = 10
{-# LINE 536 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum H = 11
{-# LINE 537 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum I = 12
{-# LINE 538 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum J = 13
{-# LINE 539 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum K = 14
{-# LINE 540 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum L = 15
{-# LINE 541 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum M = 16
{-# LINE 542 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum N = 17
{-# LINE 543 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum O = 18
{-# LINE 544 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum P = 19
{-# LINE 545 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Q = 20
{-# LINE 546 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum R = 21
{-# LINE 547 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum S = 22
{-# LINE 548 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum T = 23
{-# LINE 549 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum U = 24
{-# LINE 550 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum V = 25
{-# LINE 551 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum W = 26
{-# LINE 552 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum X = 27
{-# LINE 553 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Y = 28
{-# LINE 554 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Z = 29
{-# LINE 555 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Number1 = 30
{-# LINE 556 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Number2 = 31
{-# LINE 557 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Number3 = 32
{-# LINE 558 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Number4 = 33
{-# LINE 559 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Number5 = 34
{-# LINE 560 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Number6 = 35
{-# LINE 561 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Number7 = 36
{-# LINE 562 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Number8 = 37
{-# LINE 563 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Number9 = 38
{-# LINE 564 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Number0 = 39
{-# LINE 565 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Return = 40
{-# LINE 566 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Escape = 41
{-# LINE 567 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Backspace = 42
{-# LINE 568 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Tab = 43
{-# LINE 569 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Space = 44
{-# LINE 570 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Minus = 45
{-# LINE 571 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Equals = 46
{-# LINE 572 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum LeftBracket = 47
{-# LINE 573 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum RightBracket = 48
{-# LINE 574 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Backslash = 49
{-# LINE 575 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum NonUSHash = 50
{-# LINE 576 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Semicolon = 51
{-# LINE 577 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Apostrophe = 52
{-# LINE 578 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Grave = 53
{-# LINE 579 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Comma = 54
{-# LINE 580 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Period = 55
{-# LINE 581 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Slash = 56
{-# LINE 582 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Capslock = 57
{-# LINE 583 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F1 = 58
{-# LINE 584 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F2 = 59
{-# LINE 585 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F3 = 60
{-# LINE 586 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F4 = 61
{-# LINE 587 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F5 = 62
{-# LINE 588 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F6 = 63
{-# LINE 589 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F7 = 64
{-# LINE 590 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F8 = 65
{-# LINE 591 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F9 = 66
{-# LINE 592 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F10 = 67
{-# LINE 593 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F11 = 68
{-# LINE 594 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F12 = 69
{-# LINE 595 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum PrintScreen = 70
{-# LINE 596 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ScrollLock = 71
{-# LINE 597 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Pause = 72
{-# LINE 598 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Insert = 73
{-# LINE 599 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Home = 74
{-# LINE 600 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum PageUp = 75
{-# LINE 601 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Delete = 76
{-# LINE 602 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum End = 77
{-# LINE 603 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum PageDown = 78
{-# LINE 604 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Right = 79
{-# LINE 605 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Left = 80
{-# LINE 606 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Down = 81
{-# LINE 607 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Up = 82
{-# LINE 608 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum NumLockClear = 83
{-# LINE 609 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadDivide = 84
{-# LINE 610 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadMultiply = 85
{-# LINE 611 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadMinus = 86
{-# LINE 612 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadPlus = 87
{-# LINE 613 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadEnter = 88
{-# LINE 614 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad1 = 89
{-# LINE 615 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad2 = 90
{-# LINE 616 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad3 = 91
{-# LINE 617 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad4 = 92
{-# LINE 618 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad5 = 93
{-# LINE 619 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad6 = 94
{-# LINE 620 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad7 = 95
{-# LINE 621 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad8 = 96
{-# LINE 622 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad9 = 97
{-# LINE 623 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad0 = 98
{-# LINE 624 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadPeriod = 99
{-# LINE 625 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum NonUSBackslash = 100
{-# LINE 626 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Application = 101
{-# LINE 627 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Power = 102
{-# LINE 628 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadEquals = 103
{-# LINE 629 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F13 = 104
{-# LINE 630 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F14 = 105
{-# LINE 631 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F15 = 106
{-# LINE 632 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F16 = 107
{-# LINE 633 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F17 = 108
{-# LINE 634 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F18 = 109
{-# LINE 635 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F19 = 110
{-# LINE 636 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F20 = 111
{-# LINE 637 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F21 = 112
{-# LINE 638 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F22 = 113
{-# LINE 639 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F23 = 114
{-# LINE 640 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum F24 = 115
{-# LINE 641 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Execute = 116
{-# LINE 642 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Help = 117
{-# LINE 643 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Menu = 118
{-# LINE 644 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Select = 119
{-# LINE 645 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Stop = 120
{-# LINE 646 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Again = 121
{-# LINE 647 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Undo = 122
{-# LINE 648 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Cut = 123
{-# LINE 649 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Copy = 124
{-# LINE 650 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Paste = 125
{-# LINE 651 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Find = 126
{-# LINE 652 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Mute = 127
{-# LINE 653 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum VolumeUp = 128
{-# LINE 654 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum VolumeDown = 129
{-# LINE 655 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadComma = 133
{-# LINE 656 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeyPadEqualsAs400 = 134
{-# LINE 657 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum International1 = 135
{-# LINE 658 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum International2 = 136
{-# LINE 659 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum International3 = 137
{-# LINE 660 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum International4 = 138
{-# LINE 661 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum International5 = 139
{-# LINE 662 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum International6 = 140
{-# LINE 663 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum International7 = 141
{-# LINE 664 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum International8 = 142
{-# LINE 665 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum International9 = 143
{-# LINE 666 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Lang1 = 144
{-# LINE 667 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Lang2 = 145
{-# LINE 668 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Lang3 = 146
{-# LINE 669 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Lang4 = 147
{-# LINE 670 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Lang5 = 148
{-# LINE 671 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Lang6 = 149
{-# LINE 672 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Lang7 = 150
{-# LINE 673 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Lang8 = 151
{-# LINE 674 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Lang9 = 152
{-# LINE 675 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum AltErase = 153
{-# LINE 676 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum SysReq = 154
{-# LINE 677 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Cancel = 155
{-# LINE 678 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Clear = 156
{-# LINE 679 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Prior = 157
{-# LINE 680 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Return2 = 158
{-# LINE 681 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Separator = 159
{-# LINE 682 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Out = 160
{-# LINE 683 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Oper = 161
{-# LINE 684 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ClearAgain = 162
{-# LINE 685 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum CrSel = 163
{-# LINE 686 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ExSel = 164
{-# LINE 687 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad00 = 176
{-# LINE 688 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Keypad000 = 177
{-# LINE 689 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ThousandSeparator = 178
{-# LINE 690 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum DecimalSeparator = 179
{-# LINE 691 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum CurrencyUnit = 180
{-# LINE 692 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum CurrencySubunit = 181
{-# LINE 693 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadLeftParen = 182
{-# LINE 694 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadRightParen = 183
{-# LINE 695 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadLeftBrace = 184
{-# LINE 696 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadRightBrace = 185
{-# LINE 697 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadTab = 186
{-# LINE 698 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadBackspace = 187
{-# LINE 699 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadA = 188
{-# LINE 700 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadB = 189
{-# LINE 701 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadC = 190
{-# LINE 702 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadD = 191
{-# LINE 703 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadE = 192
{-# LINE 704 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadF = 193
{-# LINE 705 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadXOR = 194
{-# LINE 706 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadPower = 195
{-# LINE 707 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadPercent = 196
{-# LINE 708 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadLess = 197
{-# LINE 709 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadGreater = 198
{-# LINE 710 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadAmpersand = 199
{-# LINE 711 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadDoubleAmpersand = 200
{-# LINE 712 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadVerticalBar = 201
{-# LINE 713 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadDoubleVerticalBar = 202
{-# LINE 714 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadColon = 203
{-# LINE 715 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadHash = 204
{-# LINE 716 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadSpace = 205
{-# LINE 717 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadAt = 206
{-# LINE 718 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadExclamation = 207
{-# LINE 719 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadMemStore = 208
{-# LINE 720 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadMemRecall = 209
{-# LINE 721 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadMemClear = 210
{-# LINE 722 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadMemAdd = 211
{-# LINE 723 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadMemSubstract = 212
{-# LINE 724 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadMemMultiply = 213
{-# LINE 725 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadMemDivide = 214
{-# LINE 726 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadPlusMinus = 215
{-# LINE 727 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadClear = 216
{-# LINE 728 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadClearEntry = 217
{-# LINE 729 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadBinary = 218
{-# LINE 730 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadOctal = 219
{-# LINE 731 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadDecimal = 220
{-# LINE 732 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KeypadHexadecimal = 221
{-# LINE 733 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum LeftControl = 224
{-# LINE 734 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum LeftShift = 225
{-# LINE 735 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum LeftAlt = 226
{-# LINE 736 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum LeftGUI = 227
{-# LINE 737 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum RightControl = 228
{-# LINE 738 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum RightShift = 229
{-# LINE 739 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum RightAlt = 230
{-# LINE 740 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum RightGUI = 231
{-# LINE 741 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Mode = 257
{-# LINE 742 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum AudioNext = 258
{-# LINE 743 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum AudioPrevious = 259
{-# LINE 744 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum AudioStop = 260
{-# LINE 745 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum AudioPlay = 261
{-# LINE 746 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum AudioMute = 262
{-# LINE 747 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum MediaSelect = 263
{-# LINE 748 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum WWW = 264
{-# LINE 749 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Mail = 265
{-# LINE 750 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Calculator = 266
{-# LINE 751 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Computer = 267
{-# LINE 752 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ACSearch = 268
{-# LINE 753 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ACHome = 269
{-# LINE 754 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ACBack = 270
{-# LINE 755 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ACForward = 271
{-# LINE 756 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ACStop = 272
{-# LINE 757 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ACRefresh = 273
{-# LINE 758 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum ACBookmarks = 274
{-# LINE 759 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum BrightnessDown = 275
{-# LINE 760 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum BrightnessUp = 276
{-# LINE 761 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum DisplaySwitch = 277
{-# LINE 762 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KBIllumToggle = 278
{-# LINE 763 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KBIllumDown = 279
{-# LINE 764 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum KBIllumUp = 280
{-# LINE 765 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Eject = 281
{-# LINE 766 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum Sleep = 282
{-# LINE 767 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum App1 = 283
{-# LINE 768 "Graphics/UI/SDL/Keysym.hsc" #-}
  fromEnum App2 = 284
{-# LINE 769 "Graphics/UI/SDL/Keysym.hsc" #-}
