{-# LINE 1 "Graphics/UI/SDL/Keycode.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Keycode.hsc" #-}
module Graphics.UI.SDL.Keycode where

import Prelude hiding (Either(Left,Right))

data Keycode
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
  | Semicolon
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
  | Ampersand
  | Asterisk
  | At
  | Caret
  | Colon
  | Dollar
  | Exclaim
  | Greater
  | Hash
  | LeftParen
  | Less
  | Percent
  | Plus
  | Question
  | DoubleQuote
  | RightParen
  | Underscore
  | Unknown
  deriving (Eq, Ord, Show)

instance Enum Keycode where
  toEnum 97 = A
{-# LINE 245 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 98 = B
{-# LINE 246 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 99 = C
{-# LINE 247 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 100 = D
{-# LINE 248 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 101 = E
{-# LINE 249 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 102 = F
{-# LINE 250 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 103 = G
{-# LINE 251 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 104 = H
{-# LINE 252 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 105 = I
{-# LINE 253 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 106 = J
{-# LINE 254 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 107 = K
{-# LINE 255 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 108 = L
{-# LINE 256 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 109 = M
{-# LINE 257 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 110 = N
{-# LINE 258 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 111 = O
{-# LINE 259 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 112 = P
{-# LINE 260 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 113 = Q
{-# LINE 261 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 114 = R
{-# LINE 262 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 115 = S
{-# LINE 263 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 116 = T
{-# LINE 264 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 117 = U
{-# LINE 265 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 118 = V
{-# LINE 266 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 119 = W
{-# LINE 267 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 120 = X
{-# LINE 268 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 121 = Y
{-# LINE 269 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 122 = Z
{-# LINE 270 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 49 = Number1
{-# LINE 271 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 50 = Number2
{-# LINE 272 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 51 = Number3
{-# LINE 273 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 52 = Number4
{-# LINE 274 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 53 = Number5
{-# LINE 275 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 54 = Number6
{-# LINE 276 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 55 = Number7
{-# LINE 277 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 56 = Number8
{-# LINE 278 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 57 = Number9
{-# LINE 279 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 48 = Number0
{-# LINE 280 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 13 = Return
{-# LINE 281 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 27 = Escape
{-# LINE 282 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 8 = Backspace
{-# LINE 283 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 9 = Tab
{-# LINE 284 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 32 = Space
{-# LINE 285 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 45 = Minus
{-# LINE 286 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 61 = Equals
{-# LINE 287 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 91 = LeftBracket
{-# LINE 288 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 93 = RightBracket
{-# LINE 289 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 92 = Backslash
{-# LINE 290 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 59 = Semicolon
{-# LINE 291 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 44 = Comma
{-# LINE 292 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 46 = Period
{-# LINE 293 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 47 = Slash
{-# LINE 294 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741881 = Capslock
{-# LINE 295 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741882 = F1
{-# LINE 296 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741883 = F2
{-# LINE 297 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741884 = F3
{-# LINE 298 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741885 = F4
{-# LINE 299 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741886 = F5
{-# LINE 300 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741887 = F6
{-# LINE 301 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741888 = F7
{-# LINE 302 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741889 = F8
{-# LINE 303 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741890 = F9
{-# LINE 304 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741891 = F10
{-# LINE 305 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741892 = F11
{-# LINE 306 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741893 = F12
{-# LINE 307 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741894 = PrintScreen
{-# LINE 308 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741895 = ScrollLock
{-# LINE 309 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741896 = Pause
{-# LINE 310 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741897 = Insert
{-# LINE 311 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741898 = Home
{-# LINE 312 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741899 = PageUp
{-# LINE 313 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 127 = Delete
{-# LINE 314 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741901 = End
{-# LINE 315 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741902 = PageDown
{-# LINE 316 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741903 = Right
{-# LINE 317 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741904 = Left
{-# LINE 318 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741905 = Down
{-# LINE 319 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741906 = Up
{-# LINE 320 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741907 = NumLockClear
{-# LINE 321 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741908 = KeypadDivide
{-# LINE 322 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741909 = KeypadMultiply
{-# LINE 323 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741910 = KeypadMinus
{-# LINE 324 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741911 = KeypadPlus
{-# LINE 325 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741912 = KeypadEnter
{-# LINE 326 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741913 = Keypad1
{-# LINE 327 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741914 = Keypad2
{-# LINE 328 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741915 = Keypad3
{-# LINE 329 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741916 = Keypad4
{-# LINE 330 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741917 = Keypad5
{-# LINE 331 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741918 = Keypad6
{-# LINE 332 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741919 = Keypad7
{-# LINE 333 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741920 = Keypad8
{-# LINE 334 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741921 = Keypad9
{-# LINE 335 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741922 = Keypad0
{-# LINE 336 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741923 = KeypadPeriod
{-# LINE 337 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741925 = Application
{-# LINE 338 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741926 = Power
{-# LINE 339 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741927 = KeypadEquals
{-# LINE 340 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741928 = F13
{-# LINE 341 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741929 = F14
{-# LINE 342 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741930 = F15
{-# LINE 343 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741931 = F16
{-# LINE 344 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741932 = F17
{-# LINE 345 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741933 = F18
{-# LINE 346 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741934 = F19
{-# LINE 347 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741935 = F20
{-# LINE 348 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741936 = F21
{-# LINE 349 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741937 = F22
{-# LINE 350 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741938 = F23
{-# LINE 351 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741939 = F24
{-# LINE 352 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741940 = Execute
{-# LINE 353 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741941 = Help
{-# LINE 354 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741942 = Menu
{-# LINE 355 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741943 = Select
{-# LINE 356 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741944 = Stop
{-# LINE 357 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741945 = Again
{-# LINE 358 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741946 = Undo
{-# LINE 359 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741947 = Cut
{-# LINE 360 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741948 = Copy
{-# LINE 361 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741949 = Paste
{-# LINE 362 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741950 = Find
{-# LINE 363 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741951 = Mute
{-# LINE 364 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741952 = VolumeUp
{-# LINE 365 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741953 = VolumeDown
{-# LINE 366 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741957 = KeypadComma
{-# LINE 367 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741958 = KeyPadEqualsAs400
{-# LINE 368 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741977 = AltErase
{-# LINE 369 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741978 = SysReq
{-# LINE 370 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741979 = Cancel
{-# LINE 371 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741980 = Clear
{-# LINE 372 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741981 = Prior
{-# LINE 373 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741982 = Return2
{-# LINE 374 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741983 = Separator
{-# LINE 375 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741984 = Out
{-# LINE 376 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741985 = Oper
{-# LINE 377 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741986 = ClearAgain
{-# LINE 378 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741987 = CrSel
{-# LINE 379 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073741988 = ExSel
{-# LINE 380 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742000 = Keypad00
{-# LINE 381 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742001 = Keypad000
{-# LINE 382 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742002 = ThousandSeparator
{-# LINE 383 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742003 = DecimalSeparator
{-# LINE 384 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742004 = CurrencyUnit
{-# LINE 385 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742005 = CurrencySubunit
{-# LINE 386 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742006 = KeypadLeftParen
{-# LINE 387 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742007 = KeypadRightParen
{-# LINE 388 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742008 = KeypadLeftBrace
{-# LINE 389 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742009 = KeypadRightBrace
{-# LINE 390 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742010 = KeypadTab
{-# LINE 391 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742011 = KeypadBackspace
{-# LINE 392 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742012 = KeypadA
{-# LINE 393 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742013 = KeypadB
{-# LINE 394 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742014 = KeypadC
{-# LINE 395 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742015 = KeypadD
{-# LINE 396 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742016 = KeypadE
{-# LINE 397 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742017 = KeypadF
{-# LINE 398 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742018 = KeypadXOR
{-# LINE 399 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742019 = KeypadPower
{-# LINE 400 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742020 = KeypadPercent
{-# LINE 401 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742021 = KeypadLess
{-# LINE 402 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742022 = KeypadGreater
{-# LINE 403 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742023 = KeypadAmpersand
{-# LINE 404 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742024 = KeypadDoubleAmpersand
{-# LINE 405 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742025 = KeypadVerticalBar
{-# LINE 406 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742026 = KeypadDoubleVerticalBar
{-# LINE 407 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742027 = KeypadColon
{-# LINE 408 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742028 = KeypadHash
{-# LINE 409 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742029 = KeypadSpace
{-# LINE 410 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742030 = KeypadAt
{-# LINE 411 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742031 = KeypadExclamation
{-# LINE 412 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742032 = KeypadMemStore
{-# LINE 413 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742033 = KeypadMemRecall
{-# LINE 414 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742034 = KeypadMemClear
{-# LINE 415 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742035 = KeypadMemAdd
{-# LINE 416 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742036 = KeypadMemSubstract
{-# LINE 417 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742037 = KeypadMemMultiply
{-# LINE 418 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742038 = KeypadMemDivide
{-# LINE 419 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742039 = KeypadPlusMinus
{-# LINE 420 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742040 = KeypadClear
{-# LINE 421 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742041 = KeypadClearEntry
{-# LINE 422 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742042 = KeypadBinary
{-# LINE 423 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742043 = KeypadOctal
{-# LINE 424 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742044 = KeypadDecimal
{-# LINE 425 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742045 = KeypadHexadecimal
{-# LINE 426 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742048 = LeftControl
{-# LINE 427 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742049 = LeftShift
{-# LINE 428 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742050 = LeftAlt
{-# LINE 429 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742051 = LeftGUI
{-# LINE 430 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742052 = RightControl
{-# LINE 431 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742053 = RightShift
{-# LINE 432 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742054 = RightAlt
{-# LINE 433 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742055 = RightGUI
{-# LINE 434 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742081 = Mode
{-# LINE 435 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742082 = AudioNext
{-# LINE 436 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742083 = AudioPrevious
{-# LINE 437 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742084 = AudioStop
{-# LINE 438 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742085 = AudioPlay
{-# LINE 439 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742086 = AudioMute
{-# LINE 440 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742087 = MediaSelect
{-# LINE 441 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742088 = WWW
{-# LINE 442 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742089 = Mail
{-# LINE 443 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742090 = Calculator
{-# LINE 444 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742091 = Computer
{-# LINE 445 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742092 = ACSearch
{-# LINE 446 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742093 = ACHome
{-# LINE 447 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742094 = ACBack
{-# LINE 448 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742095 = ACForward
{-# LINE 449 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742096 = ACStop
{-# LINE 450 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742097 = ACRefresh
{-# LINE 451 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742098 = ACBookmarks
{-# LINE 452 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742099 = BrightnessDown
{-# LINE 453 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742100 = BrightnessUp
{-# LINE 454 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742101 = DisplaySwitch
{-# LINE 455 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742102 = KBIllumToggle
{-# LINE 456 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742103 = KBIllumDown
{-# LINE 457 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742104 = KBIllumUp
{-# LINE 458 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742105 = Eject
{-# LINE 459 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 1073742106 = Sleep
{-# LINE 460 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 38 = Ampersand
{-# LINE 461 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 42 = Asterisk
{-# LINE 462 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 64 = At
{-# LINE 463 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 94 = Caret
{-# LINE 464 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 58 = Colon
{-# LINE 465 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 36 = Dollar
{-# LINE 466 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 33 = Exclaim
{-# LINE 467 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 62 = Greater
{-# LINE 468 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 35 = Hash
{-# LINE 469 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 40 = LeftParen
{-# LINE 470 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 60 = Less
{-# LINE 471 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 37 = Percent
{-# LINE 472 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 43 = Plus
{-# LINE 473 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 63 = Question
{-# LINE 474 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 34 = DoubleQuote
{-# LINE 475 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 41 = RightParen
{-# LINE 476 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 95 = Underscore
{-# LINE 477 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum 0 = Unknown
{-# LINE 478 "Graphics/UI/SDL/Keycode.hsc" #-}
  toEnum _ = error "Keycode.toEnum: Invalid argument."

  fromEnum A = 97
{-# LINE 481 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum B = 98
{-# LINE 482 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum C = 99
{-# LINE 483 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum D = 100
{-# LINE 484 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum E = 101
{-# LINE 485 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F = 102
{-# LINE 486 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum G = 103
{-# LINE 487 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum H = 104
{-# LINE 488 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum I = 105
{-# LINE 489 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum J = 106
{-# LINE 490 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum K = 107
{-# LINE 491 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum L = 108
{-# LINE 492 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum M = 109
{-# LINE 493 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum N = 110
{-# LINE 494 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum O = 111
{-# LINE 495 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum P = 112
{-# LINE 496 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Q = 113
{-# LINE 497 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum R = 114
{-# LINE 498 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum S = 115
{-# LINE 499 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum T = 116
{-# LINE 500 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum U = 117
{-# LINE 501 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum V = 118
{-# LINE 502 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum W = 119
{-# LINE 503 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum X = 120
{-# LINE 504 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Y = 121
{-# LINE 505 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Z = 122
{-# LINE 506 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Number1 = 49
{-# LINE 507 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Number2 = 50
{-# LINE 508 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Number3 = 51
{-# LINE 509 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Number4 = 52
{-# LINE 510 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Number5 = 53
{-# LINE 511 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Number6 = 54
{-# LINE 512 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Number7 = 55
{-# LINE 513 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Number8 = 56
{-# LINE 514 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Number9 = 57
{-# LINE 515 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Number0 = 48
{-# LINE 516 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Return = 13
{-# LINE 517 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Escape = 27
{-# LINE 518 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Backspace = 8
{-# LINE 519 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Tab = 9
{-# LINE 520 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Space = 32
{-# LINE 521 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Minus = 45
{-# LINE 522 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Equals = 61
{-# LINE 523 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum LeftBracket = 91
{-# LINE 524 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum RightBracket = 93
{-# LINE 525 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Backslash = 92
{-# LINE 526 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Semicolon = 59
{-# LINE 527 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Comma = 44
{-# LINE 528 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Period = 46
{-# LINE 529 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Slash = 47
{-# LINE 530 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Capslock = 1073741881
{-# LINE 531 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F1 = 1073741882
{-# LINE 532 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F2 = 1073741883
{-# LINE 533 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F3 = 1073741884
{-# LINE 534 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F4 = 1073741885
{-# LINE 535 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F5 = 1073741886
{-# LINE 536 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F6 = 1073741887
{-# LINE 537 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F7 = 1073741888
{-# LINE 538 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F8 = 1073741889
{-# LINE 539 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F9 = 1073741890
{-# LINE 540 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F10 = 1073741891
{-# LINE 541 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F11 = 1073741892
{-# LINE 542 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F12 = 1073741893
{-# LINE 543 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum PrintScreen = 1073741894
{-# LINE 544 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ScrollLock = 1073741895
{-# LINE 545 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Pause = 1073741896
{-# LINE 546 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Insert = 1073741897
{-# LINE 547 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Home = 1073741898
{-# LINE 548 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum PageUp = 1073741899
{-# LINE 549 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Delete = 127
{-# LINE 550 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum End = 1073741901
{-# LINE 551 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum PageDown = 1073741902
{-# LINE 552 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Right = 1073741903
{-# LINE 553 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Left = 1073741904
{-# LINE 554 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Down = 1073741905
{-# LINE 555 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Up = 1073741906
{-# LINE 556 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum NumLockClear = 1073741907
{-# LINE 557 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadDivide = 1073741908
{-# LINE 558 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadMultiply = 1073741909
{-# LINE 559 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadMinus = 1073741910
{-# LINE 560 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadPlus = 1073741911
{-# LINE 561 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadEnter = 1073741912
{-# LINE 562 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad1 = 1073741913
{-# LINE 563 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad2 = 1073741914
{-# LINE 564 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad3 = 1073741915
{-# LINE 565 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad4 = 1073741916
{-# LINE 566 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad5 = 1073741917
{-# LINE 567 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad6 = 1073741918
{-# LINE 568 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad7 = 1073741919
{-# LINE 569 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad8 = 1073741920
{-# LINE 570 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad9 = 1073741921
{-# LINE 571 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad0 = 1073741922
{-# LINE 572 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadPeriod = 1073741923
{-# LINE 573 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Application = 1073741925
{-# LINE 574 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Power = 1073741926
{-# LINE 575 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadEquals = 1073741927
{-# LINE 576 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F13 = 1073741928
{-# LINE 577 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F14 = 1073741929
{-# LINE 578 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F15 = 1073741930
{-# LINE 579 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F16 = 1073741931
{-# LINE 580 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F17 = 1073741932
{-# LINE 581 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F18 = 1073741933
{-# LINE 582 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F19 = 1073741934
{-# LINE 583 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F20 = 1073741935
{-# LINE 584 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F21 = 1073741936
{-# LINE 585 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F22 = 1073741937
{-# LINE 586 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F23 = 1073741938
{-# LINE 587 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum F24 = 1073741939
{-# LINE 588 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Execute = 1073741940
{-# LINE 589 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Help = 1073741941
{-# LINE 590 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Menu = 1073741942
{-# LINE 591 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Select = 1073741943
{-# LINE 592 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Stop = 1073741944
{-# LINE 593 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Again = 1073741945
{-# LINE 594 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Undo = 1073741946
{-# LINE 595 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Cut = 1073741947
{-# LINE 596 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Copy = 1073741948
{-# LINE 597 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Paste = 1073741949
{-# LINE 598 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Find = 1073741950
{-# LINE 599 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Mute = 1073741951
{-# LINE 600 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum VolumeUp = 1073741952
{-# LINE 601 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum VolumeDown = 1073741953
{-# LINE 602 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadComma = 1073741957
{-# LINE 603 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeyPadEqualsAs400 = 1073741958
{-# LINE 604 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum AltErase = 1073741977
{-# LINE 605 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum SysReq = 1073741978
{-# LINE 606 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Cancel = 1073741979
{-# LINE 607 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Clear = 1073741980
{-# LINE 608 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Prior = 1073741981
{-# LINE 609 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Return2 = 1073741982
{-# LINE 610 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Separator = 1073741983
{-# LINE 611 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Out = 1073741984
{-# LINE 612 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Oper = 1073741985
{-# LINE 613 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ClearAgain = 1073741986
{-# LINE 614 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum CrSel = 1073741987
{-# LINE 615 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ExSel = 1073741988
{-# LINE 616 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad00 = 1073742000
{-# LINE 617 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Keypad000 = 1073742001
{-# LINE 618 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ThousandSeparator = 1073742002
{-# LINE 619 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum DecimalSeparator = 1073742003
{-# LINE 620 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum CurrencyUnit = 1073742004
{-# LINE 621 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum CurrencySubunit = 1073742005
{-# LINE 622 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadLeftParen = 1073742006
{-# LINE 623 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadRightParen = 1073742007
{-# LINE 624 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadLeftBrace = 1073742008
{-# LINE 625 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadRightBrace = 1073742009
{-# LINE 626 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadTab = 1073742010
{-# LINE 627 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadBackspace = 1073742011
{-# LINE 628 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadA = 1073742012
{-# LINE 629 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadB = 1073742013
{-# LINE 630 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadC = 1073742014
{-# LINE 631 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadD = 1073742015
{-# LINE 632 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadE = 1073742016
{-# LINE 633 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadF = 1073742017
{-# LINE 634 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadXOR = 1073742018
{-# LINE 635 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadPower = 1073742019
{-# LINE 636 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadPercent = 1073742020
{-# LINE 637 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadLess = 1073742021
{-# LINE 638 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadGreater = 1073742022
{-# LINE 639 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadAmpersand = 1073742023
{-# LINE 640 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadDoubleAmpersand = 1073742024
{-# LINE 641 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadVerticalBar = 1073742025
{-# LINE 642 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadDoubleVerticalBar = 1073742026
{-# LINE 643 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadColon = 1073742027
{-# LINE 644 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadHash = 1073742028
{-# LINE 645 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadSpace = 1073742029
{-# LINE 646 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadAt = 1073742030
{-# LINE 647 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadExclamation = 1073742031
{-# LINE 648 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadMemStore = 1073742032
{-# LINE 649 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadMemRecall = 1073742033
{-# LINE 650 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadMemClear = 1073742034
{-# LINE 651 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadMemAdd = 1073742035
{-# LINE 652 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadMemSubstract = 1073742036
{-# LINE 653 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadMemMultiply = 1073742037
{-# LINE 654 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadMemDivide = 1073742038
{-# LINE 655 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadPlusMinus = 1073742039
{-# LINE 656 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadClear = 1073742040
{-# LINE 657 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadClearEntry = 1073742041
{-# LINE 658 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadBinary = 1073742042
{-# LINE 659 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadOctal = 1073742043
{-# LINE 660 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadDecimal = 1073742044
{-# LINE 661 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KeypadHexadecimal = 1073742045
{-# LINE 662 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum LeftControl = 1073742048
{-# LINE 663 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum LeftShift = 1073742049
{-# LINE 664 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum LeftAlt = 1073742050
{-# LINE 665 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum LeftGUI = 1073742051
{-# LINE 666 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum RightControl = 1073742052
{-# LINE 667 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum RightShift = 1073742053
{-# LINE 668 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum RightAlt = 1073742054
{-# LINE 669 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum RightGUI = 1073742055
{-# LINE 670 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Mode = 1073742081
{-# LINE 671 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum AudioNext = 1073742082
{-# LINE 672 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum AudioPrevious = 1073742083
{-# LINE 673 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum AudioStop = 1073742084
{-# LINE 674 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum AudioPlay = 1073742085
{-# LINE 675 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum AudioMute = 1073742086
{-# LINE 676 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum MediaSelect = 1073742087
{-# LINE 677 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum WWW = 1073742088
{-# LINE 678 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Mail = 1073742089
{-# LINE 679 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Calculator = 1073742090
{-# LINE 680 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Computer = 1073742091
{-# LINE 681 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ACSearch = 1073742092
{-# LINE 682 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ACHome = 1073742093
{-# LINE 683 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ACBack = 1073742094
{-# LINE 684 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ACForward = 1073742095
{-# LINE 685 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ACStop = 1073742096
{-# LINE 686 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ACRefresh = 1073742097
{-# LINE 687 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum ACBookmarks = 1073742098
{-# LINE 688 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum BrightnessDown = 1073742099
{-# LINE 689 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum BrightnessUp = 1073742100
{-# LINE 690 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum DisplaySwitch = 1073742101
{-# LINE 691 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KBIllumToggle = 1073742102
{-# LINE 692 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KBIllumDown = 1073742103
{-# LINE 693 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum KBIllumUp = 1073742104
{-# LINE 694 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Eject = 1073742105
{-# LINE 695 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Sleep = 1073742106
{-# LINE 696 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Ampersand = 38
{-# LINE 697 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Asterisk = 42
{-# LINE 698 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum At = 64
{-# LINE 699 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Caret = 94
{-# LINE 700 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Colon = 58
{-# LINE 701 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Dollar = 36
{-# LINE 702 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Exclaim = 33
{-# LINE 703 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Greater = 62
{-# LINE 704 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Hash = 35
{-# LINE 705 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum LeftParen = 40
{-# LINE 706 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Less = 60
{-# LINE 707 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Percent = 37
{-# LINE 708 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Plus = 43
{-# LINE 709 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Question = 63
{-# LINE 710 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum DoubleQuote = 34
{-# LINE 711 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum RightParen = 41
{-# LINE 712 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Underscore = 95
{-# LINE 713 "Graphics/UI/SDL/Keycode.hsc" #-}
  fromEnum Unknown = 0
{-# LINE 714 "Graphics/UI/SDL/Keycode.hsc" #-}
