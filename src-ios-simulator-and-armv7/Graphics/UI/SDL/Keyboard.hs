{-# LINE 1 "Graphics/UI/SDL/Keyboard.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Keyboard.hsc" #-}
module Graphics.UI.SDL.Keyboard
  ( Keymod(..)
  , Scancode(..)
  , getKeyboardFocus
  , getModState
  , setModState
  , getKeyFromScancode
  , getScancodeFromKey
  , getScancodeName
  , getScancodeFromName
  , getKeyName
  , getKeyFromName
  , startTextInput
  , isTextInputActive
  , stopTextInput
  , setTextInputRect
  , hasScreenKeyboardSupport
  , isScreenKeyboardShown
  ) where

import Foreign
import Foreign.C.String
import Control.Applicative
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.Raw
import Graphics.UI.SDL.Keycode

data Scancode
   = ScancodeUNKNOWN
   -- alpha
   | ScancodeA | ScancodeB | ScancodeC | ScancodeD | ScancodeE | ScancodeF
   | ScancodeG | ScancodeH | ScancodeI | ScancodeJ | ScancodeK | ScancodeL
   | ScancodeM | ScancodeN | ScancodeO | ScancodeP | ScancodeQ | ScancodeR
   | ScancodeS | ScancodeT | ScancodeU | ScancodeV | ScancodeW | ScancodeX
   | ScancodeY | ScancodeZ
   -- numeric
   | Scancode1 | Scancode2 | Scancode3 | Scancode4 | Scancode5 | Scancode6
   | Scancode7 | Scancode8 | Scancode9 | Scancode0
   -- notation
   | ScancodeReturn | ScancodeEscape | ScancodeBackspace | ScancodeTab
   | ScancodeSpace | ScancodeMinus | ScancodeEquals | ScancodeLeftbracket
   | ScancodeRightbracket | ScancodeBackslash | ScancodeNonushash
   | ScancodeSemicolon | ScancodeApostrophe | ScancodeGrave | ScancodeComma
   | ScancodePeriod | ScancodeSlash | ScancodeCapslock
   -- function keys
   | ScancodeF1 | ScancodeF2 | ScancodeF3 | ScancodeF4 | ScancodeF5
   | ScancodeF6 | ScancodeF7 | ScancodeF8 | ScancodeF9 | ScancodeF10
   | ScancodeF11 | ScancodeF12
   -- shortcut keys
   | ScancodePrintscreen | ScancodeScrolllock | ScancodePause | ScancodeInsert
   | ScancodeHome | ScancodePageup | ScancodeDelete | ScancodeEnd
   | ScancodePagedown | ScancodeRight | ScancodeLeft | ScancodeDown | ScancodeUp
   | ScancodeNumlockclear | ScancodeKPDivide | ScancodeKPMultiply
   | ScancodeKPMinus | ScancodeKPplus | ScancodeKPEnter
   | ScancodeKP1 | ScancodeKP2 | ScancodeKP3 | ScancodeKP4 | ScancodeKP5
   | ScancodeKP6 | ScancodeKP7 | ScancodeKP8 | ScancodeKP9 | ScancodeKP0
   | ScancodeKPPeriod | ScancodeNonusbackslash | ScancodeApplication
   | ScancodePower | ScancodeKPEquals
   -- extended function keys
   | ScancodeF13 | ScancodeF14 | ScancodeF15 | ScancodeF16 | ScancodeF17
   | ScancodeF18 | ScancodeF19 | ScancodeF20 | ScancodeF21 | ScancodeF22
   | ScancodeF23 | ScancodeF24
   -- extended shortcut keys
   | ScancodeExecute | ScancodeHelp | ScancodeMenu | ScancodeSelect
   | ScancodeStop | ScancodeAgain | ScancodeUndo | ScancodeCut | ScancodeCopy
   | ScancodePaste | ScancodeFind | ScancodeMute | ScancodeVolumeup
   | ScancodeVolumedown | ScancodeKPComma | ScancodeKPEqualsas400
   -- international
   | ScancodeInternational1 | ScancodeInternational2 | ScancodeInternational3
   | ScancodeInternational4 | ScancodeInternational5 | ScancodeInternational6
   | ScancodeInternational7 | ScancodeInternational8 | ScancodeInternational9
   -- languages
   | ScancodeLang1 | ScancodeLang2 | ScancodeLang3 | ScancodeLang4
   | ScancodeLang5 | ScancodeLang6 | ScancodeLang7 | ScancodeLang8
   | ScancodeLang9
   -- keypad/calc
   | ScancodeAlterase | ScancodeSysreq | ScancodeCancel | ScancodeClear
   | ScancodePrior | ScancodeReturn2 | ScancodeSeparator | ScancodeOut
   | ScancodeOper | ScancodeClearagain | ScancodeCrsel | ScancodeExsel
   | ScancodeKP00 | ScancodeKP000
   | ScancodeThousandsseparator | ScancodeDecimalseparator | ScancodeCurrencyunit
   | ScancodeCurrencysubunit | ScancodeKPLeftparen | ScancodeKPRightparen
   | ScancodeKPLeftbrace | ScancodeKPRightbrace | ScancodeKPTab | ScancodeKPBackspace
   | ScancodeKPA | ScancodeKPB | ScancodeKPC | ScancodeKPD | ScancodeKPE | ScancodeKPF
   | ScancodeKPXor | ScancodeKPPower | ScancodeKPPercent | ScancodeKPLess
   | ScancodeKPGreater | ScancodeKPAmpersand | ScancodeKPDblampersand
   | ScancodeKPVerticalbar | ScancodeKPDblverticalbar | ScancodeKPColon
   | ScancodeKPHash | ScancodeKPSpace | ScancodeKPAt | ScancodeKPExclam
   | ScancodeKPMemstore | ScancodeKPMemrecall | ScancodeKPMemclear | ScancodeKPMemadd
   | ScancodeKPMemsubtract | ScancodeKPMemmultiply | ScancodeKPMemdivide
   | ScancodeKPPlusminus | ScancodeKPClear | ScancodeKPClearentry | ScancodeKPBinary
   | ScancodeKPOctal | ScancodeKPDecimal | ScancodeKPHexadecimal
   | ScancodeLCtrl | ScancodeLShift | ScancodeLAlt | ScancodeLGUI | ScancodeRCtrl
   | ScancodeRShift | ScancodeRAlt | ScancodeRGUI | ScancodeMode
   -- audio/media keys
   | ScancodeAudionext | ScancodeAudioprev | ScancodeAudiostop
   | ScancodeAudioplay | ScancodeAudiomute | ScancodeMediaselect
   | ScancodeWWW | ScancodeMail | ScancodeCalculator | ScancodeComputer
   | ScancodeACSearch | ScancodeACHome | ScancodeACBack
   | ScancodeACForward | ScancodeACStop | ScancodeACRefresh | ScancodeACBookmarks
   -- mac
   | ScancodeBrightnessDown | ScancodeBrightnessUp | ScancodeDisplaySwitch
   | ScancodeKBDILLUMTOGGLE | ScancodeKBDIlluMDown | ScancodeKBDIllumUp
   | ScancodeEject | ScancodeSleep | ScancodeApp1 | ScancodeApp2
   deriving (Eq, Show)

instance Enum Scancode where
  fromEnum x =
    case x of
      ScancodeUNKNOWN -> 0
{-# LINE 114 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeA -> 4
{-# LINE 115 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeB -> 5
{-# LINE 116 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeC -> 6
{-# LINE 117 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeD -> 7
{-# LINE 118 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeE -> 8
{-# LINE 119 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF -> 9
{-# LINE 120 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeG -> 10
{-# LINE 121 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeH -> 11
{-# LINE 122 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeI -> 12
{-# LINE 123 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeJ -> 13
{-# LINE 124 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeK -> 14
{-# LINE 125 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeL -> 15
{-# LINE 126 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeM -> 16
{-# LINE 127 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeN -> 17
{-# LINE 128 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeO -> 18
{-# LINE 129 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeP -> 19
{-# LINE 130 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeQ -> 20
{-# LINE 131 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeR -> 21
{-# LINE 132 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeS -> 22
{-# LINE 133 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeT -> 23
{-# LINE 134 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeU -> 24
{-# LINE 135 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeV -> 25
{-# LINE 136 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeW -> 26
{-# LINE 137 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeX -> 27
{-# LINE 138 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeY -> 28
{-# LINE 139 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeZ -> 29
{-# LINE 140 "Graphics/UI/SDL/Keyboard.hsc" #-}
      Scancode1 -> 30
{-# LINE 141 "Graphics/UI/SDL/Keyboard.hsc" #-}
      Scancode2 -> 31
{-# LINE 142 "Graphics/UI/SDL/Keyboard.hsc" #-}
      Scancode3 -> 32
{-# LINE 143 "Graphics/UI/SDL/Keyboard.hsc" #-}
      Scancode4 -> 33
{-# LINE 144 "Graphics/UI/SDL/Keyboard.hsc" #-}
      Scancode5 -> 34
{-# LINE 145 "Graphics/UI/SDL/Keyboard.hsc" #-}
      Scancode6 -> 35
{-# LINE 146 "Graphics/UI/SDL/Keyboard.hsc" #-}
      Scancode7 -> 36
{-# LINE 147 "Graphics/UI/SDL/Keyboard.hsc" #-}
      Scancode8 -> 37
{-# LINE 148 "Graphics/UI/SDL/Keyboard.hsc" #-}
      Scancode9 -> 38
{-# LINE 149 "Graphics/UI/SDL/Keyboard.hsc" #-}
      Scancode0 -> 39
{-# LINE 150 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeReturn -> 40
{-# LINE 151 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeEscape -> 41
{-# LINE 152 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeBackspace -> 42
{-# LINE 153 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeTab -> 43
{-# LINE 154 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeSpace -> 44
{-# LINE 155 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeMinus -> 45
{-# LINE 156 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeEquals -> 46
{-# LINE 157 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLeftbracket -> 47
{-# LINE 158 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeRightbracket -> 48
{-# LINE 159 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeBackslash -> 49
{-# LINE 160 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeNonushash -> 50
{-# LINE 161 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeSemicolon -> 51
{-# LINE 162 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeApostrophe -> 52
{-# LINE 163 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeGrave -> 53
{-# LINE 164 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeComma -> 54
{-# LINE 165 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodePeriod -> 55
{-# LINE 166 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeSlash -> 56
{-# LINE 167 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeCapslock -> 57
{-# LINE 168 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF1 -> 58
{-# LINE 169 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF2 -> 59
{-# LINE 170 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF3 -> 60
{-# LINE 171 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF4 -> 61
{-# LINE 172 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF5 -> 62
{-# LINE 173 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF6 -> 63
{-# LINE 174 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF7 -> 64
{-# LINE 175 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF8 -> 65
{-# LINE 176 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF9 -> 66
{-# LINE 177 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF10 -> 67
{-# LINE 178 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF11 -> 68
{-# LINE 179 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF12 -> 69
{-# LINE 180 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodePrintscreen -> 70
{-# LINE 181 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeScrolllock -> 71
{-# LINE 182 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodePause -> 72
{-# LINE 183 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeInsert -> 73
{-# LINE 184 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeHome -> 74
{-# LINE 185 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodePageup -> 75
{-# LINE 186 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeDelete -> 76
{-# LINE 187 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeEnd -> 77
{-# LINE 188 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodePagedown -> 78
{-# LINE 189 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeRight -> 79
{-# LINE 190 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLeft -> 80
{-# LINE 191 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeDown -> 81
{-# LINE 192 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeUp -> 82
{-# LINE 193 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeNumlockclear -> 83
{-# LINE 194 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPDivide -> 84
{-# LINE 195 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPMultiply -> 85
{-# LINE 196 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPMinus -> 86
{-# LINE 197 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPplus -> 87
{-# LINE 198 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPEnter -> 88
{-# LINE 199 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP1 -> 89
{-# LINE 200 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP2 -> 90
{-# LINE 201 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP3 -> 91
{-# LINE 202 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP4 -> 92
{-# LINE 203 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP5 -> 93
{-# LINE 204 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP6 -> 94
{-# LINE 205 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP7 -> 95
{-# LINE 206 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP8 -> 96
{-# LINE 207 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP9 -> 97
{-# LINE 208 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP0 -> 98
{-# LINE 209 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPPeriod -> 99
{-# LINE 210 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeNonusbackslash -> 100
{-# LINE 211 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeApplication -> 101
{-# LINE 212 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodePower -> 102
{-# LINE 213 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPEquals -> 103
{-# LINE 214 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF13 -> 104
{-# LINE 215 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF14 -> 105
{-# LINE 216 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF15 -> 106
{-# LINE 217 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF16 -> 107
{-# LINE 218 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF17 -> 108
{-# LINE 219 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF18 -> 109
{-# LINE 220 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF19 -> 110
{-# LINE 221 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF20 -> 111
{-# LINE 222 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF21 -> 112
{-# LINE 223 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF22 -> 113
{-# LINE 224 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF23 -> 114
{-# LINE 225 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeF24 -> 115
{-# LINE 226 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeExecute -> 116
{-# LINE 227 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeHelp -> 117
{-# LINE 228 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeMenu -> 118
{-# LINE 229 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeSelect -> 119
{-# LINE 230 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeStop -> 120
{-# LINE 231 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeAgain -> 121
{-# LINE 232 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeUndo -> 122
{-# LINE 233 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeCut -> 123
{-# LINE 234 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeCopy -> 124
{-# LINE 235 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodePaste -> 125
{-# LINE 236 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeFind -> 126
{-# LINE 237 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeMute -> 127
{-# LINE 238 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeVolumeup -> 128
{-# LINE 239 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeVolumedown -> 129
{-# LINE 240 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPComma -> 133
{-# LINE 241 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPEqualsas400 -> 134
{-# LINE 242 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeInternational1 -> 135
{-# LINE 243 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeInternational2 -> 136
{-# LINE 244 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeInternational3 -> 137
{-# LINE 245 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeInternational4 -> 138
{-# LINE 246 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeInternational5 -> 139
{-# LINE 247 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeInternational6 -> 140
{-# LINE 248 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeInternational7 -> 141
{-# LINE 249 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeInternational8 -> 142
{-# LINE 250 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeInternational9 -> 143
{-# LINE 251 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLang1 -> 144
{-# LINE 252 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLang2 -> 145
{-# LINE 253 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLang3 -> 146
{-# LINE 254 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLang4 -> 147
{-# LINE 255 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLang5 -> 148
{-# LINE 256 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLang6 -> 149
{-# LINE 257 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLang7 -> 150
{-# LINE 258 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLang8 -> 151
{-# LINE 259 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLang9 -> 152
{-# LINE 260 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeAlterase -> 153
{-# LINE 261 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeSysreq -> 154
{-# LINE 262 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeCancel -> 155
{-# LINE 263 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeClear -> 156
{-# LINE 264 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodePrior -> 157
{-# LINE 265 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeReturn2 -> 158
{-# LINE 266 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeSeparator -> 159
{-# LINE 267 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeOut -> 160
{-# LINE 268 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeOper -> 161
{-# LINE 269 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeClearagain -> 162
{-# LINE 270 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeCrsel -> 163
{-# LINE 271 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeExsel -> 164
{-# LINE 272 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP00 -> 176
{-# LINE 273 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKP000 -> 177
{-# LINE 274 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeThousandsseparator -> 178
{-# LINE 275 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeDecimalseparator -> 179
{-# LINE 276 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeCurrencyunit -> 180
{-# LINE 277 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeCurrencysubunit -> 181
{-# LINE 278 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPLeftparen -> 182
{-# LINE 279 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPRightparen -> 183
{-# LINE 280 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPLeftbrace -> 184
{-# LINE 281 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPRightbrace -> 185
{-# LINE 282 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPTab -> 186
{-# LINE 283 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPBackspace -> 187
{-# LINE 284 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPA -> 188
{-# LINE 285 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPB -> 189
{-# LINE 286 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPC -> 190
{-# LINE 287 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPD -> 191
{-# LINE 288 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPE -> 192
{-# LINE 289 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPF -> 193
{-# LINE 290 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPXor -> 194
{-# LINE 291 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPPower -> 195
{-# LINE 292 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPPercent -> 196
{-# LINE 293 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPLess -> 197
{-# LINE 294 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPGreater -> 198
{-# LINE 295 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPAmpersand -> 199
{-# LINE 296 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPDblampersand -> 200
{-# LINE 297 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPVerticalbar -> 201
{-# LINE 298 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPDblverticalbar -> 202
{-# LINE 299 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPColon -> 203
{-# LINE 300 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPHash -> 204
{-# LINE 301 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPSpace -> 205
{-# LINE 302 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPAt -> 206
{-# LINE 303 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPExclam -> 207
{-# LINE 304 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPMemstore -> 208
{-# LINE 305 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPMemrecall -> 209
{-# LINE 306 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPMemclear -> 210
{-# LINE 307 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPMemadd -> 211
{-# LINE 308 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPMemsubtract -> 212
{-# LINE 309 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPMemmultiply -> 213
{-# LINE 310 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPMemdivide -> 214
{-# LINE 311 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPPlusminus -> 215
{-# LINE 312 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPClear -> 216
{-# LINE 313 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPClearentry -> 217
{-# LINE 314 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPBinary -> 218
{-# LINE 315 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPOctal -> 219
{-# LINE 316 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPDecimal -> 220
{-# LINE 317 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKPHexadecimal -> 221
{-# LINE 318 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLCtrl -> 224
{-# LINE 319 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLShift -> 225
{-# LINE 320 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLAlt -> 226
{-# LINE 321 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeLGUI -> 227
{-# LINE 322 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeRCtrl -> 228
{-# LINE 323 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeRShift -> 229
{-# LINE 324 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeRAlt -> 230
{-# LINE 325 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeRGUI -> 231
{-# LINE 326 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeMode -> 257
{-# LINE 327 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeAudionext -> 258
{-# LINE 328 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeAudioprev -> 259
{-# LINE 329 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeAudiostop -> 260
{-# LINE 330 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeAudioplay -> 261
{-# LINE 331 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeAudiomute -> 262
{-# LINE 332 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeMediaselect -> 263
{-# LINE 333 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeWWW -> 264
{-# LINE 334 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeMail -> 265
{-# LINE 335 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeCalculator -> 266
{-# LINE 336 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeComputer -> 267
{-# LINE 337 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeACSearch -> 268
{-# LINE 338 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeACHome -> 269
{-# LINE 339 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeACBack -> 270
{-# LINE 340 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeACForward -> 271
{-# LINE 341 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeACStop -> 272
{-# LINE 342 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeACRefresh -> 273
{-# LINE 343 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeACBookmarks -> 274
{-# LINE 344 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeBrightnessDown -> 275
{-# LINE 345 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeBrightnessUp -> 276
{-# LINE 346 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeDisplaySwitch -> 277
{-# LINE 347 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKBDILLUMTOGGLE -> 278
{-# LINE 348 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKBDIlluMDown -> 279
{-# LINE 349 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeKBDIllumUp -> 280
{-# LINE 350 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeEject -> 281
{-# LINE 351 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeSleep -> 282
{-# LINE 352 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeApp1 -> 283
{-# LINE 353 "Graphics/UI/SDL/Keyboard.hsc" #-}
      ScancodeApp2 -> 284
{-# LINE 354 "Graphics/UI/SDL/Keyboard.hsc" #-}

  toEnum x =
    case x of
      0 -> ScancodeUNKNOWN
{-# LINE 358 "Graphics/UI/SDL/Keyboard.hsc" #-}
      4 -> ScancodeA
{-# LINE 359 "Graphics/UI/SDL/Keyboard.hsc" #-}
      5 -> ScancodeB
{-# LINE 360 "Graphics/UI/SDL/Keyboard.hsc" #-}
      6 -> ScancodeC
{-# LINE 361 "Graphics/UI/SDL/Keyboard.hsc" #-}
      7 -> ScancodeD
{-# LINE 362 "Graphics/UI/SDL/Keyboard.hsc" #-}
      8 -> ScancodeE
{-# LINE 363 "Graphics/UI/SDL/Keyboard.hsc" #-}
      9 -> ScancodeF
{-# LINE 364 "Graphics/UI/SDL/Keyboard.hsc" #-}
      10 -> ScancodeG
{-# LINE 365 "Graphics/UI/SDL/Keyboard.hsc" #-}
      11 -> ScancodeH
{-# LINE 366 "Graphics/UI/SDL/Keyboard.hsc" #-}
      12 -> ScancodeI
{-# LINE 367 "Graphics/UI/SDL/Keyboard.hsc" #-}
      13 -> ScancodeJ
{-# LINE 368 "Graphics/UI/SDL/Keyboard.hsc" #-}
      14 -> ScancodeK
{-# LINE 369 "Graphics/UI/SDL/Keyboard.hsc" #-}
      15 -> ScancodeL
{-# LINE 370 "Graphics/UI/SDL/Keyboard.hsc" #-}
      16 -> ScancodeM
{-# LINE 371 "Graphics/UI/SDL/Keyboard.hsc" #-}
      17 -> ScancodeN
{-# LINE 372 "Graphics/UI/SDL/Keyboard.hsc" #-}
      18 -> ScancodeO
{-# LINE 373 "Graphics/UI/SDL/Keyboard.hsc" #-}
      19 -> ScancodeP
{-# LINE 374 "Graphics/UI/SDL/Keyboard.hsc" #-}
      20 -> ScancodeQ
{-# LINE 375 "Graphics/UI/SDL/Keyboard.hsc" #-}
      21 -> ScancodeR
{-# LINE 376 "Graphics/UI/SDL/Keyboard.hsc" #-}
      22 -> ScancodeS
{-# LINE 377 "Graphics/UI/SDL/Keyboard.hsc" #-}
      23 -> ScancodeT
{-# LINE 378 "Graphics/UI/SDL/Keyboard.hsc" #-}
      24 -> ScancodeU
{-# LINE 379 "Graphics/UI/SDL/Keyboard.hsc" #-}
      25 -> ScancodeV
{-# LINE 380 "Graphics/UI/SDL/Keyboard.hsc" #-}
      26 -> ScancodeW
{-# LINE 381 "Graphics/UI/SDL/Keyboard.hsc" #-}
      27 -> ScancodeX
{-# LINE 382 "Graphics/UI/SDL/Keyboard.hsc" #-}
      28 -> ScancodeY
{-# LINE 383 "Graphics/UI/SDL/Keyboard.hsc" #-}
      29 -> ScancodeZ
{-# LINE 384 "Graphics/UI/SDL/Keyboard.hsc" #-}
      30 -> Scancode1
{-# LINE 385 "Graphics/UI/SDL/Keyboard.hsc" #-}
      31 -> Scancode2
{-# LINE 386 "Graphics/UI/SDL/Keyboard.hsc" #-}
      32 -> Scancode3
{-# LINE 387 "Graphics/UI/SDL/Keyboard.hsc" #-}
      33 -> Scancode4
{-# LINE 388 "Graphics/UI/SDL/Keyboard.hsc" #-}
      34 -> Scancode5
{-# LINE 389 "Graphics/UI/SDL/Keyboard.hsc" #-}
      35 -> Scancode6
{-# LINE 390 "Graphics/UI/SDL/Keyboard.hsc" #-}
      36 -> Scancode7
{-# LINE 391 "Graphics/UI/SDL/Keyboard.hsc" #-}
      37 -> Scancode8
{-# LINE 392 "Graphics/UI/SDL/Keyboard.hsc" #-}
      38 -> Scancode9
{-# LINE 393 "Graphics/UI/SDL/Keyboard.hsc" #-}
      39 -> Scancode0
{-# LINE 394 "Graphics/UI/SDL/Keyboard.hsc" #-}
      40 -> ScancodeReturn
{-# LINE 395 "Graphics/UI/SDL/Keyboard.hsc" #-}
      41 -> ScancodeEscape
{-# LINE 396 "Graphics/UI/SDL/Keyboard.hsc" #-}
      42 -> ScancodeBackspace
{-# LINE 397 "Graphics/UI/SDL/Keyboard.hsc" #-}
      43 -> ScancodeTab
{-# LINE 398 "Graphics/UI/SDL/Keyboard.hsc" #-}
      44 -> ScancodeSpace
{-# LINE 399 "Graphics/UI/SDL/Keyboard.hsc" #-}
      45 -> ScancodeMinus
{-# LINE 400 "Graphics/UI/SDL/Keyboard.hsc" #-}
      46 -> ScancodeEquals
{-# LINE 401 "Graphics/UI/SDL/Keyboard.hsc" #-}
      47 -> ScancodeLeftbracket
{-# LINE 402 "Graphics/UI/SDL/Keyboard.hsc" #-}
      48 -> ScancodeRightbracket
{-# LINE 403 "Graphics/UI/SDL/Keyboard.hsc" #-}
      49 -> ScancodeBackslash
{-# LINE 404 "Graphics/UI/SDL/Keyboard.hsc" #-}
      50 -> ScancodeNonushash
{-# LINE 405 "Graphics/UI/SDL/Keyboard.hsc" #-}
      51 -> ScancodeSemicolon
{-# LINE 406 "Graphics/UI/SDL/Keyboard.hsc" #-}
      52 -> ScancodeApostrophe
{-# LINE 407 "Graphics/UI/SDL/Keyboard.hsc" #-}
      53 -> ScancodeGrave
{-# LINE 408 "Graphics/UI/SDL/Keyboard.hsc" #-}
      54 -> ScancodeComma
{-# LINE 409 "Graphics/UI/SDL/Keyboard.hsc" #-}
      55 -> ScancodePeriod
{-# LINE 410 "Graphics/UI/SDL/Keyboard.hsc" #-}
      56 -> ScancodeSlash
{-# LINE 411 "Graphics/UI/SDL/Keyboard.hsc" #-}
      57 -> ScancodeCapslock
{-# LINE 412 "Graphics/UI/SDL/Keyboard.hsc" #-}
      58 -> ScancodeF1
{-# LINE 413 "Graphics/UI/SDL/Keyboard.hsc" #-}
      59 -> ScancodeF2
{-# LINE 414 "Graphics/UI/SDL/Keyboard.hsc" #-}
      60 -> ScancodeF3
{-# LINE 415 "Graphics/UI/SDL/Keyboard.hsc" #-}
      61 -> ScancodeF4
{-# LINE 416 "Graphics/UI/SDL/Keyboard.hsc" #-}
      62 -> ScancodeF5
{-# LINE 417 "Graphics/UI/SDL/Keyboard.hsc" #-}
      63 -> ScancodeF6
{-# LINE 418 "Graphics/UI/SDL/Keyboard.hsc" #-}
      64 -> ScancodeF7
{-# LINE 419 "Graphics/UI/SDL/Keyboard.hsc" #-}
      65 -> ScancodeF8
{-# LINE 420 "Graphics/UI/SDL/Keyboard.hsc" #-}
      66 -> ScancodeF9
{-# LINE 421 "Graphics/UI/SDL/Keyboard.hsc" #-}
      67 -> ScancodeF10
{-# LINE 422 "Graphics/UI/SDL/Keyboard.hsc" #-}
      68 -> ScancodeF11
{-# LINE 423 "Graphics/UI/SDL/Keyboard.hsc" #-}
      69 -> ScancodeF12
{-# LINE 424 "Graphics/UI/SDL/Keyboard.hsc" #-}
      70 -> ScancodePrintscreen
{-# LINE 425 "Graphics/UI/SDL/Keyboard.hsc" #-}
      71 -> ScancodeScrolllock
{-# LINE 426 "Graphics/UI/SDL/Keyboard.hsc" #-}
      72 -> ScancodePause
{-# LINE 427 "Graphics/UI/SDL/Keyboard.hsc" #-}
      73 -> ScancodeInsert
{-# LINE 428 "Graphics/UI/SDL/Keyboard.hsc" #-}
      74 -> ScancodeHome
{-# LINE 429 "Graphics/UI/SDL/Keyboard.hsc" #-}
      75 -> ScancodePageup
{-# LINE 430 "Graphics/UI/SDL/Keyboard.hsc" #-}
      76 -> ScancodeDelete
{-# LINE 431 "Graphics/UI/SDL/Keyboard.hsc" #-}
      77 -> ScancodeEnd
{-# LINE 432 "Graphics/UI/SDL/Keyboard.hsc" #-}
      78 -> ScancodePagedown
{-# LINE 433 "Graphics/UI/SDL/Keyboard.hsc" #-}
      79 -> ScancodeRight
{-# LINE 434 "Graphics/UI/SDL/Keyboard.hsc" #-}
      80 -> ScancodeLeft
{-# LINE 435 "Graphics/UI/SDL/Keyboard.hsc" #-}
      81 -> ScancodeDown
{-# LINE 436 "Graphics/UI/SDL/Keyboard.hsc" #-}
      82 -> ScancodeUp
{-# LINE 437 "Graphics/UI/SDL/Keyboard.hsc" #-}
      83 -> ScancodeNumlockclear
{-# LINE 438 "Graphics/UI/SDL/Keyboard.hsc" #-}
      84 -> ScancodeKPDivide
{-# LINE 439 "Graphics/UI/SDL/Keyboard.hsc" #-}
      85 -> ScancodeKPMultiply
{-# LINE 440 "Graphics/UI/SDL/Keyboard.hsc" #-}
      86 -> ScancodeKPMinus
{-# LINE 441 "Graphics/UI/SDL/Keyboard.hsc" #-}
      87 -> ScancodeKPplus
{-# LINE 442 "Graphics/UI/SDL/Keyboard.hsc" #-}
      88 -> ScancodeKPEnter
{-# LINE 443 "Graphics/UI/SDL/Keyboard.hsc" #-}
      89 -> ScancodeKP1
{-# LINE 444 "Graphics/UI/SDL/Keyboard.hsc" #-}
      90 -> ScancodeKP2
{-# LINE 445 "Graphics/UI/SDL/Keyboard.hsc" #-}
      91 -> ScancodeKP3
{-# LINE 446 "Graphics/UI/SDL/Keyboard.hsc" #-}
      92 -> ScancodeKP4
{-# LINE 447 "Graphics/UI/SDL/Keyboard.hsc" #-}
      93 -> ScancodeKP5
{-# LINE 448 "Graphics/UI/SDL/Keyboard.hsc" #-}
      94 -> ScancodeKP6
{-# LINE 449 "Graphics/UI/SDL/Keyboard.hsc" #-}
      95 -> ScancodeKP7
{-# LINE 450 "Graphics/UI/SDL/Keyboard.hsc" #-}
      96 -> ScancodeKP8
{-# LINE 451 "Graphics/UI/SDL/Keyboard.hsc" #-}
      97 -> ScancodeKP9
{-# LINE 452 "Graphics/UI/SDL/Keyboard.hsc" #-}
      98 -> ScancodeKP0
{-# LINE 453 "Graphics/UI/SDL/Keyboard.hsc" #-}
      99 -> ScancodeKPPeriod
{-# LINE 454 "Graphics/UI/SDL/Keyboard.hsc" #-}
      100 -> ScancodeNonusbackslash
{-# LINE 455 "Graphics/UI/SDL/Keyboard.hsc" #-}
      101 -> ScancodeApplication
{-# LINE 456 "Graphics/UI/SDL/Keyboard.hsc" #-}
      102 -> ScancodePower
{-# LINE 457 "Graphics/UI/SDL/Keyboard.hsc" #-}
      103 -> ScancodeKPEquals
{-# LINE 458 "Graphics/UI/SDL/Keyboard.hsc" #-}
      104 -> ScancodeF13
{-# LINE 459 "Graphics/UI/SDL/Keyboard.hsc" #-}
      105 -> ScancodeF14
{-# LINE 460 "Graphics/UI/SDL/Keyboard.hsc" #-}
      106 -> ScancodeF15
{-# LINE 461 "Graphics/UI/SDL/Keyboard.hsc" #-}
      107 -> ScancodeF16
{-# LINE 462 "Graphics/UI/SDL/Keyboard.hsc" #-}
      108 -> ScancodeF17
{-# LINE 463 "Graphics/UI/SDL/Keyboard.hsc" #-}
      109 -> ScancodeF18
{-# LINE 464 "Graphics/UI/SDL/Keyboard.hsc" #-}
      110 -> ScancodeF19
{-# LINE 465 "Graphics/UI/SDL/Keyboard.hsc" #-}
      111 -> ScancodeF20
{-# LINE 466 "Graphics/UI/SDL/Keyboard.hsc" #-}
      112 -> ScancodeF21
{-# LINE 467 "Graphics/UI/SDL/Keyboard.hsc" #-}
      113 -> ScancodeF22
{-# LINE 468 "Graphics/UI/SDL/Keyboard.hsc" #-}
      114 -> ScancodeF23
{-# LINE 469 "Graphics/UI/SDL/Keyboard.hsc" #-}
      115 -> ScancodeF24
{-# LINE 470 "Graphics/UI/SDL/Keyboard.hsc" #-}
      116 -> ScancodeExecute
{-# LINE 471 "Graphics/UI/SDL/Keyboard.hsc" #-}
      117 -> ScancodeHelp
{-# LINE 472 "Graphics/UI/SDL/Keyboard.hsc" #-}
      118 -> ScancodeMenu
{-# LINE 473 "Graphics/UI/SDL/Keyboard.hsc" #-}
      119 -> ScancodeSelect
{-# LINE 474 "Graphics/UI/SDL/Keyboard.hsc" #-}
      120 -> ScancodeStop
{-# LINE 475 "Graphics/UI/SDL/Keyboard.hsc" #-}
      121 -> ScancodeAgain
{-# LINE 476 "Graphics/UI/SDL/Keyboard.hsc" #-}
      122 -> ScancodeUndo
{-# LINE 477 "Graphics/UI/SDL/Keyboard.hsc" #-}
      123 -> ScancodeCut
{-# LINE 478 "Graphics/UI/SDL/Keyboard.hsc" #-}
      124 -> ScancodeCopy
{-# LINE 479 "Graphics/UI/SDL/Keyboard.hsc" #-}
      125 -> ScancodePaste
{-# LINE 480 "Graphics/UI/SDL/Keyboard.hsc" #-}
      126 -> ScancodeFind
{-# LINE 481 "Graphics/UI/SDL/Keyboard.hsc" #-}
      127 -> ScancodeMute
{-# LINE 482 "Graphics/UI/SDL/Keyboard.hsc" #-}
      128 -> ScancodeVolumeup
{-# LINE 483 "Graphics/UI/SDL/Keyboard.hsc" #-}
      129 -> ScancodeVolumedown
{-# LINE 484 "Graphics/UI/SDL/Keyboard.hsc" #-}
      133 -> ScancodeKPComma
{-# LINE 485 "Graphics/UI/SDL/Keyboard.hsc" #-}
      134 -> ScancodeKPEqualsas400
{-# LINE 486 "Graphics/UI/SDL/Keyboard.hsc" #-}
      135 -> ScancodeInternational1
{-# LINE 487 "Graphics/UI/SDL/Keyboard.hsc" #-}
      136 -> ScancodeInternational2
{-# LINE 488 "Graphics/UI/SDL/Keyboard.hsc" #-}
      137 -> ScancodeInternational3
{-# LINE 489 "Graphics/UI/SDL/Keyboard.hsc" #-}
      138 -> ScancodeInternational4
{-# LINE 490 "Graphics/UI/SDL/Keyboard.hsc" #-}
      139 -> ScancodeInternational5
{-# LINE 491 "Graphics/UI/SDL/Keyboard.hsc" #-}
      140 -> ScancodeInternational6
{-# LINE 492 "Graphics/UI/SDL/Keyboard.hsc" #-}
      141 -> ScancodeInternational7
{-# LINE 493 "Graphics/UI/SDL/Keyboard.hsc" #-}
      142 -> ScancodeInternational8
{-# LINE 494 "Graphics/UI/SDL/Keyboard.hsc" #-}
      143 -> ScancodeInternational9
{-# LINE 495 "Graphics/UI/SDL/Keyboard.hsc" #-}
      144 -> ScancodeLang1
{-# LINE 496 "Graphics/UI/SDL/Keyboard.hsc" #-}
      145 -> ScancodeLang2
{-# LINE 497 "Graphics/UI/SDL/Keyboard.hsc" #-}
      146 -> ScancodeLang3
{-# LINE 498 "Graphics/UI/SDL/Keyboard.hsc" #-}
      147 -> ScancodeLang4
{-# LINE 499 "Graphics/UI/SDL/Keyboard.hsc" #-}
      148 -> ScancodeLang5
{-# LINE 500 "Graphics/UI/SDL/Keyboard.hsc" #-}
      149 -> ScancodeLang6
{-# LINE 501 "Graphics/UI/SDL/Keyboard.hsc" #-}
      150 -> ScancodeLang7
{-# LINE 502 "Graphics/UI/SDL/Keyboard.hsc" #-}
      151 -> ScancodeLang8
{-# LINE 503 "Graphics/UI/SDL/Keyboard.hsc" #-}
      152 -> ScancodeLang9
{-# LINE 504 "Graphics/UI/SDL/Keyboard.hsc" #-}
      153 -> ScancodeAlterase
{-# LINE 505 "Graphics/UI/SDL/Keyboard.hsc" #-}
      154 -> ScancodeSysreq
{-# LINE 506 "Graphics/UI/SDL/Keyboard.hsc" #-}
      155 -> ScancodeCancel
{-# LINE 507 "Graphics/UI/SDL/Keyboard.hsc" #-}
      156 -> ScancodeClear
{-# LINE 508 "Graphics/UI/SDL/Keyboard.hsc" #-}
      157 -> ScancodePrior
{-# LINE 509 "Graphics/UI/SDL/Keyboard.hsc" #-}
      158 -> ScancodeReturn2
{-# LINE 510 "Graphics/UI/SDL/Keyboard.hsc" #-}
      159 -> ScancodeSeparator
{-# LINE 511 "Graphics/UI/SDL/Keyboard.hsc" #-}
      160 -> ScancodeOut
{-# LINE 512 "Graphics/UI/SDL/Keyboard.hsc" #-}
      161 -> ScancodeOper
{-# LINE 513 "Graphics/UI/SDL/Keyboard.hsc" #-}
      162 -> ScancodeClearagain
{-# LINE 514 "Graphics/UI/SDL/Keyboard.hsc" #-}
      163 -> ScancodeCrsel
{-# LINE 515 "Graphics/UI/SDL/Keyboard.hsc" #-}
      164 -> ScancodeExsel
{-# LINE 516 "Graphics/UI/SDL/Keyboard.hsc" #-}
      176 -> ScancodeKP00
{-# LINE 517 "Graphics/UI/SDL/Keyboard.hsc" #-}
      177 -> ScancodeKP000
{-# LINE 518 "Graphics/UI/SDL/Keyboard.hsc" #-}
      178 -> ScancodeThousandsseparator
{-# LINE 519 "Graphics/UI/SDL/Keyboard.hsc" #-}
      179 -> ScancodeDecimalseparator
{-# LINE 520 "Graphics/UI/SDL/Keyboard.hsc" #-}
      180 -> ScancodeCurrencyunit
{-# LINE 521 "Graphics/UI/SDL/Keyboard.hsc" #-}
      181 -> ScancodeCurrencysubunit
{-# LINE 522 "Graphics/UI/SDL/Keyboard.hsc" #-}
      182 -> ScancodeKPLeftparen
{-# LINE 523 "Graphics/UI/SDL/Keyboard.hsc" #-}
      183 -> ScancodeKPRightparen
{-# LINE 524 "Graphics/UI/SDL/Keyboard.hsc" #-}
      184 -> ScancodeKPLeftbrace
{-# LINE 525 "Graphics/UI/SDL/Keyboard.hsc" #-}
      185 -> ScancodeKPRightbrace
{-# LINE 526 "Graphics/UI/SDL/Keyboard.hsc" #-}
      186 -> ScancodeKPTab
{-# LINE 527 "Graphics/UI/SDL/Keyboard.hsc" #-}
      187 -> ScancodeKPBackspace
{-# LINE 528 "Graphics/UI/SDL/Keyboard.hsc" #-}
      188 -> ScancodeKPA
{-# LINE 529 "Graphics/UI/SDL/Keyboard.hsc" #-}
      189 -> ScancodeKPB
{-# LINE 530 "Graphics/UI/SDL/Keyboard.hsc" #-}
      190 -> ScancodeKPC
{-# LINE 531 "Graphics/UI/SDL/Keyboard.hsc" #-}
      191 -> ScancodeKPD
{-# LINE 532 "Graphics/UI/SDL/Keyboard.hsc" #-}
      192 -> ScancodeKPE
{-# LINE 533 "Graphics/UI/SDL/Keyboard.hsc" #-}
      193 -> ScancodeKPF
{-# LINE 534 "Graphics/UI/SDL/Keyboard.hsc" #-}
      194 -> ScancodeKPXor
{-# LINE 535 "Graphics/UI/SDL/Keyboard.hsc" #-}
      195 -> ScancodeKPPower
{-# LINE 536 "Graphics/UI/SDL/Keyboard.hsc" #-}
      196 -> ScancodeKPPercent
{-# LINE 537 "Graphics/UI/SDL/Keyboard.hsc" #-}
      197 -> ScancodeKPLess
{-# LINE 538 "Graphics/UI/SDL/Keyboard.hsc" #-}
      198 -> ScancodeKPGreater
{-# LINE 539 "Graphics/UI/SDL/Keyboard.hsc" #-}
      199 -> ScancodeKPAmpersand
{-# LINE 540 "Graphics/UI/SDL/Keyboard.hsc" #-}
      200 -> ScancodeKPDblampersand
{-# LINE 541 "Graphics/UI/SDL/Keyboard.hsc" #-}
      201 -> ScancodeKPVerticalbar
{-# LINE 542 "Graphics/UI/SDL/Keyboard.hsc" #-}
      202 -> ScancodeKPDblverticalbar
{-# LINE 543 "Graphics/UI/SDL/Keyboard.hsc" #-}
      203 -> ScancodeKPColon
{-# LINE 544 "Graphics/UI/SDL/Keyboard.hsc" #-}
      204 -> ScancodeKPHash
{-# LINE 545 "Graphics/UI/SDL/Keyboard.hsc" #-}
      205 -> ScancodeKPSpace
{-# LINE 546 "Graphics/UI/SDL/Keyboard.hsc" #-}
      206 -> ScancodeKPAt
{-# LINE 547 "Graphics/UI/SDL/Keyboard.hsc" #-}
      207 -> ScancodeKPExclam
{-# LINE 548 "Graphics/UI/SDL/Keyboard.hsc" #-}
      208 -> ScancodeKPMemstore
{-# LINE 549 "Graphics/UI/SDL/Keyboard.hsc" #-}
      209 -> ScancodeKPMemrecall
{-# LINE 550 "Graphics/UI/SDL/Keyboard.hsc" #-}
      210 -> ScancodeKPMemclear
{-# LINE 551 "Graphics/UI/SDL/Keyboard.hsc" #-}
      211 -> ScancodeKPMemadd
{-# LINE 552 "Graphics/UI/SDL/Keyboard.hsc" #-}
      212 -> ScancodeKPMemsubtract
{-# LINE 553 "Graphics/UI/SDL/Keyboard.hsc" #-}
      213 -> ScancodeKPMemmultiply
{-# LINE 554 "Graphics/UI/SDL/Keyboard.hsc" #-}
      214 -> ScancodeKPMemdivide
{-# LINE 555 "Graphics/UI/SDL/Keyboard.hsc" #-}
      215 -> ScancodeKPPlusminus
{-# LINE 556 "Graphics/UI/SDL/Keyboard.hsc" #-}
      216 -> ScancodeKPClear
{-# LINE 557 "Graphics/UI/SDL/Keyboard.hsc" #-}
      217 -> ScancodeKPClearentry
{-# LINE 558 "Graphics/UI/SDL/Keyboard.hsc" #-}
      218 -> ScancodeKPBinary
{-# LINE 559 "Graphics/UI/SDL/Keyboard.hsc" #-}
      219 -> ScancodeKPOctal
{-# LINE 560 "Graphics/UI/SDL/Keyboard.hsc" #-}
      220 -> ScancodeKPDecimal
{-# LINE 561 "Graphics/UI/SDL/Keyboard.hsc" #-}
      221 -> ScancodeKPHexadecimal
{-# LINE 562 "Graphics/UI/SDL/Keyboard.hsc" #-}
      224 -> ScancodeLCtrl
{-# LINE 563 "Graphics/UI/SDL/Keyboard.hsc" #-}
      225 -> ScancodeLShift
{-# LINE 564 "Graphics/UI/SDL/Keyboard.hsc" #-}
      226 -> ScancodeLAlt
{-# LINE 565 "Graphics/UI/SDL/Keyboard.hsc" #-}
      227 -> ScancodeLGUI
{-# LINE 566 "Graphics/UI/SDL/Keyboard.hsc" #-}
      228 -> ScancodeRCtrl
{-# LINE 567 "Graphics/UI/SDL/Keyboard.hsc" #-}
      229 -> ScancodeRShift
{-# LINE 568 "Graphics/UI/SDL/Keyboard.hsc" #-}
      230 -> ScancodeRAlt
{-# LINE 569 "Graphics/UI/SDL/Keyboard.hsc" #-}
      231 -> ScancodeRGUI
{-# LINE 570 "Graphics/UI/SDL/Keyboard.hsc" #-}
      257 -> ScancodeMode
{-# LINE 571 "Graphics/UI/SDL/Keyboard.hsc" #-}
      258 -> ScancodeAudionext
{-# LINE 572 "Graphics/UI/SDL/Keyboard.hsc" #-}
      259 -> ScancodeAudioprev
{-# LINE 573 "Graphics/UI/SDL/Keyboard.hsc" #-}
      260 -> ScancodeAudiostop
{-# LINE 574 "Graphics/UI/SDL/Keyboard.hsc" #-}
      261 -> ScancodeAudioplay
{-# LINE 575 "Graphics/UI/SDL/Keyboard.hsc" #-}
      262 -> ScancodeAudiomute
{-# LINE 576 "Graphics/UI/SDL/Keyboard.hsc" #-}
      263 -> ScancodeMediaselect
{-# LINE 577 "Graphics/UI/SDL/Keyboard.hsc" #-}
      264 -> ScancodeWWW
{-# LINE 578 "Graphics/UI/SDL/Keyboard.hsc" #-}
      265 -> ScancodeMail
{-# LINE 579 "Graphics/UI/SDL/Keyboard.hsc" #-}
      266 -> ScancodeCalculator
{-# LINE 580 "Graphics/UI/SDL/Keyboard.hsc" #-}
      267 -> ScancodeComputer
{-# LINE 581 "Graphics/UI/SDL/Keyboard.hsc" #-}
      268 -> ScancodeACSearch
{-# LINE 582 "Graphics/UI/SDL/Keyboard.hsc" #-}
      269 -> ScancodeACHome
{-# LINE 583 "Graphics/UI/SDL/Keyboard.hsc" #-}
      270 -> ScancodeACBack
{-# LINE 584 "Graphics/UI/SDL/Keyboard.hsc" #-}
      271 -> ScancodeACForward
{-# LINE 585 "Graphics/UI/SDL/Keyboard.hsc" #-}
      272 -> ScancodeACStop
{-# LINE 586 "Graphics/UI/SDL/Keyboard.hsc" #-}
      273 -> ScancodeACRefresh
{-# LINE 587 "Graphics/UI/SDL/Keyboard.hsc" #-}
      274 -> ScancodeACBookmarks
{-# LINE 588 "Graphics/UI/SDL/Keyboard.hsc" #-}
      275 -> ScancodeBrightnessDown
{-# LINE 589 "Graphics/UI/SDL/Keyboard.hsc" #-}
      276 -> ScancodeBrightnessUp
{-# LINE 590 "Graphics/UI/SDL/Keyboard.hsc" #-}
      277 -> ScancodeDisplaySwitch
{-# LINE 591 "Graphics/UI/SDL/Keyboard.hsc" #-}
      278 -> ScancodeKBDILLUMTOGGLE
{-# LINE 592 "Graphics/UI/SDL/Keyboard.hsc" #-}
      279 -> ScancodeKBDIlluMDown
{-# LINE 593 "Graphics/UI/SDL/Keyboard.hsc" #-}
      280 -> ScancodeKBDIllumUp
{-# LINE 594 "Graphics/UI/SDL/Keyboard.hsc" #-}
      281 -> ScancodeEject
{-# LINE 595 "Graphics/UI/SDL/Keyboard.hsc" #-}
      282 -> ScancodeSleep
{-# LINE 596 "Graphics/UI/SDL/Keyboard.hsc" #-}
      283 -> ScancodeApp1
{-# LINE 597 "Graphics/UI/SDL/Keyboard.hsc" #-}
      284 -> ScancodeApp2
{-# LINE 598 "Graphics/UI/SDL/Keyboard.hsc" #-}
      _ -> error "toScancode: unhandled scancode"

data Keymod
   = KeymodNone
   | KeymodLShift
   | KeymodRShift
   | KeymodLCtrl
   | KeymodRCtrl
   | KeymodLAlt
   | KeymodRAlt
   | KeymodLGui
   | KeymodRGui
   | KeymodNum
   | KeymodCaps
   | KeymodMode
   | KeymodReserved
   deriving (Eq, Show)

fromKeymod :: Keymod -> Word32
{-# LINE 617 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodNone = 0
{-# LINE 618 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodLShift = 1
{-# LINE 619 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodRShift = 2
{-# LINE 620 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodLCtrl = 64
{-# LINE 621 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodRCtrl = 128
{-# LINE 622 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodLAlt = 256
{-# LINE 623 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodRAlt = 512
{-# LINE 624 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodLGui = 1024
{-# LINE 625 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodRGui = 2048
{-# LINE 626 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodNum = 4096
{-# LINE 627 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodCaps = 8192
{-# LINE 628 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodMode = 16384
{-# LINE 629 "Graphics/UI/SDL/Keyboard.hsc" #-}
fromKeymod KeymodReserved = 32768
{-# LINE 630 "Graphics/UI/SDL/Keyboard.hsc" #-}

toKeymod :: Word32 -> Keymod
{-# LINE 632 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 0 = KeymodNone
{-# LINE 633 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 1 = KeymodLShift
{-# LINE 634 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 2 = KeymodRShift
{-# LINE 635 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 64 = KeymodLCtrl
{-# LINE 636 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 128 = KeymodRCtrl
{-# LINE 637 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 256 = KeymodLAlt
{-# LINE 638 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 512 = KeymodRAlt
{-# LINE 639 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 1024 = KeymodLGui
{-# LINE 640 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 2048 = KeymodRGui
{-# LINE 641 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 4096 = KeymodNum
{-# LINE 642 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 8192 = KeymodCaps
{-# LINE 643 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 16384 = KeymodMode
{-# LINE 644 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod 32768 = KeymodReserved
{-# LINE 645 "Graphics/UI/SDL/Keyboard.hsc" #-}
toKeymod _ = error "unhandled keymod"

foreign import ccall safe "SDL_GetKeyboardFocus"
  sdlGetKeyboardFocus :: IO (Ptr WindowStruct)

getKeyboardFocus :: IO Window
getKeyboardFocus =
  sdlGetKeyboardFocus >>= mkFinalizedWindow

foreign import ccall safe "SDL_GetModState"
  sdlGetModState :: IO Word32
{-# LINE 656 "Graphics/UI/SDL/Keyboard.hsc" #-}

getModState :: IO Keymod
getModState = sdlGetModState >>= return . toKeymod

foreign import ccall safe "SDL_SetModState"
  sdlSetModState :: Word32 -> IO ()
{-# LINE 662 "Graphics/UI/SDL/Keyboard.hsc" #-}

setModState :: Keymod -> IO ()
setModState keymod = sdlSetModState $ fromKeymod keymod

foreign import ccall safe "SDL_GetKeyFromScancode"
  sdlGetKeyFromScancode :: Word32 -> IO Int32
{-# LINE 668 "Graphics/UI/SDL/Keyboard.hsc" #-}

getKeyFromScancode :: Scancode -> IO Keycode
getKeyFromScancode sc =
  let sc' = fromIntegral $ fromEnum sc
  in toEnum . fromIntegral <$> sdlGetKeyFromScancode sc'

foreign import ccall safe "SDL_GetScancodeFromKey"
  sdlGetScancodeFromKey :: Int32 -> IO Word32
{-# LINE 676 "Graphics/UI/SDL/Keyboard.hsc" #-}

getScancodeFromKey :: Keycode -> IO Scancode
getScancodeFromKey kc =
  let kc' = fromIntegral $ fromEnum kc
  in toEnum . fromIntegral <$> sdlGetScancodeFromKey kc'

foreign import ccall safe "SDL_GetScancodeName"
  sdlGetScancodeName :: Word32 -> IO CString
{-# LINE 684 "Graphics/UI/SDL/Keyboard.hsc" #-}

getScancodeName :: Scancode -> IO String
getScancodeName sc =
  let sc' = fromIntegral $ fromEnum sc
  in sdlGetScancodeName sc' >>= peekCString

foreign import ccall safe "SDL_GetScancodeFromName"
  sdlGetScancodeFromName :: CString -> IO Word32
{-# LINE 692 "Graphics/UI/SDL/Keyboard.hsc" #-}

getScancodeFromName :: String -> IO Scancode
getScancodeFromName name =
  withCString name $ \name' ->
    toEnum . fromIntegral <$> sdlGetScancodeFromName name'

foreign import ccall safe "SDL_GetKeyName"
  sdlGetKeyName :: Int32 -> IO CString
{-# LINE 700 "Graphics/UI/SDL/Keyboard.hsc" #-}

getKeyName :: Keycode -> IO String
getKeyName kc =
  let kc' = fromIntegral $ fromEnum kc
  in sdlGetKeyName kc' >>= peekCString

foreign import ccall safe "SDL_GetKeyFromName"
  sdlGetKeyFromName :: CString -> IO Int32
{-# LINE 708 "Graphics/UI/SDL/Keyboard.hsc" #-}

getKeyFromName :: String -> IO Keycode
getKeyFromName name =
  withCString name $ \name' ->
    toEnum . fromIntegral <$> sdlGetKeyFromName name'

foreign import ccall safe "SDL_StartTextInput"
  startTextInput :: IO ()

foreign import ccall safe "SDL_IsTextInputActive"
  sdlIsTextInputActive :: IO Word32
{-# LINE 719 "Graphics/UI/SDL/Keyboard.hsc" #-}

isTextInputActive :: IO Bool
isTextInputActive = sdlBoolToBool <$> sdlIsTextInputActive

foreign import ccall safe "SDL_StopTextInput"
  stopTextInput :: IO ()

foreign import ccall safe "SDL_SetTextInputRect"
  sdlSetTextInputRect :: Ptr Rect -> IO ()

setTextInputRect :: Rect -> IO ()
setTextInputRect = flip with sdlSetTextInputRect

foreign import ccall safe "SDL_HasScreenKeyboardSupport"
  sdlHasScreenKeyboardSupport :: IO Word32
{-# LINE 734 "Graphics/UI/SDL/Keyboard.hsc" #-}

hasScreenKeyboardSupport :: IO Bool
hasScreenKeyboardSupport = sdlBoolToBool <$> sdlHasScreenKeyboardSupport

foreign import ccall safe "SDL_IsScreenKeyboardShown"
  sdlIsScreenKeyboardShown :: Ptr WindowStruct -> IO Word32
{-# LINE 740 "Graphics/UI/SDL/Keyboard.hsc" #-}

isScreenKeyboardShown :: Window -> IO Bool
isScreenKeyboardShown window =
  withForeignPtr window $ \window' ->
    sdlBoolToBool <$> sdlIsScreenKeyboardShown window'

