/-
  Vane.App.Input - Map afferent key codes to terminal escape sequences
-/

import Vane.Input.KeyEncoder

namespace Vane.App.Input

open Vane.Input

/-- Map afferent modifier bits to Vane Modifiers -/
def mapModifiers (bits : UInt16) : Modifiers := {
  shift := (bits &&& 1) != 0
  ctrl := (bits &&& 2) != 0
  alt := (bits &&& 4) != 0
  cmd := (bits &&& 8) != 0
}

/-- Map macOS key code to SpecialKey -/
def keyCodeToSpecial (code : UInt16) : Option SpecialKey :=
  match code.toNat with
  | 123 => some .left
  | 124 => some .right
  | 125 => some .down
  | 126 => some .up
  | 53  => some .escape
  | 48  => some .tab
  | 116 => some .pageUp
  | 121 => some .pageDown
  | 115 => some .home
  | 119 => some .end_
  | 117 => some .delete
  | 114 => some .insert
  -- Function keys
  | 122 => some .f1
  | 120 => some .f2
  | 99  => some .f3
  | 118 => some .f4
  | 96  => some .f5
  | 97  => some .f6
  | 98  => some .f7
  | 100 => some .f8
  | 101 => some .f9
  | 109 => some .f10
  | 103 => some .f11
  | 111 => some .f12
  | _   => none

/-- Map macOS key code to character (US keyboard layout) -/
def keyCodeToChar (code : UInt16) (mods : Modifiers) : Option Char :=
  let c := code.toNat
  -- Return key
  if c == 36 then some '\r'
  -- Backspace/Delete
  else if c == 51 then some (Char.ofNat 0x7F)
  -- Space
  else if c == 49 then some ' '
  -- Letter keys (a-z)
  else if c == 0 then some (if mods.shift then 'A' else 'a')
  else if c == 11 then some (if mods.shift then 'B' else 'b')
  else if c == 8 then some (if mods.shift then 'C' else 'c')
  else if c == 2 then some (if mods.shift then 'D' else 'd')
  else if c == 14 then some (if mods.shift then 'E' else 'e')
  else if c == 3 then some (if mods.shift then 'F' else 'f')
  else if c == 5 then some (if mods.shift then 'G' else 'g')
  else if c == 4 then some (if mods.shift then 'H' else 'h')
  else if c == 34 then some (if mods.shift then 'I' else 'i')
  else if c == 38 then some (if mods.shift then 'J' else 'j')
  else if c == 40 then some (if mods.shift then 'K' else 'k')
  else if c == 37 then some (if mods.shift then 'L' else 'l')
  else if c == 46 then some (if mods.shift then 'M' else 'm')
  else if c == 45 then some (if mods.shift then 'N' else 'n')
  else if c == 31 then some (if mods.shift then 'O' else 'o')
  else if c == 35 then some (if mods.shift then 'P' else 'p')
  else if c == 12 then some (if mods.shift then 'Q' else 'q')
  else if c == 15 then some (if mods.shift then 'R' else 'r')
  else if c == 1 then some (if mods.shift then 'S' else 's')
  else if c == 17 then some (if mods.shift then 'T' else 't')
  else if c == 32 then some (if mods.shift then 'U' else 'u')
  else if c == 9 then some (if mods.shift then 'V' else 'v')
  else if c == 13 then some (if mods.shift then 'W' else 'w')
  else if c == 7 then some (if mods.shift then 'X' else 'x')
  else if c == 16 then some (if mods.shift then 'Y' else 'y')
  else if c == 6 then some (if mods.shift then 'Z' else 'z')
  -- Number keys (top row) - macOS virtual key codes
  else if c == 18 then some (if mods.shift then '!' else '1')
  else if c == 19 then some (if mods.shift then '@' else '2')
  else if c == 20 then some (if mods.shift then '#' else '3')
  else if c == 21 then some (if mods.shift then '$' else '4')
  else if c == 23 then some (if mods.shift then '%' else '5')
  else if c == 22 then some (if mods.shift then '^' else '6')
  else if c == 26 then some (if mods.shift then '&' else '7')
  else if c == 28 then some (if mods.shift then '*' else '8')
  else if c == 25 then some (if mods.shift then '(' else '9')
  else if c == 29 then some (if mods.shift then ')' else '0')
  -- Punctuation
  else if c == 27 then some (if mods.shift then '_' else '-')
  else if c == 24 then some (if mods.shift then '+' else '=')
  else if c == 33 then some (if mods.shift then '{' else '[')
  else if c == 30 then some (if mods.shift then '}' else ']')
  else if c == 42 then some (if mods.shift then '|' else '\\')
  else if c == 41 then some (if mods.shift then ':' else ';')
  else if c == 39 then some (if mods.shift then '"' else '\'')
  else if c == 43 then some (if mods.shift then '<' else ',')
  else if c == 47 then some (if mods.shift then '>' else '.')
  else if c == 44 then some (if mods.shift then '?' else '/')
  else if c == 50 then some (if mods.shift then '~' else '`')
  else none

/-- Map afferent key code and modifiers to KeyInput -/
def mapKeyCode (code : UInt16) (modBits : UInt16) : Option KeyInput :=
  let mods := mapModifiers modBits
  -- First check for special keys
  match keyCodeToSpecial code with
  | some special =>
    -- Handle Shift+Tab as backtab
    if special == .tab && mods.shift then
      some (.special .backTab { mods with shift := false })
    else
      some (.special special mods)
  | none =>
    -- Check for character keys
    match keyCodeToChar code mods with
    | some c => some (.char c mods)
    | none => none

/-- Map a character input (from text input event) to KeyInput -/
def mapCharInput (c : Char) (modBits : UInt16) : KeyInput :=
  let mods := mapModifiers modBits
  .char c mods

/-- Encode a key input to bytes for PTY -/
def encodeForPty (input : KeyInput) (applicationMode : Bool := false) : ByteArray :=
  KeyEncoder.encode input applicationMode

end Vane.App.Input
