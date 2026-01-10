/-
  Vane.Core.Cell - Single styled character in the terminal buffer
-/

import Vane.Core.Style

namespace Vane

/-- A single cell in the terminal buffer -/
structure Cell where
  char : Char := ' '
  combining : Array Char := #[]
  width : Nat := 1
  fg : Color := .default
  bg : Color := .default
  modifier : Modifier := {}
  deriving Repr, BEq, Inhabited

namespace Cell

def inRanges (cp : Nat) (ranges : List (Nat × Nat)) : Bool :=
  ranges.any fun (lo, hi) => cp >= lo && cp <= hi

def zeroWidthRanges : List (Nat × Nat) := [
  (0x0300, 0x036F),
  (0x1AB0, 0x1AFF),
  (0x1DC0, 0x1DFF),
  (0x20D0, 0x20FF),
  (0xFE20, 0xFE2F),
  (0x200B, 0x200F),
  (0x2028, 0x202F),
  (0x2060, 0x206F),
  (0xFEFF, 0xFEFF),
  (0xFE00, 0xFE0F),
  (0xE0100, 0xE01EF),
  (0xE0000, 0xE007F),
  (0x1160, 0x11FF),
  (0xD7B0, 0xD7FF),
  (0x0610, 0x061A),
  (0x064B, 0x065F),
  (0x0670, 0x0670),
  (0x06D6, 0x06DC),
  (0x06DF, 0x06E4),
  (0x06E7, 0x06E8),
  (0x06EA, 0x06ED),
  (0x0591, 0x05BD),
  (0x05BF, 0x05BF),
  (0x05C1, 0x05C2),
  (0x05C4, 0x05C5),
  (0x05C7, 0x05C7),
  (0x0E31, 0x0E31),
  (0x0E34, 0x0E3A),
  (0x0E47, 0x0E4E),
  (0x0900, 0x0903),
  (0x093A, 0x094F),
  (0x0951, 0x0957),
  (0x0962, 0x0963),
  (0x0981, 0x0983),
  (0x09BC, 0x09BC),
  (0x09BE, 0x09CD),
  (0x0A01, 0x0A03),
  (0x0A3C, 0x0A51),
  (0x0A70, 0x0A71),
  (0x0A75, 0x0A75)
]

def wideRanges : List (Nat × Nat) := [
  (0x1100, 0x115F),
  (0x2E80, 0x2EFF),
  (0x2F00, 0x2FDF),
  (0x3000, 0x303F),
  (0x3040, 0x309F),
  (0x30A0, 0x30FF),
  (0x3100, 0x312F),
  (0x3130, 0x318F),
  (0x3190, 0x319F),
  (0x31A0, 0x31BF),
  (0x31C0, 0x31EF),
  (0x31F0, 0x31FF),
  (0x3200, 0x32FF),
  (0x3300, 0x33FF),
  (0x3400, 0x4DBF),
  (0x4E00, 0x9FFF),
  (0xA000, 0xA48F),
  (0xA490, 0xA4CF),
  (0xAC00, 0xD7AF),
  (0xF900, 0xFAFF),
  (0xFE10, 0xFE1F),
  (0xFE30, 0xFE4F),
  (0xFF00, 0xFF60),
  (0xFFE0, 0xFFE6),
  (0x20000, 0x2A6DF),
  (0x2A700, 0x2B73F),
  (0x2B740, 0x2B81F),
  (0x2B820, 0x2CEAF),
  (0x2CEB0, 0x2EBEF),
  (0x2F800, 0x2FA1F),
  (0x30000, 0x3134F),
  (0x1F300, 0x1F5FF),
  (0x1F600, 0x1F64F),
  (0x1F680, 0x1F6FF),
  (0x1F700, 0x1F77F),
  (0x1F900, 0x1F9FF),
  (0x1FA00, 0x1FA6F),
  (0x1FA70, 0x1FAFF),
  (0x1F100, 0x1F1FF),
  (0x1F000, 0x1F0FF)
]

def charWidth (c : Char) : Nat :=
  let cp := c.toNat
  if cp < 32 || (cp >= 0x7F && cp < 0xA0) then 0
  else if inRanges cp zeroWidthRanges then 0
  else if inRanges cp wideRanges then 2
  else 1

def empty : Cell := {}

def new (c : Char) : Cell := { char := c, width := charWidth c }

def styled (c : Char) (fg bg : Color) (mod : Modifier := {}) : Cell :=
  { char := c, width := charWidth c, fg := fg, bg := bg, modifier := mod }

def continuation (cell : Cell) : Cell :=
  { cell with char := '\x00', combining := #[], width := 0 }

def appendCombining (cell : Cell) (c : Char) : Cell :=
  { cell with combining := cell.combining.push c }

def text (cell : Cell) : String := Id.run do
  let mut s := if cell.char == '\x00' then "" else cell.char.toString
  for c in cell.combining do
    s := s.push c
  s

def withChar (cell : Cell) (c : Char) : Cell := { cell with char := c, combining := #[], width := charWidth c }

def withFg (cell : Cell) (c : Color) : Cell := { cell with fg := c }

def withBg (cell : Cell) (c : Color) : Cell := { cell with bg := c }

def withModifier (cell : Cell) (m : Modifier) : Cell :=
  { cell with modifier := Modifier.merge cell.modifier m }

def setFg (cell : Cell) (c : Color) : Cell := { cell with fg := c }

def setBg (cell : Cell) (c : Color) : Cell := { cell with bg := c }

def reset : Cell := empty

/-- Apply style to cell, preserving character -/
def applyStyle (cell : Cell) (s : Style) : Cell := {
  cell with
  fg := if s.fg == .default then cell.fg else s.fg
  bg := if s.bg == .default then cell.bg else s.bg
  modifier := Modifier.merge cell.modifier s.modifier
}

/-- Get the style of this cell -/
def toStyle (cell : Cell) : Style := {
  fg := cell.fg
  bg := cell.bg
  modifier := cell.modifier
}

/-- Check if cell is effectively empty (space with default colors) -/
def isEmpty (cell : Cell) : Bool :=
  (cell.char == ' ' || cell.char == '\x00') &&
    cell.combining.isEmpty &&
    cell.fg == .default &&
    cell.bg == .default &&
    !cell.modifier.hasAny

end Cell

end Vane
