/-
  Vane.Core.Cell - Single styled character in the terminal buffer
-/

import Vane.Core.Style

namespace Vane

/-- A single cell in the terminal buffer -/
structure Cell where
  char : Char := ' '
  fg : Color := .default
  bg : Color := .default
  modifier : Modifier := {}
  deriving Repr, BEq, Inhabited

namespace Cell

def empty : Cell := {}

def new (c : Char) : Cell := { char := c }

def styled (c : Char) (fg bg : Color) (mod : Modifier := {}) : Cell :=
  { char := c, fg := fg, bg := bg, modifier := mod }

def withChar (cell : Cell) (c : Char) : Cell := { cell with char := c }

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
  cell.char == ' ' && cell.fg == .default && cell.bg == .default && !cell.modifier.hasAny

end Cell

end Vane
