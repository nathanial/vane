/-
  Vane.Terminal.Cursor - Cursor position, visibility, and style
-/

namespace Vane.Terminal

/-- Cursor visual style -/
inductive CursorStyle where
  | block          -- Solid block (default)
  | underline      -- Underline
  | bar            -- Vertical bar (I-beam)
  | blinkBlock     -- Blinking block
  | blinkUnderline -- Blinking underline
  | blinkBar       -- Blinking bar
  deriving Repr, BEq, Inhabited

namespace CursorStyle

/-- Convert DECSCUSR parameter to cursor style -/
def fromDECSCUSR (n : Nat) : CursorStyle :=
  match n with
  | 0 => .blinkBlock      -- Default (blinking block)
  | 1 => .blinkBlock
  | 2 => .block
  | 3 => .blinkUnderline
  | 4 => .underline
  | 5 => .blinkBar
  | 6 => .bar
  | _ => .blinkBlock

/-- Check if cursor style is blinking -/
def isBlinking : CursorStyle → Bool
  | .blinkBlock | .blinkUnderline | .blinkBar => true
  | _ => false

end CursorStyle

/-- Cursor state -/
structure Cursor where
  /-- Column position (0-indexed) -/
  col : Nat := 0
  /-- Row position (0-indexed) -/
  row : Nat := 0
  /-- Whether cursor is visible -/
  visible : Bool := true
  /-- Cursor visual style -/
  style : CursorStyle := .blinkBlock
  /-- Blink state (true = visible in current blink cycle) -/
  blinkOn : Bool := true
  /-- Whether cursor wrapping is pending (at end of line) -/
  wrapPending : Bool := false
  deriving Repr, BEq, Inhabited

namespace Cursor

/-- Create cursor at origin -/
def origin : Cursor := {}

/-- Create cursor at specific position -/
def atPos (col row : Nat) : Cursor := { col, row }

/-- Move cursor to absolute position, clamping to bounds -/
def moveTo (c : Cursor) (col row : Nat) (width height : Nat) : Cursor :=
  { c with
    col := min col (width - 1)
    row := min row (height - 1)
    wrapPending := false
  }

/-- Move cursor up by n rows -/
def moveUp (c : Cursor) (n : Nat := 1) : Cursor :=
  { c with
    row := c.row - min n c.row
    wrapPending := false
  }

/-- Move cursor down by n rows, clamping to height -/
def moveDown (c : Cursor) (n : Nat := 1) (height : Nat) : Cursor :=
  { c with
    row := min (c.row + n) (height - 1)
    wrapPending := false
  }

/-- Move cursor left by n columns -/
def moveLeft (c : Cursor) (n : Nat := 1) : Cursor :=
  { c with
    col := c.col - min n c.col
    wrapPending := false
  }

/-- Move cursor right by n columns, clamping to width -/
def moveRight (c : Cursor) (n : Nat := 1) (width : Nat) : Cursor :=
  { c with
    col := min (c.col + n) (width - 1)
    wrapPending := false
  }

/-- Move cursor to beginning of current line -/
def toLineStart (c : Cursor) : Cursor :=
  { c with col := 0, wrapPending := false }

/-- Move cursor to beginning of line n rows down -/
def toNextLine (c : Cursor) (n : Nat := 1) (height : Nat) : Cursor :=
  { c with
    col := 0
    row := min (c.row + n) (height - 1)
    wrapPending := false
  }

/-- Move cursor to beginning of line n rows up -/
def toPrevLine (c : Cursor) (n : Nat := 1) : Cursor :=
  { c with
    col := 0
    row := c.row - min n c.row
    wrapPending := false
  }

/-- Move cursor to specific column -/
def toColumn (c : Cursor) (col : Nat) (width : Nat) : Cursor :=
  { c with
    col := min col (width - 1)
    wrapPending := false
  }

/-- Move cursor to specific row -/
def toRow (c : Cursor) (row : Nat) (height : Nat) : Cursor :=
  { c with
    row := min row (height - 1)
    wrapPending := false
  }

/-- Advance cursor by a number of columns (for character output) -/
def advanceBy (c : Cursor) (width height : Nat) (autoWrap : Bool) (step : Nat) : Cursor :=
  if step == 0 then
    c
  else if c.wrapPending then
    -- Pending wrap - move to next line
    if autoWrap then
      { c with col := 0, row := min (c.row + 1) (height - 1), wrapPending := false }
    else
      { c with wrapPending := false }
  else if c.col + step >= width then
    -- At or past last column - set pending wrap
    { c with col := min (c.col + step - 1) (width - 1), wrapPending := true }
  else
    -- Normal advance
    { c with col := c.col + step }

/-- Advance cursor by one position (for character output) -/
def advance (c : Cursor) (width height : Nat) (autoWrap : Bool) : Cursor :=
  c.advanceBy width height autoWrap 1

/-- Make cursor visible -/
def setVisible (c : Cursor) : Cursor :=
  { c with visible := true }

/-- Make cursor invisible -/
def setInvisible (c : Cursor) : Cursor :=
  { c with visible := false }

/-- Set cursor style -/
def setStyle (c : Cursor) (style : CursorStyle) : Cursor :=
  { c with style }

/-- Toggle blink state (for animation) -/
def toggleBlink (c : Cursor) : Cursor :=
  { c with blinkOn := !c.blinkOn }

/-- Check if cursor should be drawn (visible and in "on" blink state) -/
def shouldDraw (c : Cursor) : Bool :=
  c.visible && (c.blinkOn || !c.style.isBlinking)

end Cursor

/-- Saved cursor state (for DECSC/DECRC) -/
structure SavedCursor where
  col : Nat := 0
  row : Nat := 0
  style : CursorStyle := .blinkBlock
  -- Could also save: character set, wrap mode, origin mode
  deriving Repr, BEq, Inhabited

namespace SavedCursor

/-- Save current cursor state -/
def save (c : Cursor) : SavedCursor :=
  { col := c.col, row := c.row, style := c.style }

/-- Restore cursor from saved state -/
def restore (saved : SavedCursor) (c : Cursor) : Cursor :=
  { c with col := saved.col, row := saved.row, style := saved.style, wrapPending := false }

end SavedCursor

end Vane.Terminal
