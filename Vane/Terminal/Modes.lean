/-
  Vane.Terminal.Modes - Terminal mode flags
-/

import Vane.Parser.CSI

namespace Vane.Terminal

/-- Terminal modes (set/reset via SM/RM and DECSET/DECRST) -/
structure Modes where
  -- Standard ANSI modes
  /-- Insert mode (IRM) - insert vs replace characters -/
  insertMode : Bool := false
  /-- Keyboard action mode (KAM) - lock keyboard -/
  keyboardLocked : Bool := false
  /-- Send/receive mode (SRM) - local echo -/
  localEcho : Bool := false
  /-- Line feed/new line mode (LNM) - when true, LF also does CR -/
  newLineMode : Bool := true

  -- DEC private modes
  /-- Application cursor keys (DECCKM) -/
  applicationCursorKeys : Bool := false
  /-- ANSI/VT52 mode (DECANM) - always ANSI in our case -/
  ansiMode : Bool := true
  /-- 132 column mode (DECCOLM) -/
  column132 : Bool := false
  /-- Smooth scroll (DECSCLM) -/
  smoothScroll : Bool := false
  /-- Reverse video (DECSCNM) -/
  reverseVideo : Bool := false
  /-- Origin mode (DECOM) - cursor relative to scroll region -/
  originMode : Bool := false
  /-- Auto-wrap mode (DECAWM) -/
  autoWrap : Bool := true
  /-- Auto-repeat (DECARM) -/
  autoRepeat : Bool := true
  /-- Cursor visible (DECTCEM) -/
  cursorVisible : Bool := true
  /-- Cursor blink (att610) -/
  cursorBlink : Bool := true

  -- Mouse modes
  /-- X10 mouse reporting -/
  mouseX10 : Bool := false
  /-- VT200 mouse reporting (normal tracking) -/
  mouseVt200 : Bool := false
  /-- VT200 highlight mouse -/
  mouseVt200Highlight : Bool := false
  /-- Button-event tracking -/
  mouseBtnEvent : Bool := false
  /-- Any-event tracking -/
  mouseAnyEvent : Bool := false
  /-- Focus in/out events -/
  focusEvents : Bool := false
  /-- UTF-8 mouse mode -/
  mouseUtf8 : Bool := false
  /-- SGR mouse mode (extended coordinates) -/
  mouseSgr : Bool := false
  /-- SGR-Pixels mouse mode -/
  mousePixels : Bool := false

  -- Screen buffer modes
  /-- Use alternate screen buffer -/
  alternateScreen : Bool := false
  /-- Save cursor when switching to alternate screen -/
  saveCursorOnAltScreen : Bool := false

  -- Paste mode
  /-- Bracketed paste mode -/
  bracketedPaste : Bool := false

  deriving Repr, BEq, Inhabited

namespace Modes

/-- Default terminal modes -/
def default : Modes := {}

/-- Set a DEC private mode by number -/
def setPrivateMode (m : Modes) (mode : Nat) : Modes :=
  match mode with
  | 1    => { m with applicationCursorKeys := true }   -- DECCKM
  | 2    => { m with ansiMode := true }                -- DECANM
  | 3    => { m with column132 := true }               -- DECCOLM
  | 4    => { m with smoothScroll := true }            -- DECSCLM
  | 5    => { m with reverseVideo := true }            -- DECSCNM
  | 6    => { m with originMode := true }              -- DECOM
  | 7    => { m with autoWrap := true }                -- DECAWM
  | 8    => { m with autoRepeat := true }              -- DECARM
  | 9    => { m with mouseX10 := true }                -- X10 mouse
  | 12   => { m with cursorBlink := true }             -- att610
  | 25   => { m with cursorVisible := true }           -- DECTCEM
  | 1000 => { m with mouseVt200 := true }              -- VT200 mouse
  | 1001 => { m with mouseVt200Highlight := true }     -- VT200 highlight
  | 1002 => { m with mouseBtnEvent := true }           -- Button-event
  | 1003 => { m with mouseAnyEvent := true }           -- Any-event
  | 1004 => { m with focusEvents := true }             -- Focus events
  | 1005 => { m with mouseUtf8 := true }               -- UTF-8 mouse
  | 1006 => { m with mouseSgr := true }                -- SGR mouse
  | 1016 => { m with mousePixels := true }             -- SGR-Pixels
  | 47   => { m with alternateScreen := true }         -- Alt screen
  | 1047 => { m with alternateScreen := true }         -- Alt screen (clear)
  | 1048 => { m with saveCursorOnAltScreen := true }   -- Save cursor
  | 1049 => { m with alternateScreen := true, saveCursorOnAltScreen := true } -- Alt + save
  | 2004 => { m with bracketedPaste := true }          -- Bracketed paste
  | _    => m

/-- Reset a DEC private mode by number -/
def resetPrivateMode (m : Modes) (mode : Nat) : Modes :=
  match mode with
  | 1    => { m with applicationCursorKeys := false }
  | 2    => { m with ansiMode := false }
  | 3    => { m with column132 := false }
  | 4    => { m with smoothScroll := false }
  | 5    => { m with reverseVideo := false }
  | 6    => { m with originMode := false }
  | 7    => { m with autoWrap := false }
  | 8    => { m with autoRepeat := false }
  | 9    => { m with mouseX10 := false }
  | 12   => { m with cursorBlink := false }
  | 25   => { m with cursorVisible := false }
  | 1000 => { m with mouseVt200 := false }
  | 1001 => { m with mouseVt200Highlight := false }
  | 1002 => { m with mouseBtnEvent := false }
  | 1003 => { m with mouseAnyEvent := false }
  | 1004 => { m with focusEvents := false }
  | 1005 => { m with mouseUtf8 := false }
  | 1006 => { m with mouseSgr := false }
  | 1016 => { m with mousePixels := false }
  | 47   => { m with alternateScreen := false }
  | 1047 => { m with alternateScreen := false }
  | 1048 => { m with saveCursorOnAltScreen := false }
  | 1049 => { m with alternateScreen := false, saveCursorOnAltScreen := false }
  | 2004 => { m with bracketedPaste := false }
  | _    => m

/-- Set a standard ANSI mode by number -/
def setAnsiMode (m : Modes) (mode : Nat) : Modes :=
  match mode with
  | 4  => { m with insertMode := true }      -- IRM
  | 12 => { m with localEcho := false }      -- SRM (echo off)
  | 20 => { m with newLineMode := true }     -- LNM
  | _  => m

/-- Reset a standard ANSI mode by number -/
def resetAnsiMode (m : Modes) (mode : Nat) : Modes :=
  match mode with
  | 4  => { m with insertMode := false }
  | 12 => { m with localEcho := true }
  | 20 => { m with newLineMode := false }
  | _  => m

/-- Check if any mouse mode is active -/
def mouseEnabled (m : Modes) : Bool :=
  m.mouseX10 || m.mouseVt200 || m.mouseVt200Highlight ||
  m.mouseBtnEvent || m.mouseAnyEvent

/-- Get the active mouse encoding mode -/
def mouseEncoding (m : Modes) : String :=
  if m.mousePixels then "sgr-pixels"
  else if m.mouseSgr then "sgr"
  else if m.mouseUtf8 then "utf8"
  else "x10"

end Modes

/-- Scroll region (DECSTBM) -/
structure ScrollRegion where
  /-- Top row (0-indexed, inclusive) -/
  top : Nat
  /-- Bottom row (0-indexed, inclusive) -/
  bottom : Nat
  deriving Repr, BEq, Inhabited

namespace ScrollRegion

/-- Create scroll region for full screen -/
def fullScreen (height : Nat) : ScrollRegion :=
  { top := 0, bottom := height - 1 }

/-- Create scroll region from 1-based DECSTBM params -/
def fromDECSTBM (top bottom : Nat) (height : Nat) : ScrollRegion :=
  let t := if top == 0 then 0 else top - 1
  let b := if bottom == 0 then height - 1 else min (bottom - 1) (height - 1)
  { top := min t b, bottom := max t b }

/-- Check if a row is within the scroll region -/
def contains (sr : ScrollRegion) (row : Nat) : Bool :=
  row >= sr.top && row <= sr.bottom

/-- Get height of scroll region -/
def height (sr : ScrollRegion) : Nat :=
  sr.bottom - sr.top + 1

end ScrollRegion

/-- Tab stops -/
structure TabStops where
  /-- Set of column positions with tab stops -/
  stops : Array Nat := #[]
  /-- Default tab width (for when no explicit stops set) -/
  defaultWidth : Nat := 8
  deriving Repr, Inhabited

namespace TabStops

/-- Initialize with default tab stops every 8 columns -/
def default (width : Nat) : TabStops :=
  let stops := Id.run do
    let mut arr : Array Nat := #[]
    for i in [0:width / 8 + 1] do
      arr := arr.push (i * 8)
    arr
  { stops, defaultWidth := 8 }

/-- Set a tab stop at a column -/
def set (ts : TabStops) (col : Nat) : TabStops :=
  if ts.stops.contains col then ts
  else { ts with stops := (ts.stops.push col).insertionSort (·<·) }

/-- Clear a tab stop at a column -/
def clear (ts : TabStops) (col : Nat) : TabStops :=
  { ts with stops := ts.stops.filter (· != col) }

/-- Clear all tab stops -/
def clearAll (ts : TabStops) : TabStops :=
  { ts with stops := #[] }

/-- Find next tab stop after current column -/
def nextStop (ts : TabStops) (col : Nat) (width : Nat) : Nat :=
  -- Find first stop greater than col
  match ts.stops.find? (· > col) with
  | some stop => min stop (width - 1)
  | none =>
    -- Use default tab width
    let next := ((col / ts.defaultWidth) + 1) * ts.defaultWidth
    min next (width - 1)

/-- Find previous tab stop before current column -/
def prevStop (ts : TabStops) (col : Nat) : Nat :=
  -- Find last stop less than col
  let candidates := ts.stops.filter (· < col)
  candidates.getMax? (·<·) |>.getD 0

end TabStops

end Vane.Terminal
