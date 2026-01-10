/-
  Vane.Terminal.State - Complete terminal state
-/

import Vane.Core.Buffer
import Vane.Core.Style
import Vane.Terminal.Cursor
import Vane.Terminal.Modes

namespace Vane.Terminal

/-- Complete terminal state -/
structure TerminalState where
  /-- Primary screen buffer -/
  buffer : Buffer
  /-- Alternate screen buffer (for vim, less, etc.) -/
  altBuffer : Option Buffer := none
  /-- Whether we're currently using alternate buffer -/
  usingAltBuffer : Bool := false
  /-- Cursor state -/
  cursor : Cursor := {}
  /-- Saved cursor for primary screen -/
  savedCursor : SavedCursor := {}
  /-- Saved cursor for alternate screen -/
  savedAltCursor : SavedCursor := {}
  /-- Terminal modes -/
  modes : Modes := {}
  /-- Current scroll region -/
  scrollRegion : ScrollRegion
  /-- Tab stops -/
  tabStops : TabStops
  /-- Current text style (for new characters) -/
  currentStyle : Style := Style.default
  /-- Window title -/
  title : String := ""
  /-- Icon name -/
  iconName : String := ""
  /-- Current working directory (OSC 7) -/
  currentDirectory : String := ""
  /-- Whether terminal needs redraw -/
  dirty : Bool := true
  /-- Dirty rows (for partial redraw) -/
  dirtyRows : Array Bool
  /-- Scrollback buffer (lines scrolled off top) -/
  scrollback : Array (Array Cell) := #[]
  /-- Maximum scrollback lines -/
  maxScrollback : Nat := 10000
  /-- Current scroll offset (0 = bottom, >0 = scrolled up) -/
  scrollOffset : Nat := 0
  /-- Last printed character (for REP command) -/
  lastChar : Option Char := none
  deriving Repr

namespace TerminalState

/-- Create a new terminal state with given dimensions -/
def create (width height : Nat) (maxScrollback : Nat := 10000) : TerminalState :=
  { buffer := Buffer.create width height
    scrollRegion := ScrollRegion.fullScreen height
    tabStops := TabStops.default width
    dirtyRows := Array.replicate height true
    maxScrollback
  }

/-- Get current buffer (primary or alternate) -/
def currentBuffer (ts : TerminalState) : Buffer :=
  if ts.usingAltBuffer then
    ts.altBuffer.getD ts.buffer
  else
    ts.buffer

/-- Update current buffer -/
def withCurrentBuffer (ts : TerminalState) (f : Buffer → Buffer) : TerminalState :=
  if ts.usingAltBuffer then
    match ts.altBuffer with
    | some buf => { ts with altBuffer := some (f buf), dirty := true }
    | none => ts
  else
    { ts with buffer := f ts.buffer, dirty := true }

/-- Get terminal width -/
def width (ts : TerminalState) : Nat :=
  ts.currentBuffer.width

/-- Get terminal height -/
def height (ts : TerminalState) : Nat :=
  ts.currentBuffer.height

/-- Mark a row as dirty -/
def markRowDirty (ts : TerminalState) (row : Nat) : TerminalState :=
  if row < ts.dirtyRows.size then
    { ts with dirtyRows := ts.dirtyRows.set! row true, dirty := true }
  else
    ts

/-- Mark all rows as dirty -/
def markAllDirty (ts : TerminalState) : TerminalState :=
  { ts with dirtyRows := ts.dirtyRows.map fun _ => true, dirty := true }

/-- Clear dirty flags -/
def clearDirty (ts : TerminalState) : TerminalState :=
  { ts with dirtyRows := ts.dirtyRows.map fun _ => false, dirty := false }

/-- Mark rows in a range as dirty -/
def markRangeDirty (ts : TerminalState) (start finish : Nat) : TerminalState := Id.run do
  let mut result := ts
  for row in [start:finish] do
    result := result.markRowDirty row
  result

/-- Scroll up n lines within scroll region -/
def scrollUp (ts : TerminalState) (n : Nat := 1) : TerminalState :=
  if n == 0 then ts
  else
    -- Add top lines to scrollback (only for primary buffer, full-screen scroll)
    let ts := if !ts.usingAltBuffer && ts.scrollRegion.top == 0 then
      let buf := ts.currentBuffer
      let linesToSave := min n (ts.scrollRegion.bottom + 1)
      let newScrollback := Id.run do
        let mut sb := ts.scrollback
        for row in [0:linesToSave] do
          let line := buf.getRow row
          sb := sb.push line
        -- Trim scrollback if too long
        if sb.size > ts.maxScrollback then
          sb := sb.extract (sb.size - ts.maxScrollback) sb.size
        sb
      { ts with scrollback := newScrollback }
    else
      ts

    -- Scroll buffer within region
    let ts := ts.withCurrentBuffer fun buf =>
      buf.scrollRegionUp ts.scrollRegion.top ts.scrollRegion.bottom n

    -- Mark affected rows dirty
    ts.markRangeDirty ts.scrollRegion.top (ts.scrollRegion.bottom + 1)

/-- Scroll down n lines within scroll region -/
def scrollDown (ts : TerminalState) (n : Nat := 1) : TerminalState :=
  if n == 0 then ts
  else
    let ts := ts.withCurrentBuffer fun buf =>
      buf.scrollRegionDown ts.scrollRegion.top ts.scrollRegion.bottom n
    ts.markRangeDirty ts.scrollRegion.top (ts.scrollRegion.bottom + 1)

/-- Execute line feed (move cursor down, possibly scroll) -/
def lineFeed (ts : TerminalState) : TerminalState :=
  if ts.cursor.row >= ts.scrollRegion.bottom then
    -- At bottom of scroll region - scroll up
    ts.scrollUp 1
  else
    { ts with cursor := ts.cursor.moveDown 1 ts.height }

/-- Execute carriage return -/
def carriageReturn (ts : TerminalState) : TerminalState :=
  { ts with cursor := ts.cursor.toLineStart }

/-- Execute backspace -/
def backspace (ts : TerminalState) : TerminalState :=
  { ts with cursor := ts.cursor.moveLeft 1 }

/-- Execute horizontal tab -/
def tab (ts : TerminalState) : TerminalState :=
  let nextCol := ts.tabStops.nextStop ts.cursor.col ts.width
  { ts with cursor := ts.cursor.toColumn nextCol ts.width }

/-- Execute reverse tab (backtab) -/
def reverseTab (ts : TerminalState) : TerminalState :=
  let prevCol := ts.tabStops.prevStop ts.cursor.col
  { ts with cursor := ts.cursor.toColumn prevCol ts.width }

/-- Write a character at current cursor position -/
def writeChar (ts : TerminalState) (c : Char) : TerminalState := Id.run do
  let width := Cell.charWidth c
  if width == 0 then
    let targetCol :=
      if ts.cursor.wrapPending then
        some ts.cursor.col
      else if ts.cursor.col == 0 then
        none
      else
        some (ts.cursor.col - 1)
    match targetCol with
    | none => return ts
    | some col =>
      let buf := ts.currentBuffer.appendCombiningAt col ts.cursor.row c
      let ts := ts.withCurrentBuffer fun _ => buf
      return ts.markRowDirty ts.cursor.row

  let mut ts := if ts.cursor.wrapPending && ts.modes.autoWrap then
    let ts := ts.lineFeed
    { ts with cursor := ts.cursor.toLineStart }
  else
    { ts with cursor := { ts.cursor with wrapPending := false } }

  if width == 2 && ts.cursor.col + 1 >= ts.width then
    if ts.modes.autoWrap then
      let ts' := ts.lineFeed
      let ts' := { ts' with cursor := ts'.cursor.toLineStart }
      if ts'.cursor.col + 1 >= ts'.width then
        return ts'
      else
        ts := ts'
    else
      return ts

  let cell := Cell.styled c ts.currentStyle.fg ts.currentStyle.bg ts.currentStyle.modifier
  ts := ts.withCurrentBuffer fun buf => Id.run do
    let mut b := buf.clearWideAt ts.cursor.col ts.cursor.row
    if width == 2 then
      if ts.cursor.col + 1 < buf.width then
        b := b.clearWideAt (ts.cursor.col + 1) ts.cursor.row
        b := b.set ts.cursor.col ts.cursor.row cell
        b := b.set (ts.cursor.col + 1) ts.cursor.row (Cell.continuation cell)
        return b
      else
        return b
    return b.set ts.cursor.col ts.cursor.row cell
  ts := ts.markRowDirty ts.cursor.row

  -- Advance cursor
  { ts with
    cursor := ts.cursor.advanceBy ts.width ts.height ts.modes.autoWrap width
    lastChar := some c
  }

/-- Write a string at current cursor position -/
def writeString (ts : TerminalState) (s : String) : TerminalState :=
  s.foldl (fun ts c => ts.writeChar c) ts

/-- Switch to alternate screen buffer -/
def switchToAltScreen (ts : TerminalState) (clearScreen : Bool := false) : TerminalState :=
  if ts.usingAltBuffer then ts
  else
    let altBuf := if clearScreen then
      Buffer.create ts.width ts.height
    else
      ts.altBuffer.getD (Buffer.create ts.width ts.height)
    let savedCursor := if ts.modes.saveCursorOnAltScreen then
      SavedCursor.save ts.cursor
    else
      ts.savedCursor
    { ts with
      altBuffer := some altBuf
      usingAltBuffer := true
      savedCursor := savedCursor
      cursor := Cursor.origin
    }.markAllDirty

/-- Switch back to primary screen buffer -/
def switchToPrimaryScreen (ts : TerminalState) : TerminalState :=
  if !ts.usingAltBuffer then ts
  else
    let cursor := if ts.modes.saveCursorOnAltScreen then
      ts.savedCursor.restore ts.cursor
    else
      ts.cursor
    { ts with
      usingAltBuffer := false
      cursor := cursor
    }.markAllDirty

/-- Move cursor to position (1-based coordinates from terminal) -/
def cursorTo (ts : TerminalState) (row col : Nat) : TerminalState :=
  let row := if row == 0 then 0 else row - 1
  let col := if col == 0 then 0 else col - 1
  let row := if ts.modes.originMode then
    ts.scrollRegion.top + row
  else
    row
  { ts with cursor := ts.cursor.moveTo col row ts.width ts.height }

/-- Erase display -/
def eraseDisplay (ts : TerminalState) (mode : Nat) : TerminalState :=
  let buf := ts.currentBuffer
  let newBuf := match mode with
    | 0 => -- Erase below (from cursor to end)
      let buf := buf.clearRow ts.cursor.row (some ts.cursor.col) none
      Id.run do
        let mut b := buf
        for row in [ts.cursor.row + 1 : ts.height] do
          b := b.clearRow row none none
        b
    | 1 => -- Erase above (from start to cursor)
      let buf := buf.clearRow ts.cursor.row none (some ts.cursor.col)
      Id.run do
        let mut b := buf
        for row in [0 : ts.cursor.row] do
          b := b.clearRow row none none
        b
    | 2 => -- Erase all
      buf.clear
    | 3 => -- Erase all + scrollback
      buf.clear
    | _ => buf
  let ts := { ts with scrollback := if mode == 3 then #[] else ts.scrollback }
  (ts.withCurrentBuffer fun _ => newBuf).markAllDirty

/-- Erase line -/
def eraseLine (ts : TerminalState) (mode : Nat) : TerminalState :=
  let ts := ts.withCurrentBuffer fun buf =>
    match mode with
    | 0 => buf.clearRow ts.cursor.row (some ts.cursor.col) none  -- Erase right
    | 1 => buf.clearRow ts.cursor.row none (some ts.cursor.col)  -- Erase left
    | 2 => buf.clearRow ts.cursor.row none none                  -- Erase all
    | _ => buf
  ts.markRowDirty ts.cursor.row

/-- Insert n blank lines at cursor -/
def insertLines (ts : TerminalState) (n : Nat) : TerminalState :=
  if !ts.scrollRegion.contains ts.cursor.row then ts
  else
    -- Move cursor to start of line
    let ts := { ts with cursor := ts.cursor.toLineStart }
    -- Scroll down from cursor row
    let ts := ts.withCurrentBuffer fun buf =>
      buf.scrollRegionDown ts.cursor.row ts.scrollRegion.bottom n
    ts.markRangeDirty ts.cursor.row (ts.scrollRegion.bottom + 1)

/-- Delete n lines at cursor -/
def deleteLines (ts : TerminalState) (n : Nat) : TerminalState :=
  if !ts.scrollRegion.contains ts.cursor.row then ts
  else
    let ts := { ts with cursor := ts.cursor.toLineStart }
    let ts := ts.withCurrentBuffer fun buf =>
      buf.scrollRegionUp ts.cursor.row ts.scrollRegion.bottom n
    ts.markRangeDirty ts.cursor.row (ts.scrollRegion.bottom + 1)

/-- Insert n blank characters at cursor -/
def insertChars (ts : TerminalState) (n : Nat) : TerminalState :=
  let ts := ts.withCurrentBuffer fun buf =>
    buf.insertChars ts.cursor.col ts.cursor.row n
  ts.markRowDirty ts.cursor.row

/-- Delete n characters at cursor -/
def deleteChars (ts : TerminalState) (n : Nat) : TerminalState :=
  let ts := ts.withCurrentBuffer fun buf =>
    buf.deleteChars ts.cursor.col ts.cursor.row n
  ts.markRowDirty ts.cursor.row

/-- Erase n characters at cursor (replace with spaces) -/
def eraseChars (ts : TerminalState) (n : Nat) : TerminalState :=
  let ts := ts.withCurrentBuffer fun buf => Id.run do
    let mut b := buf
    for i in [0:n] do
      let col := ts.cursor.col + i
      if col < ts.width then
        b := b.clearWideAt col ts.cursor.row
        b := b.set col ts.cursor.row Cell.empty
    b
  ts.markRowDirty ts.cursor.row

/-- Set scroll region (1-based, 0 means default) -/
def setScrollRegion (ts : TerminalState) (top bottom : Nat) : TerminalState :=
  let region := ScrollRegion.fromDECSTBM top bottom ts.height
  let cursor := if ts.modes.originMode then
    ts.cursor.moveTo 0 region.top ts.width ts.height
  else
    ts.cursor.moveTo 0 0 ts.width ts.height
  { ts with scrollRegion := region, cursor := cursor }

/-- Save cursor position -/
def saveCursor (ts : TerminalState) : TerminalState :=
  if ts.usingAltBuffer then
    { ts with savedAltCursor := SavedCursor.save ts.cursor }
  else
    { ts with savedCursor := SavedCursor.save ts.cursor }

/-- Restore cursor position -/
def restoreCursor (ts : TerminalState) : TerminalState :=
  let saved := if ts.usingAltBuffer then ts.savedAltCursor else ts.savedCursor
  { ts with cursor := saved.restore ts.cursor }

/-- Resize terminal -/
def resize (ts : TerminalState) (newWidth newHeight : Nat) : TerminalState :=
  let buffer := ts.buffer.resize newWidth newHeight
  let altBuffer := ts.altBuffer.map fun buf => buf.resize newWidth newHeight
  let cursor := ts.cursor.moveTo (min ts.cursor.col (newWidth - 1)) (min ts.cursor.row (newHeight - 1)) newWidth newHeight
  let scrollRegion := if ts.scrollRegion.bottom >= newHeight then
    ScrollRegion.fullScreen newHeight
  else
    ts.scrollRegion
  let tabStops := TabStops.default newWidth
  let dirtyRows := Array.replicate newHeight true
  { ts with
    buffer
    altBuffer
    cursor
    scrollRegion
    tabStops
    dirtyRows
    dirty := true
  }

/-- Soft reset (DECSTR) -/
def softReset (ts : TerminalState) : TerminalState :=
  { ts with
    modes := Modes.default
    cursor := { ts.cursor with visible := true, style := .blinkBlock }
    currentStyle := Style.default
    scrollRegion := ScrollRegion.fullScreen ts.height
    tabStops := TabStops.default ts.width
  }

/-- Get line from scrollback (0 = most recent) -/
def getScrollbackLine (ts : TerminalState) (idx : Nat) : Option (Array Cell) :=
  if idx < ts.scrollback.size then
    ts.scrollback[ts.scrollback.size - 1 - idx]?
  else
    none

/-- Scroll view up (into scrollback) -/
def scrollViewUp (ts : TerminalState) (n : Nat := 1) : TerminalState :=
  { ts with scrollOffset := min (ts.scrollOffset + n) ts.scrollback.size }

/-- Scroll view down (towards live content) -/
def scrollViewDown (ts : TerminalState) (n : Nat := 1) : TerminalState :=
  { ts with scrollOffset := ts.scrollOffset - min n ts.scrollOffset }

/-- Reset scroll view to bottom -/
def scrollViewToBottom (ts : TerminalState) : TerminalState :=
  { ts with scrollOffset := 0 }

end TerminalState

end Vane.Terminal
