/-
  Vane.Core.Buffer - 2D terminal buffer with scrollback support
-/

import Vane.Core.Cell

namespace Vane

/-- A 2D buffer of terminal cells -/
structure Buffer where
  width : Nat
  height : Nat
  cells : Array Cell
  deriving Repr, Inhabited

namespace Buffer

/-- Create a new empty buffer filled with spaces -/
def create (width height : Nat) : Buffer := {
  width := width
  height := height
  cells := Array.replicate (width * height) Cell.empty
}

/-- Convert (col, row) to array index -/
@[inline]
def index (buf : Buffer) (col row : Nat) : Nat :=
  row * buf.width + col

/-- Check if coordinates are within bounds -/
@[inline]
def inBounds (buf : Buffer) (col row : Nat) : Bool :=
  col < buf.width && row < buf.height

/-- Get cell at position (returns empty cell if out of bounds) -/
def get (buf : Buffer) (col row : Nat) : Cell :=
  if buf.inBounds col row then
    buf.cells.getD (buf.index col row) Cell.empty
  else
    Cell.empty

/-- Set cell at position (no-op if out of bounds) -/
def set (buf : Buffer) (col row : Nat) (cell : Cell) : Buffer :=
  if buf.inBounds col row then
    { buf with cells := buf.cells.set! (buf.index col row) cell }
  else
    buf

/-- Set character at position, preserving style -/
def setChar (buf : Buffer) (col row : Nat) (c : Char) : Buffer :=
  if buf.inBounds col row then
    let idx := buf.index col row
    let cell := buf.cells.getD idx Cell.empty
    { buf with cells := buf.cells.set! idx { cell with char := c } }
  else
    buf

/-- Fill entire buffer with a cell -/
def fill (buf : Buffer) (cell : Cell) : Buffer :=
  { buf with cells := Array.replicate (buf.width * buf.height) cell }

/-- Fill a rectangular region -/
def fillRect (buf : Buffer) (x y w h : Nat) (cell : Cell) : Buffer := Id.run do
  let mut result := buf
  for row in [y:y+h] do
    for col in [x:x+w] do
      result := result.set col row cell
  result

/-- Clear entire buffer (fill with empty cells) -/
def clear (buf : Buffer) : Buffer :=
  buf.fill Cell.empty

/-- Clear a single row (optionally from col start to col end) -/
def clearRow (buf : Buffer) (row : Nat) (startCol : Option Nat := none) (endCol : Option Nat := none) : Buffer :=
  if row >= buf.height then
    buf
  else
    let start := startCol.getD 0
    let finish := min (endCol.map (· + 1) |>.getD buf.width) buf.width
    if start >= finish then buf
    else buf.fillRect start row (finish - start) 1 Cell.empty

/-- Clear from cursor to end of line -/
def clearToEndOfLine (buf : Buffer) (col row : Nat) : Buffer :=
  if row < buf.height && col < buf.width then
    buf.fillRect col row (buf.width - col) 1 Cell.empty
  else
    buf

/-- Clear from start of line to cursor -/
def clearFromStartOfLine (buf : Buffer) (col row : Nat) : Buffer :=
  if row < buf.height then
    buf.fillRect 0 row (min (col + 1) buf.width) 1 Cell.empty
  else
    buf

/-- Write a string starting at position -/
def writeString (buf : Buffer) (col row : Nat) (s : String) (style : Style := {}) : Buffer := Id.run do
  let mut result := buf
  let mut c := col
  for char in s.toList do
    if c >= buf.width then break
    result := result.set c row (Cell.styled char style.fg style.bg style.modifier)
    c := c + 1
  result

/-- Scroll the buffer up by n lines, filling bottom with empty cells -/
def scrollUp (buf : Buffer) (n : Nat := 1) : Buffer := Id.run do
  if n >= buf.height then
    return buf.clear
  let mut result := buf
  -- Copy rows up
  for row in [0:buf.height - n] do
    for col in [0:buf.width] do
      result := result.set col row (buf.get col (row + n))
  -- Clear bottom rows
  for row in [buf.height - n:buf.height] do
    result := result.clearRow row
  result

/-- Scroll the buffer down by n lines, filling top with empty cells -/
def scrollDown (buf : Buffer) (n : Nat := 1) : Buffer := Id.run do
  if n >= buf.height then
    return buf.clear
  let mut result := buf
  -- Copy rows down (from bottom to top to avoid overwriting)
  for i in [0:buf.height - n] do
    let row := buf.height - 1 - i
    for col in [0:buf.width] do
      result := result.set col row (buf.get col (row - n))
  -- Clear top rows
  for row in [0:n] do
    result := result.clearRow row
  result

/-- Resize buffer, preserving content where possible -/
def resize (buf : Buffer) (newWidth newHeight : Nat) : Buffer := Id.run do
  let mut result := Buffer.create newWidth newHeight
  let copyWidth := min buf.width newWidth
  let copyHeight := min buf.height newHeight
  for row in [0:copyHeight] do
    for col in [0:copyWidth] do
      result := result.set col row (buf.get col row)
  result

/-- Get a row as an array of cells -/
def getRow (buf : Buffer) (row : Nat) : Array Cell :=
  if row >= buf.height then
    #[]
  else
    let start := row * buf.width
    buf.cells.extract start (start + buf.width)

/-- Set a row from an array of cells -/
def setRow (buf : Buffer) (row : Nat) (cells : Array Cell) : Buffer :=
  if row >= buf.height then
    buf
  else Id.run do
    let mut result := buf
    for i in [0:min cells.size buf.width] do
      result := result.set i row (cells.getD i Cell.empty)
    result

/-- Insert n blank lines at row, scrolling content down -/
def insertLines (buf : Buffer) (row : Nat) (n : Nat) : Buffer := Id.run do
  if row >= buf.height then return buf
  let mut result := buf
  -- Move lines down (from bottom to avoid overwriting)
  for i in [0:buf.height - row - n] do
    let srcRow := buf.height - 1 - n - i
    let dstRow := buf.height - 1 - i
    if srcRow >= row && dstRow < buf.height then
      result := result.setRow dstRow (buf.getRow srcRow)
  -- Clear inserted lines
  for r in [row:row + n] do
    if r < buf.height then
      result := result.clearRow r
  result

/-- Delete n lines at row, scrolling content up -/
def deleteLines (buf : Buffer) (row : Nat) (n : Nat) : Buffer := Id.run do
  if row >= buf.height then return buf
  let mut result := buf
  -- Move lines up
  for srcRow in [row + n:buf.height] do
    let dstRow := srcRow - n
    result := result.setRow dstRow (buf.getRow srcRow)
  -- Clear bottom lines
  for r in [buf.height - n:buf.height] do
    result := result.clearRow r
  result

/-- Scroll up within a region (top and bottom are inclusive, 0-indexed) -/
def scrollRegionUp (buf : Buffer) (top bottom : Nat) (n : Nat := 1) : Buffer := Id.run do
  if top >= buf.height || bottom >= buf.height || top > bottom then return buf
  let regionHeight := bottom - top + 1
  if n >= regionHeight then
    -- Clear entire region
    let mut result := buf
    for row in [top:bottom + 1] do
      result := result.clearRow row
    return result

  let mut result := buf
  -- Move lines up within region
  for i in [0:regionHeight - n] do
    let srcRow := top + n + i
    let dstRow := top + i
    if srcRow <= bottom then
      result := result.setRow dstRow (buf.getRow srcRow)
  -- Clear bottom n lines of region
  for row in [bottom - n + 1:bottom + 1] do
    result := result.clearRow row
  result

/-- Scroll down within a region (top and bottom are inclusive, 0-indexed) -/
def scrollRegionDown (buf : Buffer) (top bottom : Nat) (n : Nat := 1) : Buffer := Id.run do
  if top >= buf.height || bottom >= buf.height || top > bottom then return buf
  let regionHeight := bottom - top + 1
  if n >= regionHeight then
    let mut result := buf
    for row in [top:bottom + 1] do
      result := result.clearRow row
    return result

  let mut result := buf
  -- Move lines down within region (from bottom to avoid overwriting)
  for i in [0:regionHeight - n] do
    let srcRow := bottom - n - i
    let dstRow := bottom - i
    if srcRow >= top then
      result := result.setRow dstRow (buf.getRow srcRow)
  -- Clear top n lines of region
  for row in [top:top + n] do
    result := result.clearRow row
  result

/-- Insert n blank characters at position, shifting rest of line right -/
def insertChars (buf : Buffer) (col row : Nat) (n : Nat) : Buffer := Id.run do
  if row >= buf.height || col >= buf.width then return buf
  let rowCells := buf.getRow row
  let mut result := buf
  -- Shift characters right (from end to avoid overwriting)
  for i in [0:buf.width - col - n] do
    let srcCol := buf.width - 1 - n - i
    let dstCol := buf.width - 1 - i
    if srcCol >= col && dstCol < buf.width then
      result := result.set dstCol row (rowCells.getD srcCol Cell.empty)
  -- Clear inserted positions
  for c in [col:min (col + n) buf.width] do
    result := result.set c row Cell.empty
  result

/-- Delete n characters at position, shifting rest of line left -/
def deleteChars (buf : Buffer) (col row : Nat) (n : Nat) : Buffer := Id.run do
  if row >= buf.height || col >= buf.width then return buf
  let rowCells := buf.getRow row
  let mut result := buf
  -- Shift characters left
  for dstCol in [col:buf.width - n] do
    let srcCol := dstCol + n
    if srcCol < buf.width then
      result := result.set dstCol row (rowCells.getD srcCol Cell.empty)
    else
      result := result.set dstCol row Cell.empty
  -- Clear rightmost n columns
  for c in [buf.width - n:buf.width] do
    result := result.set c row Cell.empty
  result

end Buffer

end Vane
