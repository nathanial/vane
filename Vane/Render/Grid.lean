/-
  Vane.Render.Grid - Render terminal cell grid to Canvas
-/

import Afferent
import Vane.Core.Cell
import Vane.Core.Style
import Vane.Core.Buffer
import Vane.Terminal.State
import Vane.Terminal.Cursor

namespace Vane.Render.Grid

open Afferent
open Vane.Terminal

/-- Convert Vane Color to Afferent Color -/
def vaneColorToAfferent (c : Vane.Color) (isForeground : Bool) : Afferent.Color :=
  let (r, g, b, a) := c.toRGBA isForeground
  Color.rgba r g b a

/-- Render parameters for the grid -/
structure RenderParams where
  font : Font
  cellWidth : Float
  cellHeight : Float
  paddingX : Float
  paddingY : Float
  defaultFg : Afferent.Color
  defaultBg : Afferent.Color
  cursorColor : Afferent.Color

/-- Calculate cell position on screen -/
def cellPosition (params : RenderParams) (col row : Nat) : Point :=
  ⟨params.paddingX + col.toFloat * params.cellWidth,
   params.paddingY + row.toFloat * params.cellHeight⟩

/-- Render a single cell's background -/
def renderCellBackground (canvas : Canvas) (params : RenderParams)
    (col row : Nat) (cell : Cell) : IO Canvas := do
  let bgColor := if cell.modifier.reverse then
    vaneColorToAfferent cell.fg true
  else
    vaneColorToAfferent cell.bg false

  -- Only draw background if it's not the default
  if cell.bg != .default || cell.modifier.reverse then
    let pos := cellPosition params col row
    let c := canvas.setFillColor bgColor
    c.fillRectXYWH pos.x pos.y params.cellWidth params.cellHeight
  else
    pure canvas

/-- Render a single cell's foreground (character) -/
def renderCellForeground (canvas : Canvas) (params : RenderParams)
    (col row : Nat) (cell : Cell) : IO Canvas := do
  -- Skip empty or space characters
  if cell.char == ' ' || cell.char == '\x00' then
    pure canvas
  else
    let fgColor := if cell.modifier.reverse then
      vaneColorToAfferent cell.bg false
    else if cell.modifier.hidden then
      vaneColorToAfferent cell.bg false  -- Same as background = invisible
    else
      vaneColorToAfferent cell.fg true

    let pos := cellPosition params col row
    -- Offset text slightly from top-left of cell
    let textPos := ⟨pos.x, pos.y + params.font.ascender⟩
    canvas.fillTextColor (cell.char.toString) textPos params.font fgColor

/-- Render the cursor -/
def renderCursor (canvas : Canvas) (params : RenderParams)
    (cursor : Cursor) (visible : Bool) : IO Canvas := do
  if !cursor.visible || !visible then
    pure canvas
  else
    let pos := cellPosition params cursor.col cursor.row
    let c := canvas.setFillColor params.cursorColor

    match cursor.style with
    | .block | .blinkBlock =>
      -- Full block cursor (filled rectangle)
      c.fillRectXYWH pos.x pos.y params.cellWidth params.cellHeight

    | .underline | .blinkUnderline =>
      -- Underline cursor (thin rectangle at bottom)
      let underlineHeight := params.cellHeight * 0.1
      let y := pos.y + params.cellHeight - underlineHeight
      c.fillRectXYWH pos.x y params.cellWidth underlineHeight

    | .bar | .blinkBar =>
      -- Vertical bar cursor (thin rectangle on left)
      let barWidth := params.cellWidth * 0.1
      c.fillRectXYWH pos.x pos.y barWidth params.cellHeight

/-- Render the entire terminal grid -/
def render (canvas : Canvas) (params : RenderParams)
    (terminal : TerminalState) (cursorVisible : Bool) : IO Canvas := do
  let buffer := terminal.currentBuffer
  let mut c := canvas

  -- Phase 1: Render all backgrounds
  for row in [0:terminal.height] do
    for col in [0:terminal.width] do
      let cell := buffer.get col row
      c ← renderCellBackground c params col row cell

  -- Phase 2: Render all foreground characters
  for row in [0:terminal.height] do
    for col in [0:terminal.width] do
      let cell := buffer.get col row
      c ← renderCellForeground c params col row cell

  -- Phase 3: Render cursor
  c ← renderCursor c params terminal.cursor cursorVisible

  pure c

/-- Render only dirty rows (optimization) -/
def renderDirty (canvas : Canvas) (params : RenderParams)
    (terminal : TerminalState) (cursorVisible : Bool) : IO Canvas := do
  let buffer := terminal.currentBuffer
  let mut c := canvas

  -- Only render rows marked as dirty
  for row in [0:terminal.height] do
    if terminal.dirtyRows[row]?.getD true then
      -- Render background for this row
      for col in [0:terminal.width] do
        let cell := buffer.get col row
        c ← renderCellBackground c params col row cell

      -- Render foreground for this row
      for col in [0:terminal.width] do
        let cell := buffer.get col row
        c ← renderCellForeground c params col row cell

  -- Always render cursor
  c ← renderCursor c params terminal.cursor cursorVisible

  pure c

end Vane.Render.Grid
