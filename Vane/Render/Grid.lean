/-
  Vane.Render.Grid - GPU-accelerated terminal cell grid rendering

  Uses batched rendering to minimize draw calls:
  - All cell backgrounds are batched into a single GPU draw call
  - Text is rendered per-cell (text batching not yet supported)
-/

import Afferent
import Vane.Core.Cell
import Vane.Core.Style
import Vane.Core.Buffer
import Vane.Terminal.State
import Vane.Terminal.Cursor

namespace Vane.Render.Grid

open Afferent
open Afferent (Batch)
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

/-! ## Batched Background Rendering -/

/-- Build a batch of all cell backgrounds that need rendering.
    Only includes cells with non-default background or reverse video. -/
def buildBackgroundBatch (params : RenderParams) (buffer : Buffer)
    (width height : Nat) (screenWidth screenHeight : Float)
    (rowFilter : Nat → Bool := fun _ => true) : Batch := Id.run do
  -- Estimate ~25% of cells have non-default backgrounds
  let mut batch := Batch.withCapacity (width * height / 4)

  for row in [0:height] do
    if rowFilter row then
      for col in [0:width] do
        let cell := buffer.get col row
        -- Only batch non-default backgrounds
        if cell.bg != .default || cell.modifier.reverse then
          let bgColor := if cell.modifier.reverse then
            vaneColorToAfferent cell.fg true
          else
            vaneColorToAfferent cell.bg false
          let x := params.paddingX + col.toFloat * params.cellWidth
          let y := params.paddingY + row.toFloat * params.cellHeight
          batch := batch.addAxisAlignedRect x y params.cellWidth params.cellHeight
            bgColor screenWidth screenHeight

  batch

/-- Draw a tessellation batch using GPU buffers. -/
def drawBatch (canvas : Canvas) (batch : Batch) : IO Unit := do
  if batch.isEmpty then return
  let vertexBuffer ← FFI.Buffer.createVertex canvas.ctx.renderer batch.vertices
  let indexBuffer ← FFI.Buffer.createIndex canvas.ctx.renderer batch.indices
  canvas.ctx.renderer.drawTriangles vertexBuffer indexBuffer batch.indexCount.toUInt32
  FFI.Buffer.destroy indexBuffer
  FFI.Buffer.destroy vertexBuffer

/-! ## Text Rendering -/

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

/-! ## Cursor Rendering -/

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

/-! ## Main Render Functions -/

/-- Render the entire terminal grid using GPU batching.
    All backgrounds are drawn in a single GPU call, then text is rendered per-cell. -/
def render (canvas : Canvas) (params : RenderParams)
    (terminal : TerminalState) (cursorVisible : Bool) : IO Canvas := do
  let buffer := terminal.currentBuffer
  let (screenWidth, screenHeight) ← canvas.ctx.getCurrentSize

  -- Phase 1: Batch and draw all backgrounds in one GPU call
  let bgBatch := buildBackgroundBatch params buffer terminal.width terminal.height
    screenWidth screenHeight
  drawBatch canvas bgBatch

  -- Phase 2: Render all foreground characters
  let mut c := canvas
  for row in [0:terminal.height] do
    for col in [0:terminal.width] do
      let cell := buffer.get col row
      c ← renderCellForeground c params col row cell

  -- Phase 3: Render cursor
  c ← renderCursor c params terminal.cursor cursorVisible

  pure c

/-- Render only dirty rows using GPU batching.
    Backgrounds for dirty rows are batched into a single GPU call. -/
def renderDirty (canvas : Canvas) (params : RenderParams)
    (terminal : TerminalState) (cursorVisible : Bool) : IO Canvas := do
  let buffer := terminal.currentBuffer
  let (screenWidth, screenHeight) ← canvas.ctx.getCurrentSize

  -- Row filter: only include dirty rows
  let isDirty := fun row => terminal.dirtyRows[row]?.getD true

  -- Phase 1: Batch and draw backgrounds for dirty rows
  let bgBatch := buildBackgroundBatch params buffer terminal.width terminal.height
    screenWidth screenHeight isDirty
  drawBatch canvas bgBatch

  -- Phase 2: Render foreground for dirty rows only
  let mut c := canvas
  for row in [0:terminal.height] do
    if isDirty row then
      for col in [0:terminal.width] do
        let cell := buffer.get col row
        c ← renderCellForeground c params col row cell

  -- Phase 3: Always render cursor
  c ← renderCursor c params terminal.cursor cursorVisible

  pure c

/-! ## Legacy Single-Cell Rendering (for reference) -/

/-- Render a single cell's background (legacy, non-batched) -/
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

end Vane.Render.Grid
