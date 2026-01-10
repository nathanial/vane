/-
  Vane.App.State - Application state combining all components
-/

import Afferent
import Vane.Terminal.State
import Vane.Terminal.Executor
import Vane.Parser.Machine
import Vane.PTY.FFI
import Vane.App.Config
import Vane.App.Selection
import Vane.Render.Grid

namespace Vane.App

open Afferent
open Vane.Terminal
open Vane.Parser
open Vane.PTY

/-- Complete application state -/
structure AppState where
  /-- Terminal state (buffer, cursor, modes) -/
  terminal : TerminalState
  /-- VT500 parser -/
  parser : Parser
  /-- PTY connection to shell -/
  pty : PTY
  /-- Loaded font for text rendering -/
  font : Font
  /-- Character cell width (from font measurement) -/
  cellWidth : Float
  /-- Character cell height (from font line height) -/
  cellHeight : Float
  /-- Application configuration -/
  config : Config
  /-- Last time cursor blink was toggled (milliseconds) -/
  lastBlinkTime : Nat
  /-- Whether cursor is currently visible (blink state) -/
  cursorVisible : Bool
  /-- Current window width in pixels -/
  windowWidth : Float
  /-- Current window height in pixels -/
  windowHeight : Float
  /-- Terminal width in columns -/
  termCols : Nat
  /-- Terminal height in rows -/
  termRows : Nat
  /-- Current text selection (if any) -/
  selection : Option SelectionRange := none
  /-- Whether mouse button is currently pressed -/
  mouseDown : Bool := false
  /-- Click count for multi-click detection (1=single, 2=double, 3=triple) -/
  clickCount : Nat := 0
  /-- Timestamp of last click for multi-click detection -/
  lastClickTime : Nat := 0
  /-- Column of last click for multi-click detection -/
  lastClickCol : Nat := 0
  /-- Row of last click for multi-click detection -/
  lastClickRow : Nat := 0

namespace AppState

/-- Create render parameters from app state -/
def toRenderParams (state : AppState) : Render.Grid.RenderParams := {
  font := state.font
  cellWidth := state.cellWidth
  cellHeight := state.cellHeight
  paddingX := state.config.paddingX
  paddingY := state.config.paddingY
  defaultFg := Color.rgba 0.9 0.9 0.9 1.0
  defaultBg := Color.rgba 0.1 0.1 0.1 1.0
  cursorColor := Color.rgba 0.8 0.8 0.8 0.8
}

/-- Calculate terminal dimensions from window size -/
def calculateDimensions (windowWidth windowHeight cellWidth cellHeight paddingX paddingY : Float) : Nat × Nat :=
  let usableWidth := windowWidth - paddingX * 2
  let usableHeight := windowHeight - paddingY * 2
  let cols := (usableWidth / cellWidth).toUInt32.toNat
  let rows := (usableHeight / cellHeight).toUInt32.toNat
  (max 1 cols, max 1 rows)

/-- Create initial application state -/
def create (config : Config) (canvas : Canvas) (font : Font) (pty : PTY) (screenScale : Float := 1.0) : IO AppState := do
  -- Measure cell dimensions (all in physical pixels for consistent rendering)
  let (charWidth, _) ← font.measureText "M"
  let cellHeight := font.lineHeight

  -- Get window size (in physical pixels)
  let (windowWidth, windowHeight) := (canvas.ctx.baseWidth, canvas.ctx.baseHeight)

  -- Scale padding to physical pixels
  let physicalPaddingX := config.paddingX * screenScale
  let physicalPaddingY := config.paddingY * screenScale

  -- Calculate terminal dimensions using physical padding
  let (termCols, termRows) := calculateDimensions
    windowWidth windowHeight charWidth cellHeight physicalPaddingX physicalPaddingY

  -- Create terminal state
  let terminal := TerminalState.create termCols termRows config.maxScrollback

  -- Get initial time
  let now ← IO.monoMsNow

  -- Store config with physical pixel padding for rendering
  let scaledConfig := { config with paddingX := physicalPaddingX, paddingY := physicalPaddingY }

  pure {
    terminal
    parser := Parser.new
    pty
    font
    cellWidth := charWidth
    cellHeight := cellHeight
    config := scaledConfig
    lastBlinkTime := now
    cursorVisible := true
    windowWidth
    windowHeight
    termCols
    termRows
  }

/-- Update cursor blink state -/
def updateBlink (state : AppState) (now : Nat) : AppState :=
  let elapsed := now - state.lastBlinkTime
  if elapsed >= state.config.cursorBlinkMs then
    { state with
      lastBlinkTime := now
      cursorVisible := !state.cursorVisible
    }
  else
    state

/-- Process bytes from PTY and update terminal state, returning any responses to send back -/
def processOutput (state : AppState) (bytes : ByteArray) : AppState × Array Terminal.Response :=
  if bytes.isEmpty then (state, #[])
  else
    -- Parse bytes to actions
    let (parser, actions) := state.parser.process bytes
    -- Execute actions on terminal
    let result := state.terminal.executeActions actions
    ({ state with
      parser
      terminal := result.state
    }, result.responses)

/-- Handle window resize -/
def handleResize (state : AppState) (newWidth newHeight : Float) : IO AppState := do
  if newWidth == state.windowWidth && newHeight == state.windowHeight then
    pure state
  else
    let (newCols, newRows) := calculateDimensions
      newWidth newHeight state.cellWidth state.cellHeight
      state.config.paddingX state.config.paddingY

    -- Resize PTY
    state.pty.resize newCols.toUInt16 newRows.toUInt16

    -- Resize terminal state
    let terminal := state.terminal.resize newCols newRows

    pure { state with
      windowWidth := newWidth
      windowHeight := newHeight
      termCols := newCols
      termRows := newRows
      terminal
    }

/-- Clear dirty flags after rendering -/
def clearDirty (state : AppState) : AppState :=
  { state with terminal := state.terminal.clearDirty }

/-- Clear selection -/
def clearSelection (state : AppState) : AppState :=
  { state with selection := none }

/-- Convert pixel coordinates to cell coordinates -/
def pixelToCell (state : AppState) (px py : Float) : Nat × Nat :=
  let col := ((px - state.config.paddingX) / state.cellWidth).toUInt32.toNat
  let row := ((py - state.config.paddingY) / state.cellHeight).toUInt32.toNat
  -- Clamp to valid range
  let col := min col (state.termCols - 1)
  let row := min row (state.termRows - 1)
  (col, row)

/-- Extract selected text as a string -/
def getSelectedText (state : AppState) : String :=
  match state.selection with
  | none => ""
  | some sel =>
    let sel := sel.normalize
    let buffer := state.terminal.currentBuffer
    Id.run do
      let mut result := ""
      for row in [sel.startRow : sel.endRow + 1] do
        let startCol := if row == sel.startRow then sel.startCol else 0
        let endCol := if row == sel.endRow then sel.endCol else state.termCols
        -- Get characters for this row
        for col in [startCol : endCol] do
          let cell := buffer.get col row
          let text := cell.text
          if !text.isEmpty then
            result := result ++ text
        -- Add newline between rows (not after last)
        if row < sel.endRow then
          result := result.push '\n'
      -- Trim trailing spaces from each line
      result

end AppState

end Vane.App
