/-
  Vane.App.Loop - Main event loop for terminal emulator
-/

import Afferent
import Vane.App.Config
import Vane.App.State
import Vane.App.Input
import Vane.App.Clipboard
import Vane.Render.Grid
import Vane.PTY.FFI
import Vane.Input.KeyEncoder

namespace Vane.App.Loop

open Afferent
open Vane.App
open Vane.PTY
open Vane.Input
open Vane.Render

/-- Poll PTY for output and process it -/
def pollAndProcessPty (state : AppState) (debug : Bool := false) : IO AppState := do
  -- Poll with 0 timeout (non-blocking)
  let hasData ← state.pty.poll 0
  if hasData then
    let bytes ← state.pty.read 65536
    let (newState, responses) := state.processOutput bytes
    -- Send any responses (like cursor position reports) back to the PTY
    for response in responses do
      let respBytes := response.toBytes
      if debug && !respBytes.isEmpty then
        IO.eprintln s!"[DEBUG] Sending response: {respBytes.size} bytes"
      state.pty.write respBytes
    pure newState
  else
    pure state

/-- Find word boundaries at a given position (for double-click selection) -/
def findWordAt (state : AppState) (col row : Nat) : SelectionRange :=
  let buffer := state.terminal.currentBuffer
  let isWordChar (c : Char) : Bool := c.isAlphanum || c == '_' || c == '-'
  Id.run do
    -- Scan left for word boundary
    let mut startCol := col
    while startCol > 0 do
      let cell := buffer.get (startCol - 1) row
      if !isWordChar cell.char then break
      startCol := startCol - 1
    -- Scan right for word boundary
    let mut endCol := col + 1
    while endCol < state.termCols do
      let cell := buffer.get endCol row
      if !isWordChar cell.char then break
      endCol := endCol + 1
    { startCol, startRow := row, endCol, endRow := row }

/-- Handle mouse input for text selection -/
def handleMouse (state : AppState) (canvas : Canvas) : IO AppState := do
  -- Get mouse state
  let (mx, my) ← canvas.ctx.window.getMousePos
  let buttons ← canvas.ctx.window.getMouseButtons
  let now ← IO.monoMsNow

  -- Convert to cell coordinates
  let (col, row) := state.pixelToCell mx my

  -- Check if left button is pressed
  let leftDown := (buttons &&& 1) != 0

  -- Mouse button just pressed (transition from up to down)
  if leftDown && !state.mouseDown then
    -- Check for multi-click (within 400ms and same cell)
    let timeSinceLastClick := now - state.lastClickTime
    let sameCell := col == state.lastClickCol && row == state.lastClickRow
    let isMultiClick := timeSinceLastClick < 400 && sameCell

    let newClickCount := if isMultiClick then
      if state.clickCount >= 3 then 1 else state.clickCount + 1
    else
      1

    let baseState := { state with
      mouseDown := true
      clickCount := newClickCount
      lastClickTime := now
      lastClickCol := col
      lastClickRow := row
    }

    -- Handle selection based on click count
    if newClickCount == 3 then
      -- Triple-click: select entire line
      pure { baseState with
        selection := some { startCol := 0, startRow := row, endCol := state.termCols, endRow := row }
      }
    else if newClickCount == 2 then
      -- Double-click: select word
      let wordSel := findWordAt state col row
      pure { baseState with selection := some wordSel }
    else
      -- Single click: start new selection
      pure { baseState with
        selection := some { startCol := col, startRow := row, endCol := col + 1, endRow := row }
      }

  -- Mouse drag (extend selection)
  else if leftDown && state.mouseDown then
    match state.selection with
    | some sel =>
      -- Extend selection from original start to current position
      pure { state with
        selection := some { sel with endCol := col + 1, endRow := row }
      }
    | none => pure state

  -- Mouse button released
  else if !leftDown && state.mouseDown then
    pure { state with mouseDown := false }

  else
    pure state

/-- Handle keyboard input -/
def handleKeyboard (state : AppState) (canvas : Canvas) (debug : Bool := false) : IO AppState := do
  -- Use hasKeyPressed to properly detect key code 0 (the 'a' key on macOS)
  let hasKey ← canvas.hasKeyPressed
  if !hasKey then
    pure state
  else
    let keyCode ← canvas.getKeyCode
    canvas.clearKey
    let modBits ← canvas.ctx.window.getModifiers

    if debug then
      IO.eprintln s!"[DEBUG] keyCode={keyCode} modBits={modBits}"

    -- Check for Cmd modifier (bit 8 = 0x100)
    let hasCmd := (modBits &&& 0x100) != 0

    -- Handle clipboard shortcuts (Cmd+C = copy, Cmd+V = paste)
    -- macOS key codes: C = 8, V = 9
    if hasCmd && keyCode == 8 then
      -- Cmd+C: Copy selection to clipboard
      let text := state.getSelectedText
      if !text.isEmpty then
        Clipboard.set text
      pure state
    else if hasCmd && keyCode == 9 then
      -- Cmd+V: Paste from clipboard to PTY
      let text ← Clipboard.get
      if !text.isEmpty then
        state.pty.write text.toUTF8
      -- Clear selection after paste
      pure state.clearSelection
    else
      -- Normal key handling
      match Input.mapKeyCode keyCode modBits with
      | some input =>
        let bytes := Input.encodeForPty input state.terminal.modes.applicationCursorKeys
        if debug then
          IO.eprintln s!"[DEBUG] mapped to input, sending {bytes.size} bytes"
        state.pty.write bytes
        pure state
      | none =>
        if debug then
          IO.eprintln s!"[DEBUG] keyCode {keyCode} not mapped"
        pure state

/-- Render a single frame -/
def renderFrame (canvas : Canvas) (state : AppState) : IO Canvas := do
  let params := state.toRenderParams
  Grid.render canvas params state.terminal state.cursorVisible state.selection

/-- Initialize resources and create app state -/
def init (config : Config) : IO (Canvas × AppState) := do
  -- Initialize afferent
  Afferent.FFI.init

  -- Get screen scale factor for HiDPI displays
  let screenScale ← FFI.getScreenScale

  -- Estimate window size from config (in logical pixels)
  -- Approximate cell dimensions for Menlo 14pt: charWidth ~8.4, cellHeight ~17
  -- These are conservative estimates; actual rendering will use font metrics
  let estimatedCellWidth : Float := config.fontSize.toFloat * 0.6
  let estimatedCellHeight : Float := config.fontSize.toFloat * 1.2

  let windowWidth := config.paddingX * 2 + estimatedCellWidth * config.initialCols.toFloat
  let windowHeight := config.paddingY * 2 + estimatedCellHeight * config.initialRows.toFloat

  -- Create canvas with screen scale (window in physical pixels, we'll scale down for logical)
  let physicalWidth := (windowWidth * screenScale).toUInt32
  let physicalHeight := (windowHeight * screenScale).toUInt32
  let canvas ← Canvas.createWithScale physicalWidth physicalHeight config.windowTitle screenScale

  -- Load font with scaled size for crisp rendering on HiDPI
  let font ← Font.loadScaled config.fontPath config.fontSize.toFloat screenScale

  -- Open PTY with shell
  let pty ← PTY.open config.shell config.initialCols.toUInt16 config.initialRows.toUInt16

  -- Create application state with actual cell dimensions (pass screenScale for metric conversion)
  let state ← AppState.create config canvas font pty screenScale

  pure (canvas, state)

/-- Main event loop -/
def run (config : Config) : IO Unit := do
  let (canvas, state) ← init config

  let mut c := canvas
  let mut s := state

  -- Background color
  let bgColor := Color.rgba 0.08 0.08 0.1 1.0

  while !(← c.shouldClose) do
    -- Check if shell is still alive
    let alive ← s.pty.isAlive
    if !alive then
      break

    -- Poll window events
    c.pollEvents

    -- Handle window resize
    let (windowWidth, windowHeight) ← c.ctx.getCurrentSize
    s ← s.handleResize windowWidth windowHeight

    -- Poll and process PTY output
    s ← pollAndProcessPty s

    -- Handle mouse input (text selection)
    s ← handleMouse s c

    -- Handle keyboard input
    s ← handleKeyboard s c

    -- Update cursor blink
    let now ← IO.monoMsNow
    s := s.updateBlink now

    -- Begin frame
    let ok ← c.beginFrame bgColor
    if ok then
      -- Render terminal (all coordinates in physical pixels for HiDPI)
      c ← renderFrame c s

      -- Clear dirty flags
      s := s.clearDirty

      -- End frame
      c ← c.endFrame

  -- Cleanup
  s.pty.close
  IO.println "Terminal closed."

end Vane.App.Loop
