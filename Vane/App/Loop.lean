/-
  Vane.App.Loop - Main event loop for terminal emulator
-/

import Afferent
import Vane.App.Config
import Vane.App.State
import Vane.App.Input
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
def pollAndProcessPty (state : AppState) : IO AppState := do
  -- Poll with 0 timeout (non-blocking)
  let hasData ← state.pty.poll 0
  if hasData then
    let bytes ← state.pty.read 65536
    pure (state.processOutput bytes)
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
  Grid.render canvas params state.terminal state.cursorVisible

/-- Initialize resources and create app state -/
def init (config : Config) : IO (Canvas × AppState) := do
  -- Initialize afferent
  Afferent.FFI.init

  -- Estimate window size from config
  -- Approximate cell dimensions for Menlo 14pt: charWidth ~8.4, cellHeight ~17
  -- These are conservative estimates; actual rendering will use font metrics
  let estimatedCellWidth : Float := config.fontSize.toFloat * 0.6
  let estimatedCellHeight : Float := config.fontSize.toFloat * 1.2

  let windowWidth := config.paddingX * 2 + estimatedCellWidth * config.initialCols.toFloat
  let windowHeight := config.paddingY * 2 + estimatedCellHeight * config.initialRows.toFloat

  -- Create the single canvas at estimated size
  let canvas ← Canvas.create windowWidth.toUInt32 windowHeight.toUInt32 config.windowTitle

  -- Now load font and measure actual cell dimensions
  let font ← Font.load config.fontPath config.fontSize
  let (charWidth, _) ← font.measureText "M"
  let cellHeight := font.lineHeight

  -- Open PTY with shell
  let pty ← PTY.open config.shell config.initialCols.toUInt16 config.initialRows.toUInt16

  -- Create application state with actual cell dimensions
  let state ← AppState.create config canvas font pty

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

    -- Poll and process PTY output
    s ← pollAndProcessPty s

    -- Handle keyboard input (set debug=true to see key codes)
    s ← handleKeyboard s c (debug := true)

    -- Update cursor blink
    let now ← IO.monoMsNow
    s := s.updateBlink now

    -- Begin frame
    let ok ← c.beginFrame bgColor
    if ok then
      -- Render terminal
      c ← renderFrame c s

      -- Clear dirty flags
      s := s.clearDirty

      -- End frame
      c ← c.endFrame

  -- Cleanup
  s.pty.close
  IO.println "Terminal closed."

end Vane.App.Loop
