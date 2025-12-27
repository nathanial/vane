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
def handleKeyboard (state : AppState) (canvas : Canvas) : IO AppState := do
  let keyCode ← canvas.getKeyCode
  if keyCode == 0 then
    pure state
  else
    canvas.clearKey
    let modBits ← canvas.ctx.window.getModifiers

    match Input.mapKeyCode keyCode modBits with
    | some input =>
      let bytes := Input.encodeForPty input state.terminal.modes.applicationCursorKeys
      state.pty.write bytes
      pure state
    | none =>
      pure state

/-- Render a single frame -/
def renderFrame (canvas : Canvas) (state : AppState) : IO Canvas := do
  let params := state.toRenderParams
  Grid.render canvas params state.terminal state.cursorVisible

/-- Initialize resources and create app state -/
def init (config : Config) : IO (Canvas × AppState) := do
  -- Calculate window size based on desired terminal size
  -- We need to load font first to know cell dimensions
  Afferent.FFI.init

  -- Create a temporary window to load font and measure
  let tempCanvas ← Canvas.create 800 600 config.windowTitle
  let font ← Font.load config.fontPath config.fontSize
  let (charWidth, _) ← font.measureText "M"
  let cellHeight := font.lineHeight

  -- Calculate actual window size
  let windowWidth := config.paddingX * 2 + charWidth * config.initialCols.toFloat
  let windowHeight := config.paddingY * 2 + cellHeight * config.initialRows.toFloat

  -- Destroy temp canvas and create properly sized one
  -- Note: Canvas.destroy doesn't exist, so we'll just create a new one
  -- The old one will be garbage collected
  let canvas ← Canvas.create windowWidth.toUInt32 windowHeight.toUInt32 config.windowTitle

  -- Open PTY with shell
  let pty ← PTY.open config.shell config.initialCols.toUInt16 config.initialRows.toUInt16

  -- Create application state
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

    -- Handle keyboard input
    s ← handleKeyboard s c

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
