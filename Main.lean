/-
  Vane Terminal Emulator - Main Entry Point
-/

import Vane

open Vane
open Vane.PTY

/-- Demo: Spawn a shell and read its output -/
def main : IO Unit := do
  IO.println "╔══════════════════════════════════════════════════════════════╗"
  IO.println "║           Vane Terminal Emulator v0.1.0                      ║"
  IO.println "║           PTY Integration Demo                               ║"
  IO.println "╚══════════════════════════════════════════════════════════════╝"
  IO.println ""

  -- Spawn a shell
  IO.println "Spawning shell (/bin/zsh)..."
  let pty ← PTY.openDefault "/bin/zsh"
  IO.println "Shell spawned successfully!"
  IO.println ""

  -- Give the shell a moment to initialize
  IO.sleep 100

  -- Read initial output (shell prompt, etc.)
  IO.println "Reading initial shell output..."
  IO.println "─────────────────────────────────────────"

  -- Read any available output
  for _ in [0:5] do
    let hasData ← pty.poll 50
    if hasData then
      let output ← pty.readString
      if !output.isEmpty then
        IO.print output
    else
      break

  IO.println ""
  IO.println "─────────────────────────────────────────"
  IO.println ""

  -- Send a simple command
  IO.println "Sending command: echo 'Hello from Vane!'"
  pty.writeString "echo 'Hello from Vane!'\n"

  -- Wait for response and read it
  IO.sleep 200

  IO.println "Reading response..."
  IO.println "─────────────────────────────────────────"

  for _ in [0:20] do
    let hasData ← pty.poll 100
    if hasData then
      let output ← pty.readString
      if !output.isEmpty then
        IO.print output
  IO.println ""
  IO.println "─────────────────────────────────────────"
  IO.println ""

  -- Run another command to show it's really working
  IO.println "Sending command: pwd"
  pty.writeString "pwd\n"
  IO.sleep 200

  IO.println "Reading response..."
  IO.println "─────────────────────────────────────────"
  for _ in [0:20] do
    let hasData ← pty.poll 100
    if hasData then
      let output ← pty.readString
      if !output.isEmpty then
        IO.print output
  IO.println ""
  IO.println "─────────────────────────────────────────"
  IO.println ""

  -- Send exit command
  IO.println "Sending exit command..."
  pty.writeString "exit\n"
  IO.sleep 100

  -- Check if shell exited
  let alive ← pty.isAlive
  IO.println s!"Shell still alive: {alive}"

  -- Clean up
  IO.println "Closing PTY..."
  pty.close
  IO.println ""
  IO.println "PTY demo complete!"
