/-
  Vane Terminal Emulator - Main Entry Point
-/

import Vane.App.Config
import Vane.App.Loop

def main : IO Unit := do
  let config : Vane.App.Config := {
    fontPath := "/System/Library/Fonts/Menlo.ttc"
    fontSize := 14
    shell := "/bin/zsh"
    windowTitle := "Vane Terminal"
    initialCols := 100
    initialRows := 30
    maxScrollback := 10000
    cursorBlinkMs := 530
    paddingX := 8.0
    paddingY := 8.0
  }

  Vane.App.Loop.run config
