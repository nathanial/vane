/-
  Vane.App.Config - Terminal configuration
-/

namespace Vane.App

/-- Terminal emulator configuration -/
structure Config where
  /-- Path to monospace font file -/
  fontPath : String := "/System/Library/Fonts/Menlo.ttc"
  /-- Font size in pixels -/
  fontSize : UInt32 := 14
  /-- Shell command to execute -/
  shell : String := "/bin/zsh"
  /-- Window title -/
  windowTitle : String := "Vane Terminal"
  /-- Initial terminal width in columns -/
  initialCols : Nat := 100
  /-- Initial terminal height in rows -/
  initialRows : Nat := 30
  /-- Maximum scrollback buffer size -/
  maxScrollback : Nat := 10000
  /-- Cursor blink interval in milliseconds -/
  cursorBlinkMs : Nat := 530
  /-- Horizontal padding in pixels -/
  paddingX : Float := 8.0
  /-- Vertical padding in pixels -/
  paddingY : Float := 8.0
  deriving Repr, Inhabited

namespace Config

/-- Default configuration -/
def default : Config := {}

/-- Configuration with custom font size -/
def withFontSize (c : Config) (size : UInt32) : Config :=
  { c with fontSize := size }

/-- Configuration with custom shell -/
def withShell (c : Config) (shell : String) : Config :=
  { c with shell := shell }

/-- Configuration with custom dimensions -/
def withSize (c : Config) (cols rows : Nat) : Config :=
  { c with initialCols := cols, initialRows := rows }

end Config

end Vane.App
