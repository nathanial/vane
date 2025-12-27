/-
  Vane Terminal Emulator - Main Entry Point
-/

import Vane

open Vane
open Vane.PTY
open Vane.Parser

/-- Demo: Spawn a shell, read output, and parse ANSI sequences -/
def main : IO Unit := do
  IO.println "╔══════════════════════════════════════════════════════════════╗"
  IO.println "║           Vane Terminal Emulator v0.1.0                      ║"
  IO.println "║           ANSI Parser Demo                                   ║"
  IO.println "╚══════════════════════════════════════════════════════════════╝"
  IO.println ""

  -- Spawn a shell
  IO.println "Spawning shell (/bin/zsh)..."
  let pty ← PTY.openDefault "/bin/zsh"
  IO.println "Shell spawned successfully!"
  IO.println ""

  -- Give the shell a moment to initialize
  IO.sleep 100

  -- Initialize parser
  let mut parser := Parser.new

  -- Read initial output and parse it
  IO.println "Reading and parsing initial shell output..."
  IO.println "─────────────────────────────────────────"

  for _ in [0:5] do
    let hasData ← pty.poll 50
    if hasData then
      let bytes ← pty.read 4096
      if bytes.size > 0 then
        let (newParser, actions) := parser.process bytes
        parser := newParser

        -- Show raw output
        IO.println s!"Raw bytes: {bytes.size}"

        -- Show parsed actions
        IO.println s!"Parsed {actions.size} actions:"
        for action in actions do
          match action with
          | .print c => IO.print s!"{c}"
          | .execute b =>
            if b == C0.LF then IO.println "" -- newline
            else if b == C0.CR then pure ()  -- ignore CR
            else if b == C0.BEL then IO.println "[BEL]"
            else if b == C0.BS then IO.print "[BS]"
            else IO.print s!"[C0:{b}]"
          | .csiDispatch params intermediates final =>
            let cmd := TerminalCommand.fromCSI params intermediates final
            IO.println s!"[CSI: {repr cmd}]"
          | .escDispatch intermediates final =>
            IO.println s!"[ESC {intermediates} {final}]"
          | .oscDispatch params =>
            let cmd := OSCCommand.fromParams params
            IO.println s!"[OSC: {repr cmd}]"
          | _ => IO.print s!"[{repr action}]"
    else
      break

  IO.println ""
  IO.println "─────────────────────────────────────────"
  IO.println ""

  -- Send a command with colors
  IO.println "Sending command: ls --color=auto"
  pty.writeString "ls --color=auto\n"

  -- Wait for response and parse it
  IO.sleep 300

  IO.println "Parsing colorized output..."
  IO.println "─────────────────────────────────────────"

  for _ in [0:20] do
    let hasData ← pty.poll 100
    if hasData then
      let bytes ← pty.read 4096
      if bytes.size > 0 then
        let (newParser, actions) := parser.process bytes
        parser := newParser

        for action in actions do
          match action with
          | .print c => IO.print s!"{c}"
          | .execute b =>
            if b == C0.LF then IO.println ""
            else if b == C0.CR then pure ()
            else pure ()
          | .csiDispatch params intermediates final =>
            let cmd := TerminalCommand.fromCSI params intermediates final
            match cmd with
            | .sgr state =>
              -- Show color changes
              if state.reset then IO.print "[RESET]"
              else if state.bold == some true then IO.print "[BOLD]"
              else
                match state.fg with
                | some (.ansi c) => IO.print s!"[FG:{repr c}]"
                | some (.indexed n) => IO.print s!"[FG256:{n}]"
                | some (.rgb r g b) => IO.print s!"[FG:{r},{g},{b}]"
                | _ => pure ()
            | _ => pure ()
          | _ => pure ()

  IO.println ""
  IO.println "─────────────────────────────────────────"
  IO.println ""

  -- Clean test: print "Hello" with bold
  IO.println "Testing specific ANSI sequences..."
  pty.writeString "printf '\\e[1mBOLD\\e[0m \\e[31mRED\\e[0m \\e[38;5;208mORANGE256\\e[0m \\e[38;2;255;165;0mORANGE_RGB\\e[0m\\n'\n"
  IO.sleep 200

  IO.println "─────────────────────────────────────────"
  for _ in [0:10] do
    let hasData ← pty.poll 100
    if hasData then
      let bytes ← pty.read 4096
      if bytes.size > 0 then
        let (newParser, actions) := parser.process bytes
        parser := newParser

        for action in actions do
          match action with
          | .print c => IO.print s!"{c}"
          | .execute b =>
            if b == C0.LF then IO.println ""
            else if b == C0.CR then pure ()
            else pure ()
          | .csiDispatch params intermediates final =>
            let cmd := TerminalCommand.fromCSI params intermediates final
            IO.print s!"[{repr cmd}]"
          | _ => pure ()

  IO.println ""
  IO.println "─────────────────────────────────────────"

  -- Exit
  IO.println ""
  IO.println "Sending exit command..."
  pty.writeString "exit\n"
  IO.sleep 100

  pty.close
  IO.println "Parser demo complete!"
  IO.println s!"Final parser state: {repr parser.state}"
