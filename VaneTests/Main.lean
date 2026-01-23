/-
  Vane Tests - Test suite for the terminal emulator
-/

import Crucible
import Vane
import VaneTests.InputTests

open Crucible
open Vane

namespace VaneTests

testSuite "Style Tests"

test "Color16 values are distinct" := do
  ensure (Color16.black != Color16.white) "black should not equal white"

test "Color.default is default" := do
  ensure ((Color.default : Color) == Color.default) "default should equal default"

test "Color.rgb construction" := do
  let c := Color.rgb 255 128 0
  match c with
  | .rgb r g b => ensure (r == 255 && g == 128 && b == 0) "RGB values should match"
  | _ => ensure false "expected RGB color"

test "Color.indexed construction" := do
  let c := Color.indexed 42
  match c with
  | .indexed n => ensure (n == 42) "indexed value should match"
  | _ => ensure false "expected indexed color"

test "Modifier merge combines flags" := do
  let m1 := Modifier.mkBold
  let m2 := Modifier.mkItalic
  let merged := Modifier.merge m1 m2
  ensure (merged.bold && merged.italic && !merged.underline) "merged should have bold and italic"

test "Style merge preserves non-default" := do
  let s1 := Style.fgColor Color.red
  let s2 := Style.bgColor Color.blue
  let merged := Style.merge s1 s2
  ensure (merged.fg == Color.red && merged.bg == Color.blue) "merged style should preserve colors"

testSuite "Cell Tests"

test "Cell.empty is space" := do
  ensure (Cell.empty.char == ' ') "empty cell should be space"

test "Cell.new sets char" := do
  ensure ((Cell.new 'A').char == 'A') "new cell should have char A"

test "Cell.styled sets all fields" := do
  let cell := Cell.styled 'X' Color.red Color.blue Modifier.mkBold
  ensure (cell.char == 'X' && cell.fg == Color.red && cell.bg == Color.blue && cell.modifier.bold)
    "styled cell should have all fields set"

test "Cell.withChar preserves style" := do
  let cell := Cell.styled 'A' Color.green Color.default {}
  let cell' := cell.withChar 'B'
  ensure (cell'.char == 'B' && cell'.fg == Color.green) "withChar should preserve fg color"

test "Cell.isEmpty for empty cell" := do
  ensure Cell.empty.isEmpty "empty cell should be isEmpty"

test "Cell.isEmpty false for styled cell" := do
  ensure (!(Cell.styled 'A' Color.red Color.default {}).isEmpty) "styled cell should not be isEmpty"

testSuite "Buffer Tests"

test "Buffer.create has correct size" := do
  let buf := Buffer.create 80 24
  ensure (buf.width == 80 && buf.height == 24) "buffer should be 80x24"

test "Buffer.get returns empty for new buffer" := do
  let buf := Buffer.create 10 10
  ensure ((buf.get 5 5).char == ' ') "new buffer cell should be space"

test "Buffer.set and get roundtrip" := do
  let buf := Buffer.create 10 10
  let cell := Cell.new 'X'
  let buf' := buf.set 3 4 cell
  ensure ((buf'.get 3 4).char == 'X') "set/get should roundtrip"

test "Buffer.get out of bounds returns empty" := do
  let buf := Buffer.create 10 10
  let buf' := buf.set 5 5 (Cell.new 'A')
  ensure ((buf'.get 100 100).char == ' ') "out of bounds should return space"

test "Buffer.set out of bounds is no-op" := do
  let buf := Buffer.create 10 10
  let buf' := buf.set 100 100 (Cell.new 'X')
  ensure (buf'.cells.size == buf.cells.size) "out of bounds set should preserve size"

test "Buffer.writeString writes text" := do
  let buf := Buffer.create 20 5
  let buf' := buf.writeString 0 0 "Hello"
  ensure ((buf'.get 0 0).char == 'H' &&
          (buf'.get 1 0).char == 'e' &&
          (buf'.get 4 0).char == 'o') "writeString should write characters"

test "Buffer.writeString handles combining marks" := do
  let buf := Buffer.create 5 1
  let buf' := buf.writeString 0 0 "e\u0301"
  let cell := buf'.get 0 0
  let next := buf'.get 1 0
  ensure (cell.char == 'e') "base character should be stored"
  ensure (cell.combining.size == 1) "combining mark should be stored"
  ensure (cell.text == "e\u0301") "cell.text should include combining mark"
  ensure (next.char == ' ' && next.combining.isEmpty) "next cell should be empty"

test "Buffer.writeString handles wide characters" := do
  let buf := Buffer.create 4 1
  let buf' := buf.writeString 0 0 "\u4E2DA"
  let lead := buf'.get 0 0
  let cont := buf'.get 1 0
  let after := buf'.get 2 0
  ensure (lead.char == '\u4E2D' && lead.width == 2) "lead cell should store wide char"
  ensure (cont.char == '\x00' && cont.width == 0) "continuation cell should be placeholder"
  ensure (after.char == 'A') "character after wide char should advance by 2"

test "Buffer.clear resets all cells" := do
  let buf := Buffer.create 10 10
  let buf' := buf.set 5 5 (Cell.new 'X')
  let buf'' := buf'.clear
  ensure ((buf''.get 5 5).char == ' ') "clear should reset cells"

test "Buffer.scrollUp moves content" := do
  let buf := Buffer.create 10 5
  let buf' := buf.writeString 0 0 "Line0"
  let buf'' := buf'.writeString 0 1 "Line1"
  let buf''' := buf''.scrollUp 1
  ensure ((buf'''.get 0 0).char == 'L' && (buf'''.get 4 0).char == '1')
    "scrollUp should move Line1 to row 0"

test "Buffer.resize preserves content" := do
  let buf := Buffer.create 10 10
  let buf' := buf.writeString 0 0 "Test"
  let buf'' := buf'.resize 20 20
  ensure (buf''.width == 20 && buf''.height == 20 && (buf''.get 0 0).char == 'T')
    "resize should preserve content"

-- Parser Tests
open Vane.Parser

testSuite "Parser SGR Tests"

test "SGR reset (0)" := do
  let state := parseSGR #[some 0]
  ensure state.reset "SGR 0 should set reset"

test "SGR bold (1)" := do
  let state := parseSGR #[some 1]
  ensure (state.bold == some true) "SGR 1 should set bold"

test "SGR italic (3)" := do
  let state := parseSGR #[some 3]
  ensure (state.italic == some true) "SGR 3 should set italic"

test "SGR underline (4)" := do
  let state := parseSGR #[some 4]
  ensure (state.underline == some true) "SGR 4 should set underline"

test "SGR reverse (7)" := do
  let state := parseSGR #[some 7]
  ensure (state.reverse == some true) "SGR 7 should set reverse"

test "SGR foreground red (31)" := do
  let state := parseSGR #[some 31]
  ensure (state.fg == some (Color.ansi .red)) "SGR 31 should set fg to red"

test "SGR background blue (44)" := do
  let state := parseSGR #[some 44]
  ensure (state.bg == some (Color.ansi .blue)) "SGR 44 should set bg to blue"

test "SGR bright foreground (90-97)" := do
  let state := parseSGR #[some 92]
  ensure (state.fg == some (Color.ansi .brightGreen)) "SGR 92 should set bright green"

test "SGR 256-color foreground (38;5;n)" := do
  let state := parseSGR #[some 38, some 5, some 208]
  match state.fg with
  | some (Color.indexed n) => ensure (n == 208) "should be color 208"
  | _ => ensure false "expected indexed color"

test "SGR RGB foreground (38;2;r;g;b)" := do
  let state := parseSGR #[some 38, some 2, some 255, some 128, some 64]
  match state.fg with
  | some (Color.rgb r g b) => ensure (r == 255 && g == 128 && b == 64) "should be RGB(255,128,64)"
  | _ => ensure false "expected RGB color"

test "SGR multiple params" := do
  let state := parseSGR #[some 1, some 4, some 31]
  ensure (state.bold == some true && state.underline == some true && state.fg == some (Color.ansi .red))
    "should parse bold, underline, and red"

test "SGR empty params means reset" := do
  let state := parseSGR #[]
  ensure state.reset "empty SGR should reset"

testSuite "Parser CSI Tests"

test "CSI cursor up (A)" := do
  let cmd := TerminalCommand.fromCSI #[some 5] #[] 0x41
  match cmd with
  | .cursorUp n => ensure (n == 5) "CUU should move 5"
  | _ => ensure false "expected cursorUp"

test "CSI cursor down (B)" := do
  let cmd := TerminalCommand.fromCSI #[some 3] #[] 0x42
  match cmd with
  | .cursorDown n => ensure (n == 3) "CUD should move 3"
  | _ => ensure false "expected cursorDown"

test "CSI cursor position (H)" := do
  let cmd := TerminalCommand.fromCSI #[some 10, some 20] #[] 0x48
  match cmd with
  | .cursorPosition row col => ensure (row == 10 && col == 20) "CUP should be 10,20"
  | _ => ensure false "expected cursorPosition"

test "CSI cursor position defaults (H)" := do
  let cmd := TerminalCommand.fromCSI #[] #[] 0x48
  match cmd with
  | .cursorPosition row col => ensure (row == 1 && col == 1) "CUP default should be 1,1"
  | _ => ensure false "expected cursorPosition"

test "CSI erase display (J)" := do
  let cmd := TerminalCommand.fromCSI #[some 2] #[] 0x4A
  match cmd with
  | .eraseDisplay mode => ensure (mode == 2) "ED should be mode 2"
  | _ => ensure false "expected eraseDisplay"

test "CSI erase line (K)" := do
  let cmd := TerminalCommand.fromCSI #[] #[] 0x4B
  match cmd with
  | .eraseLine mode => ensure (mode == 0) "EL default should be 0"
  | _ => ensure false "expected eraseLine"

test "CSI scroll up (S)" := do
  let cmd := TerminalCommand.fromCSI #[some 3] #[] 0x53
  match cmd with
  | .scrollUp n => ensure (n == 3) "SU should scroll 3"
  | _ => ensure false "expected scrollUp"

test "CSI set scroll region (r)" := do
  let cmd := TerminalCommand.fromCSI #[some 5, some 20] #[] 0x72
  match cmd with
  | .setScrollRegion top bottom => ensure (top == 5 && bottom == 20) "DECSTBM should be 5,20"
  | _ => ensure false "expected setScrollRegion"

test "CSI SGR (m)" := do
  let cmd := TerminalCommand.fromCSI #[some 1, some 31] #[] 0x6D
  match cmd with
  | .sgr state => ensure (state.bold == some true && state.fg == some (Color.ansi .red)) "should be bold red"
  | _ => ensure false "expected sgr"

test "CSI soft reset (p with !)" := do
  let cmd := TerminalCommand.fromCSI #[] #[0x21] 0x70
  match cmd with
  | .softReset => ensure true "should be soft reset"
  | _ => ensure false "expected softReset"

testSuite "Parser State Machine Tests"

test "Parser starts in ground state" := do
  let p := Parser.new
  ensure (p.state == .ground) "parser should start in ground"

test "Parser prints ASCII characters" := do
  let p := Parser.new
  let (_, actions) := p.process (ByteArray.mk #['A'.toNat.toUInt8])
  ensure (actions.size == 1) "should have 1 action"
  match actions[0]? with
  | some (Action.print c) => ensure (c == 'A') "should print A"
  | _ => ensure false "expected print action"

test "Parser executes C0 controls" := do
  let p := Parser.new
  let (_, actions) := p.process (ByteArray.mk #[0x07])  -- BEL
  ensure (actions.size == 1) "should have 1 action"
  match actions[0]? with
  | some (Action.execute b) => ensure (b == 0x07) "should execute BEL"
  | _ => ensure false "expected execute action"

test "Parser handles ESC [" := do
  let p := Parser.new
  let (p', _) := p.process (ByteArray.mk #[0x1B, 0x5B])  -- ESC [
  ensure (p'.state == .csiEntry) "should be in CSI entry"

test "Parser dispatches CSI sequence" := do
  let p := Parser.new
  -- ESC [ 5 A (cursor up 5)
  let (p', actions) := p.process (ByteArray.mk #[0x1B, 0x5B, 0x35, 0x41])
  ensure (p'.state == .ground) "should return to ground"
  -- Find the CSI dispatch action
  let csiAction := actions.find? fun a => match a with
    | .csiDispatch _ _ _ => true
    | _ => false
  ensure csiAction.isSome "should have CSI dispatch"

test "Parser handles OSC" := do
  let p := Parser.new
  -- ESC ] 0 ; title BEL
  let bytes := ByteArray.mk #[0x1B, 0x5D, 0x30, 0x3B, 0x54, 0x45, 0x53, 0x54, 0x07]
  let (p', actions) := p.process bytes
  ensure (p'.state == .ground) "should return to ground"
  let oscAction := actions.find? fun a => match a with
    | .oscDispatch _ => true
    | _ => false
  ensure oscAction.isSome "should have OSC dispatch"

test "Parser CAN aborts sequence" := do
  let p := Parser.new
  -- Start CSI then cancel
  let (p', _) := p.process (ByteArray.mk #[0x1B, 0x5B, 0x18])  -- ESC [ CAN
  ensure (p'.state == .ground) "CAN should return to ground"

testSuite "Parser OSC Tests"

test "OSC 0 sets title" := do
  let cmd := OSCCommand.fromParams #["0", "My Title"]
  match cmd with
  | .setTitle t => ensure (t == "My Title") "should set title"
  | _ => ensure false "expected setTitle"

test "OSC 2 sets window title" := do
  let cmd := OSCCommand.fromParams #["2", "Window"]
  match cmd with
  | .setWindowTitle t => ensure (t == "Window") "should set window title"
  | _ => ensure false "expected setWindowTitle"

test "OSC 7 sets current directory" := do
  let cmd := OSCCommand.fromParams #["7", "file:///home/user"]
  match cmd with
  | .setCurrentDirectory uri => ensure (uri == "file:///home/user") "should set directory"
  | _ => ensure false "expected setCurrentDirectory"

test "OSC 8 sets hyperlink" := do
  let cmd := OSCCommand.fromParams #["8", "id=test", "https://example.com"]
  match cmd with
  | .hyperlink params uri => ensure (params == "id=test" && uri == "https://example.com") "should set hyperlink"
  | _ => ensure false "expected hyperlink"

test "OSC 52 sets clipboard" := do
  let cmd := OSCCommand.fromParams #["52", "c", "SGVsbG8="]
  match cmd with
  | .setClipboard clipboard data => ensure (clipboard == "c" && data == "SGVsbG8=") "should set clipboard"
  | _ => ensure false "expected setClipboard"

test "OSC 52 queries clipboard" := do
  let cmd := OSCCommand.fromParams #["52", "c", "?"]
  match cmd with
  | .queryClipboard clipboard => ensure (clipboard == "c") "should query clipboard"
  | _ => ensure false "expected queryClipboard"



end VaneTests

def main : IO UInt32 := do
  IO.println "╔══════════════════════════════════════════════════════════════╗"
  IO.println "║                      Vane Test Suite                         ║"
  IO.println "╚══════════════════════════════════════════════════════════════╝"
  IO.println ""

  let result ← runAllSuites

  IO.println ""
  if result == 0 then
    IO.println "All tests passed!"
  else
    IO.println "Some tests failed"

  return result
