/-
  Vane Tests - Test suite for the terminal emulator
-/

import Crucible
import Vane

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

#generate_tests

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
