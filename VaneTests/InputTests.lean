/-
  VaneTests.InputTests - Tests for keyboard input mapping
-/

import Crucible
import Vane.App.Input
import Vane.Input.KeyEncoder

open Crucible
open Vane.App.Input
open Vane.Input

-- macOS virtual key codes (from HIToolbox/Events.h)
-- Reference: https://stackoverflow.com/questions/3202629/where-can-i-find-a-list-of-mac-virtual-key-codes

testSuite "Input.keyCodeToChar"

test "letter 'a' (keyCode 0)" := do
  let result := keyCodeToChar 0 Modifiers.empty
  ensure (result == some 'a') "keyCode 0 should map to 'a'"

test "letter 'A' with shift (keyCode 0)" := do
  let result := keyCodeToChar 0 { shift := true }
  ensure (result == some 'A') "keyCode 0 with shift should map to 'A'"

test "letter 's' (keyCode 1)" := do
  let result := keyCodeToChar 1 Modifiers.empty
  ensure (result == some 's') "keyCode 1 should map to 's'"

test "letter 'd' (keyCode 2)" := do
  let result := keyCodeToChar 2 Modifiers.empty
  ensure (result == some 'd') "keyCode 2 should map to 'd'"

test "letter 'f' (keyCode 3)" := do
  let result := keyCodeToChar 3 Modifiers.empty
  ensure (result == some 'f') "keyCode 3 should map to 'f'"

test "letter 'h' (keyCode 4)" := do
  let result := keyCodeToChar 4 Modifiers.empty
  ensure (result == some 'h') "keyCode 4 should map to 'h'"

test "letter 'g' (keyCode 5)" := do
  let result := keyCodeToChar 5 Modifiers.empty
  ensure (result == some 'g') "keyCode 5 should map to 'g'"

test "letter 'z' (keyCode 6)" := do
  let result := keyCodeToChar 6 Modifiers.empty
  ensure (result == some 'z') "keyCode 6 should map to 'z'"

test "letter 'x' (keyCode 7)" := do
  let result := keyCodeToChar 7 Modifiers.empty
  ensure (result == some 'x') "keyCode 7 should map to 'x'"

test "letter 'c' (keyCode 8)" := do
  let result := keyCodeToChar 8 Modifiers.empty
  ensure (result == some 'c') "keyCode 8 should map to 'c'"

test "letter 'v' (keyCode 9)" := do
  let result := keyCodeToChar 9 Modifiers.empty
  ensure (result == some 'v') "keyCode 9 should map to 'v'"

test "letter 'b' (keyCode 11)" := do
  let result := keyCodeToChar 11 Modifiers.empty
  ensure (result == some 'b') "keyCode 11 should map to 'b'"

test "letter 'q' (keyCode 12)" := do
  let result := keyCodeToChar 12 Modifiers.empty
  ensure (result == some 'q') "keyCode 12 should map to 'q'"

test "letter 'w' (keyCode 13)" := do
  let result := keyCodeToChar 13 Modifiers.empty
  ensure (result == some 'w') "keyCode 13 should map to 'w'"

test "letter 'e' (keyCode 14)" := do
  let result := keyCodeToChar 14 Modifiers.empty
  ensure (result == some 'e') "keyCode 14 should map to 'e'"

test "letter 'r' (keyCode 15)" := do
  let result := keyCodeToChar 15 Modifiers.empty
  ensure (result == some 'r') "keyCode 15 should map to 'r'"

test "letter 'y' (keyCode 16)" := do
  let result := keyCodeToChar 16 Modifiers.empty
  ensure (result == some 'y') "keyCode 16 should map to 'y'"

test "letter 't' (keyCode 17)" := do
  let result := keyCodeToChar 17 Modifiers.empty
  ensure (result == some 't') "keyCode 17 should map to 't'"

-- Number keys
test "number '1' (keyCode 18)" := do
  let result := keyCodeToChar 18 Modifiers.empty
  ensure (result == some '1') "keyCode 18 should map to '1'"

test "number '2' (keyCode 19)" := do
  let result := keyCodeToChar 19 Modifiers.empty
  ensure (result == some '2') "keyCode 19 should map to '2'"

test "number '3' (keyCode 20)" := do
  let result := keyCodeToChar 20 Modifiers.empty
  ensure (result == some '3') "keyCode 20 should map to '3'"

test "number '4' (keyCode 21)" := do
  let result := keyCodeToChar 21 Modifiers.empty
  ensure (result == some '4') "keyCode 21 should map to '4'"

test "number '6' (keyCode 22)" := do
  let result := keyCodeToChar 22 Modifiers.empty
  ensure (result == some '6') "keyCode 22 should map to '6'"

test "number '5' (keyCode 23)" := do
  let result := keyCodeToChar 23 Modifiers.empty
  ensure (result == some '5') "keyCode 23 should map to '5'"

-- Special keys
test "return key (keyCode 36)" := do
  let result := keyCodeToChar 36 Modifiers.empty
  ensure (result == some '\r') "keyCode 36 should map to carriage return"

test "space key (keyCode 49)" := do
  let result := keyCodeToChar 49 Modifiers.empty
  ensure (result == some ' ') "keyCode 49 should map to space"

test "backspace key (keyCode 51)" := do
  let result := keyCodeToChar 51 Modifiers.empty
  ensure (result == some (Char.ofNat 0x7F)) "keyCode 51 should map to DEL"

-- Shift symbols
test "shift+1 = '!' (keyCode 18)" := do
  let result := keyCodeToChar 18 { shift := true }
  ensure (result == some '!') "keyCode 18 with shift should map to '!'"

test "shift+2 = '@' (keyCode 19)" := do
  let result := keyCodeToChar 19 { shift := true }
  ensure (result == some '@') "keyCode 19 with shift should map to '@'"

testSuite "Input.keyCodeToSpecial"

test "left arrow (keyCode 123)" := do
  let result := keyCodeToSpecial 123
  ensure (result == some SpecialKey.left) "keyCode 123 should map to left arrow"

test "right arrow (keyCode 124)" := do
  let result := keyCodeToSpecial 124
  ensure (result == some SpecialKey.right) "keyCode 124 should map to right arrow"

test "down arrow (keyCode 125)" := do
  let result := keyCodeToSpecial 125
  ensure (result == some SpecialKey.down) "keyCode 125 should map to down arrow"

test "up arrow (keyCode 126)" := do
  let result := keyCodeToSpecial 126
  ensure (result == some SpecialKey.up) "keyCode 126 should map to up arrow"

test "escape (keyCode 53)" := do
  let result := keyCodeToSpecial 53
  ensure (result == some SpecialKey.escape) "keyCode 53 should map to escape"

test "tab (keyCode 48)" := do
  let result := keyCodeToSpecial 48
  ensure (result == some SpecialKey.tab) "keyCode 48 should map to tab"

test "F1 (keyCode 122)" := do
  let result := keyCodeToSpecial 122
  ensure (result == some SpecialKey.f1) "keyCode 122 should map to F1"

testSuite "Input.mapKeyCode"

test "maps 'a' key correctly" := do
  let result := mapKeyCode 0 0
  match result with
  | some (.char c _) => ensure (c == 'a') "mapKeyCode 0 0 should produce 'a'"
  | _ => ensure false "Expected char 'a'"

test "maps 'A' key with shift" := do
  let result := mapKeyCode 0 1  -- shift modifier
  match result with
  | some (.char c _) => ensure (c == 'A') "mapKeyCode 0 1 should produce 'A'"
  | _ => ensure false "Expected char 'A'"

test "maps arrow keys to special" := do
  let result := mapKeyCode 126 0  -- up arrow
  match result with
  | some (.special key _) => ensure (key == SpecialKey.up) "mapKeyCode 126 0 should produce up arrow"
  | _ => ensure false "Expected up arrow"

testSuite "Input.encodeForPty"

test "encodes 'a' as single byte" := do
  let input := KeyInput.char 'a' Modifiers.empty
  let bytes := encodeForPty input
  ensure (bytes.size == 1) s!"Expected 1 byte, got {bytes.size}"
  ensure (bytes[0]! == 'a'.toNat.toUInt8) "Expected 'a' byte"

test "encodes Ctrl+C as 0x03" := do
  let input := KeyInput.char 'c' { ctrl := true }
  let bytes := encodeForPty input
  ensure (bytes.size == 1) s!"Expected 1 byte, got {bytes.size}"
  ensure (bytes[0]! == 3) "Expected 0x03 (ETX)"

test "encodes up arrow as ESC[A" := do
  let input := KeyInput.special SpecialKey.up Modifiers.empty
  let bytes := encodeForPty input false
  ensure (bytes.size == 3) s!"Expected 3 bytes, got {bytes.size}"
  ensure (bytes[0]! == 0x1B) "Expected ESC"
  ensure (bytes[1]! == '['.toNat.toUInt8) "Expected '['"
  ensure (bytes[2]! == 'A'.toNat.toUInt8) "Expected 'A'"

test "encodes escape as single ESC byte" := do
  let input := KeyInput.special SpecialKey.escape Modifiers.empty
  let bytes := encodeForPty input
  ensure (bytes.size == 1) s!"Expected 1 byte, got {bytes.size}"
  ensure (bytes[0]! == 0x1B) "Expected ESC byte"
