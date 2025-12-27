/-
  Vane.Input.KeyEncoder - Keyboard to terminal escape sequence encoding
-/

namespace Vane.Input

/-- Modifier keys state -/
structure Modifiers where
  shift : Bool := false
  ctrl : Bool := false
  alt : Bool := false
  cmd : Bool := false  -- Command key on macOS (meta)
  deriving Repr, BEq, Inhabited

namespace Modifiers

def empty : Modifiers := {}

/-- Check if any modifier is pressed -/
def any (m : Modifiers) : Bool :=
  m.shift || m.ctrl || m.alt || m.cmd

/-- Get CSI modifier parameter (for modified keys) -/
def toCSIParam (m : Modifiers) : Nat :=
  -- Standard CSI modifier encoding:
  -- 1 = no modifier (default, usually omitted)
  -- 2 = Shift
  -- 3 = Alt
  -- 4 = Shift+Alt
  -- 5 = Ctrl
  -- 6 = Shift+Ctrl
  -- 7 = Alt+Ctrl
  -- 8 = Shift+Alt+Ctrl
  let base := 1
  let withShift := if m.shift then base + 1 else base
  let withAlt := if m.alt then withShift + 2 else withShift
  let withCtrl := if m.ctrl then withAlt + 4 else withAlt
  withCtrl

end Modifiers

/-- Special keys that generate escape sequences -/
inductive SpecialKey where
  -- Arrow keys
  | up
  | down
  | left
  | right
  -- Navigation
  | home
  | end_
  | pageUp
  | pageDown
  | insert
  | delete
  -- Function keys
  | f1 | f2 | f3 | f4 | f5 | f6
  | f7 | f8 | f9 | f10 | f11 | f12
  -- Keypad
  | keypadEnter
  -- Tab (for Shift-Tab)
  | tab
  | backTab  -- Shift+Tab
  -- Escape
  | escape
  deriving Repr, BEq

/-- Keyboard input event -/
inductive KeyInput where
  /-- Regular character input -/
  | char (c : Char) (mods : Modifiers)
  /-- Special key -/
  | special (key : SpecialKey) (mods : Modifiers)
  deriving Repr

namespace KeyEncoder

/-- Encode a character with Ctrl modifier -/
def encodeCtrlChar (c : Char) : Option ByteArray :=
  let code := c.toNat
  -- Ctrl+A through Ctrl+Z map to 1-26
  if code >= 'a'.toNat && code <= 'z'.toNat then
    some (ByteArray.mk #[(code - 'a'.toNat + 1).toUInt8])
  else if code >= 'A'.toNat && code <= 'Z'.toNat then
    some (ByteArray.mk #[(code - 'A'.toNat + 1).toUInt8])
  -- Special Ctrl characters
  else if c == '[' || c == '3' then  -- Ctrl+[ = ESC
    some (ByteArray.mk #[0x1B])
  else if c == '\\' || c == '4' then  -- Ctrl+\ = FS
    some (ByteArray.mk #[0x1C])
  else if c == ']' || c == '5' then  -- Ctrl+] = GS
    some (ByteArray.mk #[0x1D])
  else if c == '^' || c == '6' then  -- Ctrl+^ = RS
    some (ByteArray.mk #[0x1E])
  else if c == '_' || c == '7' then  -- Ctrl+_ = US
    some (ByteArray.mk #[0x1F])
  else if c == '@' || c == '2' then  -- Ctrl+@ = NUL
    some (ByteArray.mk #[0x00])
  else if c == ' ' then  -- Ctrl+Space = NUL
    some (ByteArray.mk #[0x00])
  else
    none

/-- Encode special key with optional modifiers -/
def encodeSpecialKey (key : SpecialKey) (mods : Modifiers) (applicationMode : Bool) : ByteArray :=
  let esc := ByteArray.mk #[0x1B]

  -- Helper to create CSI sequence with optional modifier
  let csiSeq (code : String) : ByteArray :=
    if mods.any then
      let m := mods.toCSIParam
      s!"\x1b[1;{m}{code}".toUTF8
    else
      s!"\x1b[{code}".toUTF8

  -- Helper for SS3 sequences (application mode)
  let ss3Seq (code : Char) : ByteArray :=
    if mods.any then
      let m := mods.toCSIParam
      s!"\x1b[1;{m}{code}".toUTF8
    else
      s!"\x1bO{code}".toUTF8

  match key with
  -- Arrow keys
  | .up =>
    if applicationMode && !mods.any then ss3Seq 'A' else csiSeq "A"
  | .down =>
    if applicationMode && !mods.any then ss3Seq 'B' else csiSeq "B"
  | .right =>
    if applicationMode && !mods.any then ss3Seq 'C' else csiSeq "C"
  | .left =>
    if applicationMode && !mods.any then ss3Seq 'D' else csiSeq "D"

  -- Navigation keys
  | .home => csiSeq "H"
  | .end_ => csiSeq "F"
  | .pageUp =>
    if mods.any then
      let m := mods.toCSIParam
      s!"\x1b[5;{m}~".toUTF8
    else
      "\x1b[5~".toUTF8
  | .pageDown =>
    if mods.any then
      let m := mods.toCSIParam
      s!"\x1b[6;{m}~".toUTF8
    else
      "\x1b[6~".toUTF8
  | .insert =>
    if mods.any then
      let m := mods.toCSIParam
      s!"\x1b[2;{m}~".toUTF8
    else
      "\x1b[2~".toUTF8
  | .delete =>
    if mods.any then
      let m := mods.toCSIParam
      s!"\x1b[3;{m}~".toUTF8
    else
      "\x1b[3~".toUTF8

  -- Function keys (VT220 style)
  | .f1 => if mods.any then let m := mods.toCSIParam; s!"\x1b[1;{m}P".toUTF8 else ss3Seq 'P'
  | .f2 => if mods.any then let m := mods.toCSIParam; s!"\x1b[1;{m}Q".toUTF8 else ss3Seq 'Q'
  | .f3 => if mods.any then let m := mods.toCSIParam; s!"\x1b[1;{m}R".toUTF8 else ss3Seq 'R'
  | .f4 => if mods.any then let m := mods.toCSIParam; s!"\x1b[1;{m}S".toUTF8 else ss3Seq 'S'
  | .f5 => if mods.any then let m := mods.toCSIParam; s!"\x1b[15;{m}~".toUTF8 else "\x1b[15~".toUTF8
  | .f6 => if mods.any then let m := mods.toCSIParam; s!"\x1b[17;{m}~".toUTF8 else "\x1b[17~".toUTF8
  | .f7 => if mods.any then let m := mods.toCSIParam; s!"\x1b[18;{m}~".toUTF8 else "\x1b[18~".toUTF8
  | .f8 => if mods.any then let m := mods.toCSIParam; s!"\x1b[19;{m}~".toUTF8 else "\x1b[19~".toUTF8
  | .f9 => if mods.any then let m := mods.toCSIParam; s!"\x1b[20;{m}~".toUTF8 else "\x1b[20~".toUTF8
  | .f10 => if mods.any then let m := mods.toCSIParam; s!"\x1b[21;{m}~".toUTF8 else "\x1b[21~".toUTF8
  | .f11 => if mods.any then let m := mods.toCSIParam; s!"\x1b[23;{m}~".toUTF8 else "\x1b[23~".toUTF8
  | .f12 => if mods.any then let m := mods.toCSIParam; s!"\x1b[24;{m}~".toUTF8 else "\x1b[24~".toUTF8

  -- Other special keys
  | .keypadEnter => "\x1bOM".toUTF8
  | .tab => "\t".toUTF8
  | .backTab => "\x1b[Z".toUTF8  -- CSI Z (backtab)
  | .escape => esc

/-- Encode a keyboard input event -/
def encode (input : KeyInput) (applicationMode : Bool := false) : ByteArray :=
  match input with
  | .char c mods =>
    if mods.ctrl then
      match encodeCtrlChar c with
      | some bytes => bytes
      | none =>
        -- Alt modifier sends ESC prefix
        if mods.alt then
          ByteArray.mk #[0x1B] ++ c.toString.toUTF8
        else
          c.toString.toUTF8
    else if mods.alt then
      -- Alt sends ESC prefix
      ByteArray.mk #[0x1B] ++ c.toString.toUTF8
    else
      c.toString.toUTF8
  | .special key mods =>
    encodeSpecialKey key mods applicationMode

/-- Encode bracketed paste start -/
def bracketedPasteStart : ByteArray :=
  "\x1b[200~".toUTF8

/-- Encode bracketed paste end -/
def bracketedPasteEnd : ByteArray :=
  "\x1b[201~".toUTF8

/-- Wrap text in bracketed paste markers -/
def encodePaste (text : String) (bracketedPasteEnabled : Bool) : ByteArray :=
  if bracketedPasteEnabled then
    bracketedPasteStart ++ text.toUTF8 ++ bracketedPasteEnd
  else
    text.toUTF8

end KeyEncoder

/-- Mouse button -/
inductive MouseButton where
  | left
  | middle
  | right
  | wheelUp
  | wheelDown
  | none  -- For motion events
  deriving Repr, BEq

/-- Mouse event type -/
inductive MouseEventType where
  | press
  | release
  | motion
  | drag
  deriving Repr, BEq

/-- Mouse event -/
structure MouseEvent where
  button : MouseButton
  eventType : MouseEventType
  col : Nat
  row : Nat
  mods : Modifiers := {}
  deriving Repr

namespace MouseEncoder

/-- Encode mouse event in X10 mode (limited to 223 cols/rows) -/
def encodeX10 (event : MouseEvent) : Option ByteArray :=
  if event.col > 222 || event.row > 222 then
    none  -- X10 encoding can't handle larger coordinates
  else
    let button : UInt8 := match event.button with
      | .left => 0
      | .middle => 1
      | .right => 2
      | .wheelUp => 64
      | .wheelDown => 65
      | .none => 3  -- Release or motion
    let button := button + (if event.mods.shift then 4 else 0)
    let button := button + (if event.mods.alt then 8 else 0)
    let button := button + (if event.mods.ctrl then 16 else 0)
    let button := if event.eventType == .drag then button + 32 else button

    some (ByteArray.mk #[0x1B, '['.toNat.toUInt8, 'M'.toNat.toUInt8,
                         button + 32,
                         (event.col + 33).toUInt8,
                         (event.row + 33).toUInt8])

/-- Encode mouse event in SGR mode (no coordinate limits) -/
def encodeSGR (event : MouseEvent) : ByteArray :=
  let button : Nat := match event.button with
    | .left => 0
    | .middle => 1
    | .right => 2
    | .wheelUp => 64
    | .wheelDown => 65
    | .none => 3
  let button := button + (if event.mods.shift then 4 else 0)
  let button := button + (if event.mods.alt then 8 else 0)
  let button := button + (if event.mods.ctrl then 16 else 0)
  let button := if event.eventType == .drag then button + 32 else button

  let suffix := if event.eventType == .release then "m" else "M"
  s!"\x1b[<{button};{event.col + 1};{event.row + 1}{suffix}".toUTF8

/-- Encode mouse event in SGR-Pixels mode -/
def encodeSGRPixels (event : MouseEvent) (pixelX pixelY : Nat) : ByteArray :=
  let button : Nat := match event.button with
    | .left => 0
    | .middle => 1
    | .right => 2
    | .wheelUp => 64
    | .wheelDown => 65
    | .none => 3
  let button := button + (if event.mods.shift then 4 else 0)
  let button := button + (if event.mods.alt then 8 else 0)
  let button := button + (if event.mods.ctrl then 16 else 0)
  let button := if event.eventType == .drag then button + 32 else button

  let suffix := if event.eventType == .release then "m" else "M"
  s!"\x1b[<{button};{pixelX};{pixelY}{suffix}".toUTF8

end MouseEncoder

-- Focus event encoding
namespace FocusEncoder

def focusIn : ByteArray := "\x1b[I".toUTF8
def focusOut : ByteArray := "\x1b[O".toUTF8

end FocusEncoder

end Vane.Input
