/-
  Vane.Core.Style - Colors and text styling for terminal cells
-/

namespace Vane

/-- Standard 16 ANSI colors -/
inductive Color16 where
  | black
  | red
  | green
  | yellow
  | blue
  | magenta
  | cyan
  | white
  | brightBlack
  | brightRed
  | brightGreen
  | brightYellow
  | brightBlue
  | brightMagenta
  | brightCyan
  | brightWhite
  deriving Repr, BEq, Inhabited, Hashable

/-- Color specification supporting multiple color modes -/
inductive Color where
  | default
  | ansi (c : Color16)
  | indexed (n : UInt8)
  | rgb (r g b : UInt8)
  deriving Repr, BEq, Inhabited

namespace Color

-- Convenience constructors for standard colors
def black : Color := .ansi .black
def red : Color := .ansi .red
def green : Color := .ansi .green
def yellow : Color := .ansi .yellow
def blue : Color := .ansi .blue
def magenta : Color := .ansi .magenta
def cyan : Color := .ansi .cyan
def white : Color := .ansi .white

def brightBlack : Color := .ansi .brightBlack
def brightRed : Color := .ansi .brightRed
def brightGreen : Color := .ansi .brightGreen
def brightYellow : Color := .ansi .brightYellow
def brightBlue : Color := .ansi .brightBlue
def brightMagenta : Color := .ansi .brightMagenta
def brightCyan : Color := .ansi .brightCyan
def brightWhite : Color := .ansi .brightWhite

def gray : Color := .ansi .brightBlack

/-- Convert to RGBA floats (0.0-1.0) for GPU rendering -/
def toRGBA (c : Color) (defaultFg : Bool := true) : Float × Float × Float × Float :=
  match c with
  | .default => if defaultFg then (0.9, 0.9, 0.9, 1.0) else (0.1, 0.1, 0.1, 1.0)
  | .ansi c16 => color16ToRGBA c16
  | .indexed n => indexed256ToRGBA n
  | .rgb r g b => (r.toFloat / 255.0, g.toFloat / 255.0, b.toFloat / 255.0, 1.0)
where
  color16ToRGBA : Color16 → Float × Float × Float × Float
    | .black => (0.0, 0.0, 0.0, 1.0)
    | .red => (0.8, 0.0, 0.0, 1.0)
    | .green => (0.0, 0.8, 0.0, 1.0)
    | .yellow => (0.8, 0.8, 0.0, 1.0)
    | .blue => (0.0, 0.0, 0.8, 1.0)
    | .magenta => (0.8, 0.0, 0.8, 1.0)
    | .cyan => (0.0, 0.8, 0.8, 1.0)
    | .white => (0.75, 0.75, 0.75, 1.0)
    | .brightBlack => (0.5, 0.5, 0.5, 1.0)
    | .brightRed => (1.0, 0.0, 0.0, 1.0)
    | .brightGreen => (0.0, 1.0, 0.0, 1.0)
    | .brightYellow => (1.0, 1.0, 0.0, 1.0)
    | .brightBlue => (0.0, 0.0, 1.0, 1.0)
    | .brightMagenta => (1.0, 0.0, 1.0, 1.0)
    | .brightCyan => (0.0, 1.0, 1.0, 1.0)
    | .brightWhite => (1.0, 1.0, 1.0, 1.0)

  indexed256ToRGBA (n : UInt8) : Float × Float × Float × Float :=
    let n := n.toNat
    if n < 16 then
      -- Standard 16 colors (map to Color16)
      color16ToRGBA (match n with
        | 0 => .black | 1 => .red | 2 => .green | 3 => .yellow
        | 4 => .blue | 5 => .magenta | 6 => .cyan | 7 => .white
        | 8 => .brightBlack | 9 => .brightRed | 10 => .brightGreen | 11 => .brightYellow
        | 12 => .brightBlue | 13 => .brightMagenta | 14 => .brightCyan | _ => .brightWhite)
    else if n < 232 then
      -- 216 color cube (6x6x6)
      let idx := n - 16
      let r := idx / 36
      let g := (idx / 6) % 6
      let b := idx % 6
      let toFloat (v : Nat) : Float := if v == 0 then 0.0 else (v.toFloat * 40.0 + 55.0) / 255.0
      (toFloat r, toFloat g, toFloat b, 1.0)
    else
      -- 24 grayscale colors
      let gray := ((n - 232) * 10 + 8).toFloat / 255.0
      (gray, gray, gray, 1.0)

end Color

/-- Text modifiers (attributes) -/
structure Modifier where
  bold : Bool := false
  dim : Bool := false
  italic : Bool := false
  underline : Bool := false
  blink : Bool := false
  reverse : Bool := false
  hidden : Bool := false
  strikethrough : Bool := false
  deriving Repr, BEq, Inhabited

namespace Modifier

def empty : Modifier := {}
def mkBold : Modifier := { bold := true }
def mkDim : Modifier := { dim := true }
def mkItalic : Modifier := { italic := true }
def mkUnderline : Modifier := { underline := true }
def mkBlink : Modifier := { blink := true }
def mkReverse : Modifier := { reverse := true }
def mkHidden : Modifier := { hidden := true }
def mkStrikethrough : Modifier := { strikethrough := true }

def merge (m1 m2 : Modifier) : Modifier := {
  bold := m1.bold || m2.bold
  dim := m1.dim || m2.dim
  italic := m1.italic || m2.italic
  underline := m1.underline || m2.underline
  blink := m1.blink || m2.blink
  reverse := m1.reverse || m2.reverse
  hidden := m1.hidden || m2.hidden
  strikethrough := m1.strikethrough || m2.strikethrough
}

/-- Check if any modifier is set -/
def hasAny (m : Modifier) : Bool :=
  m.bold || m.dim || m.italic || m.underline || m.blink || m.reverse || m.hidden || m.strikethrough

end Modifier

/-- Complete style specification -/
structure Style where
  fg : Color := .default
  bg : Color := .default
  modifier : Modifier := {}
  deriving Repr, BEq, Inhabited

namespace Style

def default : Style := {}

def fgColor (c : Color) : Style := { fg := c }
def bgColor (c : Color) : Style := { bg := c }

def bold : Style := { modifier := Modifier.mkBold }
def dim : Style := { modifier := Modifier.mkDim }
def italic : Style := { modifier := Modifier.mkItalic }
def underline : Style := { modifier := Modifier.mkUnderline }
def blink : Style := { modifier := Modifier.mkBlink }
def reversed : Style := { modifier := Modifier.mkReverse }
def hidden : Style := { modifier := Modifier.mkHidden }
def strikethrough : Style := { modifier := Modifier.mkStrikethrough }

def withFg (s : Style) (c : Color) : Style := { s with fg := c }
def withBg (s : Style) (c : Color) : Style := { s with bg := c }
def withModifier (s : Style) (m : Modifier) : Style := { s with modifier := Modifier.merge s.modifier m }

def merge (s1 s2 : Style) : Style := {
  fg := if s2.fg == .default then s1.fg else s2.fg
  bg := if s2.bg == .default then s1.bg else s2.bg
  modifier := Modifier.merge s1.modifier s2.modifier
}

instance : Append Style where
  append := merge

/-- Reset style to default -/
def reset : Style := default

end Style

end Vane
