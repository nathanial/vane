/-
  Vane.Parser.SGR - Select Graphic Rendition (colors and text attributes)
-/

import Vane.Core.Style

namespace Vane.Parser

/-- Result of parsing SGR parameters -/
structure SGRState where
  fg : Option Color := none
  bg : Option Color := none
  bold : Option Bool := none
  dim : Option Bool := none
  italic : Option Bool := none
  underline : Option Bool := none
  blink : Option Bool := none
  reverse : Option Bool := none
  hidden : Option Bool := none
  strikethrough : Option Bool := none
  /-- Reset all attributes -/
  reset : Bool := false
  deriving Repr

namespace SGRState

def empty : SGRState := {}

/-- Apply SGR state to a cell's style -/
def applyToStyle (s : SGRState) (style : Style) : Style :=
  if s.reset then
    Style.default
  else
    let fg := s.fg.getD style.fg
    let bg := s.bg.getD style.bg
    let modifier : Modifier := {
      bold := s.bold.getD style.modifier.bold
      dim := s.dim.getD style.modifier.dim
      italic := s.italic.getD style.modifier.italic
      underline := s.underline.getD style.modifier.underline
      blink := s.blink.getD style.modifier.blink
      reverse := s.reverse.getD style.modifier.reverse
      hidden := s.hidden.getD style.modifier.hidden
      strikethrough := s.strikethrough.getD style.modifier.strikethrough
    }
    { fg, bg, modifier }

end SGRState

/-- Parse SGR (Select Graphic Rendition) parameters -/
def parseSGR (params : Array (Option Nat)) : SGRState := Id.run do
  let mut state := SGRState.empty
  let mut i := 0

  -- Empty params means reset (CSI m == CSI 0 m)
  if params.isEmpty then
    return { state with reset := true }

  while i < params.size do
    let param := (params.getD i none).getD 0

    match param with
    -- Reset
    | 0 => state := { state with reset := true }

    -- Text attributes ON
    | 1 => state := { state with bold := some true }
    | 2 => state := { state with dim := some true }
    | 3 => state := { state with italic := some true }
    | 4 => state := { state with underline := some true }
    | 5 => state := { state with blink := some true }
    | 6 => state := { state with blink := some true }  -- Rapid blink (treat as blink)
    | 7 => state := { state with reverse := some true }
    | 8 => state := { state with hidden := some true }
    | 9 => state := { state with strikethrough := some true }

    -- Text attributes OFF
    | 21 => state := { state with bold := some false }  -- Double underline or bold off
    | 22 => state := { state with bold := some false, dim := some false }
    | 23 => state := { state with italic := some false }
    | 24 => state := { state with underline := some false }
    | 25 => state := { state with blink := some false }
    | 27 => state := { state with reverse := some false }
    | 28 => state := { state with hidden := some false }
    | 29 => state := { state with strikethrough := some false }

    -- Standard foreground colors (30-37)
    | 30 => state := { state with fg := some (.ansi .black) }
    | 31 => state := { state with fg := some (.ansi .red) }
    | 32 => state := { state with fg := some (.ansi .green) }
    | 33 => state := { state with fg := some (.ansi .yellow) }
    | 34 => state := { state with fg := some (.ansi .blue) }
    | 35 => state := { state with fg := some (.ansi .magenta) }
    | 36 => state := { state with fg := some (.ansi .cyan) }
    | 37 => state := { state with fg := some (.ansi .white) }

    -- Extended foreground color (38)
    | 38 =>
      let (color, skip) := parseExtendedColor params (i + 1)
      state := { state with fg := color }
      i := i + skip

    -- Default foreground
    | 39 => state := { state with fg := some .default }

    -- Standard background colors (40-47)
    | 40 => state := { state with bg := some (.ansi .black) }
    | 41 => state := { state with bg := some (.ansi .red) }
    | 42 => state := { state with bg := some (.ansi .green) }
    | 43 => state := { state with bg := some (.ansi .yellow) }
    | 44 => state := { state with bg := some (.ansi .blue) }
    | 45 => state := { state with bg := some (.ansi .magenta) }
    | 46 => state := { state with bg := some (.ansi .cyan) }
    | 47 => state := { state with bg := some (.ansi .white) }

    -- Extended background color (48)
    | 48 =>
      let (color, skip) := parseExtendedColor params (i + 1)
      state := { state with bg := color }
      i := i + skip

    -- Default background
    | 49 => state := { state with bg := some .default }

    -- Bright foreground colors (90-97)
    | 90 => state := { state with fg := some (.ansi .brightBlack) }
    | 91 => state := { state with fg := some (.ansi .brightRed) }
    | 92 => state := { state with fg := some (.ansi .brightGreen) }
    | 93 => state := { state with fg := some (.ansi .brightYellow) }
    | 94 => state := { state with fg := some (.ansi .brightBlue) }
    | 95 => state := { state with fg := some (.ansi .brightMagenta) }
    | 96 => state := { state with fg := some (.ansi .brightCyan) }
    | 97 => state := { state with fg := some (.ansi .brightWhite) }

    -- Bright background colors (100-107)
    | 100 => state := { state with bg := some (.ansi .brightBlack) }
    | 101 => state := { state with bg := some (.ansi .brightRed) }
    | 102 => state := { state with bg := some (.ansi .brightGreen) }
    | 103 => state := { state with bg := some (.ansi .brightYellow) }
    | 104 => state := { state with bg := some (.ansi .brightBlue) }
    | 105 => state := { state with bg := some (.ansi .brightMagenta) }
    | 106 => state := { state with bg := some (.ansi .brightCyan) }
    | 107 => state := { state with bg := some (.ansi .brightWhite) }

    -- Unknown - ignore
    | _ => pure ()

    i := i + 1

  state
where
  /-- Parse extended color (256-color or RGB)
      Returns (color, number of additional params consumed) -/
  parseExtendedColor (params : Array (Option Nat)) (idx : Nat) : Option Color × Nat :=
    if idx >= params.size then
      (none, 0)
    else
      let mode := (params.getD idx none).getD 0
      match mode with
      | 5 =>  -- 256-color mode: 38;5;n or 48;5;n
        if idx + 1 >= params.size then
          (none, 1)
        else
          let colorIdx := (params.getD (idx + 1) none).getD 0
          (some (.indexed colorIdx.toUInt8), 2)
      | 2 =>  -- RGB mode: 38;2;r;g;b or 48;2;r;g;b
        if idx + 3 >= params.size then
          (none, 1)
        else
          let r := (params.getD (idx + 1) none).getD 0
          let g := (params.getD (idx + 2) none).getD 0
          let b := (params.getD (idx + 3) none).getD 0
          (some (.rgb r.toUInt8 g.toUInt8 b.toUInt8), 4)
      | _ =>
        (none, 1)

end Vane.Parser
