/-
  Vane.Parser.Machine - VT500 state machine implementation
-/

import Vane.Parser.State

namespace Vane.Parser

/-- Result of processing a single byte -/
structure StepResult where
  parser : Parser
  actions : Array Action
  deriving Repr

namespace Parser

/-- Process a single byte through the state machine -/
def step (p : Parser) (byte : UInt8) : StepResult :=
  -- Handle C0 controls that are always executed (except in passthrough states)
  if byte == C0.CAN || byte == C0.SUB then
    -- CAN and SUB always abort and return to ground
    { parser := p.reset, actions := #[] }
  else if byte == C0.ESC then
    -- ESC always transitions to escape state
    { parser := { p.clearParams with state := .escape }, actions := #[] }
  else
    match p.state with
    | .ground => stepGround p byte
    | .escape => stepEscape p byte
    | .escapeIntermediate => stepEscapeIntermediate p byte
    | .csiEntry => stepCSIEntry p byte
    | .csiParam => stepCSIParam p byte
    | .csiIntermediate => stepCSIIntermediate p byte
    | .csiIgnore => stepCSIIgnore p byte
    | .dcsEntry => stepDCSEntry p byte
    | .dcsParam => stepDCSParam p byte
    | .dcsIntermediate => stepDCSIntermediate p byte
    | .dcsPassthrough => stepDCSPassthrough p byte
    | .dcsIgnore => stepDCSIgnore p byte
    | .oscString => stepOSCString p byte
    | .sosPmApcString => stepSOSPMAPC p byte
where
  /-- Ground state: print characters, execute controls -/
  stepGround (p : Parser) (byte : UInt8) : StepResult :=
    if byte < 0x20 then
      -- C0 control - execute it
      { parser := p, actions := #[.execute byte] }
    else if byte < 0x80 then
      -- Printable ASCII
      { parser := p, actions := #[.print (Char.ofNat byte.toNat)] }
    else if byte < 0xA0 then
      -- C1 control range (0x80-0x9F) - treat as printable in ground
      { parser := p, actions := #[.print (Char.ofNat byte.toNat)] }
    else
      -- High bytes (0xA0-0xFF) - print as-is (Latin-1 or UTF-8 lead byte)
      { parser := p, actions := #[.print (Char.ofNat byte.toNat)] }

  /-- Escape state: after ESC -/
  stepEscape (p : Parser) (byte : UInt8) : StepResult :=
    if byte < 0x20 then
      -- C0 control during escape - execute and stay in escape
      { parser := p, actions := #[.execute byte] }
    else if byte >= 0x20 && byte <= 0x2F then
      -- Intermediate bytes - collect them
      { parser := { p.addIntermediate byte with state := .escapeIntermediate }, actions := #[] }
    else if byte == C1.CSI then  -- '['
      -- CSI introducer
      { parser := { p.clearParams with state := .csiEntry }, actions := #[] }
    else if byte == C1.OSC then  -- ']'
      -- OSC introducer
      { parser := { p with state := .oscString, oscString := "", oscParams := #[] }, actions := #[] }
    else if byte == C1.DCS then  -- 'P'
      -- DCS introducer
      { parser := { p.clearParams with state := .dcsEntry }, actions := #[] }
    else if byte == C1.SOS || byte == C1.PM || byte == C1.APC then
      -- String sequences we ignore
      { parser := { p with state := .sosPmApcString }, actions := #[] }
    else if byte >= 0x30 && byte <= 0x7E then
      -- Final byte - dispatch ESC sequence
      { parser := p.reset, actions := #[.escDispatch p.intermediates byte] }
    else
      -- Invalid - return to ground
      { parser := p.reset, actions := #[] }

  /-- Escape intermediate: collecting intermediate bytes -/
  stepEscapeIntermediate (p : Parser) (byte : UInt8) : StepResult :=
    if byte < 0x20 then
      -- C0 control - execute and stay
      { parser := p, actions := #[.execute byte] }
    else if byte >= 0x20 && byte <= 0x2F then
      -- More intermediate bytes
      { parser := p.addIntermediate byte, actions := #[] }
    else if byte >= 0x30 && byte <= 0x7E then
      -- Final byte - dispatch
      { parser := p.reset, actions := #[.escDispatch p.intermediates byte] }
    else
      -- Invalid - return to ground
      { parser := p.reset, actions := #[] }

  /-- CSI entry: after ESC [ -/
  stepCSIEntry (p : Parser) (byte : UInt8) : StepResult :=
    if byte < 0x20 then
      -- C0 control - execute and stay
      { parser := p, actions := #[.execute byte] }
    else if byte >= 0x30 && byte <= 0x39 then
      -- Digit - start collecting parameter
      let digit := byte.toNat - 0x30
      { parser := { p.addDigit digit with state := .csiParam }, actions := #[] }
    else if byte == 0x3A then  -- ':'
      -- Colon - sub-parameter separator (treat as param separator for now)
      { parser := { p with state := .csiParam }, actions := #[] }
    else if byte == 0x3B then  -- ';'
      -- Semicolon - empty parameter
      { parser := { p.finishParam with state := .csiParam }, actions := #[] }
    else if byte >= 0x3C && byte <= 0x3F then  -- '<', '=', '>', '?'
      -- Private marker
      { parser := { p.setPrivateMarker byte with state := .csiParam }, actions := #[] }
    else if byte >= 0x20 && byte <= 0x2F then
      -- Intermediate byte
      { parser := { p.addIntermediate byte with state := .csiIntermediate }, actions := #[] }
    else if byte >= 0x40 && byte <= 0x7E then
      -- Final byte - dispatch with no params
      let params := p.getAllParams
      { parser := p.reset, actions := #[.csiDispatch params p.intermediates byte] }
    else
      -- Invalid
      { parser := { p with state := .csiIgnore }, actions := #[] }

  /-- CSI param: collecting parameters -/
  stepCSIParam (p : Parser) (byte : UInt8) : StepResult :=
    if byte < 0x20 then
      -- C0 control - execute and stay
      { parser := p, actions := #[.execute byte] }
    else if byte >= 0x30 && byte <= 0x39 then
      -- Digit
      let digit := byte.toNat - 0x30
      { parser := p.addDigit digit, actions := #[] }
    else if byte == 0x3A then  -- ':'
      -- Colon (sub-parameter) - treat as separator for simplicity
      { parser := p.finishParam, actions := #[] }
    else if byte == 0x3B then  -- ';'
      -- Semicolon - finish current param, start new one
      { parser := p.finishParam, actions := #[] }
    else if byte >= 0x3C && byte <= 0x3F then  -- '<', '=', '>', '?'
      -- Private marker in wrong position - go to ignore
      { parser := { p with state := .csiIgnore }, actions := #[] }
    else if byte >= 0x20 && byte <= 0x2F then
      -- Intermediate byte
      { parser := { p.addIntermediate byte with state := .csiIntermediate }, actions := #[] }
    else if byte >= 0x40 && byte <= 0x7E then
      -- Final byte - dispatch
      let params := p.getAllParams
      { parser := p.reset, actions := #[.csiDispatch params p.intermediates byte] }
    else
      -- Invalid
      { parser := { p with state := .csiIgnore }, actions := #[] }

  /-- CSI intermediate: have intermediate bytes, waiting for final -/
  stepCSIIntermediate (p : Parser) (byte : UInt8) : StepResult :=
    if byte < 0x20 then
      { parser := p, actions := #[.execute byte] }
    else if byte >= 0x20 && byte <= 0x2F then
      { parser := p.addIntermediate byte, actions := #[] }
    else if byte >= 0x30 && byte <= 0x3F then
      -- Parameter byte after intermediate - invalid
      { parser := { p with state := .csiIgnore }, actions := #[] }
    else if byte >= 0x40 && byte <= 0x7E then
      let params := p.getAllParams
      { parser := p.reset, actions := #[.csiDispatch params p.intermediates byte] }
    else
      { parser := { p with state := .csiIgnore }, actions := #[] }

  /-- CSI ignore: invalid sequence, wait for final byte -/
  stepCSIIgnore (p : Parser) (byte : UInt8) : StepResult :=
    if byte < 0x20 then
      { parser := p, actions := #[.execute byte] }
    else if byte >= 0x40 && byte <= 0x7E then
      -- Final byte - return to ground (don't dispatch)
      { parser := p.reset, actions := #[] }
    else
      { parser := p, actions := #[] }

  /-- DCS entry: after ESC P -/
  stepDCSEntry (p : Parser) (byte : UInt8) : StepResult :=
    if byte < 0x20 then
      { parser := p, actions := #[.execute byte] }
    else if byte >= 0x30 && byte <= 0x39 then
      let digit := byte.toNat - 0x30
      { parser := { p.addDigit digit with state := .dcsParam }, actions := #[] }
    else if byte == 0x3B then
      { parser := { p.finishParam with state := .dcsParam }, actions := #[] }
    else if byte >= 0x3C && byte <= 0x3F then
      { parser := { p.setPrivateMarker byte with state := .dcsParam }, actions := #[] }
    else if byte >= 0x20 && byte <= 0x2F then
      { parser := { p.addIntermediate byte with state := .dcsIntermediate }, actions := #[] }
    else if byte >= 0x40 && byte <= 0x7E then
      let params := p.getAllParams
      { parser := { p with state := .dcsPassthrough }, actions := #[.dcsHook params p.intermediates byte] }
    else
      { parser := { p with state := .dcsIgnore }, actions := #[] }

  /-- DCS param: collecting parameters -/
  stepDCSParam (p : Parser) (byte : UInt8) : StepResult :=
    if byte < 0x20 then
      { parser := p, actions := #[.execute byte] }
    else if byte >= 0x30 && byte <= 0x39 then
      let digit := byte.toNat - 0x30
      { parser := p.addDigit digit, actions := #[] }
    else if byte == 0x3B then
      { parser := p.finishParam, actions := #[] }
    else if byte >= 0x3C && byte <= 0x3F then
      { parser := { p with state := .dcsIgnore }, actions := #[] }
    else if byte >= 0x20 && byte <= 0x2F then
      { parser := { p.addIntermediate byte with state := .dcsIntermediate }, actions := #[] }
    else if byte >= 0x40 && byte <= 0x7E then
      let params := p.getAllParams
      { parser := { p with state := .dcsPassthrough }, actions := #[.dcsHook params p.intermediates byte] }
    else
      { parser := { p with state := .dcsIgnore }, actions := #[] }

  /-- DCS intermediate -/
  stepDCSIntermediate (p : Parser) (byte : UInt8) : StepResult :=
    if byte < 0x20 then
      { parser := p, actions := #[.execute byte] }
    else if byte >= 0x20 && byte <= 0x2F then
      { parser := p.addIntermediate byte, actions := #[] }
    else if byte >= 0x30 && byte <= 0x3F then
      { parser := { p with state := .dcsIgnore }, actions := #[] }
    else if byte >= 0x40 && byte <= 0x7E then
      let params := p.getAllParams
      { parser := { p with state := .dcsPassthrough }, actions := #[.dcsHook params p.intermediates byte] }
    else
      { parser := { p with state := .dcsIgnore }, actions := #[] }

  /-- DCS passthrough: receiving string content -/
  stepDCSPassthrough (p : Parser) (byte : UInt8) : StepResult :=
    if byte == C0.ESC then
      -- Potential ST coming
      { parser := { p with state := .escape }, actions := #[.dcsUnhook] }
    else if byte < 0x20 && byte != C0.ESC then
      -- Ignore C0 in passthrough (except ESC)
      { parser := p, actions := #[] }
    else if byte == 0x7F then
      -- DEL - ignore
      { parser := p, actions := #[] }
    else
      -- Pass data through
      { parser := p, actions := #[.dcsPut byte] }

  /-- DCS ignore: wait for ST -/
  stepDCSIgnore (p : Parser) (byte : UInt8) : StepResult :=
    if byte == C0.ESC then
      { parser := { p with state := .escape }, actions := #[] }
    else
      { parser := p, actions := #[] }

  /-- OSC string: collecting OSC content -/
  stepOSCString (p : Parser) (byte : UInt8) : StepResult :=
    if byte == C0.ESC then
      -- Potential ST (ESC \) coming - finish OSC
      let p' := p.finishOSCParam
      { parser := { p' with state := .escape }, actions := #[.oscDispatch p'.oscParams] }
    else if byte == C0.BEL then
      -- BEL also terminates OSC (xterm extension)
      let p' := p.finishOSCParam
      { parser := p'.reset, actions := #[.oscDispatch p'.oscParams] }
    else if byte == 0x3B then  -- ';'
      -- Parameter separator
      { parser := p.finishOSCParam, actions := #[] }
    else if byte >= 0x20 && byte < 0x7F then
      -- Printable - add to string
      { parser := p.appendOSC (Char.ofNat byte.toNat), actions := #[] }
    else
      -- Ignore other bytes in OSC
      { parser := p, actions := #[] }

  /-- SOS/PM/APC string: ignore until ST -/
  stepSOSPMAPC (p : Parser) (byte : UInt8) : StepResult :=
    if byte == C0.ESC then
      { parser := { p with state := .escape }, actions := #[] }
    else
      { parser := p, actions := #[] }

/-- Process multiple bytes, collecting all actions -/
def process (p : Parser) (bytes : ByteArray) : Parser × Array Action := Id.run do
  let mut parser := p
  let mut actions : Array Action := #[]
  for b in bytes.toList do
    let result := parser.step b
    parser := result.parser
    actions := actions ++ result.actions
  (parser, actions)

/-- Process a single byte, returning new parser and actions -/
def advance (p : Parser) (byte : UInt8) : Parser × Array Action :=
  let result := p.step byte
  (result.parser, result.actions)

end Parser

end Vane.Parser
