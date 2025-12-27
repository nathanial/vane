/-
  Vane.Parser.State - VT500 state machine states and parser state
-/

import Vane.Parser.Types

namespace Vane.Parser

/--
  VT500-compatible parser states.

  The parser is a state machine that processes bytes one at a time,
  transitioning between states and emitting actions.

  Reference: https://vt100.net/emu/dec_ansi_parser
-/
inductive ParserState where
  /-- Normal state - characters are printed, controls are executed -/
  | ground
  /-- After receiving ESC (0x1B) -/
  | escape
  /-- ESC followed by intermediate bytes (0x20-0x2F) -/
  | escapeIntermediate
  /-- After ESC [ (CSI) - ready for parameters -/
  | csiEntry
  /-- Collecting CSI parameters (digits, semicolons) -/
  | csiParam
  /-- CSI with intermediate bytes after parameters -/
  | csiIntermediate
  /-- Invalid CSI sequence - ignore until final byte -/
  | csiIgnore
  /-- After ESC P (DCS) - ready for parameters -/
  | dcsEntry
  /-- Collecting DCS parameters -/
  | dcsParam
  /-- DCS with intermediate bytes -/
  | dcsIntermediate
  /-- Receiving DCS string content -/
  | dcsPassthrough
  /-- Invalid DCS sequence - ignore until ST -/
  | dcsIgnore
  /-- Receiving OSC string (ESC ]) -/
  | oscString
  /-- Receiving SOS/PM/APC string -/
  | sosPmApcString
  deriving Repr, BEq, Inhabited

/-- The complete parser state -/
structure Parser where
  /-- Current state machine state -/
  state : ParserState := .ground
  /-- CSI/DCS parameters being collected -/
  params : Array (Option Nat) := #[]
  /-- Current parameter being built (before semicolon) -/
  currentParam : Option Nat := none
  /-- Intermediate bytes (0x20-0x2F) -/
  intermediates : Array UInt8 := #[]
  /-- OSC string buffer -/
  oscString : String := ""
  /-- OSC parameters (split by semicolons) -/
  oscParams : Array String := #[]
  /-- Whether we've seen a '?' or other private marker in CSI -/
  privateMarker : Option UInt8 := none
  deriving Repr, Inhabited

namespace Parser

/-- Create a fresh parser in ground state -/
def new : Parser := {}

/-- Reset parser to ground state, clearing all buffers -/
def reset (p : Parser) : Parser := { p with
  state := .ground
  params := #[]
  currentParam := none
  intermediates := #[]
  oscString := ""
  oscParams := #[]
  privateMarker := none
}

/-- Clear parameter state (used when entering CSI/DCS) -/
def clearParams (p : Parser) : Parser := { p with
  params := #[]
  currentParam := none
  intermediates := #[]
  privateMarker := none
}

/-- Add a digit to the current parameter -/
def addDigit (p : Parser) (digit : Nat) : Parser :=
  let current := p.currentParam.getD 0
  { p with currentParam := some (current * 10 + digit) }

/-- Finish the current parameter (on semicolon or final byte) -/
def finishParam (p : Parser) : Parser := { p with
  params := p.params.push p.currentParam
  currentParam := none
}

/-- Add an intermediate byte -/
def addIntermediate (p : Parser) (b : UInt8) : Parser := { p with
  intermediates := p.intermediates.push b
}

/-- Set private marker (?, >, <, =) for CSI -/
def setPrivateMarker (p : Parser) (b : UInt8) : Parser := { p with
  privateMarker := some b
}

/-- Append to OSC string -/
def appendOSC (p : Parser) (c : Char) : Parser := { p with
  oscString := p.oscString.push c
}

/-- Finish current OSC param (on semicolon) -/
def finishOSCParam (p : Parser) : Parser := { p with
  oscParams := p.oscParams.push p.oscString
  oscString := ""
}

/-- Get all collected parameters including current one -/
def getAllParams (p : Parser) : Array (Option Nat) :=
  if p.currentParam.isSome || p.params.isEmpty then
    p.params.push p.currentParam
  else
    p.params

end Parser

end Vane.Parser
