/-
  Vane.Parser.Types - Types for the ANSI/VT100 escape sequence parser
-/

namespace Vane.Parser

-- Control characters (C0 set)
namespace C0
  def NUL : UInt8 := 0x00  -- Null
  def SOH : UInt8 := 0x01  -- Start of Heading
  def STX : UInt8 := 0x02  -- Start of Text
  def ETX : UInt8 := 0x03  -- End of Text (Ctrl+C)
  def EOT : UInt8 := 0x04  -- End of Transmission (Ctrl+D)
  def ENQ : UInt8 := 0x05  -- Enquiry
  def ACK : UInt8 := 0x06  -- Acknowledge
  def BEL : UInt8 := 0x07  -- Bell
  def BS  : UInt8 := 0x08  -- Backspace
  def HT  : UInt8 := 0x09  -- Horizontal Tab
  def LF  : UInt8 := 0x0A  -- Line Feed
  def VT  : UInt8 := 0x0B  -- Vertical Tab
  def FF  : UInt8 := 0x0C  -- Form Feed
  def CR  : UInt8 := 0x0D  -- Carriage Return
  def SO  : UInt8 := 0x0E  -- Shift Out
  def SI  : UInt8 := 0x0F  -- Shift In
  def DLE : UInt8 := 0x10  -- Data Link Escape
  def DC1 : UInt8 := 0x11  -- Device Control 1 (XON)
  def DC2 : UInt8 := 0x12  -- Device Control 2
  def DC3 : UInt8 := 0x13  -- Device Control 3 (XOFF)
  def DC4 : UInt8 := 0x14  -- Device Control 4
  def NAK : UInt8 := 0x15  -- Negative Acknowledge
  def SYN : UInt8 := 0x16  -- Synchronous Idle
  def ETB : UInt8 := 0x17  -- End of Transmission Block
  def CAN : UInt8 := 0x18  -- Cancel
  def EM  : UInt8 := 0x19  -- End of Medium
  def SUB : UInt8 := 0x1A  -- Substitute (Ctrl+Z)
  def ESC : UInt8 := 0x1B  -- Escape
  def FS  : UInt8 := 0x1C  -- File Separator
  def GS  : UInt8 := 0x1D  -- Group Separator
  def RS  : UInt8 := 0x1E  -- Record Separator
  def US  : UInt8 := 0x1F  -- Unit Separator
  def DEL : UInt8 := 0x7F  -- Delete
end C0

-- C1 control characters (when received as ESC + byte)
namespace C1
  def IND : UInt8 := 0x44  -- 'D' - Index (move down one line)
  def NEL : UInt8 := 0x45  -- 'E' - Next Line
  def HTS : UInt8 := 0x48  -- 'H' - Horizontal Tab Set
  def RI  : UInt8 := 0x4D  -- 'M' - Reverse Index (move up one line)
  def SS2 : UInt8 := 0x4E  -- 'N' - Single Shift 2
  def SS3 : UInt8 := 0x4F  -- 'O' - Single Shift 3
  def DCS : UInt8 := 0x50  -- 'P' - Device Control String
  def SPA : UInt8 := 0x56  -- 'V' - Start of Protected Area
  def EPA : UInt8 := 0x57  -- 'W' - End of Protected Area
  def SOS : UInt8 := 0x58  -- 'X' - Start of String
  def DECID : UInt8 := 0x5A -- 'Z' - DEC identification
  def CSI : UInt8 := 0x5B  -- '[' - Control Sequence Introducer
  def ST  : UInt8 := 0x5C  -- '\' - String Terminator
  def OSC : UInt8 := 0x5D  -- ']' - Operating System Command
  def PM  : UInt8 := 0x5E  -- '^' - Privacy Message
  def APC : UInt8 := 0x5F  -- '_' - Application Program Command
end C1

/-- Actions the parser produces for the terminal to execute -/
inductive Action where
  /-- Print a character at the current cursor position -/
  | print (char : Char)
  /-- Execute a C0 control character (BEL, BS, HT, LF, CR, etc.) -/
  | execute (byte : UInt8)
  /-- A CSI sequence has been parsed -/
  | csiDispatch (params : Array (Option Nat)) (intermediates : Array UInt8) (finalByte : UInt8)
  /-- An ESC sequence has been parsed (non-CSI) -/
  | escDispatch (intermediates : Array UInt8) (finalByte : UInt8)
  /-- OSC string received -/
  | oscDispatch (params : Array String)
  /-- DCS hook (start of DCS string) -/
  | dcsHook (params : Array (Option Nat)) (intermediates : Array UInt8) (finalByte : UInt8)
  /-- DCS data byte -/
  | dcsPut (byte : UInt8)
  /-- DCS unhook (end of DCS string) -/
  | dcsUnhook
  /-- Error/invalid sequence (for debugging) -/
  | error (msg : String)
  deriving Repr, BEq

/-- Check if a byte is a C0 control character -/
def isC0Control (b : UInt8) : Bool :=
  b < 0x20 || b == C0.DEL

/-- Check if a byte is printable (0x20-0x7E) -/
def isPrintable (b : UInt8) : Bool :=
  b >= 0x20 && b < 0x7F

/-- Check if a byte is a CSI parameter byte (0x30-0x3F: 0-9, :, ;, <, =, >, ?) -/
def isCSIParam (b : UInt8) : Bool :=
  b >= 0x30 && b <= 0x3F

/-- Check if a byte is a CSI intermediate byte (0x20-0x2F: space through /) -/
def isCSIIntermediate (b : UInt8) : Bool :=
  b >= 0x20 && b <= 0x2F

/-- Check if a byte is a CSI final byte (0x40-0x7E: @ through ~) -/
def isCSIFinal (b : UInt8) : Bool :=
  b >= 0x40 && b <= 0x7E

/-- Check if a byte starts an ESC sequence that leads to a string (DCS, OSC, etc.) -/
def isStringStart (b : UInt8) : Bool :=
  b == C1.DCS || b == C1.OSC || b == C1.SOS || b == C1.PM || b == C1.APC

end Vane.Parser
