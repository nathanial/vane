/-
  Vane.Parser.CSI - CSI sequence dispatch and terminal commands
-/

import Vane.Parser.SGR

namespace Vane.Parser

/-- Terminal commands produced by parsing CSI sequences -/
inductive TerminalCommand where
  /-- Move cursor up n rows -/
  | cursorUp (n : Nat)
  /-- Move cursor down n rows -/
  | cursorDown (n : Nat)
  /-- Move cursor forward n columns -/
  | cursorForward (n : Nat)
  /-- Move cursor back n columns -/
  | cursorBack (n : Nat)
  /-- Move cursor to next line, n lines down -/
  | cursorNextLine (n : Nat)
  /-- Move cursor to previous line, n lines up -/
  | cursorPrevLine (n : Nat)
  /-- Move cursor to column n -/
  | cursorColumn (col : Nat)
  /-- Move cursor to row n -/
  | cursorRow (row : Nat)
  /-- Move cursor to (row, col) - 1-based -/
  | cursorPosition (row col : Nat)
  /-- Erase in display: 0=below, 1=above, 2=all, 3=all+scrollback -/
  | eraseDisplay (mode : Nat)
  /-- Erase in line: 0=right, 1=left, 2=all -/
  | eraseLine (mode : Nat)
  /-- Insert n blank lines at cursor -/
  | insertLines (n : Nat)
  /-- Delete n lines at cursor -/
  | deleteLines (n : Nat)
  /-- Insert n blank characters at cursor -/
  | insertChars (n : Nat)
  /-- Delete n characters at cursor -/
  | deleteChars (n : Nat)
  /-- Erase n characters at cursor (replace with spaces) -/
  | eraseChars (n : Nat)
  /-- Scroll up n lines -/
  | scrollUp (n : Nat)
  /-- Scroll down n lines -/
  | scrollDown (n : Nat)
  /-- Set scroll region (top, bottom) - 1-based, 0 means default -/
  | setScrollRegion (top bottom : Nat)
  /-- Apply SGR (colors/attributes) -/
  | sgr (state : SGRState)
  /-- Set mode (SM) -/
  | setMode (mode : Nat) (private_ : Bool)
  /-- Reset mode (RM) -/
  | resetMode (mode : Nat) (private_ : Bool)
  /-- Device status report request -/
  | deviceStatusReport (mode : Nat)
  /-- Cursor position report request -/
  | cursorPositionReport
  /-- Save cursor position (DECSC via CSI s) -/
  | saveCursor
  /-- Restore cursor position (DECRC via CSI u) -/
  | restoreCursor
  /-- Soft terminal reset -/
  | softReset
  /-- Tab clear -/
  | tabClear (mode : Nat)
  /-- Set tab stop at current column -/
  | setTabStop
  /-- Repeat last character n times -/
  | repeatChar (n : Nat)
  /-- Unknown/unhandled sequence -/
  | unknown (params : Array (Option Nat)) (intermediates : Array UInt8) (finalByte : UInt8)
  deriving Repr

namespace TerminalCommand

/-- Get parameter with default value -/
private def getParam (params : Array (Option Nat)) (idx : Nat) (default : Nat := 1) : Nat :=
  (params.getD idx none).getD default

/-- Check if intermediates contain a specific byte -/
private def hasIntermediate (intermediates : Array UInt8) (b : UInt8) : Bool :=
  intermediates.contains b

/--
  Dispatch a CSI sequence to a terminal command.

  CSI format: ESC [ params intermediates final
  - params: semicolon-separated numbers (possibly with ? prefix for private)
  - intermediates: bytes 0x20-0x2F
  - final: byte 0x40-0x7E
-/
def fromCSI (params : Array (Option Nat)) (intermediates : Array UInt8) (finalByte : UInt8)
    (privateMarker : Option UInt8 := none) : TerminalCommand :=
  let isPrivate := privateMarker == some 0x3F  -- '?'

  match finalByte.toNat with
  -- Cursor movement
  | 0x41 => .cursorUp (getParam params 0)           -- 'A' - CUU
  | 0x42 => .cursorDown (getParam params 0)         -- 'B' - CUD
  | 0x43 => .cursorForward (getParam params 0)      -- 'C' - CUF
  | 0x44 => .cursorBack (getParam params 0)         -- 'D' - CUB
  | 0x45 => .cursorNextLine (getParam params 0)     -- 'E' - CNL
  | 0x46 => .cursorPrevLine (getParam params 0)     -- 'F' - CPL
  | 0x47 => .cursorColumn (getParam params 0)       -- 'G' - CHA
  | 0x48 => .cursorPosition (getParam params 0) (getParam params 1)  -- 'H' - CUP
  | 0x49 => .cursorForward (getParam params 0)      -- 'I' - CHT (tab forward)
  | 0x4A =>                                         -- 'J' - ED (erase display)
    .eraseDisplay (getParam params 0 0)
  | 0x4B =>                                         -- 'K' - EL (erase line)
    .eraseLine (getParam params 0 0)
  | 0x4C => .insertLines (getParam params 0)        -- 'L' - IL
  | 0x4D => .deleteLines (getParam params 0)        -- 'M' - DL
  | 0x50 => .deleteChars (getParam params 0)        -- 'P' - DCH
  | 0x53 => .scrollUp (getParam params 0)           -- 'S' - SU
  | 0x54 => .scrollDown (getParam params 0)         -- 'T' - SD
  | 0x58 => .eraseChars (getParam params 0)         -- 'X' - ECH
  | 0x5A => .cursorBack (getParam params 0)         -- 'Z' - CBT (tab backward)
  | 0x60 => .cursorColumn (getParam params 0)       -- '`' - HPA
  | 0x61 => .cursorForward (getParam params 0)      -- 'a' - HPR
  | 0x62 => .repeatChar (getParam params 0)         -- 'b' - REP
  | 0x64 => .cursorRow (getParam params 0)          -- 'd' - VPA
  | 0x65 => .cursorDown (getParam params 0)         -- 'e' - VPR
  | 0x66 => .cursorPosition (getParam params 0) (getParam params 1)  -- 'f' - HVP
  | 0x67 => .tabClear (getParam params 0 0)         -- 'g' - TBC
  | 0x68 =>                                         -- 'h' - SM (set mode)
    .setMode (getParam params 0 0) isPrivate
  | 0x6C =>                                         -- 'l' - RM (reset mode)
    .resetMode (getParam params 0 0) isPrivate
  | 0x6D =>                                         -- 'm' - SGR
    .sgr (parseSGR params)
  | 0x6E =>                                         -- 'n' - DSR
    if getParam params 0 0 == 6 then .cursorPositionReport else .deviceStatusReport (getParam params 0 0)
  | 0x70 =>                                         -- 'p' - various
    if hasIntermediate intermediates 0x21 then      -- '!'
      .softReset  -- DECSTR
    else
      .unknown params intermediates finalByte
  | 0x72 =>                                         -- 'r' - DECSTBM (set scroll region)
    .setScrollRegion (getParam params 0 0) (getParam params 1 0)
  | 0x73 => .saveCursor                             -- 's' - SCOSC (or DECSLRM with params)
  | 0x75 => .restoreCursor                          -- 'u' - SCORC
  | 0x40 => .insertChars (getParam params 0)        -- '@' - ICH

  | _ => .unknown params intermediates finalByte

end TerminalCommand

-- Well-known DEC private modes
namespace DecMode
  def cursorKeys : Nat := 1          -- DECCKM: Application cursor keys
  def ansi : Nat := 2                -- DECANM: ANSI/VT52 mode
  def column132 : Nat := 3           -- DECCOLM: 132 column mode
  def smoothScroll : Nat := 4        -- DECSCLM: Smooth scroll
  def reverseVideo : Nat := 5        -- DECSCNM: Reverse video
  def origin : Nat := 6              -- DECOM: Origin mode
  def autoWrap : Nat := 7            -- DECAWM: Auto-wrap
  def autoRepeat : Nat := 8          -- DECARM: Auto-repeat
  def cursorBlink : Nat := 12        -- att610: Cursor blink
  def cursorVisible : Nat := 25      -- DECTCEM: Cursor visible
  def mouseX10 : Nat := 9            -- X10 mouse reporting
  def mouseVt200 : Nat := 1000       -- VT200 mouse reporting
  def mouseVt200Highlight : Nat := 1001
  def mouseBtnEvent : Nat := 1002    -- Button-event tracking
  def mouseAnyEvent : Nat := 1003    -- Any-event tracking
  def mouseFocusEvent : Nat := 1004  -- Focus in/out events
  def mouseUtf8 : Nat := 1005        -- UTF-8 mouse mode
  def mouseSgr : Nat := 1006         -- SGR mouse mode
  def mouseUrxvt : Nat := 1015       -- urxvt mouse mode
  def mousePixels : Nat := 1016      -- SGR-Pixels mouse mode
  def altScreen : Nat := 47          -- Use alternate screen buffer
  def altScreenClear : Nat := 1047   -- Use alt screen, clear on switch
  def saveCursor : Nat := 1048       -- Save cursor
  def altScreenSaveCursor : Nat := 1049  -- Alt screen + save cursor
  def bracketedPaste : Nat := 2004   -- Bracketed paste mode
end DecMode

end Vane.Parser
