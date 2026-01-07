/-
  Vane.Terminal.Executor - Apply parsed commands to terminal state
-/

import Vane.Parser.Types
import Vane.Parser.CSI
import Vane.Parser.OSC
import Vane.Parser.SGR
import Vane.Terminal.State

namespace Vane.Terminal

open Parser

/-- Response to send back to PTY -/
inductive Response where
  | none
  | data (bytes : ByteArray)
  | cursorPosition (row col : Nat)
  | deviceAttributes
  | primaryDeviceAttributes

namespace Response

/-- Encode response as bytes to send to PTY -/
def toBytes (r : Response) : ByteArray :=
  match r with
  | .none => ByteArray.empty
  | .data bytes => bytes
  | .cursorPosition row col =>
    -- CSI row ; col R
    let s := s!"\x1b[{row + 1};{col + 1}R"
    s.toUTF8
  | .deviceAttributes =>
    -- VT100 response
    "\x1b[?1;0c".toUTF8
  | .primaryDeviceAttributes =>
    "\x1b[?62;c".toUTF8

end Response

/-- Result of executing actions -/
structure ExecResult where
  state : TerminalState
  responses : Array Response := #[]

namespace TerminalState

/-- Execute a C0 control character -/
def executeC0 (ts : TerminalState) (byte : UInt8) : TerminalState :=
  match byte.toNat with
  | 0x07 => ts  -- BEL: ring bell (handled by frontend)
  | 0x08 => ts.backspace  -- BS
  | 0x09 => ts.tab  -- HT
  | 0x0A => -- LF (respects newLineMode)
    let ts := ts.lineFeed
    if ts.modes.newLineMode then ts.carriageReturn else ts
  | 0x0B => -- VT (same as LF)
    let ts := ts.lineFeed
    if ts.modes.newLineMode then ts.carriageReturn else ts
  | 0x0C => -- FF (same as LF)
    let ts := ts.lineFeed
    if ts.modes.newLineMode then ts.carriageReturn else ts
  | 0x0D => ts.carriageReturn  -- CR
  | 0x0E => ts  -- SO: shift out (character set, ignored)
  | 0x0F => ts  -- SI: shift in (character set, ignored)
  | _ => ts

/-- Execute an ESC sequence (non-CSI) -/
def executeEsc (ts : TerminalState) (intermediates : Array UInt8) (finalByte : UInt8) : TerminalState :=
  if intermediates.isEmpty then
    match finalByte.toNat with
    | 0x37 => ts.saveCursor      -- ESC 7 (DECSC)
    | 0x38 => ts.restoreCursor   -- ESC 8 (DECRC)
    | 0x44 => ts.lineFeed        -- ESC D (IND - index down)
    | 0x45 =>                    -- ESC E (NEL - next line)
      let ts := ts.lineFeed
      ts.carriageReturn
    | 0x48 =>                    -- ESC H (HTS - set tab stop)
      { ts with tabStops := ts.tabStops.set ts.cursor.col }
    | 0x4D =>                    -- ESC M (RI - reverse index)
      if ts.cursor.row <= ts.scrollRegion.top then
        ts.scrollDown 1
      else
        { ts with cursor := ts.cursor.moveUp 1 }
    | 0x63 => TerminalState.create ts.width ts.height ts.maxScrollback  -- ESC c (RIS - full reset)
    | _ => ts
  else if intermediates.size == 1 && intermediates[0]! == 0x23 then
    -- ESC # sequences
    match finalByte.toNat with
    | 0x38 =>  -- ESC # 8 (DECALN - screen alignment test)
      ts.withCurrentBuffer fun buf =>
        buf.fill (Cell.new 'E')
    | _ => ts
  else
    ts

/-- Execute a CSI command -/
def executeCSI (ts : TerminalState) (cmd : TerminalCommand) : ExecResult :=
  match cmd with
  | .cursorUp n =>
    { state := { ts with cursor := ts.cursor.moveUp n } }
  | .cursorDown n =>
    { state := { ts with cursor := ts.cursor.moveDown n ts.height } }
  | .cursorForward n =>
    { state := { ts with cursor := ts.cursor.moveRight n ts.width } }
  | .cursorBack n =>
    { state := { ts with cursor := ts.cursor.moveLeft n } }
  | .cursorNextLine n =>
    { state := { ts with cursor := ts.cursor.toNextLine n ts.height } }
  | .cursorPrevLine n =>
    { state := { ts with cursor := ts.cursor.toPrevLine n } }
  | .cursorColumn col =>
    { state := { ts with cursor := ts.cursor.toColumn (if col == 0 then 0 else col - 1) ts.width } }
  | .cursorRow row =>
    { state := { ts with cursor := ts.cursor.toRow (if row == 0 then 0 else row - 1) ts.height } }
  | .cursorPosition row col =>
    { state := ts.cursorTo row col }
  | .eraseDisplay mode =>
    { state := ts.eraseDisplay mode }
  | .eraseLine mode =>
    { state := ts.eraseLine mode }
  | .insertLines n =>
    { state := ts.insertLines n }
  | .deleteLines n =>
    { state := ts.deleteLines n }
  | .insertChars n =>
    { state := ts.insertChars n }
  | .deleteChars n =>
    { state := ts.deleteChars n }
  | .eraseChars n =>
    { state := ts.eraseChars n }
  | .scrollUp n =>
    { state := ts.scrollUp n }
  | .scrollDown n =>
    { state := ts.scrollDown n }
  | .setScrollRegion top bottom =>
    { state := ts.setScrollRegion top bottom }
  | .sgr sgrState =>
    let newStyle := sgrState.applyToStyle ts.currentStyle
    { state := { ts with currentStyle := newStyle } }
  | .setMode mode isPrivate =>
    let modes := if isPrivate then
      ts.modes.setPrivateMode mode
    else
      ts.modes.setAnsiMode mode
    -- Handle special mode side effects
    let ts := if isPrivate && mode == 1049 then
      ts.switchToAltScreen true
    else if isPrivate && mode == 47 then
      ts.switchToAltScreen false
    else if isPrivate && mode == 1047 then
      ts.switchToAltScreen true
    else
      ts
    { state := { ts with modes := modes } }
  | .resetMode mode isPrivate =>
    let modes := if isPrivate then
      ts.modes.resetPrivateMode mode
    else
      ts.modes.resetAnsiMode mode
    -- Handle special mode side effects
    let ts := if isPrivate && (mode == 1049 || mode == 47 || mode == 1047) then
      ts.switchToPrimaryScreen
    else
      ts
    { state := { ts with modes := modes } }
  | .deviceStatusReport mode =>
    match mode with
    | 5 => -- Report status (we're always OK)
      { state := ts, responses := #[.data "\x1b[0n".toUTF8] }
    | 6 => -- Report cursor position
      { state := ts, responses := #[.cursorPosition ts.cursor.row ts.cursor.col] }
    | _ => { state := ts }
  | .cursorPositionReport =>
    { state := ts, responses := #[.cursorPosition ts.cursor.row ts.cursor.col] }
  | .saveCursor =>
    { state := ts.saveCursor }
  | .restoreCursor =>
    { state := ts.restoreCursor }
  | .softReset =>
    { state := ts.softReset }
  | .tabClear mode =>
    match mode with
    | 0 => { state := { ts with tabStops := ts.tabStops.clear ts.cursor.col } }
    | 3 => { state := { ts with tabStops := ts.tabStops.clearAll } }
    | _ => { state := ts }
  | .setTabStop =>
    { state := { ts with tabStops := ts.tabStops.set ts.cursor.col } }
  | .repeatChar n =>
    match ts.lastChar with
    | some c =>
      let result := Id.run do
        let mut r := ts
        for _ in [0:n] do
          r := r.writeChar c
        r
      { state := result }
    | none => { state := ts }
  | .unknown _ _ _ =>
    { state := ts }

/-- Execute an OSC command -/
def executeOSC (ts : TerminalState) (cmd : OSCCommand) : TerminalState :=
  match cmd with
  | .setTitle title => { ts with title := title, iconName := title }
  | .setIconName name => { ts with iconName := name }
  | .setWindowTitle title => { ts with title := title }
  | .setCurrentDirectory uri => { ts with currentDirectory := uri }
  | .setColor _ _ => ts  -- Color palette changes (handled by frontend)
  | .resetColor _ => ts
  | .hyperlink _ _ => ts  -- Hyperlinks (could be stored in cells)
  | .setClipboard _ _ => ts  -- Clipboard (handled by frontend)
  | .queryClipboard _ => ts
  | .notify _ => ts  -- Notifications (handled by frontend)
  | .setXProperty _ _ => ts
  | .unknown _ => ts

/-- Execute a single parser action -/
def executeAction (ts : TerminalState) (action : Action) : ExecResult :=
  match action with
  | .print c =>
    { state := ts.writeChar c }
  | .execute byte =>
    { state := ts.executeC0 byte }
  | .csiDispatch params intermediates finalByte =>
    let cmd := TerminalCommand.fromCSI params intermediates finalByte
    ts.executeCSI cmd
  | .escDispatch intermediates finalByte =>
    { state := ts.executeEsc intermediates finalByte }
  | .oscDispatch params =>
    let cmd := OSCCommand.fromParams params
    { state := ts.executeOSC cmd }
  | .dcsHook _ _ _ => { state := ts }  -- DCS handling (not implemented yet)
  | .dcsPut _ => { state := ts }
  | .dcsUnhook => { state := ts }
  | .error _ => { state := ts }

/-- Execute multiple parser actions -/
def executeActions (ts : TerminalState) (actions : Array Action) : ExecResult := Id.run do
  let mut currentState := ts
  let mut responses : Array Response := #[]

  for action in actions do
    let result := currentState.executeAction action
    currentState := result.state
    responses := responses ++ result.responses

  -- Reset scroll view on any input (typing brings view to bottom)
  let finalState := if actions.any fun a => match a with | .print _ => true | _ => false then
    currentState.scrollViewToBottom
  else
    currentState

  { state := finalState, responses }

end TerminalState

end Vane.Terminal
