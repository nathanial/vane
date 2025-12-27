/-
  Vane.PTY.FFI - FFI bindings for pseudo-terminal management
-/

import Vane.PTY.Types

namespace Vane.PTY

/--
  Open a new PTY with the specified shell.

  Spawns a child process running the shell, connected via a pseudo-terminal.
  The PTY is set to non-blocking mode for reads.

  - `shell`: Path to the shell executable (e.g., "/bin/zsh")
  - `size`: Initial terminal size in columns and rows
-/
@[extern "vane_pty_open"]
opaque PTY.open (shell : @& String) (cols rows : UInt16) : IO PTY

/--
  Read bytes from the PTY (non-blocking).

  Returns available bytes up to `maxBytes`. If no data is available,
  returns an empty ByteArray (does not block).
-/
@[extern "vane_pty_read"]
opaque PTY.read (pty : @& PTY) (maxBytes : UInt32) : IO ByteArray

/--
  Write bytes to the PTY.

  Sends the given bytes to the shell process's stdin.
  Blocks until all bytes are written.
-/
@[extern "vane_pty_write"]
opaque PTY.write (pty : @& PTY) (data : @& ByteArray) : IO Unit

/--
  Resize the PTY.

  Notifies the shell process of the new terminal size via TIOCSWINSZ.
  The shell will typically send SIGWINCH to foreground processes.
-/
@[extern "vane_pty_resize"]
opaque PTY.resize (pty : @& PTY) (cols rows : UInt16) : IO Unit

/--
  Poll for available data (non-blocking check).

  - `timeoutMs`: Maximum time to wait in milliseconds (0 for immediate check)
  - Returns: `true` if data is available to read
-/
@[extern "vane_pty_poll"]
opaque PTY.poll (pty : @& PTY) (timeoutMs : UInt32) : IO Bool

/--
  Close the PTY and terminate the shell process.

  Sends SIGHUP to the child process and waits for it to exit.
  The PTY handle should not be used after this call.
-/
@[extern "vane_pty_close"]
opaque PTY.close (pty : PTY) : IO Unit

/--
  Check if the shell process is still alive.

  Returns `false` if the child has exited.
-/
@[extern "vane_pty_is_alive"]
opaque PTY.isAlive (pty : @& PTY) : IO Bool

namespace PTY

/-- Open a PTY with the given shell and size -/
def openWithSize (shell : String) (size : Size) : IO PTY :=
  PTY.open shell size.cols size.rows

/-- Open a PTY with default size (80x24) -/
def openDefault (shell : String := "/bin/zsh") : IO PTY :=
  PTY.open shell 80 24

/-- Read available bytes as a String (UTF-8 decode) -/
def readString (pty : PTY) (maxBytes : UInt32 := 65536) : IO String := do
  let bytes ← pty.read maxBytes
  pure (String.fromUTF8! bytes)

/-- Write a String to the PTY -/
def writeString (pty : PTY) (s : String) : IO Unit :=
  pty.write s.toUTF8

/-- Resize using a Size struct -/
def resizeToSize (pty : PTY) (size : Size) : IO Unit :=
  pty.resize size.cols size.rows

/-- Read with timeout, returning None if no data available -/
def readWithTimeout (pty : PTY) (timeoutMs : UInt32) (maxBytes : UInt32 := 65536) : IO (Option ByteArray) := do
  let hasData ← pty.poll timeoutMs
  if hasData then
    let bytes ← pty.read maxBytes
    pure (some bytes)
  else
    pure none

end PTY

end Vane.PTY
