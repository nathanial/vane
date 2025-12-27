/-
  Vane - Hardware-accelerated terminal emulator for Lean 4

  A GPU-accelerated terminal emulator using afferent for Metal rendering.
-/

-- Core types
import Vane.Core.Style
import Vane.Core.Cell
import Vane.Core.Buffer

-- PTY management
import Vane.PTY.Types
import Vane.PTY.FFI

-- ANSI/VT100 Parser
import Vane.Parser.Types
import Vane.Parser.State
import Vane.Parser.Machine
import Vane.Parser.SGR
import Vane.Parser.CSI
import Vane.Parser.OSC

-- Terminal state machine
import Vane.Terminal.Cursor
import Vane.Terminal.Modes
import Vane.Terminal.State
import Vane.Terminal.Executor

-- Input handling
import Vane.Input.KeyEncoder

-- Rendering
import Vane.Render.Grid

-- Application
import Vane.App.Config
import Vane.App.Input
import Vane.App.State
import Vane.App.Loop

namespace Vane

-- Version information
def version : String := "0.1.0"

end Vane
