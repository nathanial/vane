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

namespace Vane

-- Version information
def version : String := "0.1.0"

end Vane
