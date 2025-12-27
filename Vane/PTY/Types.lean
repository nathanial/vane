/-
  Vane.PTY.Types - Opaque PTY handle type
-/

namespace Vane.PTY

/-- Opaque handle to a pseudo-terminal -/
opaque PTYPointed : NonemptyType
def PTY : Type := PTYPointed.type

instance : Nonempty PTY := PTYPointed.property

/-- Terminal size in columns and rows -/
structure Size where
  cols : UInt16
  rows : UInt16
  deriving Repr, BEq

namespace Size

def default : Size := { cols := 80, rows := 24 }

def fromNat (cols rows : Nat) : Size := {
  cols := cols.toUInt16
  rows := rows.toUInt16
}

end Size

end Vane.PTY
