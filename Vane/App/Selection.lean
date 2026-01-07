/-
  Vane.App.Selection - Text selection types and utilities
-/

namespace Vane.App

/-- Text selection range in cell coordinates -/
structure SelectionRange where
  /-- Start column -/
  startCol : Nat
  /-- Start row -/
  startRow : Nat
  /-- End column (exclusive) -/
  endCol : Nat
  /-- End row -/
  endRow : Nat
  deriving Repr, BEq, Inhabited

namespace SelectionRange

/-- Check if a cell is within the selection -/
def contains (sel : SelectionRange) (col row : Nat) : Bool :=
  -- Normalize so start <= end
  let (sc, sr, ec, er) :=
    if sel.startRow < sel.endRow || (sel.startRow == sel.endRow && sel.startCol <= sel.endCol) then
      (sel.startCol, sel.startRow, sel.endCol, sel.endRow)
    else
      (sel.endCol, sel.endRow, sel.startCol, sel.startRow)
  -- Check if cell is in selection
  if sr == er then
    -- Single line selection
    row == sr && col >= sc && col < ec
  else if row == sr then
    -- First line of multi-line selection
    col >= sc
  else if row == er then
    -- Last line of multi-line selection
    col < ec
  else
    -- Middle lines are fully selected
    row > sr && row < er

/-- Get normalized selection (start <= end) -/
def normalize (sel : SelectionRange) : SelectionRange :=
  if sel.startRow < sel.endRow || (sel.startRow == sel.endRow && sel.startCol <= sel.endCol) then
    sel
  else
    { startCol := sel.endCol, startRow := sel.endRow,
      endCol := sel.startCol, endRow := sel.startRow }

/-- Check if selection is empty -/
def isEmpty (sel : SelectionRange) : Bool :=
  sel.startCol == sel.endCol && sel.startRow == sel.endRow

end SelectionRange

end Vane.App
