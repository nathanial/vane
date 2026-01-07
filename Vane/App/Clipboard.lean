/-
  Vane.App.Clipboard - System clipboard FFI bindings
-/

namespace Vane.App.Clipboard

/-- Set text to system clipboard -/
@[extern "lean_vane_clipboard_set"]
opaque set (text : @& String) : IO Unit

/-- Get text from system clipboard -/
@[extern "lean_vane_clipboard_get"]
opaque get : IO String

end Vane.App.Clipboard
