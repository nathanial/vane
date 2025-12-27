/-
  Vane.Parser.OSC - Operating System Command handling
-/

namespace Vane.Parser

/-- OSC commands -/
inductive OSCCommand where
  /-- Set window/icon title (OSC 0) -/
  | setTitle (title : String)
  /-- Set icon name (OSC 1) -/
  | setIconName (name : String)
  /-- Set window title (OSC 2) -/
  | setWindowTitle (title : String)
  /-- Set X property (OSC 3) -/
  | setXProperty (prop value : String)
  /-- Change color (OSC 4, 10-19) -/
  | setColor (index : Nat) (spec : String)
  /-- Reset color (OSC 104, 110-119) -/
  | resetColor (index : Nat)
  /-- Hyperlink (OSC 8) -/
  | hyperlink (params uri : String)
  /-- Set clipboard (OSC 52) -/
  | setClipboard (clipboard data : String)
  /-- Query clipboard (OSC 52 with ?) -/
  | queryClipboard (clipboard : String)
  /-- Notify (OSC 9 or OSC 777) -/
  | notify (message : String)
  /-- Set current directory (OSC 7) -/
  | setCurrentDirectory (uri : String)
  /-- Unknown OSC -/
  | unknown (params : Array String)
  deriving Repr

namespace OSCCommand

/-- Parse OSC parameters into a command -/
def fromParams (params : Array String) : OSCCommand :=
  if params.isEmpty then
    .unknown params
  else
    -- First param is the command code
    let codeStr := params.getD 0 ""
    match codeStr.toNat? with
    | some 0 =>
      -- OSC 0 ; title ST - Set icon name and window title
      let title := params.getD 1 ""
      .setTitle title
    | some 1 =>
      -- OSC 1 ; name ST - Set icon name
      let name := params.getD 1 ""
      .setIconName name
    | some 2 =>
      -- OSC 2 ; title ST - Set window title
      let title := params.getD 1 ""
      .setWindowTitle title
    | some 3 =>
      -- OSC 3 ; prop=value ST - Set X property
      let propVal := params.getD 1 ""
      match propVal.splitOn "=" with
      | [prop, val] => .setXProperty prop val
      | _ => .unknown params
    | some 4 =>
      -- OSC 4 ; index ; spec ST - Change color palette entry
      if params.size >= 3 then
        match (params.getD 1 "").toNat? with
        | some idx => .setColor idx (params.getD 2 "")
        | none => .unknown params
      else
        .unknown params
    | some 7 =>
      -- OSC 7 ; uri ST - Set current working directory
      .setCurrentDirectory (params.getD 1 "")
    | some 8 =>
      -- OSC 8 ; params ; uri ST - Hyperlink
      let paramStr := params.getD 1 ""
      let uri := params.getD 2 ""
      .hyperlink paramStr uri
    | some 9 =>
      -- OSC 9 ; message ST - iTerm2 notification
      .notify (params.getD 1 "")
    | some 10 =>
      -- OSC 10 ; spec ST - Set foreground color
      .setColor 10 (params.getD 1 "")
    | some 11 =>
      -- OSC 11 ; spec ST - Set background color
      .setColor 11 (params.getD 1 "")
    | some 12 =>
      -- OSC 12 ; spec ST - Set cursor color
      .setColor 12 (params.getD 1 "")
    | some 52 =>
      -- OSC 52 ; clipboard ; data ST - Clipboard
      let clipboard := params.getD 1 ""
      let data := params.getD 2 ""
      if data == "?" then
        .queryClipboard clipboard
      else
        .setClipboard clipboard data
    | some 104 =>
      -- OSC 104 ; index ST - Reset color palette entry
      match (params.getD 1 "").toNat? with
      | some idx => .resetColor idx
      | none => .resetColor 0  -- Reset all if no index
    | some 110 => .resetColor 10  -- Reset foreground
    | some 111 => .resetColor 11  -- Reset background
    | some 112 => .resetColor 12  -- Reset cursor color
    | some 777 =>
      -- OSC 777 ; notify ; ... ST - rxvt-unicode notification
      .notify (params.getD 2 "")
    | _ =>
      .unknown params

end OSCCommand

end Vane.Parser
