import Lake
open Lake DSL
open System (FilePath)

package vane where
  version := v!"0.1.0"

require afferent from git "https://github.com/nathanial/afferent" @ "v0.0.6"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.3"

-- Common link arguments (inherit from afferent for Metal/Cocoa)
def commonLinkArgs : Array String := #[
  "-framework", "Metal",
  "-framework", "Cocoa",
  "-framework", "QuartzCore",
  "-framework", "Foundation",
  "-lobjc",
  "-L/opt/homebrew/lib",
  "-L/usr/local/lib",
  "-lfreetype",
  "-lutil",  -- For forkpty on macOS
  "-lc++"
]

@[default_target]
lean_lib Vane where
  roots := #[`Vane]

lean_lib VaneTests where
  globs := #[.submodules `VaneTests]

lean_exe vane where
  root := `Main
  moreLinkArgs := commonLinkArgs

@[test_driver]
lean_exe vane_tests where
  root := `VaneTests.Main
  moreLinkArgs := commonLinkArgs

-- PTY FFI
target pty_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "pty.o"
  let srcFile := pkg.dir / "native" / "src" / "pty.c"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

extern_lib libvane_native pkg := do
  let name := nameToStaticLib "vane_native"
  let ptyO ← pty_o.fetch
  buildStaticLib (pkg.staticLibDir / name) #[ptyO]
