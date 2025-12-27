# Vane

A hardware-accelerated terminal emulator written in Lean 4, using Metal for GPU rendering.

## Overview

Vane is a modern terminal emulator that leverages the [afferent](../afferent) graphics framework to provide GPU-accelerated text rendering on macOS. It features a pure-Lean VT100/ANSI escape sequence parser and full color support.

## Features

- **GPU-accelerated rendering** via Metal (macOS)
- **Pure Lean ANSI parser** - no external dependencies like libvte
- **Full color support** - 16-color, 256-color, and 24-bit RGB true color
- **Unicode support** - box-drawing characters and common symbols
- **Scrollback buffer** - configurable history (default 10,000 lines)
- **Text selection** - mouse selection with clipboard integration

## Requirements

- macOS (Metal GPU required)
- Lean 4.26.0
- FreeType (`brew install freetype`)

## Building

```bash
# Build the terminal emulator
./build.sh

# Run
lake exe vane

# Run tests
./test.sh
```

## Project Structure

```
vane/
├── Vane/
│   ├── Core/           # Core types (Cell, Buffer, Style, Color)
│   ├── Parser/         # VT100/ANSI escape sequence parser
│   ├── Terminal/       # Terminal state management
│   ├── PTY/            # Pseudo-terminal FFI bindings
│   ├── Input/          # Keyboard input encoding
│   ├── Render/         # GPU rendering integration
│   └── App/            # Application loop and configuration
├── VaneTests/          # Test suite
└── native/
    └── src/
        └── pty.c       # PTY FFI implementation (forkpty)
```

## Architecture

Vane is built on several components:

1. **Core Types** (`Vane/Core/`)
   - `Cell` - A single terminal character with foreground, background, and modifiers
   - `Buffer` - 2D grid of cells with scrollback support
   - `Style` - Color and text attribute specifications

2. **ANSI Parser** (`Vane/Parser/`)
   - VT500-compatible state machine
   - CSI sequence handling (cursor movement, screen manipulation)
   - SGR parsing for colors and text attributes

3. **PTY Management** (`Vane/PTY/`, `native/src/pty.c`)
   - Shell process spawning via `forkpty()`
   - Non-blocking I/O
   - Terminal resize handling

4. **Rendering** (`Vane/Render/`)
   - Integration with afferent's Metal-based text rendering
   - Efficient cell grid rendering with dirty region tracking

## Dependencies

- [afferent](../afferent) - Metal-based 2D/3D graphics framework
- [crucible](../crucible) - Test framework

## Status

**Work in Progress**

- [x] Core types (Cell, Buffer, Style, Color)
- [x] PTY FFI implementation (forkpty, read, write, resize, poll)
- [x] PTY Lean bindings with shell spawn demo
- [x] ANSI/VT100 parser
  - [x] VT500-compatible state machine
  - [x] CSI sequence dispatch (cursor, erase, scroll, modes)
  - [x] SGR parsing (16-color, 256-color, 24-bit RGB)
  - [x] OSC command parsing (window title, hyperlinks, clipboard)
- [x] Terminal state machine
  - [x] Cursor management (position, visibility, style, blink)
  - [x] Terminal modes (DECAWM, origin mode, alt screen, etc.)
  - [x] Complete terminal state with scrollback buffer
  - [x] Command executor (apply parsed commands to buffer)
- [x] Keyboard input encoding
  - [x] Special keys (arrows, function keys, navigation)
  - [x] Modifier handling (Ctrl, Alt, Shift, Cmd)
  - [x] Mouse event encoding (X10, SGR, SGR-Pixels)
  - [x] Bracketed paste support
- [ ] GPU rendering integration
- [ ] Scrollback navigation (view scrolling implemented)
- [ ] Text selection

## License

MIT License - see [LICENSE](LICENSE)
