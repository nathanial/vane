# Vane Terminal Emulator Roadmap

This document outlines potential improvements, new features, and code cleanup opportunities for the Vane hardware-accelerated terminal emulator.

---

## Feature Proposals

### [Priority: High] GPU Rendering Integration
**Description:** Complete the GPU-accelerated rendering pipeline using afferent's Metal backend.
**Rationale:** Listed as incomplete in README. This is the core differentiating feature of Vane compared to other terminal emulators.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Render/Grid.lean`
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Loop.lean`
**Estimated Effort:** Large
**Dependencies:** afferent rendering pipeline, font atlas generation

### [Priority: High] Text Selection and Clipboard
**Description:** Implement mouse-based text selection with clipboard copy/paste support.
**Rationale:** Listed as incomplete in README. Essential for terminal usability.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/State.lean` (selection state)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Render/Grid.lean` (selection highlighting)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Loop.lean` (mouse events)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Input/KeyEncoder.lean` (paste encoding)
**Estimated Effort:** Medium
**Dependencies:** OSC 52 clipboard support already partially implemented

### [Priority: High] Window Resize Handling
**Description:** Implement window resize detection and proper terminal dimension recalculation.
**Rationale:** Currently `handleResize` exists in State.lean but is not wired up in the main loop.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Loop.lean` (line 102-131)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/State.lean`
**Estimated Effort:** Small
**Dependencies:** None

### [Priority: High] Scrollback Navigation
**Description:** Enable keyboard and mouse wheel scrollback buffer navigation.
**Rationale:** Listed as incomplete in README. Terminal state already has `scrollOffset`, `scrollViewUp`, `scrollViewDown` but they are not connected to input.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Loop.lean` (add scroll input handling)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Render/Grid.lean` (render scrollback lines)
**Estimated Effort:** Medium
**Dependencies:** None

### [Priority: Medium] Unicode and Wide Character Support
**Description:** Proper handling of Unicode combining characters and East Asian wide characters (double-width).
**Rationale:** Current implementation treats all characters as single-width. Many terminal applications use wide characters.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Core/Cell.lean` (add width property)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Core/Buffer.lean` (handle wide characters spanning cells)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Terminal/State.lean` (writeChar wide char handling)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Render/Grid.lean` (wide character rendering)
**Estimated Effort:** Medium
**Dependencies:** Unicode character width detection library

### [Priority: Medium] Font Style Rendering (Bold/Italic)
**Description:** Render bold, italic, and bold-italic text using font variants or synthetic styling.
**Rationale:** SGR modifiers are parsed and stored but rendering ignores them except for color inversion.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Render/Grid.lean` (line 54-71)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/State.lean` (multiple font loading)
**Estimated Effort:** Medium
**Dependencies:** afferent font loading with variants

### [Priority: Medium] Underline and Strikethrough Rendering
**Description:** Draw underline and strikethrough decorations for styled text.
**Rationale:** SGR codes 4 and 9 are parsed but not rendered.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Render/Grid.lean`
**Estimated Effort:** Small
**Dependencies:** None

### [Priority: Medium] Configurable Color Schemes
**Description:** Allow users to configure the 16 ANSI colors and default foreground/background.
**Rationale:** Currently colors are hardcoded in `Color.toRGBA` (Style.lean lines 59-104).
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Core/Style.lean`
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Config.lean`
**Estimated Effort:** Medium
**Dependencies:** Configuration file parsing (could use totem)

### [Priority: Medium] Mouse Input Forwarding
**Description:** Forward mouse events to the PTY when mouse reporting modes are enabled.
**Rationale:** Mouse encoding is fully implemented in `KeyEncoder.lean` (MouseEncoder) but not connected.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Loop.lean`
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Terminal/Modes.lean` (already has mouse mode flags)
**Estimated Effort:** Medium
**Dependencies:** afferent mouse event API

### [Priority: Medium] Bell Notification
**Description:** Visual and/or audio notification when BEL character is received.
**Rationale:** BEL (0x07) is handled in `Executor.lean` line 51 but does nothing.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Terminal/Executor.lean`
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/State.lean` (bell state for visual bell)
**Estimated Effort:** Small
**Dependencies:** None

### [Priority: Medium] DCS Sequence Support
**Description:** Implement Device Control String handling for advanced terminal features.
**Rationale:** DCS parsing is implemented but execution is stubbed (Executor.lean lines 228-230).
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Terminal/Executor.lean`
**Estimated Effort:** Medium
**Dependencies:** None

### [Priority: Low] Hyperlink Support (OSC 8)
**Description:** Store hyperlink information in cells and allow clicking to open URLs.
**Rationale:** OSC 8 is parsed (OSC.lean lines 74-78) but links are not stored or rendered.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Core/Cell.lean` (add hyperlink field)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Terminal/Executor.lean`
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Render/Grid.lean`
**Estimated Effort:** Medium
**Dependencies:** None

### [Priority: Low] Sixel Graphics Support
**Description:** Render inline graphics via Sixel escape sequences.
**Rationale:** Modern terminal feature for displaying images.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Parser/` (new Sixel parser)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Render/Grid.lean`
**Estimated Effort:** Large
**Dependencies:** None

### [Priority: Low] Configuration File Support
**Description:** Load configuration from a TOML file on startup.
**Rationale:** Current configuration is hardcoded in Main.lean.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Config.lean`
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Main.lean`
**Estimated Effort:** Small
**Dependencies:** totem library for TOML parsing

### [Priority: Low] Keyboard Layout Support
**Description:** Support non-US keyboard layouts beyond hardcoded US mapping.
**Rationale:** `keyCodeToChar` in Input.lean is hardcoded to US QWERTY layout.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Input.lean`
**Estimated Effort:** Medium
**Dependencies:** macOS keyboard layout API

---

## Code Improvements

### [Priority: High] Remove Debug Output in Production
**Current State:** `handleKeyboard` in Loop.lean has `debug := true` hardcoded (line 115), causing excessive debug output.
**Proposed Change:** Set `debug := false` by default or make it configurable.
**Benefits:** Cleaner output, better performance.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Loop.lean` (line 115)
**Estimated Effort:** Trivial

### [Priority: High] Connect Response Writing to PTY
**Current State:** `ExecResult` in Executor.lean collects responses (e.g., cursor position reports) but they are never written back to PTY.
**Proposed Change:** Write responses back to PTY after executing actions.
**Benefits:** Proper terminal emulation - many programs query terminal state.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/State.lean` (line 122-128)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Loop.lean`
**Estimated Effort:** Small

### [Priority: Medium] Optimize Buffer Operations
**Current State:** Many Buffer operations iterate cell-by-cell with functional updates (e.g., `scrollUp`, `scrollRegionUp`, `fillRect`).
**Proposed Change:** Use `Array.set!` in batch operations; consider mutable buffer for hot paths.
**Benefits:** Improved performance for large buffers and frequent scrolling.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Core/Buffer.lean`
**Estimated Effort:** Medium

### [Priority: Medium] Add Private Marker to CSI Dispatch
**Current State:** `privateMarker` is tracked in parser state but not passed to `TerminalCommand.fromCSI`.
**Proposed Change:** Pass private marker to CSI dispatch for proper `?` sequence handling.
**Benefits:** Correct parsing of private mode sequences (CSI ? n h/l).
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Parser/Machine.lean` (lines 121-153)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Parser/CSI.lean`
**Estimated Effort:** Small

### [Priority: Medium] Render Dirty Rows Optimization
**Current State:** `renderDirty` function exists but appears unused; `render` always renders all cells.
**Proposed Change:** Use `renderDirty` in the main loop to only re-render changed rows.
**Benefits:** Significant rendering performance improvement.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Loop.lean` (line 60)
**Estimated Effort:** Small

### [Priority: Medium] Extract Cell Rendering to Batched Draw Calls
**Current State:** Each cell is rendered individually with separate Canvas operations.
**Proposed Change:** Batch cells with same background color; use text batching for foreground.
**Benefits:** Reduced draw call overhead, better GPU utilization.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Render/Grid.lean`
**Estimated Effort:** Medium

### [Priority: Medium] Add Proper Error Handling in PTY Operations
**Current State:** PTY FFI functions can fail but errors are not always handled gracefully.
**Proposed Change:** Add try/catch blocks in main loop for PTY operations.
**Benefits:** Graceful handling of shell exit, PTY errors.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Loop.lean`
**Estimated Effort:** Small

### [Priority: Low] Use Tincture for Color Operations
**Current State:** Color is defined locally in Style.lean with custom RGB conversion.
**Proposed Change:** Integrate with tincture library for color operations.
**Benefits:** Consistent color handling across the graphics stack, color blending support.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Core/Style.lean`
- `/Users/Shared/Projects/lean-workspace/graphics/vane/lakefile.lean` (add dependency)
**Estimated Effort:** Medium

### [Priority: Low] Extract Parser Actions to Separate File
**Current State:** Parser step functions are defined inside `where` clauses in Machine.lean, making them hard to test individually.
**Proposed Change:** Extract step functions as top-level definitions.
**Benefits:** Better testability, cleaner code organization.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Parser/Machine.lean`
**Estimated Effort:** Small

---

## Code Cleanup

### [Priority: High] Add Tests for Terminal State Operations
**Issue:** Terminal state operations (scrollUp, scrollDown, insertLines, etc.) have no dedicated tests.
**Location:** `/Users/Shared/Projects/lean-workspace/graphics/vane/VaneTests/Main.lean`
**Action Required:** Add test suite for `TerminalState` operations including cursor movement, scroll regions, and buffer modifications.
**Estimated Effort:** Medium

### [Priority: High] Add Tests for Executor
**Issue:** The Executor module that applies parsed commands has no tests.
**Location:** `/Users/Shared/Projects/lean-workspace/graphics/vane/VaneTests/`
**Action Required:** Create `ExecutorTests.lean` with tests for C0 execution, ESC sequences, CSI commands, and OSC commands.
**Estimated Effort:** Medium

### [Priority: Medium] Add Tests for Cursor Operations
**Issue:** Cursor module is untested.
**Location:** `/Users/Shared/Projects/lean-workspace/graphics/vane/VaneTests/`
**Action Required:** Create tests for cursor movement, bounds clamping, wrap pending behavior, and blink state.
**Estimated Effort:** Small

### [Priority: Medium] Add Tests for Modes
**Issue:** Terminal modes (setPrivateMode, resetPrivateMode) are untested.
**Location:** `/Users/Shared/Projects/lean-workspace/graphics/vane/VaneTests/`
**Action Required:** Create tests for mode flag setting/resetting.
**Estimated Effort:** Small

### [Priority: Medium] Consolidate Duplicate withFg/setFg Methods
**Issue:** Cell has both `withFg` and `setFg` methods that do the same thing (lines 26 and 35).
**Location:** `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Core/Cell.lean`
**Action Required:** Remove duplicate `setFg`/`setBg` methods, keep `withFg`/`withBg` for consistency.
**Estimated Effort:** Trivial

### [Priority: Medium] Document C0 and C1 Control Characters
**Issue:** C0 and C1 namespace constants lack documentation explaining their purpose.
**Location:** `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Parser/Types.lean` (lines 7-62)
**Action Required:** Add docstrings explaining each control character's function in terminal emulation.
**Estimated Effort:** Small

### [Priority: Low] Add Module-Level Documentation
**Issue:** Most files lack module-level docstrings explaining their purpose and usage.
**Location:** All source files
**Action Required:** Add `/-- ... -/` docstrings at the top of each module.
**Estimated Effort:** Small

### [Priority: Low] Extract Magic Numbers in Color Conversion
**Issue:** 256-color palette calculation uses magic numbers (lines 93-104 in Style.lean).
**Location:** `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Core/Style.lean`
**Action Required:** Add comments explaining the 6x6x6 color cube and grayscale ramp formulas.
**Estimated Effort:** Trivial

### [Priority: Low] Add Type Annotations to Complex Functions
**Issue:** Some complex functions lack explicit return type annotations, reducing readability.
**Location:** Various files, especially in Parser/ and Terminal/
**Action Required:** Add explicit return type annotations where inference may be unclear.
**Estimated Effort:** Small

### [Priority: Low] Remove Unused OSC Commands
**Issue:** Several OSC commands are parsed but do nothing (setColor, resetColor, notify, setXProperty).
**Location:** `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Terminal/Executor.lean` (lines 198-211)
**Action Required:** Either implement these features or add TODO comments explaining planned functionality.
**Estimated Effort:** Trivial

---

## Architectural Improvements

### [Priority: Medium] Separate Parser from Terminal State
**Current State:** Parser and terminal state are tightly coupled through Action type.
**Proposed Change:** Make Action type more abstract; terminal could implement an ActionHandler trait.
**Benefits:** Easier testing, potential reuse of parser in other contexts.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Parser/Types.lean`
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/Terminal/Executor.lean`
**Estimated Effort:** Medium

### [Priority: Medium] Extract Platform-Specific Code
**Current State:** macOS-specific code (keycodes, PTY FFI) is mixed with portable code.
**Proposed Change:** Create platform abstraction layer for PTY and input handling.
**Benefits:** Easier future porting to Linux/Windows.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/PTY/` (already somewhat isolated)
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Input.lean`
**Estimated Effort:** Medium

### [Priority: Low] Consider Event-Driven Architecture
**Current State:** Main loop polls PTY, keyboard, and window events separately.
**Proposed Change:** Use event queue pattern for unified event handling.
**Benefits:** Cleaner architecture, easier testing, potential async support.
**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/graphics/vane/Vane/App/Loop.lean`
**Estimated Effort:** Large

---

## Summary

**High Priority Items (Essential for MVP):**
1. Complete GPU rendering integration
2. Implement text selection and clipboard
3. Add window resize handling
4. Enable scrollback navigation
5. Remove debug output from production
6. Connect response writing to PTY
7. Add terminal state tests
8. Add executor tests

**Medium Priority Items (Important for Usability):**
1. Unicode wide character support
2. Font style rendering (bold/italic)
3. Underline/strikethrough rendering
4. Configurable color schemes
5. Mouse input forwarding
6. Various performance optimizations

**Low Priority Items (Nice to Have):**
1. Hyperlink support
2. Sixel graphics
3. Configuration file support
4. Non-US keyboard layouts
5. Code documentation improvements
