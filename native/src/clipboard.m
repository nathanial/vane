// clipboard.m - macOS clipboard FFI for Vane terminal
#import <Cocoa/Cocoa.h>
#include <lean/lean.h>

// Set text to clipboard
// lean_vane_clipboard_set : String → IO Unit
LEAN_EXPORT lean_obj_res lean_vane_clipboard_set(b_lean_obj_arg text, lean_obj_arg world) {
    @autoreleasepool {
        const char* str = lean_string_cstr(text);
        NSString* nsStr = [NSString stringWithUTF8String:str];

        NSPasteboard* pasteboard = [NSPasteboard generalPasteboard];
        [pasteboard clearContents];
        [pasteboard setString:nsStr forType:NSPasteboardTypeString];
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// Get text from clipboard
// lean_vane_clipboard_get : IO String
LEAN_EXPORT lean_obj_res lean_vane_clipboard_get(lean_obj_arg world) {
    @autoreleasepool {
        NSPasteboard* pasteboard = [NSPasteboard generalPasteboard];
        NSString* str = [pasteboard stringForType:NSPasteboardTypeString];

        if (str == nil) {
            return lean_io_result_mk_ok(lean_mk_string(""));
        }

        const char* cstr = [str UTF8String];
        return lean_io_result_mk_ok(lean_mk_string(cstr));
    }
}
