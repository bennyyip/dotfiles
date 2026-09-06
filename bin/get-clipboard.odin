#!/bin/sh
// 2>/dev/null; exec odin run "$0" -file -- "$@"
// odin build -o:size -out:$HOME/bin/get-clipboard.exe -file get-clipboard.odin

package main
import "core:os"
import win32 "core:sys/windows"

read_clipboard :: proc() -> string {
	if !win32.OpenClipboard(nil) {
		return ""
	}
	defer win32.CloseClipboard()


	handle := win32.GetClipboardData(win32.CF_TEXT)
	if handle == nil {
		return ""
	}

	text := cstring(handle)
	return string(text)
}

main :: proc() {
	os.write_string(os.stdout, read_clipboard())
}
