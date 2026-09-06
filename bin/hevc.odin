#!/bin/sh
// 2>/dev/null; exec odin run "$0" -file -- "$@"
package main
import "core:fmt"
import "core:os"
import "core:strings"

println :: fmt.println

get_video_codec :: proc(file: string) -> string {
	desc := os.Process_Desc {
		command = {
			"ffprobe",
			"-v",
			"error",
			"-select_streams",
			"v:0",
			"-show_entries",
			"stream=codec_name",
			"-of",
			"default=noprint_wrappers=1:nokey=1",
			file,
		},
	}
	state, stdout, stderr, err := os.process_exec(desc, context.allocator)
	if err == nil && state.exit_code == 0 {
		return strings.trim_space(string(stdout))
	}
	// println(state, string(stderr), err, sep = "\n=====================\n")
	return ""
}

get_files :: proc(path: string) -> (files: []string, err: os.Error) {
	xs := os.read_directory_by_path(path, -1, context.allocator) or_return

	res := make([dynamic]string)
	for &x in xs {
		if !(x.type == .Regular &&
			   !strings.contains(x.name, "hevc") &&
			   (strings.ends_with(x.name, ".mkv") ||
					   strings.ends_with(x.name, ".mp4") ||
					   strings.ends_with(x.name, ".ts"))) {
			continue
		}
		codec := get_video_codec(x.fullpath)
		if codec != "av1" && codec != "hevc" && codec != "" {
			append(&res, x.fullpath)
		}
	}
	return res[:], nil
}

with_suffix :: proc(path: string, suffix: string) -> string {
	dot_idx := strings.last_index_byte(path, '.')
	sb := strings.builder_make()
	strings.write_string(&sb, path[:dot_idx])
	strings.write_string(&sb, suffix)
	return strings.to_string(sb)
}

to_hevc :: proc(path: string) -> (err: os.Error) {
	output_path := with_suffix(path, ".hevc.mkv")
	desc := os.Process_Desc {
		command = {
			"ffmpeg",
			"-hide_banner",
			"-y",
			"-nostdin",
			"-i",
			path,
			"-c:v",
			"hevc_nvenc",
			"-c:a",
			"copy",
			"-rc-lookahead",
			"20",
			"-spatial-aq",
			"1",
			"-aq-strength",
			"15",
			"-temporal_aq",
			"1",
			output_path,
		},
		stdout  = os.stdout,
		stderr  = os.stderr,
	}
	process := os.process_start(desc) or_return
	state := os.process_wait(process) or_return
	assert(state.exit_code == 0)
	os.remove(path) or_return
	return
}

check_ffmpeg :: proc() -> bool {
	desc := os.Process_Desc {
		command = {"ffmpeg", "-h"},
	}
	state, _, _, err := os.process_exec(desc, context.allocator)
	if err == nil && state.exit_code == 0 {
		return true
	}
	return false
}


main :: proc() {
	if !check_ffmpeg() {
		fmt.eprintln("ffmpeg is not in PATH")
		os.exit(1)
	}

	dir: string
	if len(os.args) < 2 {
		dir = "."
	} else {
		dir = os.args[1]
	}
	files, err := get_files(dir)
	assert(err == nil)
	for f in files {
		context.allocator = context.temp_allocator
		println("Converting", f)
		err := to_hevc(f)
		assert(err == nil)
		free_all(context.temp_allocator)
	}
}
