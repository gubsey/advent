package main

import "core:fmt"
import "core:io"
import "core:os"
import "core:strconv"
import "core:strings"

main :: proc() {
	in_stream := os.stream_from_handle(os.stdin)
	buf: [dynamic]byte
	defer delete(buf)
	for {
		n, e := io.read_byte(in_stream)
		if e == .EOF {
			break
		}
		append(&buf, n)
	}
	input_str := string(buf[:])

	input: [dynamic][2]int
	indexes: [dynamic]int
	for line, i in strings.split_lines(input_str) {
		n := strconv.parse_int(line) or_continue
		append(&input, [2]int{n, i})
		append(&indexes, i)
	}

	fmt.println(input)
}

