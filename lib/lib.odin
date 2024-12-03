package lib

import "core:strings"
import "core:os"

get_stdin :: proc() -> string {
	catb: strings.Builder
	buf: [1024]u8
	for {
		n, err := os.read(os.stdin, buf[:])
		strings.write_bytes(&catb, buf[:n])
		if n < 1024 {
			break
		}
	}
	return strings.trim_space(strings.to_string(catb))
}
