package main

import "core:fmt"
import "core:log"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"

p1 :: proc(input: string) -> int {
	lines := strings.split_lines(input)
	defer delete(lines)
	a, b := input_lists(lines)
	defer delete(a)
	defer delete(b)
	slice.sort(a[:])
	slice.sort(b[:])
	sum := 0
	for i := 0; i < len(a); i += 1 {
		sum += abs(a[i] - b[i])
	}
	return sum
}

p2 :: proc(input: string) -> int {
	lines := strings.split_lines(input)
	defer delete(lines)
	a, b := input_lists(lines)
	defer delete(a)
	defer delete(b)
	sum := 0
	for x in a {
		mul := 0
		for y in b {
			if x == y {
				mul += 1
			}
		}
		sum += x * mul
	}
	return sum
}

main :: proc() {
	cat := get_stdin()
	defer delete(cat)
	fmt.println(p1(cat))
	fmt.println(p2(cat))
}

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

input_lists :: proc(lines: []string) -> (a: [dynamic]int, b: [dynamic]int) {
	for line in lines {
		words := strings.fields(line)

		w1, _ := strconv.parse_int(words[0])
		append(&a, w1)
		w2, _ := strconv.parse_int(words[1])
		append(&b, w2)
	}
	return
}

