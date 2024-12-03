package main

import "../../lib"
import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"
import "core:testing"

main :: proc() {
	cat := lib.get_stdin()

	lines := strings.split_lines(cat)
	defer delete(lines)
	data := make([][]int, len(lines))
	defer delete(data)
	defer for row in data {
		delete(row)
	}

	for line, i in lines {
		fields := strings.fields(line)
		defer delete(fields)
		data[i] = make([]int, len(fields))
		for field, j in fields {
			n, _ := strconv.parse_int(field)
			data[i][j] = n
		}
	}

	for row in data {
		ascending: bool = row[0] < row[1]
		for i in 0 ..< len(row) - 1 {
            
        }
	}

	fmt.println(data)
}
