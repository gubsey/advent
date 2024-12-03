package main

import "../../lib"
import "core:fmt"
import "core:log"
import "core:os"
import "core:strconv"
import "core:testing"
import "core:text/regex"

main :: proc() {
	cat := lib.get_stdin()
	rex, err := regex.create(`(?:mul\((\d+),(\d+)\))|(?:(?:do\(\))|(?:don't\(\)))`, {.Global})
	defer regex.destroy(rex)
	if err != nil {
		fmt.println(err)
	}
    
    dont := false
	sum := 0
	sum2 := 0
	for cap in regex.match(rex, string(cat)) {
		defer regex.destroy(cap)

		pos := cap.pos[0]
		cat = cat[pos[1]:]

        if cap.groups[0] == "do()" {
            dont = false
        } else if cap.groups[0] == "don't()" {
            dont = true
        }

        fmt.println(cap)

		if len(cap.groups) != 3 {
			continue
		}

		a, _ := strconv.parse_int(cap.groups[1])
		b, _ := strconv.parse_int(cap.groups[2])
        p := a*b
		sum += p
        if !dont {
            sum2 += p
        }
	}

	fmt.println(sum)
	fmt.println(sum2)
}
