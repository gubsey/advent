package main

import "../../lib"
import "core:fmt"
import "core:log"
import "core:os"
import "core:testing"
import "core:text/regex"

main :: proc() {
	cat, _ := os.read_entire_file_from_filename("ex.txt")
	rex, err := regex.create(`mul\(\d+,\d+\)`)
	defer regex.destroy(rex)
	if err != nil {
		log.fatal(err)
	}
	matches, _ := regex.match(rex, string(cat))
	fmt.println(matches, string(cat))
}
