const std = @import("std");
const Allocator = std.mem.Allocator;
const AnyReader = std.io.AnyReader;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const rocks = try Rock.get_rocks(alloc);
    for (rocks) |rock| {
        rock.print();
        std.debug.print("\n", .{});
    }
}

const Rock = struct {
    width: usize,
    height: usize,
    profile: [][]bool,

    fn get_rocks(alloc: Allocator) ![]Rock {
        const file_str = @embedFile("rocks.txt");
        var rock_strings = std.mem.tokenizeSequence(u8, file_str, "\n\n");

        var rocks = std.ArrayList(Rock).init(alloc);

        while (rock_strings.next()) |rock_str| {
            var rock = std.ArrayList([]bool).init(alloc);
            var rock_row = std.ArrayList(bool).init(alloc);

            for (rock_str) |char| {
                switch (char) {
                    '.' => try rock_row.append(false),
                    '#' => try rock_row.append(true),
                    '\n' => try rock.append(try rock_row.toOwnedSlice()),
                    else => return error.invalid_rock_char,
                }
            }
            try rock.append(try rock_row.toOwnedSlice());

            const rock_slice = try rock.toOwnedSlice();

            try rocks.append(.{
                .profile = rock_slice,
                .width = rock_slice[0].len,
                .height = rock_slice.len,
            });
        }

        return rocks.toOwnedSlice();
    }

    fn print(self: Rock) void {
        for (self.profile) |row| {
            for (row) |cell| {
                const p: u8 = if (cell) '#' else '.';

                std.debug.print("{c}", .{p});
            }
            std.debug.print("\n", .{});
        }
    }
};

const Direction = enum { Right, Left };
