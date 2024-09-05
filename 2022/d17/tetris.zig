const std = @import("std");

const Y = Cell.Y;
const N = Cell.N;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const input = try get_stdin(alloc);

    var new_stage = Stage{
        .cells = std.ArrayList([7]Cell).init(alloc),
    };

    var i: usize = 0;
    while (true) {
        if (new_stage.shapei > 2022) break;
        const char = input[i % input.len];
        defer i += 1;

        // const h = try std.io.getStdIn().reader().readByte();
        // std.debug.print("{c}\n", .{char});
        // _ = h;

        const dir = switch (char) {
            '<' => Direction.Left,
            '>' => Direction.Right,
            else => return error.InvalidCharacterInput,
        };

        try new_stage.move(dir);
    }

    std.debug.print("{d}\n", .{new_stage.height});
}

const Stage = struct {
    cells: std.ArrayList([7]Cell),
    height: usize = 0,
    shapei: usize = 0,
    float: ?FallingShape = null,

    fn set_next_float(self: *@This()) void {
        self.float = .{
            .position = Point{
                .x = 2,
                .y = self.height + 3,
            },
            .shape = SHAPES[self.shapei % SHAPES.len],
        };

        self.shapei += 1;
    }

    /// panics if `[self.float]` is null
    fn add_height(self: *@This()) !void {
        const float_y = self.float.?.position.y;
        const float_h = self.float.?.shape.height;

        while (self.cells.items.len < float_y + float_h) {
            var row: [7]Cell = undefined;
            @memset(&row, .N);
            try self.cells.append(row);
        }
    }

    fn detect_collision(self: @This()) bool {
        const float = self.float.?;
        for (float.shape.cells, float.position.y..) |frow, i| {
            for (float.position.x.., frow) |j, fcell| {
                if (i >= self.cells.items.len) continue;
                if (j >= 7) continue;
                if (self.cells.items[i][j] == .Y and fcell == .Y) {
                    return true;
                }
            }
        }
        return false;
    }

    fn move(self: *@This(), dir: Direction) !void {
        if (self.float == null) self.set_next_float();
        const float = &self.float.?;
        try self.add_height();

        const old_x = float.position.x;
        switch (dir) {
            .Left => if (float.position.x > 0) {
                float.position.x -= 1;
            },
            .Right => if (float.position.x + float.shape.width < 7) {
                float.position.x += 1;
            },
        }

        if (self.detect_collision()) float.position.x = old_x;

        if (float.position.y == 0) {
            self.set(float.*);
            self.height = float.position.y + float.shape.height;

            self.float = null;
            return;
        }

        float.position.y -= 1;

        if (float.position.y > self.height) return;

        if (!self.detect_collision()) return;

        float.position.y += 1;

        self.set(float.*);
        self.height = float.position.y + float.shape.height;

        self.float = null;
    }

    fn init(alloc: std.mem.Allocator) @This() {
        return .{
            .cells = std.ArrayList([7]Cell).init(alloc),
        };
    }

    fn set(stage: *@This(), float: FallingShape) void {
        for (0..float.shape.height) |shape_y| {
            const stage_y = shape_y + float.position.y;
            for (0..float.shape.width) |shape_x| {
                const stage_x = shape_x + float.position.x;

                stage.cells.items[stage_y][stage_x] = float.shape.cells[shape_y][shape_x];
            }
        }
    }

    fn print(self: @This()) void {
        const cells = self.cells.items;
        for (0..cells.len) |i| {
            const row_ndx = cells.len - (i + 1);
            const row = cells[row_ndx];
            for (0..row.len) |j| {
                const cell = row[j];

                var c: u8 = switch (cell) {
                    .Y => '#',
                    .N => '.',
                };

                if (self.float) |f| {
                    if ((row_ndx >= f.position.y) and (row_ndx < f.position.y + f.shape.height)) {
                        if ((j >= f.position.x) and (j < f.position.x + f.shape.width)) {
                            c = switch (f.shape.cells[row_ndx - f.position.y][j - f.position.x]) {
                                .Y => '@',
                                else => c,
                            };
                        }
                    }
                }
                std.debug.print("{c}", .{c});
            }
            std.debug.print("\n", .{});
        }
    }
};

const Direction = enum { Left, Right };

fn get_stdin(alloc: std.mem.Allocator) ![]const u8 {
    const stdin = std.io.getStdIn().reader();
    const input_opt = try stdin.readUntilDelimiterOrEofAlloc(alloc, 10, 1024 * 1024);
    return input_opt.?;
}

const Point = struct { x: usize, y: usize };

const FallingShape = struct {
    position: Point,
    shape: Shape,
};

const Shape = struct {
    cells: [4][4]Cell,
    width: usize,
    height: usize,
};

pub const SHAPES = [_]Shape{ .{
    .cells = .{
        .{ Y, Y, Y, Y },
        .{ N, N, N, N },
        .{ N, N, N, N },
        .{ N, N, N, N },
    },
    .width = 4,
    .height = 1,
}, .{
    .cells = .{
        .{ N, Y, N, N },
        .{ Y, Y, Y, N },
        .{ N, Y, N, N },
        .{ N, N, N, N },
    },
    .width = 3,
    .height = 3,
}, .{
    .cells = .{
        .{ Y, Y, Y, N },
        .{ N, N, Y, N },
        .{ N, N, Y, N },
        .{ N, N, N, N },
    },
    .width = 3,
    .height = 3,
}, .{
    .cells = .{
        .{ Y, N, N, N },
        .{ Y, N, N, N },
        .{ Y, N, N, N },
        .{ Y, N, N, N },
    },
    .width = 1,
    .height = 4,
}, .{
    .cells = .{
        .{ Y, Y, N, N },
        .{ Y, Y, N, N },
        .{ N, N, N, N },
        .{ N, N, N, N },
    },
    .width = 2,
    .height = 2,
} };

pub const Cell = enum { Y, N };
