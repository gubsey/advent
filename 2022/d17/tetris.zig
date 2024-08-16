const std = @import("std");

const Y = Cell.Y;
const N = Cell.N;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const input = try get_stdin(alloc);

    const empty_row: [7]Cell = .{ N, N, N, N, N, N, N };

    var new_stage = Stage{
        .cells = std.ArrayList([7]Cell).init(alloc),
    };

    var selected_shape: usize = 0;

    for (input) |dir| {
        if (floating == null) {
            if (selected_shape >= SHAPES.len) selected_shape = 0;
            floating = .{
                .position = Point{
                    .x = 2,
                    .y = new_stage.height + 3,
                },
                .shape = SHAPES[selected_shape],
            };
            selected_shape += 1;
        }
        std.debug.print("{c}\n{any}\n", .{ dir, floating.?.position });

        const float = &floating.?;

        ////////////////////

        float.position.y -= 1;

        if (float.position.y > new_stage.height) continue;
        var still_falling = false;

        outer: for (0..float.shape.height) |shape_y| {
            const stage_y = shape_y + float.position.y;
            for (0..float.shape.width) |shape_x| {
                const stage_x = shape_x + float.position.x;

                const stage_cell = new_stage.cells.items[stage_y][stage_x];
                const shape_cell = float.shape.cells[shape_y][shape_x];

                if (shape_cell == .Y and stage_cell == .Y) {
                    float.position.y += 1;
                    still_falling = false;
                    break :outer;
                }
            }
        }

        if (still_falling) continue;

        new_stage.set(float);

        floating = null;
    }

    std.debug.print("\n", .{});
}

const Stage = struct {
    cells: std.ArrayList([7]Cell),
    height: usize = 0,
    float: ?FallingShape,

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
        for (0..self.cells.len) |i| {
            const row_ndx = self.cells.len - (i + 1);
            const row = self.cells[row_ndx];
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

    fn move(self: *@This(), dir: Direction) void {
        if (self.float == null) return;
        const float = &self.float.?;

        while (self.cells.items.len < float.position.y + float.shape.height) {
            var row: [7]Cell = undefined;
            @memset(&row, .N);
            try self.cells.append(row);
        }

        switch (dir) {
            .Left => if (float.position.x > 0) {
                float.position.x -= 1;
            },
            .Right => if (float.position.x + float.shape.width < 7) {
                float.position.x += 1;
            },
        }

        if (self.float.position.y == 0) {
            self.set(float);
            self.height = float.position.y + float.shape.height;

            self.floating = null;
            return;
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
        .{ N, N, Y, N },
        .{ N, N, Y, N },
        .{ Y, Y, Y, N },
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
