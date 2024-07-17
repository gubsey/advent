const std = @import("std");
const Map = std.StringHashMap(Valve);

const INF = "∞";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    //defer if (gpa.deinit() == .leak) std.process.exit(1);
    const alloc = gpa.allocator();

    const dir = try std.fs.cwd().openDir(".", .{ .iterate = true });
    var files = dir.iterate();
    while (try files.next()) |file| {
        std.debug.print("{s}\n", .{file.name});
    }

    var valves = try load_valves(alloc, "example.txt");
    defer destroy_valve_map(&valves);

    var valve_iter = valves.iterator();
    while (valve_iter.next()) |entry| {
        std.debug.print("{s} : {d} {s}\n", .{ entry.key_ptr.*, entry.value_ptr.*.flow_rate, entry.value_ptr.tunnels });
    }

    const florshall = try floyd_warshall_graph_from_valve_map(valves);
    _ = florshall;
}

fn floyd_warshall_graph_from_valve_map(map: Map) !Florshall {
    var graph = try valve_map_to_start_floyd(map);
    const size = graph.keys.len;

    for (0..size) |a| {
        graph.print();
        std.debug.print("\n", .{});
        for (0..size) |i| {
            for (0..size) |j| {
                const ij = graph.table[i][j];
                if (ij == 0 or i == a or j == a) continue;

                const ia = graph.table[i][a] orelse continue;
                const aj = graph.table[a][j] orelse continue;
                const n = ia + aj;

                if (ij) |x| {
                    if (n < x) {
                        graph.table[i][j] = n;
                    }
                } else {
                    graph.table[i][j] = n;
                }
            }
        }
    }

    graph.print();

    return graph;
}

fn valve_map_to_start_floyd(map: Map) !Florshall {
    var graph = std.ArrayList([]?i32).init(map.allocator);

    var key_iter = map.keyIterator();
    var key_list = std.ArrayList([]const u8).init(map.allocator);
    while (key_iter.next()) |key| {
        // if (map.get(key.*).?.flow_rate == 0 and !std.mem.eql(u8, key.*, "AA")) continue;
        try key_list.append(key.*);
    }
    const keys = try key_list.toOwnedSlice();

    for (0..keys.len, keys) |i, keyi| {
        var row = std.ArrayList(?i32).init(map.allocator);
        const this = map.get(keyi).?;

        for (0..keys.len, keys) |j, keyj| {
            var node: ?i32 = null;

            if (i == j) {
                node = 0;
            } else {
                for (this.tunnels) |t| {
                    if (std.mem.eql(u8, t, keyj)) {
                        node = 1;
                    }
                }
            }

            try row.append(node);
        }

        const ros = try row.toOwnedSlice();

        try graph.append(ros);
    }

    return .{ .keys = keys, .table = try graph.toOwnedSlice() };
}

/// floyd warshall
const Florshall = struct {
    keys: [][]const u8,
    table: [][]?i32,

    fn print(self: @This()) void {
        std.debug.print("   ", .{});
        for (self.keys) |key| {
            std.debug.print("{s:>4}", .{key});
        }
        std.debug.print("\n", .{});
        for (self.table, 0..) |row, i| {
            std.debug.print("{s} ", .{self.keys[i]});
            for (row) |cell| {
                if (cell) |x| {
                    var buf: [20]u8 = undefined;
                    const s = std.fmt.bufPrint(&buf, "{d}", .{x}) catch unreachable;
                    std.debug.print("{s:4}", .{s});
                } else {
                    std.debug.print("{s:>4}", .{INF});
                }
            }
            std.debug.print("\n", .{});
        }
    }
};

fn load_valves(alloc: std.mem.Allocator, file_str: []const u8) !Map {
    const file = try std.fs.cwd()
        .readFileAlloc(alloc, file_str, 1024 * 1024);
    defer alloc.free(file);

    var map = Map.init(alloc);

    var lines = std.mem.tokenizeAny(u8, file, "\n");
    while (lines.next()) |line| {
        var words_iter = std.mem.tokenizeAny(u8, line, " =;,");

        var words_list = std.ArrayList([]const u8).init(alloc);
        defer words_list.deinit();

        while (words_iter.next()) |word| {
            try words_list.append(word);
        }
        const words = words_list.items;

        const name = try alloc.dupe(u8, words[1]);
        const flow_rate = try std.fmt.parseInt(i32, words[5], 10);

        var tunnels = std.ArrayList([]const u8).init(alloc);
        for (words[10..]) |word| {
            try tunnels.append(try alloc.dupe(u8, word));
        }

        try map.put(name, .{ .flow_rate = flow_rate, .tunnels = try tunnels.toOwnedSlice(), .open = false });
    }

    return map;
}

test "parse example file" {
    var valves = try load_valves(std.testing.allocator, "../example.txt");
    defer destroy_valve_map(&valves);
    try std.testing.expect(valves.count() == 10);
}

fn destroy_valve_map(valves: *Map) void {
    var keys = valves.keyIterator();
    while (keys.next()) |key| {
        var entry = valves.fetchRemove(key.*).?;
        entry.value.deinit(valves.allocator);
        valves.allocator.free(entry.key);
        keys = valves.keyIterator();
    }
    valves.deinit();
}

const Valve = struct {
    flow_rate: i32,
    tunnels: [][]const u8,
    open: bool,

    fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
        for (self.tunnels) |word| {
            alloc.free(word);
        }
        alloc.free(self.tunnels);
    }
};
