const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) std.process.exit(1);
    const alloc = gpa.allocator();

    var valves = try load_valves(alloc, "../example.txt");
    defer destroy_valve_map(&valves) catch unreachable;

    var valve_iter = valves.iterator();
    while (valve_iter.next()) |entry| {
        std.debug.print("{s} : {d} {s}\n", .{ entry.key_ptr.*, entry.value_ptr.*.flow_rate, entry.value_ptr.tunnels });
    }
}

fn load_valves(alloc: std.mem.Allocator, file_str: []const u8) !std.StringHashMap(Valve) {
    const file = try std.fs.cwd()
        .readFileAlloc(alloc, file_str, 1024 * 1024);
    defer alloc.free(file);

    var map = std.StringHashMap(Valve).init(alloc);

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
    defer destroy_valve_map(&valves) catch unreachable;

    var valve_iter = valves.iterator();
    while (valve_iter.next()) |entry| {
        std.debug.print("{s} : {d} {s}\n", .{ entry.key_ptr.*, entry.value_ptr.*.flow_rate, entry.value_ptr.tunnels });
    }
}

fn destroy_valve_map(valves: *std.StringHashMap(Valve)) !void {
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
