const std = @import("std");

const INF = "∞";

pub fn main() !void {
    var gba = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gba.deinit();
    const alc = gba.allocator();

    const args = try std.process.argsAlloc(alc);
    defer std.process.argsFree(alc, args);

    const filename = args[1];
    const file_contents = try std.fs.cwd().readFileAlloc(alc, filename, 1024 * 1024);
    defer alc.free(file_contents);

    var line_iter = std.mem.tokenizeAny(u8, file_contents, "\n");
    const lines = try collect([]const u8, alc, &line_iter);
    defer alc.free(lines);

    var valve_map = try ValveMap.from_lines(alc, lines);
    defer valve_map.deinit();
}

fn collect(comptime T: type, alc: std.mem.Allocator, iter: anytype) ![]T {
    var r = std.ArrayList(T).init(alc);
    while (iter.next()) |item| {
        try r.append(item);
    }
    return r.toOwnedSlice();
}

const Valve = struct {
    flow_rate: i32,
    tunnels: std.StringHashMap(void),
};

const ValveMap = struct {
    map: std.StringHashMap(Valve),
    allocator: std.mem.Allocator,

    fn from_lines(alc: std.mem.Allocator, lines: [][]const u8) !ValveMap {
        var map = std.StringHashMap(Valve).init(alc);
        for (lines) |line| {
            var words_iter = std.mem.tokenizeAny(u8, line, " =;,");
            const words = try collect([]const u8, alc, &words_iter);
            defer alc.free(words);

            const name = try alc.dupe(u8, words[1]);
            const flow_rate = try std.fmt.parseInt(i32, words[5], 10);

            var tunnels = std.StringHashMap(void).init(alc);
            for (words[10..]) |word| {
                try tunnels.put(try alc.dupe(u8, word), {});
            }

            try map.put(name, .{ .flow_rate = flow_rate, .tunnels = tunnels });
        }
        return .{ .map = map, .allocator = alc };
    }

    fn deinit(self: *@This()) void {
        var valves = self.map.iterator();
        while (valves.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            var keys = entry.value_ptr.tunnels.keyIterator();
            while (keys.next()) |key| {
                self.allocator.free(key.*);
            }
            entry.value_ptr.tunnels.deinit();
        }
        self.map.deinit();
    }

    fn keysAlloc(self: *@This()) !void {
        var keys = self.map.keyIterator();
        return try collect([]const u8, self.allocator, &keys);
    }
};
