const std = @import("std");

const INF = "∞";
const START = "AA";

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

    var fw = try FloydWarshall.from_valve_map(&valve_map);
    defer fw.deinit();

    while (try fw.next()) |_| {
        //std.debug.print("\n", .{});
        //fw.print();
    }

    var flows = std.StringHashMap(i32).init(alc);
    defer flows.deinit();

    var valve_iter = valve_map.map.iterator();
    while (valve_iter.next()) |entry| {
        const rate = entry.value_ptr.*.flow_rate;
        if (rate == 0) continue;

        const name = entry.key_ptr.*;

        try flows.put(name, rate);
    }

    const path_map = try alc.create(std.StringHashMap(void));
    path_map.* = std.StringHashMap(void).init(alc);

    var pq = PQ.init(alc, {});
    defer pq.deinit();
    try pq.add(.{
        .name = START,
        .path = path_map,
        .score = 0,
        .time_remaining = 30,
    });

    var answers = std.ArrayList(i32).init(alc);

    while (pq.removeOrNull()) |queue_item| {
        defer alc.destroy(queue_item.path);
        defer queue_item.path.deinit();

        if (queue_item.path.count() == flows.count()) {
            try answers.append(queue_item.score);
        }

        var flow_iter = flows.iterator();
        while (flow_iter.next()) |flow| {
            if (queue_item.path.contains(flow.key_ptr.*)) continue;
            // delta time
            const dt: i32 = fw.table.get(queue_item.name).?.get(flow.key_ptr.*).? + 1;
            // remaining time
            const rt = queue_item.time_remaining - dt;

            if (rt < 0) continue;

            const ds = rt * flow.value_ptr.*;
            var new_path = try alc.create(std.StringHashMap(void));
            new_path.* = try queue_item.path.clone();
            try new_path.put(flow.key_ptr.*, {});

            try pq.add(.{ .name = flow.key_ptr.*, .path = new_path, .score = queue_item.score + ds, .time_remaining = rt });
        }
    }

    while (pq.removeOrNull()) |qi| {
        defer qi.path.deinit();
        defer alc.destroy(qi.path);
    }

    const answer_slice = try answers.toOwnedSlice();
    defer alc.free(answer_slice);

    std.debug.print("{d}", .{answer_slice});
}

const QueueItem = struct {
    name: []const u8,
    path: *std.StringHashMap(void),
    score: i32,
    time_remaining: i32,
};

fn queueFn(context: void, a: QueueItem, b: QueueItem) std.math.Order {
    _ = context;
    return std.math.order(a.score, b.score).invert();
}

const PQ = std.PriorityQueue(QueueItem, void, queueFn);

fn part1(fw: FloydWarshall, flow_map: std.StringHashMap(i32)) u32 {
    _ = fw;
    _ = flow_map;
}

fn max(comptime T: type, a: T, b: T) T {
    if (a < b) {
        return b;
    } else {
        return a;
    }
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
    alloc: std.mem.Allocator,
    order: [][]const u8,

    fn from_lines(alc: std.mem.Allocator, lines: [][]const u8) !ValveMap {
        var map = std.StringHashMap(Valve).init(alc);
        var order = std.ArrayList([]u8).init(alc);
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
            try order.append(name);
        }
        return .{ .map = map, .alloc = alc, .order = try order.toOwnedSlice() };
    }

    fn deinit(self: *@This()) void {
        var valves = self.map.iterator();
        while (valves.next()) |entry| {
            self.alloc.free(entry.key_ptr.*);
            var keys = entry.value_ptr.tunnels.keyIterator();
            while (keys.next()) |key| {
                self.alloc.free(key.*);
            }
            entry.value_ptr.tunnels.deinit();
        }
        self.map.deinit();
        self.alloc.free(self.order);
    }
};

const FloydWarshall = struct {
    map: *ValveMap,
    table: std.StringHashMap(*std.StringHashMap(i32)),
    //table: [][]?i32,
    alloc: std.mem.Allocator,
    step: usize = 0,

    fn from_valve_map(map: *ValveMap) !FloydWarshall {
        var table = std.StringHashMap(*std.StringHashMap(i32)).init(map.alloc);
        const keys = map.order;

        for (0..keys.len, keys) |i, keyi| {
            const valve = map.map.get(keyi).?;
            var row = try map.alloc.create(std.StringHashMap(i32));
            row.* = std.StringHashMap(i32).init(map.alloc);

            for (0..keys.len, keys) |j, keyj| {
                if (i == j) {
                    try row.put(keyj, 0);
                    continue;
                }

                if (valve.tunnels.get(keyj)) |_| {
                    try row.put(keyj, 1);
                }
            }

            try table.put(keyi, row);
        }

        return .{ .map = map, .table = table, .alloc = map.alloc };
    }

    fn next(self: *@This()) !?void {
        const keys = self.map.order;
        if (self.step >= keys.len) return null;

        const a = keys[self.step];

        for (keys) |i| {
            for (keys) |j| {
                const ia = self.table.get(i).?.get(a) orelse continue;
                const aj = self.table.get(a).?.get(j) orelse continue;
                const sum = ia + aj;

                if (self.table.get(i).?.get(j)) |n| {
                    if (n <= sum) continue;
                }

                try self.table.get(i).?.put(j, sum);
            }
        }

        self.step += 1;
    }

    fn deinit(self: *@This()) void {
        var val_iter = self.table.valueIterator();
        while (val_iter.next()) |row| {
            row.*.deinit();
            self.map.alloc.destroy(row.*);
        }
        self.table.deinit();
    }

    fn print(self: @This()) void {
        const padding = 3;
        std.debug.print("   ", .{});
        for (self.map.order) |key| {
            std.debug.print("{s:>[1]}", .{ key, padding });
        }
        std.debug.print("\n", .{});
        for (self.map.order) |i| {
            std.debug.print("{s} ", .{i});
            const row = self.table.get(i).?;
            for (self.map.order) |j| {
                if (row.get(j)) |x| {
                    var buf: [padding]u8 = undefined;
                    const s = std.fmt.bufPrint(&buf, "{d}", .{x}) catch unreachable;
                    std.debug.print("{s:[1]}", .{ s, padding });
                } else {
                    std.debug.print("{s:>[1]}", .{ INF, padding });
                }
            }
            std.debug.print("\n", .{});
        }
    }
};
